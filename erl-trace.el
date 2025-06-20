;;; erl-trace.el --- a simple package                     -*- lexical-binding: t; -*-
;; Package-Version: 20240326.2506
;; Package-X-Original-Version: 20211028.1412

;; Copyright (C) 2021
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(defconst erl-trace-macro
  "-ifndef(FUNC).
-define(FUNC, element(2, element(2, process_info(self(), current_function)))).
-endif.
")

(defconst erl-trace-iotrace-macro
  "-define(IO_TRACE_DEBUG, true).
-define(iotd(Fmt), erl_trace(Fmt, [?MODULE, ?FUNCTION_NAME, ?LINE], {io, simple})).
-define(iotd(Fmt, Args), erl_trace(Fmt, [?MODULE, ?FUNCTION_NAME, ?LINE] ++ Args, {io, simple})).
-define(iotdd(Fmt), erl_trace(Fmt, {io, [?MODULE, ?FUNCTION_NAME, ?LINE],complex})).
-define(iotdd(Fmt, Args), erl_trace(Fmt, [?MODULE, ?FUNCTION_NAME, ?LINE] ++ Args, {io, complex})).
-define(cttd(Fmt), erl_trace(Fmt, [?MODULE, ?FUNCTION_NAME, ?LINE], {ct, simple})).
-define(cttd(Fmt, Args), erl_trace(Fmt, [?MODULE, ?FUNCTION_NAME, ?LINE] ++ Args, {ct, simple})).
-define(cttdd(Fmt), erl_trace(Fmt, {ct, [?MODULE, ?FUNCTION_NAME, ?LINE],complex})).
-define(cttdd(Fmt, Args), erl_trace(Fmt, [?MODULE, ?FUNCTION_NAME, ?LINE] ++ Args, {ct, complex})).
")

(defconst erl-trace-function
  "%% erl-trace support functions
-ifdef(IO_TRACE_DEBUG).
erl_trace(Fmt, Args, {Type, Level}) ->
    case ?IO_TRACE_DEBUG of
        true ->
            {FullFmt, FullArgs} =
                case Level of
                    simple -> {\"~p:~p:~p \"++Fmt, Args};
                    complex -> {\"~p:~p:~s:~p:~p:~p: \"++Fmt++\"~n\",
                                [erl_trace_process_info(),
                                 erl_trace_get_user(),
                                 erl_trace_timestamp()] ++ Args}
                end,
            case Type of
                io -> io:format(FullFmt, FullArgs);
                ct -> ct:pal(FullFmt, FullArgs)
            end;
       false -> ok
    end.")

(defconst erl-trace-timestamp
  "erl_trace_timestamp() ->
    {MS, S, US} = os:timestamp(),
    {_, {Hour, Min, Sec}} = calendar:now_to_local_time({MS, S, US}),
    MSec = trunc(US/1000),
    lists:flatten(io_lib:format(\"~2.10.0B:~2.10.0B:~2.10.0B.~3.10.0B\",
                                [Hour, Min, Sec, MSec])).
-endif.")

(defconst erl-trace-get-user-name
  "erl_trace_get_user() ->
    string:strip(os:cmd(\"id -F\"), right, $\\n).")

(defconst erl-trace-procinfo
  "erl_trace_process_info() ->
    proplists:get_value(registered_name, process_info(self()), self()).")

(defconst erl-trace-release
  "erl -noshell -eval 'erlang:display(erlang:system_info(otp_release)), halt().'")

(defconst erl-trace-stacktrace
  "try throw(exit)
catch _:_ ->
        ST-!- = erlang:get_stacktrace(),
        ok
end,")

(defconst erl-trace-stacktrace-otp-22
  "try throw(exit)
catch _Class:_Reason:Stacktrace-!- ->
        ok
end,")

(defconst erl-trace-option-keys
  '(:detail :location :newline)
  "Valid trace options used by `erl-trace`.

- :detail   → Include process registered name or PID and timestamp in the trace
- :location → Include location info as ?MODULE:?FUNCTION_NAME:?LINE
- :newline  → If non-nil, format each variable on a new line (using ~n)")

(defun erl-get-version ()
  "Return the current OTP version as a string, e.g., \"25\"."
  (string-trim
   (shell-command-to-string
    "erl -noshell -eval 'io:format(\"~s\", [erlang:system_info(otp_release)]), halt().'")))

(defvar erl-trace-erlang-vsn (erl-get-version))

(defconst erl-trace-value-fmt " = ~p")

(defvar erl-trace-mode 'debug
  "Current mode for inserting Erlang trace lines.
Supported values:
- 'debug  → Insert ?LOG_DEBUG(...)
- 'info   → Insert ?LOG_INFO(...)
- 'error  → Insert ?LOG_ERROR(...)
- 'ct     → Insert ct:pal(...) for Common Test
- 'io     → Insert raw io:format(...) trace
- 'macro  → Reserved for custom macro injection")

(defvar erl-trace-prefix '())

(defvar erl-trace-stored '())

(defvar erl-trace-options
  (let ((opts nil))
    (dolist (opt erl-trace-option-keys)
      (setq opts (plist-put opts opt t)))
    opts)
  "Plist storing current erl-trace configuration.")

;; Functions
(defun erl-trace-run-cmd ()
  "Show current trace config and prompt user for a trace command."
  (interactive)
  (let* ((commands '(("mode"       . erl-trace-set-mode)
                     ("toggle"     . toggle-option-wrapper)
                     ("clause"     . erl-trace-clause)
                     ("stacktrace" . erl-trace-stacktrace)
                     ("set-prefix" . erl-trace-set-prefix)))
         (info (erl-trace-info-line))
         (choice (completing-read
                  (concat "Trace config: " info "\nRun erl-trace: ")
                  (mapcar #'car commands)))
         (fn (cdr (assoc choice commands))))
    (when fn (funcall fn))))

(defun erl-trace-info-line ()
  "Return a compact one-line summary of erl-trace config state."
  (let ((opts (mapconcat
               (lambda (opt)
                 (format "%s:%s"
                         (substring (symbol-name opt) 1)
                         (if (plist-get erl-trace-options opt) "on" "off")))
               erl-trace-option-keys "  ")))
    (format "vsn:%s  mode:%s  prefix:%s  stored:%s  %s"
            (or erl-trace-erlang-vsn "N/A")
            (or erl-trace-mode "nil")
            (or erl-trace-prefix "")
            (if erl-trace-stored "yes" "no")
            opts)))

(defun toggle-option-wrapper ()
  "Prompt user with current trace option values, and toggle selected one."
  (let* ((display-items
          (mapcar (lambda (opt)
                    (let ((val (plist-get erl-trace-options opt)))
                      (cons (format "%s (%s)"
                                    (substring (symbol-name opt) 1)
                                    (if val "on" "off"))
                            opt)))
                  erl-trace-option-keys))
         (choice (completing-read "Toggle trace option: " (mapcar #'car display-items)))
         (key (cdr (assoc choice display-items))))
    (erl-trace-toggle-option key)))

(defun erl-trace-toggle-option (key)
  "Toggle a boolean option in `erl-trace-options`. KEY must be a keyword."
  (when (memq key erl-trace-option-keys)
    (let ((new-val (not (plist-get erl-trace-options key))))
      (setq erl-trace-options (plist-put erl-trace-options key new-val))
      (message "Set %s: %s" key new-val))))

(defun erl-trace-show-options ()
  "Show current values of all trace options."
  (message "Current erl-trace-options: %s" erl-trace-options))

(defun erl-trace-key-bindings ()
  (global-set-key (kbd "C-c C-t") 'erl-trace-insert)
  (global-set-key (kbd "C-c C-e") 'erl-trace-run-cmd)
  (global-set-key (kbd "C-c C-r") 'erl-trace-store)
  )

(defun erl-trace-show-info ()
  "Show current trace configuration in minibuffer."
  (let ((summary
         (mapconcat (lambda (opt)
                      (format "%s:%s"
                              (substring (symbol-name opt) 1)
                              (if (plist-get erl-trace-options opt) "on" "off")))
                    erl-trace-option-keys "  ")))
    (message "Trace options → %s" summary)))

(defun erl-trace-set-mode ()
  "Interactively set the value of `erl-trace-mode`.

This controls how trace lines are inserted in Erlang code.
Available modes include: \"debug\", \"info\", \"error\", \"io\", \"ct\", and \"macro\".

See the variable `erl-trace-mode` for more details on what each mode does."
  (interactive)
  (let* ((choices '("debug" "info" "error" "io" "ct" "macro"))
         (tracemode (completing-read "Mode: " choices))
         (mode-symbol (intern tracemode)))
    (when (member tracemode choices)
      (setq erl-trace-mode mode-symbol))))

(defun erl-trace-set-prefix ()
  (interactive)
  (setq user-input (read-string "Prefix: "))
  (setq erl-trace-prefix
        (if (equal user-input "user")
            (getenv "USER") user-input))
  (erl-trace-debug-msg "trace-set-prefix is set to %s" erl-trace-prefix)
  )

(defun erl-trace-stacktrace ()
  "Adding stacktrace at point."
  (erl-trace-maybe-insert-iotrace-macro)
  (erl-trace-maybe-insert-supfun)
  (let ((start-point (point)))
    (newline)
    (if (< erl-trace-erlang-vsn 22)
        (insert erl-trace-stacktrace)
      (insert erl-trace-stacktrace-otp-22))
    (save-excursion
      (goto-char start-point)
      (erl-trace-goto-marked "-!-")
      (erl-trace-insert))
    (indent-region start-point (point))))

(defun erl-trace-store ()
  (interactive)
  (when (equal (erl-trace-what-at-point) 'variable)
    (let* ((var (thing-at-point 'symbol))
           (nstored (append erl-trace-stored (list var))))
      (setq erl-trace-stored nstored)
      (message "Stored: %s" erl-trace-stored))))

(defun erl-trace-insert ()
  "Insert an Erlang trace statement based on the current context."
  (interactive)
  (erl-trace-maybe-insert-iotrace-macro)
  (erl-trace-maybe-insert-supfun)
  (let ((what (erl-trace-what-at-point)))
    (cl-case what
      (atom
       (erl-trace-debug-msg "Insert atom...")
       (erl-trace-insert-string (erl-trace-build-string 'atom)))

      (variable
       (erl-trace-debug-msg "Insert variable...")
       (let ((str (erl-trace-build-string 'variable)))
         (erl-trace-debug-msg "String: %s" str)
         (erl-trace-insert-string str)))

      (nothing
       (if erl-trace-stored
           (progn
             (erl-trace-debug-msg "Insert stored...")
             (erl-trace-insert-string (erl-trace-build-string 'stored)))
         (progn
           (erl-trace-debug-msg "Insert nothing...")
           (erl-trace-insert-string (erl-trace-build-string 'nothing))
           (erl-trace-goto-marked "-!-"))))

      (t
       (erl-trace-debug-msg "Unknown context for trace insert: %s" what)))))

(defun erl-trace-maybe-insert-supfun ()
  "Check whether we need to insert macro or support functions."
  (if (and (string-match ".*.erl" (buffer-file-name))
           (equal erl-trace-mode 'macro))
      (progn
        (erl-trace-erlang-version)
        (save-excursion
          (when (erl-trace-need-macro) (erl-trace-insert-macros))
          (when (erl-trace-need-supfun) (erl-trace-insert-supfun))
          ))))

(defun erl-trace-insert-supfun-macro ()
  "Insert erl-trace support functions and macros"
  (interactive)
  (save-excursion (erl-trace-insert-supfun)
                  (erl-trace-insert-macros)))

(defun erl-trace-insert-supfun ()
  (erl-trace-debug-msg "Insert %s..." "support function")
  (goto-char (point-max))
  (insert "\n")
  (let ((support-data-strings
         (list erl-trace-timestamp
               erl-trace-get-user-name
               erl-trace-procinfo
               erl-trace-function)))
    (dolist (str support-data-strings)
      (erl-trace-insert-string str t)
      (insert "\n"))))

(defun erl-trace-insert-macros ()
  (erl-trace-debug-msg "Insert %s..." "macro")
  (goto-char (point-min))
  (erl-trace-insert-string erl-trace-macro t))

(defun erl-trace-maybe-insert-iotrace-macro ()
  "Check whether we need to insert macro for explicit io:format."
  (save-excursion
    (when (and (erl-trace-no-iotrace-macro)
               (string-match ".*.erl" (buffer-file-name))
               (equal erl-trace-mode 'macro))
      (erl-trace-debug-msg "Insert %s..." "erlang trace macro")
      (goto-char (point-min))
      (erl-trace-insert-string erl-trace-iotrace-macro t))))

(defun erl-trace-maybe-delete-iotrace-macro ()
  "Check whether we need to delete macro for explicit io:format."
  (save-excursion
    (when (erl-trace-no-iotrace-macro)
      (erl-trace-debug-msg "Delete %s..." "explicit")
      (goto-char (point-min))
      )))

(defun erl-trace-erlang-version ()
  "Get and cache Erlang/OTP version."
  (if erl-trace-erlang-vsn erl-trace-erlang-vsn
    (let ((vsn (erl-trace-get-erlang-version)))
      (erl-trace-debug-msg "Erlang Version: %s" vsn)
      (setq erl-trace-erlang-vsn vsn))))

(defun erl-trace-get-erlang-version ()
  (let ((raw (shell-command-to-string erl-trace-release)))
    (string-match "\\\"\\(.*\\)\\\"" raw)
    (string-to-number (match-string 1 raw))))

(defun erl-trace-need-macro ()
  (when (< erl-trace-erlang-vsn 19)
    (goto-char (point-min))
    (if (condition-case
            nil (search-forward "-ifndef(FUNC).")
          (error nil))
        nil t)))

(defun erl-trace-no-iotrace-macro ()
  (goto-char (point-min))
  (if (condition-case
          nil (search-forward "IO_TRACE_DEBUG")
        (error nil))
      nil t))

(defun erl-trace-need-supfun ()
  (goto-char (point-max))
  (if (condition-case
          nil (search-backward "erl-trace support functions")
        (error nil))
      nil t))

(defun erl-trace-build-string (type)
  "Generate a formatted Erlang trace string based on TYPE.
Supports 'nothing, 'atom, 'variable, 'stored.
 - 'nothing: insert a simple trace marker
 - 'atom: insert the current symbol name as a tag
 - 'variable: format the current variable with its value
 - 'stored: use the stored list of variables"
  (cl-case type
    (nothing
     (erl-trace-concat " -!-" ""))

    (atom
     (let* ((atom (thing-at-point 'symbol))
            (fmt (concat atom ""))
            (args ""))
       (erl-trace-concat fmt args)))

    (variable
     (let* ((var (thing-at-point 'symbol))
            (fmt (concat var erl-trace-value-fmt))
            (args var))
       (erl-trace-concat fmt args)))

    (stored
     (let* ((vars (delete-dups erl-trace-stored))
            (fmt (erl-trace-format-multi-vars vars))
            (args (mapconcat #'identity vars ", ")))
       (setq erl-trace-stored '())
       (erl-trace-concat fmt args)))

    (t
     (error "Unsupported trace type: %s" type))))

(defun erl-trace-format-multi-vars (vars)
  "Format a list of VARS with ~p per line, optionally starting with ~n."
  (let* ((newline (plist-get erl-trace-options :newline))
         (fmt-list
          (mapcar (lambda (var) (concat var erl-trace-value-fmt)) vars))
         )
    (mapconcat #'identity fmt-list (if newline "~n" " "))))

(defun erl-trace-concat (fmt args)
  (if (equal erl-trace-mode 'macro)
      (erl-trace-concat-macro fmt args)
    (erl-trace-concat-normal fmt args)
    )
  )

(defun erl-trace-concat-normal (fmt args)
  "Generate an Erlang trace log line with optional detail and location."
  (erl-trace-debug-msg "fmt: %s args: %s" fmt args)
  (let* (
         (printf (pcase erl-trace-mode
                   ('debug "?LOG_DEBUG")
                   ('error "?LOG_ERROR")
                   ('info  "?LOG_INFO")
                   ('ct    "ct:pal")
                   (_      "io:format")))
         (detail (plist-get erl-trace-options :detail))
         (location (plist-get erl-trace-options :location))
         (fmt-prefix (string-join (append
                                   (list erl-trace-prefix)
                                   (when detail '("~p:~p"))
                                   (when location '("~p:~p:~p")))
                                  " "))
         (args-prefix
          (append
           (when detail '("erl_trace_process_info()" "erl_trace_timestamp()"))
           (when location '("?MODULE" "?FUNCTION_NAME" "?LINE"))))
         (argslist (string-trim args))
         (final-args
          (string-join
           (append args-prefix (unless (string-empty-p argslist) (list argslist)))
           ", ")
          )
         )
    (format "%s(\"%s %s\", [%s]),"
            printf
            (concat (if (equal printf "io:format") "~n" "") fmt-prefix)
            fmt
            final-args)))

(defun erl-trace-concat-macro (fmt args)
  (let* (
         (detail (plist-get erl-trace-options :detail))
         (prefix
          (cond
           ((and (equal erl-trace-mode 'macro) detail ) "?iotd")
           ((and (equal erl-trace-mode 'macro) detail) "?iotdd")
           ((and (equal erl-trace-mode 'ctpal) detail) "?cttd")
           ((and (equal erl-trace-mode 'ctpal) detail) "?cttdd")
           ("io:format")
           ))
         (argslist (if (equal args "") "" (concat ", [" args "]")))
         (endfmt (if (or (equal erl-trace-mode 'ct)
                         (string-match ".*_SUITE.erl" (buffer-file-name)))
                     "" "~n")))
    (concat prefix "(\"" fmt endfmt "\"" argslist "),"))
  )

(defun erl-trace-fmt-vars (vars acc)
  (if (cdr vars)
      (let* ((head (car vars))
             (tail (cdr vars))
             (nacc (concat acc head erl-trace-value-fmt)))
        (erl-trace-fmt-vars tail nacc))
    (concat acc (car vars) erl-trace-value-fmt)))

(defun erl-trace-args-vars (vars acc)
  (message "var%s acc: %s" vars acc)
  (if (cdr vars)
      (let* ((head (car vars))
             (tail (cdr vars))
             (nacc (if (equal erl-trace-mode 'io) (concat acc ", " head) (concat acc head ", "))))
        (erl-trace-args-vars tail nacc))
    (if (equal erl-trace-mode 'io) (concat acc ", " (car vars)) (concat acc (car vars))))
  )

(defun erl-trace-insert-string (string &optional auto)
  (erl-trace-debug-msg "Insert %s" string)
  (save-excursion
    (when (not auto) (erl-trace-goto-eof-header-if-needed))
    (let ((start-point (point))
          (at-bol (bolp)))
      ;; If we are not standing at beginning-of-line
      (unless at-bol (progn (end-of-line) (newline)))
      (insert string)
      (when at-bol (newline))
      (indent-region start-point (point)))))

(defun erl-trace-goto-marked (string)
  (when (condition-case nil (search-forward string) (error nil))
    (delete-char (length string))))

(defun erl-trace-what-at-point ()
  "Returns `variable' `atom' or `nothing' at cursor."
  (let ((thing (thing-at-point 'symbol))
        (case-fold-search nil))
    (if thing
        (if (equal (string-match-p erlang-variable-regexp thing) 0)
            'variable 'atom)
      'nothing)))

(defun erl-trace-func-header-p ()
  (save-excursion
    (let ((current (point))
          (begin (progn (erl-trace-boc-safe) (point)))
          (end (search-forward "->")))
      (if (and (<= begin current) (< current end)) t nil))))

(defun erl-trace-boc-safe ()
  ;; We are not stand at beginning of function clause,
  ;; Lets move to it.
  (unless (erlang-get-function-name) (erlang-beginning-of-clause)))

(defun erl-trace-goto-eof-header-if-needed ()
  ;; We are standing in the middle of function header,
  ;; Lets move to end of function header.
  (when (erl-trace-func-header-p) (search-forward "->")))

;; -----------------------------------------------------------------------------
;; CLAUSE
;; -----------------------------------------------------------------------------
;; Increase the number of lisp bindings.
(setq max-specpdl-size 13000)
(defun erl-trace-clause ()
  (save-excursion
    (erl-trace-maybe-insert-iotrace-macro)
    (erl-trace-maybe-insert-supfun)
    (erlang-end-of-function)
    (let ((stop-point (point)))
      (erlang-beginning-of-function)
      (let ((start-point (point)))
        (erl-trace-clause-loop stop-point 1)
        (indent-region start-point (point))))))

(defun erl-trace-clause-loop (stop number)
  (search-forward "->")
  (let ((current (point)))
    (if (nth 4 (syntax-ppss))
        (erl-trace-clause-loop stop number)
      (let* ((fmt (concat "case: " (number-to-string number)))
             (str (erl-trace-concat fmt "")))
        (when (< current stop)
          (newline-and-indent)
          (insert str)
          (erl-trace-clause-loop (+ (- (point) current) stop) (+ number 1)))))))

(defvar erl-trace-debug nil
  "Control whether printing debug messages.")

(defun erl-trace-debug-msg (format &rest args)
  "Debug message."
  (when erl-trace-debug
    (apply 'message (cons format args))))


(provide 'erl-trace)
;;; erl-trace.el ends here
