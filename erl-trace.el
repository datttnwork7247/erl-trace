;;; erl-trace.el --- a simple package                     -*- lexical-binding: t; -*-
;; Package-Version: 20211028.1412

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

(defvar erl-trace-erlang-vsn 23)
(defvar erl-trace-mode 'debug)
(defvar erl-trace-level 'simple)
(defvar erl-trace-newline-begin 'false)
(defvar erl-trace-newline-split 't)
(defvar erl-trace-prefix '())
(defvar erl-trace-stored '())

;; -----------------------------------------------------------------------------
;; SHOW-INFO
;; -----------------------------------------------------------------------------
(defun erl-trace-show-info ()
  (message "Version=%s Mode=%s Level=%s Prefix=%s Stored=%s"
           erl-trace-erlang-vsn erl-trace-mode erl-trace-level
           erl-trace-prefix erl-trace-stored)
  )

;; -----------------------------------------------------------------------------
;; LEVEL-TOGGLE
;; -----------------------------------------------------------------------------
(defun erl-trace-level-toggle ()
  "Toggle erl-trace between `simple' and `detail'."
  (if (equal erl-trace-level 'simple)
      (setq erl-trace-level 'detail)
    (setq erl-trace-level 'simple)))

;; -----------------------------------------------------------------------------
;; CT-PAL-TOOGLE
;; -----------------------------------------------------------------------------
;; (defun erl-trace-ct-pal-toggle ()
;;   "Toggle erl-trace between `io:format' and `ct:pal'."
;;   (if (equal erl-trace-ct-pal 'false)
;;       (setq erl-trace-ct-pal 'true)
;;     (setq erl-trace-ct-pal 'false)))

;; -----------------------------------------------------------------------------
;; SET-MODE
;; -----------------------------------------------------------------------------
(defun erl-trace-set-mode ()
  (interactive)
  (let ((tracemode (completing-read "Mode:" '("debug"
                                               "info"
                                               "error"
                                               "io"
                                               "ct"
                                               "macro"))))
    (cond ((equal tracemode "debug") (setq erl-trace-mode 'debug))
          ((equal tracemode "error") (setq erl-trace-mode 'error))
          ((equal tracemode "info") (setq erl-trace-mode 'info))
          ((equal tracemode "io") (setq erl-trace-mode 'io))
          ((equal tracemode "ct") (setq erl-trace-mode 'ct))
          ((equal tracemode "macro") (setq erl-trace-mode 'macro))
          (t 'io)
          )
    )
  )

(defun erl-trace-set-prefix ()
  (interactive)
  (setq user-input (read-string "Prefix: "))
  (setq erl-trace-prefix user-input)
  )


(defun erlang-get-current-function ()
  "Get the name of the current Erlang function."
  (interactive)
  (require 'erlang)
  (save-excursion
    (erlang-beginning-of-function)
    (erlang-get-function-name)))

;; -----------------------------------------------------------------------------
;; ADD STACKTRACE
;; -----------------------------------------------------------------------------
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

;; -----------------------------------------------------------------------------
;; STORE
;; -----------------------------------------------------------------------------
(defun erl-trace-store ()
  (interactive)
  (when (equal (erl-trace-what-at-point) 'variable)
    (let* ((var (thing-at-point 'symbol))
           (nstored (append erl-trace-stored (list var))))
      (setq erl-trace-stored nstored)
      (message "Stored: %s" erl-trace-stored))))

;; -----------------------------------------------------------------------------
;; INSERT
;; -----------------------------------------------------------------------------
(defun erl-trace-insert ()
  (interactive)
  (erl-trace-maybe-insert-iotrace-macro)
  (erl-trace-maybe-insert-supfun)
  (cond ((equal (erl-trace-what-at-point) 'atom)
         (erl-trace-debug-msg "Insert %s..." "atom")
         (erl-trace-insert-string (erl-trace-build-string 'atom)))
        ((equal (erl-trace-what-at-point) 'variable)
         (erl-trace-debug-msg "Insert %s..." "variable")
         (let ((string (erl-trace-build-string 'variable)))
           (erl-trace-debug-msg "String %s" string))
         (erl-trace-insert-string (erl-trace-build-string 'variable)))
        ((and (equal (erl-trace-what-at-point) 'nothing)
              erl-trace-stored)
         (erl-trace-debug-msg "Insert %s..." "stored")
         (erl-trace-insert-string (erl-trace-build-string 'stored)))
        ((and (equal (erl-trace-what-at-point) 'nothing)
              (equal erl-trace-stored nil))
         (erl-trace-debug-msg "Insert %s..." "nothing")
         (erl-trace-insert-string (erl-trace-build-string 'nothing))
         (erl-trace-goto-marked "-!-"))))

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
  (end-of-buffer)
  (newline)
  (erl-trace-insert-string erl-trace-timestamp t)
  (goto-char (- (point) 1))
  (newline)
  (erl-trace-insert-string erl-trace-get-user-name t)
  (goto-char (- (point) 1))
  (newline)
  (erl-trace-insert-string erl-trace-procinfo t)
  (goto-char (- (point) 1))
  (newline)
  (erl-trace-insert-string erl-trace-function t)
  )

(defun erl-trace-insert-macros ()
  (erl-trace-debug-msg "Insert %s..." "macro")
  (beginning-of-buffer)
  (erl-trace-insert-string erl-trace-macro t))

(defun erl-trace-maybe-insert-iotrace-macro ()
  "Check whether we need to insert macro for explicit io:format."
  (save-excursion
    (when (and (erl-trace-no-iotrace-macro)
               (string-match ".*.erl" (buffer-file-name))
               (equal erl-trace-mode 'macro))
      (erl-trace-debug-msg "Insert %s..." "erlang trace macro")
      (beginning-of-buffer)
      (erl-trace-insert-string erl-trace-iotrace-macro t))))

(defun erl-trace-maybe-delete-iotrace-macro ()
  "Check whether we need to delete macro for explicit io:format."
  (save-excursion
    (when (erl-trace-no-iotrace-macro)
      (erl-trace-debug-msg "Delete %s..." "explicit")
      (beginning-of-buffer)
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
    (beginning-of-buffer)
    (if (condition-case
            nil (search-forward "-ifndef(FUNC).")
          (error nil))
        nil t)))

(defun erl-trace-no-iotrace-macro ()
  (beginning-of-buffer)
  (if (condition-case
          nil (search-forward "IO_TRACE_DEBUG")
        (error nil))
      nil t))

(defun erl-trace-need-supfun ()
  (end-of-buffer)
  (if (condition-case
          nil (search-backward "erl-trace support functions")
        (error nil))
      nil t))

(defun erl-trace-build-string (type)
  (cond ((equal type 'nothing)
         (let* ((fmt " -!-")
                (args ""))
           (erl-trace-concat fmt args)))
        ((equal type 'atom)
         (let* ((atom (thing-at-point 'symbol))
                (fmt (concat atom ": "))
                (args ""))
           (erl-trace-concat fmt args)))
        ((equal type 'variable)
         (let* ((var (thing-at-point 'symbol))
                (fmt (if (or (string-match-p "IKeypath" var)
                             (string-match-p "IKP" var))
                         (concat " -=>" var ": ~0p")
                       (if (equal erl-trace-newline-begin 'true)
                           (concat "~n" var " = ~p")
                         (concat var " = ~p"))
                       ))
                (args  var))
           (erl-trace-concat fmt args)))
        ((equal type 'variable)
         (let* ((var (thing-at-point 'symbol))
                (fmt (concat "~n" var " = ~p "))
                (args var))
           (erl-trace-concat fmt args)))
        ((equal type 'stored)
         (let* ((vars (delete-dups erl-trace-stored))
                (fmt (erl-trace-fmt-vars vars ""))
                (args (erl-trace-args-vars vars "")))
           (setq erl-trace-stored '())
           (erl-trace-concat fmt args)))))

(defun erl-trace-concat (fmt args)
  (if (equal erl-trace-mode 'macro) (erl-trace-concat-macro fmt args)
    (erl-trace-concat-normal fmt args)
    )
  )

(defun erl-trace-concat-normal (fmt args)
  (let ((user (getenv "USER"))
        (prefix (if (equal erl-trace-prefix '"") (erlang-get-current-function) erl-trace-prefix))
        (func (if (< erl-trace-erlang-vsn 19) "?FUNC" "?FUNCTION_NAME"))
        (printf (cond ((equal erl-trace-mode 'debug) "?LOG_DEBUG")
                      ((equal erl-trace-mode 'error) "?LOG_ERROR")
                      ((equal erl-trace-mode 'info) "?LOG_INFO")
                      ((equal erl-trace-mode 'ct) "ct:pal")
                      (t "io:format")
                ))
        (endfmt (if (or (equal erl-trace-mode 'ct)
                        (string-match ".*_SUITE.erl" (buffer-file-name)))
                    "" ""))
        (argslist (if (equal args "") ""
                    (concat ", [" args "]")))
        )
    (if (equal erl-trace-level 'detail)
        (concat printf "(\"~p:" user ":~s:~p:~p:~p:" fmt endfmt "\",\n"
                "[erl_trace_process_info(), erl_trace_timestamp(),\n?MODULE, "
                func ", ?LINE" args "]),")
      (concat printf "(\"" prefix ": " fmt endfmt "\"" argslist "),")
      )
    ))

(defun erl-trace-concat-macro (fmt args)
  (let ((prefix
         (cond
          ;; iotd
          ((and (equal erl-trace-mode 'macro)
                (equal erl-trace-level 'simple)
                )
           "?iotd")
          ((and (equal erl-trace-mode 'macro)
                (equal erl-trace-level 'detail))
           "?iotdd")

          ;; ctpal
          ((and (equal erl-trace-mode 'ctpal)
                (equal erl-trace-level 'simple))
           "?cttd")
          ((and (equal erl-trace-mode 'ctpal)
                (equal erl-trace-level 'detail))
           "?cttdd")

          ("io:format")
          ))
        (argslist (if (equal args "") ""
                    (concat ", [" args "]")))
        (endfmt (if (or (equal erl-trace-mode 'ct)
                        (string-match ".*_SUITE.erl" (buffer-file-name)))
                    "" "~n")))
    (concat prefix "(\"" fmt endfmt "\"" argslist "),"))
  )

(defun erl-trace-fmt-vars (vars acc)
  (let ((newline-split (if (boundp 'erl-trace-newline-split) erl-trace-newline-split t)))
    (message "erl-trace-newline-split: %s" erl-trace-newline-split)
    (if (cdr vars)
        (let* ((head (car vars))
               (tail (cdr vars))
               (nacc (concat acc head " = ~p" (if (eq erl-trace-newline-split t) "~n" ""))))
          (erl-trace-fmt-vars tail nacc))
      (concat acc (car vars) " = ~p"))))

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
    (delete-backward-char (length string))))

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

;; -----------------------------------------------------------------------------
;; RUN-COMMAND
;; -----------------------------------------------------------------------------
(defun erl-trace-run-cmd ()
  (interactive)
  (let ((command (completing-read "Run erl-trace: " '("erl-trace-set-mode"
                                                      "erl-trace-info"
                                                      "erl-trace-set-prefix"
                                                      "erl-trace-level"
                                                      "erl-trace-clause"
                                                      "erl-trace-stacktrace"))))
    (cond
     ((equal command "erl-trace-set-mode")       (erl-trace-set-mode))
     ((equal command "erl-trace-info")       (erl-trace-show-info))
     ((equal command "erl-trace-level")      (erl-trace-level-toggle))
     ((equal command "erl-trace-clause")     (erl-trace-clause))
     ((equal command "erl-trace-stacktrace") (erl-trace-stacktrace))
     ((equal command "erl-trace-set-prefix") (erl-trace-set-prefix))
     )))

;; -----------------------------------------------------------------------------
;; RUN-COMMAND
;; -----------------------------------------------------------------------------
(defun erl-trace-key-bindings ()
  (global-set-key (kbd "C-c C-t") 'erl-trace-insert)
  (global-set-key (kbd "C-c C-e") 'erl-trace-run-cmd)
  (global-set-key (kbd "C-c C-r") 'erl-trace-store)
  )
(erl-trace-key-bindings)

;; -----------------------------------------------------------------------------
;; DEBUG-MSG
;; -----------------------------------------------------------------------------
(defvar erl-trace-debug nil
  "Control whether printing debug messages.")

(defun erl-trace-debug-msg (format &rest args)
  "Debug message."
  (when erl-trace-debug (apply 'message (add-to-list 'args format))))

(provide 'erl-trace)

;;; erl-trace.el ends here
