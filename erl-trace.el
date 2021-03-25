;;; erl-trace.el --- a simple package                     -*- lexical-binding: t; -*-

;; Copyright (C) 2021

;; Author: MrX <abc@xyz>
;; Keywords: erl-trace
;; Version: 1.0

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

;;; Commentary:
;; Put a description of the package here
;; Automatically insert io:format to:
;; - Print out a atom or value of a parameter.[C-c C-t]
;; - Store parameters for print out [C-c C-r]
;; - Put clauses trace after `->' characters [C-e erl-trace-clause]
;; - Put stack trace [C-e erl-trace-stack]
;; - And more

(defconst erl-trace-macro
  "-ifndef(FUNC).
-define(FUNC, element(2, element(2, process_info(self(), current_function)))).
-endif.
")

(defconst erl-trace-iotrace-debug-macro
  "-define(IO_TRACE_DEBUG, true).
-if(?IO_TRACE_DEBUG).
-define(iotd(Fmt), io:format(\"~p:~p:~p \"++Fmt++\"~n\", [?MODULE, ?FUNCTION_NAME, ?LINE])).
-define(iotd(Fmt, Args), io:format(\"~p:~p:~p \"++Fmt++\"~n\", [?MODULE, ?FUNCTION_NAME, ?LINE] ++ Args)).
-define(iotdd(Fmt, Args),
        io:format(\"~p:~p:~s:~p:~p:~p: \"++Fmt++\"~n\",
                  [erl_trace_process_info(),
                   erl_trace_get_user(),
                   erl_trace_timestamp(),
                   ?MODULE, ?FUNCTION_NAME, ?LINE] ++ Args)).
-elif(true).
-define(iotd(_Fmt), ok).
-define(iotd(_Fmt, _Args), ok).
-define(iotdd(_Fmt, _Args), ok).
-endif.
")

(defconst erl-trace-timestamp
  "erl_trace_timestamp() ->
    {MS, S, US} = os:timestamp(),
    {_, {Hour, Min, Sec}} = calendar:now_to_local_time({MS, S, US}),
    MSec = trunc(US/1000),
    lists:flatten(io_lib:format(\"~2.10.0B:~2.10.0B:~2.10.0B.~3.10.0B\",
                                [Hour, Min, Sec, MSec])).")

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

(defvar erl-trace-erlang-vsn nil)
(defvar erl-trace-level 'simple)
(defvar erl-trace-ct-pal 'false)
(defvar erl-trace-explicit 'false)
(defvar erl-trace-stored '())

;; -----------------------------------------------------------------------------
;; SHOW-INFO
;; -----------------------------------------------------------------------------
(defun erl-trace-show-info ()
  ;; Show erl-trace parameters
  ;; (message "Erlang version: %s" erl-trace-erlang-vsn)
  (message "Erlang version: %s\nLevel: %s\nExplicit: %s\nCtpal: %s\nStored: %s"
           erl-trace-erlang-vsn erl-trace-level erl-trace-explicit erl-trace-ct-pal
           erl-trace-stored)
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
(defun erl-trace-ct-pal-toggle ()
  "Toggle erl-trace between `io:format' and `ct:pal'."
  (if (equal erl-trace-ct-pal 'false)
      (setq erl-trace-ct-pal 'true)
    (setq erl-trace-ct-pal 'false)))

;; -----------------------------------------------------------------------------
;; EXPLICIT-TOOGLE
;; -----------------------------------------------------------------------------
(defun erl-trace-explicit-toggle ()
  "Toggle erl-trace between long and short (with macro) format."
  (if (equal erl-trace-explicit 'true)
      (progn
        (setq erl-trace-explicit 'false)
        (erl-trace-maybe-insert-iotrace-macro)
        )
    (progn
      (setq erl-trace-explicit 'true)
      (erl-trace-maybe-delete-iotrace-macro))
    )
  )

;; -----------------------------------------------------------------------------
;; ADD STACKTRACE
;; -----------------------------------------------------------------------------
(defun erl-trace-stacktrace ()
  "Adding stacktrace at point."
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
  (erl-trace-maybe-insert-supfun)
  (erl-trace-maybe-insert-iotrace-macro)
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
  (erl-trace-erlang-version)
  (save-excursion
    (when (erl-trace-need-macro)
      (erl-trace-debug-msg "Insert %s..." "macro")
      (beginning-of-buffer)
      (erl-trace-insert-string erl-trace-macro t))
    (when (erl-trace-need-supfun)
      (erl-trace-debug-msg "Insert %s..." "support function")
      (end-of-buffer)
      (newline)
      (erl-trace-insert-string erl-trace-timestamp t)
      (goto-char (- (point) 1))
      (newline)
      (erl-trace-insert-string erl-trace-get-user-name t)
      (goto-char (- (point) 1))
      (newline)
      (erl-trace-insert-string erl-trace-procinfo t))))

(defun erl-trace-maybe-insert-iotrace-macro ()
  "Check whether we need to insert macro for explicit io:format."
  (save-excursion
    (when (erl-trace-no-iotrace-macro)
      (erl-trace-debug-msg "Insert %s..." "explicit")
      (beginning-of-buffer)
      (erl-trace-insert-string erl-trace-iotrace-debug-macro t))))

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
  (when (equal erl-trace-level 'detail)
    (end-of-buffer)
    (if (condition-case
            nil (search-backward "erl_trace_timestamp() ->")
          (error nil))
        nil t)))

(defun erl-trace-build-string (type)
  (cond ((equal type 'nothing)
         (let* ((fmt " -!-")
                (args ""))
           (erl-trace-concat fmt args)))
        ((equal type 'atom)
         (let* ((atom (thing-at-point 'symbol))
                (fmt (concat " " atom))
                (args ""))
           (erl-trace-concat fmt args)))
        ((equal type 'variable)
         (let* ((var (thing-at-point 'symbol))
                (fmt (if (or (string-match-p "IKeypath" var)
                             (string-match-p "IKP" var))
                         (concat " -->" var ": ~999p")
                       (concat "~n" var ": ~p")))
                (args (if (equal erl-trace-explicit 'false) var (concat ", " var))))
           (erl-trace-concat fmt args)))
        ((equal type 'variable)
         (let* ((var (thing-at-point 'symbol))
                (fmt (concat "~n" var ": ~p"))
                (args (if (equal erl-trace-explicit 'false) var (concat ", " var))))
           (erl-trace-concat fmt args)))
        ((equal type 'stored)
         (let* ((vars (delete-dups erl-trace-stored))
                (fmt (erl-trace-fmt-vars vars ""))
                (args (erl-trace-args-vars vars "")))
           (setq erl-trace-stored '())
           (erl-trace-concat fmt args)))))

(defun erl-trace-concat (fmt args)
  (if (equal erl-trace-explicit 'true)
      (erl-trace-concat-explicit fmt args)
    (erl-trace-concat-use-macro fmt args))
  )

(defun erl-trace-concat-explicit (fmt args)
  (let ((user (getenv "USER"))
        (func (if (< erl-trace-erlang-vsn 19) "?FUNC" "?FUNCTION_NAME"))
        (printf (if (or (equal erl-trace-ct-pal 'true)
                        (string-match ".*_SUITE.erl" (buffer-file-name)))
                    "ct:pal" "io:format"))
        (endfmt (if (or (equal erl-trace-ct-pal 'true)
                        (string-match ".*_SUITE.erl" (buffer-file-name)))
                    "" "~n")))
    (if (equal erl-trace-level 'detail)
        (concat printf "(\"~p:" user ":~s:~p:~p:~p:" fmt endfmt "\",\n"
                "[erl-trace_process_info(), erl-trace_timestamp(),\n?MODULE, "
                func ", ?LINE" args "]),")
      (concat printf "(\"~p:~p:~p:" fmt endfmt "\", [?MODULE, " func ", ?LINE"
              args "]),"))))

(defun erl-trace-concat-use-macro (fmt args)
  (let ((prefix (if (equal erl-trace-level 'detail) "?iotdd" "?iotd"))
        (argslist (if (equal args "") ""
                    (concat ", [" args "]")))
        (endfmt (if (or (equal erl-trace-ct-pal 'true)
                        (string-match ".*_SUITE.erl" (buffer-file-name)))
                    "" "~n")))
    (concat prefix "(\"" fmt endfmt "\"" argslist "),"))
  )

(defun erl-trace-fmt-vars (vars acc)
  (if (cdr vars)
      (let* ((head (car vars))
             (tail (cdr vars))
             (nacc (concat acc head ": ~p~n")))
        (erl-trace-fmt-vars tail nacc))
    (concat "~n" acc (car vars) ": ~p")))

(defun erl-trace-args-vars (vars acc)
  (message "var%s acc: %s" vars acc)
  (if (cdr vars)
      (let* ((head (car vars))
             (tail (cdr vars))
             (nacc (if (equal erl-trace-explicit 'false)
                       (concat acc head ", ")
                     (concat acc ", " head))))
        (erl-trace-args-vars tail nacc))
    (if (equal erl-trace-explicit 'false)
        (concat acc (car vars))
      (concat acc ", " (car vars)))
    ))

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
  (let ((command (completing-read "Run erl-trace: " '("erl-trace-info"
                                                      "erl-trace-level"
                                                      "erl-trace-ctpal"
                                                      "erl-trace-explicit"
                                                      "erl-trace-clause"
                                                      "erl-trace-stacktrace"))))
    (cond ((equal command "erl-trace-info")       (erl-trace-show-info))
          ((equal command "erl-trace-level")      (erl-trace-level-toggle))
          ((equal command "erl-trace-ctpal")      (erl-trace-ct-pal-toggle))
          ((equal command "erl-trace-explicit")    (erl-trace-explicit-toggle))
          ((equal command "erl-trace-clause")     (erl-trace-clause))
          ((equal command "erl-trace-stacktrace") (erl-trace-stacktrace))
          )))

;; -----------------------------------------------------------------------------
;; DEBUG-MSG
;; -----------------------------------------------------------------------------
(defvar erl-trace-debug nil
  "Control whether printing debug messages.")

(defun erl-trace-debug-msg (format &rest args)
  "Debug message."
  (when erl-trace-debug (apply 'message (add-to-list 'args format))))

(provide 'erl-trace)
