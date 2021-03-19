;; TODO
;;
;; 1. erltrace-clause skipped commented code.                        DONE
;; 2. erltrace-clause on detailed-level was not indented.            DONE
;; 3. Add erltrace-stacktrace.                                       DONE
;; 4. Replace io:format with ct:pal in .*_SUITE.erl.                 DONE
;; 5. When in ct:pal mode, does not add ~n to the end of fmt string. DONE
;; 6. Add debug print for C and Python.
;;      fprintf( stdout, "%s:%d\n", __func__, __LINE__ );            -
;; 7. Make erltrace-stacktrace work on OTP.22.                       DONE
;; 8. Auto insert to end of function header.                         DONE
;; 9. Do not treat macro as variable                                 -

(defconst erltrace-macro
  "-ifndef(FUNC).
-define(FUNC, element(2, element(2, process_info(self(), current_function)))).
-endif.")

(defconst erltrace-timestamp
  "erltrace_timestamp() ->
    {MS, S, US} = os:timestamp(),
    {_, {Hour, Min, Sec}} = calendar:now_to_local_time({MS, S, US}),
    MSec = trunc(US/1000),
    lists:flatten(io_lib:format(\"~2.10.0B:~2.10.0B:~2.10.0B.~3.10.0B\",
                                [Hour, Min, Sec, MSec])).")

(defconst erltrace-procinfo
  "erltrace_process_info() ->
    proplists:get_value(registered_name, process_info(self()), self()).")

(defconst erltrace-release
  "erl -noshell -eval 'erlang:display(erlang:system_info(otp_release)), halt().'")

(defconst erltrace-stacktrace
  "try throw(exit)
catch _:_ ->
        ST-!- = erlang:get_stacktrace(),
        ok
end,")

(defconst erltrace-stacktrace-otp-22
  "try throw(exit)
catch _:_:ST-!- ->
        ok
end,")

(defvar erltrace-erlang-vsn nil)
(defvar erltrace-level 'simple)
(defvar erltrace-ct-pal 'false)
(defvar erltrace-stored '())

;; -----------------------------------------------------------------------------
;; LEVEL-TOGGLE
;; -----------------------------------------------------------------------------
(defun erltrace-level-toggle ()
  "Toggle erltrace between `simple' and `detail'."
  (if (equal erltrace-level 'simple)
      (setq erltrace-level 'detail)
    (setq erltrace-level 'simple)))

;; -----------------------------------------------------------------------------
;; CT-PAL-TOOGLE
;; -----------------------------------------------------------------------------
(defun erltrace-ct-pal-toggle ()
  "Toggle erltrace between `io:format' and `ct:pal'."
  (if (equal erltrace-ct-pal 'false)
      (setq erltrace-ct-pal 'true)
    (setq erltrace-ct-pal 'false)))

;; -----------------------------------------------------------------------------
;; ADD STACKTRACE
;; -----------------------------------------------------------------------------
(defun erltrace-stacktrace ()
  "Adding stacktrace at point."
  (let ((start-point (point)))
    (newline)
    (if (< erltrace-erlang-vsn 22)
        (insert erltrace-stacktrace)
      (insert erltrace-stacktrace-otp-22))
    (save-excursion
      (goto-char start-point)
      (erltrace-goto-marked "-!-")
      (erltrace-insert))
    (indent-region start-point (point))))

;; -----------------------------------------------------------------------------
;; STORE
;; -----------------------------------------------------------------------------
(defun erltrace-store ()
  (interactive)
  (when (equal (erltrace-what-at-point) 'variable)
    (let* ((var (thing-at-point 'symbol))
           (nstored (append erltrace-stored (list var))))
      (setq erltrace-stored nstored))))
(global-set-key (kbd "C-c C-r") 'erltrace-store)

;; -----------------------------------------------------------------------------
;; INSERT
;; -----------------------------------------------------------------------------
(defun erltrace-insert ()
  (interactive)
  (erltrace-maybe-insert-supfun)
  (cond ((equal (erltrace-what-at-point) 'atom)
         (plutil-debug-msg "Insert %s..." "atom")
         (erltrace-insert-string (erltrace-build-string 'atom)))
        ((equal (erltrace-what-at-point) 'variable)
         (plutil-debug-msg "Insert %s..." "variable")
         (let ((string (erltrace-build-string 'variable)))
           (plutil-debug-msg "String %s" string))
         (erltrace-insert-string (erltrace-build-string 'variable)))
        ((and (equal (erltrace-what-at-point) 'nothing)
              erltrace-stored)
         (plutil-debug-msg "Insert %s..." "stored")
         (erltrace-insert-string (erltrace-build-string 'stored)))
        ((and (equal (erltrace-what-at-point) 'nothing)
              (equal erltrace-stored nil))
         (plutil-debug-msg "Insert %s..." "nothing")
         (erltrace-insert-string (erltrace-build-string 'nothing))
         (erltrace-goto-marked "-!-"))))
(global-set-key (kbd "C-c C-t") 'erltrace-insert)

(defun erltrace-maybe-insert-supfun ()
  "Check whether we need to insert macro or support functions."
  (erltrace-erlang-version)
  (save-excursion
    (when (erltrace-need-macro)
      (plutil-debug-msg "Insert %s..." "macro")
      (beginning-of-buffer)
      (erltrace-insert-string erltrace-macro t))
    (when (erltrace-need-supfun)
      (plutil-debug-msg "Insert %s..." "support function")
      (end-of-buffer)
      (newline)
      (erltrace-insert-string erltrace-timestamp t)
      (goto-char (- (point) 1))
      (newline)
      (erltrace-insert-string erltrace-procinfo t))))

(defun erltrace-erlang-version ()
  "Get and cache Erlang/OTP version."
  (if erltrace-erlang-vsn erltrace-erlang-vsn
    (let ((vsn (erltrace-get-erlang-version)))
      (plutil-debug-msg "Erlang Version: %s" vsn)
      (setq erltrace-erlang-vsn vsn))))

(defun erltrace-get-erlang-version ()
  (let ((raw (shell-command-to-string erltrace-release)))
    (string-match "\\\"\\(.*\\)\\\"" raw)
    (string-to-number (match-string 1 raw))))

(defun erltrace-need-macro ()
  (when (< erltrace-erlang-vsn 19)
    (beginning-of-buffer)
    (if (condition-case
            nil (search-forward "-ifndef(FUNC).")
          (error nil))
        nil t)))

(defun erltrace-need-supfun ()
  (when (equal erltrace-level 'detail)
    (end-of-buffer)
    (if (condition-case
            nil (search-backward "erltrace_timestamp() ->")
          (error nil))
        nil t)))

(defun erltrace-build-string (type)
  (cond ((equal type 'nothing)
         (let* ((fmt " -!-")
                (args ""))
           (erltrace-concat fmt args)))
        ((equal type 'atom)
         (let* ((atom (thing-at-point 'symbol))
                (fmt (concat " " atom))
                (args ""))
           (erltrace-concat fmt args)))
        ((equal type 'variable)
         (let* ((var (thing-at-point 'symbol))
                (fmt (if (or (string-match-p "IKeypath" var)
                             (string-match-p "IKP" var))
                         (concat " " var ": ~999p")
                       (concat "~n" var ": ~p")))
                (args (concat ", " var)))
           (erltrace-concat fmt args)))
        ((equal type 'variable)
         (let* ((var (thing-at-point 'symbol))
                (fmt (concat "~n" var ": ~p"))
                (args (concat ", " var)))
           (erltrace-concat fmt args)))
        ((equal type 'stored)
         (let* ((vars (delete-dups erltrace-stored))
                (fmt (erltrace-fmt-vars vars ""))
                (args (erltrace-args-vars vars "")))
           (setq erltrace-stored '())
           (erltrace-concat fmt args)))))

(defun erltrace-concat (fmt args)
  (let ((user (getenv "USER"))
        (func (if (< erltrace-erlang-vsn 19) "?FUNC" "?FUNCTION_NAME"))
        (printf (if (or (equal erltrace-ct-pal 'true)
                        (string-match ".*_SUITE.erl" (buffer-file-name)))
                    "ct:pal" "io:format"))
        (endfmt (if (or (equal erltrace-ct-pal 'true)
                        (string-match ".*_SUITE.erl" (buffer-file-name)))
                    "" "~n")))
    (if (equal erltrace-level 'detail)
        (concat printf "(\"~p:" user ":~s:~p:~p:~p:" fmt endfmt "\",\n"
                "[erltrace_process_info(), erltrace_timestamp(),\n?MODULE, "
                func ", ?LINE" args "]),")
      (concat printf "(\"~p:~p:~p:" fmt endfmt "\", [?MODULE, " func ", ?LINE"
              args "]),"))))

(defun erltrace-fmt-vars (vars acc)
  (if (cdr vars)
      (let* ((head (car vars))
             (tail (cdr vars))
             (nacc (concat acc head ": ~p~n")))
        (erltrace-fmt-vars tail nacc))
    (concat "~n" acc (car vars) ": ~p")))

(defun erltrace-args-vars (vars acc)
  (if (cdr vars)
      (let* ((head (car vars))
             (tail (cdr vars))
             (nacc (concat acc ", " head)))
        (erltrace-args-vars tail nacc))
    (concat acc ", " (car vars))))

(defun erltrace-insert-string (string &optional auto)
  (plutil-debug-msg "Insert %s" string)
  (save-excursion
    (when (not auto) (erltrace-goto-eof-header-if-needed))
    (let ((start-point (point))
          (at-bol (bolp)))
      ;; If we are not standing at beginning-of-line
      (unless at-bol (progn (end-of-line) (newline)))
      (insert string)
      (when at-bol (newline))
      (indent-region start-point (point)))))

(defun erltrace-goto-marked (string)
  (when (condition-case nil (search-forward string) (error nil))
    (delete-backward-char (length string))))

(defun erltrace-what-at-point ()
  "Returns `variable' `atom' or `nothing' at cursor."
  (let ((thing (thing-at-point 'symbol))
        (case-fold-search nil))
    (if thing
        (if (equal (string-match-p erlang-variable-regexp thing) 0)
            'variable 'atom)
      'nothing)))

(defun erltrace-func-header-p ()
  (save-excursion
    (let ((current (point))
          (begin (progn (erltrace-boc-safe) (point)))
          (end (search-forward "->")))
      (if (and (<= begin current) (< current end)) t nil))))

(defun erltrace-boc-safe ()
  ;; We are not stand at beginning of function clause,
  ;; Lets move to it.
  (unless (erlang-get-function-name) (erlang-beginning-of-clause)))

(defun erltrace-goto-eof-header-if-needed ()
  ;; We are standing in the middle of function header,
  ;; Lets move to end of function header.
  (when (erltrace-func-header-p) (search-forward "->")))

;; -----------------------------------------------------------------------------
;; CLAUSE
;; -----------------------------------------------------------------------------
;; Increase the number of lisp bindings.
(setq max-specpdl-size 13000)
(defun erltrace-clause ()
  (save-excursion
    (erltrace-maybe-insert-supfun)
    (erlang-end-of-function)
    (let ((stop-point (point)))
      (erlang-beginning-of-function)
      (let ((start-point (point)))
        (erltrace-clause-loop stop-point 1)
        (indent-region start-point (point))))))

(defun erltrace-clause-loop (stop number)
  (search-forward "->")
  (let ((current (point)))
    (if (nth 4 (syntax-ppss))
        (erltrace-clause-loop stop number)
      (let* ((fmt (concat " case: " (number-to-string number)))
             (str (erltrace-concat fmt "")))
        (when (< current stop)
          (newline-and-indent)
          (insert str)
          (erltrace-clause-loop (+ (- (point) current) stop) (+ number 1)))))))

(provide 'erl-trace)
