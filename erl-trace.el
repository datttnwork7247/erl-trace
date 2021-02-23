;;; erl-trace.el --- a simple package                     -*- lexical-binding: t; -*-

;; Copyright (C) 2014  Nic Ferrier

;; Author: Dat <abc@xyz>
;; Keywords: lisp
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

;;; Code:
;; -----------------------------------------------------------------------------
;; Part 0. Global configurations
;; -----------------------------------------------------------------------------
(defvar erl-trace-dbg nil)
(defvar erl-trace-store '())
(defvar erl-trace-level 'simple)
(defvar erl-trace-macro nil)
(defvar erl-trace-stored-cmd nil)

(defun erl-trace-macro ()
  (interactive)
  (if erl-trace-macro
      (progn
        (message "Disable erl-trace macro!")
        (setq erl-trace-macro nil)
        )
    (progn
      (message "Enable erl-trace macro!")
      (setq erl-trace-macro t)
      )
    )
  )

;; -----------------------------------------------------------------------------
;; Part 1. Analyze erl-trace type
;; -----------------------------------------------------------------------------
(defun insert-erl-trace ()
  (interactive)
  (erl-trace-maybe-insert-supfun)
  (cond ((use-region-p)
         (erl-trace-msg "Insert erl-trace: %s" "variables in region")
         (setq string (build-erl-trace-string 'variables))
         (goto-region-end)
         (insert-erl-trace-string 'keep string))
        (t (cond ((equal (what-at-point) 'atom)
                  (erl-trace-msg "Insert erl-trace: %s" "atom at point")
                  (setq string (build-erl-trace-string 'atom))
                  (insert-erl-trace-string 'keep string))
                 ((equal (what-at-point) 'variable)
                  (erl-trace-msg "Insert erl-trace: %s" "variable at point")
                  (setq string (build-erl-trace-string 'variable))
                  (insert-erl-trace-string 'keep string))
                 ((equal (what-at-point) 'nothing)
                  (cond ((equal erl-trace-store nil)
                         (erl-trace-msg "Insert erl-trace: %s" "other cases")
                         (setq string (build-erl-trace-string 'other))
                         (insert-erl-trace-string 'move string))
                        (t (erl-trace-msg "Insert erl-trace: %s" "stored")
                           (setq string (build-erl-trace-string 'store))
                           (insert-erl-trace-string 'keep string))))))))

(defun erl-trace-maybe-insert-supfun ()
  (save-excursion
    (when (erl-trace-insert-supfun-p) (erl-trace-insert-supfun))
    ))

(defun erl-trace-insert-supfun-p ()
  (cond ((and (equal erl-trace-level 'simple)
              (equal erl-trace-macro nil)
              )
         nil
         )
        ((and (equal erl-trace-level 'simple)
              (equal erl-trace-macro t)
              )
         (not (erl-trace-function-name-on-top-p))
         )
        ((and (equal erl-trace-level 'complex)
              (equal erl-trace-macro nil)
              )
         (not (erl-trace-timestamp-at-bottom-p))
         )
        ((and (equal erl-trace-level 'complex)
              (equal erl-trace-macro t)
              )
         (not (erl-trace-function-name-on-top-p))
         )
        )
  )

(defun erl-trace-timestamp-at-bottom-p ()
  (end-of-buffer)
  (if (condition-case nil
          (search-backward "erl_trace_timestamp() ->")
        (error nil)) t nil)
  )

(defun erl-trace-function-name-on-top-p ()
  (beginning-of-buffer)
  (if (condition-case nil
          (search-forward "-ifndef(FUNC).")
        (error nil)) t nil)
  )

(defun erl-trace-insert-supfun ()
  (when erl-trace-macro
    (beginning-of-buffer)
    (insert (build-erl-trace-string 'macro))
    (newline)
    )
  (when (equal erl-trace-level 'complex)
    (end-of-buffer)
    (newline)
    (insert (build-erl-trace-string 'timestamp))
    (newline)
    (insert (build-erl-trace-string 'procinfo))
    )
  )

(defun what-at-point ()
  (cond ((thing-at-point 'symbol)
         (if (is-erl-variable) 'variable 'atom))
        (t 'nothing)))
(defun is-erl-variable ()
  (let ((thing (thing-at-point 'symbol))
        (case-fold-search nil))
    (cond ((equal (string-match-p "[A-Z].*" thing) 0) t)
          (t nil))))

(defun goto-region-end ()
  (let ((from (region-beginning))
        (to (region-end)))
    (if (> from to)
        (goto-char from)
      (goto-char to))))

;; -----------------------------------------------------------------------------
;; Part 2. Build erl-trace string
;; -----------------------------------------------------------------------------
(defun build-erl-trace-string (type)
  (cond ((equal type 'atom)
         (erl-trace-msg "Build %s!" "Atom")
         (erl-trace-string type))
        ((equal type 'variable)
         (erl-trace-msg "Build %s!" "Variable")
         (erl-trace-string type))
        ((equal type 'other)
         (erl-trace-msg "Build %s!" "Other")
         (concat-erl-string " -!-~n" ""))
        ((equal type 'variables)
         (erl-trace-msg "Build %s!" "Variables")
         (erl-trace-string type))
        ((equal type 'store)
         (erl-trace-msg "Build %s!" "Stored")
         (erl-trace-string type))
        ((equal type 'macro)
         (erl-trace-msg "Build %s!" "Macro")
         (erl-trace-macro-string))
        ((equal type 'timestamp)
         (erl-trace-msg "Build %s!" "Timestamp")
         (erl-trace-timestamp-string))
        ((equal type 'procinfo)
         (erl-trace-msg "Build %s!" "Process Info")
         (erl-trace-procinfo-string))
        (t "")))

(defun erl-trace-macro-string ()
  "-ifndef(FUNC).
-define(FUNC, element(2, element(2, process_info(self(), current_function)))).
-endif.")

(defun erl-trace-timestamp-string ()
  "erl_trace_timestamp() ->
    {MS, S, US} = os:timestamp(),
    {_, {Hour, Min, Sec}} = calendar:now_to_local_time({MS, S, US}),
    MilliSec = trunc(US/1000),
    lists:flatten(io_lib:format(\"~2.10.0B:~2.10.0B:~2.10.0B.~3.10.0B\",
                                [Hour, Min, Sec, MilliSec])).")

(defun erl-trace-procinfo-string()
  "proc_info() ->
    Pid = self(),
    ProcessInfo = process_info(Pid),
    proplists:get_value(registered_name, ProcessInfo, Pid).")

(defun erl-trace-string (type)
  (cond ((equal type 'atom)
         (let* ((atom (thing-at-point 'symbol))
                (up (concatenate 'string " " atom "~n")))
           (concat-erl-string up "")))
        ((equal type 'variable)
         (let* ((variable (thing-at-point 'symbol))
                (up (concatenate 'string " ~n" variable ": ~p~n"))
                (down (concatenate 'string ", " variable)))
           (concat-erl-string up down)))
        ((equal type 'variables)
         (let* ((variables (get-erl-variables))
                (up (build-erl-variables-up variables ""))
                (down (build-erl-variables-down variables "")))
           (concat-erl-string up down)))
        ((equal type 'store)
         (let* ((variables (remove-duplicate-variables erl-trace-store '()))
         ;; (let* ((variables (remove-duplicates erl-trace-store '()))
                (up (build-erl-variables-up variables ""))
                (down (build-erl-variables-down variables "")))
           (setq erl-trace-store '())
           (concat-erl-string up down)))))

(defun erl-trace-level-toogle ()
  (interactive)
  (cond ((equal erl-trace-level 'complex)
         (setq erl-trace-level 'simple))
        ((equal erl-trace-level 'simple)
         (setq erl-trace-level 'complex))))

(defun concat-erl-string (up down)
  (let ((user (getenv "USER"))
        (function-name (if erl-trace-macro "?FUNC" "?FUNCTION_NAME"))
        )
    (cond ((equal erl-trace-level 'complex)
           (concat "io:format(\"~p:" user ":~s:~p:~p:~p:" up "\",\n"
                   "[proc_info(), erl_trace_timestamp(), ?MODULE, " function-name
                   ", ?LINE" down "]),"))
          ((equal erl-trace-level 'simple)
           (concat "io:format(\"~p:~p:~p:" up "\", [?MODULE, " function-name
                   ", ?LINE" down "]),")))))

(defun get-erl-variables ()
  (let ((from (region-beginning))
        (to (region-end)))
    (setq region-content (buffer-substring from to))
    (setq all-erl-variables (get-all-erl-variables region-content '()))
    (remove-duplicate-variables all-erl-variables '())))
    ;; (remove-duplicates all-erl-variables '())))

(defun get-all-erl-variables (string result)
  (cond ((goto-next-erl-variable string)
         (setq new-string (goto-next-erl-variable string))
         (erl-trace-msg "First New String: %s" new-string)
         (cond ((get-erl-variable new-string)
                (setq erl-variable (get-erl-variable new-string))
                (setq new-result (cons erl-variable result))
                (setq new-string (remove-erl-variable new-string))
                (erl-trace-msg "Erl Variable: %s" erl-variable)
                (erl-trace-msg "New Result: %s" new-result)
                (erl-trace-msg "Second New String: %s" new-string)
                (get-all-erl-variables new-string new-result))
               (t (reverse result))))
        (t (reverse result))))
(defun goto-next-erl-variable (string)
  (condition-case nil
      (let ((case-fold-search nil))
        (setq pos (+ (string-match "[{(, ][A-Z].*" string) 1))
        (subseq string pos))
    (error nil)))
(defun get-erl-variable (string)
  (condition-case nil
      (let ((case-fold-search nil))
        (setq pos (string-match "[, =})]" string))
        (subseq string 0 pos))
    (error nil)))
(defun remove-erl-variable (string)
  (condition-case nil
      (let ((case-fold-search nil))
        (setq pos (string-match "[, =})]" string))
        (subseq string pos))
    (error nil)))

(defun remove-duplicate-variables (variables result)
  (cond ((cdr variables)
         (let ((head (car variables))
               (tail (cdr variables)))
           (erl-trace-msg "Head: %s" head)
           (erl-trace-msg "Tail: %s" tail)
           (erl-trace-msg "Result: %s" result)
           (cond ((is-member head result)
                  (remove-duplicate-variables tail result))
                 (t (remove-duplicate-variables tail (cons head result))))))
        (t (cond ((is-member (car variables) result)
                  (reverse result))
                 (t (reverse (cons (car variables) result)))))))
(defun is-member (element list)
  (cond ((car list)
         (let ((check-string (car list)))
           (cond ((string= element check-string) t)
                 (t (is-member element (cdr list))))))
        (t nil)))

(defun build-erl-variables-up (variables result)
  (cond ((cdr variables)
         (let ((head (car variables))
               (tail (cdr variables)))
           (setq new-result (concatenate 'string result head ": ~p~n"))
           (build-erl-variables-up tail new-result)))
        (t (concatenate 'string "~n" result (car variables) ": ~p~n"))))
(defun build-erl-variables-down (variables result)
  (cond ((cdr variables)
         (let ((head (car variables))
               (tail (cdr variables)))
           (setq new-result (concatenate 'string result ", " head))
           (build-erl-variables-down tail new-result)))
        (t (concatenate 'string result ", " (car variables)))))

;; -----------------------------------------------------------------------------
;; Part 3. Insert erl-trace string
;; -----------------------------------------------------------------------------
(defun insert-erl-trace-string (mode string)
  (cond ((equal mode 'keep)
         (save-excursion
           (erl-trace-msg "Mode: %s" mode)
           (insert-erl-trace-string-header-or-body string)))
        ((equal mode 'move)
         (erl-trace-msg "Mode: %s" mode)
         (insert-erl-trace-string-header-or-body string)
         (goto-marked "-!-"))))
(defun goto-marked (string)
  (when (condition-case nil
            (search-backward string)
          (error nil))
    (delete-forward-char (length string))))

(defun insert-erl-trace-string-header-or-body (string)
  (cond ((is-erl-function-header)
         (erl-trace-msg "Special case: %s" "erlang function header")
         ;; (goto-end-of-erl-function-header)
         )
        (t t))
  (insert-erl-trace-string-help string))
(defun is-erl-function-header ()
  (save-excursion
    (setq pos-head (condition-case nil
                       (search-forward "->")
                     (error nil))))
  (save-excursion
    (setq pos-end (condition-case nil
                      (re-search-forward "[;.]")
                    (error nil))))
  (when (and pos-head pos-end)
    (if (< pos-head pos-end) t nil)))
(defun goto-end-of-erl-function-header ()
  (search-forward "->"))
(defun insert-erl-trace-string-help (string)
  (end-of-line)
  (newline-and-indent)
  (insert string)
  (indent-according-to-mode))

;; -----------------------------------------------------------------------------
;; Part 4. Debugging
;; -----------------------------------------------------------------------------
(defun erl-trace-debug ()
  (interactive)
  (if erl-trace-dbg
      (progn
        (message "Disable erl-trace dbg!")
        (setq erl-trace-dbg nil)
        )
    (progn
      (message "Enable erl-trace dbg!")
      (setq erl-trace-dbg t)
      )
    ))

(defun erl-trace-msg (string info)
  (when erl-trace-dbg
    (message string info)))

;; -----------------------------------------------------------------------------
;; Part 5. Store erl-trace variable
;; -----------------------------------------------------------------------------
(defun store-erl-trace-variable ()
  (interactive)
  (cond ((use-region-p)
         (erl-trace-msg "Store erl-trace: %s" "variables in region")
         (setq store-variable (get-erl-variables)))
        ((equal (what-at-point) 'variable)
         (erl-trace-msg "Store erl-trace: %s" "variable at point")
         (setq store-variable (thing-at-point 'symbol))))
  (deactivate-mark)
  (cond ((listp store-variable)
         (setq erl-trace-store (merge-list store-variable erl-trace-store)))
        ((not (is-member store-variable erl-trace-store))
         (setq reverse-variables (reverse erl-trace-store))
         (setq new-reverse-variables (cons store-variable reverse-variables))
         (setq new-variables (reverse new-reverse-variables))
         (setq erl-trace-store new-variables))
        (t t))
  (erl-trace-msg "erl-trace-store: %s" erl-trace-store)
  (message "Stored %s" store-variable))
(defun merge-list (first-list second-list)
  (let ((reverse-second-list (reverse second-list)))
    (cond ((cdr first-list)
           (setq head (car first-list))
           (setq tail (cdr first-list))
           (setq new-reverse-second-list (cons head reverse-second-list))
           (setq new-second-list (reverse new-reverse-second-list))
           (merge-list tail new-second-list))
          (t (setq head (car first-list))
             (setq new-reverse-second-list (cons head reverse-second-list))
             (reverse new-reverse-second-list)))))

;; -----------------------------------------------------------------------------
;; Part 6. Additional insert erl-trace
;; -----------------------------------------------------------------------------
(defun insert-erl-trace-additional ()
  (interactive)
  (setq command (completing-read "Which erl-trace command you want: "
                                 '(
                                   ;; "sip-msg"
                                   ;; "make-ft"
                                   "trace-clause"
                                   "trace-stack"
                                   "cleanup"
                                   "shell"
                                   "run"
                                   "funinfo"
                                   )
                                 nil t ""))
  (cond
   ;; ((equal command "sip-msg")
   ;;  (insert-erl-trace-sip-msg))
   ;; ((equal command "make-ft")
   ;;  (insert-erl-trace-make-ft))
   ((equal command "trace-clause")
    (insert-erl-trace-trace-clause))
   ((equal command "trace-stack")
    (insert-erl-trace-trace-stack))
   ((equal command "cleanup")
    (insert-erl-trace-cleanup))
   ((equal command "shell")
    (erl-trace-shell))
   ((equal command "run")
    (erl-trace-run))
   ((equal command "funinfo")
    (insert-erl-trace-funinfo))
   ))

;; -----------------------------------------------------------------------------
;; Part 6.3 Insert erl-trace trace-clause
;; -----------------------------------------------------------------------------
(defun insert-erl-trace-trace-clause ()
  (save-excursion
    (erlang-end-of-function)
    (setq stop-point (point))
    (erlang-beginning-of-function)
    (erl-trace-msg "Stop Point: %s" stop-point)
    (erl-trace-clause-loop stop-point 1)))

(defun erl-trace-clause-loop (stop number)
  (search-forward "->")
  (setq current (point))
  (erl-trace-msg "Current: %s" current)
  (cond ((< current stop)
         (newline-and-indent)
         (setq up (concat " case: " (number-to-string number) "~n"))
         (setq string (concat-erl-string up ""))
         (insert string)
         (indent-according-to-mode)
         (setq offset (- (point) current))
         (erl-trace-clause-loop (+ offset stop) (+ number 1)))
        (t t)))

;; -----------------------------------------------------------------------------
;; Part 6.x Insert erl-trace trace-stack
;; -----------------------------------------------------------------------------
(defun insert-erl-trace-trace-stack ()
  (setq string
        "try throw(exit)
    catch _Class:_Reason:Stacktrace ->
            io:format(\"~p:~p:~p: ~nStacktrace: ~p~n\", [?MODULE, ?FUNCTION_NAME, ?LINE, Stacktrace])
    end,"
        )
  (insert-erl-trace-string 'keep string)
  (indent-according-to-mode)
  )

;; -----------------------------------------------------------------------------
;; Part 6.4 Insert erl-trace cleanup
;; -----------------------------------------------------------------------------
(defun insert-erl-trace-cleanup ()
  "If current cursor belongs to a function, cleanup this function.
   If current cursor does not belong to a function, cleanup the whole module.
   The cleanup action includes:
     * Replace TAB chars with SPACE chars
     * Remove traling whitespaces
     * Remove multiple SPACE chars
     * Remove multiple empty lines
     * Reindent."
  (save-excursion
    (if (erl-trace-point-at-function-p)
        (erl-trace-narrow-function) nil)
    (untabify (point-min) (point-max))
    (erl-trace-space)
    (erl-trace-line)
    (indent-region (point-min) (point-max))
    (widen)))

(defun erl-trace-point-at-function-p ()
  "Check whether current cursor belongs to an erlang function."
  (let ((current (point))
        (func-start (progn (goto-char (+ (point) 1))
                           (erlang-beginning-of-function)
                           (point)))
        (func-end (progn (erlang-end-of-function)
                         (- (point) 2))))
    (message "Func Start: %s, Current: %s, Func End: %s"
             func-start current func-end)
    (if (and (<= func-start current)
             (>= func-end current))
        t nil)))

(defun erl-trace-narrow-function ()
  (let ((start (progn (erlang-beginning-of-function)
                      (point)))
        (stop (progn (erlang-end-of-function)
                     (- (point) 1))))
    (message "Start: %s. Stop: %s" start stop)
    (narrow-to-region start stop)))

(defun erl-trace-space ()
  (save-excursion
    (delete-trailing-whitespace)
    (erl-trace-remove-multiple-space)
    ;; (erl-trace-add-space-before)
    (erl-trace-add-space-after)
    ;; (erl-trace-add-space-2)
    ))

(defun erl-trace-remove-multiple-space ()
  (beginning-of-buffer)
  (while (re-search-forward "\\s-+" nil t)
    (replace-match " ")))

(defun erl-trace-add-space-after ()
  "Auto add space after:
   Char: ,
  "
  (save-excursion
    (beginning-of-buffer)
    (while (re-search-forward "\\([,]\\)\\([^ \n]\\)" nil t)
      (replace-match "\\1 \\2" nil nil))))

(defun erl-trace-line ()
  (save-excursion
    (erl-trace-remove-multiple-empty-line)))

(defun erl-trace-remove-multiple-empty-line ()
  (beginning-of-buffer)
  (while (re-search-forward "\n\n+" nil t)
    (replace-match "\n\n")))

;; -----------------------------------------------------------------------------
;; 6.5 erl-trace-shell
;; -----------------------------------------------------------------------------
(defvar erl-trace-shell-number 0)
(defun erl-trace-shell ()
  (let* ((user-input (read-string "Number of shell: " nil nil "2" nil))
         (no-shell (string-to-number user-input))
         (i 2)
         )
    ;; Delete current shell.
    (erl-trace-delete-current-shells erl-trace-shell-number)
    (unless (< no-shell 5) (error "No more than 5, please!"))
    (delete-other-windows)
    (split-window-horizontally)
    (shell "1-shell")
    (while (<= i no-shell)
      (split-window-vertically)
      (shell (concat (number-to-string i) "-shell"))
      (setq i (+ i 1)))
    (balance-windows)
    ;; Save number of shell, use for delete shell.
    (setq erl-trace-shell-number no-shell)
    ))

(defun erl-trace-delete-current-shells (no-shell)
  (let ((i 1))
    (while (<= i no-shell)
      (let ((buffer-name (concat (number-to-string i) "-shell")))
        (when (get-buffer buffer-name)
          (kill-buffer buffer-name)))
      (setq i (+ i 1)))
    ))

(defun goto-shell-buffer (shell-name)
  (cond ((get-buffer shell-name)
         (switch-to-buffer-other-window shell-name)
         (end-of-buffer))
        (t (cond ((one-window-p)
                  (split-window-horizontally)
                  (shell shell-name))
                 (t (shell shell-name))))))

;; -----------------------------------------------------------------------------
;; Part 7.1 erl-trace run stored command
;; -----------------------------------------------------------------------------
(defun erl-trace-run (&optional buffer)
  (interactive
   (when current-prefix-arg (setq erl-trace-stored-cmd nil)))
  (save-window-excursion
    (when (not erl-trace-stored-cmd)
      (erl-trace-ask-for-command))
    (async-shell-command erl-trace-stored-cmd)))

(defun erl-trace-ask-for-command ()
  (setq erl-trace-stored-cmd (read-string "Command: ")))

;; -----------------------------------------------------------------------------
;; Part 6.7 erl-trace run at point
;; -----------------------------------------------------------------------------
(defun erl-trace-run ()
  (let* ((file (erl-trace-run-file))
         (dir (erl-trace-run-dir file))
         )
    (if (one-window-p)
        (progn
          (beginning-of-line)
          (kill-line)
          (insert file)
          (save-buffer)
          (goto-shell-buffer "*shell*")
          (end-of-buffer)
          (insert (concat "cd " dir " && make clean build && lux " file))
          )
      (save-window-excursion
        (beginning-of-line)
        (kill-line)
        (insert file)
        (save-buffer)
        (goto-shell-buffer "*shell*")
        (end-of-buffer)
        (insert (concat "cd " dir " && make clean build && lux " file))
        (comint-send-input nil t)
        )
      )
    ))

(defun erl-trace-run-file ()
  (let* ((text1 (thing-at-point 'line t))
         (text2 (split-string text1 ":"))
         (text3 (car text2)))
    (erl-trace-msg "Text: %s" text3)
    (cond ((string-match "$W/" text3) (substring text1 0 (- (length text1) 1)))
          (t (concat "$W/" text3)))))

(defun erl-trace-run-dir (file)
  (let* ((text1 (split-string file "/"))
         (text2 (butlast text1 1))
         (text3 (mylib-lists-join text2 "/"))
         )
    text3
    ))

(defun mylib-lists-join (list sep)
  (let ((acc ""))
    (dolist (elem list acc)
      (setq acc (concat acc elem sep)))))

;; -----------------------------------------------------------------------------
;; Part 6.8 Insert erl-trace Fun Info
;; -----------------------------------------------------------------------------
(defun insert-erl-trace-funinfo ()
  (cond ((equal (what-at-point) 'variable)
         (let ((variable (thing-at-point 'symbol)))
           (setq string (build-erl-trace-funinfo variable))
           (do-insert-erl-trace-funinfo string)))
        (t t)))
(defun build-erl-trace-funinfo (variable)
  (concat "FunInfo = erlang:fun_info(" variable "),\n"
          (concat-erl-string " FunInfo: ~p~n" ", FunInfo")))
(defun do-insert-erl-trace-funinfo (string)
  (erl-trace-maybe-insert-supfun)
  (let ((current-point (point)))
    (end-of-line)
    (newline)
    (insert string)
    (set-mark (point))
    (goto-char current-point)
    (indent-for-tab-command)))

(provide 'erl-trace)
