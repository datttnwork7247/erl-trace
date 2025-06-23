;;; erl-ct.el --- a simple package                     -*- lexical-binding: t; -*-

(defun erlang-ct-get-testcase-at-point ()
  "Return the Erlang test case name at point."
  (save-excursion
    (end-of-line)
    (when (re-search-backward "^\\s-*\\([a-zA-Z0-9_]+\\)\\s-*([^)]+)\\s-*->" nil t)
      (match-string 1))))

(defun erlang-ct-get-group-at-point (testcase)
  "Find the group name that contains TESTCASE using testcases(Group) -> ... clauses."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward (regexp-quote testcase) nil t)
      (when (re-search-backward "^testcases(\\([a-zA-Z0-9_]+\\))\\s-*->" nil t)
        (match-string 1)))))

(defun erlang-mk-common-test-command ()
  "Generate `make ct-<suite>` command for Erlang.mk at point."
  (interactive)
  (let* ((file (buffer-file-name))
         (basename (file-name-base file))
         (suite (if (string-match "\\(.*\\)_SUITE" basename)
                    (match-string 1 basename)
                  nil))
         (testcase (erlang-ct-get-testcase-at-point))
         (group (erlang-ct-get-group-at-point testcase))
         (group-path nil))

    (let ((cmd
           (cond
            ((and suite group testcase)
             (format "make ct-%s t=%s:%s" suite group testcase))
            ((and suite group)
             (format "make ct-%s t=%s" suite group))
            ((and suite testcase)
             (format "make ct-%s c=%s" suite testcase))
            ((and suite)
             (format "make ct-%s" suite))
            (t
             "make ct"))))
      (kill-new cmd)
      (message "Command copied to clipboard: %s" cmd))))

(provide 'erl-ct)
