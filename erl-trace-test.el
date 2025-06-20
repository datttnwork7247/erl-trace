;;; erl-trace-test.el --- Unit tests for erl-trace -*- lexical-binding: t; -*-

(require 'ert)
(require 'subr-x)
(load-file "erl-trace.el")

;; Stub/override thing-at-point to mock point context
(defmacro with-mocked-thing-at-point (value &rest body)
  `(cl-letf (((symbol-function 'thing-at-point) (lambda (_), value)))
     ,@body))

;; Base test macro to simplify definition

(defmacro define-trace-test (name mode opts prefix input-symbol type expected)
  `(ert-deftest,
    name ()
    (let* ((erl-trace-mode, mode)
           (erl-trace-options, opts)
           (erl-trace-prefix, prefix))
      (with-mocked-thing-at-point,
       input-symbol
       (should (equal (erl-trace-build-string, type), expected))))))

;; --- Test cases ---

;; 'nothing case (fixed string, no args)
(define-trace-test etbt--debug-nothing
  'debug '(:detail nil :location nil :newline nil) "tag" nil 'nothing
  "?LOG_DEBUG(\"tag  -!-\", []),")

;; 'atom case
(define-trace-test etbt--info-atom-prefix
  'info '(:detail nil :location nil :newline nil) "prefix" "MyAtom" 'atom
  "?LOG_INFO(\"prefix MyAtom\", []),")

;; 'variable with no detail/location
(define-trace-test etbt--error-var-basic
  'error '(:detail nil :location nil :newline nil) "pre" "SomeVar" 'variable
  "?LOG_ERROR(\"pre SomeVar = ~p\", [SomeVar]),")

;; 'variable with detail only
(define-trace-test etbt--debug-var-detail
  'debug '(:detail t :location nil :newline nil) "mytag" "X" 'variable
  "?LOG_DEBUG(\"mytag ~p:~p X = ~p\", [erl_trace_process_info(), erl_trace_timestamp(), X]),")

;; 'variable with location only
(define-trace-test etbt--info-var-loc
  'info '(:detail nil :location t :newline nil) "mytag" "X" 'variable
  "?LOG_INFO(\"mytag ~p:~p:~p X = ~p\", [?MODULE, ?FUNCTION_NAME, ?LINE, X]),")

;; 'variable with detail + location
(define-trace-test etbt--debug-var-all
  'debug '(:detail t :location t :newline nil) "mytag" "V" 'variable
  "?LOG_DEBUG(\"mytag ~p:~p ~p:~p:~p V = ~p\", [erl_trace_process_info(), erl_trace_timestamp(), ?MODULE, ?FUNCTION_NAME, ?LINE, V]),")

;; 'io variable with all
(define-trace-test etbt--io-var-all
  'io '(:detail t :location t :newline t) "mytag" "V" 'variable
  "io:format(\"~nmytag ~p:~p ~p:~p:~p V = ~p\", [erl_trace_process_info(), erl_trace_timestamp(), ?MODULE, ?FUNCTION_NAME, ?LINE, V]),")

;; 'io variable with no option
(define-trace-test etbt--io-var-nil
  'io '(:detail nil :location nil :newline nil) "" "V" 'variable
  "io:format(\"~n V = ~p\", [V]),")

;; 'io variable with detail + location
(define-trace-test etbt--io-var-all
  'io '(:detail t :location t :newline nil) "mytag" "V" 'variable
  "io:format(\"~nmytag ~p:~p ~p:~p:~p V = ~p\", [erl_trace_process_info(), erl_trace_timestamp(), ?MODULE, ?FUNCTION_NAME, ?LINE, V]),")

;; 'io atom with location only
(define-trace-test etbt--io-var-loc-only
  'io '(:detail nil :location t :newline nil) "mytag" "V" 'variable
  "io:format(\"~nmytag ~p:~p:~p V = ~p\", [?MODULE, ?FUNCTION_NAME, ?LINE, V]),")

;; 'stored vars: 2 items, with newline formatting
(ert-deftest etbt--stored-vars ()
  (let* ((erl-trace-mode 'io)
         (erl-trace-options '(:detail nil :location nil :newline t))
         (erl-trace-prefix "tag")
         (erl-trace-stored '("X" "Y")))
    (let ((result (erl-trace-build-string 'stored)))
      (should (equal result
                     "io:format(\"~ntag X = ~p~nY = ~p\", [X, Y]),")))))

;; Corner case: empty stored list
(ert-deftest etbt--stored-empty ()
  (let* ((erl-trace-mode 'ct)
         (erl-trace-options '(:detail t :location nil :newline nil))
         (erl-trace-prefix "TAG")
         (erl-trace-stored '()))
    (let ((result (erl-trace-build-string 'stored)))
      (should
       (equal
        result
        "ct:pal(\"TAG ~p:~p \", [erl_trace_process_info(), erl_trace_timestamp()]),")
       ))))
