;;;; Adapted for Guile Scheme

(define all-tests '())

(define (compile-program expr)
  (error "Requires implementation."))

(define-syntax add-tests-with-string-output
  (syntax-rules (=>)
    ((_ name (expr => output-string) ...)
     (set! all-tests
           (cons
            '(name (expr string output-string) ...)
            all-tests)))))

(define-syntax add-tests-with-string-output-noboot
  (syntax-rules (=>)
    ((_ test-name (expr => output-string) ...)
     (set! all-tests
           (cons
            '(test-name [expr string output-string noboot] ...)
            all-tests)))))

(define (run-compile expr)
  (let ((p (open-file "./bin/stst.s" "w")))
    (parameterize ((compile-port p))
      (compile-program expr))
    (close-output-port p)))

(define (build)
  (unless (zero? (system "gcc -o ./bin/stst src/startup.c ./bin/stst.s"))
    (error 'make "Could not build target.")))

(define (execute)
  (unless (zero? (system "./bin/stst > ./bin/stst.out"))
    (error 'make "Produced program exited abnormally.")))

(define (build-program expr)
  (run-compile expr)
  (build))

(define (get-string)
  (with-output-to-string
    (lambda ()
      (with-input-from-file "./bin/stst.out"
        (lambda ()
          (let f ()
            (let ((c (read-char)))
              (cond
               ((eof-object? c) (%make-void-port "r+"))
               (else (display c)
                     (f))))))))))

(define (test-with-string-output test-id expr expected-output)
  (run-compile expr)
  (build)
  (execute)
  (unless (string=? expected-output (get-string))
    (error 'test (format "Output mismatch for test ~s, expr ~s, expected ~s, got ~s"
                  test-id expr expected-output (get-string)))))

(define (test-one test-id test)
  (let
      ((expr (car test))
       (type (cadr test))
       (out (caddr test)))
    (format #t "test ~s:~s ..." test-id expr)
    (force-output)
    (case type
      ((string) (test-with-string-output test-id expr out))
      (else (error 'test "invalid test type ~s" type)))
    (format #t " ok\n")))

(define (test-all)
  (let f ((i 0) (ls (reverse all-tests)))
    (if (null? ls)
        (format #t "passed all ~s tests\n" i)
        (let ((x (car ls)) (ls (cdr ls)))
          (let*
              ((test-name (car x))
               (tests (cdr x))
               (n (length tests)))
            (format #t "Performing ~a tests ...\n" test-name)
            (let g ((i i) (tests tests))
              (cond
               ((null? tests) (f i ls))
               (else
                (test-one i (car tests))
                (g (+ i 1) (cdr tests))))))))))

(define input-filter
  (make-parameter
   (lambda (x) x)
   (lambda (x)
     (unless (procedure? x)
       (error 'input-filter "not a procedure ~s" x))
     x)))

(define runtime-file
  (make-parameter
   "runtime.c"
   (lambda (fname)
     (unless (string? fname)
       (error 'runtime-file "not a string" fname))
     fname)))

(define compile-port
  (make-parameter
   (current-output-port)
   (lambda (p)
     (unless (output-port? p)
       (error 'compile-port "not an output port ~s" p))
     p)))

(define show-compiler-output (make-parameter #f))

(define (emit . args)
  (apply format (compile-port) args)
  (newline (compile-port)))
