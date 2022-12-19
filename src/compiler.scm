(define all-tests '())

(load "src/test-driver.scm")
(load "src/test/tests-1.1-req.scm")
(load "src/test/tests-1.2-req.scm")
(load "src/test/tests-1.3-req.scm")
(load "src/test/tests-1.4-req.scm")

(define bool-t #x6F)
(define bool-f #x2F)
(define empty-list #x3F)
; 7th bit determines boolean value. False -> 00101111b, True -> 01101111b
(define bool-bit-shift 6)
(define charshift 8)
(define chartag #x0F)
(define fxmask #x03)
(define fxshift 2)
(define fxtag #x00)

(define non-fixnum-immediates
  `(
    (#t . ,bool-t)
    (#f . ,bool-f)
    (() . ,empty-list)
    (#\tab . #x90F)
    (#\lf . #xA0F)
    (#\cr . #xD0F)
    (#\space . #x200F)
    (#\! . #x210F)
    (#\" . #x220F)
    (#\# . #x230F)
    (#\$ . #x240F)
    (#\% . #x250F)
    (#\& . #x260F)
    (#\' . #x270F)
    (#\( . #x280F)
    (#\) . #x290F)
    (#\* . #x2A0F)
    (#\+ . #x2B0F)
    (#\, . #x2C0F)
    (#\- . #x2D0F)
    (#\. . #x2E0F)
    (#\/ . #x2F0F)
    (#\0 . #x300F)
    (#\1 . #x310F)
    (#\2 . #x320F)
    (#\3 . #x330F)
    (#\4 . #x340F)
    (#\5 . #x350F)
    (#\6 . #x360F)
    (#\7 . #x370F)
    (#\8 . #x380F)
    (#\9 . #x390F)
    (#\: . #x3A0F)
    (#\; . #x3B0F)
    (#\< . #x3C0F)
    (#\= . #x3D0F)
    (#\> . #x3E0F)
    (#\? . #x3F0F)
    (#\@ . #x400F)
    (#\A . #x410F)
    (#\B . #x420F)
    (#\C . #x430F)
    (#\D . #x440F)
    (#\E . #x450F)
    (#\F . #x460F)
    (#\G . #x470F)
    (#\H . #x480F)
    (#\I . #x490F)
    (#\J . #x4A0F)
    (#\K . #x4B0F)
    (#\L . #x4C0F)
    (#\M . #x4D0F)
    (#\N . #x4E0F)
    (#\O . #x4F0F)
    (#\P . #x500F)
    (#\Q . #x510F)
    (#\R . #x520F)
    (#\S . #x530F)
    (#\T . #x540F)
    (#\U . #x550F)
    (#\V . #x560F)
    (#\W . #x570F)
    (#\X . #x580F)
    (#\Y . #x590F)
    (#\Z . #x5A0F)
    (#\[ . #x5B0F)
    (#\\ . #x5C0F)
    (#\] . #x5D0F)
    (#\^ . #x5E0F)
    (#\_ . #x5F0F)
    (#\` . #x600F)
    (#\a . #x610F)
    (#\b . #x620F)
    (#\c . #x630F)
    (#\d . #x640F)
    (#\e . #x650F)
    (#\f . #x660F)
    (#\g . #x670F)
    (#\h . #x680F)
    (#\i . #x690F)
    (#\j . #x6A0F)
    (#\k . #x6B0F)
    (#\l . #x6C0F)
    (#\m . #x6D0F)
    (#\n . #x6E0F)
    (#\o . #x6F0F)
    (#\p . #x700F)
    (#\q . #x710F)
    (#\r . #x720F)
    (#\s . #x730F)
    (#\t . #x740F)
    (#\u . #x750F)
    (#\v . #x760F)
    (#\w . #x770F)
    (#\x . #x780F)
    (#\y . #x790F)
    (#\z . #x7A0F)
    (#\{ . #x7B0F)
    (#\| . #x7C0F)
    (#\} . #x7D0F)
    (#\~ . #x7E0F)
    ))

(define (non-fixnum-immediate? x)
  (in? x (map (lambda (x) (car x)) non-fixnum-immediates)))

(define (immediate? x)
  (or (integer? x) (non-fixnum-immediate? x)))

(define supported-types
  (list immediate?))

(define (immediate-rep x)
  (let
      ((fxshift 2))
    (cond
     ((integer? x)
      (ash x fxshift))
     ((non-fixnum-immediate? x)
      (cdr (assoc x non-fixnum-immediates)))
     (else (error 'immediate-rep "Unknown type" x)))))

;; Verifies whether element e exists in list l.
;; Returns #t if e is found, #f otherwise.
;; Example: (in? #\! (list #\space #\!))
(define (in? e l)
  (cond
   ((null? l) #f)
   ((eq? e (car l)) #t)
   (else (in? e (cdr l)))))

;; ORs a list l of booleans.
;; Example: (or-l (list #t #f)).
(define (or-l l)
  (in? #t l))

(define (primitive? x)
  (and (symbol? x) (symbol-property x '*is-prim*)))

(define (primcall? expr)
  (and (pair? expr) (primitive? (car expr))))

(define (if? expr)
  (and (pair? expr) (eq? 'if (car expr))))

(define (primitive-emitter x)
  (or (symbol-property x '*emitter*) (error 'primitive-emitter x)))

(define (check-primcall-args prim args)
  (unless (= (length args) (symbol-property prim '*arg-count*))
    (error 'check-primcall-args 'argument-length-mismatch prim args)))

(define (emit-primcall expr)
  (let ((prim (car expr)) (args (cdr expr)))
    (check-primcall-args prim args)
    (apply (primitive-emitter prim) args)))

(define (emit-immediate x)
  (emit "      movl    $~s, %eax" (immediate-rep x)))

(define (if-test expr)
  (cadr expr))

(define (if-conseq expr)
  (caddr expr))

(define (if-altern expr)
  (cadddr expr))

(define (emit-if expr)
  (let ((alt-label (unique-label))
        (end-label (unique-label)))
    (emit-expr (if-test expr))
    (emit "      cmp $~s, %al" bool-f)
    (emit "      je ~a" alt-label)
    (emit-expr (if-conseq expr))
    (emit "      jmp ~a" end-label)
    (emit "~a:" alt-label)
    (emit-expr (if-altern expr))
    (emit "~a:" end-label)))

(define (and? expr)
  (and (pair? expr) (eq? 'and (car expr))))

(define (and-transformer expr)
  (cond ((and (pair? expr)
              (eq? (car expr) 'and)
              (null? (cdr expr))) #t) ; ((and) #t)
        ((and (pair? expr)
              (eq? (car expr) 'and)
              (not (null? (cdr expr)))
              (= (length (cdr expr)) 1))
         (cadr expr)) ; ((and test) test)
        ((and (pair? expr)
              (eq? (car expr) 'and)
              (> (length (cdr expr)) 1))
         (list 'if (cadr expr) (and-transformer `(and ,@(cddr expr))) #f))
        (else (error and-transformer "Unexpected syntax" expr))))

(define (or? expr)
  (and (pair? expr) (eq? 'or (car expr))))

(define (or-transformer expr)
  (cond ((and (pair? expr)
              (eq? (car expr) 'or)
              (null? (cdr expr))) #t) ; ((or) #t)
        ((and (pair? expr)
              (eq? (car expr) 'or)
              (not (null? (cdr expr)))
              (= (length (cdr expr)) 1))
         (cadr expr)) ; ((or test) test)
        ((and (pair? expr)
              (eq? (car expr) 'or)
              (> (length (cdr expr)) 1))
         (list 'if (cadr expr) #t (or-transformer `(or ,@(cddr expr)))))
        (else (error or-transformer "Unexpected syntax" expr))))


(define (emit-expr expr)
  (cond
   ((immediate? expr) (emit-immediate expr))
   ((if? expr) (emit-if expr))
   ((and? expr)
    (emit-expr (and-transformer expr)))
   ((or? expr)
    (emit-expr (or-transformer expr)))
   ((primcall? expr)  (emit-primcall expr))
   (else (error 'emit-expr 'unsupported expr))))

(define-syntax define-primitive
  (syntax-rules ()
    ((_ (prim-name arg* ...) b b* ...)
     (begin
       (set-symbol-property! 'prim-name '*is-prim* #t)
       (set-symbol-property! 'prim-name '*arg-count*
                             (length '(arg* ...)))
       (set-symbol-property! 'prim-name '*emitter*
                             (lambda (arg* ...) b b* ...))))))

(define (get-predicate-result)
  (emit "      movzbl %al, %eax")    ; sign extend lower half of register to upper half
  (emit "      sal $~s, %al" bool-bit-shift) ; left shift
  (emit "      or $~s, %al" bool-f)    ; ORs the result with bool-f to obtain our boolean equivalent of the result.
  )

;; Similar to get-predicate-result but return the not value.
(define (get-predicate-result-not)
  (emit "      movzbl %al, %eax")
  (emit "      sal $~s, %al" bool-bit-shift)
  (emit "      xor $~s, %al" bool-f))

(define-primitive (fxadd1 arg)
  (emit-expr arg)
  (emit "      addl $~s, %eax" (immediate-rep 1)))

(define-primitive (fxsub1 arg)
  (emit-expr arg)
  (emit "      subl $~s, %eax" (immediate-rep 1)))

(define-primitive (char->fixnum arg)
  (emit-expr arg)
  (emit "      shrl $~s, %eax" (- charshift fxshift))) ; shift right

(define-primitive (fixnum->char arg)
  (emit-expr arg)
  (emit "      shll $~s, %eax" (- charshift fxshift)) ; shift left
  (emit "      orl $~s, %eax" chartag))               ; bitwise or

;; x -> extended
;; e -> extra
;; eax -> extra extended, 32 bits
;;
;; 00000000 00000000 00000000 00000000
;; ----------------------------------- -> %eax
;;                            -------- -> %al
;;                   --------          -> %ah
;; -----------------                   -> %ax
(define-primitive (fixnum? arg)
  (emit-expr arg)
  (emit "      and $~s, %al" fxmask) ; and arg with fixnum mask
  (emit "      cmp $~s, %al" fxtag)  ; compare the result of above with the fixnum tag
  (emit "      sete %al")            ; sets the result to 1 if the above is true
  (get-predicate-result))

;; same as fixnum? but
;; 1. without the "and" instruction
;; 2. and cmpl on %eax for 32 bit register since
;;    we want to compare the whole number instead
;;    of part of the register %al as done in fixnum?
(define-primitive (fxzero? arg)
  (emit-expr arg)
  (emit "      cmpl $~s, %eax" #x00)  ; compare full arg (cmpl, 32 bits) with 0 (#x00)
  (emit "      sete %al")            ; sets the result to 1 if the above is true
  (get-predicate-result))

(define-primitive (null? arg)
  (emit-expr arg)
  (emit "      cmpl $~s, %eax" empty-list)
  (emit "      sete %al")
  (get-predicate-result))

(define-primitive (boolean? arg)
  (emit-expr arg)
  (emit "      orl $~s, %eax" #x40) ; #x40, 7th bit set to 1, sets a false to true.
  (emit "      cmpl $~s, %eax" bool-t)
  (emit "      sete %al")
  (get-predicate-result))

(define-primitive (char? arg)
  (emit-expr arg)
  (emit "      cmp $~s, %al" chartag)
  (emit "      sete %al")
  (get-predicate-result))

(define-primitive (not arg)
  (emit-expr arg)
  (emit "      cmp $~s, %al" bool-f)
  (emit "      sete %al")
  (get-predicate-result-not))

(define-primitive (fxlognot arg)
  (emit-expr arg)
  (emit "      xorl $~s, %eax" #xFFFFFFFF) ; xor with all bits set to 1. This will flip all bits. 
  (emit "      andl $~s, %eax" #xFFFFFFFC)) ; set first 2 LSB to zero.

(define unique-label
  (let ((count 0))
    (lambda ()
      (let ((L (format #f "L_~s" count)))
        (set! count (+ 1 count))
        L))))

;; Referred to as emit-program in the tutorial.
(define (compile-program expr)
  (emit "      .globl  _scheme_entry")
  (emit "      .p2align    4, 0x90")
  (emit "  _scheme_entry:")
  (emit "       .cfi_startproc")
  (emit-expr expr)
  (emit "      retq")
  (emit "      .cfi_endproc"))

(test-all)

