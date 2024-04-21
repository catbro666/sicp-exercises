#lang sicp

(#%require (only racket provide))

; target: the register in which the compiled code
; is to return the value of the expression
; linkage: describes how the code resulting from
; the compilation of the expression should proceed
; when it has finished its execution.
; next|return|label
(define (compile exp target linkage)
  (cond ((self-evaluating? exp)
         (compile-self-evaluating 
          exp target linkage))
        ((quoted? exp) 
         (compile-quoted exp target linkage))
        ((variable? exp)
         (compile-variable 
          exp target linkage))
        ((assignment? exp)
         (compile-assignment
          exp target linkage))
        ((definition? exp)
         (compile-definition
          exp target linkage))
        ((if? exp)
         (compile-if exp target linkage))
        ((lambda? exp)
         (compile-lambda exp target linkage))
        ((begin? exp)
         (compile-sequence 
          (begin-actions exp) target linkage))
        ((cond? exp) 
         (compile 
          (cond->if exp) target linkage))
        ((let? exp)
         (compile
           (let->combination exp) target linkage))
        ((open-code-application-arbi? exp)
         (compile-open-code-arbi exp target linkage))
        ((open-code-application? exp)
         (compile-open-code exp target linkage))
        ((application? exp)
         (compile-application 
          exp target linkage))
        (else
         (error "Unknown expression type: 
                 COMPILE" 
                exp))))

; needs: registers that must be initialized
; modifies: registers whose values are modified
; statements: the actual instructions
(define (make-instruction-sequence 
         needs modifies statements)
  (list needs modifies statements))

(define (empty-instruction-sequence)
  (make-instruction-sequence '() '() '()))

; compiling linkage code
(define (compile-linkage linkage)
  (cond ((eq? linkage 'return)
         (make-instruction-sequence 
          '(continue)
          '()
          '((goto (reg continue)))))
        ((eq? linkage 'next)
         (empty-instruction-sequence))
        (else
         (make-instruction-sequence '() '()
          `((goto (label ,linkage)))))))

(define (end-with-linkage 
         linkage instruction-sequence)
  (preserving '(continue)
   instruction-sequence
   (compile-linkage linkage)))

; compiling simple expressions
(define (compile-self-evaluating 
         exp target linkage)
  (end-with-linkage
   linkage (make-instruction-sequence 
            '()
            (list target)
            `((assign ,target (const ,exp))))))

(define (compile-quoted exp target linkage)
  (end-with-linkage
   linkage
   (make-instruction-sequence
    '()
    (list target)
    `((assign 
       ,target
       (const ,(text-of-quotation exp)))))))

(define (compile-variable
         exp target linkage)
  (end-with-linkage 
   linkage
   (make-instruction-sequence 
    '(env)
    (list target)
    `((assign ,target
              (op lookup-variable-value)
              (const ,exp)
              (reg env))))))

(define (compile-assignment 
         exp target linkage)
  (let ((var (assignment-variable exp))
        (get-value-code
         (compile (assignment-value exp) 
                  'val
                  'next)))
    (end-with-linkage 
     linkage
     (preserving 
      '(env)
      get-value-code
      (make-instruction-sequence
       '(env val)
       (list target)
       `((perform (op set-variable-value!)
                  (const ,var)
                  (reg val)
                  (reg env))
         (assign ,target (const ok))))))))

(define (compile-definition 
         exp target linkage)
  (let ((var (definition-variable exp))
        (get-value-code
         (compile (definition-value exp)
                  'val
                  'next)))
    (end-with-linkage
     linkage
     (preserving 
      '(env)
      get-value-code
      (make-instruction-sequence
       '(env val)
       (list target)
       `((perform (op define-variable!)
                  (const ,var)
                  (reg val)
                  (reg env))
         (assign ,target (const ok))))))))

; compiling conditional expressions
(define (compile-if exp target linkage)
  (let ((t-branch (make-label 'true-branch))
        (f-branch (make-label 'false-branch))
        (after-if (make-label 'after-if)))
    (let ((consequent-linkage
           (if (eq? linkage 'next) 
               after-if
               linkage)))
      (let ((p-code 
             (compile (if-predicate exp)
                      'val
                      'next))
            (c-code
             (compile (if-consequent exp) 
                      target 
                      consequent-linkage))
            (a-code
             (compile (if-alternative exp)
                      target
                      linkage)))
        (preserving 
         '(env continue)
         p-code
         (append-instruction-sequences
          (make-instruction-sequence 
           '(val) 
           '()
           `((test (op false?) (reg val))
             (branch (label ,f-branch))))
          (parallel-instruction-sequences
           (append-instruction-sequences 
            t-branch c-code)
           (append-instruction-sequences
            f-branch a-code))
          after-if))))))

; compiling sequences
(define (compile-sequence seq target linkage)
  (if (last-exp? seq)
      (compile (first-exp seq) target linkage)
      (preserving '(env continue)
       (compile (first-exp seq) target 'next)
       (compile-sequence (rest-exps seq)
                         target
                         linkage))))

; compiling lambda expressions
(define (compile-lambda exp target linkage)
  (let ((proc-entry 
         (make-label 'entry))
        (after-lambda 
         (make-label 'after-lambda)))
    (let ((lambda-linkage
           (if (eq? linkage 'next)
               after-lambda
               linkage)))
      (append-instruction-sequences
       (tack-on-instruction-sequence
        (end-with-linkage 
         lambda-linkage
         (make-instruction-sequence 
          '(env)
          (list target)
          `((assign 
             ,target
             (op make-compiled-procedure)
             (label ,proc-entry)
             (reg env)))))
        (compile-lambda-body exp proc-entry))
       after-lambda))))

(define (compile-lambda-body exp proc-entry)
  (let ((formals (lambda-parameters exp)))
    (append-instruction-sequences
     (make-instruction-sequence 
      '(env proc argl)
      '(env)
      `(,proc-entry
        (assign env 
                (op compiled-procedure-env)
                (reg proc))
        (assign env
                (op extend-environment)
                (const ,formals)
                (reg argl)
                (reg env))))
     (compile-sequence (lambda-body exp)
                       'val
                       'return))))

; compiling combinations
(define (compile-application 
         exp target linkage)
  (let ((proc-code 
         (compile (operator exp) 'proc 'next))
        (operand-codes
         (map (lambda (operand)
                (compile operand 'val 'next))
              (operands exp))))
    (preserving 
     '(env continue)
     proc-code
     (preserving 
      '(proc continue)
      (construct-arglist operand-codes)
      (compile-procedure-call 
       target
       linkage)))))

(define (construct-arglist operand-codes)
  (let ((operand-codes 
         (reverse operand-codes)))
    (if (null? operand-codes)
        (make-instruction-sequence 
         '() 
         '(argl)
         '((assign argl (const ()))))
        (let ((code-to-get-last-arg
               (append-instruction-sequences
                (car operand-codes)
                (make-instruction-sequence 
                 '(val)
                 '(argl)
                 '((assign argl
                           (op list)
                           (reg val)))))))
          (if (null? (cdr operand-codes))
              code-to-get-last-arg
              (preserving 
               '(env)
               code-to-get-last-arg
               (code-to-get-rest-args
                (cdr operand-codes))))))))

(define (code-to-get-rest-args operand-codes)
  (let ((code-for-next-arg
         (preserving 
          '(argl)
          (car operand-codes)
          (make-instruction-sequence 
           '(val argl)
           '(argl)
           '((assign argl
                     (op cons)
                     (reg val)
                     (reg argl)))))))
    (if (null? (cdr operand-codes))
        code-for-next-arg
        (preserving
         '(env)
         code-for-next-arg
         (code-to-get-rest-args 
          (cdr operand-codes))))))

; applying procedures
(define (compile-procedure-call target linkage)
  (let ((primitive-branch (make-label 'primitive-branch))
        (compiled-branch (make-label 'compiled-branch))
        (compound-branch (make-label 'compound-branch))                 ;;;
        (after-call (make-label 'after-call)))
    (let ((compiled-or-compound-linkage
           (if (eq? linkage 'next) after-call linkage)))
      (append-instruction-sequences
       (make-instruction-sequence '(proc) '()
        `((test (op primitive-procedure?) (reg proc))
          (branch (label ,primitive-branch))))
       (parallel-instruction-sequences
		(append-instruction-sequences                                   ;;;
		 (make-instruction-sequence '(proc) '()                         ;;;
		  `((test (op compound-procedure?) (reg proc))                  ;;;
			(branch (label ,compound-branch))))                         ;;;
		 (parallel-instruction-sequences                                ;;;
		  (append-instruction-sequences                                 ;;;
           compiled-branch                                              ;;;
           (compile-proc-appl target compiled-or-compound-linkage))     ;;;
		  (append-instruction-sequences                                 ;;;
		   compound-branch                                              ;;;
		   (compound-proc-appl target compiled-or-compound-linkage))))  ;;;
        (append-instruction-sequences
         primitive-branch
         (end-with-linkage linkage
          (make-instruction-sequence '(proc argl)
                                     (list target)
           `((assign ,target
                     (op apply-primitive-procedure)
                     (reg proc)
                     (reg argl)))))))
       after-call))))

;(define (compile-procedure-call
;         target linkage)
;  (let ((primitive-branch
;         (make-label 'primitive-branch))
;        (compound-branch
;         (make-label 'compound-branch))
;        (compiled-branch
;         (make-label 'compiled-branch))
;        (after-call
;         (make-label 'after-call)))
;    (let ((compiled-linkage
;           (if (eq? linkage 'next)
;               after-call
;               linkage)))
;      (append-instruction-sequences
;       (make-instruction-sequence
;        '(proc)
;        '()
;        `((test
;           (op primitive-procedure?)
;           (reg proc))
;          (branch
;           (label ,primitive-branch))
;          (test (op compound-procedure?)
;                (reg proc))
;          (branch (label ,compound-branch))))
;       (parallel-instruction-sequences
;        (parallel-instruction-sequences
;         (append-instruction-sequences
;          compiled-branch
;          (compile-proc-appl
;           target
;           compiled-linkage))
;         (append-instruction-sequences
;          compound-branch
;          (compound-proc-appl
;           target
;           compiled-linkage)))
;        (append-instruction-sequences
;         primitive-branch
;         (end-with-linkage
;          linkage
;          (make-instruction-sequence
;           '(proc argl)
;           (list target)
;           `((assign
;              ,target
;              (op apply-primitive-procedure)
;              (reg proc)
;              (reg argl)))))))
;       after-call))))

(define (compound-proc-appl target linkage)
  (cond ((and (eq? target 'val)
              (not (eq? linkage 'return)))
         (make-instruction-sequence
          '(proc)
          all-regs
          `((assign continue (label ,linkage))
            (save continue)
            (goto (reg compapp)))))
        ((and (not (eq? target 'val))
              (not (eq? linkage 'return)))
         (let ((proc-return
                (make-label 'proc-return)))
           (make-instruction-sequence
            '(proc)
            all-regs
            `((assign continue
                      (label ,proc-return))
              (save continue)
              (goto (reg compapp))
              ,proc-return
              (assign ,target (reg val))
              (goto (label ,linkage))))))
        ((and (eq? target 'val)
              (eq? linkage 'return))
         (make-instruction-sequence
          '(proc continue)
          all-regs
          '((save continue)
            (goto (reg compapp)))))
        ((and (not (eq? target 'val))
              (eq? linkage 'return))
         (error "return linkage,
                 target not val: COMPILE"
                target))))

(define (compile-proc-appl target linkage)
  (cond ((and (eq? target 'val)
              (not (eq? linkage 'return)))
         (make-instruction-sequence 
          '(proc)
          all-regs
          `((assign continue (label ,linkage))
            (assign 
             val 
             (op compiled-procedure-entry)
             (reg proc))
            (goto (reg val)))))
        ((and (not (eq? target 'val))
              (not (eq? linkage 'return)))
         (let ((proc-return 
                (make-label 'proc-return)))
           (make-instruction-sequence 
            '(proc)
            all-regs
            `((assign continue 
                      (label ,proc-return))
              (assign 
               val 
               (op compiled-procedure-entry)
               (reg proc))
              (goto (reg val))
              ,proc-return
              (assign ,target (reg val))
              (goto (label ,linkage))))))
        ((and (eq? target 'val)
              (eq? linkage 'return))
         (make-instruction-sequence 
          '(proc continue) 
          all-regs
          '((assign 
             val 
             (op compiled-procedure-entry)
             (reg proc))
            (goto (reg val)))))
        ((and (not (eq? target 'val))
              (eq? linkage 'return))
         (error "return linkage, 
                 target not val: COMPILE"
                target))))

(define all-regs '(env proc val argl continue))

; open-coded primitives
(define (spread-arguments argl)
  (let ((operand-code1 (compile (car argl) 'arg1 'next))
        (operand-code2 (compile (cadr argl) 'arg2 'next)))
    (list operand-code1 operand-code2)))

(define (compile-open-code exp target linkage)
  (let ((op (operator exp))
        (args (spread-arguments (operands exp))))
    (end-with-linkage
     linkage
     (preserving
      '(env)
      (cadr args)
      (preserving
       '(arg2)
       (car args)
       (make-instruction-sequence
        '(arg1 arg2)
        (list target)
        `((assign ,target (op ,op) (reg arg1) (reg arg2)))))))))

(define (compile-open-code-arbi exp target linkage)
  (let ((op (operator exp))
        (args (reverse (operands exp))))
    (let ((args-len (length args)))
      (cond
       ((< args-len 2) (error "the number of arguments must be equal to or more than 2: COMPILE"))
       (else
        (end-with-linkage
         linkage
         (preserving
          '(env)
          (compile (car args) 'arg1 'next)
          (compile-open-code-rest op (cdr args) target))))))))

(define (compile-open-code-rest operator operands target)
  (let* ((last-arg? (null? (cdr operands)))
         (trgt (if last-arg? target 'arg1))
         (once-more-code
          (preserving
           '(arg1)
           (compile (car operands) 'arg2 'next)
           (make-instruction-sequence
            '(arg1 arg2)
            `(,trgt)
            `((assign ,trgt (op ,operator)
                      (reg arg1) (reg arg2)))))))
    (if last-arg?
        once-more-code
        (preserving
         '(env)
         once-more-code
         (compile-open-code-rest operator
                                 (cdr operands)
                                 target)))))

; combining instrution sequences
(define (registers-needed s)
  (if (symbol? s) '() (car s)))
(define (registers-modified s)
  (if (symbol? s) '() (cadr s)))
(define (statements s)
  (if (symbol? s) (list s) (caddr s)))

(define (needs-register? seq reg)
  (memq reg (registers-needed seq)))
(define (modifies-register? seq reg)
  (memq reg (registers-modified seq)))

(define (append-instruction-sequences . seqs)
  (define (append-2-sequences seq1 seq2)
    (make-instruction-sequence
     (list-union 
      (registers-needed seq1)
      (list-difference 
       (registers-needed seq2)
       (registers-modified seq1)))
     (list-union
      (registers-modified seq1)
      (registers-modified seq2))
     (append (statements seq1)
             (statements seq2))))
  (define (append-seq-list seqs)
    (if (null? seqs)
        (empty-instruction-sequence)
        (append-2-sequences 
         (car seqs)
         (append-seq-list (cdr seqs)))))
  (append-seq-list seqs))

(define (list-union s1 s2)
  (cond ((null? s1) s2)
        ((memq (car s1) s2)
         (list-union (cdr s1) s2))
        (else
         (cons (car s1)
               (list-union (cdr s1) s2)))))

(define (list-difference s1 s2)
  (cond ((null? s1) '())
        ((memq (car s1) s2)
         (list-difference (cdr s1) s2))
        (else 
         (cons (car s1)
               (list-difference (cdr s1)
                                s2)))))

(define (preserving regs seq1 seq2)
  (if (null? regs)
      (append-instruction-sequences seq1 seq2)
      (let ((first-reg (car regs)))
        (if (and 
             (needs-register? seq2 first-reg)
             (modifies-register? seq1 
                                 first-reg))
            (preserving 
             (cdr regs)
             (make-instruction-sequence
              (list-union 
               (list first-reg)
               (registers-needed seq1))
              (list-difference
               (registers-modified seq1)
               (list first-reg))
              (append `((save ,first-reg))
                      (statements seq1)
                      `((restore ,first-reg))))
             seq2)
            (preserving 
             (cdr regs)
             seq1
             seq2)))))

(define (tack-on-instruction-sequence 
         seq body-seq)
  (make-instruction-sequence
   (registers-needed seq)
   (registers-modified seq)
   (append (statements seq)
           (statements body-seq))))

(define (parallel-instruction-sequences 
         seq1 seq2)
  (make-instruction-sequence
   (list-union (registers-needed seq1)
               (registers-needed seq2))
   (list-union (registers-modified seq1)
               (registers-modified seq2))
   (append (statements seq1)
           (statements seq2))))

; label
(define label-counter 0)

(define (new-label-number)
  (set! label-counter (+ 1 label-counter))
  label-counter)

(define (make-label name)
  (string->symbol
   (string-append 
    (symbol->string name)
    (number->string (new-label-number)))))

; syntax
(define (make-combination operator operands)
  (cons operator operands))

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (variable? exp) (symbol? exp))

; (quote <text-of-quotation>>)
(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp)
  (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

; (set! <var> <value>)
(define (assignment? exp)
  (tagged-list? exp 'set!))
(define (assignment-variable exp)
  (cadr exp))
(define (assignment-value exp) (caddr exp))

(define (definition? exp)
  (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)    ; (define <var> <value>)
      (caadr exp))) ; (define (<var> <param1> ... <paramn>) <body>)
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda
       (cdadr exp)   ; formal parameters
       (cddr exp)))) ; body
(define (make-define name exp)
  (list 'define name exp))

(define (lambda? exp)
  (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
; constructor used by definition-value
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))
; constructor used by cond->if
(define (make-if predicate
                 consequent
                 alternative)
  (list 'if
        predicate
        consequent
        alternative))

(define (begin? exp)
  (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))
; constructor used by cond->if
(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (cond? exp)
  (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause)
  (car clause))
(define (cond-actions clause)
  (cdr clause))
(define (apply-procedure? actions)
  (and (pair? actions)
       (eq? '=> (car actions))
       (not (null? (cdr actions)))
       (null? (cddr actions))))
(define (bad-syntax? actions)
  (and (pair? actions)
       (eq? '=> (car actions))
       (or (null? (cdr actions))
           (not (null? (cddr actions))))))
(define (actions-procedure actions)
  (cadr actions))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))
(define (expand-clauses clauses)
  (if (null? clauses)
      'false     ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp
                 (cond-actions first))
                (error "ELSE clause isn't
                        last: COND->IF"
                       clauses))
            (let ((predicate (cond-predicate first))
                  (actions (cond-actions first)))
              (make-if predicate
                       (cond
                         ((apply-procedure? actions)
                          (list (actions-procedure actions) predicate))  ; apply the procedure
                         ((bad-syntax? actions)
                          (error "COND clause syntax error! => must be followed by a procedure of one argument"))
                         (else
                          (sequence->exp actions)))
                       (expand-clauses rest)))))))

(define (let? exp)
  (tagged-list? exp 'let))
(define (named-let? exp) ; (let <var> <bindings> <body>)
  (not (pair? (cadr exp))))
(define (let-name exp)
  (if (named-let? exp)
      (cadr exp)
      #f))
(define (let-vars exp)
  (if (named-let? exp)
      (map car (caddr exp))
      (map car (cadr exp))))
(define (let-inits exp)
  (if (named-let? exp)
      (map cadr (caddr exp))
      (map cadr (cadr exp))))
(define (let-body exp)
  (if (named-let? exp)
      (cdddr exp)
      (cddr exp)))

(define (let->combination exp)
  (let ((name (let-name exp))
        (vars (let-vars exp))
        (inits (let-inits exp))
        (body (let-body exp)))
    (if name
        (sequence->exp
         (list (make-define (cons name vars) (sequence->exp body))
               (make-combination name inits)))
        (make-combination (make-lambda vars body)
                          inits))))
(define (make-let lop body)
  (cons 'let (cons lop body)))     ;Note: body needs to be sequence of expressions

(define (open-code-application? exp)
  (and (application? exp)
       (memq (operator exp) '(+ - * / =))))

(define (open-code-application-arbi? exp)
  (and (application? exp)
       (memq (operator exp) '(+ *))))


(provide compile registers-needed registers-modified statements)
