#lang sicp

(#%require (only racket provide))

(define mceval-exp '(begin
(define (eval exp env) ((analyze exp) env))

(define (analyze exp)
  (cond ((self-evaluating? exp)
         (analyze-self-evaluating exp))
        ((quoted? exp)
         (analyze-quoted exp))
        ((variable? exp)
         (analyze-variable exp))
        ((assignment? exp)
         (analyze-assignment exp))
        ((definition? exp)
         (analyze-definition exp))
        ((unbound? exp)
         (analyze-unbound exp))
        ((if? exp)
         (analyze-if exp))
        ((lambda? exp)
         (analyze-lambda exp))
        ((begin? exp)
         (analyze-sequence
          (begin-actions exp)))
        ((cond? exp)
         (analyze (cond->if exp)))
        ((and? exp)
         (analyze (and->if exp)))
        ((or? exp)
         (analyze (or->if exp)))
        ((let? exp)
         (analyze (let->combination exp)))
        ((let*? exp)
         (analyze (let*->nested-lets exp)))
        ((letrec? exp)
         (analyze (letrec->let exp)))
        ((application? exp)
         (analyze-application exp))
        (else
         (error "Unknown expression
                 type: ANALYZE"
                exp))))

(define (analyze-self-evaluating exp)
  (lambda (env) exp))

(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env) qval)))

(define (analyze-variable exp)
  (lambda (env)
    (lookup-variable-value exp env)))

(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze
                (assignment-value exp))))
    (lambda (env)
      (set-variable-value!
       var (vproc env) env)
      'ok)))

(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze
                (definition-value exp))))
    (lambda (env)
      (define-variable! var (vproc env) env)
      'ok)))

(define (analyze-unbound exp)
  (let ((var (unbound-variable exp)))
    (lambda (env)
      (unbound-variable! var env)
      'ok)))

(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env)
      (if (true? (pproc env))
          (cproc env)
          (aproc env)))))

(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (scan-out-defines
                (lambda-body exp))))
    (lambda (env)
      (make-procedure vars bproc env))))

(define (analyze-sequence exps)
  (define (sequentially proc1 proc2)
    (lambda (env) (proc1 env) (proc2 env)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc
                            (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence: ANALYZE")
        (loop (car procs) (cdr procs)))))

(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env)
      (execute-application
       (fproc env)
       (map (lambda (aproc) (aproc env))
            aprocs)))))

(define (execute-application proc args)
  (cond ((primitive-procedure? proc)
         (apply-primitive-procedure proc args))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment
           (procedure-parameters proc)
           args
           (procedure-environment proc))))
        (else (error "Unknown procedure type:
                      EXECUTE-APPLICATION"
                     proc))))

(define (applyn procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure
          procedure
          arguments))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters
            procedure)
           arguments
           (procedure-environment
            procedure))))
        (else
         (error "Unknown procedure
                 type: APPLY"
                procedure))))

(define (make-combination operator operands)
  (cons operator operands))

(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps)
         (eval (first-exp exps) env))
        (else
         (eval (first-exp exps) env)
         (eval-sequence (rest-exps exps)
                        env))))

(define (eval-assignment exp env)
  (set-variable-value!
   (assignment-variable exp)
   (eval (assignment-value exp) env)
   env)
  'ok)

(define (eval-definition exp env)
  (define-variable!
    (definition-variable exp)
    (eval (definition-value exp) env)
    env)
  'ok)

(define (eval-unbound exp env)
  (unbound-variable!
   (unbound-variable exp)
   env)
  'ok)

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (variable? exp) (symbol? exp))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp)
  (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp)
  (cadr exp))

(define (assignment-value exp) (caddr exp))

(define (make-assignment var val)
  (list 'set! var val))

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

(define (unbound? exp)
  (tagged-list? exp 'make-unbound!))

(define (unbound-variable exp)
  (cadr exp))

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
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

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

(define (and? exp)
  (tagged-list? exp 'and))
(define (and-exps exp) (cdr exp))
(define no-exps? null?)

(define (eval-and exp env)
  (eval-and-exps (and-exps exp) env))

(define (eval-and-exps exps env)
  (cond
    ((no-exps? exps) 'true)
    ((last-exp? exp) (eval (first-exp exps) env))
    ((true? (eval (first-exp exps) env)) (eval-and-exps (rest-exps exps) env))
    (else 'false)))

(define (and->if exp)
  (expand-and-exps (and-exps exp)))
(define (expand-and-exps exps)
  (if (null? exps)
      'true     ; no expressions
      (let ((first (car exps))
            (rest (cdr exps)))
        (if (no-exps? rest)  ; the last expression
            first            ; return redirectly
            (make-if first
                     (expand-and-exps rest)
                     'false)))))

(define (or? exp)
  (tagged-list? exp 'or))
(define (or-exps exp) (cdr exp))

(define (eval-or exp env)
  (eval-or-exps (and-exps exp) env))

(define (eval-or-exps exps env)
  (cond
    ((no-exps? exps) 'false)
    ((last-exp? exp) (eval (first-exp exps) env))
    ((true? (eval (first-exp exps) env)) (eval (first-exp exps) env))
    (else (eval-and-exps (rest-exps exps) env))))

(define (or->if exp)
  (expand-or-exps (or-exps exp)))
(define (expand-or-exps exps)
  (if (null? exps)
      'false     ; no expressions
      (let ((first (car exps))
            (rest (cdr exps)))
        (if (no-exps? rest)  ; the last expression
            first            ; return redirectly
            (make-if first
                     first
                     (expand-or-exps rest))))))

(define (let? exp)
  (tagged-list? exp 'let))
(define (named-let? exp) ; (let <var> <bindings> <body>)
  (not (pair? (cadr exp))))
(define (let-name exp)
  (if (named-let? exp)
      (cadr exp)
      false))
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

(define (let*? exp)
  (tagged-list? exp 'let*))
(define (let*-lop exp)
  (cadr exp))
(define (let*-body exp)
  (cddr exp))
(define (let*->nested-lets exp)
  (let ((lop (let*-lop exp))
        (body (let*-body exp)))
    (define (iter lop)
      (cond
       ((null? lop) (sequence->exp body)) ; 统一转成非list,因为make-let返回的不是list
       (else (make-let (list (car lop))
                       (list (iter (cdr lop)))))))
    (iter lop)))

(define (letrec? exp)
  (tagged-list? exp 'letrec))
(define (letrec-lop exp)
  (cadr exp))
(define (letrec-body exp)
  (cddr exp))
(define (declare-variables exp)
  (map (lambda (x) (list (car x) ''*unassigned*))
       (letrec-lop exp)))
(define (set-variables exp)
  (map (lambda (x) (make-assignment (car x) (cadr x)))
       (letrec-lop exp)))
(define (letrec->let exp)
  (make-let (declare-variables exp)
            (append (set-variables exp) (letrec-body exp))))


; evaluator data structures
(define (true? x)
  (not (eq? x false)))

(define (false? x)
  (eq? x false))

; primitive procedure objects
(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc)
  (cadr proc))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list '> >)
        (list '< <)
        (list '= =)
        ))
;⟨more primitives⟩

(define (primitive-procedure-names)
  (map car primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc)
         (list 'primitive (cadr proc)))
       primitive-procedures))

(define (apply-primitive-procedure proc args)
  (apply
   (primitive-implementation proc) args))

; compound procedures
(define (make-procedure parameters body env)
  ;(list 'procedure parameters (scan-out-defines body) env))
  (list 'procedure parameters body env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

(define (scan-out-defines body)
  (define (iter seq)
    (cond
     ((null? seq) '(() ()))
     (else (let ((exp (car seq))
                 (res (iter (cdr seq))))
             (if (definition? exp)
                 (let ((var (definition-variable exp))
                       (val (definition-value exp)))
                   (list (cons (make-assignment var val)
                               (car res))
                         (cons (list var ''*unassigned*) (cadr res))))
                 (list (cons exp (car res)) (cadr res)))))))
  (let ((res (iter body)))
    (let ((newbody (car res))
          (vvs (cadr res)))
      (if (null? vvs)
          (analyze-sequence newbody)
          (list (make-let vvs (analyze-sequence newbody)))))))

; operations on environments
; 为了实现这些操作,我们将环境表示为一个帧的list,一个环境的enclosing环境是这个list的cdr,空环境是空list。
(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

; 每个帧表达式为一个lists的pair,一个变量list和一个关联值的list。
(define (make-frame variables values)
  (cons variables values))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (set-frame-variables! frame vars)
  (set-car! frame vars))
(define (set-frame-values! frame vals)
  (set-cdr! frame vals))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (frame-variables frame)))
  (set-cdr! frame (cons val (frame-values frame))))
(define (first-variable vars) (car vars))
(define (rest-variables vars) (cdr vars))

(define (first-value vals) (car vals))
(define (rest-values vals) (cdr vals))
(define (set-first-value! vals val) (set-car! vals val))
(define (length-variables vars) (length vars))
(define (length-values vals) (length vals))

; 扩展环境,加一帧
(define (extend-environment vars vals base-env)
  (if (= (length-variables vars) (length-values vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length-variables vars) (length-values vals))
          (error "Too many arguments supplied"
                 vars
                 vals)
          (error "Too few arguments supplied"
                 vars
                 vals))))

(define (env-loop env var next op)
  (define (iter env)
    (define (scan vars vals)
      (cond ((null? vars)
             (if next
                 (next env)
                 (iter (enclosing-environment env))))
            ((eq? var (first-variable vars))
             (op vals))
            (else (scan (rest-variables vars)
                        (rest-values vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (iter env))

(define (lookup-variable-value var env)
  (env-loop env var false
            (lambda (vals)
              (let ((val (first-value vals)))
                (if (eq? '*unassigned* val)
                    (error "use variable before it's defined" var)
                    val)))))

(define (set-variable-value! var val env)
  (env-loop env var false
            (lambda (vals) (set-first-value! vals val))))

(define (define-variable! var val env)
  (env-loop env var
            (lambda (env)
              (add-binding-to-frame! var val
                                     (first-frame env)))
            (lambda (vals)
              (set-first-value! vals val))))

(define (unbound-variable! var env)
  (define (unbound vars vals)
    (cond ((null? vars) (make-frame '() '()))
          ((eq? var (first-variable vars))
           (make-frame (rest-variables vars) (rest-values vals)))
          (else
           (let ((res (unbound (rest-variables vars) (rest-values vals))))
             (add-binding-to-frame! (first-variable vars)
                                    (first-value vals)
                                    res)
             res))))
  (if (not (eq? env the-empty-environment))
      (let* ((frame (first-frame env))
             (res (unbound (frame-variables frame)
                           (frame-values frame))))
        (set-frame-variables! frame (frame-variables res))
        (set-frame-values! frame (frame-values res)))))

; primitive and global env
(define (setup-environment)
  (let ((initial-env
         (extend-environment
          (primitive-procedure-names)
          (primitive-procedure-objects)
          the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(define the-global-environment
  (setup-environment))

; read-eval-print loop
(define input-prompt  ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output
           (eval input
                 the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))  ; avoid printing the env part of a compund procedure
  (driver-loop))

(define (prompt-for-input string)
  (newline) (newline)
  (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display
       (list 'compound-procedure
             (procedure-parameters object)
             (procedure-body object)
             '<procedure-env>))
      (display object)))

(driver-loop)
    ))

(provide mceval-exp)
