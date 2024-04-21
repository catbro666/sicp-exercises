#lang racket

(require rnrs/mutable-pairs-6)
(require compatibility/mlist)

; Core of Evaluator: eval/apply
(define (ambeval exp env succeed fail)
  ((analyze exp) env succeed fail))

(define (analyze exp)
  (cond ((self-evaluating? exp)
         (analyze-self-evaluating exp))
        ((quoted? exp) 
         (analyze-quoted exp))
        ((variable? exp) 
         (analyze-variable exp))
        ((assignment? exp)
         (analyze-assignment exp))
        ((permanent-assignment? exp)
         (analyze-permanent-assignment exp))
        ((definition? exp) 
         (analyze-definition exp))
        ((unbound? exp)
         (analyze-unbound exp))
        ((if? exp) 
         (analyze-if exp))
        ((if-fail? exp) 
         (analyze-if-fail exp))
        ((must-fail? exp)
         (analyze-must-fail exp))
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
        ((amb? exp) (analyze-amb exp))
        ((ramb? exp) (analyze-ramb exp))
        ((count? exp) (analyze-count exp))
        ((call/cc? exp) (analyze-call/cc exp))
        ((application? exp) 
         (analyze-application exp))
        (else
         (error "Unknown expression 
                 type: ANALYZE" 
                exp))))

(define (analyze-self-evaluating exp)
  (lambda (env succeed fail)
    (succeed exp fail)))

(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env succeed fail)
      (succeed qval fail))))

(define (analyze-variable exp)
  (lambda (env succeed fail)
    (succeed (lookup-variable-value exp env)
             fail)))

; 赋值是我们第一次真正使用延续的地方
(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze 
                (assignment-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)    ; *1*
               (let ((old-value
                      (lookup-variable-value-ex 
                       var 
                       env)))
                 (set-variable-value!
                  var 
                  val 
                  env)
                 (succeed 
                  'ok
                  (lambda ()    ; *2*
                    (set-variable-value! 
                     var
                     old-value
                     env)
                    (fail2)))))
             fail))))

(define (analyze-permanent-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze 
                (assignment-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)    ; *1*
               (set-variable-value! var val env)
               (succeed 'ok fail2))
             fail))))

; 我们不用担心撤销定义,因为我们假设内部定义已经被扫描出去了。
(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze 
                (definition-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)
               (define-variable! var val env)
               (succeed 'ok fail2))
             fail))))

; 只unbound当前帧的,否则在撤销的时候需要知道是在哪个帧
(define (analyze-unbound exp)
  (let ((var (unbound-variable exp)))
    (lambda (env succeed fail)
      (let ((old-value (lookup-variable-value var env)))
        (unbound-variable! var env)
        (succeed 'ok
                 (lambda ()
                   (define-variable! var old-value env)
                   (fail)))))))

; if 语句里也可能有amb,会失败。
(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env succeed fail)
      (pproc env
             ;; success continuation for evaluating
             ;; the predicate to obtain pred-value
             (lambda (pred-value fail2)
               (if (true? pred-value)
                   (cproc env succeed fail2)
                   (aproc env succeed fail2)))
             ;; failure continuation for
             ;; evaluating the predicate
             fail))))

;(define (analyze-lambda exp)
;  (let ((vars (lambda-parameters exp))
;        (bproc (analyze-sequence 
;                (lambda-body exp))))
;    (lambda (env) 
;      (make-procedure vars bproc env))))

(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (scan-out-defines 
                (lambda-body exp))))
    (lambda (env succeed fail)
      (succeed (make-procedure vars bproc env)
               fail))))

(define (analyze-sequence exps)
  (define (sequentially a b)
    (lambda (env succeed fail)
      (a env
         ;; success continuation for calling a
         (lambda (a-value fail2)
           (b env succeed fail2))
         ;; failure continuation for calling a
         fail)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc 
                            (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence: ANALYZE")
        (void))
    (loop (car procs) (cdr procs))))

(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env succeed fail)
      (fproc env
             (lambda (proc fail2)
               (get-args aprocs env
                         (lambda (args fail3)
                           (execute-application
                            proc args succeed fail3))
                         fail2))
             fail))))

(define (get-args aprocs env succeed fail)
  (if (null? aprocs)
      (succeed '() fail)
      ((car aprocs) 
       env
       ;; success continuation for this aproc
       (lambda (arg fail2)
         (get-args 
          (cdr aprocs)
          env
          ;; success continuation for
          ;; recursive call to get-args
          (lambda (args fail3)
            (succeed (cons arg args)
                     fail3))
          fail2))
       fail)))

(define (execute-application 
         proc args succeed fail)
  (cond ((primitive-procedure? proc)
         (succeed 
          (apply-primitive-procedure 
           proc args)
          fail))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment 
           (procedure-parameters proc)
           args
           (procedure-environment proc))
          succeed
          fail))
        (else (error "Unknown procedure type: 
                      EXECUTE-APPLICATION"
                     proc))))

(define (analyze-amb exp)
  (let ((cprocs (map analyze (amb-choices exp))))
    (lambda (env succeed fail)
      (define (try-next choices)
        (if (null? choices)
            (fail)
            ((car choices)
             env
             succeed
             (lambda ()
               (try-next (cdr choices))))))
      (try-next cprocs))))

(define (shuffle lst) 
  (map cdr 
       (sort 
        (map (lambda (x) (cons (random) x)) lst) 
        (lambda (x y) (< (car x) (car y))))))

(define (analyze-ramb exp)
  (let ((cprocs (map analyze (ramb-choices exp))))
    (lambda (env succeed fail)
      (define (try-next choices)
        (if (null? choices)
            (fail)
            ((car choices)
             env
             succeed
             (lambda ()
               (try-next (cdr choices))))))
      (try-next (shuffle cprocs))))) ; shuffle at runtime

(define (analyze-if-fail exp)
  (let ((proc1 (analyze (if-fail-exp1 exp)))
        (proc2 (analyze (if-fail-exp2 exp))))
    (lambda (env succeed fail)
      (proc1 env
             succeed
             (lambda ()
               (proc2 env succeed fail))))))

(define (analyze-must-fail exp)
  (let ((proc (analyze (must-fail-exp exp))))
    (lambda (env succeed fail)
      (proc env
            (lambda (val fail2)
              (fail))
            (lambda ()
              (succeed 'ok fail))))))

(define (analyze-count exp)
  (let ((num (count-number exp))
        (proc (analyze (count-exp exp))))
    (lambda (env succeed fail)
      (let ((count 0))
        (proc env
              (lambda (val try-again)
                (set! count (+ count 1))
                (if (> count num)
                    (fail)        ; count has exceed, fail
                    (try-again))) ; otherwise try again
              (lambda ()
                (if (= count num)
                    (succeed 'ok fail) ; count precisely equals to num, succeed
                    (fail))))))))

(define (analyze-call/cc exp)
  (let ((lproc (analyze (call/cc-lambda exp))))
    (lambda (env succeed fail)
      (lproc env
             (lambda (proc fail2)
               (execute-application
                ; pass the outer succeed/fail by the continuation lambda
                proc (list (make-procedure '(v)
                                           (lambda (env s f) ;ignore s f
                                             (succeed (lookup-variable-value 'v env) fail))
                                           env))
                succeed fail2))
             fail))))

(define (make-combination operator operands)
  (cons operator operands))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

; from left to right
;(define (list-of-values exps env)
;  (if (no-operands? exps)
;      '()
;      (let ((first (eval (first-operand exps) env)))
;        (cons first (list-of-values (rest-operands exps) env)))))

; from right to left
;(define (list-of-values exps env)
;  (if (no-operands? exps)
;      '()
;      (let ((rest (list-of-values (rest-operand exps) env)))
;        (cons (eval (first-operands exps) env) rest))))

; Representing Expressions
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

(define (make-assignment var val)
  (list 'set! var val))

(define (permanent-assignment? exp)
  (tagged-list? exp 'permanent-set!))

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

(define (amb? exp) (tagged-list? exp 'amb))
(define (amb-choices exp) (cdr exp))

(define (ramb? exp) (tagged-list? exp 'ramb))
(define (ramb-choices exp) (cdr exp))

(define (if-fail? exp) (tagged-list? exp 'if-fail))
(define (if-fail-exp1 exp) (cadr exp))
(define (if-fail-exp2 exp) (caddr exp))

(define (must-fail? exp) (tagged-list? exp 'must-fail))
(define (must-fail-exp exp) (cadr exp))
(define (count? exp) (tagged-list? exp 'count=))
(define (count-number exp) (cadr exp))
(define (count-exp exp) (caddr exp))
(define (call/cc? exp) (tagged-list? exp 'call/cc))
(define (call/cc-lambda exp) (cadr exp))

; data-directed style
;(put 'eval 'quote (lambda (exp env)
;                    (text-of-quotation exp)))
;(put 'eval 'set! eval-assignment)
;(put 'eval 'define eval-definition)
;(put 'eval 'make-unbound! eval-unbound)
;(put 'eval 'if eval-if)
;(put 'eval 'lambda (lambda (exp env)
;                     (make-procedure
;                      (lambda-parameters exp)
;                      (lambda-body exp)
;                      env)))
;(put 'eval 'begin (lambda (exp env)
;                    (eval-sequence
;                     (begin-actions exp)
;                     env)))
;(put 'eval 'cond (lambda (exp env)
;                   (evaln (cond->if exp) env)))
;(put 'eval 'and (lambda (exp env)
;                   (evaln (and->if exp) env)))
;(put 'eval 'or (lambda (exp env)
;                   (evaln (or->if exp) env)))
;(put 'eval 'let (lambda (exp env)
;                   (evaln (let->combination exp) env)))
;(put 'eval 'let* (lambda (exp env)
;                    (evaln (let->combination (let*->nested-lets exp)) env)))
;(put 'eval 'letrec (lambda (exp env)
;                     (evaln (letrec->let exp) env)))
;
;(define (evaln exp env)
;  (cond ((self-evaluating? exp) exp)
;        ((variable? exp)
;         (lookup-variable-value exp env))
;        ((and (pair? exp) (get 'eval (car exp)))
;         ((get 'eval (car exp)) exp env))
;        ((application? exp)
;         (applyn (eval (operator exp) env)
;                (list-of-values 
;                 (operands exp) 
;                 env)))
;        (else (error "Unknown expression 
;                 type: EVAL" exp))))

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

(define (original-set! var val)
  (set! var val))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cadr cadr)
        (list 'cddr cddr)
        (list 'caddr caddr)
        (list 'cons cons)
        (list 'null? null?)
        (list 'list list)
        (list 'list-ref list-ref)
        (list 'pair? pair?)
        (list 'append append)
        (list 'original-set! original-set!)
        (list 'not not)
        (list 'void void)
        (list 'eq? eq?)
        (list 'equal? equal?)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list '> >)
        (list '< <)
        (list '= =)
        (list '<= <=)
        (list '>= >=)
        (list 'sqrt sqrt)
        (list 'ceiling ceiling)
        (list 'floor floor)
        (list 'integer? integer?)
        (list 'symbol? symbol?)
        (list 'number? number?)
        (list 'member member)
        (list 'memq memq)
        (list 'abs abs)
        (list 'range range)
        (list 'sort sort)
        (list 'random random)
        (list 'even? even?)
        (list 'newline newline)
        (list 'display display)
        (list 'eval eval)
        (list 'apply apply)
        (list 'symbol->string symbol->string)
        (list 'string->symbol string->symbol)
        (list 'string=? string=?)
        (list 'substring substring)
        (list 'string-length string-length)
        (list 'string-append string-append)
        (list 'number->string number->string)
        (list 'assoc assoc)
        (list 'make-base-namespace make-base-namespace)
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
          (analyze (make-let vvs newbody))))))

; operations on environments
; 为了实现这些操作,我们将环境表示为一个帧的list,一个环境的enclosing环境是这个list的cdr,空环境是空list。
(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

; 每个帧表达式为一个lists的pair,一个变量list和一个关联值的list。
(define (make-frame variables values)
  (mcons variables values))
(define (frame-variables frame) (mcar frame))
(define (frame-values frame) (mcdr frame))
(define (set-frame-variables! frame vars)
  (set-mcar! frame vars))
(define (set-frame-values! frame vals)
  (set-mcdr! frame vals))
(define (add-binding-to-frame! var val frame)
  (set-mcar! frame (mcons var (frame-variables frame)))
  (set-mcdr! frame (mcons val (frame-values frame))))
(define (first-variable vars) (mcar vars))
(define (rest-variables vars) (mcdr vars))
(define (first-value vals) (mcar vals))
(define (rest-values vals) (mcdr vals))
(define (set-first-value! vals val) (set-mcar! vals val))
(define (length-variables vars) (mlength vars))
(define (length-values vals) (mlength vals))

; 扩展环境,加一帧
(define (extend-environment vars vals base-env)
  (let ((vars (if (pair? vars) (list->mlist vars) vars))
        (vals (if (pair? vals) (list->mlist vals) vals)))
    (if (= (length-variables vars) (length-values vals))
        (cons (make-frame vars vals) base-env)
        (if (< (length-variables vars) (length-values vals))
            (error "Too many arguments supplied" 
                   vars 
                   vals)
            (error "Too few arguments supplied" 
                   vars 
                   vals)))))

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
  (env-loop env var #false
            (lambda (vals)
              (let ((val (first-value vals)))
                (if (eq? '*unassigned* val)
                    (error "use variable before it's defined" var)
                    val)))))

(define (lookup-variable-value-ex var env)
  (env-loop env var #false
            (lambda (vals)
              (first-value vals))))

(define (set-variable-value! var val env)
  (env-loop env var #false
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
  (if (eq? env the-empty-environment)
      (error "variable doesn't exist")
      (let* ((frame (first-frame env))
             (res (unbound (frame-variables frame)
                           (frame-values frame))))
        (set-frame-variables! frame (frame-variables res))
        (set-frame-values! frame (frame-values res)))))

; 每个帧表达式为一个lists的pair,一个变量list和一个关联值的list。
;(define (make-frame variables values)
;  (cons 'frame
;        (map cons variables values)))
;(define (frame-bindings frame)
;  (cdr frame))
;(define (add-binding-to-frame! var val frame)
;  (set-cdr! frame (cons (cons var val)
;                        (frame-bindings frame))))
;
;(define (env-loop env var next op)
;  (define (iter env)
;    (define (scan bindings)
;      (cond ((null? bindings)
;             (if next
;                 (next op)
;                 (iter (enclosing-environment env))))
;            ((eq? var (caar bindings))
;             (op bindings))
;            (else (scan (cdr bindings)))))
;    (if (eq? env the-empty-environment)
;        (error "Unbound variable" var)
;        (scan (frame-bindings (first-frame env)))))
;  (iter env))
;
;(define (lookup-variable-value var env)
;  (env-loop env var #false cdar))
;
;(define (set-variable-value! var val env)
;  (env-loop env var #false
;            (lambda (bindings)
;              (set-cdr! (car bindings) val))))
;
;(define (define-variable! var val env)
;  (env-loop env var
;            (lambda (env)
;              (add-binding-to-frame!
;               var val (first-frame env)))
;            (lambda (bindings)
;              (set-cdr! (car bindings) val))))
;
;(define (unbound-variable! var env)
;  (define (unbound bindings)
;    (cond ((null? bindings) '())
;          ((eq? var (caar bindings))
;           (cdr bindings))
;          (else
;           (let ((res (unbound (cdr bindings))))
;             (cons (car bindings) res)))))
;  (if (eq? env the-empty-environment)
;      (void) ; don't raise error
;      (let* ((frame (first-frame env))
;             (res (unbound (frame-bindings frame))))
;        (set-cdr! frame res))))

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

; standard procedures
(define standard-procedures
  (list
   '(define (require p)
      (if (not p) (amb) (void)))
   '(define (list-amb l)
      (if (null? l)
          (amb)
          (amb (car l) (list-amb (cdr l)))))
   '(define (map proc l)
      (if (null? l)
          '()
          (cons (proc (car l))
                (map proc (cdr l)))))
   '(define (ormap proc l)
      (cond
        ((null? l) false)
        (else (let ((res (proc (car l))))
                (if res
                    res
                    (ormap proc (cdr l)))))))
   '(define (filter proc l)
      (cond
        ((null? l) '())
        (else (if (proc (car l))
                  (cons (car l) (filter proc (cdr l)))
                  (filter proc (cdr l))))))
   '(define (for-each proc l)
      (cond
        ((null? l) (void))
        (else (proc (car l)) (for-each proc (cdr l)))))
   ))

(map (lambda (exp)
       (ambeval exp the-global-environment
                (lambda (val fail) 'ok)
                (lambda () (error "fail to define"))))
     standard-procedures)

; read-eval-print loop
(define input-prompt  ";;; Amb-Eval input:")
(define output-prompt ";;; Amb-Eval value:")

(define (driver-loop)
  (define (internal-loop try-again)
    (prompt-for-input input-prompt)
    (let ((input (read)))
      (if (eq? input 'try-again)
          (try-again)
          (begin
            (newline)
            (display 
             ";;; Starting a new problem ")
            (ambeval 
             input
             the-global-environment
             ;; ambeval success
             (lambda (val next-alternative)
               (announce-output 
                output-prompt)
               (user-print val)
               (internal-loop 
                next-alternative))
             ;; ambeval failure
             (lambda ()
               (announce-output
                ";;; There are no 
                 more values of")
               (user-print input)
               (driver-loop)))))))
  (internal-loop
   (lambda ()
     (newline)
     (display 
      ";;; There is no current problem")
     (driver-loop))))


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