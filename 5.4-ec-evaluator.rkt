#lang sicp

;(#%require rackunit)

(#%require "5.2-register-machine-simulator.rkt")
(#%require "5.4-eceval-operations.rkt")
(#%require "5.5-compiler-lexical-addressing.rkt")
;(#%require "5.5-compiler.rkt")
(#%require "5.5-ambeval-exp.rkt")
(#%require "5.5-mceval-exp.rkt")

(define (compile-and-run expression)
  (let ((stack (stack eceval))
        (instructions
         (assemble
          (append
           (statements
            (compile expression 'val 'next
                     the-empty-cenv
                     ))
           '((restore continue)
             (goto (reg continue))))
          eceval)))
    ; the next instruction in primitive-apply are
    ; (restore continue)
    ; (goto (reg continue))
    (push stack instructions))) ; as continue

(add-primitives
 (list
  (list 'compile-and-run compile-and-run)
  ))

(reset-global-environment)

(define (compile-and-assemble expression)
  (assemble
   (statements
    (compile expression 'val 'return
             the-empty-cenv
             ))
   eceval))

(define all-operations
  (append eceval-operations
          (list
           (list 'compile-and-assemble compile-and-assemble)
           )))

(define eceval
  (make-machine
   all-operations
   '(
  (assign compapp (label compound-apply))
  (branch (label external-entry))
;read-eval-print-loop
;  (perform (op initialize-stack))
;  (perform (op prompt-for-input)
;           (const ";;; EC-Eval input:"))
;  (assign exp (op read))
;  (assign env (op get-global-environment))
;  (assign continue (label print-result))
;  (goto (label eval-dispatch))
read-compile-execute-print-loop
  (perform (op initialize-stack))
  (perform (op prompt-for-input)
           (const ";;; EC-Eval input:"))
  (assign exp (op read))
  (assign val (op compile-and-assemble) (reg exp))
  (assign env (op get-global-environment))
  (assign continue (label print-result))
  (goto (reg val))
print-result
  ; DO STACK STATISTICS
  (perform (op print-stack-statistics))
  (perform (op announce-output)
           (const ";;; EC-Eval value:"))
  (perform (op user-print) (reg val))
  ;(goto (label read-eval-print-loop))
  (goto (label read-compile-execute-print-loop))
  
eval-dispatch
  (test (op self-evaluating?) (reg exp))
  (branch (label ev-self-eval))
  (test (op variable?) (reg exp))
  (branch (label ev-variable))
  (test (op quoted?) (reg exp))
  (branch (label ev-quoted))
  (test (op assignment?) (reg exp))
  (branch (label ev-assignment))
  (test (op definition?) (reg exp))
  (branch (label ev-definition))
  (test (op if?) (reg exp))
  (branch (label ev-if))
  (test (op cond?) (reg exp))
  (branch (label ev-cond))
  (test (op and?) (reg exp))
  (branch (label ev-and))
  (test (op or?) (reg exp))
  (branch (label ev-or))
  (test (op let?) (reg exp))
  (branch (label ev-let))
  (test (op let*?) (reg exp))
  (branch (label ev-let*))
  (test (op letrec?) (reg exp))
  (branch (label ev-letrec))
  (test (op lambda?) (reg exp))
  (branch (label ev-lambda))
  (test (op begin?) (reg exp))
  (branch (label ev-begin))
  (test (op application?) (reg exp))
  (branch (label ev-application))
  (goto (label unknown-expression-type))

     ; simple expressions
ev-self-eval
  (assign val (reg exp))
  (goto (reg continue))
ev-variable
  (assign val
          (op lookup-variable-value)
          (reg exp)
          (reg env))
  (goto (reg continue))
ev-quoted
  (assign val
          (op text-of-quotation)
          (reg exp))
  (goto (reg continue))
ev-lambda
  (assign unev
          (op lambda-parameters)
          (reg exp))
  (assign exp 
          (op lambda-body)
          (reg exp))
  (assign val 
          (op make-procedure)
          (reg unev)
          (reg exp)
          (reg env))
  (goto (reg continue))

; evaluatin procedure application
ev-application
  (save continue)
  (assign unev (op operands) (reg exp))
  (assign exp (op operator) (reg exp))
  (test (op symbol?) (reg exp))
  (branch (label ev-appl-operator-symbol))
  (save env)
  (save unev)
  (assign
   continue (label ev-appl-did-operator))
  (goto (label eval-dispatch))
ev-appl-operator-symbol
  (assign
   continue (label ev-appl-did-operator-symbol))
  (goto (label eval-dispatch)) 

ev-appl-did-operator
  (restore unev)             ; the operands
  (restore env)
ev-appl-did-operator-symbol
  (assign argl (op empty-arglist))
  (assign proc (reg val))    ; the operator
  (test (op no-operands?) (reg unev))
  (branch (label apply-dispatch))
  (save proc)

ev-appl-operand-loop
  (save argl)
  (assign exp
          (op first-operand)
          (reg unev))
  (test (op last-operand?) (reg unev))
  (branch (label ev-appl-last-arg))
  (save env)
  (save unev)
  (assign continue 
          (label ev-appl-accumulate-arg))
  (goto (label eval-dispatch))

ev-appl-accumulate-arg
  (restore unev)
  (restore env)
  (restore argl)
  (assign argl 
          (op adjoin-arg)
          (reg val)
          (reg argl))
  (assign unev
          (op rest-operands)
          (reg unev))
  (goto (label ev-appl-operand-loop))

ev-appl-last-arg
  (assign continue 
          (label ev-appl-accum-last-arg))
  (goto (label eval-dispatch))
ev-appl-accum-last-arg
  (restore argl)
  (assign argl 
          (op adjoin-arg)
          (reg val)
          (reg argl))
  (restore proc)
  (goto (label apply-dispatch)) ; apply-dispatch
  
; procedure application
apply-dispatch
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-apply))
  (test (op compound-procedure?) (reg proc))
  (branch (label compound-apply))
  (test (op compiled-procedure?) (reg proc))
  (branch (label compiled-apply))
  (goto (label unknown-procedure-type))

primitive-apply
  (assign val (op apply-primitive-procedure)
              (reg proc)
              (reg argl))
  (restore continue)
  (goto (reg continue))

compound-apply
  (assign unev 
          (op procedure-parameters)
          (reg proc))
  (assign env
          (op procedure-environment)
          (reg proc))
  (assign env
          (op extend-environment)
          (reg unev)
          (reg argl)
          (reg env))
  (assign unev
          (op procedure-body)
          (reg proc))
  (goto (label ev-sequence))

compiled-apply
  (restore continue)
  (assign val
          (op compiled-procedure-entry)
          (reg proc))
  (goto (reg val))

ev-begin
  (assign unev
          (op begin-actions)
          (reg exp))
  (save continue)  ; because in ev-sequnce it doesn't save continue
  (goto (label ev-sequence))

ev-sequence
  (assign exp (op first-exp) (reg unev))
  (test (op last-exp?) (reg unev))
  (branch (label ev-sequence-last-exp))
  (save unev)
  (save env)
  (assign continue
          (label ev-sequence-continue))
  (goto (label eval-dispatch))
ev-sequence-continue
  (restore env)
  (restore unev)
  (assign unev
          (op rest-exps)
          (reg unev))
  (goto (label ev-sequence))
ev-sequence-last-exp
  (restore continue)
  (goto (label eval-dispatch)) ; tail-recursive

; doesn't take advantage of the tail-recursive
;ev-sequence
;  (test (op no-more-exps?) (reg unev))
;  (branch (label ev-sequence-end))
;  (assign exp (op first-exp) (reg unev))
;  (save unev)
;  (save env)
;  (assign continue
;          (label ev-sequence-continue))
;  (goto (label eval-dispatch))
;ev-sequence-continue
;  (restore env)
;  (restore unev)
;  (assign unev (op rest-exps) (reg unev))
;  (goto (label ev-sequence))
;ev-sequence-end
;  (restore continue)
;  (goto (reg continue))

ev-if
  (save exp)   ; save expression for later
  (save env)
  (save continue)
  (assign continue (label ev-if-decide))
  (assign exp (op if-predicate) (reg exp))
  ; evaluate the predicate:
  (goto (label eval-dispatch))

ev-if-decide
  (restore continue)
  (restore env)
  (restore exp)
  (test (op true?) (reg val))
  (branch (label ev-if-consequent))
ev-if-alternative
  (assign exp (op if-alternative) (reg exp))
  (goto (label eval-dispatch))
ev-if-consequent
  (assign exp (op if-consequent) (reg exp))
  (goto (label eval-dispatch))

ev-assignment
  (assign unev 
          (op assignment-variable)
          (reg exp))
  (save unev)   ; save variable for later
  (assign exp
          (op assignment-value)
          (reg exp))
  (save env)
  (save continue)
  (assign continue
          (label ev-assignment-1))
  ; evaluate the assignment value:
  (goto (label eval-dispatch))  
ev-assignment-1
  (restore continue)
  (restore env)
  (restore unev)
  (perform (op set-variable-value!)
           (reg unev)
           (reg val)
           (reg env))
  (assign val
          (const ok))
  (goto (reg continue))

ev-definition
  (assign unev 
          (op definition-variable)
          (reg exp))
  (save unev)   ; save variable for later
  (assign exp 
          (op definition-value)
          (reg exp))
  (save env)
  (save continue)
  (assign continue (label ev-definition-1))
  ; evaluate the definition value:
  (goto (label eval-dispatch))  
ev-definition-1
  (restore continue)
  (restore env)
  (restore unev)
  (perform (op define-variable!)
           (reg unev)
           (reg val)
           (reg env))
  (assign val (const ok))
  (goto (reg continue))

ev-cond
 (assign exp (op cond->if) (reg exp))
 (goto (label ev-if))
ev-and
 (assign exp (op and->if) (reg exp))
 (goto (label ev-if))
ev-or
 (assign exp (op or->if) (reg exp))
 (goto (label ev-if))
 
ev-let
 (assign exp (op let->combination) (reg exp))
 (goto (label eval-dispatch))
ev-let*
 (assign exp (op let*->nested-lets) (reg exp))
 (goto (label ev-let))
ev-letrec
 (assign exp (op letrec->let) (reg exp))
 (goto (label ev-let))

;ev-cond
; (assign unev (op cond-clauses) (reg exp))
;loop
; (test (op null?) (reg unev))
; (branch (label no-else))
; (assign exp (op car) (reg unev)) ; first clause
; (test (op cond-else-clause?) (reg exp))
; (branch (label else-clause))
; 
; (save continue)
; (save env)
; (save unev)  ; clauses
; (save exp)   ; save clause for later
; (assign continue (label after-predicate))
; (assign exp (op cond-predicate) (reg exp))
; (goto (label eval-dispatch))
;after-predicate
; (restore exp) ; restore clause
; (restore unev)
; (restore env)
; (restore continue)
; (test (op true?) (reg val))       ; test predicate
; (branch (label actions))
; (assign unev (op cdr) (reg unev)) ; rest clauses
; (goto (label loop))
;actions
; (assign unev (op cond-actions) (reg exp))
; (test (op apply-procedure?) (reg unev))
; (branch (label procedure-action))
; (test (op bad-syntax?) (reg unev))
; (branch (label bad-syntax))
; (save continue)  ; because in ev-sequnce it doesn't save continue
; (goto (label ev-sequence))       ; directly eval the actions
;bad-syntax
; (assign val (const "COND clause syntax error!"))
; (goto (label signal-error))
;procedure-action
; (assign argl (op list) (reg val))
; (save continue)
; (save env)
; (save argl)
; (assign exp (op actions-procedure) (reg unev))
; (assign continue (label after-operator))
; (goto (label eval-dispatch))
;after-operator
; (restore argl)             ; the operands
; (restore env)
; (restore continue)        ; save again
; (assign proc (reg val))    ; the operator
; (goto (label apply-dispatch))
;else-clause
; (assign unev (op cdr) (reg unev)) ;rest clause
; (test (op null?) (reg unev))
; (branch (label else-actions))
; (assign val (const "ELSE clause isn't last: COND"))
; (goto (label signal-error))
;else-actions
; (assign unev (op cond-actions) (reg exp))
; (save continue)  ; because in ev-sequnce it doesn't save continue
; (goto (label ev-sequence))
;no-else
; (assign val (const false))
; (goto (reg continue))

unknown-expression-type
  (assign 
   val
   (const unknown-expression-type-error))
  (goto (label signal-error))
unknown-procedure-type
  ; clean up stack (from apply-dispatch):
  (restore continue)    
  (assign 
   val
   (const unknown-procedure-type-error))
  (goto (label signal-error))
signal-error
  (perform (op user-print) (reg val))
  ;(goto (label read-eval-print-loop))
  (goto (label read-compile-execute-print-loop))

external-entry
  (perform (op initialize-stack))
  (assign env (op get-global-environment))
  (assign continue (label print-result))
  (goto (reg val))
  )))

(define (start-eceval)
  (set-register-contents! eceval 'flag false)
  (start eceval))

(define (compile-and-go expression)
  (let ((instructions
         (assemble 
          (statements
           (compile 
            expression 'val 'return
            the-empty-cenv
            ))
          eceval)))
    ;(for-each (lambda (x) (display (car x)) (newline)) instructions)
    (set-register-contents! 
     eceval 'val instructions)
    (set-register-contents! 
     eceval 'flag true)
    (start eceval)))


(define stdlib
  '(begin
    (define (map proc l)
      (if (null? l)
          '()
          (cons (proc (car l)) (map proc (cdr l)))))
    ))

(define lib-and-exp
  `(begin ,stdlib ,mceval-exp))

;(define (start-mceval)
;  (compile-and-go lib-and-exp))

(define mceval-code
  (statements
   (compile lib-and-exp 'val 'next
            the-empty-cenv
            )))

(define program
  (cons '(assign env (op get-global-environment))
        mceval-code))

(define mceval-machine
  (make-machine all-operations program))

(define (start-ambeval)
  (start mceval-machine))
