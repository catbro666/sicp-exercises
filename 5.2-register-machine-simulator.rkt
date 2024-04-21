#lang sicp

;(#%require racket)
(#%require (only racket provide))

;; machine
(define (make-machine ops 
                      controller-text)
  (let ((machine (make-new-machine)))
    ((machine 'install-operations) ops)
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine))
    machine))

; registers
(define (make-register name)
  (let ((contents '*unassigned*)
        (tracing false))
    (define (dispatch message)
      (cond ((eq? message 'get) contents)
            ((eq? message 'set)
             (lambda (value)
               (if tracing
                   (begin (display (list "reg" name ":"
                                         contents "->" value))
                          (newline)))
               (set! contents value)))
            ((eq? message 'trace-on)
             (set! tracing true))
            ((eq? message 'trace-off)
             (set! tracing false))
            (else
             (error "Unknown request: 
                     REGISTER"
                    message))))
    dispatch))

(define (get-contents register)
  (register 'get))

(define (set-contents! register value)
  ((register 'set) value))

(define (reg-trace-on register)
  (register 'trace-on))

(define (reg-trace-off register)
  (register 'trace-off))

; stack
(define (make-stack)
  (let ((s '())
        (number-pushes 0)
        (max-depth 0)
        (current-depth 0))
    (define (push x)
      (set! s (cons x s))
      (set! number-pushes (+ 1 number-pushes))
      (set! current-depth (+ 1 current-depth))
      (set! max-depth 
            (max current-depth max-depth)))
    (define (pop)
      (if (null? s)
          (error "Empty stack: POP")
          (let ((top (car s)))
            (set! s (cdr s))
            (set! current-depth
                  (- current-depth 1))
            top)))
    (define (initialize)
      (set! s '())
      (set! number-pushes 0)
      (set! max-depth 0)
      (set! current-depth 0)
      'done)

    (define (print-statistics)
      (newline)
      (display (list 'total-pushes 
                     '= 
                     number-pushes
                     'maximum-depth
                     '=
                     max-depth)))
    (define (dispatch message)
      (cond ((eq? message 'push) push)
            ((eq? message 'pop) (pop))
            ((eq? message 'initialize)
             (initialize))
            ((eq? message 'print-statistics)
             (print-statistics))
            (else
             (error "Unknown request: STACK"
                    message))))
    dispatch))

(define (pop stack) (stack 'pop))
(define (push stack value)
  ((stack 'push) value))

;; the basic machine
(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '())
        (count 0)
        (tracing false)
        (pos (make-position 'start 0))
        (bps '())
        (rlabels '())
        (inst-infos '())
        (entry-point-infos '())
        (saved-reg-infos '())
        (reg-source-infos '()))
    (let ((the-ops
           (list
            (list 'initialize-stack
                  (lambda ()
                    (stack 'initialize)))
            (list 'print-stack-statistics
                  (lambda ()
                    (stack 'print-statistics)))))
          (register-table
           (list (list 'pc pc)
                 (list 'flag flag))))
      (define (allocate-register name)
        (if (assoc name register-table)
            (error
             "Multiply defined register: "
             name)
            (set! register-table
                  (cons
                   (list name
                         (make-register name))
                   register-table)))
        'register-allocated)
      (define (lookup-register name)
        (let ((val
               (assoc name register-table)))
          (if val
              (cadr val)
              (begin (allocate-register name)
                     (lookup-register name)))))
      (define (register-exist? name)
        (assoc name register-table))
      (define (breakpoint? pos)
        (define (iter l)
          (cond
            ((null? l) #f)
            ((position=? pos (car l)) #t)
            (else (iter (cdr l)))))
        (iter bps))
      (define (execute)
        (let ((insts (get-contents pc)))
          (let ((rlabel (lookup-rlabel rlabels insts)))
            (if rlabel
                (begin (set! pos (make-position rlabel 1))
                       (if tracing
                           (begin (display rlabel) (newline))))
                (set! pos (make-position (position-label pos)
                                         (+ 1 (position-offset pos))))))
          (if (breakpoint? pos)
              (begin (display (list "breakpoint:" (position-label pos)
                                    (position-offset pos)))
                     (newline))
              (proceed))))
      (define (proceed)
        (let ((insts (get-contents pc)))
          (if (null? insts)
              'done
              (begin
                (if tracing
                    (begin (display (instruction-text (car insts)))
                           (newline))
                    )
                ((instruction-execution-proc
                  (car insts)))
                (set! count (+ count 1))
                (execute)))))
      (define (add-if-nonexist e l)
        (if (member e l)
            l
            (cons e l)))
      (define (gather type)
        (cond
         ((eq? type 'inst) gather-inst)
         ((eq? type 'entry-point) gather-entry-point)
         ((eq? type 'saved-reg) gather-saved-reg)
         ((eq? type 'reg-source) gather-reg-source)))
      (define (gather-inst inst)
        (set! inst-infos
              (add-if-nonexist inst inst-infos)))
      (define (gather-entry-point reg-name)
        (set! entry-point-infos
              (add-if-nonexist reg-name entry-point-infos)))
      (define (gather-saved-reg reg-name)
        (set! saved-reg-infos
              (add-if-nonexist reg-name saved-reg-infos)))
      (define (gather-reg-source reg-name source)
        (let ((source-list (assoc reg-name reg-source-infos)))
          (if source-list
              (set-cdr! source-list
                        (add-if-nonexist source (cdr source-list)))
              (set! reg-source-infos
                    (cons (cons reg-name (add-if-nonexist source '()))
                          reg-source-infos)))))
      (define (display-infos)
        (display "Instrustions:\n")
        (for-each
         (lambda (inst)
           (display inst)
           (newline))
         inst-infos)
         ;(sort inst-infos (lambda (a b)
         ;                   (string<? (symbol->string (inst-type a))
         ;                             (symbol->string (inst-type b))))))
        (newline)
        
        (display "Entry Point Registers:\n")
        (for-each
         (lambda (reg-name)
           (display reg-name)
           (display " "))
         entry-point-infos)
        (newline)
        (newline)
        
        (display "Saved/Restored Registers:\n")
        (for-each
         (lambda (reg-name)
           (display reg-name)
           (display " "))
         saved-reg-infos)
        (newline)
        (newline)
        
        (display "Register Sources:\n")
        (for-each
         (lambda (rs)
           (display (car rs))
           (display ":\n")
           (for-each
            (lambda (s)
              (display s)
              (newline))
            (cdr rs)))
         reg-source-infos))
      (define (set-breakpoint label offset)
        (let ((p (make-position label offset)))
          (if (not (assoc p bps))
              (set! bps (cons p bps)))
          'done))
      (define (cancel-breakpoint label offset)
        (let ((p (make-position label offset)))
          (define (iter l)
            (cond
              ((null? l) '())
              ((position=? p (car l)) (cdr l))
              (else (cons (car l) (iter (cdr l))))))
          (set! bps (iter bps))))
      (define (cancel-all-breakpoints)
        (set! bps '()))
      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents! 
                pc
                the-instruction-sequence)
               (execute))
              ((eq? message 'proceed) (proceed))
              ((eq? 
                message 
                'install-instruction-sequence)
               (lambda (seq) 
                 (set! 
                  the-instruction-sequence 
                  seq)))
              ((eq? message 
                    'allocate-register)
               allocate-register)
              ((eq? message 'get-register) 
               lookup-register)
              ((eq? message 
                    'install-operations)
               (lambda (ops) 
                 (set! the-ops 
                       (append the-ops ops))))
              ((eq? message 'stack) stack)
              ((eq? message 'operations) 
               the-ops)
              ((eq? message 'gather) gather)
              ((eq? message 'display-infos) display-infos)
              ((eq? message 'get-count) count)
              ((eq? message 'reset-count) (set! count 0))
              ((eq? message 'trace-on) (set! tracing true))
              ((eq? message 'trace-off) (set! tracing false))
              ((eq? message 'set-rlabels) (lambda (x) (set! rlabels x)))
              ((eq? message 'set-breakpoint) set-breakpoint)
              ((eq? message 'cancel-breakpoint) cancel-breakpoint)
              ((eq? message 'cancel-all-breakpoints) (cancel-all-breakpoints))
              (else (error "Unknown request: 
                            MACHINE"
                           message))))
      dispatch)))

(define (start machine)
  (machine 'start))

(define (proceed-machine machine)
  (machine 'proceed))

(define (get-register-contents 
         machine register-name)
  (get-contents
   (get-register machine register-name)))

(define (set-register-contents!
         machine register-name value)
  (set-contents!
   (get-register machine register-name)
   value)
  'done)

(define (trace-on-register machine register-name)
  (reg-trace-on (get-register machine register-name)))

(define (trace-off-register machine register-name)
  (reg-trace-off (get-register machine register-name)))

(define (stack machine)
  (machine 'stack))

(define (get-count machine)
  (machine 'get-count))

(define (reset-count machine)
  (machine 'reset-count))

(define (trace-on machine)
  (machine 'trace-on))

(define (trace-off machine)
  (machine 'trace-off))

(define (get-register machine reg-name)
  ((machine 'get-register) reg-name))

(define (gather-inst machine inst)
  (((machine 'gather) 'inst) inst))

(define (gather-entry-point machine reg-name)
  (((machine 'gather) 'entry-point) reg-name))

(define (gather-saved-reg machine reg-name)
  (((machine 'gather) 'saved-reg) reg-name))

(define (gather-reg-source machine reg-name source)
  (((machine 'gather) 'reg-source) reg-name source))

(define (display-infos machine)
  ((machine 'display-infos)))

(define (set-rlabels! machine rlabels)
  ((machine 'set-rlabels) rlabels))

(define (set-breakpoint machine label n)
  ((machine 'set-breakpoint) label n))

(define (cancel-breakpoint machine label n)
  ((machine 'cancel-breakpoint) label n))

(define (cancel-all-breakpoints machine)
  (machine 'cancel-all-breakpoints))

; position
(define (make-position label offset)
  (cons label offset))

(define (position-label pos)
  (car pos))

(define (position-offset pos)
  (cdr pos))

(define (position=? p1 p2)
  (and (eq? (position-label p1)
            (position-label p2))
       (= (position-offset p1)
          (position-offset p2))))

;; the assembler
(define (assemble controller-text machine)
  (extract-labels controller-text
    (lambda (insts labels rlabels)
      (update-insts! insts labels rlabels machine)
      insts)))

(define (extract-labels text receive)
  (if (null? text)
      (receive '() '() '())
      (extract-labels 
       (cdr text)
       (lambda (insts labels rlabels)
         (let ((next-inst (car text)))
           (if (symbol? next-inst)
               (if (assoc next-inst labels)
                   (error "duplicated label:" next-inst)
                   (receive
                    insts
                    (cons 
                     (make-label-entry 
                      next-inst
                      insts)
                     labels)
                    (cons (make-rlabel-entry insts next-inst) rlabels)))
               (receive 
                (cons (make-instruction 
                       next-inst)
                      insts)
                labels rlabels)))))))

(define (update-insts! insts labels rlabels machine)
  (let ((pc (get-register machine 'pc))
        (flag (get-register machine 'flag))
        (stack (machine 'stack))
        (ops (machine 'operations)))
    (set-rlabels! machine rlabels)
    (for-each
     (lambda (inst)
       (gather-info (instruction-text inst) machine)
       (set-instruction-execution-proc!
        inst
        (make-execution-procedure
         (instruction-text inst) 
         labels
         machine
         pc
         flag
         stack
         ops)))
     insts)))

(define (gather-info inst machine)
  (gather-inst machine inst)
  (cond
   ((and (goto? inst)
         (register-exp? (goto-dest inst)))
    (gather-entry-point machine (register-exp-reg
                                 (goto-dest inst))))
   ((or (save? inst) (restore? inst))
    (gather-saved-reg machine (stack-inst-reg-name inst)))
   ((assign? inst)
    (gather-reg-source machine (assign-reg-name inst)
                       (assign-value-exp inst)))))

(define (make-instruction text)
  (cons text '()))
(define (instruction-text inst) (car inst))
(define (instruction-execution-proc inst)
  (cdr inst))
(define (set-instruction-execution-proc!
         inst
         proc)
  (set-cdr! inst proc))

(define (make-label-entry label-name insts)
  (cons label-name insts))

(define (lookup-label labels label-name)
  (let ((val (assoc label-name labels)))
    (if val
        (cdr val)
        (error "Undefined label: ASSEMBLE"
               label-name))))

(define (make-rlabel-entry insts label-name)
  (cons insts label-name))

(define (lookup-rlabel rlabels insts)
  (let ((val (assoc insts rlabels)))
    (if val
        (cdr val)
        false)))

;; execution-procedures
(define (make-execution-procedure 
         inst labels machine pc flag stack ops)
  (cond ((assign? inst)
         (make-assign 
          inst machine labels ops pc))
        ((test? inst)
         (make-test 
          inst machine labels ops flag pc))
        ((branch? inst)
         (make-branch 
          inst machine labels flag pc))
        ((goto? inst)
         (make-goto inst machine labels pc))
        ((save? inst)
         (make-save inst machine stack pc))
        ((restore? inst)
         (make-restore inst machine stack pc))
        ((perform? inst)
         (make-perform
          inst machine labels ops pc))
        (else (error "Unknown instruction 
                      type: ASSEMBLE"
                     inst))))

(define (inst-type inst)
  (car inst))
(define (assign? inst)
  (tagged-list? inst 'assign))
(define (test? inst)
  (tagged-list? inst 'test))
(define (branch? inst)
  (tagged-list? inst 'branch))
(define (goto? inst)
  (tagged-list? inst 'goto))
(define (save? inst)
  (tagged-list? inst  'save))
(define (restore? inst)
  (tagged-list? inst 'restore))
(define (perform? inst)
  (tagged-list? inst 'perform))

; assign
(define (make-assign 
         inst machine labels operations pc)
  (let ((target 
         (get-register 
          machine 
          (assign-reg-name inst)))
        (value-exp (assign-value-exp inst)))
    (let ((value-proc
           (if (operation-exp? value-exp)
               (make-operation-exp
                value-exp 
                machine
                labels
                operations)
               (make-primitive-exp
                (car value-exp)
                machine
                labels))))
      (lambda ()   ; execution procedure
                   ; for assign
        (set-contents! target (value-proc))
        (advance-pc pc)))))

(define (assign-reg-name assign-instruction)
  (cadr assign-instruction))
(define (assign-value-exp assign-instruction)
  (cddr assign-instruction))

(define (advance-pc pc)
  (set-contents! pc (cdr (get-contents pc))))

; test/branch/goto
(define 
  (make-test 
   inst machine labels operations flag pc)
  (let ((condition (test-condition inst)))
    (if (operation-exp? condition)
        (let ((condition-proc
               (make-operation-exp
                condition 
                machine
                labels
                operations)))
          (lambda () 
            (set-contents! 
             flag (condition-proc))
            (advance-pc pc)))
        (error "Bad TEST instruction: 
                ASSEMBLE" inst))))

(define (test-condition test-instruction)
  (cdr test-instruction))

(define 
  (make-branch 
   inst machine labels flag pc)
  (let ((dest (branch-dest inst)))
    (if (label-exp? dest)
        (let ((insts
               (lookup-label 
                labels 
                (label-exp-label dest))))
          (lambda ()
            (if (get-contents flag)
                (set-contents! pc insts)
                (advance-pc pc))))
        (error "Bad BRANCH instruction: 
                ASSEMBLE"
               inst))))

(define (branch-dest branch-instruction)
  (cadr branch-instruction))

(define (make-goto inst machine labels pc)
  (let ((dest (goto-dest inst)))
    (cond ((label-exp? dest)
           (let ((insts
                  (lookup-label 
                   labels
                   (label-exp-label dest))))
             (lambda () 
               (set-contents! pc insts))))
          ((register-exp? dest)
           (let ((reg
                  (get-register 
                   machine
                   (register-exp-reg dest))))
             (lambda ()
               (set-contents! 
                pc
                (get-contents reg)))))
          (else (error "Bad GOTO instruction:
                        ASSEMBLE"
                       inst)))))

(define (goto-dest goto-instruction)
  (cadr goto-instruction))

; other instructions
(define (make-save inst machine stack pc)
  (let ((reg (get-register 
              machine
              (stack-inst-reg-name inst))))
    (lambda ()
      (push stack (get-contents reg))
      (advance-pc pc))))

(define (make-restore inst machine stack pc)
  (let ((reg (get-register
              machine
              (stack-inst-reg-name inst))))
    (lambda ()
      (set-contents! reg (pop stack))
      (advance-pc pc))))

(define (stack-inst-reg-name 
         stack-instruction)
  (cadr stack-instruction))

(define (make-perform 
         inst machine labels operations pc)
  (let ((action (perform-action inst)))
    (if (operation-exp? action)
        (let ((action-proc
               (make-operation-exp
                action
                machine
                labels
                operations)))
          (lambda ()
            (action-proc)
            (advance-pc pc)))
        (error "Bad PERFORM instruction: 
                ASSEMBLE"
               inst))))

(define (perform-action inst) (cdr inst))

; subexpressions
(define (make-primitive-exp exp machine labels)
  (cond ((constant-exp? exp)
         (let ((c (constant-exp-value exp)))
           (lambda () c)))
        ((label-exp? exp)
         (let ((insts
                (lookup-label 
                 labels
                 (label-exp-label exp))))
           (lambda () insts)))
        ((register-exp? exp)
         (let ((r (get-register
                   machine
                   (register-exp-reg exp))))
           (lambda () (get-contents r))))
        (else (error "Unknown expression type: 
                      ASSEMBLE"
                     exp))))

(define (register-exp? exp)
  (tagged-list? exp 'reg))
(define (register-exp-reg exp)
  (cadr exp))
(define (constant-exp? exp)
  (tagged-list? exp 'const))
(define (constant-exp-value exp)
  (cadr exp))
(define (label-exp? exp)
  (tagged-list? exp 'label))
(define (label-exp-label exp) 
  (cadr exp))

(define (make-operation-exp
         exp machine labels operations)
  (let ((op (lookup-prim 
             (operation-exp-op exp)
             operations))
        (aprocs
         (map (lambda (e)
                (make-primitive-exp 
                  e machine labels))
              (operation-exp-operands exp))))
    (lambda () (apply op (map (lambda (p) (p))
                              aprocs)))))

(define (operation-exp? exp)
  (and (pair? exp)
       (tagged-list? (car exp) 'op)))
(define (operation-exp-op operation-exp)
  (cadr (car operation-exp)))
(define (operation-exp-operands operation-exp)
  (cdr operation-exp))

(define (lookup-prim symbol operations)
  (let ((val (assoc symbol operations)))
    (if val
        (cadr val)
        (error "Unknown operation: ASSEMBLE"
               symbol))))

; utils
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (print x)
  (display x)
  (newline))

(provide make-machine assemble start proceed-machine get-register-contents set-register-contents!
         trace-on-register trace-off-register stack pop push get-count reset-count trace-on trace-off
         display-infos set-breakpoint cancel-breakpoint cancel-all-breakpoints print)
