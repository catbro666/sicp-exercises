#lang racket
; streams
(define-syntax delay
  (syntax-rules ()
    ((delay expr)
     (lambda ()
       expr))))

(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream a b)
     (cons a (delay b)))))

(define (force delayed-object)
  (delayed-object))

(define (stream-car stream) 
  (car stream))

(define (stream-cdr stream) 
  (force (cdr stream)))

(define the-empty-stream '())

(define stream-null? null?)

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream 
       (proc (stream-car s))
       (stream-map proc (stream-cdr s)))))

(define (stream-ormap proc s)
  (if (stream-null? s)
      #f
      (let ((res (proc (stream-car s))))
        (if res
            res
            (stream-ormap proc (stream-cdr s))))))

(define (stream-andmap proc s)
  (define (iter s res)
    (if (stream-null? s)
        res
        (let ((res (proc (stream-car s))))
          (if res
              (iter (stream-cdr s) res)
              res))))
  (iter s #t))

(define (stream-append s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream 
       (stream-car s1)
       (stream-append (stream-cdr s1) s2))))

(define (stream-filter pred s)
  (if (stream-null? s)
      the-empty-stream
      (let ((a (stream-car s))
            (d (stream-cdr s)))
        (if (pred a)
            (cons-stream a (stream-filter pred d))
            (stream-filter pred d)))))

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream 
       (stream-car s1)
       (interleave s2 (stream-cdr s1)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin 
        (proc (stream-car s))
        (stream-for-each proc 
                         (stream-cdr s)))))

(define (stream-for-each-n proc s n)
  (if (or (stream-null? s) (= n 0))
      'done
      (begin 
        (proc (stream-car s))
        (stream-for-each-n proc 
                           (stream-cdr s)
                           (- n 1)))))

(define (display-stream s)
  (stream-for-each-n display-line s 10))

(define (display-line x)
  (newline)
  (display x))

; get/put
(define lof '())
(define (put tag op func)
  (set! lof (cons (list op tag func) lof)))
(define (get tag op)
  (define (iter l)
    (cond
      ((null? l) #f) ; automatically create table for database
      ((and (equal? (car (car l)) op)
            (equal? (cadr (car l)) tag))
       (caddr (car l)))
      (else (iter (cdr l)))))
  (iter lof))

; driver-loop
(define input-prompt  ";;; Query input:")
(define output-prompt ";;; Query results:")

; when read sees a dot, instead of making the next item
; be the next element of a list, it makes the next item
; be the cdr of the list structure.
; the pattern (computer ?type) could be structured as
; (cons 'computer (cons '?type '()))
; (computer . ?type) could be structured as
; (cons 'computer '?type)
(define (query-driver-loop)
  (prompt-for-input input-prompt)
  (let ((q (query-syntax-process (read))))
    (cond ((assertion-to-be-added? q)
           (add-rule-or-assertion! 
            (add-assertion-body q))
           (newline)
           (display 
            "Assertion added to data base.")
           (query-driver-loop))
          (else
           (newline)
           (display output-prompt)
           (display-stream
            (stream-map
             (lambda (frame)
               (instantiate
                   q
                 frame
                 (lambda (v f)
                   (contract-question-mark v))))
             (stream-filter
              (lambda (frame)
                (try-promises frame 'force))
              (qeval q (singleton-stream (make-frame '() '())) '())))) ; empty frame denotes no variable binding, e.g. the literal search
           (query-driver-loop)))))

(define (prompt-for-input string)
  (newline) (newline)
  (display string) (newline))

; Recursively instantiate as the value could contain variable
(define (instantiate 
            exp frame unbound-var-handler)
  (define (copy exp)
    (cond ((var? exp)
           (let ((binding 
                  (binding-in-frame 
                   exp frame)))
             (if binding
                 (copy                     ; recursively
                  (binding-value binding))
                 (unbound-var-handler
                  exp frame))))
          ((pair? exp)
           (cons (copy (car exp))
                 (copy (cdr exp))))
          (else exp)))
  (copy exp))

(define (qeval query frame-stream hist)
  (let ((qproc (get (type query) 'qeval)))    ; data-directed dispatch
    (if qproc
        (qproc (contents query) frame-stream hist) ; special form
        (simple-query query frame-stream hist))))

(define (simple-query query-pattern 
                      frame-stream hist)
  (stream-flatmap
   (lambda (frame)
     (stream-append-delayed
      (find-assertions query-pattern frame)
      (delay 
        (apply-rules query-pattern frame hist))))
   frame-stream))

; compound queries
(define (conjoin conjuncts frame-stream hist)
  (if (empty-conjunction? conjuncts)
      frame-stream
      (conjoin (rest-conjuncts conjuncts)
               (qeval 
                (first-conjunct conjuncts)
                frame-stream hist)
               hist)))

; another strategy for and
; parallely eval clauses and then merge
(define (new-conjoin conjuncts frame-stream hist)
  (define (iter conjuncts last-stream)
    (if (or (empty-conjunction? conjuncts)
            (stream-null? last-stream))
        last-stream
        (let ((stream (qeval (first-conjunct conjuncts)
                             frame-stream
                             hist)))
          (iter (rest-conjuncts conjuncts)
                    (merge-frame-stream last-stream stream)))))
  (iter conjuncts frame-stream))

(put 'and 'qeval conjoin)

(define (disjoin disjuncts frame-stream hist)
  (if (empty-disjunction? disjuncts)
      the-empty-stream
      (interleave-delayed
       (qeval (first-disjunct disjuncts) 
              frame-stream hist)
       (delay (disjoin 
               (rest-disjuncts disjuncts)
               frame-stream hist)))))
(put 'or 'qeval disjoin)

(define (uniquely-asserted operands frame-stream hist)
  (stream-flatmap
   (lambda (frame)
     (let ((stream (qeval (unique-query operands)
                          (singleton-stream frame)
                          hist)))
       (if (and (not (stream-null? stream))
                (stream-null? (stream-cdr stream)))
           (singleton-stream (stream-car stream))
           the-empty-stream)))
   frame-stream))
(put 'unique 'qeval uniquely-asserted)

; filters
(define (negate operands frame-stream hist)
  (stream-flatmap
   (lambda (frame)
     (if (stream-null? 
          (qeval (negated-query operands)
                 (singleton-stream frame)
                 hist))
         (singleton-stream frame)
         the-empty-stream))
   frame-stream))

(define (new-negate operands frame-stream hist)
  (let* ((query (negated-query operands))
         (promise (make-promise query
                                (lambda (query frame)
                                  (stream-null?
                                   (qeval query
                                          (singleton-stream frame)
                                          hist))))))
    (frame-stream-add-and-try-promise promise frame-stream)))

(put 'not 'qeval new-negate)

(define (lisp-value call frame-stream hist)
  (stream-flatmap
   (lambda (frame)
     (if (execute
          (instantiate
              call
            frame
            (lambda (v f)
              (error 
               "Unknown pat var: LISP-VALUE" 
               v))))
         (singleton-stream frame)
         the-empty-stream))
   frame-stream))

(define (new-lisp-value call frame-stream hist)
  (let ((promise
         (make-promise call
                       (lambda (query frame)
                         (execute
                          (instantiate call frame
                                       (lambda (v f)
                                         (error
                                          "Unknown pat var: LISP-VALUE"
                                          v))))))))
    (frame-stream-add-and-try-promise promise frame-stream)))

(put 'lisp-value 'qeval new-lisp-value)

(define (frame-stream-add-and-try-promise promise frame-stream)
  (stream-flatmap
   (lambda (frame)
     (add-and-try-promise promise frame))
   frame-stream))

(define (add-and-try-promise promise frame)
  (let ((nframe (try-promises
                      (add-promise-to-frame promise frame) 'once))) ; only try the promise we just extend
         (if (eq? nframe 'failed)
             the-empty-stream
             (singleton-stream nframe))))

; only eval the predicate
(define (execute exp)
  (apply (eval (predicate exp)
               (make-base-namespace))
         (args exp)))
;(define user-initial-environment '())

; used for rules without bodies
(define (always-true ignore frame-stream hist) 
  frame-stream)
(put 'always-true 'qeval always-true)

(define (find-assertions pattern frame)
  (stream-flatmap 
   (lambda (datum) 
     (check-an-assertion datum pattern frame))
   (fetch-assertions pattern frame)))

(define (check-an-assertion 
         assertion query-pat query-frame)
  (let ((match-result
         (pattern-match 
          query-pat assertion query-frame)))
    (if (eq? match-result 'failed)
        the-empty-stream
        (singleton-stream match-result))))

(define (pattern-match pat dat frame)
  (cond ((eq? frame 'failed) 'failed) ; recursively call may produre 'failed
        ((equal? pat dat) frame)
        ((var? pat) 
         (extend-if-consistent 
          pat dat frame))
        ((and (pair? pat) (pair? dat))
         (pattern-match 
          (cdr pat)
          (cdr dat)
          (pattern-match
           (car pat) (car dat) frame)))
        (else 'failed)))

(define (extend-if-consistent var dat frame)
  (let ((binding (binding-in-frame var frame)))
    (if binding
        (pattern-match 
         (binding-value binding) dat frame)
        (extend var dat frame))))

(define (merge-frame-stream s1 s2)
  (cond
   ((stream-null? s1) the-empty-stream)
   ((stream-null? s2) the-empty-stream)
   (else
    (stream-flatmap
   (lambda (f1)
     (stream-flatmap
      (lambda (f2)
        (merge-if-consistent f1 f2))
      s2))
   s1))))

(define (merge-if-consistent f1 f2)
  (define (iter bindings f2)
    (cond
     ((empty-bindings? bindings) (singleton-stream f2))
     (else (let ((binding (first-binding bindings)))
            (let ((var (binding-variable binding))
                  (val (binding-value binding)))
              (let ((nf2 (extend-if-consistent var val f2)))
                (if (eq? nf2 'failed)
                    the-empty-stream
                    (iter (rest-bindings bindings) nf2))))))))
  (iter (frame-bindings f1) f2))

; rules and unification
(define (apply-rules pattern frame hist)
  (stream-flatmap 
   (lambda (rule)
     (apply-a-rule rule pattern frame hist))
   (fetch-rules pattern frame)))

(define (apply-a-rule rule
                      query-pattern
                      query-frame
                      hist)
  (let ((conclusion (rule-conclusion rule))
        (clean-rule 
         (rename-variables-in rule))) ; avoid confusion
    (let ((unify-result
           (unify-match query-pattern
                        (rule-conclusion clean-rule)
                        query-frame)))
      (cond
        ((eq? unify-result 'failed) the-empty-stream)
        (else
         (let ((state (pattern-state query-pattern unify-result)))
           (if (in-progress? conclusion state unify-result hist)
               the-empty-stream
               (qeval (rule-body clean-rule)
                      (singleton-stream 
                       unify-result)
                      (extend-history conclusion query-pattern state hist)))))))))

; ?x -> ?x-7, ?y -> ?y-7
(define (rename-variables-in rule)
  (let ((rule-application-id 
         (new-rule-application-id)))
    (define (tree-walk exp)
      (cond ((var? exp)
             (make-new-variable 
              exp 
              rule-application-id))
            ((pair? exp)
             (cons (tree-walk (car exp))
                   (tree-walk (cdr exp))))
            (else exp)))
    (tree-walk rule)))

; the unifier is like the pattern matcher
; except that it is symmetrical
(define (unify-match p1 p2 frame)
  (cond ((eq? frame 'failed) 'failed)
        ((equal? p1 p2) frame)
        ((var? p1)
         (extend-if-possible p1 p2 frame))
        ((var? p2)
         (extend-if-possible 
          p2 
          p1 
          frame))        ; ***
        ((and (pair? p1) 
              (pair? p2))
         (unify-match 
          (cdr p1) 
          (cdr p2)
          (unify-match 
           (car p1)
           (car p2)
           frame)))
        (else 'failed)))

(define (extend-if-possible var val frame)
  (let ((binding (binding-in-frame var frame)))
    (cond (binding
           (unify-match
            (binding-value binding) val frame))
          ((var? val)                   ; ***
           (let ((binding 
                  (binding-in-frame 
                   val
                   frame)))
             (if binding
                 (unify-match
                  var 
                  (binding-value binding) 
                  frame)
                 (extend var val frame))))
          ; reject such bindings
          ; unify ?y with an expression involving ?y
          ((depends-on? val var frame)  ; ***
           'failed)
          (else (extend var val frame)))))

(define (depends-on? exp var frame)
  (define (tree-walk e)
    (cond ((var? e)
           (if (equal? var e)
               true
               (let
                   ((b (binding-in-frame 
                        e 
                        frame)))
                 (if b
                     (tree-walk 
                      (binding-value b))
                     false))))
          ((pair? e)
           (or (tree-walk (car e))
               (tree-walk (cdr e))))
          (else false)))
  (tree-walk exp))

; work history to avoid infinite loop
(define (in-progress? conclusion state frame hist)
  ; only check the rules with same conclusion
  ; I think it works even in the mutual recursion case
  (let ((shist (select-history conclusion hist)))
    (define (iter hist)
      (cond
       ((empty-history? hist) #f)
       (else
        (let ((h (first-history hist)))
          (let ((hstate (history-state h)))
            ; the pattern already occured and the history pattern state keeps the same
            ; that means no any free vars get bound after this unify-match
            (or (and (equal? state hstate)
                     (equal? hstate (pattern-state (history-pattern h) frame)))
                (iter (rest-history hist))))))))
    (iter shist)))

(define (pattern-state pat frame)
  (cond
    ((var? pat) (let ((binding (binding-in-frame pat frame)))
                  (if binding
                      (pattern-state (binding-value binding) frame)
                      ;(binding-value binding)
                      binding)))
    ((pair? pat) (cons (pattern-state (car pat) frame)
                       (pattern-state (cdr pat) frame)))
    (else pat)))

; for simplicity we didn't index by conclusion
(define (history-conclusion hist)
  (car hist))
(define (history-pattern hist)
  (cadr hist))
(define (history-state hist)
  (caddr hist))
(define (make-history conclusion pattern state)
  (list conclusion pattern state))

(define (extend-history conclusion pattern state history)
  (let ((hist (make-history conclusion pattern state)))
    (cons hist history)))

(define (select-history conclusion history)
  (filter (lambda (hist)
            (equal? conclusion (history-conclusion hist)))
          history))

(define (first-history history)
  (car history))
(define (rest-history history)
  (cdr history))
(define (empty-history? history)
  (null? history))

; in addition to storing all assertions in one big stream
; we store all assertions whose cars are constant symbols in separate streams,
; in a table indexed by the symbol
(define THE-ASSERTIONS the-empty-stream)

(define (fetch-assertions pattern frame)
  (if (use-index? pattern)
      (get-indexed-assertions pattern)
      (get-all-assertions)))

(define (get-all-assertions) THE-ASSERTIONS)

(define (get-indexed-assertions pattern)
  (get-stream (index-key-of pattern)
              'assertion-stream))

(define (get-stream key1 key2)
  (let ((s (get key1 key2)))
    (if s s the-empty-stream)))

; rules are stored using the car of the rule conclusion
; A pattern whose car is a constant symbol can match rules
; whose conclusions start with a variable as well as rules
; whose conclusions have the same car.
; we store all rules whose conclusions starts with a variable
; in a separate stream in our table, indexed by the symbol ?
(define THE-RULES the-empty-stream)

(define (fetch-rules pattern frame)
  (if (use-index? pattern)
      (get-indexed-rules pattern)
      (get-all-rules)))

(define (get-all-rules) THE-RULES)

(define (get-indexed-rules pattern)
  (stream-append
   (get-stream (index-key-of pattern)
               'rule-stream)
   (get-stream '? 'rule-stream)))

(define (add-rule-or-assertion! assertion)
  (if (rule? assertion)
      (add-rule! assertion)
      (add-assertion! assertion)))

(define (add-assertion! assertion)
  (store-assertion-in-index assertion)
  (let ((old-assertions THE-ASSERTIONS))
    (set! THE-ASSERTIONS
          (cons-stream assertion 
                       old-assertions))
    'ok))

(define (add-rule! rule)
  (store-rule-in-index rule)
  (let ((old-rules THE-RULES))
    (set! THE-RULES
          (cons-stream rule old-rules))
    'ok))

(define (store-assertion-in-index assertion)
  (if (indexable? assertion)
      (let ((key (index-key-of assertion)))
        (let ((current-assertion-stream
               (get-stream 
                key 'assertion-stream)))
          (put key
               'assertion-stream
               (cons-stream 
                assertion
                current-assertion-stream))))
      (void)))

(define (store-rule-in-index rule)
  (let ((pattern (rule-conclusion rule)))
    (if (indexable? pattern)
        (let ((key (index-key-of pattern)))
          (let ((current-rule-stream
                 (get-stream 
                  key 'rule-stream)))
            (put key
                 'rule-stream
                 (cons-stream 
                  rule
                  current-rule-stream))))
        (void))))

(define (indexable? pat)
  (or (constant-symbol? (car pat))
      (var? (car pat))))

(define (index-key-of pat)
  (let ((key (car pat)))
    (if (var? key) '? key)))

(define (use-index? pat)
  (constant-symbol? (car pat)))

; stream operations
(define (stream-append-delayed s1 delayed-s2)
  (if (stream-null? s1)
      (force delayed-s2)
      (cons-stream
       (stream-car s1)
       (stream-append-delayed (stream-cdr s1)
                              delayed-s2))))

(define (interleave-delayed s1 delayed-s2)
  (if (stream-null? s1)
      (force delayed-s2)
      (cons-stream
       (stream-car s1)
       (interleave-delayed 
        (force delayed-s2)
        (delay (stream-cdr s1))))))

(define (stream-flatmap proc s)
  (flatten-stream (stream-map proc s)))

(define (flatten-stream stream)
  (if (stream-null? stream)
      the-empty-stream
      (interleave-delayed
       (stream-car stream)
       (delay (flatten-stream
               (stream-cdr stream))))))

(define (singleton-stream x)
  (cons-stream x the-empty-stream))

; query syntax procedures
(define (type exp)
  (if (pair? exp)
      (car exp)
      (error "Unknown expression TYPE"
             exp)))

(define (contents exp)
  (if (pair? exp)
      (cdr exp)
      (error "Unknown expression CONTENTS"
             exp)))

(define (assertion-to-be-added? exp)
  (eq? (type exp) 'assert!))

(define (add-assertion-body exp)
  (car (contents exp)))

(define (empty-conjunction? exps) (null? exps))
(define (first-conjunct exps) (car exps))
(define (rest-conjuncts exps) (cdr exps))
(define (empty-disjunction? exps) (null? exps))
(define (first-disjunct exps) (car exps))
(define (rest-disjuncts exps) (cdr exps))
(define (negated-query exps) (car exps))
(define (predicate exps) (car exps))
(define (args exps) (cdr exps))
(define (unique-query exps) (car exps))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (rule? statement)
  (tagged-list? statement 'rule))

(define (rule-conclusion rule) (cadr rule))

(define (rule-body rule)
  (if (null? (cddr rule))
      '(always-true)
      (caddr rule)))

(define (query-syntax-process exp)
  (map-over-symbols expand-question-mark exp))

(define (map-over-symbols proc exp)
  (cond ((pair? exp)
         (cons (map-over-symbols 
                proc (car exp))
               (map-over-symbols 
                proc (cdr exp))))
        ((symbol? exp) (proc exp))
        (else exp)))

; (job ?x ?y) -> (job (? x) (? y))
(define (expand-question-mark symbol)
  (let ((chars (symbol->string symbol)))
    (if (string=? (substring chars 0 1) "?")
        (list '? (string->symbol
                  (substring
                   chars 
                   1 
                   (string-length chars))))
        symbol)))

(define (var? exp) (tagged-list? exp '?))
(define (constant-symbol? exp) (symbol? exp))

(define rule-counter 0)

(define (new-rule-application-id)
  (set! rule-counter (+ 1 rule-counter))
  rule-counter)

(define (make-new-variable 
         var rule-application-id)
  (cons '? (cons rule-application-id
                 (cdr var))))

(define (contract-question-mark variable)
  (string->symbol
   (string-append "?"
                  (if (number? (cadr variable))
                      (string-append
                       (symbol->string (caddr variable))
                       "-"
                       (number->string (cadr variable)))
                      (symbol->string (cadr variable))))))

; frames and bindings and promises
(define (make-frame bindings promises)
  (cons bindings promises))   

(define (frame-bindings frame)
  (car frame))
(define (frame-promises frame)
  (cdr frame))

(define (binding-in-frame variable frame)
  (assoc variable (frame-bindings frame)))

; if the current frame is filtered, return 'failed
(define (extend variable value frame)
  (try-promises
   (add-binding-to-frame
    (make-binding variable value)
    frame)
   'all))

(define (add-binding-to-frame binding frame)
  (make-frame (extend-bindings binding
                               (frame-bindings frame))
              (frame-promises frame)))

(define (add-promise-to-frame promise frame)
  (make-frame (frame-bindings frame)
              (extend-promises promise
                               (frame-promises frame))))

(define (extend-bindings binding bindings)
  (cons binding bindings))

(define (extend-promises promise promises)
  (cons promise promises))

(define (make-binding variable value)
  (cons variable value))

(define (binding-variable binding)
  (car binding))

(define (binding-value binding)
  (cdr binding))

(define (first-binding bindings)
  (car bindings))

(define (rest-bindings bindings)
  (cdr bindings))

(define (empty-bindings? bindings)
  (null? bindings))
  
(define (make-promise query filter)
  (cons query filter))

(define (promise-query promise)
  (car promise))

(define (promise-filter promise)
  (cdr promise))

(define (first-promise promises)
  (car promises))

(define (rest-promises promises)
  (cdr promises))

(define (empty-promises? promises)
  (null? promises))

(define (all-bounded? pat frame)
  (cond
    ((var? pat)
     (let ((binding (binding-in-frame pat frame)))
       (and binding
            (all-bounded? (binding-value binding) frame))))
    ((pair? pat)
     (and (all-bounded? (car pat) frame)
          (all-bounded? (cdr pat) frame)))
    (else #t)))

(define (try-promises frame type)
  (let ((bindings (frame-bindings frame))
        (promises (frame-promises frame)))
    (let ((frame-w/o-promises (make-frame bindings '())))
      (define (iter promises delayed-promises count)
        (cond
          ((or (empty-promises? promises)
               (and (eq? 'once type) (= count 1)))
           (make-frame bindings delayed-promises))
          (else
           (let ((promise (first-promise promises)))
             (let ((query (promise-query promise)))
               (if (or (eq? 'force type)
                       (all-bounded? query frame))
                   ; evaluate the filter with frame without promises
                   ; to avoid infinite loop
                   (if ((promise-filter promise) query frame-w/o-promises)
                       (iter (rest-promises promises) delayed-promises
                             (+ count 1))
                       'failed)
                   (iter (rest-promises promises)
                         (extend-promises promise delayed-promises)
                         (+ count 1))))))))
      (iter promises '() 0))))

(define (init-database l)
  (map
   (lambda (q)
     (add-rule-or-assertion!
      (add-assertion-body q)))
   l))

(define rules-and-assertions
  '((assert! (address (Bitdiddle Ben) 
          (Slumerville (Ridge Road) 10)))
(assert! (job (Bitdiddle Ben) (computer wizard)))
(assert! (salary (Bitdiddle Ben) 60000))

(assert! (address (Hacker Alyssa P) 
         (Cambridge (Mass Ave) 78)))
(assert! (job (Hacker Alyssa P) (computer programmer)))
(assert! (salary (Hacker Alyssa P) 40000))
(assert! (supervisor (Hacker Alyssa P) (Bitdiddle Ben)))

(assert! (address (Fect Cy D) 
         (Cambridge (Ames Street) 3)))
(assert! (job (Fect Cy D) (computer programmer)))
(assert! (salary (Fect Cy D) 35000))
(assert! (supervisor (Fect Cy D) (Bitdiddle Ben)))

(assert! (address (Tweakit Lem E) 
         (Boston (Bay State Road) 22)))
(assert! (job (Tweakit Lem E) (computer technician)))
(assert! (salary (Tweakit Lem E) 25000))
(assert! (supervisor (Tweakit Lem E) (Bitdiddle Ben)))

(assert! (address (Reasoner Louis) 
         (Slumerville (Pine Tree Road) 80)))
(assert! (job (Reasoner Louis) 
     (computer programmer trainee)))
(assert! (salary (Reasoner Louis) 30000))
(assert! (supervisor (Reasoner Louis) 
            (Hacker Alyssa P)))

(assert! (supervisor (Bitdiddle Ben) (Warbucks Oliver)))
(assert! (address (Warbucks Oliver) 
         (Swellesley (Top Heap Road))))
(assert! (job (Warbucks Oliver) 
     (administration big wheel)))
(assert! (salary (Warbucks Oliver) 150000))

(assert! (address (Scrooge Eben) 
         (Weston (Shady Lane) 10)))
(assert! (job (Scrooge Eben) 
     (accounting chief accountant)))
(assert! (salary (Scrooge Eben) 75000))
(assert! (supervisor (Scrooge Eben) (Warbucks Oliver)))

(assert! (address (Cratchet Robert) 
         (Allston (N Harvard Street) 16)))
(assert! (job (Cratchet Robert) (accounting scrivener)))
(assert! (salary (Cratchet Robert) 18000))
(assert! (supervisor (Cratchet Robert) (Scrooge Eben)))

(assert! (address (Aull DeWitt) 
         (Slumerville (Onion Square) 5)))
(assert! (job (Aull DeWitt) (administration secretary)))
(assert! (salary (Aull DeWitt) 25000))
(assert! (supervisor (Aull DeWitt) (Warbucks Oliver)))

(assert! (can-do-job (computer wizard) 
            (computer programmer)))

(assert! (can-do-job (computer wizard) 
            (computer technician)))

(assert! (can-do-job (computer programmer)
            (computer programmer trainee)))

(assert! (can-do-job (administration secretary)
            (administration big wheel)))

(assert! (rule (same ?x ?x)))

(assert! (rule (lives-near ?person-1 ?person-2)
      (and (address ?person-1 
                    (?town . ?rest-1))
           (address ?person-2 
                    (?town . ?rest-2))
           (not (same ?person-1 ?person-2))
           (alphabet< ?person-1 ?person-2))))

(assert! (rule (wheel ?person)
      (and (supervisor ?middle-manager 
                       ?person)
           (supervisor ?x ?middle-manager))))

(assert! (rule (outranked-by ?staff-person ?boss)
      (or (supervisor ?staff-person ?boss)
          (and (outranked-by ?middle-manager 
                             ?boss)
               (supervisor ?staff-person 
                           ?middle-manager)
               
               ))))

(assert! (rule (can-replace ?person1 ?person2) 
                (and (job ?person1 ?job1) 
                     (job ?person2 ?job2) 
                     (or (same ?job1 ?job2) 
                         (can-do-job ?job1 ?job2)) 
                     (not (same ?person1 ?person2)))))

(assert! (rule (bigshot ?p ?div)
      (and (job ?p (?div . ?post))
           (not (and (supervisor ?p ?super)
                     (job ?super (?div . ?any)))))))

(assert! (rule (alphabet< ?x ?y)
      (lisp-value
       (lambda (x y)
         (define (list->string l)
           (foldr string-append ""
                  (map symbol->string l)))
         (cond
          ((pair? x)
           (string<? (list->string x) (list->string y)))
          ((symbol? x) (string<? (symbol->string x) (symbol->string y)))
          ((number? x) (< x y))))
       ?x ?y)))

(assert! (rule (append-to-form (?u . ?v) ?y (?u . ?z))
      (append-to-form ?v ?y ?z)))
(assert! (rule (append-to-form () ?y ?y)))

(assert! (rule (?x next-to ?y in (?v . ?z))
      (?x next-to ?y in ?z)))
(assert! (rule (?x next-to ?y in (?x ?y . ?u))))

(assert! (rule (last-pair (?u . ?v) (?x))
      (last-pair ?v (?x))))
(assert! (rule (last-pair (?x) (?x))))

(assert! (son Adam Cain))
(assert! (son Cain Enoch))
(assert! (son Enoch Irad))
(assert! (son Irad Mehujael))
(assert! (son Mehujael Methushael))
(assert! (son Methushael Lamech))
(assert! (wife Lamech Ada))
(assert! (son Ada Jabal))
(assert! (son Ada Jubal))

(assert! (rule (partner ?a ?b)
      (or (wife ?a ?b)
          (wife ?b ?a))))

(assert! (rule (partner-uniq ?a ?b)
      (and (partner ?a ?b)
           (alphabet< ?a ?b))))

(assert! (rule (hasson ?p ?s)
      (or (son ?p ?s)
          (and (partner ?p ?p2)
               (son ?p2 ?s)))))

(assert! (rule (grandson ?g ?s)
      (and (hasson ?g ?p)
           (hasson ?p ?s))))

(assert! (rule (end-in-grandson (?x . ?rest))
      (end-in-grandson ?rest)))
(assert! (rule (end-in-grandson (grandson))))
(assert! (rule ((grandson) ?x ?y)
               (grandson ?x ?y)))
; Note that the end-with-gs predicate has to be the last one 
(assert! (rule ((great . ?rel) ?x ?y)
      (and (son ?x ?z)
           (?rel ?z ?y)
           (end-in-grandson ?rel))))

(assert! (rule (reverse ?x ?y)
      (and (append-to-form (?first) ?rest ?x)
           (append-to-form ?rev-rest (?first) ?y)
           (reverse ?rest ?rev-rest))))
(assert! (rule (reverse () ())))
))

(init-database rules-and-assertions)

(query-driver-loop)