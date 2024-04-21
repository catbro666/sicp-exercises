#lang racket

(define (memo-proc proc)
  (let ((already-run? false) (result false))
    (lambda ()
      (if (not already-run?)
          (begin (set! result (proc))
                 (set! already-run? true)
                 result)
          result))))

;(define-syntax delay
;  (syntax-rules ()
;    ((delay expr)
;     (lambda () expr))))

(define-syntax delay
  (syntax-rules ()
    ((delay exp)
     (memo-proc (lambda () exp)))))

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

(define (stream-cadr s)
  (stream-car (stream-cdr s)))
(define (stream-caddr s)
  (stream-cadr (stream-cdr s)))
(define (stream-cddr s)
  (stream-cdr (stream-cdr s)))

(define (stream . args)
  (define (iter l)
    (cond
      ((null? l) the-empty-stream)
      (else (cons-stream (car l) (iter (cdr l))))))
  (iter args))

; util
(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc
                    (map stream-cdr argstreams))))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin 
        (proc (stream-car s))
        (stream-for-each proc 
                         (stream-cdr s)))))

(define (stream-for-each-n proc s n)
  (define (iter s i)
    (if (or (stream-null? s)
            (= i n))
        'done
        (begin
          (proc (stream-car s))
          (iter (stream-cdr s) (+ i 1)))))
  (iter s 0))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) 
         the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream 
          (stream-car stream)
          (stream-filter 
           pred
           (stream-cdr stream))))
        (else (stream-filter 
               pred 
               (stream-cdr stream)))))

(define (display-line x)
  (display x)
  (newline))

(define (display-stream s)
  (stream-for-each display-line s))

(define (displayn s n)
  (stream-for-each-n display-line s n))


; 流高层操作方法
; 放缩
(define (scale-stream stream factor)
  (stream-map
   (lambda (x) (* x factor))
   stream))

(define (add-streams s1 s2) 
  (stream-map + s1 s2))

(define (sub-streams s1 s2) 
  (stream-map - s1 s2))

(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define (partial-sums s)
  (add-streams s (cons-stream 0 (partial-sums s))))

(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (cons-stream 
                   s1car 
                   (merge (stream-cdr s1) 
                          s2)))
                 ((> s1car s2car)
                  (cons-stream 
                   s2car 
                   (merge s1 
                          (stream-cdr s2))))
                 (else
                  (cons-stream 
                   s1car
                   (merge 
                    (stream-cdr s1)
                    (stream-cdr s2)))))))))



(define (divisible? x y) (= (remainder x y) 0))

; infinite streams
; 无穷1
(define ones (cons-stream 1 ones))

; 整数
(define (integers-starting-from n)
  (cons-stream 
   n (integers-starting-from (+ n 1))))

(define integers 
  (cons-stream 1 (add-streams ones integers)))

; Fibonacci
(define fibs 
  (cons-stream 
   0 (cons-stream
      1 (add-streams 
         (stream-cdr fibs) fibs))))

; 质数
(define primes
  (cons-stream
   2 (stream-filter 
      prime? (integers-starting-from 3))))

(define (prime? n)
  (define (iter ps)
    (cond ((> (sqr (stream-car ps)) n) true)
          ((divisible? n (stream-car ps)) false)
          (else (iter (stream-cdr ps)))))
  (iter primes))

(define factorials 
  (cons-stream 1 (mul-streams factorials integers)))

; 因子只有2，3，5的数
(define S (cons-stream 1 (merge (scale-stream S 2) (merge (scale-stream S 3) (scale-stream S 5)))))

; Exercise 3.58
(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) 
           den 
           radix)))

; power series
(define (integrate-series s)
  (stream-map / s integers))

(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1)
                  (stream-car s2))
               (add-streams (scale-stream (stream-cdr s2) (stream-car s1))
                            (mul-series (stream-cdr s1) s2))))

(define (invert-series s)
  (let ((c (stream-car s))
        (sr (stream-cdr s)))
    (if (= c 0)
        (error "constant term can't be zero")
        (cons-stream (/ 1 c)
                     (scale-stream (mul-series sr (invert-series s))
                                   -1)))))

(define (div-series s1 s2)
  (mul-series s1 (invert-series s2)))

(define exp-series
  (cons-stream
   1 (integrate-series exp-series)))

(define cosine-series
  (cons-stream 1 (stream-map - (integrate-series sine-series))))

(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))

(define tangent-series
  (div-series sine-series cosine-series))

; formulating iterations as stream processes
(define (average a b)
  (/ (+ a b) 2.0))

; square root
(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (cons-stream 
     1.0 (stream-map
          (lambda (guess)
            (sqrt-improve guess x))
          guesses)))
  guesses)

; pi
(define (pi-summands n)
  (cons-stream 
   (/ 1.0 n)
   (stream-map - (pi-summands (+ n 2)))))

(define pi-stream
  (scale-stream 
   (partial-sums (pi-summands 1)) 4))

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))     ; Sn-1
        (s1 (stream-ref s 1))     ; Sn
        (s2 (stream-ref s 2)))    ; Sn+1
    (cons-stream 
     (- s2 (/ (sqr (- s2 s1))
              (+ s0 (* -2 s1) s2)))
     (euler-transform (stream-cdr s)))))

(define (make-tableau transform s)
  (cons-stream 
   s
   (make-tableau
    transform
    (transform s))))
; s00 s01 s02 s03 s04 ...
;     s10 s11 s12 s13 ...
;         s20 s21 s22 ...

; 然后取每一行的第一项
(define (accelerated-sequence transform s)
  (stream-map stream-car
              (make-tableau transform s)))

(define (ln2-summands n)
  (cons-stream 
   (/ 1.0 n)
   (stream-map - (ln2-summands (+ n 1)))))

(define ln2-stream
  (partial-sums (ln2-summands 1)))

; infinite streams of pairs
; (i, j) i <= j
(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) 
                  (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s)
           (stream-cdr t)))))

(define (pairs-all s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (interleave
     (stream-map (lambda (x) 
                   (list (stream-car s) x))
                 (stream-cdr t))
     (stream-map (lambda (x) 
                   (list x (stream-car t)))
                 (stream-cdr s)))
    (pairs (stream-cdr s)
           (stream-cdr t)))))

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream 
       (stream-car s1)
       (interleave s2 (stream-cdr s1)))))

; triples
;(define (triples s t u)
;  (cons-stream
;   (list (stream-car s)
;         (stream-car t)
;         (stream-car u))
;   (interleave
;    (stream-map (lambda (x)
;                  (cons (stream-car s) x))
;                (stream-cdr (pairs t u)))
;    (triples (stream-cdr s)
;             (stream-cdr t)
;             (stream-cdr u)))))

(define first-of-integer-pair 
  (stream-map car (pairs integers integers))) 
  
(define (triples s t u) 
  (let ((pairs-tu (pairs t u))) ;; compute pairs only *once* 
    (define (rec si i ptu top-i) 
      (cons-stream 
       (cons (stream-car si) (stream-car ptu)) 
       (if (= i (stream-car top-i)) 
           (rec s 1 (stream-cdr ptu) (stream-cdr top-i)) 
           ;; restart s cycle with next ptu 
           (rec (stream-cdr si) (+ 1 i) ptu top-i)))) 
    (rec s 1 pairs-tu first-of-integer-pair))) 

(define phythagorean-numbers
  (stream-filter (lambda (x)
                   (= (sqr (caddr x))
                      (+ (sqr (car x))
                         (sqr (cadr x)))))
                 (triples integers integers integers)))


(define (merge-weighted s1 s2 weight)
  (define (iter s1 s2)
    (cond
      ((stream-null? s1) s2)
      ((stream-null? s2) s1)
      (else
       (let* ((s1car (stream-car s1))
              (s2car (stream-car s2))
              (w1 (weight s1car))
              (w2 (weight s2car)))
         (cond ((< w1 w2)
                (cons-stream 
                 s1car
                 (iter (stream-cdr s1) s2)))
               ((< w2 w1)
                (cons-stream
                 s2car
                 (iter s1 (stream-cdr s2))))
               (else
                (cons-stream
                 s1car              ; s1car in the front
                 (cons-stream
                  s2car             ; cons both
                  (iter (stream-cdr s1) (stream-cdr s2))))))))))
  (iter s1 s2))

; i<=j
(define (weighted-pairs s t weight)
  (define (iter s t)
    (cons-stream
     (list (stream-car s) (stream-car t))
     (merge-weighted
      (stream-map (lambda (x)
                    (list (stream-car s) x))
                  (stream-cdr t))
      (iter (stream-cdr s) (stream-cdr t))
      weight)))
  (iter s t))

(define (weight-sum pair)
  (apply + pair))
(define stream-a (weighted-pairs integers integers weight-sum))

(define (weight-2i3j5ij pair)
  (let ((i (car pair))
        (j (cadr pair)))
    (+ (* 2 i)
       (* 3 j)
       (* 5 i j))))

(define stream235
  (stream-filter
   (lambda (x)
     (not (or (divisible? x 2) (divisible? x 3) (divisible? x 5))))
   integers))
(define stream-b (weighted-pairs stream235 stream235 weight-2i3j5ij))

(define (cube x)
  (* x x x))
(define (cubesum pair)
  (apply + (map cube pair)))
(define pair-stream
  (weighted-pairs integers integers cubesum))
(define ramanujan-numbers
  (stream-filter (lambda (x)
                   (= (cubesum (caddr x)) (car x))) ; compare the weight with next pair
                 (stream-map list
                             (stream-map cubesum pair-stream)
                             pair-stream
                             (stream-cdr pair-stream))))

(define (squaresum pair)
  (apply + (map sqr pair)))

(define pair-stream2
  (weighted-pairs integers integers squaresum))

(define 3way-sqrsum-numbers
  (stream-filter
   (lambda (x)
     (= (car x)
        (squaresum (caddr x))
        (squaresum (cadddr x))))
   (stream-map list
               (stream-map squaresum pair-stream2)
               pair-stream2
               (stream-cdr pair-stream2)
               (stream-cdr (stream-cdr pair-stream2)))))

; streams as signals
; (define (integral integrand initial-value dt)
;   (define int
;     (cons-stream 
;      initial-value
;      (add-streams (scale-stream integrand dt)
;                   int)))
;   int)

(define (RC R C dt)
  (lambda (i v0)
    (add-streams
     (integral (scale-stream i (/ 1 C)) v0 dt)
     (scale-stream i R))))

(define RC1 (RC 5 1 0.5))

; Exercise 3.74
; (define (make-zero-crossings
;          input-stream last-value)
;   (cons-stream
;    (sign-change-detector 
;     (stream-car input-stream) 
;     last-value)
;    (make-zero-crossings 
;     (stream-cdr input-stream)
;     (stream-car input-stream))))
; 
; (define zero-crossings 
;   (make-zero-crossings sense-data 0))

; 更优雅
; (define (make-zero-crossings input-stream) 
;    (stream-map sign-change-detector (stream-cdr sense-data) sense-data)) 

; (define (make-zero-crossings input-stream last-value last-avpt)
;   (let ((avpt (/ (+ (stream-car input-stream) last-value) 2)))
;     (cons-stream (sign-change-detetor avpt last-avpt)
;                  (make-zero-crossings (stream-cdr input-stream)
;                                       (stream-car input-stream)
;                                       avpt))))

; (define (average-sign-change-detector x y z)
;   (let ((avg1 (average x y))
;         (avg2 (average y z)))
;     (sign-change-detector avg2 avg1)))
;  
; (define (make-zero-crossings s)
;   (stream-map average-sign-change-detector s (stream-cdr s) (stream-cddr s)))

; (define (smooth input-stream)
;   (stream-map (lambda(x y)(/ (+ x y) 2)) input-stream (stream-cdr input-stream)))
;   
; (define (zero-crossings input-stream)
;   (stream-map sign-change-detector input-stream (stream-cdr input-stream)))
;   
; (define (smoothed-zero-crossing sense-data)
;   (zero-crossings (smooth sense-data)))

; streams and delayed evaluation
(define (integral
         delayed-integrand initial-value dt)
  (define int
    (cons-stream 
     initial-value
     (let ((integrand 
            (force delayed-integrand)))
       (add-streams 
        (scale-stream integrand dt)
        int))))
  int)

(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

(stream-ref (solve (lambda (y) y) 1 0.001) 1000)

;(define (solve-2nd a b dt y0 dy0)
;  (define y (integral (delay dy) y0 dt))
;  (define dy (integral (delay ddy) dy0 dt))
;  (define ddy (add-streams (scale-stream dy a)
;                           (scale-stream y b)))
;  y)

(define (solve-2nd f dt y0 dy0)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (stream-map f dy y))
  y)

(define (RLC R L C dt)
  (lambda (vC0 iL0)
    (define vC (integral (delay dvC) vC0 dt)) ; in the front
    (define iL (integral (delay diL) iL0 dt)) ; in the front
    (define dvC (scale-stream iL (/ -1 C)))
    (define diL (sub-streams
                 (scale-stream vC (/ 1 L))
                 (scale-stream iL (/ R L))))
    (cons vC iL))) ; using cons

(define RLC-circuit (RLC 1 1 0.2 0.1))
(define RLC-instance (RLC-circuit 3 1))

; modularity of functinal programs
(define (rand-update x) (+ x 1)) ; A not-very-evolved PNRG
(define random-init 0)
(define random-numbers
  (cons-stream random-init
               (stream-map rand-update 
                           random-numbers)))

(define (map-successive-pairs f s)
  (cons-stream
   (f (stream-car s) 
      (stream-car (stream-cdr s)))
   (map-successive-pairs 
    f (stream-cdr (stream-cdr s)))))

(define cesaro-stream
  (map-successive-pairs
   (lambda (r1 r2) (= (gcd r1 r2) 1))
   random-numbers))

(define (monte-carlo experiment-stream 
                     passed 
                     failed)
  (define (next passed failed)
    (cons-stream
     (/ passed (+ passed failed))
     (monte-carlo
      (stream-cdr experiment-stream) 
      passed 
      failed)))
  (if (stream-car experiment-stream)
      (next (+ passed 1) failed)
      (next passed (+ failed 1))))

(define pi
  (stream-map
   (lambda (p) (sqrt (/ 6 p)))
   (monte-carlo cesaro-stream 0 0)))


(define (make-rand init request-stream)
  (define random-numbers
    (cons-stream init
                 (stream-map
                  (lambda (req num)
                    (cond
                     ((eq? 'generate (car req))
                      (rand-update num))
                     ((eq? 'reset (car req))
                      (cadr req))
                     (else (error "invalid req: " req))))
                  request-stream random-numbers))) ; request-stream in the front
  random-numbers)

(define randoms (make-rand 0 (stream '(generate)
                                     '(generate)
                                     '(generate)
                                     '(reset 100)
                                     '(generate)
                                     '(generate))))
; Exercise 3.82
(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (* range (random)))))

(define (random-number-pairs low1 high1 low2 high2)
  (cons-stream (cons (random-in-range low1 high1)
                     (random-in-range low2 high2))
               (random-number-pairs low1 high1 low2 high2)))

(define (monte-carlo2 experiment-stream passed failed)
  (define (next passed failed)
    (cons-stream
     (/ passed (+ passed failed))
     (monte-carlo2
      (stream-cdr experiment-stream) 
      passed 
      failed)))
  (if (stream-car experiment-stream)
      (next (+ passed 1) failed)
      (next passed (+ failed 1))))

(define (in-circle? pair)
  (let ((xl (sqr (- (car pair) 5)))
        (yl (sqr (- (cdr pair) 7))))
    (<= (+ xl yl) 9)))

; return size stream
(define (estimate-integral P x1 x2 y1 y2)
  (let* ((size (* (- x2 x1) (- y2 y1)))
         (randoms (random-number-pairs x1 x2 y1 y2))
         (probability
          (monte-carlo2 (stream-map P randoms) 0 0)))
    (scale-stream probability size)))

(define estimate-pi (scale-stream (estimate-integral in-circle? 2 8 4 10) (/ 1 9.0))) ; divided by r^2