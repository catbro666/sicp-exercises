# Foreword

What does matter is how well they perform and how smoothly they fit with other programs in the creation of still greater programs. The programmer must seek both perfection of part and adequacy of collection.

The source of the exhilaration associated with computer programming is the continual unfolding within the mind and on the computer of mechanisms expressed as programs and the explosion of perception they generate.

随着程序变大变复杂，规范本身的充分性、一致性和正确性值得怀疑，所以很难大程序很难有完整正式的正确性论证。因此开发一个标准程序结构库（idoms）并且使用经证明有价值的组织技术将它们组合成更大的结构变得非常重要。

Lisp vs Pascal

The list, Lisp’s native data structure, is largely responsible for such growth of utility.  The simple structure and natural applicability of lists are reflected in functions that are amazingly nonidiosyncratic.  In Pascal the plethora of declarable data structures induces a specialization within functions that inhibits and penalizes casual cooperation.

It is better to have 100 functions operate on one data structure than to have 10 functions operate on 10 data structures.  As a result the pyramid must stand unchanged for a millennium; the organism must evolve or perish.

Lisp语法和语义的简单性导致了所有Lisp程序员所导致的负担和自由。No Lisp program of any size beyond a few lines can be written without being saturated with discretionary functions.  Invent and fit; have fits and reinvent!  We toast the Lisp programmer who pens his thoughts within nests of parentheses.



计算机语言不仅仅是计算机执行操作的一种方法，而是表达关于方法论的点子的新颖正式的媒介。

本书，作为入门级读物，主要关注**用于控制大软件系统智力复杂度的技术**。

Mathematics provides a framework for dealing precisely with notions of “what is.”  Computation provides a framework for dealing precisely with notions of “how to.”

# 1 Building Abstractions with Procedures

The acts of the mind, wherein it exerts its power over simple ideas, are chiefly these three: 1. Combining several simple ideas into one compound one, and thus all complex ideas are made.  2. The second is bringing two ideas, whether simple or complex, together, and setting them by one another so as to take a view of them at once, without uniting them into one, by which it gets all its ideas of relations.  3. The third is separating them from all other ideas that accompany them in their real existence: this is called abstraction, and thus all its general ideas are made.

—John Locke, An Essay Concerning Human Understanding (1690)



We are about to study the idea of a  computational process. Computational **process**es are abstract beings that inhabit computers.  As they evolve, processes manipulate other abstract things called **data**.  The evolution of a process is directed by a pattern of rules called a **program**.  People create programs to direct processes.  In effect, we conjure the spirits of the computer with our spells.

A computational process is indeed much like a sorcerer’s idea of a spirit.  It cannot be seen or touched.  It is not composed of matter at all.  However, it is very real.  It can perform intellectual work.  It can answer questions.  It can affect the world by disbursing money at a bank or by controlling a robot arm in a factory.  The programs we use to conjure processes are like a sorcerer’s spells.  They are carefully composed from symbolic expressions in arcane and esoteric **programming languages** that prescribe the tasks we want our processes to perform.



## 1.1 The Elements of Programming

编程语言不仅是一种让计算机执行任务的方法。语言也是一种组织我们关于进程想法的框架。为了将简单的想法组合成更加复杂的想法，每个强大的语言都有3个机制：

- primitive expressions
- means of combination
- means of abstraction

### Expressions

`(/ 10 2)`

combination operator operands

**prefix notation**: no ambiguity. very straightforward to allow combinations nested.

### Naming and the Environment

`(define size 2)`

It should be clear that the possibility of associating values with symbols and later retrieving them means that the interpreter must maintain some sort of memory that keeps track of the name-object pairs.  This memory is called the **environment** (more precisely the global environment)

### Evaluating Combinations

the evaluation rule is recursive in nature.

Each special form like `define` has its own evaluation rule. The various kinds of expressions constitute the syntax of the programming language.

### Compound Procedures

some of the elements that must appear in any powerful pl.

- Numbers and arithmetic operations are primitive data and procedures.
-  Nesting of combinations provides a means of combining operations.
-  Definitions that associate names with values provide a limited means of abstraction.

**procedure definitions**: 

`(define (square x) (* x x)`

### The Substitution Model for Procedure Application

applicative-order evaluation: evaluate the arguments and then apply.

normal-order evaluation: fully expand and then reduce.

List uses applicative-order evaluation, partly because of the additional efficiency obtained from avoiding multiple evaluations of expressions, and more significantly, because normal-order evaluation becomes much more complicated to deal with when we leave the realm of procedures that can be modeled by substitution.

### Conditional Expressions and Predicates

`(cond (<p1> <e1>) ... (<pn> <en>))`

predicate, consequent expression

`if <predicate> <consequent> <alternative>`



**Exercise 1.5** if the interpreter uses applicative-order evaluation, it'll enter the death loop.

**Exercise 1.6** the then-clause and else-clause will always be evaluated.

The contrast between function and procedure is a reflection of the general distinction between describing properties of things and describing how to do things, or as it is sometimes referred to, the distinction between declarative knowledge and imperative knowledge.

### Procedures as Black-Box Abstractions

suppress the detail.

## 1.2 Procedures and the Processes They Generate

learn to visualize the processes generated by various types of procedures.

In this section we will examine some common “shapes” for processes generated by simple procedures.  We will also investigate the rates at which these processes consume the important computational resources of time and space.”

### Linear Recursion and Iteration

recursive process 进程本身如何发展

recursive procedure 语法上该procedure的定义引用了自己

尾递归可以用递归的形式表示迭代。

### Tree Recursion

时间正比于节点数，空间正比于树的深度。

Example: counting change 换零钱

```scheme
(define (count-change amount)
  (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) 
             (= kinds-of-coins 0)) 
         0)
        (else 
         (+ (cc amount (- kinds-of-coins 1))
            (cc (- amount (first-denomination 
                           kinds-of-coins))
                kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))
```

类似斐波那契数列，有很多冗余的计算。



```scheme
(define (count-change2 n)
  (cond
    [(< n 0) 0]
    [(= n 0) 1]
    [else (cc2 n 0 1 1 0 0 0)]))
; n: target amount
; cur: current amount
; sum: current number of ways to change
; d1: the delta of the last 2 sum's
; d2
; d25: 目前25/50的表示方法数
; 前一轮的所有方法数,这一轮加面值1
; +5的必须前一轮没有面值1,否则跟原来有5+1的重复
; +10的必须前一轮只有一个面值5,因为d都是前一轮加5的个数
; 所以需要再前一轮不加5的个数,即d2-d3
; +25的,25时为1,后面每满50再加1
(define (cc2 n cur sum d1 d2 d3 d25)
  (let ([next (+ cur 5)])
    (if (> next n) sum
        (let* ([new5 d1]
               [new10 (- d2 d3)]
               [new25 (cond
                        [(or (= next 25)
                             (= 0 (remainder next 50)))
                         (add1 d25)]
                        [(= 0 (remainder next 25)) d25]
                        [else 0])]
               [newd1 (+ new5 new10 new25)]
               [newd25 (if (= 0 new25) d25 new25)])
          (cc2 n next (+ sum newd1) newd1 d1 d2 newd25)))))
```

如果面值可变呢？这不是一种通用的解法。

Exercise 1.12 Pascal's triangle

f(x y) = f(x-1 y-1) + f(x-1 y)

for x==1 or x==y, f==1



Exercise 1.13 Prove that Fib(n) is the closest integer to `(φ^n)/√5`, where `φ=(1+√5)/2`.

??

### Orders of Growth

Exercise 1.14: count-change的复杂度

时间和空间都是O(n)

### Exponentiation

Exercise 1.16 迭代方式的指数实现，要求对数时间

```scheme
(define (even? n)
  (= (remainder n 2) 0))

(define (fast-expt2 b n)
  (fexp 1 b n))

(define (fexp a b n)
  (cond
    [(= n 0) a]
    [(even? n) (fexp a (sqr b) (/ n 2))]
    [else (fexp (* a b) b (- n 1))]))
```

Exercise 1.17/18 用加法，double和halve模拟乘法，要求对数时间，迭代

```scheme
(define (double x) (* x 2))
(define (halve x) (/ x 2))

(define (fast-mul a b)
  (fmul 0 a b))

(define (fmul n a b)
  (cond
    [(= b 0) n]
    [(even? b) (fmul n (double a) (halve b))]
    [else (fmul (+ n a) a (- b 1))]))
```

Exercise 1.19: 快速计算Fibonacci的方法，两步并一步 p=0，q=1

a <- bq+aq+ap, b <- bp+aq

两次变换合成一次，将p', q'用p和q表示

p' = p^2+q^2, q' = 2pq + q^2

```scheme
(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0) 
         b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (sqr p) (sqr q))
                   (+ (* 2 p q) (sqr q))
                   (/ count 2)))
        (else 
         (fib-iter (+ (* b q) 
                      (* a q) 
                      (* a p))
                   (+ (* b p) 
                      (* a q))
                   p
                   q
                   (- count 1)))))
```

### Greatest Common Divisors

```scheme
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))
```

由Lamé’s Theorem可知是对数复杂度

### Example: Testing for Primality

搜索除数，复杂度O(√n)

Fermat小定理：n是质数，a是小于n任意正整数，a的n次方与a模n同余。

随机选取a<n，测试失败则是非质数，成功次数越多概率越高。这是一种概率算法。

## 1.3 Formulating Abstractions with Higher-Order Procedures

### Procedures as Arguments

```scheme
(define (cube x) (* x x x))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))
```



Exercise 1.29: Simpson's Rule计算积分

```scheme
(define (integral f a b n)
  (define h (/ (- b a) n))
  (define (add-h x) (+ x h))
  (define h/3 (/ h 3))
  (define (coef c)
    (cond
     [(or (= c 0)
          (= c n))
      1]
     [(odd? c) 4]
     [else 2]))
  (define (sum x c)
    (if (> x b)
        0
        (+ (* (coef c) (f x))
           (sum (add-h x) (inc c)))))
  (* h/3 (sum a 0)))
```



Exercise 1.30: 重写sum函数为迭代方式

```scheme
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))
```

Exercise 1.31: 实现连乘product

```scheme
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

(define (pi n)
  (define (term a)
    (if (even? a)
        (/ a (add1 a))
        (/ (add1 a) a)))
  (* 4 (product term 2 add1 n)))

(exact->inexact (pi 100))
```

Exercise 1.32: 比sum和product更一般的accumulate

```scheme
(define (accumulate combiner null-value term a next b)
  (define (iter a)
    (if (> a b)
        null-value
        (combiner (term a)
                  (iter (next a)))))
  (iter a))

(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))
```

Exercise 1.33: 比accumulate更通用的版本，引入一个filter，只combine满足条件的那些terms

```scheme
(define (filtered-accumulate combiner filter null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a)
              (if (filter a)
                  (combine result (term a))
                  result))))
  (iter a null-value))
```

a到b范围内质数的平方和

```scheme
(define (sosqop a b)
  (filtered-accumulate + prime? 0 sqr a add1 b))
```

所有小于n且与n互质的正整数的乘积

```scheme
(define (porp n)
  (filtered-accumulate * (lambda (i)
                           (= 1 (gcd i n)))
                       1 identity 2 add1 n))
```

### Constructing Procedures Using Lambda

let表达式只是lambda application的语法糖。

### Procedures as General Methods

半区间法求方程的根

```scheme
(define (search f neg-point pos-point)
  (let ((midpoint 
         (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond 
           ((positive? test-value)
            (search f neg-point midpoint))
           ((negative? test-value)
            (search f midpoint pos-point))
           (else midpoint))))))
```

求函数的固定点f(x)=x，可以这么猜测f(x), f(f(x)), f(f(f(x))), ...

```scheme
(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))
```

Exercise 1.35: 黄金比是x -> 1 + 1/x的固定点

```scheme
(define golden-ratio
  (fixed-point
   (lambda (x)
     (+ 1 (/ 1 x)))
   1.6))
```

Exercise 1.36: 修改fixed-point，使用newline和display打印近似值序列。然后求解x^x = 1000，（通过求固定点x->log(1000)/log(x)）比较有和没有平均值阻尼的步数。

```scheme
(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (newline)
      (display next)
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))
```



```scheme
(define x^x1000
  (fixed-point
   (lambda (x)
     (/ (log 1000) (log x)))
   4))

(define (average a b)
  (/ (+ a b) 2))

(define x^x1000
  (fixed-point
   (lambda (x)
     (average x
              (/ (log 1000) (log x))))
   4))
```

有阻尼的更快收敛

Exercise 1.37: 连续分数f=N1/(D1+N2/(D2+N3/(D3+…)))

然后用cont-frac生成近似的1/φ

```scheme
(define (cont-frac n d k)
  (define (iter i)
    (if (> i k)
        0
        (/ (n i) (+ (d i) (iter (add1 i))))))
  (iter 1))

(define (cont-frac n d k)
  (define (iter i res)
    (if (= i 0)
        res
        (iter (dec1 i) (/ (n i) (+ (d i) res)))))
  (iter k 0))

(define (phi k)
  (cont-frac (lambda (i) 1.0)
             (lambda (i) 1.0)
             k))
```

Exercise 1.38: e-2的连续分数扩展，Ni全是1，Di是1,2,1,1,4,1,1,6,1,1,8,...

```scheme
(define (e k)
  (define (n i) 1.0)
  (define (d i)
    (let ([r (remainder (add1 i) 3)])
      (if (= r 0)
          (* 2 (/ (add1 i) 3))
          1)))
  (+ 2 (cont-frac n d k)))
```

Exercise 1.39: tan的连续分数表示tan x = x/(1-(x^2/(3-(x^2/(5-...))))，其中x是弧度

```scheme
(define (tan-cf x k)
  (define (n i)
    (if (= i 1)
        x
        (- (sqr x))))
  (define (d i) (- (* 2 i) 1))
  (cont-frac n d k))
```

### Procedures as Returned Values

继续看固定的的例子，把平均阻尼表示为

```scheme
(define (average-damp f)
  (lambda (x)
    (average x (f x))))
```

然后平方根可以重写为

```scheme
(define (sqrt x)
  (fixed-point
   (average-damp
    (lambda (y) (/ x y)))
   1.0))
```

牛顿法，如果g(x)可微，g(x)=0的一个解是函数f(x)的一个固定点f(x)=x - g(x)/Dg(x)

```scheme
(define (deriv g)
  (let ([dx 0.00001])
    (lambda (x)
      (/ (- (g (+ x dx)) (g x))
         dx))))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x)
            ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g)
               guess))
```

求x的平方根可以用牛顿法求y->y^2-x的零值。注意这里y是变量，x看作常数。

```scheme
(define (sqrt x)
  (newtons-method
   (lambda (y)
     (- (sqr y) x))
   1.0))
```

高阶步骤的意义是使我们可以将这些抽象显式表示为编程语言中的元素。

更一般地来说，编程语言对计算元素的操作方式施加限制。具有最少限制的元素被称为具有一等地位first-class status。其特权包括：

- 可以命名为变量
- 可以作为参数传递
- 可以作返回值返回
- 可以包含在数据结构中

Exercise 1.40: 定义cubic可以与newtons-method一起求近似零值

```scheme
(define (cubic a b c)
  (lambda (x)
    (+ (cube x)
       (* a (sqr x))
       (* b x)
       c)))
```

Exercie 1.41: 定义double接收一个一个参数的步骤返回一个步骤应用原来的步骤两次

```scheme
(define (double f)
  (lambda (x)
    (f (f x))))
```

`(((double (double double)) add1) 5)`结果是21，每多一个，平方。

Exercise 1.42: f和g是两个单参数函数，compose函数组合f和g为x->f(g(x))

```scheme
(define (compose f g)
  (lambda (x)
    (f (g x))))
```

Exercise 1.43: 计算n次重复应用f

```scheme
(define (repeated f n)
  (lambda (x)
    (define (iter n)
       (if (= n 0)
           x
           (f (iter (sub1 n)))))
     (iter n)))

(define (repeated f n)
  (lambda (x)
    (define (iter n res)
      (if (= n 0)
          res
          (iter (sub1 n) (f res))))
    (iter n x)))
```

Exercise 1.44: f的光滑版本是f(x-dx), f(x), f(x+dx)的平均值。然后用smooth和repeated生成n-fold smoothed function

```scheme
(define (smooth f)
  (lambda (x)
    (let ([dx 0.01])
      (average (f (- x dx))
               (f x)
               (f (+ x dx))))))

(define (n-fold-smooth f n)
  (repeated (smooth f) n))
```

Exercise 1.45: 计算平方根一次平均阻尼就收敛了，立方根也只要一次就收敛了，但是4次方根需要两次才收敛。实验计算n次方根需要几次平均阻尼，然后用fixed-point, average-damp实现一个计算n次方根的方法。

```scheme
(define (nthroot n r x)
  (fixed-point
   ((repeated average-damp r)
    (lambda (y) (/ x (expt y (sub1 n)))))
   1.0))

(define (test-converge n r)
  (for ([i 1 1000])
    (nthroot n r
             (add 1 (random 1000000000)))))
```

4开始需要2次，8次方开始需要3次，16次方开始需要4次

```scheme
(define (nthroot n x)
  (fixed-point
   ((repeated average-damp
              (floor (log n 2)))
    (lambda (y) (/ x (expt y (sub1 n)))))
   1.0))

(define (test-converge n)
  (for ([i 1000])
    (nthroot n (add1 (random 1000000000)))))
```

Exercise 1.46: 前面多次涉及了一个通用的计算策略：迭代提升。实现iterative-improve接收两个步骤作为参数，一个判断guess是否足够好，一个提升guess。然后用它重写sqrt和fixed-point

```scheme
(define (iterative-improve good-enough? improve)
  (lambda (x first)
    (define (try guess)
      (let ([next (improve guess x)])
        (if (good-enough? guess next)
            next
            (try next))))
    (try first-guess)))

(define sqrt2
  (iterative-improve
   (lambda (guess next)
     (< (abs (- guess next)) 0.00001))
   (lambda (guess x)
     (average guess (/ x guess)))))

(define fixed-point
  (iterative-improve
   (lambda (guess next)
     (< (abs (- guess next)) 0.00001))
   (lambda (guess x)
     (x guess))))
```



# 2 Building Abstractions with Data

第一章关注通过组合过程形成复合过程来构建抽象，本章聚焦通过组合数据对象形成复合数据来构建抽象。

为什么要复合数据？跟我们要复合过程相同的原因。 to elevate the conceptual level at which we can design our programs, to increase the modularity of our designs, and to enhance the expressive power of our language.

## 2.1 Introduction to Data Abstraction

步骤抽象抑制了实现细节，数据抽象隔离了复合数据的内部构造细节。具体数据表现的定义独立于使用数据的程序。两者之间的接口是一组过程，selectors和constructors。

### Example: Arithmetic Operations for Rational Numbers

假设我们已经有了构造函数和分子分母提取函数`(make-rat <n> <d>)`, `(numer <x>)`, `(denom <x>)`。

这里使用了一种强大的综合策略(strategy of synthesis): **wishful thinking**. 我们还不知道有理数怎么表示，3个函数怎么实现的，但我们可以用它们来进行运算了。

cons: construct

car: Contents of Address part of Register

cdr: Contents of Decrement part of Register

Exercise 2.1: 改善make-rat支持正负数，如果是负数，分子为负。

```scheme
(define (make-rat n d)
  (let* ((g (gcd n d))
         (n1 (< n 0))
         (n2 (< d 0))
         (isneg (xor n1 n2))
         (n (if (xor n1 isneg) (- n) n))
         (d (abs d)))
    (cons (/ n g) 
          (/ d g))))
```

### Abstraction Barriers

抽象栅栏隔离不同的层，每层栅栏将使用数据抽象的程序和实现数据抽象的程序分开。

Exercise 2.2: 考虑平面内线段的表示问题，make-segment, start-segment, end-segment。点表示为一对数字，make-point, x-point, y-point。最后定义一个过程midpoint-segment接收一个线段返回其中点。

```scheme
(define (make-segment sp ep)
  (cons sp ep))

(define (start-segment seg)
  (car seg))

(define (end-segment seg)
  (cdr seg))

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (midpoint-segment seg)
  (let* ((sp (start-segment seg))
         (ep (end-segment seg))
         (sx (x-point sp))
         (sy (y-point sp))
         (ex (x-point ep))
         (ey (y-point ep)))
    (make-point (/ (+ sx ex) 2)
                (/ (+ sy ey) 2))))
```

Exercise 2.3: 实现平面中长方形的表示，然后创建计算周长和面积的步骤。然后改变长方形的表示方式，你设计的系统时候有合适的抽象栅栏，从而不用修改上层的周长和面积过程。

```scheme
(define (make-rectangle l w cp ang)
  (list l w cp ang))

(define (len-rectangle rect)
  (first rect))

(define (wid-rectangle rect)
  (second rect))

(define (perimeter-rectangle rect)
  (* 2 (+ (len-rectangle rect)
          (wid-rectangle rect))))

(define (area-rectangle rect)
  (* (len-rectangle rect)
     (wid-rectangle rect)))
```

### What Is Meant by Data?

数据可以认为是步骤+这些步骤必须满足的条件。

所以我们其实可以不用任何数据结构，只用步骤就实现cons, car, cdr

```scheme
(define (cons x y)
  (define (dispatch m)
    (cond
     ((= m 0) x)
     ((= m 1) y)
     (else
      (error "Argument not 0 or 1: CONS" m))))
  dispatch)

(define (car z) (z 0))
(define (cdr z) (z 1))
```

数据的步骤化表示在编程剧目中发挥核心作用。这种编程风格经常被叫做message passing。第3章处理建模和仿真问题时会把它作为一个基本工具。

Exercise 2.4: 下面是pairs的另一种过程式表示。前面那种行为基本由cons决定，这种则交由car/cdr决定

```scheme
(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

(define (cdr z)
  (z (lambda (p q) q)))
```

Exercise 2.5: 将a和b对表示一个整数(2^a)*(3^b)，给出对应的cons, car, cdr定义。

```scheme
(define (cons x y)
  (* (expt 2 x) (expt 3 y)))

(define (car z)
  (define (iter z n)
    (let ((r (remainder z 2)))
      (if (= r 0)
          (iter (/ z 2) (add1 n))
          n)))
  (iter z 0))

(define (cdr z)
  (define (iter z n)
    (let ((r (remainder z 3)))
      (if (= r 0)
          (iter (/ z 3) (add1 n))
          n)))
  (iter z 0))
```

Exercise: 2.6: 如果将pairs表示为步骤还不够烧脑的话，考虑一下在一个可以操作步骤的语言中，我们可以通过实现0和加1的操作来解决没有数字的问题（至少就非负整数而言）

```scheme
(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))
```

这种表示叫做**Church numerals**。直接定义one和two（不是用zero和add-1，用代入法评估(add-1 zero)）。直接定义加法步骤（不是用重复应用add-1）

```scheme
(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))
(define three (lambda (f) (lambda (x) (f (f (f x))))))

(define (+ m n)
  (lambda (f) (lambda (x) ((n f) ((m f) x)))))

(define (* m n)
  (lambda (f) (lambda (x) ((n (m f)) x))))

> (((+ three three) add1) 0)
6
>  (((* three three) add1) 0)
9
```

### Extended Exercise: Interval Arithmetic

区间算术，操作已知精度的不精确量。

```scheme
(define (add-interval x y)
  (make-interval (+ (lower-bound x) 
                    (lower-bound y))
                 (+ (upper-bound x) 
                    (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) 
               (lower-bound y)))
        (p2 (* (lower-bound x) 
               (upper-bound y)))
        (p3 (* (upper-bound x) 
               (lower-bound y)))
        (p4 (* (upper-bound x) 
               (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x 
                (make-interval 
                 (/ 1.0 (upper-bound y)) 
                 (/ 1.0 (lower-bound y)))))
```

Exercise 2.7: 区间抽象实现

```scheme
(define (make-interval a b) (cons a b))

(define (upper-bound x) (cdr x))

(define (lower-bound x) (car x))
```

Exercise 2.8: 区间减法实现sub-interval

```scheme
(define (sub-interval x y)
  (make-interval (- (lower-bound x)
                    (upper-bound y))
                 (- (upper-bound x)
                    (lower-bound y))))
```

Exercise 2.9: width是区间长度的一半，加减法的结果的width是参数的width的函数，乘除法则不是这样。

```scheme
(define (width-interval x)
  (/ (- (upper-bound x)
        (lower-bound x))
     2))
```

Exercise 2.10: 除以一个跨越0的区间意义不清楚，修改代码检查这个条件，出现时报错。

```scheme
(define (div-interval x y)
  (let* ((y-up (upper-bound y))
         (y-low (lower-bound y))
         (p (* y-up y-low)))
    (if (<= p 0)
        (error "can not divide by an interval that spans zero")
        (mul-interval
         x
         (make-interval 
          (/ 1.0 (upper-bound y))
          (/ 1.0 (lower-bound y)))))))
```

Exercise 2.11: 通过测试区间端点的符号，可以将mul-interval分成9种情况，只有1种需要超过2次乘法。

```scheme
(define (mul-interval x y)
  (let* ((xl (lower-bound x))
         (xu (upper-bound x))
         (yl (lower-bound y))
         (yu (upper-bound y))
         (sxl (>= xl 0))
         (sxu (>= xu 0))
         (syl (>= yl 0))
         (syu (>= yu 0)))
    (cond
     ((and sxl syl)                     ; [1 2] [1 2]
      (make-interval (* xl yl)
                     (* xu yu)))
     ((and (not sxu) (not syu))         ; [-2 -1] [-2 -1]
      (make-interval (* xu yu)
                     (* xl yl)))
     ((and sxl (not syu))               ; [1 2] [-2 -1]
      (make-interval (* xu yl)
                     (* xl yu)))
     ((and (not sxu) syl)               ; [-2 -1] [1 2]
      (make-interval (* xl yu)
                     (* xu yl)))
     ((and (not sxl) sxu syl)           ; [-1 1] [1 2]
      (make-interval (* xl yu)
                     (* xu yu)))
     ((and (not sxl) sxu (not syu))     ; [-1 1] [-2 -1]
      (make-interval (* xu yl)
                     (* xl yl)))
     ((and sxl (not syl) syu)           ; [1 2] [-1 1]
      (make-interval (* xu yl)
                     (* xu yu)))
     ((and (not sxu) (not syl) syu)     ; [-2 -1] [-1 1]
      (make-interval (* xl yu)
                     (* xl yl)))
     (else                              ; [-1 1] [-1 2]
      (make-interval (min (* xl yu)
                          (* xu yl))
                     (max (* xl yl)
                          (* xu yu)))))))
```

Exercise 2.12: 表示成中心+误差方式

```scheme
(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) 
        (upper-bound i)) 
     2))

(define (width i)
  (/ (- (upper-bound i) 
        (lower-bound i)) 
     2))
```

表示成width关于interval的百分比的形式

```scheme
(define (make-center-percent c p)
  (make-interval (* c (- 1 p))
                 (* c (+ 1 p))))

(define (percent i)
  (let* ((low (car i))
         (up (cdr i))
         (wid (/ (- up low) 2))
         (center (/ (+ low up) 2)))
    (/ wid center)))
```

Exercise 2.13: 对于非常小的误差百分比，乘积的误差是两个因子误差百分比的和。计算并联电阻两种方式的结果不一样

```scheme
(define (par1 r1 r2)
  (div-interval 
   (mul-interval r1 r2)
   (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval 
     one
     (add-interval 
      (div-interval one r1) 
      (div-interval one r2)))))
```

方法1的误差大，乘法和除法都将误差相加了。方法2只有一次加法误差取大者。



Exercise 2.16: 为什么代数上等价的表达式会导致不一样的结果。可以设计一个没有这种缺点的区间算术包吗？这是否不可能？（警告：这个问题很难）

同一个变量出现了多次，它们其实是同一个值，所以它们的误差不应该被叠加。但是不是每个表达式都可以改写成每个变量都只出现一次的情况。所以答案是否定的。

## 2.2 Hierarchical Data and the Closure Property

可以用**box-and-pointer notation**

closure property。对于一个组合数据对象的操作，如果组合的结果仍然可以使用相同的操作进行组合，我们说它满足关闭属性。（closure一词来源于抽象代数，如果对集合中元素的操作生成的元素仍然是集合的元素，我们说这个集合对于这个操作是关闭的。不幸的是closure还被用于另一个完全不相关的概念：过程+自由变量）

Closure使得我们可以创建层级结构。

### Representing Sequences

nil来源于拉丁词nihil，是nothing的意思。

Exercise 2.17: 定义last-pair，接收一个非空list，返回只包含最后一个元素的list

```scheme
(define (last-pair l)
  (cond
   [(= (length l) 1) l]
   [else (last-pair (cdr l))]))
```

Exercise 2.18: 定义reverse反序list

```scheme
(define (reverse l)
  (define (iter l rl)
    (cond
     [(null? l) rl]
     [else (iter (cdr l) (cons (car l) rl))]))
  (iter l '()))
```

Exercise 2.19: 修改换零钱程序，硬币面值改成可配

```scheme
(define us-coins 
  (list 50 25 10 5 1))

(define uk-coins 
  (list 100 50 20 10 5 2 1 0.5))

(define (cc amount coin-values)
  (cond ((= amount 0) 
         1)
        ((or (< amount 0) 
             (null? coin-values)) 
         0)
        (else
         (+ (cc 
             amount
             (cdr coin-values))
            (cc 
             (- amount
                (car coin-values))
             coin-values)))))
```

coin-values的顺序不影响结果，因为只是交换了组合的顺序。

Exercise 2.20: dotted-tail notation

```scheme
(define (f x y . z) <body>) ; two or more arguments
(define (g . w) <body>)     ; zero or more arguments
```

写一个步骤same-parity，接收一个或更多整数，返回和第一个参数奇偶性相同的数的list

```scheme
(define (same-parity x . l)
  (let ([rx (remainder x 2)])
    (define (same-parity? a)
      (let ([ra (remainder a 2)])
        (= rx ra)))
    (define (iter l)
      (cond
       [(null? l) '()]
       [(same-parity? (car l))
        (cons (car l) (iter (cdr l)))]
       [else
        (iter (cdr l))]))
    (cons x (iter l))))
```

map建立了处理list的高层抽象，保留从序列转换到序列的概念框架。

Exercise 2.21: square-list的两个实现

```scheme
(define (square-list items)
  (if (null? items)
      '()
      (cons (sqr (car items))
            (square-list (cdr items)))))

(define (square-list items)
  (map (lambda (x) (sqr x))
       items))
```

Exercise 2.22: 改成迭代之后反序了，因为先cons的在后面。但是又不能交换cons的位置，否则遍历的方式就变了。

Exercise 2.23: for-each只是依次应用过程，过程的返回值没有使用。实现for-each

```scheme
(define (for-each f l)
  (cond
   [(null? l) (void)]
   [else (f (car l))
         (for-each f (cdr l))]))
```

### Hierarchical Structures

除了用box-and-pointer，还可以用树来表示。

Exercise 2.25: 给出car和cdr的组合选取下面list中的7

```scheme
(car (cdr (car (cdr (cdr '(1 3 (5 7) 9))))))
(car (car '((7))))
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr '(1 (2 (3 (4 (5 (6 7))))))))))))))))))
```

Exercise 2.26:

```scheme
(define x (list 1 2 3))
(define y (list 4 5 6))
(append x y) ; '(1 2 3 4 5 6)
(cons x y)   ; '((1 2 3) 4 5 6)
(list x y)   ; '((1 2 3) (4 5 6))
```

append是简单的一个指针修改，将前一个list的最后一个pair的cdr指向后一个list。两个串合成一个

cons是新增了一个box，box的car指向第一个list，cdr指向后一个list。前一个串变成后一个串的第一个元素。

list有几个成员就添加几个box，几个box串成一个list。

Exercise 2.27: 修改练习2.18写一个deep-reverse，使其子串也反序

```scheme
(define (deep-reverse l)
  (define (iter l rl)
    (cond
     [(null? l) rl]
     [(pair? (car l))
      (iter (cdr l) (cons (iter (car l) '()) rl))]
     [else (iter (cdr l) (cons (car l) rl))]))
  (iter l '()))

(define x 
  (list (list 1 2) (list 3 4)))
(deep-reverse x)  ; ((4 3) (2 1))
```

Exercise 2.28: 写一个fringe函数，参数时一个树，返回一个list包含所有的叶子节点，从左往右的顺序。

```scheme
(define (fringe t)
  (define (iter l rl)
    (cond
     [(null? l) rl]
     [(pair? (car l))
      (iter (cdr l)
            (iter (car l) rl))]
     [else (iter (cdr l)
                 (cons (car l) rl))]))
  (iter t '()))
; 这个反序了

(define (fringe t)
  (define (iter l)
    (cond
     [(null? l) '()]
     [(pair? (car l))
      (append (iter (car l))
              (iter (cdr l)))]
     [else (cons (car l) (iter (cdr l)))]))
  (iter t))
```

Exercise 2.29: 一个二进制mobile雕塑有两个分支，每个枝是一根特定长度的杆子，上面挂一个重物或者另一个binary mobile。

```scheme
(define (make-mobile left right)
  (list left right))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (car (cdr mobile)))

; structure is either a number or another mobile
(define (make-branch length structre)
  (list length structure))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (car (cdr branch)))

(define (branch-weight branch)
  (let ((struct (branch-structure branch)))
    (if (pair? struct)
        (mobile-weight struct)
        struct)))

(define (mobile-weight m)
  (+ (branch-weight (left-branch m))
     (branch-weight (right-branch m))))
```

如果左枝长度乘以重量跟右边相等（子mobile也同样如此），则说一个mobile是平衡的。设计一个谓词判断是否平衡

```scheme
(define (balanced? mobile)
  (define (iter m)
    (let* ([left (left-branch m)]
           [right (right-branch m)]
           [leftlen (branch-length left)]
           [rightlen (branch-length right)]
           [leftstruct (branch-structure left)]
           [rightstruct (branch-structure right)]
           [leftweight (if (pair? leftstruct)
                           (iter leftstruct)
                           leftstruct)])
      (if (boolean? leftweight)
          #false
          (let ([rightweight (if (pair? rightstruct)
                                 (iter rightstruct)
                                 rightstruct)])
            (cond
             [(boolean? rightweight) #false]
             [(= (* leftlen leftweight)
                 (* rightlen rightweight))
              (+ leftweight rightweight)]
             [else #false]))))))
```

如果mobile和branch都改成cons，只需要改构造函数和selector。



Mapping over trees

```scheme
(define (scale-tree tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (scale-tree sub-tree factor)
             (* sub-tree factor)))
       tree))
```



Exercise 2.30: 定义square-tree类似square-list

```scheme
(define (square-tree items)
  (map (lambda (subtree)
         (if (pair? subtree)
             (square-tree subtree)
             (sqr subtree)))
       items))

(define (square-tree items)
  (cond
   [(null? items) '()]
   [(not (pair? items))
    (sqr items)]
   [else
    (cons (square-tree (car items))
          (square-tree (cdr items)))]))

(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))
; (1 (4 (9 16) 25) (36 49))
```

Exercise 2.31: 抽象上一练习的结果，生成一个tree-map过程

```scheme
(define (tree-map f t)
  (map (lambda (subtree)
         (if (pair? subtree)
             (tree-map f subtree)
             (f subtree)))
       t))

(define (square-tree tree)
  (tree-map sqr tree))
```

Exercise 2.32: 生成一个集合的所有子集的集合

```scheme
(define (subsets s)
  (if (null? s)
      '(())
      (let ((rest (subsets (cdr s)))
            (e (car s)))
        (append rest (map (lambda (subset)
                            (cons e subset))
                          rest)))))
(subsets '(1 2 3))
```

集合s的所有子集=除去某个元素e之后的集合的所有子集+这些子集每个中都加上元素e

### Sequences as Conventional Interfaces

Sequence Operations

组织程序以使其能够更清晰地反映信号流结构的关键是专注于流从一个阶段到下一个阶段的“信号”。如果我们将这些信号表示成list，那么我就可以使用list操作来实现每个阶段的处理。

```scheme
(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate 
                       (cdr sequence))))
        (else  (filter predicate 
                       (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op 
                      initial 
                      (cdr sequence)))))

(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append 
               (enumerate-tree (car tree))
               (enumerate-tree (cdr tree))))))

(define (sum-odd-squares tree)
  (accumulate 
   +
   0
   (map square
        (filter odd?
                (enumerate-tree tree)))))

(define (even-fibs n)
  (accumulate 
   cons
   nil
   (filter even?
           (map fib
                (enumerate-interval 0 n)))))
```

将程序表达成序列操作的价值是这可以帮助我们使程序设计模块化。我们可以通过提供标准组件库以及用灵活的方式连接这些组件的一个传统接口来鼓励模块化设计。

```scheme
; 前n+1个fib数的平方的list
(define (list-fib-squares n)
  (accumulate 
   cons
   nil
   (map square
        (map fib
             (enumerate-interval 0 n)))))

(list-fib-squares 10)
(0 1 1 4 9 25 64 169 441 1156 3025)
```

序列，这里实现为lists，作为传统的接口允许我们组合处理模块。另外，当我们统一表示结构为序列，我们也将程序中数据结构的依赖局限在较少的序列操作上。

Exercise 2.33: 将几个基本的列操作表示成accumulate

```scheme
(define (map p sequence)
  (accumulate (lambda (x y)
                (cons (p x) y)) 
              '() sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length sequence)
  (accumulate (lambda (x y)
                (add1 y))
              0 sequence))
```

Exercise 2.34: 关于x的多项式在某个给定x处的求值可以被表述为一种积累。

`(...(anx+an-1)x+...+a1)x+a0`

```scheme
; 假设系数是从a0到an
(define 
  (horner-eval x coefficient-sequence)
  (accumulate 
   (lambda (this-coeff higher-terms)
     (+ this-coeff (* higher-terms x)))
   0
   coefficient-sequence))

```

Exercise 2.35: 重新定义2.2.2的count-leaves为一个积累

```scheme
(define (count-leaves t)
  (accumulate (lambda (c s)
                (+ c s))
              0
              (map (lambda (e)
                     (if (pair? e)
                         (count-leaves e)
                         1))
                   t)))
```

Exercise 2.36: 步骤accumulate-n类似于accumulate，除了它的第三个参数是一个序列的序列，每个元素的长度是相同的。它应用指定的积累函数组合所有序列的第一个元素，组合所有序列的第二个元素，等等，然后返回结果的序列。

```scheme
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))
```

Exercise 2.37: 矢量和矩阵的计算

```scheme
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (w)
         (dot-product w v)) m))

(define (transpose mat)
  (accumulate-n cons '() mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (v)
           (matrix-*-vector cols v)) m)))
```

Exercise 2.38: accumulate又叫fold-right，因为它组合序列的第一个元素与右边所有元素的组合结果。fold-left类似，只是方向相反，从左往右组合，注意op的两个参数位置。

```scheme
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(fold-right / 1 (list 1 2 3)) ; 3/2
(fold-left / 1 (list 1 2 3))  ; 1/6
(fold-right list '() (list 1 2 3)) ; '(1 (2 (3 ())))
(fold-left list '() (list 1 2 3))  ; '(((() 1) 2) 3)
```

fold-right `(op x1 (op x2 (... (op xn init) ...)`

fold-left `(op (op ... (op init x1) x2) ... xn) `

给出op需要满足的一个特性，以使fold-right和fold-left将对任何序列产生相同的值。

两个参数满足交换率的情况，如+，*。

Exercise 2.39: 将reverse（练习2.18）用fold-right和fold-left表示

```scheme
(define (reverse sequence)
  (fold-right 
   (lambda (x y) (append y (list x))) '() sequence)) ; y是右边已经处理的结果

(define (reverse sequence)
  (fold-left 
   (lambda (x y) (cons y x)) '() sequence))            ; x是左边已经处理的结果
```



Nested Mappings

我们可以扩展序列范式以包括很多一般用嵌套循环来表示的计算。例如1<=j<i<=n的ij对，使其和为质数。

```scheme
(accumulate 
 append
 nil
 (map (lambda (i)
        (map (lambda (j) 
               (list i j))
             (enumerate-interval 1 (- i 1))))
      (enumerate-interval 1 n)))
```

这种mapping和accumulating append的组合非常常见，我们将它独立为一个过程

```scheme
(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))
```

然后过滤，最后生成结果

```scheme
(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) 
        (cadr pair) 
        (+ (car pair) (cadr pair))))
```

完整的函数如下

```scheme
(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter 
        prime-sum?
        (flatmap
         (lambda (i)
           (map (lambda (j) 
                  (list i j))
                (enumerate-interval 
                 1 
                 (- i 1))))
         (enumerate-interval 1 n)))))
```

不仅仅是枚举区间，例如生成一个集合的所有排列。

```scheme
(define (permutations s)
  (if (null? s)   ; empty set?
      (list '())  ; sequence containing empty set
      (flatmap (lambda (x)
                 (map (lambda (p) 
                        (cons x p))
                      (permutations 
                       (remove x s))))
               s)))

(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))
```

Exercise 2.40: 定义一个步骤unique-pairs，给出所有1<=j<i<=n的pairs(i,j)的序列。然后用它简化prime-sum-pairs的定义

```scheme
(define (unique-pairs n)
  (flatmap
   (lambda (i)
     (map (lambda (j)
            (list i j))
          (enumerate-interval 1 (sub1 i))))
   (enumerate-interval 1 n)))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter
        prime-sum?
        (unique-pairs n))))
```

Exercise 2.41: 写一个过程寻找所有满足以下条件的已排序正整数三元组i,j,k，它们都不大于n，各不相同，和为s。

```scheme
(define (unique-triples n)
  (flatmap
   (lambda (i)
     (flatmap
      (lambda (j)
        (map
         (lambda (k)
           (list k j i))
         (range 1 j)))
      (range 1 i)))
   (range 1 (add1 n))))

(define (given-sum-triples n s)
  (filter
   (lambda (t)
     (= s (+ (car t)
             (cadr t)
             (caddr t))))
   (unique-triples n)))
```

Exercise 2.42:八皇后问题。我们可以递归地构思这种方法，假设我们已经得到了前面k-1列放k-1个皇后的所有方法，然后将一个皇后放到第k列的每一行，过滤出安全的位置。

```scheme
(define (adjoin-position r c rest-of-queens)
  (cons (cons r c) rest-of-queens))

(define (safe? k positions)
  (let ((new-queen (car positions)))
    (define (threaten-new? a)
      (let ((ra (car a))
            (ca (cdr a))
            (rb (car new-queen))
            (cb k))
        (or (= ra rb)
            (= (- ca ra) (- cb rb))
            (= (+ ca ra) (+ cb rb)))))
    (not (ormap threaten-new? (cdr positions)))))

(define empty-board '())

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) 
           (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position 
                    new-row
                    k 
                    rest-of-queens))
                 (range 
                  1 
                  (add1 board-size))))
          (queen-cols (- k 1))))))
  (queen-cols board-size))
```

Exercise 2.43: Louis交换了嵌套mapping的顺序，导致程序很慢？为什么？

```scheme
(flatmap
 (lambda (new-row)
   (map (lambda (rest-of-queens)
          (adjoin-position 
           new-row k rest-of-queens))
        (queen-cols (- k 1))))
 (enumerate-interval 1 board-size))
```

因为`(queen-cols (-k 1))`在内层循环，所以执行了n次，相比前面只要执行一次。

对于8皇后，大概需要8^8倍的时间。其中S(i)是放i个皇后的方法数

```scheme
; original solution
; T(0)=1, T(k)=T(k-1) + S(k-1) * n
; T(n)=a[S(0)+S(1)+...+S(n-1)] * n

; Louis's solution
; t(0)=1, t(k)=[t(k-1) + S(k-1)] * n
; t(n)=(...((S(0)*n + S(1))*n + S(2))*n)...+S(n-1))*n
; =aS(0)n^n+...
```

### Example: A Picture Language

```scheme
(define (flipped-pairs painter)
  (let ((painter2 
         (beside painter 
                 (flip-vert painter))))
    (below painter2 painter2)))

(define wave4 (flipped-pairs wave))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter 
                                  (- n 1))))
        (beside painter 
                (below smaller smaller)))))

(define (corner-split painter n)
 (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter 
                                (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right 
                                   right))
              (corner (corner-split painter 
                                    (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right 
                         corner))))))

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) 
                        quarter)))
      (below (flip-vert half) half))))
```

Exercise 2.44: 定义up-split步骤，跟right-split类似，只是交换了below和beside的角色

```scheme
(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter 
                               (- n 1))))
        (below painter 
               (beside smaller smaller)))))
```



Higher-order operations

安排4个拷贝的抽象过程

```scheme
(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) 
                       (tr painter)))
          (bottom (beside (bl painter) 
                          (br painter))))
      (below bottom top))))

(define (flipped-pairs painter)
  (let ((combine4 
         (square-of-four identity 
                         flip-vert
                         identity 
                         flip-vert)))
    (combine4 painter)))

(define (square-limit painter n)
  (let ((combine4 
         (square-of-four flip-horiz 
                         identity
                         rotate180 
                         flip-vert)))
    (combine4 (corner-split painter n))))
```

Exercise 2.45: right-split和up-split可以表示为一个通用splitting操作的实例。定义一个split步骤

```scheme
(define right-split (split beside below))
(define up-split (split below beside))

(define (split op1 op2)
  (define (split-instance painter n)
    (if (= n 0)
        painter
        (let ((smaller (split-instance painter
                                       (- n 1))))
          (op1 painter
               (op2 smaller smaller)))))
  split-instance)
```



Frames

框架表示为3个矢量，一个origin矢量和两个edge矢量。我们使用一个单位正方形内的坐标来指定图像。每个框架关联一个框架坐标映射，用来平移和放缩图像。

`Origin(Frame) + x*Edge1(Frame) + y*Edge2(Frame)`

```scheme
(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect 
      (scale-vect (xcor-vect v)
                  (edge1-frame frame))
      (scale-vect (ycor-vect v)
                  (edge2-frame frame))))))
```

Exercise 2.46: 实现矢量的数据抽象

```scheme
(define (make-vect x y)
  (list x y))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (car (cdr v)))

(define (add-vect a b)
  (make-vector (+ (xcor-vect a)
                  (xcor-vect b))
               (+ (ycor-vect a)
                  (ycor-vect b))))

(define (sub-vect a b)
  (make-vector (- (xcor-vect a)
                  (xcor-vect b))
               (- (ycor-vect a)
                  (ycor-vect b))))

(define (scale-vect a s)
  (make-vector (* s (xcor-vect a))
               (* s (ycor-vect a))))
```

Exercise 2.47: 下面是两种可能的框架构造函数，分别写出合适的selectors

```scheme
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame f)
  (car f))

(define (edge1-frame f)
  (car (cdr f)))

(define (edge2-frame f)
  (car (cdr (cdr f))))

(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (origin-frame f)
  (car f))

(define (edge1-frame f)
  (car (cdr f)))

(define (edge2-frame f)
  (cdr (cdr f)))
```



Painters

一个painter表示为一个步骤，以一个frame为参数。

```scheme
(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame) 
         (start-segment segment))
        ((frame-coord-map frame) 
         (end-segment segment))))
     segment-list)))
```



Exercise 2.48: 使用2.46的矢量表示来定义线段的表示

```scheme
(define (make-segment start end)
  (cons start end))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (cdr s))
```

Exercise 2.49: 使用segments->painter定义以下基本painters

```scheme
(define outline
  (segments->painter
   (list
    (make-segment (make-vect 0 0) (make-vect 0 1))
    (make-segment (make-vect 0 1) (make-vect 1 1))
    (make-segment (make-vect 1 1) (make-vect 1 0))
    (make-segment (make-vect 1 0) (make-vect 0 0)))))

(define x-painter
  (segments->painter
   (list
    (make-segment (make-vect 0 0) (make-vect 1 1))
    (make-segment (make-vect 1 0) (make-vect 0 1)))))

(define diamond
  (segments->painter
   (list
    (make-segment (make-vect 0.5 0) (make-vect 0 0.5))
    (make-segment (make-vect 0.5 0) (make-vect 1 0.5))
    (make-segment (make-vect 0.5 1) (make-vect 0 0.5))
    (make-segment (make-vect 0.5 1) (make-vect 1 0.5)))))
```



Transforming and combining painters

在painter上的操作（例如flip-vert或beside）是这样工作的，它以参数frame推导出的新frames调用原始的painters创建一个新的painter。所以并不需要知道原来的painter是如何工作的，只需要知道如何转换框架。

```scheme
(define (transform-painter 
         painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter (make-frame new-origin
                  (sub-vect (m corner1) 
                            new-origin)
                  (sub-vect (m corner2)
                            new-origin)))))))
```

先来看看flip-vert怎么用transform-painter实现

```scheme
(define (flip-vert painter)
  (transform-painter
   painter
   (make-vect 0.0 1.0)
   (make-vect 1.0 1.0)
   (make-vect 0.0 0.0)))
```

将图片收缩到框架右上角的1/4

```scheme
(define (shrink-to-upper-right painter)
  (transform-painter painter
                     (make-vect 0.5 0.5)
                     (make-vect 1.0 0.5)
                     (make-vect 0.5 1.0)))
```

逆时针旋转90度

```scheme
(define (rotate90 painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))
```

组合操作也可以用transform-painter

```scheme
(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left  (transform-painter
                        painter1
                        (make-vect 0.0 0.0)
                        split-point
                        (make-vect 0.0 1.0)))
          (paint-right (transform-painter
                        painter2
                        split-point
                        (make-vect 1.0 0.0)
                        (make-vect 0.5 1.0))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))
```

Exercise 2.50: 定义flip-horiz水平翻转以及逆时针旋转180度和270度的变换

```scheme
(define (flip-horiz painter)
  (transform-painter
   painter
   (make-vect 1.0 0.0)
   (make-vect 0.0 0.0)
   (make-vect 1.0 1.0)))

(define (rotate180 painter)
  (transform-painter painter
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 0.0)))

(define (rotate270 painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))
```

Exercise 2.51: 定义below，第一个painter在下面。分别用两种方式实现，第一种类似beside，第二种先用beside再旋转。

```scheme
(define (below painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-bottom  (transform-painter
                          painter1
                          (make-vect 0.0 0.0)
                          (make-vect 1.0 0.0)
                          split-point))
          (paint-top (transform-painter
                      painter2
                      split-point
                      (make-vect 1.0 0.5)
                      (make-vect 1.0 1.0))))
      (lambda (frame)
        (paint-bottm frame)
        (paint-top frame)))))
```



Levels of language for robust design

语言和程序设计的的一个关键思想：stratified design分层设计。一个复杂系统应该构建为一个分层序列，而这些层由一个语音的序列描述。每一层的构建都是通过组合在该层被认为是原语的部分。



## 2.3 Symbolic Data

### Quotation

### Example: Symbolic Differentiation

The differentiation program with abstract data

我们只考虑两个操作数的加法和乘法，假设我们已经有了变量加法乘法的原语。

```scheme
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product 
           (multiplier exp)
           (deriv (multiplicand exp) var))
          (make-product 
           (deriv (multiplier exp) var)
           (multiplicand exp))))
        (else (error "unknown expression 
                      type: DERIV" exp))))
```



Representing algebraic expressions

一个特别直接的选择是使用List用于组合的括号前缀表示法。例如ax+b表示为`(+ (* a x) b)`

```scheme
(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))

(define (make-sum a1 a2) (list '+ a1 a2))
(define (make-product m1 m2) (list '* m1 m2))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s) (caddr s))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p) (caddr p))
```

上面的程序可以产生正确的结果，但是没有化简

```scheme
(deriv '(+ x 3) 'x)
(+ 1 0)

(deriv '(* x y) 'x)
(+ (* x 0) (* 1 y))

(deriv '(* (* x y) (+ x 3)) 'x)
(+ (* (* x y) (+ 1 0))
   (* (+ (* x 0) (* 1 y))
      (+  x 3)))
```

我们不用修改deriv而是修改它的原语

```scheme
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) 
         (+ a1 a2))
        (else (list '+ a1 a2))))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-product m1 m2)
  (cond ((=number? m1 0) 0)
        ((=number? m2 0) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2))
         (* m1 m2))
        (else (list '* m1 m2))))
```

现在结果好很多了，但是第三个表达式还不是最简的。代数简化问题是复杂的，其中之一是因为某种形式在一个目的下是最简的，但对另一个目的则不是。

```scheme
(deriv '(+ x 3) 'x)
1

(deriv '(* x y) 'x)
y

(deriv '(* (* x y) (+ x 3)) 'x)
(+ (* x y) (* y (+ x 3)))
```

Exercise 2.56: 扩展我们的微分器，支持指数

```scheme
(define (make-exponentiation b e)  ; e is number
  (cond
   ((=number? e 0) 1)
   ((=number? e 1) b)
   (list '** b e)))

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))

(define (base x) (cadr x))
(define (exponent x) (caddr x))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product 
           (multiplier exp)
           (deriv (multiplicand exp) var))
          (make-product 
           (deriv (multiplier exp) var)
           (multiplicand exp))))
        ((exponentiation? exp)
         (make-product
          (exponent exp)
          (make-exponentiation
           (base exp)
           (- (exponent exp) 1))))
        (else (error "unknown expression 
                      type: DERIV" exp))))
```

Exericse 2.57: 扩展微分程序支持任意个数参数的加法和乘法。尝试不该deriv，例如addend取第一项，augend取剩余所有项的和

```scheme
(define (make-sum-list l) 
  (if (= (length l) 2)
      (list '+ (car l) (cadr l)) 
      (make-sum (car l) (make-sum-list (cdr l))))) 
 
(define (addend s) (cadr s))

(define (augend s)
  (let ((rest (cddr s)))
    (cond
     ((= (length rest) 1) (car rest))
     (else (make-sum-list rest)))))

(define (make-product-list l) 
  (if (= (length l) 2) 
      (list '* (car l) (cadr l)) 
      (make-product (car l) (make-product-list (cdr l))))) 

(define (multiplier p) (cadr p))

(define (multiplicand p)
  (let ((rest (cddr p)))
    (cond
     ((= (length rest) 1) (car rest))
     (else (make-product-list rest)))))
```

Exercise 2.58: 假设我们想修改微分程序为一般的数学表示法，即+和*在中间。由于我们是用的抽象数据，我们只需要修改构造和选择函数和谓词。

```scheme
; 简化问题，假设只有两个参数，且括号都是完全的
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) 
         (+ a1 a2))
        (else (list a1 '+ a2))))

(define (make-product m1 m2)
  (cond ((=number? m1 0) 0)
        ((=number? m2 0) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2))
         (* m1 m2))
        (else (list m1 '* m2))))

(define (sum? x)
  (and (pair? x) (eq? (cadr x) '+)))

(define (addend s) (car s))

(define (augend s) (caddr s))

(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))

(define (multiplier p) (car p))

(define (multiplicand p) (caddr p))

(deriv '(x + (3 * (x + (y + 2)))) 'x)

; 如果有运算符优先级，即可以省略一些不必要的括号，问题就变得相当困难了
; 如果是加法，且后面还有运算符，后面结合为一个新的运算
; 如果是乘法，且后面还有运算符，前两个先结合为一个乘法
; 这种方法很麻烦，没发解决先乘后加的问题
; 先统一修改表达式，补充括号
; 长度3以内
; 如果是加法，后面的结合
; 如果是乘法，一直往后结合，直到遇到加法

(define (combine-from-3 exp)
  (list (car exp)
        (cadr exp)
        (modify-exp (cddr exp))))

(define (combine-first-3 exp)
  (modify-exp
   (cons
    (list (car exp)
           (cadr exp)
           (caddr exp))
    (cdddr exp))))

(define (modify-sum exp)
  (let ((len (length exp)))
    (cond
     ((= len 3) exp)
     (else (combine-from-3 exp)))))

(define (modify-product exp)
  (let ((len (length exp)))
    (cond
     ((= len 3) exp)
     (else (combine-first-3 exp)))))

(module+ test
  (require rackunit)
  (check-equal? (modify-exp 1) 1)
  (check-equal? (modify-exp 'x) 'x)
  (check-equal? (modify-exp '(1 + 2)) '(1 + 2))
  (check-equal? (modify-exp '(1 * 2)) '(1 * 2))
  (check-equal? (modify-exp '(1 + 2 + 3)) '(1 + (2 + 3)))
  (check-equal? (modify-exp '(1 + 2 * 3)) '(1 + (2 * 3)))
  (check-equal? (modify-exp '(1 + 2 + 3 * 4)) '(1 + (2 + (3 * 4))))
  (check-equal? (modify-exp '(1 + 2 * 3 + 4)) '(1 + ((2 * 3) + 4)))
  (check-equal? (modify-exp '(1 * 2 + 3)) '((1 * 2) + 3))
  (check-equal? (modify-exp '(1 * 2 * 3)) '((1 * 2) * 3))
  (check-equal? (modify-exp '(1 * 2 + 3 + 4)) '((1 * 2) + (3 + 4)))
  (check-equal? (modify-exp '(1 * 2 + 3 * 4)) '((1 * 2) + (3 * 4)))
  (check-equal? (modify-exp '(1 * 2 * 3 + 4)) '(((1 * 2) * 3) + 4))
  (check-equal? (modify-exp '(1 * 2 * 3 * 4)) '(((1 * 2) * 3) * 4))
  )

(define (modify-exp exp)
  (cond
   ((number? exp) exp)
   ((variable? exp) exp)
   ((sum? exp) (modify-sum exp))
   ((product? exp) (modify-product exp))
   (else (error "unknown expression type: " exp))))
  
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product 
           (multiplier exp)
           (deriv (multiplicand exp) var))
          (make-product 
           (deriv (multiplier exp) var)
           (multiplicand exp))))
        (else (error "unknown expression 
                      type: DERIV" exp))))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) 
         (+ a1 a2))
        (else (list a1 '+ a2))))

(define (make-product m1 m2)
  (cond ((=number? m1 0) 0)
        ((=number? m2 0) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2))
         (* m1 m2))
        (else (list m1 '* m2))))

(define (sum? x)
  (and (pair? x) (eq? (cadr x) '+)))

(define (addend s) (car s))

(define (augend s) (caddr s))

(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))

(define (multiplier p) (car p))

(define (multiplicand p) (caddr p))

(deriv (modify-exp '(x + 3 * (x + y + 2))) 'x)
```

把指数也加进来

modify-exp中加法和乘法的优先级处理是硬编码的，并不是通用的，例如把指数加进来之后，原来乘法的处理就有问题了。更一般的做法是，判断当前运算符和下一个的优先级关系，那个优先则结合哪个。

```scheme
(define (precedence op)
  (cond
    ((equal? op '+) 0)
    ((equal? op '*) 1)
    ((equal? op '**) 2)
    (else (error "unknown operator " op))))

(module+ test
  (require rackunit)
  (check-equal? (higher-precedence? '(1 + 2 * 3)) #f)
  (check-equal? (higher-precedence? '(1 * 2 + 3)) #t)
  )
(define (higher-precedence? exp)
  (if (< (length exp) 5)
      (error "invalid expression: " exp)
      (>= (precedence (cadr exp))
          (precedence (cadddr exp)))))

(define (combine-from-3 exp)
  (list (car exp)
        (cadr exp)
        (modify-exp (cddr exp))))

(define (combine-first-3 exp)
  (modify-exp
   (cons
    (list (car exp)
          (cadr exp)
          (caddr exp))
    (cdddr exp))))

(module+ test
  (require rackunit)
  (check-equal? (modify-exp 1) 1)
  (check-equal? (modify-exp 'x) 'x)
  (check-equal? (modify-exp '(1 + 2)) '(1 + 2))
  (check-equal? (modify-exp '(1 * 2)) '(1 * 2))
  (check-equal? (modify-exp '(1 + 2 + 3)) '((1 + 2) + 3))
  (check-equal? (modify-exp '(1 + 2 * 3)) '(1 + (2 * 3)))
  (check-equal? (modify-exp '(1 + 2 + 3 * 4)) '((1 + 2) + (3 * 4)))
  (check-equal? (modify-exp '(1 + 2 * 3 + 4)) '(1 + ((2 * 3) + 4)))
  (check-equal? (modify-exp '(1 * 2 + 3)) '((1 * 2) + 3))
  (check-equal? (modify-exp '(1 * 2 * 3)) '((1 * 2) * 3))
  (check-equal? (modify-exp '(1 * 2 + 3 + 4)) '(((1 * 2) + 3) + 4))
  (check-equal? (modify-exp '(1 * 2 + 3 * 4)) '((1 * 2) + (3 * 4)))
  (check-equal? (modify-exp '(1 * 2 * 3 + 4)) '(((1 * 2) * 3) + 4))
  (check-equal? (modify-exp '(1 * 2 * 3 * 4)) '(((1 * 2) * 3) * 4))
  (check-equal? (modify-exp '(1 + 2 * 3 ** 4)) '(1 + (2 * (3 ** 4))))
  (check-equal? (modify-exp '(1 ** 2 * 3 + 4)) '(((1 ** 2) * 3) + 4))
  (check-equal? (modify-exp '(1 + 2 ** 3 * 4)) '(1 + ((2 ** 3) * 4)))
  (check-equal? (modify-exp '(1 * 2 + 3 ** 4)) '((1 * 2) + (3 ** 4)))
  )

(define (modify-exp exp)
  (cond
    ((number? exp) exp)
    ((variable? exp) exp)
    ((= (length exp) 3) exp)
    ((higher-precedence? exp) (combine-first-3 exp))
    (else (combine-from-3 exp))))
  
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product 
           (multiplier exp)
           (deriv (multiplicand exp) var))
          (make-product 
           (deriv (multiplier exp) var)
           (multiplicand exp))))
        ((exponentiation? exp)
         (make-product
          (exponent exp)
          (make-exponentiation
           (base exp)
           (- (exponent exp) 1))))
        (else (error "unknown expression 
                      type: DERIV" exp))))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) 
         (+ a1 a2))
        (else (list a1 '+ a2))))

(define (make-product m1 m2)
  (cond ((=number? m1 0) 0)
        ((=number? m2 0) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2))
         (* m1 m2))
        (else (list m1 '* m2))))

(define (sum? x)
  (and (pair? x) (eq? (cadr x) '+)))

(define (addend s) (car s))

(define (augend s) (caddr s))

(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))

(define (multiplier p) (car p))

(define (multiplicand p) (caddr p))

(define (make-exponentiation b e)  ; e is number
  (cond
    ((=number? e 0) 1)
    ((=number? e 1) b)
    (list b '** e)))

(define (exponentiation? x)
  (and (pair? x) (eq? (cadr x) '**)))

(define (base x) (car x))
(define (exponent x) (caddr x))
```

### Example: Representing Sets

Sets as unordered lists

```scheme
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) 
         '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) 
                                 set2)))
        (else (intersection-set (cdr set1) 
                                set2))))
```

选择和插入都是O(n)，交集和并集是O(n^2)

Exercise 2.59: 实现union-set

```scheme
(define (union-set set1 set2)
  (define (iter set1)
    (cond
     ((null? set1) set2)
     ((element-of-set? (car set1) set2)
      (union-set (cdr set1) set2))
     (else (cons (car set1)
                 (union-set (cdr set1) set2)))))
  (cond
   ((null? set2) set1)
   (else (iter set1)))
```

Exercise 2.60: 现在假设集合中允许重复元素，再次设计上面4个函数。它们的效率如何变化？是否有应用会优先使用这种可重复的表现形式？

```scheme
; element-of-set? 不变

(define (adjoin-set x set)
  (cons x set))

; intersection-set 不变

(define (union-set set1 set2)
  (append set1 set2))
```

适合插入和并集占主导的场合（不太可能？🤔）。



Sets as ordered lists

```scheme
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1 (intersection-set 
                         (cdr set1)
                         (cdr set2))))
              ((< x1 x2) (intersection-set 
                          (cdr set1) 
                          set2))
              ((< x2 x1) (intersection-set 
                          set1 
                          (cdr set2)))))))
```

交集变成O(n)了

Exercise 2.61: 使用有序表现实现adjoin-set，类比element-of-set?如何利用有序的优点生成平均只需要一半步数的步骤

```scheme
(define (adjoin-set x set)
  (cond
   ((null? set) (cons x set))
   ((= x (car set)) set)
   ((< x (car set)) (cons x set))
   (else (cons (car set)
               (adjoin-set x (cdr set))))))
```

Exercise 2.62: 使用有序表示给出union-set的一个O(n)实现。

```scheme
(define (union-set set1 set2)
  (cond
   ((null? set1) set2)
   ((null? set2) set1)
   (else
    (let ((x1 (car set1))
          (x2 (car set2)))
      (cond
       ((= x1 x2)
        (cons x1 (union-set (cdr set1) (cdr set2))))
       ((< x1 x2)
        (cons x1 (union-set (cdr set1) set2)))
       (else
        (cons x2 (union-set set1 (cdr set2)))))))))
```



Sets as binary tress

我们可以用lists来表现树，每个节点有3项，本节点的条目，左子树和右子树。

```scheme
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? 
          x 
          (left-branch set)))
        ((> x (entry set))
         (element-of-set? 
          x 
          (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree 
          (entry set)
          (adjoin-set x (left-branch set))
          (right-branch set)))
        ((> x (entry set))
         (make-tree
          (entry set)
          (left-branch set)
          (adjoin-set x (right-branch set))))))
```

Exercise 2.63: 两种tree转list的方法，结果是否一样，复杂度呢？

```scheme
(module+ test
  (require rackunit)
  (check-equal? (tree->list-1 t1) resultlist)
  (check-equal? (tree->list-1 t2) resultlist)
  (check-equal? (tree->list-1 t3) resultlist) 
  )
(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append 
       (tree->list-1 
        (left-branch tree))
       (cons (entry tree)
             (tree->list-1 
              (right-branch tree))))))

(module+ test
  (require rackunit)
  (check-equal? (tree->list-2 t1) resultlist)
  (check-equal? (tree->list-2 t2) resultlist)
  (check-equal? (tree->list-2 t3) resultlist) 
  )
(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list 
         (left-branch tree)
         (cons (entry tree)
               (copy-to-list 
                (right-branch tree)
                result-list)))))
  (copy-to-list tree '()))

(define t1 (make-tree 7 (make-tree 3 (make-tree 1 '() '()) (make-tree 5 '() '()))
                        (make-tree 9 '() (make-tree 11 '() '()))))
(define t2 (make-tree 3 (make-tree 1 '() '()) (make-tree 7 (make-tree 5 '() '()) (make-tree 9 '() (make-tree 11 '() '())))))
(define t3 (make-tree 5 (make-tree 3 (make-tree 1 '() '()) '()) (make-tree 9 (make-tree 7 '() '()) (make-tree 11 '() '()))))
(define resultlist '(1 3 5 7 9 11))
```

结果一样，复杂度第一种高，因为append是O(n)的，总和起来就是O(nlgn)。`T(n)=2(T/2)+O(n)`

而第二种是O(n)，`T(n)=2*T(n/2)+O(1)`

把例子全展开之后开一下，也可以看出端倪。



Sets and information retrieval

我们将数据表现为一个纪录的集合，lookup查找纪录

```scheme
(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
        ((equal? given-key 
                 (key (car set-of-records)))
         (car set-of-records))
        (else 
         (lookup given-key 
                 (cdr set-of-records)))))
```

Exercise 2.66: Example: 纪录集表示为一个二叉树时，实现lookup

```scheme
(define (lookup key set)
  (cond
   ((null? set) #false)
   (else
    (let ((record (car set)))
      (cond
       ((= key (entry record)) record)
       ((< key (entry record)) (lookup key (left-branch record)))
       (else (lookup key (right-branch record))))))))
```



### Exercise: Huffman Encoding Trees

将数据表现为01的序列，像ASCII码那样的是固定长度码。有时候使用可变长度码更有优势，例如摩斯码。

使用可变长度码的一个难点是如何知道已经到达了一个符号的末尾。摩斯码是通过使用一个特殊的间隔码（暂停）来解决这个问题的。

另一种解决方式是将code设计成任何符号的完整码都不是另一个符号码的开始（或前缀）。这种码被叫做前缀码。

Huffman编码方式就是这样一种可变长度前缀码。左子树表示0，右子树表示1。



Generating Huffman trees

给定一组符号以及它们的相对频率，如何构建一个最优码。

```scheme
Initial {(A 8) (B 3) (C 1) (D 1) 
leaves   (E 1) (F 1) (G 1) (H 1)}

Merge   {(A 8) (B 3) ({C D} 2) 
         (E 1) (F 1) (G 1) (H 1)}

Merge   {(A 8) (B 3) ({C D} 2) 
         ({E F} 2) (G 1) (H 1)}

Merge   {(A 8) (B 3) ({C D} 2) 
         ({E F} 2) ({G H} 2)}

Merge   {(A 8) (B 3) ({C D} 2) 
         ({E F G H} 4)}

Merge   {(A 8) ({B C D} 5)
         ({E F G H} 4)}

Merge   {(A 8) ({B C D E F G H} 9)}

Final   {({A B C D E F G H} 17)}
merge 
```



Representing Huffman trees

```scheme
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) 
                (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (cond
   ((null? tree) '())
   ((leaf? tree) (list (symbol-leaf tree)))
   (else (caddr tree))))

(define (weight tree)
  (cond
   ((null? tree) 0)
   ((leaf? tree) (weight-leaf tree))
   (else (cadddr tree))))
```

symbols和weight可以作为**generic procedures**的简单例子（可以处理不止一种数据的步骤）



The decoding procedure

```scheme
(define (decode bits tree)
  (define (decode-1 bits current-branch)

    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch 
                (car bits) 
                current-branch)))
          (if (leaf? next-branch)
              (cons 
               (symbol-leaf next-branch)
               (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) 
                        next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit: 
               CHOOSE-BRANCH" bit))))
```



Sets of weighted elements

因为前面讨论的树生成算法需要处理叶子和树的集合，且需要重复地找到权重最小的项，所以我们将其表示为权重增序的list

```scheme
(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) 
         (cons x set))
        (else 
         (cons (car set)
               (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set 
         (make-leaf (car pair)    ; symbol
                    (cadr pair))  ; frequency
         (make-leaf-set (cdr pairs))))))
```

Exercise 2.67: 定义一个编码树和实例消息，使用decode解码消息，给出结果

```scheme
(define sample-tree
  (make-code-tree 
   (make-leaf 'A 4)
   (make-code-tree
    (make-leaf 'B 2)
    (make-code-tree 
     (make-leaf 'D 1)
     (make-leaf 'C 1)))))

(define sample-message 
  '(0 1 1 0 0 1 0 1 0 1 1 1 0))

; '(A D A B B C A)
```

Exercise 2.68: encode步骤接收一个message和一个tree参数，生成bits list。实现encode-symbol，注意如果符号不在树中，则出错。使用上一个例子sample-tree测试

```scheme
(define (encode message tree)
  (if (null? message)
      '()
      (append 
       (encode-symbol (car message) 
                      tree)
       (encode (cdr message) tree))))

(define (encode-symbol sym tree)
  (define (iter tree bits)
    (cond
     ((leaf? tree) bits)
     ((memq sym (symbols (left-branch tree)))
      (iter (left-branch tree) (append bits '(0))))
     (else
      (iter (right-branch tree) (append bits '(1))))))
  (cond
   ((null? tree) (error "empty tree"))
   ((memq sym (symbols tree)) (iter tree '()))
   (else (error "unknown symbol: " sym))))
```

Exercise 2.69: 下面步骤接收一个符号-频率对的list，生成一个Huffman编码树

```scheme
(module+ test
  (require rackunit)
  (check-equal? (generate-huffman-tree '((A 4) (B 2) (C 1) (D 1)))
                sample-tree)
  (check-equal? (generate-huffman-tree '((A 4))) (make-code-tree '(leaf A 4) '()))
  (check-equal? (generate-huffman-tree '((A 4) (B 2))) (make-code-tree '(leaf B 2) '(leaf A 4)))
  )
(define (generate-huffman-tree pairs)
  (successive-merge
   (make-leaf-set pairs)))

(define (successive-merge l)
  (define (iter l)
    (let ((len (length l))
          (newitem (make-code-tree (car l) (cadr l))))
      (cond
       ((= len 2) newitem)
       (else (iter (adjoin-set newitem (cddr l)))))))
  (cond
   ((not (pair? l)) (error "not a list"))
   ((null? l) (error "empty list"))
   ((= 1 (length l)) (make-code-tree (car l) '()))
   (else (iter l)))) 
```

Exercise 2.70: 使用generate-huffman-tree生成一个对应的Huffman树，然后使用encode编码。需要多少比特？如果使用8符号的固定长度码需要多少bit？

```scheme
;A    2    NA  16
;BOOM 1    SHA  3
;GET  2    YIP  9
;JOB  2    WAH  1

(define rock-song-tree
  (generate-huffman-tree '((a 2) (boom 1) (Get 2) (job 2) (na 16) (Sha 3) (yip 9) (Wah 1))))

(define lyrics '(Get a job Sha na na na na na na na na Get a job Sha na na na na na na na na Wah yip yip yip yip yip yip yip yip yip Sha boom))
```

84比特。108比特。

Exercise 2.71: 假设我们有n个符号的一个Huffman树，它们的相对频率是1,2,4,…,2^n-1。素描n=5和n=10的情况。最高频符号需要多少比特，最低频符号呢？

n=5最高1比特，最低4比特。n=10最高1比特，最低9比特。

最高1比特，最低n-1比特。

Exercise 2.72: 考虑2.68中的编码过程。编码一个符号所需步骤的增长情况是？一般性地回答这个问题是困难的。考虑2.71的特殊情况，给出编码最高频率和最低频率符号所需的步骤数的增长order。

搜索O(n)，深度看树是否平衡。

## 2.4 Multiple Representations for Abstract Data

一个数据对象可能有不止一种有用的表示。数据表示的选择可能会改变。所以除了数据抽象屏障将数据的表示和使用隔离开，还需要抽象屏障来隔离不同的设计选择。另外，还需要允许增量地包含模块进更大的系统，而不需要重新设计或重新实现这些模块。

本节将学习如何处理那些在程序的不同部分可能表示为不同方式的数据。这需要**generic procedures**，构建元过程的主要技术是数据对象包含类型标签，显示指示如何处理数据对象。我们还会讨论data-directed编程。

### Reprensentations  for Complex Numbers

加法运算时表示为实部虚部方便，而乘法操作时表示为极坐标形式更方便。

```scheme
(define (add-complex z1 z2)
  (make-from-real-imag 
   (+ (real-part z1) (real-part z2))
   (+ (imag-part z1) (imag-part z2))))

(define (sub-complex z1 z2)
  (make-from-real-imag 
   (- (real-part z1) (real-part z2))
   (- (imag-part z1) (imag-part z2))))

(define (mul-complex z1 z2)
  (make-from-mag-ang 
   (* (magnitude z1) (magnitude z2))
   (+ (angle z1) (angle z2))))

(define (div-complex z1 z2)
  (make-from-mag-ang 
   (/ (magnitude z1) (magnitude z2))
   (- (angle z1) (angle z2))))
```

底层表示为直角坐标形式

```scheme
(define (real-part z) (car z))
(define (imag-part z) (cdr z))

(define (magnitude z)
  (sqrt (+ (square (real-part z)) 
           (square (imag-part z)))))

(define (angle z)
  (atan (imag-part z) (real-part z)))

(define (make-from-real-imag x y) 
  (cons x y))

(define (make-from-mag-ang r a)
  (cons (* r (cos a)) (* r (sin a))))
```

底层表示为极坐标形式

```scheme
(define (real-part z)
  (* (magnitude z) (cos (angle z))))

(define (imag-part z)
  (* (magnitude z) (sin (angle z))))

(define (magnitude z) (car z))
(define (angle z) (cdr z))

(define (make-from-real-imag x y)
  (cons (sqrt (+ (square x) (square y)))
        (atan y x)))

(define (make-from-mag-ang r a) 
  (cons r a))
```

### Tagged data

一种看待数据抽象的方式是作为“最小共识原则”的一种应用。如意我们想在设计选择和构造函数之后还维持表示的模糊性，可以使用类型tag。

```scheme
(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum: 
              TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum: 
              CONTENTS" datum)))

(define (rectangular? z)
  (eq? (type-tag z) 'rectangular))

(define (polar? z)
  (eq? (type-tag z) 'polar))

; 然后就可以同时支持两种表示形式
; rectangular
(define (real-part-rectangular z) (car z))
(define (imag-part-rectangular z) (cdr z))

(define (magnitude-rectangular z)
  (sqrt (+ (square (real-part-rectangular z))
           (square (imag-part-rectangular z)))))

(define (angle-rectangular z)
  (atan (imag-part-rectangular z)
        (real-part-rectangular z)))

(define (make-from-real-imag-rectangular x y)
  (attach-tag 'rectangular (cons x y)))

(define (make-from-mag-ang-rectangular r a)
  (attach-tag 
   'rectangular
   (cons (* r (cos a)) (* r (sin a)))))

; polar
(define (real-part-polar z)
  (* (magnitude-polar z) 
     (cos (angle-polar z))))

(define (imag-part-polar z)
  (* (magnitude-polar z) 
     (sin (angle-polar z))))

(define (magnitude-polar z) (car z))
(define (angle-polar z) (cdr z))

(define (make-from-real-imag-polar x y)
  (attach-tag 
   'polar
   (cons (sqrt (+ (square x) (square y)))
         (atan y x))))

(define (make-from-mag-ang-polar r a)
  (attach-tag 'polar (cons r a)))”

; generic selectors
(define (real-part z)
  (cond ((rectangular? z)
         (real-part-rectangular (contents z)))
        ((polar? z)
         (real-part-polar (contents z)))
        (else (error "Unknown type: 
               REAL-PART" z))))

(define (imag-part z)
  (cond ((rectangular? z)
         (imag-part-rectangular (contents z)))
        ((polar? z)
         (imag-part-polar (contents z)))
        (else (error "Unknown type: 
               IMAG-PART" z))))

(define (magnitude z)
  (cond ((rectangular? z)
         (magnitude-rectangular (contents z)))
        ((polar? z)
         (magnitude-polar (contents z)))
        (else (error "Unknown type: 
               MAGNITUDE" z))))

(define (angle z)
  (cond ((rectangular? z)
         (angle-rectangular (contents z)))
        ((polar? z)
         (angle-polar (contents z)))
        (else (error "Unknown type: 
               ANGLE" z))))

; arithmetic operations
(define (add-complex z1 z2)
  (make-from-real-imag 
   (+ (real-part z1) (real-part z2))
   (+ (imag-part z1) (imag-part z2))))

(define (sub-complex z1 z2)
  (make-from-real-imag 
   (- (real-part z1) (real-part z2))
   (- (imag-part z1) (imag-part z2))))

(define (mul-complex z1 z2)
  (make-from-mag-ang 
   (* (magnitude z1) (magnitude z2))
   (+ (angle z1) (angle z2))))

(define (div-complex z1 z2)
  (make-from-mag-ang 
   (/ (magnitude z1) (magnitude z2))
   (- (angle z1) (angle z2))))

(define (make-from-real-imag x y)
  (make-from-real-imag-rectangular x y))

(define (make-from-mag-ang r a)
  (make-from-mag-ang-polar r a))
```

### Data-Directed Programming and Additivity

这种检查数据类型并调用合适步骤的一般性策略叫做**dispatching on type**。这是在系统设计中获得模块化的一个强大的策略。但是像上一节那样实现分派有两个重要的弱点。一个是通用接口过程必须知道所有不同的表现。另一个是即使单个表现可以单独设计码，我们必须保证在整个系统中任何两个过程都不重名。

这两个弱点的潜在问题都是实现通用接口的技术不是additive增量的，每次有一个新的表现，都必须修改这些通用接口过程。我们需要进一步的模块化系统设计，这由叫做**data-directed programming**数据导向编程的编程技术提供。这种设计程序的技术直接跟operation-and-type工作。假设我们有两个操作operation-and-type表的过程put和get。

- `(put <op> <type> <item>)`将item安装到表中
- `(get <op> <type>)`根据操作和类型查找，返回找到的item，否则返回false

现在暂且假设这两个函数已经实现。下面是如何将数据导向编程用在复数系统中。直角坐标形式表现的实现跟之前一样，定义一个过程的集合，或叫package，通过将条目加到表中将这些过程接到系统的其他部分。

```scheme
(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) 
    (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  ;; interface to the rest of the system
  (define (tag x) 
    (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part) ; 使用'(rectangular)而非'rectangular，允许多个参数操作的可能性，不是所有参数都是相同类型
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) 
         (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) 
         (tag (make-from-mag-ang r a))))
  'done)

; 极坐标的实现也是类似
(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) 
         (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) 
         (tag (make-from-mag-ang r a))))
  'done)
```

选择器通过一个一般过程apply-generic访问表

```scheme
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
            "No method for these types: 
             APPLY-GENERIC"
            (list op type-tags))))))

(define (real-part z) 
  (apply-generic 'real-part z))
(define (imag-part z) 
  (apply-generic 'imag-part z))
(define (magnitude z) 
  (apply-generic 'magnitude z))
(define (angle z) 
  (apply-generic 'angle z))
```

构造函数同样从表中抽取

```scheme
(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 
        'rectangular) 
   x y))

(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 
        'polar) 
   r a)) 
```

Exercise 2.73: 2.3.2中执行符号微分的程序，我们也可以认为是根据表达式的类型执行分派。这种情况下类型tag是运算符，执行的操作是deriv。我们可以把这个程序转换成数据导向风格

```scheme
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) 
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product 
            (multiplier exp)
            (deriv (multiplicand exp) var))
           (make-product 
            (deriv (multiplier exp) var)
            (multiplicand exp))))
        ⟨more rules can be added here⟩
        (else (error "unknown expression type:
                      DERIV" exp))))
; data-directed style
(define (deriv exp var)
   (cond ((number? exp) 0)
         ((variable? exp) 
           (if (same-variable? exp var) 
               1 
               0))
         (else ((get 'deriv (operator exp)) 
                (operands exp) 
                var))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
```

1. 解释上面干了什么。为什么我们不能将numbers?和variable?也吸收进数据导向分派中？

   get函数从table中获取操作符对应的求导函数。因为这两种基本形式的格式不一样。

2. 写加法和乘法的微分步骤以及安装的辅助代码

   ```scheme
   (define (install-sum-package)
     ;; internal procedures
     (define (deriv-sum operands var)
       (apply (get 'make '+)
        (map (lambda (opd)
               (deriv opd var)) ; global deriv
             operands)))
     (define (make-sum a . l)
       (define (remove-zeros l)
         (cond
          ((null? l) '())
          ((=number? (car l) 0) (remove-zeros (cdr l)))
          (else (cons (car l) (remove-zeros (cdr l))))))
       (let ((l (remove-zeros (cons a l))))
         (cond
          ((andmap number? l) (foldr + 0 l))
          ((= 1 (length l)) (car l))
          (else (cons '+ l)))))
     ;; interface to the rest of the system
     (put 'deriv '+ deriv-sum)
     (put 'make '+ make-sum)
     'done)
   
   (define (install-product-package)
     ;; internal procedures
     (define (multiplier opds) (car opds))
     (define (multiplicand opds)
       (let ((rest (cdr opds)))
         (cond
          ((= (length rest) 1) (car rest))
          (else (make-product rest)))))
     (define (deriv-product operands var)
       ((get 'make '+)
        ((get 'make '*)
         (multiplier operands)
         (deriv (multiplicand operands) var))
        ((get 'make '*)
         (deriv (multiplier operands) var)
         (multiplicand operands))))
     (define (make-product a . l)
       (define (remove-ones l)
         (cond
          ((null? l) '())
          ((=number? (car l) 1) (remove-ones (cdr l)))
          (else (cons (car l) (remove-ones (cdr l))))))
       (define (=0? v)
         (=number? v 0))
       (let ((l (remove-ones (cons a l))))
         (cond
          ((ormap =0? l) 0)
          ((andmap number? l) (foldr * 1 l))
          ((= 1 (length l)) (car l))
          (else (cons '* l)))))
     ;; interface to the rest of the system
     (put 'deriv '* deriv-product)
     (put 'make '* make-product)
     'done)
   ```

   辅助及测试代码，get/put简单实现了一下用于测试

   ```scheme
   (define lof '())
   (define (put op tag func)
     (set! lof (cons (list op tag func) lof)))
   (define (get op tag)
     (define (iter l)
       (cond
         ((null? l) (error "operation" op "for type" tag "not found!"))
         ((and (equal? (car (car l)) op)
               (equal? (cadr (car l)) tag))
          (caddr (car l)))
         (else (iter (cdr l)))))
     (iter lof))
   
   (define (variable? x) (symbol? x))
   (define (same-variable? v1 v2)
     (and (variable? v1)
          (variable? v2)
          (eq? v1 v2)))
   (define (=number? exp num)
     (and (number? exp) (= exp num)))
   
   (define (init)
     (install-sum-package)
     (install-product-package)
     (install-exponent-package)
     )
   
   (init)
   
   (deriv '(+ x 3) 'x) ; 1
   (deriv '(* x y) 'x) ; 'y
   (deriv '(* (* x y) (+ x 3)) 'x) ; '(+ (* x y) (* y (+ x 3)))
   (deriv '(** x 4) 'x) ; '(* 4 (** x 3))
   (deriv '(+ (* y (** x 3)) (* x y)) 'x) ;'(+ (* y (* 3 (** x 2))) y)
   ```

   

3. 选择任何额外的微分规则，比如指数，加入这个数据导向系统中

   ```scheme
   (define (install-exponent-package)
     ;; internal procedures
     (define (base opds) (car opds))
     (define (exponent opds) (cadr opds))
     (define (deriv-exponent operands var)
       ((get 'make '*)
        (exponent operands)
        ((get 'make '**)
         (base operands)
         (- (exponent operands) 1))))
     (define (make-exponent b e)
       (cond
        ((=number? e 0) 1)
        ((=number? e 1) b)
        (else (list '** b e))))
     ;; interface to the rest of the system
     (put 'deriv '** deriv-exponent)
     (put 'make '** make-exponent)
     'done)
   ```

   

4. 假设deriv中的分派行如下，有哪些需要修改的？

   ```scheme
   ((get (operator exp) 'deriv) 
    (operands exp) var)
   ```

   所以get和put调用的地方都需要修改。

Exercise 2.74: 各个分部实现了不同的数据结构体。雇员的信息是一个以名字为key的集合，各个分部的集合结构各不相同，每个纪录本身也是一个set（以id为键），各部门也不相同。

```scheme
; 1. 为总部实现一个get-record步骤，从特定文件中检索特定雇员纪录。每个部门的文件需要怎么构建？尤其是必须提供什么信息？必须提供tag信息，表明它是哪个分部的。
(define (get-record name file)
  (let* ((tag (division file))
         (cont (content file))
         (record ((get 'get-record tag) name cont)))
    (if record
        (attach-tag tag record)
        #f)))

(define (attach-tag division-tag content)
  (cons division-tag content))

(define (division x) (car x))
(define (content x) (cdr x))

; 2. 实现get-salary从任意分部获取的雇员纪录中获取工资信息
(define (get-salary record)
  (let ((tag (division record))
        (cont (content record)))
    ((get 'get-salary tag) cont)))

; 3. 实现find-employee-record，搜索所有分部文件查找指定雇员，返回其记录
; 假设该过程接收两个参数雇员名字及所有分部文件的list
(define (find-employee-record name lof)
  (define (iter l)
    (cond
     ((null? l) #f)
     (else
      (let* ((file (car lof))
             (record (get-record name file)))
        (if record
            record
            (iter (cdr l)))))))
  (iter lof))

; 4. 新合并一个公司，需要做什么改动以合并新雇员信息到中心系统
; 新公司提供一个安装包，包括get-record和get-salary等方法实现
```



**Message passing**

我们前面是按行分解operation-and-type表，每个操作关心自己的分派。

另一种实现方式是按列分解表，不再使用根据数据类型分派的智能操作，而是根据操作名字进行分派的智能数据对象。这种情况下，一个数据对象，例如一个指标坐标的复数，表现为一个过程，它接收一个操作名字然后执行对应的操作。例如

```scheme
(define (make-from-real-imag x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) x)
          ((eq? op 'imag-part) y)
          ((eq? op 'magnitude)
           (sqrt (+ (square x) (square y))))
          ((eq? op 'angle) (atan y x))
          (else
           (error "Unknown op: 
            MAKE-FROM-REAL-IMAG" op))))
  dispatch)

; 简单地将操作名字喂给数据对象
(define (apply-generic op arg) (arg op))
```

OOP可以理解为这种方式？

这种编程风格叫做**message passing**，我们在2.1.3已经看到过一个消息传递的例子，在那里我们只用过程就定义了cons，car和cdr。

Exercise 2.75: 用消息传递风格实现构造函数make-from-mag-ang。这个过程跟上面的make-from-real-imag类似。

```scheme
(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* r (cos a)))
          ((eq? op 'imag-part) (* r (sin a)))
          ((eq? op 'magnitude) r)
          ((eq? op 'angle) a)
          (else
           (error "Unknown op: 
            MAKE-FROM-REAL-IMAG" op))))
  dispatch)
```

Exercise 2.76: 随着一个具有通用操作的大系统发展，可能需要新的数据对象或新的操作。对于以上三种策略—显式分派的通用操作、数据导向风格和消息传递风格，描述系统要新增类型或操作所必须做出的改动。

显示分派：对于新增类型，需要实现所有具体操作，然后每个旧的通用操作需要修改以支持分派给新类型。对于新增操作，每个类型需要实现具体操作，然后新增一个对应的通用操作用于分派。不需要修改旧代码。

数据导向：对于新增类型，需要实现一个新类型的安装包，然后在中心系统中安装它。对于新增操作，每个安装包中新增具体操作，系统无需修改。

消息传递：对于新增类型，需要实现一个新的构造函数，无需修改旧代码。对于新增操作，每个旧的构造函数中新增对应具体操作。

所以经常新增类型，数据导向或消息传递。经常新增操作，显式分派可能更好，数据导向和消息传递差不多。

其实每种方式需要修改的代码量都是差不多的，只是分散集中的程度以及定位难易程度的不同。



## 2.5 Systems with Generic Operations

使用同样的思想，我们不仅可以定义对不同表现通用的操作，也可以定义对不同类型参数通用的操作。我们将构建一个合并到目前为止所有算术包的包。

### Generic Arithmetic Operations

```scheme
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

; scheme-package
(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  'done)

; user of the scheme-package
(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

; rational-package
(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))

; complex-package
(define (install-complex-package)
  ;; imported procedures from rectangular 
  ;; and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 
          'rectangular) 
     x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) 
     r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag 
     (+ (real-part z1) (real-part z2))
     (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag 
     (- (real-part z1) (real-part z2))
     (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang
     (* (magnitude z1) (magnitude z2))
     (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang 
     (/ (magnitude z1) (magnitude z2))
     (- (angle z1) (angle z2))))
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) 
         (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) 
         (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) 
         (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) 
         (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) 
         (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) 
         (tag (make-from-mag-ang r a))))
  'done)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))
```



Exercise 2.77: 直接对复数调用magnitude报了一个对类型complex不存在magnitude操作。因为只对polar和rectangular定义了这个操作。需要在complex包中增加以下内容

```scheme
  (define (real-part z)
    ((get 'real-part (list (type-tag z)))
     (contents z)))
  (define (imag-part z)
    ((get 'imag-part (list (type-tag z)))
     (contents z)))
  (define (magnitude z)
    ((get 'magnitude (list (type-tag z)))
     (contents z)))
  (define (angle z)
    ((get 'angle (list (type-tag z)))
     (contents z)))
(put 'real-part '(complex) real-part)
(put 'imag-part '(complex) imag-part)
(put 'magnitude '(complex) magnitude)
(put 'angle '(complex) angle)
```

有两层标签，去掉外面complex这一层。

Exercise 2.78: 所有Lisp实现都内置类型系统。修改2.4.2中type-tag, contents, attach-tag的定义，以使我们的通用系统可以利用内置类型系统。也就是说，不加tag的普通数字可以在系统中正常工作。

```scheme
(define (attach-tag type-tag contents)
  (if (number? contents)
      contents
      (cons type-tag contents)))

(define (type-tag datum)
  (cond
   ((number? datum) 'scheme-number)
   ((pair? datum) (car datum))
   (else (error "Bad tagged datum: 
              TYPE-TAG" datum))))

(define (contents datum)
  (cond
   ((number? datum) datum)
   ((pair? datum) (cdr datum))
   (else (error "Bad tagged datum: 
              TYPE-TAG" datum))))
```

Exercise 2.79: 定义一个通用的谓词equ?可以测试两个数是否相等。

```scheme
 (define (install-scheme-number-package) 
   ;; ... 
   (put 'equ? '(scheme-number scheme-number) =) 
   'done) 
  
 (define (install-rational-package) 
   ;; ... 
   (define (equ? x y) 
     (= (* (numer x) (denom y)) (* (numer y) (denom x)))) 
   ;; ... 
   (put 'equ? '(rational rational) equ?) 
   'done) 
  
 (define (install-complex-package) 
   ;; ... 
   (define (equ? x y) 
     (and (= (real-part x) (real-part y)) (= (imag-part x) (imag-part y)))) 
   ;; ... 
   (put 'equ? '(complex complex) equ?) 
   'done) 
  
 (define (equ? x y) (apply-generic 'equ? x y)) 
```

Exercise 2.80: 定义一个通用的=zero?测试参数是否为0

```scheme
 (define (install-scheme-number-package) 
   ;; ... 
   (put '=zero? '(scheme-number)
        (lambda (x) (= x 0))) 
   'done) 
  
 (define (install-rational-package) 
   ;; ... 
   (put '=zero? '(rational)
        (lambda (x) (= (numer x) 0))) 
   'done) 
  
 (define (install-complex-package) 
   ;; ...
   (put '=zero? '(complex)
        (lambda (x) (= (magnitude x) 0))) 
   'done) 
  
 (define (=zero? x) (apply-generic '=zero? x))
```

### Combining Data of Different Types

前面定义的操作将不同的数据类型看成是完全独立的，而跨类型边界的操作也是有意义的。

处理交叉操作的一种方法是给每种可能的组合都设计一个不同的过程。这种方法很笨重，引入新类型不仅要构建这个类型的过程的包，还是构建和安装跨类型的操作。而且责任不好划分。



Coercion强迫/威压

一种类型的对象可能可以看成是另一种类型，这个过程叫做coercion。

一般地，我们可以设计coercion过程来转换对象。

```scheme
(define (scheme-number->complex n)
  (make-complex-from-real-imag 
   (contents n) 0))

(put-coercion 'scheme-number 'complex 
              scheme-number->complex)
```

一旦我们设置好了coercion表，就可以通过修改apply-generic过程以统一的方式来处理coercion。

```scheme
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (let ((t1->t2 
                       (get-coercion type1
                                     type2))
                      (t2->t1 
                       (get-coercion type2 
                                     type1)))
                  (cond (t1->t2
                         (apply-generic 
                          op (t1->t2 a1) a2))
                        (t2->t1
                         (apply-generic 
                          op a1 (t2->t1 a2)))
                        (else
                         (error 
                          "No method for 
                           these types"
                          (list 
                           op 
                           type-tags))))))
              (error 
               "No method for these types"
              (list op type-tags)))))))
```

已经大大简化了，但是还不够通用，可能a和b都不能相互转化，但是都可以转化为第三种类型。



Hierarchies of types

执行操作时可以提升类型，直到所有对象类型相同。同时，对于结果又可以降低到最小类型。



Inadequacies of hierarchies

实际关系可能很复杂，有多个超类型或子类型。

Exercise 2.81: a增加了相同类型的coercion函数之后，apply-generic对于没找到对应操作的情况会进入无限递归。b是需要做些改动，类型相同时终止递归

```scheme
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (if (equal? type1 type2)
                    (error "No method for these types" (list op type-tags))
                    (let ((t1->t2 
                           (get-coercion type1
                                         type2))
                          (t2->t1 
                           (get-coercion type2 
                                         type1)))
                      (cond (t1->t2
                             (apply-generic 
                              op (t1->t2 a1) a2))
                            (t2->t1
                             (apply-generic 
                              op a1 (t2->t1 a2)))
                            (else
                             (error 
                              "No method for 
                           these types"
                              (list 
                               op 
                               type-tags)))))))
              (error 
               "No method for these types"
               (list op type-tags)))))))
```

Exercise 2.82: 一般化apply-generic处理多个参数的coercion。一种策略是依次尝试将所有参数转成某个参数的类型。这种策略不够一般，有些混合类型的操作不会被试到。

Exercise 2.83: 你在设计一个通用算术系统，处理前面的类型塔：integer, rational, real, complex。给每种类型设计一个步骤用于提升一个类型。如何安装一个通用的raise操作对每个类型都能工作。

```scheme
(put 'raise '(scheme-number)
     (lambda (x) (make-rational x 1)))

(put 'raise '(rational)
     (lambda (x) (make-real (/ (numer x) (denom x)))))

(put 'raise '(real)
     (lambda (x) (make-complex-from-real-imag x 0)))

(define (raise x)
  (apply-generic 'raise x))
```

Exercise 2.84: 使用raise操作，修改apply-generic。需要想出一个方法测试两个类型哪个在上面。并且要求兼容系统的其他部分，增加新层也不会导致问题。

```scheme
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (equ? x y) (apply-generic 'equ? x y))
(define (=zero? x) (apply-generic '=zero? x))
(define (raise x) (apply-generic 'raise x))
(define (level x) (apply-generic 'level x))

; user of the scheme-package
(define (make-integer n)
  ((get 'make 'integer) n))

(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (make-real n)
  ((get 'make 'real) n))

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(define (all-equal? a . l)
  (define (iter l)
    (cond
      ((null? l) #t)
      ((not (equal? a  (car l))) #f)
      (else (iter (cdr l)))))
  (iter l))

(module+ test
  (require rackunit)
  (check-equal? (raise-to-highest `(,(make-integer 1) ,(make-integer 1))) `(,(make-integer 1) ,(make-integer 1)))
  (check-equal? (raise-to-highest `(,(make-rational 1 2) ,(make-rational 1 4)))
                `(,(make-rational 1 2) ,(make-rational 1 4)))
  (check-equal? (raise-to-highest '(1.5 1.5)) '(1.5 1.5))
  (check-equal? (raise-to-highest `(,(make-complex-from-real-imag 1 1) ,(make-complex-from-real-imag 1 1)))
                `(,(make-complex-from-real-imag 1 1) ,(make-complex-from-real-imag 1 1)))
  (check-equal? (raise-to-highest `(,(make-complex-from-mag-ang 1 1) ,(make-complex-from-mag-ang 1 1)))
                `(,(make-complex-from-mag-ang 1 1) ,(make-complex-from-mag-ang 1 1)))
  (check-equal? (raise-to-highest `(,(make-integer 1) ,(make-rational 1 2))) `(,(make-rational 1 1) ,(make-rational 1 2)))
  (check-equal? (raise-to-highest `(,(make-integer 1) 1.5)) '(1 1.5))
  (check-equal? (raise-to-highest `(,(make-integer 1) ,(make-complex-from-real-imag 1 1)))
                `(,(make-complex-from-real-imag 1 0) ,(make-complex-from-real-imag 1 1)))
  (check-equal? (raise-to-highest `(,(make-rational 1 2) ,(make-complex-from-real-imag 1 1)))
                `(,(make-complex-from-real-imag 1/2 0) ,(make-complex-from-real-imag 1 1)))
  (check-equal? (raise-to-highest `(,(make-rational 1 2) 1.5)) `(1/2 1.5))
  (check-equal? (raise-to-highest `(1.0 ,(make-complex-from-real-imag 1 1)))
                `(,(make-complex-from-real-imag 1.0 0) ,(make-complex-from-real-imag 1 1)))
  )
(define (raise-to-highest l)
  (define (raise-to x t)
    (define (iter x)
      (cond
        ((eq? (type-tag x) t) x)
        (else (iter (raise x)))))
    (iter x))
  (let ((highest (foldr (lambda (x high)
                          (if (> (level x) (level high))
                              x
                              high))
                        (make-integer 0) ; integer
                        l)))
    (map (lambda (x)
           (raise-to x (type-tag highest)))
         l)))

(define (apply-generic op . args)
  (let* ((type-tags (map type-tag args))
         (proc (get op type-tags)))
    (if proc
        (apply proc (map contents args))
        (if (apply all-equal? type-tags)
            (error "No method for these types" (list op type-tags))
            (apply apply-generic op (raise-to-highest args))))))

; get/put
(define lof '())
(define (put op tag func)
  (set! lof (cons (list op tag func) lof)))
(define (get op tag)
  (define (iter l)
    (cond
      ((null? l) #f)
      ((and (equal? (car (car l)) op)
            (equal? (cadr (car l)) tag))
       (caddr (car l)))
      (else (iter (cdr l)))))
  (iter lof))

; integer-package
(define (install-integer-package)
  (define (tag x)
    (attach-tag 'integer x))
  (put 'add '(integer integer)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(integer integer)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(integer integer)
       (lambda (x y) (tag (* x y))))
  (put 'div '(integer integer)
       (lambda (x y) (tag (/ x y))))
  (put 'equ? '(integer integer) =)
  (put '=zero? '(integer)
       (lambda (x) (= x 0)))
  (put 'raise '(integer)
       (lambda (x) (make-rational x 1)))
  (put 'level '(integer) (lambda (x) 1))
  (put 'make 'integer
       (lambda (x)
         (cond
           ((exact-integer? x) (tag x))
           ((integer? x) (tag (inexact->exact x)))
           (else "expect an integer, given " x))))
  'done)

; rational-package
(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  (define (equ? x y) 
    (= (* (numer x) (denom y)) (* (numer y) (denom x))))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'equ? '(rational rational) equ?)
  (put '=zero? '(rational)
       (lambda (x) (= (numer x) 0)))
  (put 'raise '(rational)
       (lambda (x) (make-real (/ (numer x) (denom x)))))
  (put 'level '(rational)
       (lambda (x) (+ 1 (level (make-integer 1)))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d)))) ; check params
  'done)

; real-package
(define (install-real-package)
  (define (tag x)
    (attach-tag 'real x))
  (put 'add '(real real)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(real real)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(real real)
       (lambda (x y) (tag (* x y))))
  (put 'div '(real real)
       (lambda (x y) (tag (/ x y))))
  (put 'equ? '(real real) =)
  (put '=zero? '(real)
       (lambda (x) (= x 0)))
  (put 'raise '(real)
       (lambda (x) (make-complex-from-real-imag x 0)))
  (put 'level '(real)
       (lambda (x) (+ 1 (level (make-rational 1 2)))))
  (put 'make 'real
       (lambda (x) (tag x)))
  'done)


; complex-package
(define (install-complex-package)
  ;; imported procedures from rectangular 
  ;; and polar packages
  (define (real-part z)
    ((get 'real-part (list (type-tag z)))
     (contents z)))
  (define (imag-part z)
    ((get 'imag-part (list (type-tag z)))
     (contents z)))
  (define (magnitude z)
    ((get 'magnitude (list (type-tag z)))
     (contents z)))
  (define (angle z)
    ((get 'angle (list (type-tag z)))
     (contents z)))
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 
          'rectangular) 
     x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) 
     r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag 
     (+ (real-part z1) (real-part z2))
     (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag 
     (- (real-part z1) (real-part z2))
     (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang
     (* (magnitude z1) (magnitude z2))
     (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang 
     (/ (magnitude z1) (magnitude z2))
     (- (angle z1) (angle z2))))
  (define (equ? x y) 
    (and (= (real-part x) (real-part y)) (= (imag-part x) (imag-part y))))
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  (put 'add '(complex complex)
       (lambda (z1 z2) 
         (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) 
         (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) 
         (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) 
         (tag (div-complex z1 z2))))
  (put 'equ? '(complex complex) equ?)
  (put '=zero? '(complex)
       (lambda (x) (= (magnitude x) 0)))
  (put 'level '(complex)
       (lambda (x) (+ 1 (level 1.0))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) 
         (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) 
         (tag (make-from-mag-ang r a))))
  'done)


(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (magnitude z)
    (sqrt (+ (sqr (real-part z))
             (sqr (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-real-imag x y) 
    (cons x y))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  ;; interface to the rest of the system
  (define (tag x) 
    (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part) ; 使用'(rectangular)而非'rectangular,允许多个参数操作的可能性,不是所有参数都是相同类型
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) 
         (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) 
         (tag (make-from-mag-ang r a))))
  'done)

; 极坐标的实现也是类似
(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (sqr x) (sqr y)))
          (atan y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) 
         (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) 
         (tag (make-from-mag-ang r a))))
  'done)


; low level util
(define (attach-tag type-tag contents)
  (if (equal? type-tag 'real)
      contents
      (cons type-tag contents)))

(define (type-tag datum)
  (cond
    ((number? datum) 'real)
    ((pair? datum) (car datum))
    (else (error "Bad tagged datum: 
              TYPE-TAG" datum))))
   
(define (contents datum)
  (cond
    ((number? datum) datum)
    ((pair? datum) (cdr datum))
    (else (error "Bad tagged datum: 
              TYPE-TAG" datum))))


; integrate
(define (init)
  (install-integer-package)
  (install-rational-package)
  (install-real-package)
  (install-complex-package)
  (install-rectangular-package)
  (install-polar-package)
  )
(init)

; test
;> (add (make-integer 1) (make-integer 1))
;'(integer . 2)
;> (add (make-integer 1) (make-rational 1 2))
;'(rational 3 . 2)
;> (add (make-integer 1) 1.5)
;2.5
;> (add (make-integer 1) (make-complex-from-real-imag 1 1))
;'(complex rectangular 2 . 1)
;> (add (make-rational 1 2) 1.5)
;2.0
;> (add (make-rational 1 2) (make-complex-from-real-imag 1 1))
;'(complex rectangular 1 1/2 . 1)
;> (add 1.0 (make-complex-from-real-imag 1 1))
;'(complex rectangular 2.0 . 1)
```



Exercise 2.85: 定义drop步骤简化。每个数据类型定义project将数据类型往下压一层，如果再raise上来之后结果不变，说明可以往下drop。然后用drop重写apply-generic

```scheme
(define (project x) (apply-generic 'project x))

(put 'project '(rational)
     (lambda (x) (make-integer (quotient (numer x) (denom x)))))

(put 'project '(real)
       (lambda (x) (make-rational (inexact->exact (numerator x))
                                  (inexact->exact (denominator x)))))
(put 'project '(complex)
       (lambda (x) (real-part x)))

(define (dropable? op)
  (if (or (equal? op 'add)
          (equal? op 'sub)
          (equal? op 'mul)
          (equal? op 'div))
      #t
      #f))

(define (drop x)
  (if (= 1 (level x))
      x
      (let ((lower (project x)))
        (if (equ? (raise lower) x)
            (drop lower)
            x))))

(define (apply-generic op . args)
  (let* ((type-tags (map type-tag args))
         (proc (get op type-tags)))
    (if proc
        (let ((res (apply proc (map contents args))))
          (if (dropable? op)
              (drop res)
              res))
        (if (apply all-equal? type-tags)
            (error "No method for these types" (list op type-tags))
            (apply apply-generic op (raise-to-highest args))))))

> (add (make-rational 1 2) (make-rational 1 2))
'(integer . 1)
> (add 1.5 1.5)
'(integer . 3)
> (add (make-complex-from-real-imag 1 1) (make-complex-from-real-imag 1 -1))
'(integer . 2)
> (add (make-complex-from-real-imag 1 1) (make-complex-from-real-imag 1.5 -1))
'(rational 5 . 2)
```



Exercise 2.86: 如果想让复数的实部虚部幅度角度都可以是普通数、有理数或者其他我们可能加到系统中的数。描述系统需要做哪些改变并实现。你必须定义普通数和有理数通用的sine和cosine，real-part和imag-part，sqr和atan。同时复数的四则运算不使用内建的过程而是使用我们定义的通用版本。



### Example: Symbolic Algebra

Arithmetic on Polynomials

```scheme
(define (install-polynomial-package)
  ;; internal procedures
  ;; representation of poly
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1)
         (variable? v2)
         (eq? v1 v2)))

  ;; representation of terms and term lists
(define (adjoin-term term term-list)
  (if (=zero? (coeff term))
      term-list
      (cons term term-list)))
(define (the-empty-termlist) '())
(define (first-term term-list) (car term-list))
(define (rest-terms term-list) (cdr term-list))
(define (empty-termlist? term-list) 
  (null? term-list))
(define (make-term order coeff) 
  (list order coeff))
(define (order term) (car term))
(define (coeff term) (cadr term))

 (define (add-poly p1 p2)
  (if (same-variable? (variable p1) 
                      (variable p2))
      (make-poly 
       (variable p1)
       (add-terms (term-list p1)
                  (term-list p2)))
      (error "Polys not in same var: 
              ADD-POLY"
             (list p1 p2))))
 (define (add-terms L1 L2)
  (cond ((empty-termlist? L1) L2)
        ((empty-termlist? L2) L1)
        (else
         (let ((t1 (first-term L1)) 
               (t2 (first-term L2)))
           (cond ((> (order t1) (order t2))
                  (adjoin-term
                   t1 
                   (add-terms (rest-terms L1) 
                              L2)))
                 ((< (order t1) (order t2))
                  (adjoin-term
                   t2 
                   (add-terms 
                    L1 
                    (rest-terms L2))))
                 (else
                  (adjoin-term
                   (make-term 
                    (order t1)
                    (add (coeff t1) ; support nested polynomials (还需要支持类型提升)
                         (coeff t2)))
                   (add-terms 
                    (rest-terms L1)
                    (rest-terms L2)))))))))

 (define (mul-poly p1 p2)
  (if (same-variable? (variable p1) 
                      (variable p2))
      (make-poly 
       (variable p1)
       (mul-terms (term-list p1)
                  (term-list p2)))
      (error "Polys not in same var: 
              MUL-POLY"
             (list p1 p2))))
(define (mul-terms L1 L2)
  (if (empty-termlist? L1)
      (the-empty-termlist)
      (add-terms 
       (mul-term-by-all-terms 
        (first-term L1) L2)
       (mul-terms (rest-terms L1) L2))))

(define (mul-term-by-all-terms t1 L)
  (if (empty-termlist? L)
      (the-empty-termlist)
      (let ((t2 (first-term L)))
        (adjoin-term
         (make-term 
          (+ (order t1) (order t2))
          (mul (coeff t1) (coeff t2)))
         (mul-term-by-all-terms 
          t1 
          (rest-terms L))))))


  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) 
         (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) 
         (tag (mul-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms) 
         (tag (make-poly var terms))))
  'done)

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))
```



**Representing term lists**

dense的直接表示成系数的list，sparse的表示成非零项的list，每一项包含order和coefficient。



Exercise 2.87: 实现多项式的=zero?

```scheme
(define (zero-poly? poly)
  (define (zero-termlist? term-list)
    (cond
     ((empty-termlist? term-list) #t)
     ((=zero? (coeff (first-term term-list)))
      (zero-termlist? (rest-terms term-list)))
     (else #f)))
  (zero-termlist? (term-list poly)))

(put '=zero? '(polynomial) zero-poly?)
```

Exercise 2.88: 扩展多项式系统支持减法（定义一个通用的取反操作可能很有用）

```scheme
#lang racket

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (neg x) (apply-generic 'neg x))
(define (equ? x y) (apply-generic 'equ? x y))
(define (=zero? x) (apply-generic '=zero? x))
(define (raise x) (apply-generic 'raise x))
(define (level x) (apply-generic 'level x))
(define (project x) (apply-generic 'project x))

(define (dropable? op)
  (if (or (equal? op 'add)
          (equal? op 'sub)
          (equal? op 'mul)
          (equal? op 'div))
      #t
      #f))

; user of the scheme-package
(define (make-integer n)
  ((get 'make 'integer) n))

(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (make-real n)
  ((get 'make 'real) n))

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))

(define (all-equal? a . l)
  (define (iter l)
    (cond
      ((null? l) #t)
      ((not (equal? a  (car l))) #f)
      (else (iter (cdr l)))))
  (iter l))

(module+ test
  (require rackunit)
  (check-equal? (raise-to-highest `(,(make-integer 1) ,(make-integer 1))) `(,(make-integer 1) ,(make-integer 1)))
  (check-equal? (raise-to-highest `(,(make-rational 1 2) ,(make-rational 1 4)))
                `(,(make-rational 1 2) ,(make-rational 1 4)))
  (check-equal? (raise-to-highest '(1.5 1.5)) '(1.5 1.5))
  (check-equal? (raise-to-highest `(,(make-complex-from-real-imag 1 1) ,(make-complex-from-real-imag 1 1)))
                `(,(make-complex-from-real-imag 1 1) ,(make-complex-from-real-imag 1 1)))
  (check-equal? (raise-to-highest `(,(make-complex-from-mag-ang 1 1) ,(make-complex-from-mag-ang 1 1)))
                `(,(make-complex-from-mag-ang 1 1) ,(make-complex-from-mag-ang 1 1)))
  (check-equal? (raise-to-highest `(,(make-integer 1) ,(make-rational 1 2))) `(,(make-rational 1 1) ,(make-rational 1 2)))
  (check-equal? (raise-to-highest `(,(make-integer 1) 1.5)) '(1 1.5))
  (check-equal? (raise-to-highest `(,(make-integer 1) ,(make-complex-from-real-imag 1 1)))
                `(,(make-complex-from-real-imag 1 0) ,(make-complex-from-real-imag 1 1)))
  (check-equal? (raise-to-highest `(,(make-rational 1 2) ,(make-complex-from-real-imag 1 1)))
                `(,(make-complex-from-real-imag 1/2 0) ,(make-complex-from-real-imag 1 1)))
  (check-equal? (raise-to-highest `(,(make-rational 1 2) 1.5)) `(1/2 1.5))
  (check-equal? (raise-to-highest `(1.0 ,(make-complex-from-real-imag 1 1)))
                `(,(make-complex-from-real-imag 1.0 0) ,(make-complex-from-real-imag 1 1)))
  )
; XXX add raise-able
(define (raise-to-highest l)
  (define (raise-to x t)
    (define (iter x)
      (cond
        ((eq? (type-tag x) t) x)
        (else (iter (raise x)))))
    (iter x))
  (let ((highest (foldr (lambda (x high)
                          (if (> (level x) (level high))
                              x
                              high))
                        (make-integer 0) ; integer
                        l)))
    (map (lambda (x)
           (raise-to x (type-tag highest)))
         l)))

(define (drop x)
  (if (= 1 (level x))
      x
      (let ((lower (project x)))
        (if (equ? (raise lower) x)
            (drop lower)
            x))))

(define (apply-generic op . args)
  (let* ((type-tags (map type-tag args))
         (proc (get op type-tags)))
    (if proc
        (let ((res (apply proc (map contents args))))
          (if (dropable? op)
              (drop res)
              res))
        (if (apply all-equal? type-tags)
            (error "No method for these types" (list op type-tags))
            (apply apply-generic op (raise-to-highest args))))))

; get/put
(define lof '())
(define (put op tag func)
  (set! lof (cons (list op tag func) lof)))
(define (get op tag)
  (define (iter l)
    (cond
      ((null? l) #f)
      ((and (equal? (car (car l)) op)
            (equal? (cadr (car l)) tag))
       (caddr (car l)))
      (else (iter (cdr l)))))
  (iter lof))

; integer-package
(define (install-integer-package)
  (define (tag x)
    (attach-tag 'integer x))
  (put 'add '(integer integer)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(integer integer)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(integer integer)
       (lambda (x y) (tag (* x y))))
  (put 'div '(integer integer)
       (lambda (x y) (tag (/ x y))))
  (put 'neg '(integer)
       (lambda (x) (tag (- x))))
  (put 'equ? '(integer integer) =)
  (put '=zero? '(integer)
       (lambda (x) (= x 0)))
  (put 'raise '(integer)
       (lambda (x) (make-rational x 1)))
  (put 'level '(integer) (lambda (x) 1))
  ; no project, the lowest level
  (put 'make 'integer
       (lambda (x)
         (cond
           ((exact-integer? x) (tag x))
           ((integer? x) (tag (inexact->exact x)))
           (else "expect an integer, given " x))))
  'done)

; rational-package
(define (install-rational-package)
  ;; internal procedures
  (define level-rational (+ 1 (level (make-integer 1))))
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  
  (define (equ? x y) 
    (= (* (numer x) (denom y)) (* (numer y) (denom x))))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'neg '(rational)
       (lambda (x) (tag (make-rational (- (numer x))
                                       (denom x)))))
  (put 'equ? '(rational rational) equ?)
  (put '=zero? '(rational)
       (lambda (x) (= (numer x) 0)))
  (put 'raise '(rational)
       (lambda (x) (make-real (/ (numer x) (denom x)))))
  (put 'level '(rational)
       (lambda (x) level-rational))
  (put 'project '(rational)
       (lambda (x) (make-integer (quotient (numer x) (denom x)))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d)))) ; check params
  'done)

; real-package
(define (install-real-package)
  (define level-real (+ 1 (level (make-rational 1 2))))
  (define (tag x)
    (attach-tag 'real x))
  (put 'add '(real real)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(real real)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(real real)
       (lambda (x y) (tag (* x y))))
  (put 'div '(real real)
       (lambda (x y) (tag (/ x y))))
  (put 'neg '(real)
       (lambda (x) (tag (- x))))
  (put 'equ? '(real real) =)
  (put '=zero? '(real)
       (lambda (x) (= x 0)))
  (put 'raise '(real)
       (lambda (x) (make-complex-from-real-imag x 0)))
  (put 'level '(real)
       (lambda (x) level-real))
  (put 'project '(real)
       (lambda (x) (make-rational (inexact->exact (numerator x))
                                  (inexact->exact (denominator x)))))
  (put 'make 'real
       (lambda (x) (tag x))) ; check param
  'done)


; complex-package
(define (install-complex-package)
  ;; imported procedures from rectangular 
  ;; and polar packages
  (define (real-part z)
    ((get 'real-part (list (type-tag z)))
     (contents z)))
  (define (imag-part z)
    ((get 'imag-part (list (type-tag z)))
     (contents z)))
  (define (magnitude z)
    ((get 'magnitude (list (type-tag z)))
     (contents z)))
  (define (angle z)
    ((get 'angle (list (type-tag z)))
     (contents z)))
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 
          'rectangular) 
     x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) 
     r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag 
     (+ (real-part z1) (real-part z2))
     (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag 
     (- (real-part z1) (real-part z2))
     (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang
     (* (magnitude z1) (magnitude z2))
     (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang 
     (/ (magnitude z1) (magnitude z2))
     (- (angle z1) (angle z2))))
  (define (equ? x y) 
    (and (= (real-part x) (real-part y)) (= (imag-part x) (imag-part y))))
  (define level-complex (+ 1 (level 1.0)))
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  (put 'add '(complex complex)
       (lambda (z1 z2) 
         (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) 
         (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) 
         (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) 
         (tag (div-complex z1 z2))))
  (put 'neg '(complex)
       (lambda (x) (tag (neg x))))  ; pass to lower level
  (put 'equ? '(complex complex) equ?)
  (put '=zero? '(complex)
       (lambda (x) (= (magnitude x) 0)))
  (put 'raise '(complex)
       (lambda (x) (make-polynomial 'any `((0 ,(tag x))))))
  (put 'level '(complex)
       (lambda (x) level-complex))
  (put 'project '(complex)
       (lambda (x) (make-real (real-part x))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) 
         (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) 
         (tag (make-from-mag-ang r a))))
  'done)


(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (magnitude z)
    (sqrt (+ (sqr (real-part z))
             (sqr (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-real-imag x y) 
    (cons x y))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  ;; interface to the rest of the system
  (define (tag x) 
    (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part) ; 使用'(rectangular)而非'rectangular,允许多个参数操作的可能性,不是所有参数都是相同类型
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'neg '(rectangular)
       (lambda (x) (tag (make-from-real-imag
                         (- (real-part x))
                         (- (imag-part x))))))
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) 
         (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) 
         (tag (make-from-mag-ang r a))))
  'done)

; 极坐标的实现也是类似
(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (sqr x) (sqr y)))
          (atan y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'neg '(polar)
       (lambda (x) (tag (make-from-mag-ang
                         (magnitude x)
                         (- (angle x))))))
  (put 'make-from-real-imag 'polar
       (lambda (x y) 
         (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) 
         (tag (make-from-mag-ang r a))))
  'done)


; polynomials package
(define (install-polynomial-package)
  ;; internal procedures
  ;; representation of poly
  (define level-poly (+ 1 (level (make-complex-from-real-imag 1 0))))
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1)
         (variable? v2)
         (eq? v1 v2)))
  (define any-variable 'any)
  (define (process-any p1 p2)
    (cond
      ((same-variable? (variable p1) any-variable)
       (list (make-poly (variable p2) (term-list p1)) p2))
      ((same-variable? (variable p2) any-variable)
       (list p1 (make-poly (variable p1) (term-list p2))))
      (else (list p1 p2))))

  ;; representation of terms and term lists
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))
  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) 
    (null? term-list))
  (define (make-term order coeff) 
    (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))

  (define (add-poly p1 p2)
    (let* ((new_polys (process-any p1 p2)) ; process the poly raised from complex
           (p1 (car new_polys))
           (p2 (cadr new_polys)))
      (if (same-variable? (variable p1) 
                          (variable p2))
          (make-poly 
           (variable p1)
           (add-terms (term-list p1)
                      (term-list p2)))
          (error "Polys not in same var: 
              ADD-POLY"
                 (list p1 p2)))))
    
  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
           (let ((t1 (first-term L1)) 
                 (t2 (first-term L2)))
             (cond ((> (order t1) (order t2))
                    (adjoin-term
                     t1 
                     (add-terms (rest-terms L1) 
                                L2)))
                   ((< (order t1) (order t2))
                    (adjoin-term
                     t2 
                     (add-terms 
                      L1 
                      (rest-terms L2))))
                   (else
                    (adjoin-term
                     (make-term 
                      (order t1)
                      (add (coeff t1) ; support nested polynomials (还需要支持类型提升)
                           (coeff t2)))
                     (add-terms 
                      (rest-terms L1)
                      (rest-terms L2)))))))))

  (define (mul-poly p1 p2)
    (let* ((new_polys (process-any p1 p2)) ; process the poly raised from complex
           (p1 (car new_polys))
           (p2 (cadr new_polys)))
      (if (same-variable? (variable p1) 
                          (variable p2))
          (make-poly 
           (variable p1)
           (mul-terms (term-list p1)
                      (term-list p2)))
          (error "Polys not in same var: 
              MUL-POLY"
                 (list p1 p2)))))
  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-terms 
         (mul-term-by-all-terms 
          (first-term L1) L2)
         (mul-terms (rest-terms L1) L2))))

  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t2 (first-term L)))
          (adjoin-term
           (make-term 
            (+ (order t1) (order t2))
            (mul (coeff t1) (coeff t2)))
           (mul-term-by-all-terms 
            t1 
            (rest-terms L))))))

  (define (neg-term term)
    (make-term (order term) (neg (coeff term))))
  (define (neg-terms terms)
    (cond
      ((empty-termlist? terms) empty-termlist?)
      (else (adjoin-term (neg-term (first-term terms))
                         (neg-terms (rest-terms terms))))))

  (define (zero-poly? poly)
    (define (zero-termlist? term-list)
      (cond
        ((empty-termlist? term-list) #t)
        ((=zero? (coeff (first-term term-list)))
         (zero-termlist? (rest-terms term-list)))
        (else #f)))
    (zero-termlist? (term-list poly)))

  (define (project-poly poly)
    (define (iter terms)
      (cond
        ((null? terms) 0)
        ((= 0 (order (first-term terms))) (coeff (first-term terms)))
        (else (iter (rest-terms terms)))))
    (iter (term-list poly)))

  (define (equ-terms? l1 l2)
    (cond
      ((and (empty-termlist? l1) (empty-termlist? l2)) #t)
      ((or (empty-termlist? l1) (empty-termlist? l2)) #f)
      (else
       (let ((t1 (first-term l1))
             (t2 (first-term l2)))
         (if (or (not (= (order t1) (order t2)))
                 (not (equ? (coeff t1) (coeff t2))))
             #f
             (equ-terms? (rest-terms l1) (rest-terms l2)))))))
      
  (define (equ-poly? p1 p2)
    (let* ((new_polys (process-any p1 p2)) ; process the poly raised from complex
           (p1 (car new_polys))
           (p2 (cadr new_polys)))
      (if (same-variable? (variable p1) 
                          (variable p2))
          (equ-terms? (term-list p1) (term-list p2))
          #f)))

  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) 
         (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) 
         (tag (mul-poly p1 p2))))
  (put 'neg '(polynomial)
       (lambda (p)
         (tag (neg-terms (term-list p)))))
  (put 'sub '(polynomial)
       (lambda (p1 p2)
         (tag (add p1 (neg p2)))))
  (put '=zero? '(polynomial) zero-poly?)
  (put 'equ? '(polynomial polynomial) equ-poly?)
  (put 'raise '(polynomial)
       (lambda (x) (make-polynomial 'any `((0 ,(tag x)))))) ; needed after being projected
  (put 'level '(polynomial)
       (lambda (x) level-poly))
  (put 'project '(polynomial)
       project-poly)
  (put 'make 'polynomial
       (lambda (var terms) 
         (tag (make-poly var terms))))
  'done)

; low level util
(define (attach-tag type-tag contents)
  (if (equal? type-tag 'real)
      contents
      (cons type-tag contents)))

(define (type-tag datum)
  (cond
    ((number? datum) 'real)
    ((pair? datum) (car datum))
    (else (error "Bad tagged datum: 
              TYPE-TAG" datum))))
   
(define (contents datum)
  (cond
    ((number? datum) datum)
    ((pair? datum) (cdr datum))
    (else (error "Bad tagged datum: 
              TYPE-TAG" datum))))


; integrate, keep the order
(define (init)
  (install-integer-package)
  (install-rational-package)
  (install-real-package)
  (install-rectangular-package)
  (install-polar-package)
  (install-complex-package)
  (install-polynomial-package)
  )

(init)

;>  (add (make-polynomial 'x '((2 1) (1 2) (0 3))) (make-polynomial 'x '((2 1) (0 1))))
;'(polynomial x (2 (integer . 2)) (1 2) (0 (integer . 4)))
;> (add (make-polynomial 'x '((1 1) (0 3))) (make-polynomial 'x (list (list 1 (make-polynomial 'y '((1 1) (0 2)))) (list 0 (make-polynomial 'y '((2 1) (0 1)))))))
;'(polynomial x (1 (polynomial y (1 1) (0 (integer . 3)))) (0 (polynomial y (2 1) (0 (integer . 4)))))
```

Exercise 2.89: 定义term-list的另一种适合dense多项式的表现方式

```scheme
; 只需要修改empty-terms?, first-term, rest-terms和adjoin-term
; 还需要修改polynomial/complex的raise, 不好的实现应该封装在polynomial的实现中，不需要修改外部complex包
  (define (empty-terms? l)
    (cond
      ((null? l) #t)
      ((=zero? (car l)) (empty-terms? (cdr l)))
      (else #f)))
  ; first non-zero term
  (define (first-term l)
    (if (=zero? (car l))
        (first-term (cdr l))
        (else (make-term
               (- (length l) 1)
               (car l)))))
  (define (rest-terms l)
    (if (=zero? (car l))
        (rest-terms (cdr l))
        (else (cdr l))))
  (define (adjoin-term term term-list)
    (cond
      ((=zero? (coeff term)) term-list)
      ((= (order term) (length term-list)) (cons (coeff term) term-list))
      (else (adjoin-term term (cons 0 term-list)))))

; 另外一个不相关的修改，poly用list更加清晰
  (define (make-poly variable term-list)
    (list variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cadr p))

;> (add (make-polynomial 'x '(1 2 3)) (make-polynomial 'x '(1 0 1)))
;'(polynomial x ((integer . 2) (integer . 2) (integer . 4)))
;> (add (make-polynomial 'x '(1 3)) (make-polynomial 'x (list (make-polynomial 'y '(1 2)) (make-polynomial 'y '(1 0 1)))))
;'(polynomial x ((polynomial y (1 (integer . 3))) (polynomial y (1 0 (integer . 4))))) 
;> (mul (make-polynomial 'x '(1 1)) (make-polynomial 'x '(1 2)))
;'(polynomial x ((integer . 1) (integer . 3) (integer . 2)))
;> (mul (make-polynomial 'x '(1 1)) (make-polynomial 'x (list (make-polynomial 'y '(1 1)) 2)))
;'(polynomial
;  x
;  ((polynomial y ((integer . 1) (integer . 1))) (polynomial y ((integer . 1) (integer . 3))) (integer . 2)))
```

Exercise 2.90: 假设我们像让多项式系统对于dense和sparse多项式都高效，一种方法就是在系统中允许两种term-list的表现形式。类似2.4节中复数的情况，我们必须区分两种表现形式并使term lists上的操作通用。重新设计多项式系统实现这个一般化。（这不是一个小改动，而是需要大动作）

```scheme
; term和term-list都加tag
; 支持同层转换transform
; 顺便修复之前多项式neg/sub的问题

(define (make-sparse-polynomial var terms)
  ((get 'make-poly-sparse 'polynomial) var terms))
(define (make-dense-polynomial var terms)
  ((get 'make-poly-dense 'polynomial) var terms))

; 支持同层转换transform
(define (raise-to-highest l)
  (define (raise-to x h)
    (define (iter x)
      (cond
        ((eq? (level x) (level h))
         (let ((trans (get 'transform
                           (list (type-tag x) (type-tag h)))))
           (if trans (trans (contents x) (contents h)) x)))
        (else (iter (raise x)))))
    (iter x))
  (let ((highest (foldr (lambda (x high)
                          (cond
                            ((equal? high #f) x)
                            ((> (level x) (level high)) x)
                            (else high)))
                        #f ; lowest level
                        l)))
    (map (lambda (x)
           (raise-to x highest))
         l)))

; complex 和 polynomial的raise函数需要更新
(put 'raise '(complex)
       (lambda (x) (make-sparse-polynomial 'any (list (list 0 (tag x))))))
(put 'raise '(polynomial)
       (lambda (x) (make-sparse-polynomial 'any (list (list 0 (tag x)))))) ; needed after being projected
 
; sparse-term-list package
; ((order coeff) (order coeff) ..)
(define (install-sparse-term-package)
  ;...
  'done)

; dense-term-list package
; (coeff 0 coeff)
(define (install-dense-term-package)
  ;...
  'done)

; polynomials package
(define (install-polynomial-package)
  ;; dependency
  (install-sparse-term-package)
  (install-dense-term-package)
  (define (add-terms x y) (apply-generic 'add-terms x y))
  (define (mul-terms x y) (apply-generic 'mul-terms x y))
  (define (neg-terms x) (apply-generic 'neg-terms x))
  (define (zero-terms? x) (apply-generic 'zero-terms? x))
  (define (order0-term-coeff x) (apply-generic 'order0-term-coeff x))
  (define (equ-terms? x y) (apply-generic 'equ-terms? x y))
  (define (make-terms-sparse x)
    ((get 'make-terms 'sparse) x))
  (define (make-terms-dense x)
    ((get 'make-terms 'dense) x))
  ;...
  'done)

;> (add (make-dense-polynomial 'x '(1 2 3)) (make-dense-polynomial 'x '(1 0 1)))
;'(polynomial x (dense (integer . 2) 2 (integer . 4)))
;> (add (make-dense-polynomial 'x '(1 2 3)) (make-sparse-polynomial 'x '((2 1) (0 1))))
;'(polynomial x (sparse (2 (integer . 2)) (1 2) (0 (integer . 4))))
;> (add (make-sparse-polynomial 'x '((2 1) (1 2) (0 3))) (make-sparse-polynomial 'x '((2 1) (0 1))))
;'(polynomial x (sparse (2 (integer . 2)) (1 2) (0 (integer . 4))))
;> (sub (make-dense-polynomial 'x '(1 2 3)) (make-sparse-polynomial 'x '((2 2) (0 1))))
;'(polynomial x (sparse (2 (integer . -1)) (1 2) (0 (integer . 2))))
;> (add (make-dense-polynomial 'x '(1 3)) (make-dense-polynomial 'x (list (make-dense-polynomial 'y '(1 2)) (make-dense-polynomial 'y '(1 0 1)))))
;'(polynomial x (dense (polynomial y (dense 1 (integer . 3))) (polynomial y (dense 1 0 (integer . 4)))))

;> (mul (make-dense-polynomial 'x '(1 1)) (make-dense-polynomial 'x '(1 2)))
;'(polynomial x (dense (integer . 1) (integer . 3) (integer . 2)))
;> (mul (make-sparse-polynomial 'x '((1 1) (0 1))) (make-sparse-polynomial 'x '((1 1) (0 2))))
;'(polynomial x (sparse (2 (integer . 1)) (1 (integer . 3)) (0 (integer . 2))))
;> (mul (make-sparse-polynomial 'x '((1 1) (0 1))) (make-dense-polynomial 'x '(1 2)))
;'(polynomial x (dense (integer . 1) (integer . 3) (integer . 2)))

;> (mul (make-dense-polynomial 'x '(1 1)) (make-dense-polynomial 'x (list (make-dense-polynomial 'y '(1 1)) 2)))
;'(polynomial
;  x
;  (dense
;   (polynomial y (dense (integer . 1) (integer . 1)))
;   (polynomial y (dense (integer . 1) (integer . 3)))
;   (integer . 2)))
```

Exercise 2.91: 实现多项式除法

((x^5-1)/(x^2-1)) = (x^3 +x), remainder x-1

```scheme
; dense多项式前导0的问题
; 两个结果作为list返回，需要特殊处理，有没有通用处理方式？put的时候加一项，指示返回值是否是list

(define (div-terms L1 L2)
  (if (empty-termlist? L1)
      (list (the-empty-termlist) 
            (the-empty-termlist))
      (let ((t1 (first-term L1))
            (t2 (first-term L2)))
        (if (> (order t2) (order t1))
            (list (the-empty-termlist) L1)
            (let* ((new-c (div (coeff t1) 
                               (coeff t2)))
                   (new-o (- (order t1) 
                             (order t2)))
                   (new-term (make-term new-o new-c)))
              (let ((rest-of-result
                     (div-terms (add-terms L1 (neg-terms (mul-terms L2 (adjoin-term new-term (the-empty-termlist))))) L2)))
                (list (adjoin-term new-term (car rest-of-result))
                      (cadr rest-of-result))))))))

;> (div (make-dense-polynomial 'x '(1 0 0 0 0 -1)) (make-dense-polynomial 'x '(1 0 -1)))
;'((polynomial x (dense (integer . 1) 0 (integer . 1) 0)) (polynomial x (dense (integer . 1) -1)))
;> (div (make-sparse-polynomial 'x '((5 1) (0 -1))) (make-sparse-polynomial 'x '((2 1) (0 -1))))
;'((polynomial x (sparse (3 (integer . 1)) (1 (integer . 1)))) (polynomial x (sparse (1 (integer . 1)) (0 -1))))
;> (div (make-sparse-polynomial 'x '((5 1) (0 -1))) (make-dense-polynomial 'x '(1 0 -1)))
;'((polynomial x (dense (integer . 1) 0 (integer . 1) 0)) (polynomial x (dense (integer . 1) -1)))
```



完整代码

```scheme
#lang racket

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (neg x) (apply-generic 'neg x))
(define (equ? x y) (apply-generic 'equ? x y))
(define (=zero? x) (apply-generic '=zero? x))
(define (raise x) (apply-generic 'raise x))
(define (level x) (apply-generic 'level x))
(define (project x) (apply-generic 'project x))

(define (dropable? op)
  (if (or (equal? op 'add)
          (equal? op 'sub)
          (equal? op 'mul)
          (equal? op 'div))
      #t
      #f))

; user of the scheme-package
(define (make-integer n)
  ((get 'make 'integer) n))

(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (make-real n)
  ((get 'make 'real) n))

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(define (make-sparse-polynomial var terms)
  ((get 'make-poly-sparse 'polynomial) var terms))
(define (make-dense-polynomial var terms)
  ((get 'make-poly-dense 'polynomial) var terms))

(define (all-equal? a . l)
  (define (iter l)
    (cond
      ((null? l) #t)
      ((not (equal? a  (car l))) #f)
      (else (iter (cdr l)))))
  (iter l))

(module+ test
  (require rackunit)
  (check-equal? (raise-to-highest `(,(make-integer 1) ,(make-integer 1))) `(,(make-integer 1) ,(make-integer 1)))
  (check-equal? (raise-to-highest `(,(make-rational 1 2) ,(make-rational 1 4)))
                `(,(make-rational 1 2) ,(make-rational 1 4)))
  (check-equal? (raise-to-highest '(1.5 1.5)) '(1.5 1.5))
  (check-equal? (raise-to-highest `(,(make-complex-from-real-imag 1 1) ,(make-complex-from-real-imag 1 1)))
                `(,(make-complex-from-real-imag 1 1) ,(make-complex-from-real-imag 1 1)))
  (check-equal? (raise-to-highest `(,(make-complex-from-mag-ang 1 1) ,(make-complex-from-mag-ang 1 1)))
                `(,(make-complex-from-mag-ang 1 1) ,(make-complex-from-mag-ang 1 1)))
  (check-equal? (raise-to-highest `(,(make-integer 1) ,(make-rational 1 2))) `(,(make-rational 1 1) ,(make-rational 1 2)))
  (check-equal? (raise-to-highest `(,(make-integer 1) 1.5)) '(1 1.5))
  (check-equal? (raise-to-highest `(,(make-integer 1) ,(make-complex-from-real-imag 1 1)))
                `(,(make-complex-from-real-imag 1 0) ,(make-complex-from-real-imag 1 1)))
  (check-equal? (raise-to-highest `(,(make-rational 1 2) ,(make-complex-from-real-imag 1 1)))
                `(,(make-complex-from-real-imag 1/2 0) ,(make-complex-from-real-imag 1 1)))
  (check-equal? (raise-to-highest `(,(make-rational 1 2) 1.5)) `(1/2 1.5))
  (check-equal? (raise-to-highest `(1.0 ,(make-complex-from-real-imag 1 1)))
                `(,(make-complex-from-real-imag 1.0 0) ,(make-complex-from-real-imag 1 1)))
  (check-equal? (raise-to-highest '((sparse (2 1) (0 1)) (dense 1 0 1)))
                '((dense 1 0 1) (dense 1 0 1)))
  (check-equal? (raise-to-highest '((dense 1 0 1) (sparse (2 1) (0 1))))
                '((sparse (2 1) (0 1)) (sparse (2 1) (0 1))))
  )
; XXX add raise-able
(define (raise-to-highest l)
  (define (raise-to x h)
    (define (iter x)
      (cond
        ((eq? (level x) (level h))
         (let ((trans (get 'transform
                           (list (type-tag x) (type-tag h)))))
           (if trans (trans (contents x) (contents h)) x)))
        (else (iter (raise x)))))
    (iter x))
  (let ((highest (foldr (lambda (x high)
                          (cond
                            ((equal? high #f) x)
                            ((> (level x) (level high)) x)
                            (else high)))
                        #f ; lowest level
                        l)))
    (map (lambda (x)
           (raise-to x highest))
         l)))

(define (drop x)
  (if (= 1 (level x))
      x
      (let ((lower (project x)))
        (if (equ? (raise lower) x)
            (drop lower)
            x))))

(define (drop-list l)
  (map drop l))

(define (apply-generic op . args)
  (let* ((type-tags (map type-tag args))
         (proc (get op type-tags)))
    (if proc
        (let ((res (apply proc (map contents args))))
          (cond
            ((not (dropable? op)) res)
            ((eq? (type-tag res) (type-tag (car args)))
             (drop res))
            (else (drop-list res))))
        (if (apply all-equal? type-tags)
            (error "No method for these types" (list op type-tags))
            (apply apply-generic op (raise-to-highest args))))))

; get/put
(define lof '())
(define (put op tag func)
  (set! lof (cons (list op tag func) lof)))
(define (get op tag)
  (define (iter l)
    (cond
      ((null? l) #f)
      ((and (equal? (car (car l)) op)
            (equal? (cadr (car l)) tag))
       (caddr (car l)))
      (else (iter (cdr l)))))
  (iter lof))

; integer-package
(define (install-integer-package)
  (define (tag x)
    (attach-tag 'integer x))
  (put 'add '(integer integer)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(integer integer)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(integer integer)
       (lambda (x y) (tag (* x y))))
  (put 'div '(integer integer)
       (lambda (x y) (tag (/ x y))))
  (put 'neg '(integer)
       (lambda (x) (tag (- x))))
  (put 'equ? '(integer integer) =)
  (put '=zero? '(integer)
       (lambda (x) (= x 0)))
  (put 'raise '(integer)
       (lambda (x) (make-rational x 1)))
  (put 'level '(integer) (lambda (x) 1))
  ; no project, the lowest level
  (put 'make 'integer
       (lambda (x)
         (cond
           ((exact-integer? x) (tag x))
           ((integer? x) (tag (inexact->exact x)))
           (else "expect an integer, given " x))))
  'done)

; rational-package
(define (install-rational-package)
  ;; internal procedures
  (define level-rational (+ 1 (level (make-integer 1))))
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  
  (define (equ? x y) 
    (= (* (numer x) (denom y)) (* (numer y) (denom x))))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'neg '(rational)
       (lambda (x) (tag (make-rational (- (numer x))
                                       (denom x)))))
  (put 'equ? '(rational rational) equ?)
  (put '=zero? '(rational)
       (lambda (x) (= (numer x) 0)))
  (put 'raise '(rational)
       (lambda (x) (make-real (/ (numer x) (denom x)))))
  (put 'level '(rational)
       (lambda (x) level-rational))
  (put 'project '(rational)
       (lambda (x) (make-integer (quotient (numer x) (denom x)))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d)))) ; check params
  'done)

; real-package
(define (install-real-package)
  (define level-real (+ 1 (level (make-rational 1 2))))
  (define (tag x)
    (attach-tag 'real x))
  (put 'add '(real real)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(real real)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(real real)
       (lambda (x y) (tag (* x y))))
  (put 'div '(real real)
       (lambda (x y) (tag (/ x y))))
  (put 'neg '(real)
       (lambda (x) (tag (- x))))
  (put 'equ? '(real real) =)
  (put '=zero? '(real)
       (lambda (x) (= x 0)))
  (put 'raise '(real)
       (lambda (x) (make-complex-from-real-imag x 0)))
  (put 'level '(real)
       (lambda (x) level-real))
  (put 'project '(real)
       (lambda (x) (make-rational (inexact->exact (numerator x))
                                  (inexact->exact (denominator x)))))
  (put 'make 'real
       (lambda (x) (tag x))) ; check param
  'done)


; complex-package
(define (install-complex-package)
  ;; imported procedures from rectangular 
  ;; and polar packages
  (define (real-part z)
    ((get 'real-part (list (type-tag z)))
     (contents z)))
  (define (imag-part z)
    ((get 'imag-part (list (type-tag z)))
     (contents z)))
  (define (magnitude z)
    ((get 'magnitude (list (type-tag z)))
     (contents z)))
  (define (angle z)
    ((get 'angle (list (type-tag z)))
     (contents z)))
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 
          'rectangular) 
     x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) 
     r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag 
     (+ (real-part z1) (real-part z2))
     (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag 
     (- (real-part z1) (real-part z2))
     (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang
     (* (magnitude z1) (magnitude z2))
     (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang 
     (/ (magnitude z1) (magnitude z2))
     (- (angle z1) (angle z2))))
  (define (equ? x y) 
    (and (= (real-part x) (real-part y)) (= (imag-part x) (imag-part y))))
  (define level-complex (+ 1 (level 1.0)))
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  (put 'add '(complex complex)
       (lambda (z1 z2) 
         (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) 
         (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) 
         (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) 
         (tag (div-complex z1 z2))))
  (put 'neg '(complex)
       (lambda (x) (tag (neg x))))  ; pass to lower level
  (put 'equ? '(complex complex) equ?)
  (put '=zero? '(complex)
       (lambda (x) (= (magnitude x) 0)))
  (put 'raise '(complex)
       (lambda (x) (make-sparse-polynomial 'any (list (list 0 (tag x))))))
  (put 'level '(complex)
       (lambda (x) level-complex))
  (put 'project '(complex)
       (lambda (x) (make-real (real-part x))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) 
         (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) 
         (tag (make-from-mag-ang r a))))
  'done)


(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (magnitude z)
    (sqrt (+ (sqr (real-part z))
             (sqr (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-real-imag x y) 
    (cons x y))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  ;; interface to the rest of the system
  (define (tag x) 
    (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part) ; 使用'(rectangular)而非'rectangular,允许多个参数操作的可能性,不是所有参数都是相同类型
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'neg '(rectangular)
       (lambda (x) (tag (make-from-real-imag
                         (- (real-part x))
                         (- (imag-part x))))))
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) 
         (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) 
         (tag (make-from-mag-ang r a))))
  'done)

; 极坐标的实现也是类似
(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (sqr x) (sqr y)))
          (atan y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'neg '(polar)
       (lambda (x) (tag (make-from-mag-ang
                         (magnitude x)
                         (- (angle x))))))
  (put 'make-from-real-imag 'polar
       (lambda (x y) 
         (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) 
         (tag (make-from-mag-ang r a))))
  'done)

; sparse-term-list package
; ((order coeff) (order coeff) ..)
(define (install-sparse-term-package)
  ;; internal procedures
  (define (tag p) (attach-tag 'sparse p))
  (define (make-term order coeff) 
    (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))
  (define (the-empty-terms) '())
  (define (empty-terms? term-list) 
    (null? term-list))
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))
  (define (dense2sparse terms)
    (cond
      ((null? terms) '())
      ((=zero? (car terms)) (dense2sparse (cdr terms)))
      (else (cons (list (- (length terms) 1) (car terms))
                  (dense2sparse (cdr terms))))))
  
  (define (add-terms-sparse L1 L2)
    (cond ((empty-terms? L1) L2)
          ((empty-terms? L2) L1)
          (else
           (let ((t1 (first-term L1)) 
                 (t2 (first-term L2)))
             (cond ((> (order t1) (order t2))
                    (adjoin-term
                     t1 
                     (add-terms-sparse (rest-terms L1) 
                                       L2)))
                   ((< (order t1) (order t2))
                    (adjoin-term
                     t2 
                     (add-terms-sparse 
                      L1 
                      (rest-terms L2))))
                   (else
                    (adjoin-term
                     (make-term 
                      (order t1)
                      (add (coeff t1) ; support nested polynomials (还需要支持类型提升)
                           (coeff t2)))
                     (add-terms-sparse 
                      (rest-terms L1)
                      (rest-terms L2)))))))))
  (define (mul-terms-sparse L1 L2)
    (if (empty-terms? L1)
        (the-empty-terms)
        (add-terms-sparse 
         (mul-term-by-all-terms 
          (first-term L1) L2)
         (mul-terms-sparse (rest-terms L1) L2))))
  (define (mul-term-by-all-terms t1 L)
    (if (empty-terms? L)
        (the-empty-terms)
        (let ((t2 (first-term L)))
          (adjoin-term
           (make-term 
            (+ (order t1) (order t2))
            (mul (coeff t1) (coeff t2)))
           (mul-term-by-all-terms 
            t1 
            (rest-terms L))))))
  (define (neg-term term)
    (make-term (order term) (neg (coeff term))))
  (define (neg-terms-sparse terms)
    (cond
      ((empty-terms? terms) (the-empty-terms))
      (else (adjoin-term (neg-term (first-term terms))
                         (neg-terms-sparse (rest-terms terms))))))
  (define (zero-terms-sparse? term-list)
    (cond
      ((empty-terms? term-list) #t)
      ((=zero? (coeff (first-term term-list)))
       (zero-terms-sparse? (rest-terms term-list)))
      (else #f)))
  (define (order0-term-coeff-sparse terms)
    (cond
      ((empty-terms? terms) 0)
      ((= 0 (order (first-term terms))) (coeff (first-term terms)))
      (else (order0-term-coeff-sparse (rest-terms terms)))))
  (define (equ-terms-sparse? l1 l2)
    (cond
      ((and (empty-terms? l1) (empty-terms? l2)) #t)
      ((or (empty-terms? l1) (empty-terms? l2)) #f)
      (else
       (let ((t1 (first-term l1))
             (t2 (first-term l2)))
         (if (or (not (= (order t1) (order t2)))
                 (not (equ? (coeff t1) (coeff t2))))
             #f
             (equ-terms-sparse? (rest-terms l1) (rest-terms l2)))))))
  (define (div-terms-sparse L1 L2)
    (if (empty-terms? L1)
        (list (the-empty-terms) 
              (the-empty-terms))
        (let ((t1 (first-term L1))
              (t2 (first-term L2)))
          (if (> (order t2) (order t1))
              (list (the-empty-terms) L1)
              (let* ((new-c (div (coeff t1) 
                                 (coeff t2)))
                     (new-o (- (order t1) 
                               (order t2)))
                     (new-term (make-term new-o new-c)))
                (let ((rest-of-result
                       (div-terms-sparse (add-terms-sparse L1 (neg-terms-sparse (mul-terms-sparse L2 (adjoin-term new-term (the-empty-terms))))) L2)))
                  (list (adjoin-term new-term (car rest-of-result))
                        (cadr rest-of-result))))))))
  
  ;; interface to the rest of the system
  (put 'add-terms '(sparse sparse)
       (lambda (l1 l2)
         (tag (add-terms-sparse l1 l2))))
  (put 'mul-terms '(sparse sparse)
       (lambda (l1 l2)
         (tag (mul-terms-sparse l1 l2))))
  (put 'neg-terms '(sparse)
       (lambda (l)
         (tag (neg-terms-sparse l))))
  (put 'div-terms '(sparse sparse)
       (lambda (l1 l2)
         (let ((res (div-terms-sparse l1 l2)))
           (list (tag (car res))
                 (tag (cadr res))))))
  (put 'zero-terms? '(sparse) zero-terms-sparse?)
  (put 'order0-term-coeff '(sparse) order0-term-coeff-sparse)
  (put 'equ-terms? '(sparse sparse) equ-terms-sparse?)
  (put 'level '(sparse) (lambda (x) 1))
  (put 'transform '(sparse sparse) (lambda (f t) (tag f)))
  (put 'transform '(dense sparse) (lambda (f t) (tag (dense2sparse f))))
  (put 'make-terms 'sparse
       (lambda (x) (tag x)))
  'done
  )

; dense-term-list package
; (coeff 0 coeff)
(define (install-dense-term-package)
  ;; internal procedures
  (define (tag p) (attach-tag 'dense p))
  (define (make-term order coeff) 
    (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))
  (define (the-empty-terms) '())
  (define (remove-prefix0 l)
    (cond
      ((null? l) (the-empty-terms))
      ((=zero? (car l)) (remove-prefix0 (cdr l)))
      (else l)))
  (define (empty-terms? l)
    (let ((nl (remove-prefix0 l)))
      (cond
        ((null? nl) #t)
        (else #f))))
  ; first non-zero term
  (define (first-term l)
    (let ((nl (remove-prefix0 l)))
      (make-term
       (- (length nl) 1)
       (car nl))))
  (define (rest-terms l)
    (let ((nl (remove-prefix0 l)))
      (remove-prefix0 (cdr nl))))

  (define (adjoin-term term term-list)
    (cond
      ((=zero? (coeff term)) term-list)
      ((= (order term) (length term-list)) (cons (coeff term) term-list))
      ((< (order term) (length term-list)) (display term-list) (error "order of term smaller than that of term-list"))
      (else (adjoin-term term (cons 0 term-list)))))
  (define (sparse2dense terms)
    (define (cons0 n l)
      (cond
        ((<= n 0) l)
        (else (cons0 (sub1 n) (cons 0 l)))))
    (define (iter l last)
      (cond
        ((null? l) (cons0 last '()))
        (else
         (let* ((t (car l))
                (order (car t))
                (coeff (cadr t))
                (gap (- last order 1)))
           (cons0 gap (cons coeff (iter (cdr l) order)))))))
    (iter terms 0))
  
  (define (add-terms-dense L1 L2)
    (cond ((empty-terms? L1) L2)
          ((empty-terms? L2) L1)
          (else
           (let ((t1 (first-term L1)) 
                 (t2 (first-term L2)))
             (cond ((> (order t1) (order t2))
                    (adjoin-term
                     t1 
                     (add-terms-dense (rest-terms L1) 
                                      L2)))
                   ((< (order t1) (order t2))
                    (adjoin-term
                     t2 
                     (add-terms-dense 
                      L1 
                      (rest-terms L2))))
                   (else
                    (adjoin-term
                     (make-term 
                      (order t1)
                      (add (coeff t1) ; support nested polynomials (还需要支持类型提升)
                           (coeff t2)))
                     (add-terms-dense 
                      (rest-terms L1)
                      (rest-terms L2)))))))))
  (define (mul-terms-dense L1 L2)
    (if (empty-terms? L1)
        (the-empty-terms)
        (add-terms-dense 
         (mul-term-by-all-terms 
          (first-term L1) L2)
         (mul-terms-dense (rest-terms L1) L2))))
  (define (mul-term-by-all-terms t1 L)
    (if (empty-terms? L)
        (the-empty-terms)
        (let ((t2 (first-term L)))
          (adjoin-term
           (make-term 
            (+ (order t1) (order t2))
            (mul (coeff t1) (coeff t2)))
           (mul-term-by-all-terms 
            t1 
            (rest-terms L))))))
  (define (neg-term term)
    (make-term (order term) (neg (coeff term))))
  (define (neg-terms-dense terms)
    (cond
      ((empty-terms? terms) (the-empty-terms))
      (else (adjoin-term (neg-term (first-term terms))
                         (neg-terms-dense (rest-terms terms))))))
  (define (zero-terms-dense? term-list)
    (cond
      ((empty-terms? term-list) #t)
      ((=zero? (coeff (first-term term-list)))
       (zero-terms-dense? (rest-terms term-list)))
      (else #f)))
  (define (order0-term-coeff-dense terms)
    (cond
      ((empty-terms? terms) 0)
      ((= 0 (order (first-term terms))) (coeff (first-term terms)))
      (else (order0-term-coeff-dense (rest-terms terms)))))
  (define (equ-terms-dense? l1 l2)
    (cond
      ((and (empty-terms? l1) (empty-terms? l2)) #t)
      ((or (empty-terms? l1) (empty-terms? l2)) #f)
      (else
       (let ((t1 (first-term l1))
             (t2 (first-term l2)))
         (if (or (not (= (order t1) (order t2)))
                 (not (equ? (coeff t1) (coeff t2))))
             #f
             (equ-terms-dense? (rest-terms l1) (rest-terms l2)))))))
  (define (div-terms-dense L1 L2)
    (if (empty-terms? L1)
        (list (the-empty-terms) 
              (the-empty-terms))
        (let ((t1 (first-term L1))
              (t2 (first-term L2)))
          (if (> (order t2) (order t1))
              (list (the-empty-terms) L1)
              (let* ((new-c (div (coeff t1) 
                                 (coeff t2)))
                     (new-o (- (order t1) 
                               (order t2)))
                     (new-term (make-term new-o new-c)))
                (let ((rest-of-result
                       (div-terms-dense (add-terms-dense L1 (neg-terms-dense (mul-terms-dense L2 (adjoin-term new-term (the-empty-terms))))) L2)))
                  (list (adjoin-term new-term (car rest-of-result))
                        (cadr rest-of-result))))))))
  
  ;; interface to the rest of the system
  (put 'add-terms '(dense dense)
       (lambda (l1 l2)
         (tag (add-terms-dense l1 l2))))
  (put 'mul-terms '(dense dense)
       (lambda (l1 l2)
         (tag (mul-terms-dense l1 l2))))
  (put 'neg-terms '(dense)
       (lambda (l)
         (tag (neg-terms-dense l))))
  (put 'div-terms '(dense dense)
       (lambda (l1 l2)
         (let ((res (div-terms-dense l1 l2)))
           (list (tag (car res))
                 (tag (cadr res))))))
  (put 'zero-terms? '(dense) zero-terms-dense?)
  (put 'order0-term-coeff '(dense) order0-term-coeff-dense)
  (put 'equ-terms? '(dense dense) equ-terms-dense?)
  (put 'level '(dense) (lambda (x) 1))
  (put 'transform '(dense dense) (lambda (f t) (tag f)))
  (put 'transform '(sparse dense) (lambda (f t) (tag (sparse2dense f))))
  (put 'make-terms 'dense
       (lambda (x) (tag x)))
  'done
  )


; polynomials package
(define (install-polynomial-package)
  ;; dependency
  (install-sparse-term-package)
  (install-dense-term-package)
  (define (add-terms x y) (apply-generic 'add-terms x y))
  (define (mul-terms x y) (apply-generic 'mul-terms x y))
  (define (neg-terms x) (apply-generic 'neg-terms x))
  (define (div-terms x y) (apply-generic 'div-terms x y))
  (define (zero-terms? x) (apply-generic 'zero-terms? x))
  (define (order0-term-coeff x) (apply-generic 'order0-term-coeff x))
  (define (equ-terms? x y) (apply-generic 'equ-terms? x y))
  (define (make-terms-sparse x)
    ((get 'make-terms 'sparse) x))
  (define (make-terms-dense x)
    ((get 'make-terms 'dense) x))

  ;; internal procedures
  ;; representation of poly
  (define level-poly (+ 1 (level (make-complex-from-real-imag 1 0))))
  (define (make-poly-sparse variable term-list)
    (list variable (make-terms-sparse term-list)))
  (define (make-poly-dense variable term-list)
    (list variable (make-terms-dense term-list)))
  (define (make-poly variable term-list)
    (list variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cadr p))
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1)
         (variable? v2)
         (eq? v1 v2)))
  (define any-variable 'any)
  (define (process-any p1 p2)
    (cond
      ((same-variable? (variable p1) any-variable)
       (list (make-poly (variable p2) (term-list p1)) p2))
      ((same-variable? (variable p2) any-variable)
       (list p1 (make-poly (variable p1) (term-list p2))))
      (else (list p1 p2))))
  
  (define (add-poly p1 p2)
    (let* ((new_polys (process-any p1 p2)) ; process the poly raised from complex
           (p1 (car new_polys))
           (p2 (cadr new_polys)))
      (if (same-variable? (variable p1) 
                          (variable p2))
          (make-poly 
           (variable p1)
           (add-terms (term-list p1)
                      (term-list p2)))
          (error "Polys not in same var: 
              ADD-POLY"
                 (list p1 p2)))))
  (define (mul-poly p1 p2)
    (let* ((new_polys (process-any p1 p2)) ; process the poly raised from complex
           (p1 (car new_polys))
           (p2 (cadr new_polys)))
      (if (same-variable? (variable p1) 
                          (variable p2))
          (make-poly 
           (variable p1)
           (mul-terms (term-list p1)
                      (term-list p2)))
          (error "Polys not in same var: 
              MUL-POLY"
                 (list p1 p2)))))
  (define (div-poly p1 p2)
    (let* ((new_polys (process-any p1 p2)) ; process the poly raised from complex
           (p1 (car new_polys))
           (p2 (cadr new_polys))
           (v1 (variable p1))
           (v2 (variable p2)))
      (if (same-variable? v1 v2)
          (let ((res (div-terms (term-list p1)
                                (term-list p2))))
            (list (make-poly v1 (car res))
                  (make-poly v1 (cadr res))))
          (error "Polys not in same var: 
              DIV-POLY"
                 (list p1 p2)))))
  (define (zero-poly? poly)
    (zero-terms? (term-list poly)))
  (define (project-poly poly)
    (order0-term-coeff (term-list poly)))
  (define (equ-poly? p1 p2)
    (let* ((polys_new (process-any p1 p2)) ; process the poly raised from complex
           (p1 (car polys_new))
           (p2 (cadr polys_new)))
      (if (same-variable? (variable p1) 
                          (variable p2))
          (equ-terms? (term-list p1) (term-list p2))
          #f)))
  (define (neg-poly p)
    (make-poly (variable p) (neg-terms (term-list p))))

  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) 
         (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) 
         (tag (mul-poly p1 p2))))
  (put 'neg '(polynomial)
       (lambda (p)
         (tag (neg-poly p))))
  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2)
         (tag (add-poly p1 (neg-poly p2)))))
  (put 'div '(polynomial polynomial)
       (lambda (p1 p2)
         (let ((res (div-poly p1 p2)))
           (list (tag (car res))
                 (tag (cadr res))))))
  (put '=zero? '(polynomial) zero-poly?)
  (put 'equ? '(polynomial polynomial) equ-poly?)
  (put 'raise '(polynomial)
       (lambda (x) (make-sparse-polynomial 'any (list (list 0 (tag x)))))) ; needed after being projected
  (put 'level '(polynomial)
       (lambda (x) level-poly))
  (put 'project '(polynomial)
       project-poly)
  (put 'make-poly-sparse 'polynomial
       (lambda (var terms) 
         (tag (make-poly-sparse var terms))))
  (put 'make-poly-dense 'polynomial
       (lambda (var terms) 
         (tag (make-poly-dense var terms))))
  'done)

; low level util
(define (attach-tag type-tag contents)
  (if (equal? type-tag 'real)
      contents
      (cons type-tag contents)))

(define (type-tag datum)
  (cond
    ((number? datum) 'real)
    ((pair? datum) (car datum))
    (else (error "Bad tagged datum: 
              TYPE-TAG" datum))))
   
(define (contents datum)
  (cond
    ((number? datum) datum)
    ((pair? datum) (cdr datum))
    (else (error "Bad tagged datum: 
              TYPE-TAG" datum))))


; integrate, keep the order
(define (init)
  (install-integer-package)
  (install-rational-package)
  (install-real-package)
  (install-rectangular-package)
  (install-polar-package)
  (install-complex-package)
  (install-polynomial-package)
  )

(init)
```

**Hierarchies of types in symbolic algebra**

Exercise 2.92: 通过强制变量顺序，扩展多项式包，使其可以对不同变量主导的多项式进行运算。（这不简单！）

不再需要any类型了，不同变量的poly也可以运算了。不同变量的多项式level不同，可以raise和project。如果变量是乱序需要支持排序归一化，这需要对多项式进行调整。通过遍历多项式，上面层的作为因子传入，直到叶子节点为止，将每一层相乘，所有叶子节点的计算结果相加。最终计算结果已经是根据变量优先级排序了的。

目前的数据导向编程都是根据所有参数的类型来自动选择对应的函数，但是有些函数只希望通过其中某些参数的类型来进行分配，所以修改apply-generic实现，增加第一个参数表示使用前面几个参数的类型来进行查表分配（如何表示所有参数？目前暂时用-1）。

normalize放到外面去做，在raise之前。不放外面，放外面

多项式除法返回两个结果，跟之前其他类型不统一？如何处理？

project-raise的化简方法不适用poly，其他系数也可以化简，为0的项可以省略。正常在递归计算的时候系数应该已经简化了。根本上，还是因为多项式这种递归结构没有进行递归化简。

```scheme
 (add (make-dense-polynomial 'x '(1 2 3)) 1)
 (add (make-dense-polynomial 'x '(1 2 3)) (make-dense-polynomial 'y '(1 0 1)))
 (mul (make-sparse-polynomial 'y '((1 1))) (make-dense-polynomial 'x '(1 2)))
 (normalize (make-dense-polynomial 'y (list (make-dense-polynomial 'x '(1 2)) 0)))
 (add (make-dense-polynomial 'x '(1 3)) (make-dense-polynomial 'y (list (make-dense-polynomial 'x '(1 2)) (make-dense-polynomial 'x '(1 0 1))))); 没有化简
 (mul (make-dense-polynomial 'x '(1 3)) (make-dense-polynomial 'y '(1 2)))
 (div (make-dense-polynomial 'x (list (make-dense-polynomial 'y (list 1 2)) (make-dense-polynomial 'y (list 3 6)))) (make-dense-polynomial 'y (list 1 2)))
 (equ? '(polynomial x (sparse)) '(polynomial x (sparse (0 0)))); 空和0
 (div (make-dense-polynomial 'x (list (make-dense-polynomial 'y (list 1 2)) (make-dense-polynomial 'y (list 3 7)))) (make-dense-polynomial 'y (list 1 2)))
```

完整代码

```scheme
#lang racket

(define (add x y) (apply-generic -1 'add x y))
(define (sub x y) (apply-generic -1 'sub x y))
(define (mul x y) (apply-generic -1 'mul x y))
(define (div x y) (apply-generic -1 'div x y))
(define (neg x) (apply-generic 1 'neg x))
(define (equ? x y) (apply-generic -1 'equ? x y))
(define (=zero? x) (apply-generic 1 '=zero? x))
(define (raise x) (apply-generic 1 'raise x))
(define (level x) (apply-generic 1 'level x))
(define (project x) (apply-generic 1 'project x))
(module+ test
  (require rackunit)
  (check-equal? (need-normalize? (make-dense-polynomial 'x '(1 2 3)) +inf.0) #f)
  (check-equal? (need-normalize? (make-dense-polynomial 'z '(1 2 3)) +inf.0) #f)
  (check-equal? (need-normalize? (make-dense-polynomial 'x `(1 ,(make-dense-polynomial 'y `(1 ,(make-dense-polynomial 'z '(1 1)))))) +inf.0) #f)
  (check-equal? (need-normalize? (make-dense-polynomial 'y (list (make-dense-polynomial 'x '(1 2)))) +inf.0) #t)
  (check-equal? (need-normalize? (make-dense-polynomial 'x `(1 ,(make-dense-polynomial 'z `(1 ,(make-dense-polynomial 'y '(1 1)))))) +inf.0) #t)

  (check-equal? (normalize (make-dense-polynomial 'x '(1 2 3))) '(polynomial x (dense 1 2 3)))
  )
(define (need-normalize? x y) (apply-generic 1 'need-normalize? x y))
(define (normalize x . y) (apply apply-generic 1 'normalize x y))
(define (simplify-leaf x) (apply-generic 1 'simplify-leaf x))

(define (simplifiable? op)
  (if (or (equal? op 'add)
          (equal? op 'sub)
          (equal? op 'mul)
          (equal? op 'div))
      #t
      #f))

(define (composite? x)
  (eq? (type-tag x) 'polynomial))

; user of the scheme-package
(define (make-integer n)
  ((get 'make 'integer) n))

(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (make-real n)
  ((get 'make 'real) n))

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(define (make-sparse-polynomial var terms)
  ((get 'make-poly-sparse 'polynomial) var terms))
(define (make-dense-polynomial var terms)
  ((get 'make-poly-dense 'polynomial) var terms))

(define (all-equal? a . l)
  (define (iter l)
    (cond
      ((null? l) #t)
      ((not (equal? a  (car l))) #f)
      (else (iter (cdr l)))))
  (iter l))

(module+ test
  (require rackunit)
  (check-equal? (raise-to-highest `(,(make-integer 1) ,(make-integer 1))) `(,(make-integer 1) ,(make-integer 1)))
  (check-equal? (raise-to-highest `(,(make-rational 1 2) ,(make-rational 1 4)))
                `(,(make-rational 1 2) ,(make-rational 1 4)))
  (check-equal? (raise-to-highest '(1.5 1.5)) '(1.5 1.5))
  (check-equal? (raise-to-highest `(,(make-complex-from-real-imag 1 1) ,(make-complex-from-real-imag 1 1)))
                `(,(make-complex-from-real-imag 1 1) ,(make-complex-from-real-imag 1 1)))
  (check-equal? (raise-to-highest `(,(make-complex-from-mag-ang 1 1) ,(make-complex-from-mag-ang 1 1)))
                `(,(make-complex-from-mag-ang 1 1) ,(make-complex-from-mag-ang 1 1)))
  (check-equal? (raise-to-highest `(,(make-integer 1) ,(make-rational 1 2))) `(,(make-rational 1 1) ,(make-rational 1 2)))
  (check-equal? (raise-to-highest `(,(make-integer 1) 1.5)) '(1 1.5))
  (check-equal? (raise-to-highest `(,(make-integer 1) ,(make-complex-from-real-imag 1 1)))
                `(,(make-complex-from-real-imag 1 0) ,(make-complex-from-real-imag 1 1)))
  (check-equal? (raise-to-highest `(,(make-rational 1 2) ,(make-complex-from-real-imag 1 1)))
                `(,(make-complex-from-real-imag 1/2 0) ,(make-complex-from-real-imag 1 1)))
  (check-equal? (raise-to-highest `(,(make-rational 1 2) 1.5)) `(1/2 1.5))
  (check-equal? (raise-to-highest `(1.0 ,(make-complex-from-real-imag 1 1)))
                `(,(make-complex-from-real-imag 1.0 0) ,(make-complex-from-real-imag 1 1)))
  (check-equal? (raise-to-highest '((sparse (2 1) (0 1)) (dense 1 0 1)))
                '((dense 1 0 1) (dense 1 0 1)))
  (check-equal? (raise-to-highest '((dense 1 0 1) (sparse (2 1) (0 1))))
                '((sparse (2 1) (0 1)) (sparse (2 1) (0 1))))
  )
; XXX add raise-able
(define (raise-to-highest l)
  (define (raise-to x h)
    (define (iter x)
      (cond
        ((eq? (level x) (level h))
         (let ((trans (get 'transform
                           (list (type-tag x) (type-tag h)))))
           (if trans (trans (contents x) (contents h)) x)))
        (else (iter (raise x)))))
    (iter x))
  (let ((highest (foldr (lambda (x high)
                          (cond
                            ((equal? high #f) x)
                            ((> (level x) (level high)) x)
                            (else high)))
                        #f ; lowest level
                        l)))
    (map (lambda (x)
           (raise-to x highest))
         l)))

(define (simplify x)
  (if (= 1 (level x))
      (if (composite? x) (simplify-leaf x) x)
      (let ((lower (project x)))
        (if (equ? (raise lower) x)
            (simplify lower)
            (if (composite? x) (simplify-leaf x) x)))))

(define (simplify-list l)
  (map simplify l))

(define (apply-generic n op . args)
  (let* ((type-args (if (= -1 n) args (take args n)))
         (normal-args (if (= -1 n) '() (drop args n)))
         (type-tags (map type-tag type-args))
         (proc (get op type-tags)))
    (if proc
        (let ((res (apply proc (append (map contents type-args) normal-args))))
          (cond
            ((not (simplifiable? op)) res)
            ((eq? (type-tag res) (type-tag (car args))) ; not elegant, should be more generic
             (simplify res))
            (else (simplify-list res))))
        (if (apply all-equal? type-tags)
            (error "No method for these types" (list op type-tags))
            (apply apply-generic n op (raise-to-highest args))))))

; get/put
(define lof '())
(define (put op tag func)
  (set! lof (cons (list op tag func) lof)))
(define (get op tag)
  (define (iter l)
    (cond
      ((null? l) #f)
      ((and (equal? (car (car l)) op)
            (equal? (cadr (car l)) tag))
       (caddr (car l)))
      (else (iter (cdr l)))))
  (iter lof))

; integer-package
(define (install-integer-package)
  (define (tag x)
    (attach-tag 'integer x))
  (put 'add '(integer integer)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(integer integer)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(integer integer)
       (lambda (x y) (tag (* x y))))
  (put 'div '(integer integer)
       (lambda (x y) (tag (/ x y))))
  (put 'neg '(integer)
       (lambda (x) (tag (- x))))
  (put 'equ? '(integer integer) =)
  (put '=zero? '(integer)
       (lambda (x) (= x 0)))
  (put 'raise '(integer)
       (lambda (x) (make-rational x 1)))
  (put 'level '(integer) (lambda (x) 1))
  ; no project, the lowest level
  (put 'make 'integer
       (lambda (x)
         (cond
           ((exact-integer? x) (tag x))
           ((integer? x) (tag (inexact->exact x)))
           (else "expect an integer, given " x))))
  'done)

; rational-package
(define (install-rational-package)
  ;; internal procedures
  (define level-rational (+ 1 (level (make-integer 1))))
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  
  (define (equ? x y) 
    (= (* (numer x) (denom y)) (* (numer y) (denom x))))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'neg '(rational)
       (lambda (x) (tag (make-rational (- (numer x))
                                       (denom x)))))
  (put 'equ? '(rational rational) equ?)
  (put '=zero? '(rational)
       (lambda (x) (= (numer x) 0)))
  (put 'raise '(rational)
       (lambda (x) (make-real (/ (numer x) (denom x)))))
  (put 'level '(rational)
       (lambda (x) level-rational))
  (put 'project '(rational)
       (lambda (x) (make-integer (quotient (numer x) (denom x)))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d)))) ; check params
  'done)

; real-package
(define (install-real-package)
  (define level-real (+ 1 (level (make-rational 1 2))))
  (define (tag x)
    (attach-tag 'real x))
  (put 'add '(real real)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(real real)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(real real)
       (lambda (x y) (tag (* x y))))
  (put 'div '(real real)
       (lambda (x y) (tag (/ x y))))
  (put 'neg '(real)
       (lambda (x) (tag (- x))))
  (put 'equ? '(real real) =)
  (put '=zero? '(real)
       (lambda (x) (= x 0)))
  (put 'raise '(real)
       (lambda (x) (make-complex-from-real-imag x 0)))
  (put 'level '(real)
       (lambda (x) level-real))
  (put 'project '(real)
       (lambda (x) (make-rational (inexact->exact (numerator x))
                                  (inexact->exact (denominator x)))))
  (put 'make 'real
       (lambda (x) (tag x))) ; check param
  'done)


; complex-package
(define (install-complex-package)
  ;; imported procedures from rectangular 
  ;; and polar packages
  (define (real-part z)
    ((get 'real-part (list (type-tag z)))
     (contents z)))
  (define (imag-part z)
    ((get 'imag-part (list (type-tag z)))
     (contents z)))
  (define (magnitude z)
    ((get 'magnitude (list (type-tag z)))
     (contents z)))
  (define (angle z)
    ((get 'angle (list (type-tag z)))
     (contents z)))
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 
          'rectangular) 
     x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) 
     r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag 
     (+ (real-part z1) (real-part z2))
     (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag 
     (- (real-part z1) (real-part z2))
     (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang
     (* (magnitude z1) (magnitude z2))
     (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang 
     (/ (magnitude z1) (magnitude z2))
     (- (angle z1) (angle z2))))
  (define (equ? x y) 
    (and (= (real-part x) (real-part y)) (= (imag-part x) (imag-part y))))
  (define level-complex (+ 1 (level 1.0)))
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  (put 'add '(complex complex)
       (lambda (z1 z2) 
         (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) 
         (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) 
         (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) 
         (tag (div-complex z1 z2))))
  (put 'neg '(complex)
       (lambda (x) (tag (neg x))))  ; pass to lower level
  (put 'equ? '(complex complex) equ?)
  (put '=zero? '(complex)
       (lambda (x) (= (magnitude x) 0)))
  (put 'raise '(complex)
       (lambda (x) (make-sparse-polynomial 'z (list (list 0 (tag x)))))) ; z: the lowest priority variable
  (put 'level '(complex)
       (lambda (x) level-complex))
  (put 'project '(complex)
       (lambda (x) (make-real (real-part x))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) 
         (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) 
         (tag (make-from-mag-ang r a))))
  'done)


(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (magnitude z)
    (sqrt (+ (sqr (real-part z))
             (sqr (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-real-imag x y) 
    (cons x y))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  ;; interface to the rest of the system
  (define (tag x) 
    (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part) ; 使用'(rectangular)而非'rectangular,允许多个参数操作的可能性,不是所有参数都是相同类型
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'neg '(rectangular)
       (lambda (x) (tag (make-from-real-imag
                         (- (real-part x))
                         (- (imag-part x))))))
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) 
         (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) 
         (tag (make-from-mag-ang r a))))
  'done)

; 极坐标的实现也是类似
(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (sqr x) (sqr y)))
          (atan y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'neg '(polar)
       (lambda (x) (tag (make-from-mag-ang
                         (magnitude x)
                         (- (angle x))))))
  (put 'make-from-real-imag 'polar
       (lambda (x y) 
         (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) 
         (tag (make-from-mag-ang r a))))
  'done)

;common procedues for term packages

; sparse-term-list package
; ((order coeff) (order coeff) ..)
(define (install-sparse-term-package)
  ;; internal procedures
  (define (tag p) (attach-tag 'sparse p))
  (define (make-term order coeff) 
    (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))
  (define (the-empty-terms) '())
  (define (remove-prefix0 l)
    (cond
      ((null? l) (the-empty-terms))
      ((=zero? (coeff (car l))) (remove-prefix0 (cdr l)))
      (else l)))
  (define (empty-terms? l)
    (let ((nl (remove-prefix0 l)))
      (cond
        ((null? nl) #t)
        (else #f))))
  ; first non-zero term
  (define (first-term l)
    (let ((nl (remove-prefix0 l)))
      (car nl)))
  (define (rest-terms l)
    (let ((nl (remove-prefix0 l)))
      (remove-prefix0 (cdr nl))))
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))
  (define (dense2sparse terms)
    (cond
      ((null? terms) '())
      ((=zero? (car terms)) (dense2sparse (cdr terms)))
      (else (cons (list (- (length terms) 1) (car terms))
                  (dense2sparse (cdr terms))))))
  
  (define (add-terms-sparse L1 L2)
    (cond ((empty-terms? L1) L2)
          ((empty-terms? L2) L1)
          (else
           (let ((t1 (first-term L1)) 
                 (t2 (first-term L2)))
             (cond ((> (order t1) (order t2))
                    (adjoin-term
                     t1 
                     (add-terms-sparse (rest-terms L1) 
                                       L2)))
                   ((< (order t1) (order t2))
                    (adjoin-term
                     t2 
                     (add-terms-sparse 
                      L1 
                      (rest-terms L2))))
                   (else
                    (adjoin-term
                     (make-term 
                      (order t1)
                      (add (coeff t1) ; support nested polynomials (还需要支持类型提升)
                           (coeff t2)))
                     (add-terms-sparse 
                      (rest-terms L1)
                      (rest-terms L2)))))))))
  (define (mul-terms-sparse L1 L2)
    (if (empty-terms? L1)
        (the-empty-terms)
        (add-terms-sparse 
         (mul-term-by-all-terms 
          (first-term L1) L2)
         (mul-terms-sparse (rest-terms L1) L2))))
  (define (mul-term-by-all-terms t1 L)
    (if (empty-terms? L)
        (the-empty-terms)
        (let ((t2 (first-term L)))
          (adjoin-term
           (make-term 
            (+ (order t1) (order t2))
            (mul (coeff t1) (coeff t2)))
           (mul-term-by-all-terms 
            t1 
            (rest-terms L))))))
  (define (neg-term term)
    (make-term (order term) (neg (coeff term))))
  (define (neg-terms-sparse terms)
    (cond
      ((empty-terms? terms) (the-empty-terms))
      (else (adjoin-term (neg-term (first-term terms))
                         (neg-terms-sparse (rest-terms terms))))))
  (define (zero-terms-sparse? term-list)
    (cond
      ((empty-terms? term-list) #t)
      ((=zero? (coeff (first-term term-list)))
       (zero-terms-sparse? (rest-terms term-list)))
      (else #f)))
  (define (order0-term-coeff-sparse terms)
    (cond
      ((empty-terms? terms) 0)
      ((= 0 (order (first-term terms))) (coeff (first-term terms)))
      (else (order0-term-coeff-sparse (rest-terms terms)))))
  (define (equ-terms-sparse? l1 l2)
    (cond
      ((and (empty-terms? l1) (empty-terms? l2)) #t)
      ((or (empty-terms? l1) (empty-terms? l2)) #f)
      (else
       (let ((t1 (first-term l1))
             (t2 (first-term l2)))
         (if (or (not (= (order t1) (order t2)))
                 (not (equ? (coeff t1) (coeff t2))))
             #f
             (equ-terms-sparse? (rest-terms l1) (rest-terms l2)))))))
  (define (div-terms-sparse L1 L2)
    (if (empty-terms? L1)
        (list (the-empty-terms) 
              (the-empty-terms))
        (let ((t1 (first-term L1))
              (t2 (first-term L2)))
          (if (> (order t2) (order t1))
              (list (the-empty-terms) L1)
              (let* ((new-c (div (coeff t1) 
                                 (coeff t2)))
                     (new-c (if (pair? (type-tag new-c)) (car new-c) new-c));hack, div-poly return a list
                     (new-o (- (order t1) 
                               (order t2)))
                     (new-term (make-term new-o new-c)))
                (if (=zero? new-c)
                    (list (the-empty-terms) L1)
                    (let* ((new-o (- (order t1) 
                                     (order t2)))
                           (new-term (make-term new-o new-c))
                           (remain (add-terms-sparse L1 (neg-terms-sparse (mul-terms-sparse L2 (adjoin-term new-term (the-empty-terms))))))
                           (rest-of-result (div-terms-sparse remain L2)))
                      (list (adjoin-term new-term (car rest-of-result))
                            (cadr rest-of-result)))))))))
  (define (need-normalize-sparse? terms level)
    (cond
      ((empty-terms? terms) #f)
      ((eq? 'polynomial (type-tag (coeff (first-term terms))))
       (or (need-normalize? (coeff (first-term terms)) level)
           (need-normalize-sparse? (rest-terms terms) level)))
      (else (need-normalize-sparse? (rest-terms terms) level))))
  (define (normalize-sparse terms var)
    (define (iter terms sum)
      (cond
        ((empty-terms? terms) sum)
        (else
         (let* ((t (first-term terms))
                (o (order t))
                (c (coeff t))
                (factor (make-sparse-polynomial var (list (list o 1)))))
           (if (eq? 'polynomial (type-tag c))
               (iter (rest-terms terms) (mul factor (normalize c)))
               (iter (rest-terms terms) (mul factor c)))))))
    (iter terms 0))
  (define (simplify-leaf-sparse terms)
    (cond
      ((empty-terms? terms) (the-empty-terms))
      (else
       (let* ((term (first-term terms))
              (o (order term))
              (c (coeff term))
              (new-c (simplify c)))
         (if (=zero? new-c)
             (simplify-leaf-sparse (rest-terms terms))
             (adjoin-term (make-term o new-c)
                          (simplify-leaf-sparse (rest-terms terms))))))))
  
  ;; interface to the rest of the system
  (put 'add-terms '(sparse sparse)
       (lambda (l1 l2)
         (tag (add-terms-sparse l1 l2))))
  (put 'mul-terms '(sparse sparse)
       (lambda (l1 l2)
         (tag (mul-terms-sparse l1 l2))))
  (put 'neg-terms '(sparse)
       (lambda (l)
         (tag (neg-terms-sparse l))))
  (put 'div-terms '(sparse sparse)
       (lambda (l1 l2)
         (let ((res (div-terms-sparse l1 l2)))
           (list (tag (car res))
                 (tag (cadr res))))))
  (put 'zero-terms? '(sparse) zero-terms-sparse?)
  (put 'order0-term-coeff '(sparse) order0-term-coeff-sparse)
  (put 'equ-terms? '(sparse sparse) equ-terms-sparse?)
  (put 'level '(sparse) (lambda (x) 1))
  (put 'transform '(sparse sparse) (lambda (f t) (tag f)))
  (put 'transform '(dense sparse) (lambda (f t) (tag (dense2sparse f))))
  (put 'need-normalize? '(sparse) need-normalize-sparse?)
  (put 'normalize '(sparse)
       (lambda (term var)
         (normalize-sparse term var)))
  (put 'simplify-leaf '(sparse)
       (lambda (x)
         (tag (simplify-leaf-sparse x))))
  (put 'make-terms 'sparse
       (lambda (x) (tag x)))
  'done
  )

; dense-term-list package
; (coeff 0 coeff)
(define (install-dense-term-package)
  ;; internal procedures
  
  (define (tag p) (attach-tag 'dense p))
  (define (make-term order coeff) 
    (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))
  (define (the-empty-terms) '())
  (define (remove-prefix0 l)
    (cond
      ((null? l) (the-empty-terms))
      ((=zero? (car l)) (remove-prefix0 (cdr l)))
      (else l)))
  (define (empty-terms? l)
    (let ((nl (remove-prefix0 l)))
      (cond
        ((null? nl) #t)
        (else #f))))
  ; first non-zero term
  (define (first-term l)
    (let ((nl (remove-prefix0 l)))
      (make-term
       (- (length nl) 1)
       (car nl))))
  (define (rest-terms l)
    (let ((nl (remove-prefix0 l)))
      (remove-prefix0 (cdr nl))))

  (define (adjoin-term term term-list)
    (cond
      ((=zero? (coeff term)) term-list)
      ((= (order term) (length term-list)) (cons (coeff term) term-list))
      ((< (order term) (length term-list)) (error "order of term smaller than that of term-list"))
      (else (adjoin-term term (cons 0 term-list)))))
  (define (sparse2dense terms)
    (define (cons0 n l)
      (cond
        ((<= n 0) l)
        (else (cons0 (sub1 n) (cons 0 l)))))
    (define (iter l last)
      (cond
        ((null? l) (cons0 last '()))
        (else
         (let* ((t (car l))
                (order (car t))
                (coeff (cadr t))
                (gap (- last order 1)))
           (cons0 gap (cons coeff (iter (cdr l) order)))))))
    (iter terms 0))
  
  (define (add-terms-dense L1 L2)
    (cond ((empty-terms? L1) L2)
          ((empty-terms? L2) L1)
          (else
           (let ((t1 (first-term L1)) 
                 (t2 (first-term L2)))
             (cond ((> (order t1) (order t2))
                    (adjoin-term
                     t1 
                     (add-terms-dense (rest-terms L1) 
                                      L2)))
                   ((< (order t1) (order t2))
                    (adjoin-term
                     t2 
                     (add-terms-dense 
                      L1 
                      (rest-terms L2))))
                   (else
                    (adjoin-term
                     (make-term 
                      (order t1)
                      (add (coeff t1) ; support nested polynomials (还需要支持类型提升)
                           (coeff t2)))
                     (add-terms-dense 
                      (rest-terms L1)
                      (rest-terms L2)))))))))
  (define (mul-terms-dense L1 L2)
    (if (empty-terms? L1)
        (the-empty-terms)
        (add-terms-dense 
         (mul-term-by-all-terms 
          (first-term L1) L2)
         (mul-terms-dense (rest-terms L1) L2))))
  (define (mul-term-by-all-terms t1 L)
    (if (empty-terms? L)
        (the-empty-terms)
        (let ((t2 (first-term L)))
          (adjoin-term
           (make-term 
            (+ (order t1) (order t2))
            (mul (coeff t1) (coeff t2)))
           (mul-term-by-all-terms 
            t1 
            (rest-terms L))))))
  (define (neg-term term)
    (make-term (order term) (neg (coeff term))))
  (define (neg-terms-dense terms)
    (cond
      ((empty-terms? terms) (the-empty-terms))
      (else (adjoin-term (neg-term (first-term terms))
                         (neg-terms-dense (rest-terms terms))))))
  (define (zero-terms-dense? term-list)
    (cond
      ((empty-terms? term-list) #t)
      ((=zero? (coeff (first-term term-list)))
       (zero-terms-dense? (rest-terms term-list)))
      (else #f)))
  (define (order0-term-coeff-dense terms)
    (cond
      ((empty-terms? terms) 0)
      ((= 0 (order (first-term terms))) (coeff (first-term terms)))
      (else (order0-term-coeff-dense (rest-terms terms)))))
  (define (equ-terms-dense? l1 l2)
    (cond
      ((and (empty-terms? l1) (empty-terms? l2)) #t)
      ((or (empty-terms? l1) (empty-terms? l2)) #f)
      (else
       (let ((t1 (first-term l1))
             (t2 (first-term l2)))
         (if (or (not (= (order t1) (order t2)))
                 (not (equ? (coeff t1) (coeff t2))))
             #f
             (equ-terms-dense? (rest-terms l1) (rest-terms l2)))))))
  (define (div-terms-dense L1 L2)
    (if (empty-terms? L1)
        (list (the-empty-terms) 
              (the-empty-terms))
        (let ((t1 (first-term L1))
              (t2 (first-term L2)))
          (if (> (order t2) (order t1))
              (list (the-empty-terms) L1)
              (let* ((new-c (div (coeff t1) 
                                 (coeff t2)))
                     (new-c (if (pair? (type-tag new-c)) (car new-c) new-c));hack, div-poly return a list
                     (new-o (- (order t1) 
                               (order t2)))
                     (new-term (make-term new-o new-c)))
                (if (=zero? new-c)
                    (list (the-empty-terms) L1)
                    (let* ((new-o (- (order t1) 
                                     (order t2)))
                           (new-term (make-term new-o new-c))
                           (remain (add-terms-dense L1 (neg-terms-dense (mul-terms-dense L2 (adjoin-term new-term (the-empty-terms))))))
                           (rest-of-result (div-terms-dense remain L2)))
                      (list (adjoin-term new-term (car rest-of-result))
                            (cadr rest-of-result)))))))))
  (define (need-normalize-dense? terms level)
    (cond
      ((empty-terms? terms) #f)
      ((eq? 'polynomial (type-tag (coeff (first-term terms))))
       (or (need-normalize? (coeff (first-term terms)) level)
           (need-normalize-dense? (rest-terms terms) level)))
      (else (need-normalize-dense? (rest-terms terms) level))))
  (define (normalize-dense terms var)
    (define (iter terms sum)
      (cond
        ((empty-terms? terms) sum)
        (else
         (let* ((t (first-term terms))
                (o (order t))
                (c (coeff t))
                (factor (make-sparse-polynomial var (list (list o 1)))))
           (if (eq? 'polynomial (type-tag c))
               (iter (rest-terms terms) (add sum (mul factor (normalize c))))
               (iter (rest-terms terms) (add sum (mul factor c))))))))
    (iter terms 0))
  (define (simplify-leaf-dense terms)
    (cond
      ((empty-terms? terms) (the-empty-terms))
      (else
       (let* ((term (first-term terms))
              (o (order term))
              (c (coeff term))
              (new-c (simplify c)))
         (if (=zero? new-c)
             (simplify-leaf-dense (rest-terms terms))
             (adjoin-term (make-term o new-c)
                          (simplify-leaf-dense (rest-terms terms))))))))
  
  ;; interface to the rest of the system
  (put 'add-terms '(dense dense)
       (lambda (l1 l2)
         (tag (add-terms-dense l1 l2))))
  (put 'mul-terms '(dense dense)
       (lambda (l1 l2)
         (tag (mul-terms-dense l1 l2))))
  (put 'neg-terms '(dense)
       (lambda (l)
         (tag (neg-terms-dense l))))
  (put 'div-terms '(dense dense)
       (lambda (l1 l2)
         (let ((res (div-terms-dense l1 l2)))
           (list (tag (car res))
                 (tag (cadr res))))))
  (put 'zero-terms? '(dense) zero-terms-dense?)
  (put 'order0-term-coeff '(dense) order0-term-coeff-dense)
  (put 'equ-terms? '(dense dense) equ-terms-dense?)
  (put 'level '(dense) (lambda (x) 1))
  (put 'transform '(dense dense) (lambda (f t) (tag f)))
  (put 'transform '(sparse dense) (lambda (f t) (tag (sparse2dense f))))
  (put 'need-normalize? '(dense) need-normalize-dense?)
  (put 'normalize '(dense)
       (lambda (term var)
         (normalize-dense term var)))
  (put 'simplify-leaf '(dense)
       (lambda (x)
         (tag (simplify-leaf-dense x))))
  (put 'make-terms 'dense
       (lambda (x) (tag x)))
  'done
  )


; polynomials package
(define (install-polynomial-package)
  ;; dependency
  (install-sparse-term-package)
  (install-dense-term-package)
  (define (add-terms x y) (apply-generic -1 'add-terms x y))
  (define (mul-terms x y) (apply-generic -1 'mul-terms x y))
  (define (neg-terms x) (apply-generic 1 'neg-terms x))
  (define (div-terms x y) (apply-generic -1 'div-terms x y))
  (define (zero-terms? x) (apply-generic -1 'zero-terms? x))
  (define (order0-term-coeff x) (apply-generic 1 'order0-term-coeff x))
  (define (equ-terms? x y) (apply-generic -1 'equ-terms? x y))
  (define (make-terms-sparse x)
    ((get 'make-terms 'sparse) x))
  (define (make-terms-dense x)
    ((get 'make-terms 'dense) x))

  ;; internal procedures
  ;; representation of poly
  (define level-complex (level (make-complex-from-real-imag 1 0)))
  (define (make-poly-sparse variable term-list)
    (list variable (make-terms-sparse term-list)))
  (define (make-poly-dense variable term-list)
    (list variable (make-terms-dense term-list)))
  (define (make-poly variable term-list)
    (list variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cadr p))
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1)
         (variable? v2)
         (eq? v1 v2)))
  (define any-variable 'any)
  (define (process-any p1 p2)
    (cond
      ((same-variable? (variable p1) any-variable)
       (list (make-poly (variable p2) (term-list p1)) p2))
      ((same-variable? (variable p2) any-variable)
       (list p1 (make-poly (variable p1) (term-list p2))))
      (else (list p1 p2))))

  ; normalize
  ; TODO: auto sort by the char value
  (define (variable->level v)
    (cond
      ((eq? v 'x) (+ level-complex 3))
      ((eq? v 'y) (+ level-complex 2))
      ((eq? v 'z) (+ level-complex 1))
      (else (error "unknown variable " v))))
  (define (level->variable lvl)
    (cond
      ((= (+ level-complex 3) lvl) 'x)
      ((= (+ level-complex 2) lvl) 'y)
      ((= (+ level-complex 1) lvl) 'z)
      (else (error "not a level of polynomial"))))
  (define (level-poly x)
    (variable->level (variable x)))
  (define (need-normalize-poly? poly level)
    (let ((lvl (variable->level (variable poly))))
      (if (>= lvl level)
          #t
          (need-normalize? (term-list poly) lvl))))
  (define (normalize-poly poly)
    (if (need-normalize-poly? poly +inf.0)
        (let ((v (variable poly))
              (terms (term-list poly)))
          (contents (normalize terms v)))
        poly))
  (define (simplify-leaf-poly poly)
    (let ((v (variable poly))
          (terms (term-list poly)))
      (make-poly v (simplify-leaf terms))))
  
  (define (add-poly p1 p2)
    (let* ((p1 (normalize-poly p1))
           (p2 (normalize-poly p2))
           (v1 (variable p1))
           (v2 (variable p2)))
      (cond
        ((same-variable? v1 v2)
         (make-poly v1
                    (add-terms (term-list p1)
                               (term-list p2))))
        ((> (variable->level v1) (variable->level v2))
         (add-poly p1 (make-poly-sparse v1 (list (list 0 (tag p2))))))
        (else
         (add-poly (make-poly-sparse v2 (list (list 0 (tag p1)))) p2)))))
  (define (mul-poly p1 p2)
    (let* ((p1 (normalize-poly p1))
           (p2 (normalize-poly p2))
           (v1 (variable p1))
           (v2 (variable p2)))
      (cond
        ((same-variable? v1 v2)
         (make-poly 
          (variable p1)
          (mul-terms (term-list p1)
                     (term-list p2))))
        ((> (variable->level v1) (variable->level v2))
         (mul-poly p1 (make-poly-sparse v1 (list (list 0 (tag p2))))))
        (else
         (mul-poly (make-poly-sparse v2 (list (list 0 (tag p1)))) p2)))))
  (define (div-poly p1 p2)
    (let* ((p1 (normalize-poly p1))
           (p2 (normalize-poly p2))
           (v1 (variable p1))
           (v2 (variable p2)))
      (cond
        ((same-variable? v1 v2)
         (let ((res (div-terms (term-list p1)
                               (term-list p2))))
           (list (make-poly v1 (car res))
                 (make-poly v1 (cadr res)))))
        ((> (variable->level v1) (variable->level v2))
         (div-poly p1 (make-poly-sparse v1 (list (list 0 (tag p2))))))
        (else
         (div-poly (make-poly-sparse v2 (list (list 0 (tag p1)))) p2)))))
  (define (zero-poly? poly)
    (zero-terms? (term-list poly)))
  (define (project-poly poly)
    (order0-term-coeff (term-list poly)))
  (define (equ-poly? p1 p2)
    (let* ((p1 (normalize-poly p1))
           (p2 (normalize-poly p2))
           (v1 (variable p1))
           (v2 (variable p2)))
      (if (same-variable? v1 v2)
          (equ-terms? (term-list p1) (term-list p2))
          #f)))
  (define (neg-poly p)
    (make-poly (variable p) (neg-terms (term-list p))))

  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) 
         (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) 
         (tag (mul-poly p1 p2))))
  (put 'neg '(polynomial)
       (lambda (p)
         (tag (neg-poly p))))
  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2)
         (tag (add-poly p1 (neg-poly p2)))))
  (put 'div '(polynomial polynomial)
       (lambda (p1 p2)
         (let ((res (div-poly p1 p2)))
           (list (tag (car res))
                 (tag (cadr res))))))
  (put '=zero? '(polynomial) zero-poly?)
  (put 'equ? '(polynomial polynomial) equ-poly?)
  (put 'raise '(polynomial)
       (lambda (x)
         (let* ((lvl (level-poly x))
                (upvar (level->variable (add1 lvl))))
           (make-sparse-polynomial upvar (list (list 0 (tag x)))))))
  (put 'level '(polynomial) level-poly)
  (put 'project '(polynomial)
       project-poly)
  (put 'need-normalize? '(polynomial) need-normalize-poly?)
  (put 'normalize '(polynomial)
       (lambda (x)
         (tag (normalize-poly x))))
  (put 'simplify-leaf '(polynomial)
       (lambda (x)
         (tag (simplify-leaf-poly x))))
  (put 'make-poly-sparse 'polynomial
       (lambda (var terms) 
         (tag (make-poly-sparse var terms))))
  (put 'make-poly-dense 'polynomial
       (lambda (var terms) 
         (tag (make-poly-dense var terms))))
  'done)

; low level util
(define (attach-tag type-tag contents)
  (if (equal? type-tag 'real)
      contents
      (cons type-tag contents)))

(define (type-tag datum)
  (cond
    ((number? datum) 'real)
    ((pair? datum) (car datum))
    (else (error "Bad tagged datum: 
              TYPE-TAG" datum))))
   
(define (contents datum)
  (cond
    ((number? datum) datum)
    ((pair? datum) (cdr datum))
    (else (error "Bad tagged datum: 
              TYPE-TAG" datum))))


; integrate, keep the order
(define (init)
  (install-integer-package)
  (install-rational-package)
  (install-real-package)
  (install-rectangular-package)
  (install-polar-package)
  (install-complex-package)
  (install-polynomial-package)
  )

(init)
```

Exercise 2.93: 修改有理数算术包使用通用操作，但是需要修改make-rat使其不尝试简化。

加法乘法改成使用通用的add和mul，但是和原有系统有冲突。应该分成不同的系统，原子数字一个系统，复合类型是一个系统。

Exercise 2.94: 使用div-terms实现remainder-terms。然后写gcd-poly。

```scheme
 ;; add into polynomial package 
 (define (remainder-terms p1 p2) 
   (cadr (div-terms p1 p2))) 
  
 (define (gcd-terms a b) 
   (if (empty-termlist? b) 
     a 
     (gcd-terms b (remainder-terms a b)))) 
  
 (define (gcd-poly p1 p2) 
   (if (same-varaible? (variable p1) (variable p2)) 
         (make-poly (variable p1) 
                    (gcd-terms (term-list p1) 
                               (term-list p2)) 
         (error "not the same variable -- GCD-POLY" (list p1 p2))))) 
```

Exercise 2.95: 

Exercise 2.96: 被除数先乘以一个数，保证在除法过程中不会出现分数。因为在化简有理数这个情况下，公约数是要被分子分母同时除的，所以没有关系。

实现pseudoremainder-terms

```scheme
 ;; a 
 (define (pseudoremainder-terms a b) 
   (let* ((o1 (order (first-term a))) 
          (o2 (order (first-term b))) 
          (c (coeff (first-term b))) 
          (divident (mul-terms (make-term 0  
                                          (expt c (+ 1 (- o1 o2)))) 
                               a))) 
     (cadr (div-terms divident b)))) 
  
 (define (gcd-terms a b) 
   (if (empty-termlist? b) 
     a 
     (gcd-terms b (pseudoremainder-terms a b)))) 
  
 ;; b 
 (define (gcd-terms a b) 
   (if (empty-termlist? b) 
     (let* ((coeff-list (map cadr a)) 
            (gcd-coeff (apply gcd coeff-list))) 
       (div-terms a (make-term 0  gcd-coeff))) 
     (gcd-terms b (pseudoremainder-terms a b)))) 
```

Exercise 2.97: 1. 实现reduece-terms, reduce-poly。 2. 通用操作reduce

```scheme
 ;;a 
 (define (reduce-terms n d) 
   (let ((gcdterms (gcd-terms n d))) 
         (list (car (div-terms n gcdterms)) 
               (car (div-terms d gcdterms))))) 
  
 (define (reduce-poly p1 p2) 
   (if (same-variable? (variable p1) (variable p2)) 
     (let ((result (reduce-terms (term-list p1) (term-list p2)))) 
       (list (make-poly (variable p1) (car result)) 
             (make-poly (variable p1) (cadr result)))) 
     (error "not the same variable--REDUCE-POLY" (list p1 p2)))) 
 ;;b. skip this, I had done such work many times, I'm tired of it. 
```



# 3 Modularity, Objects, and State

两种著名的系统结构的组织策略。一种聚焦对象，将一个大系统视作不同**对象**的集合，对象的行为可以随着时间变化。另一种聚焦在系统中流动的**信息流**。

对于对象我们必须关注一个计算对象如何能改变但是维持其身份。当我们的计算模型中需要处理时间，尤其是允许并发时就变得更加困难。当我们将模型中仿真时间和评估期间发生的时间顺序解耦时，流式方法就得到了最充分的利用。我们将使用一种被称为delayed evaluation技术来实现这个。

## 3.1 Assignment and Local State

### Local State Variables

`(set! <name> <new-value>)  `

scheme的命名传统，修改变量值的操作的名字后面以感叹号结尾。

`(begin <exp1> <exp2> ... <expk>)`

评估多个表达式，最后一个的作为返回值

引入赋值之后，过程应用模型需要更新，支持闭包。

Exercise 3.1: 写make-accumulator用于生成累加器，其输入作为和的初始值

```scheme
(define (make-accumulator sum)
  (lambda (n)
    (begin (set! sum (+ sum n))
           sum)))
```

Exercise 3.2: 统计过程被调用的次数。写一个make-monitored接收一个过程f作为参数，过程f本身也接受一个输入。make-monitored返回的一个函数，比如叫mf，它用内部计数器追踪调用的次数。当输入参数为特殊符号how-many-calls?时返回次数。当输入参数为特殊符号reset-count时将计数器置零。对于其他所有值，返回f的调用结果并递增计数器。

```scheme
(define (make-monitored f)
  (let ((count 0))
    (lambda (x)
      (cond
       ((eq? x 'how-many-calls?) count)
       ((eq? x 'reset-count) (set! count 0))
       (else (begin (set! count (+ count 1))
                    (f x)))))))
```

Exercise 3.3: 修改make-account创建密码保护的账户，接受一个符号作为额外的参数

```scheme
(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance 
                     (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m p)
    (cond ((not (eq? p password)) "Incorrect password")
          ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request: 
                 MAKE-ACCOUNT" m))))
  dispatch)
```

Exercise 3.4: 修改make-account，连续7次密码错误，调用call-the-cops

```scheme
(define (make-account balance password)
  (define (call-the-cops) "Call the cops")
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance 
                     (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((count 0)
        (limit 7))
    (define (dispatch m p)
      (if (not (eq? p password))
          (begin (set! count (+ count 1))
                 (if (> count limit)
                     (call-the-cops)
                     "Incorrect password"))
          (begin (set! count 0)
                 (cond
                   ((eq? m 'withdraw) withdraw)
                   ((eq? m 'deposit) deposit)
                   (else (error "Unknown request: 
                 MAKE-ACCOUNT" m))))))
    dispatch))
```

### The Benefits of Introducing Assignment

用rand函数实现蒙特卡洛方法

```scheme
(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials 
                          cesaro-test))))
(define (cesaro-test)
   (= (gcd (rand) (rand)) 1))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) 
                 (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) 
                 trials-passed))))
  (iter trials 0))
```

引入了赋值以及将状态隐藏在局部变量中的技术，我们可以用更模块化的方式构建系统。

Exercise 3.5: 蒙特卡洛积分是用蒙特卡洛仿真估计定积分的方法。实现estimate-integral，接受一个谓词P，长方形的x1 x2 y1 y2以及尝试的次数

```scheme
(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (* range (random)))))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) 
                 (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) 
                 trials-passed))))
  (iter trials 0))

(define (in-circle? x y)
  (let ((xl (sqr (- x 5)))
        (yl (sqr (- y 7))))
    (<= (+ xl yl) 9)))

(define (estimate-integral P x1 x2 y1 y2 trial)
  (let ((size (* (- x2 x1) (- y2 y1)))
        (probability
         (monte-carlo trial
                      (lambda ()
                        (let ((x (random-in-range x1 x2))
                              (y (random-in-range y1 y2)))
                          (P x y))))))
    (/ (* probability size))))

(define (estimate-pi trial)
  (/ (estimate-integral in-circle? 2 8 4 10 trial) 9))
```

Exercise 3.6: 设计新的rand过程接受一个参数指定生成新随机数还是重置为新值

```scheme
(define (rand-update x) (+ x 1)) ; A not-very-evolved PNRG 
; x: init-value, update: get-next-value
(define (make-rand x update)
  (lambda (sym)
    (cond
     ((eq? sym 'generate)
      (set! x (update x))
      x)
     ((eq? sym 'reset)
      (lambda (new)
        (set! x new)))
     (else (error "unknown symbol" sym)))))
(define rand1 (make-rand 0 rand-update))
(define rand2 (make-rand 0 rand-update))
```



### The Costs of Introducing Assignment

前两章没有赋值的编程称为函数式编程functional programming。原先变量只是个符号，是值的名字。引入set!使变量的值可变之后，变量就变成引用一个存储值的地方。

**Sameness and change**

支持在表达式中“等于可以被等于替换”而不改变表达式的值的语言，被称为引用透明referentially transparent。而set!的加入违背了引用透明。

**Pitfalls of imperative programming**

与函数式编程相对的，大量使用赋值的编程被叫做命令式编程imperative programming。命令式编程不仅增加了计算模型的复杂度，写命令式程序也更易出现bug。

以阶乘为例子，命令式中赋值顺序一换结果就不对了。

```scheme
(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product)
              (+ counter 1))))
  (iter 1 1))

(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product)
              (+ counter 1))))
  (iter 1 1))
```

如果是多进程并发执行，命令式编程的复杂度将变得更大。

Exercise 3.7: 考虑练习3.3中带密码保护的make-account，定义个make-joint，第一个参数是密码保护的账户，第二个是原始密码，第三个是关联账户的新密码

```scheme
(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance 
                     (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m p)
    (cond ((not (eq? p password)) "Incorrect password")
          ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request: 
                 MAKE-ACCOUNT" m))))
  dispatch)

(define (make-joint acc pw npw)
  (define (dispatch m p)
    (cond
      ((not (eq? p npw)) "Incorrect password")
      (else (acc m pw))))
  (if (equal? (acc 'withdraw pw) "Incorrect password")
      "Incorrect password"
      dispatch))

(define peter-acc (make-account 100 'peter))
(define paul-acc (make-joint peter-acc 'peter 'paul))
```

Exercise 3.8: 1.1.3节中当我们定义评估模型时，我们没有提到子表达式的评估顺序问题。当我们引入赋值之后，参数的评估顺序会造成结果的不同。定义一个简单的过程f，对于如下表达式，如果参数从左往右评估则返回0，反之返回1.

```scheme
;(+ (f 0) (f 1))
(define f
  (let ((a #f))
    (lambda (x)
      (if a
          a
          (begin (set! a (/ x 2)) a)))))
```



## 3.2 The Environment Model of Evaluation

在我们引入赋值之后，1.1.5的评估模型已经不充分了。新模型中，这些存放变量值的地方被维护在叫做环境**environments**的结构体中.

一个环境是一个帧**frames**的序列。每个帧是一个**bindings**的表，关联变量名和其对应的值。每个帧都有一个指向**enclosing environment**的指针，除了该帧被认为是**global**的。一个变量关于一个环境的值是该环境中第一个包含该变量binding的帧中的值。如果没有任何帧绑定了该变量，那么就说这个变量在环境中是unbound的。

后面的binding可以shadow前面的binding。所以表达式只在评估的环境中有意义。

### The Rules for Evaluation

评估一个combination，先评估子表达式，然后将操作符子表达式的值应用到操作数子表达式的值。

在评估环境模型中，一个程序总是由代码和一个环境的指针组成。过程由评估一个λ-expression来创建。程序的代码来自λ-expression的文本，程序的环境来自λ-expression被评估的环境。

一般来说，**define**通过将bindings添加到帧中来创建定义。

而程序被应用的步骤是这样的：创建一个新环境包含一个帧将形参绑定到实参上。该帧的enclosing environment是由该程序指定的环境。然后在新环境中，评估程序的body。

define在当前帧创建新binding，set!更新变量binding的值。

### Applying Simple Procedures

### Frames as the Repository of Local State

### Internal Definitions

## 3.3 Modeling with Mutable Data

第二章的数据抽象，数据结构由构造函数和选择器指定。想要建模对象可变的系统，需要修改数据对象的能力，我们叫这些操作**mutators**。

### Mutable List Structure

对于pairs的基础mutators是set-car!和set-cdr!

Exercise 3.12:

下面两种append有一定的区别，append是生成一个新list，原先的x不受影响。append!直接在x后面接上。

```scheme
(define (append x y)
  (if (null? x)
      y
      (cons (car x) (append (cdr x) y))))

(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))
```

Exercise 3.13: make-cycle生成环形链表

```scheme
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)
```

Exercise 3.14: 下面的程序很有用，但是不太好懂

```scheme
(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))
```

对链表进行反序。

**Sharing and identity**

考虑下面两种情况，如果只用cons, car, cdr操作list，那么两者看起来是一样的，共享完全探测不到。但是如果我们允许mutator，共享就变得重要的。

```scheme
(define x (list 'a 'b))
(define z1 (cons x x))

(define z2 
  (cons (list 'a 'b) (list 'a 'b)))
```

探测list结构是否共享的一种方法是使用`eq?`，它测试两个符号是否相等，更一般地说，测试两者是否是同一个对象，即指针是否相等。

Exercise 3.16: 数pair的个数，简单像下面程序一样做是有问题的。画出3个pair可能返回3、4、7以及不返回的情况。

```scheme
(require rnrs/mutable-pairs-6)
(require compatibility/mlist)

(define cons mcons)
(define car mcar)
(define cdr mcdr)
(define list mlist)
(define pair? mpair?)
(define set-car! set-mcar!)
(define set-cdr! set-mcdr!)
(define memq mmemq)
(define length mlength)

(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

(define (make-cycle x)
  (set-mcdr! (last-pair x) x)
  x)
(define (last-pair x)
  (if (null? (mcdr x))
      x
      (last-pair (mcdr x))))

(define x1 (list 1 2 3)) ; 3
(define a (cons 1 2))
(define b (cons a 3))
(define x2 (cons a b))   ; 4
(define c (cons 1 2))
(define d (cons c c))
(define x3 (cons d d))   ; 7
(define e (mlist 1 2 3))
(define x4 (make-cycle e)) ; no result
```

Exercise 3.17: 设计正确的count-pairs

```scheme
(define (count-pairs x)
  (define (iter x seen)
    (if (or (not (pair? x))
            (memq x seen))
        seen
        (iter (cdr x) (iter (car x) (cons x seen)))))
  (length (iter x '())))
```

Exercise 3.18: 写一个程序探测一个list是否为环。(不是嵌套的结构不用迭代car)

```scheme
(define (cycle? x)
  (define (iter x seen)
    (cond
     ((not (pair? x)) #f)
     ((memq x seen) #t)
     (else (or (iter (car x) (cons x seen))
               (iter (cdr x) (cons x seen))))))
  (iter x '()))
```

Exercise 3.19: 重做上一题，使用一个只使用常量空间的算法（需要一个非常聪明的想法）

```scheme
; 双指针法
(define (cycle? x)
  (define (iter x y)
    (cond
     ((not (pair? y)) #f)
     ((eq? x y) #t)
     ((not (pair? (cdr y))) #f)
     (else (iter (cdr x) (cdr (cdr y))))))
  (if (pair? x)
      (iter x (cdr x))
      #f))
; 如果是棵树呢？
; 因为左右子树都得递归，没法尾递归
; 需要记录领先指针的路径，后面的指针走相同的路。
; 因为一定要回到自己走过的地方还是环
```



**Mutation is just assignment**

2.1.3中我们已经看到了pairs可以纯用程序来表现

```scheme
define (cons x y)
  (define (dispatch m)
    (cond ((eq? m 'car) x)
          ((eq? m 'cdr) y)
          (else (error "Undefined 
                 operation: CONS" m))))
  dispatch)

(define (car z) (z 'car))
(define (cdr z) (z 'cdr))
```

对于可变数据还是如此。

```scheme
(define (cons x y)
  (define (set-x! v) (set! x v))
  (define (set-y! v) (set! y v))
  (define (dispatch m)
    (cond ((eq? m 'car) x)
          ((eq? m 'cdr) y)
          ((eq? m 'set-car!) set-x!)
          ((eq? m 'set-cdr!) set-y!)
          (else (error "Undefined 
                 operation: CONS" m))))
  dispatch)

(define (car z) (z 'car))
(define (cdr z) (z 'cdr))

(define (set-car! z new-value)
  ((z 'set-car!) new-value)
  z)

(define (set-cdr! z new-value)
  ((z 'set-cdr!) new-value)
  z)
```



### Representing Queues

我们用首尾两个指针来表示队列，两个指针本身用一个pair表示。

```scheme
; data abstract
(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) 
  (set-car! queue item))
(define (set-rear-ptr! queue item) 
  (set-cdr! queue item))

; constructor
(define (make-queue) (cons '() '()))
; selector
(define (empty-queue? queue) 
  (null? (front-ptr queue)))
(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an 
              empty queue" queue)
      (car (front-ptr queue))))
; mutator
(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else (set-cdr! (rear-ptr queue) 
                          new-pair)
                (set-rear-ptr! queue new-pair)
                queue))))
(define (delete-queue! queue)
  (cond
   ((empty-queue? queue)
    (error "DELETE called with an 
            empty queue" queue))
   ((eq? (front-ptr queue) (rear-ptr queue))
    (set-front-ptr! queue '())
    (set-rear-ptr! queue '())
    queue)
   (else
    (set-front-ptr! queue (cdr (front-ptr queue)))
    queue)))
```

Exercise 3.21: 实现print-queue打印queue中的项目

```scheme
(define (print-queue queue)
  (let ((start (front-ptr queue)))
    (define (iter cur)
      (if (null? cur)
          (newline)
          (begin (display (car cur))
                 (display " ")
                 (iter (cdr cur)))))
    (iter start)))

(define (print-queue queue)
  (display (front-ptr queue))
  (newline))
```

Exercise 3.22: 我们可以将队列构建为带局部变量的程序

```scheme
(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (empty-queue?)
      (null? front-ptr))
    (define (front-queue)
      (if (empty-queue?)
          (error "FRONT called with an 
              empty queue")
          (car front-ptr)))
    (define (insert-queue! item)
      (let ((new-pair (cons item '())))
        (cond
         ((empty-queue?)
          (set! front-ptr new-pair)
          (set! rear-ptr new-pair))
         (else
          (set-cdr! rear-ptr new-pair)
          (set! rear-ptr new-pair)))))
    (define (delete-queue!)
      (cond
       ((empty-queue?)
        (error "DELETE called with an 
            empty queue"))
       ((eq? front-ptr rear-ptr)
        (set! front-ptr '())
        (set! rear-ptr '()))
       (else (set! front-ptr (cdr front-ptr)))))
    (define (print-queue) front-ptr)
    (define (dispatch m)
      (cond
       ((eq? m 'empty-queue?) empty-queue?)
       ((eq? m 'front-queue) front-queue)
       ((eq? m 'insert-queue!) insert-queue!)
       ((eq? m 'delete-queue!) delete-queue!)
       ((eq? m 'print-queue) print-queue)))
    dispatch))
```

Exercise 3.23: 展示如何使用pairs表示deques，并给出操作的实现。所以操作都应该是O(1)时间复杂度。

```scheme
; data abstract
(define (front-ptr deque) (car deque))
(define (rear-ptr deque) (cdr deque))
(define (set-front-ptr! deque item) 
  (set-car! deque item))
(define (set-rear-ptr! deque item) 
  (set-cdr! deque item))
(define (value item)
  (car item))
(define (prev item)
  (car (cdr item)))
(define (next item)
  (cdr (cdr item)))
(define (set-prev! item p)
  (set-car! (cdr item) p))
(define (set-next! item n)
  (set-cdr! (cdr item) n))
(define (new-item val)
  (cons val (cons '() '())))

; constructor
(define (make-deque) (cons '() '()))
; selector
(define (empty-deque? deque) 
  (null? (front-ptr deque)))
(define (front-deque deque)
  (if (empty-deque? deque)
      (error "FRONT called with an 
              empty deque" deque)
      (value (front-ptr deque))))
(define (rear-deque deque)
  (if (empty-deque? deque)
      (error "REAR called with an 
              empty deque" deque)
      (value (rear-ptr deque))))
; mutator
(define (front-insert-deque! deque val)
  (let ((item (new-item val)))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque item)
           (set-rear-ptr! deque item)
           deque)
          (else (set-next! item (front-ptr deque))
                (set-prev! (front-ptr deque) item)
                (set-front-ptr! deque item)
                deque))))
(define (rear-insert-deque! deque val)
  (let ((item (new-item val)))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque item)
           (set-rear-ptr! deque item)
           deque)
          (else (set-next! (rear-ptr deque) item)
                (set-prev! item (rear-ptr deque))
                (set-rear-ptr! deque item)
                deque))))
(define (front-delete-deque! deque)
  (cond
   ((empty-deque? deque)
    (error "DELETE called with an 
            empty deque" deque))
   ((eq? (front-ptr deque) (rear-ptr deque))
    (set-front-ptr! deque '())
    (set-rear-ptr! deque '())
    deque)
   (else
    (set-front-ptr! deque (next (front-ptr deque)))
    ;(set-prev! (front-ptr deque) '())
    deque)))
(define (rear-delete-deque! deque)
  (cond
   ((empty-deque? deque)
    (error "DELETE called with an 
            empty deque" deque))
   ((eq? (front-ptr deque) (rear-ptr deque))
    (set-front-ptr! deque '())
    (set-rear-ptr! deque '())
    deque)
   (else
    (set-rear-ptr! deque (prev (rear-ptr deque)))
    ;(set-nex! (rear-ptr deque) '())
    deque)))
```

### Representing Tables

将table实现为一个记录的list，每个记录实现为一个键值对，一个骨架list将所有记录串起来。

 ```scheme
(define (record-key record)
  (car record))
(define (record-value record)
  (cdr record))
(define (new-record key value)
  (cons key value))
(define (set-record! record value)
  (set-cdr! record value))
(define (empty-records? records)
  (null? records))
(define (empty-records)
  '())
(define (first-record records)
  (car records))
(define (rest-records records)
  (cdr records))
(define (insert-record! records record)
  (cons record records))
(define (table-name table)
  (car table))
(define (table-records table)
  (cdr table))
(define (set-table-name! table name)
  (set-car! table name))
(define (set-table-records! table records)
  (set-cdr! table records))

(define (lookup table key)
  (let ((record (assoc key (table-records table))))
    (if record
        (record-value record)
        #false)))

(define (assoc records key)
  (cond ((empty-records? records) #false)
        ((equal? key (record-key (first-record records)))
         (first-record records))
        (else (assoc (rest-records records) key))))

(define (insert! table key value)
  (let* ((records (table-records table))
         (record (assoc records key)))
    (if record
        (set-record! record value)
        (begin
         (set! record (new-record key value))
         (set-table-records! table
                             (insert-record! records record))))))

(define (make-table)
  (list '*table*))
 ```

**Two-dimensionaal tables**

一维表的每个键识别一个子表。

```scheme
(define (make-subtable key)
  (list key))

(define (lookup table key-1 key-2)
  (let ((subtable (assoc (table-records table) key-1)))
    (if subtable
        (let ((record 
               (assoc (table-records subtable) key-2)))
          (if record (record-value record) #false))
        #false)))

(define (insert2! key-1 key-2 value table)
  (let ((subtable (assoc (table-records table) key-1)))
    (if subtable
        (let ((record 
               (assoc (table-records subtable) key-2)))
          (if record
              (set-record! record value)
              (begin (set! record (new-record key-2 value))
                     (set-table-records! subtable
                                         (insert-record! (table-records subtable) record)))))
        (set-table-records! 
         table
         (begin (set! subtable (make-subtable key-1))
                (insert! subtable key-2 value)))))
  'ok)
```

**Creating local tables**

我们可以用程序表示表。

```scheme
(define (make-table)
  (let ((local-table (list '*table*)))
    ; internal
    (define (record-key record)
      (car record))
    (define (record-value record)
      (cdr record))
    (define (new-record key value)
      (cons key value))
    (define (set-record! record value)
      (set-cdr! record value))
    (define (empty-records? records)
      (null? records))
    (define (empty-records)
      '())
    (define (first-record records)
      (car records))
    (define (rest-records records)
      (cdr records))
    (define (insert-record! records record)
      (cons record records))
    (define (table-name table)
      (car table))
    (define (table-records table)
      (cdr table))
    (define (set-table-name! table name)
      (set-car! table name))
    (define (set-table-records! table records)
      (set-cdr! table records))
    (define (make-subtable key)
      (list key))
    
    (define (assoc records key)
      (cond ((empty-records? records) #false)
            ((equal? key (record-key (first-record records)))
             (first-record records))
            (else (assoc (rest-records records) key))))
    
    (define (lookup key-1 key-2)
      (let ((subtable 
             (assoc (table-records local-table) key-1)))
        (if subtable
            (let ((record 
                   (assoc (table-records subtable) key-2)))
              (if record (record-value record) #false))
            #false)))
    (define (insert-subtable! table key value)
      (let* ((records (table-records table))
             (record (assoc records key)))
        (if record
            (set-record! record value)
            (begin
              (set! record (new-record key value))
              (set-table-records! table
                                  (insert-record! records record))))))
    
    (define (insert! key-1 key-2 value)
      (let ((subtable 
             (assoc (table-records local-table) key-1)))
        (if subtable
            (let ((record 
                   (assoc (table-records subtable) key-2)))
              (if record
                  (set-record! record value)
                  (insert-subtable! subtable key-2 value)))
            (set-table-records! 
             local-table
             (begin (set! subtable (make-subtable key-1))
                    (insert-subtable! subtable key-2 value)
                    (insert-record! (table-records local-table) subtable)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            ((eq? m 'print-proc) local-table)
            (else (error "Unknown operation: 
                          TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))
(define print (operation-table  'print-proc))

(get 'A 'a)
(put 'A 'a 1)
(get 'A 'a)
(put 'A 'b 2)
(get 'A 'b)
(put 'B 'a 3)
(get 'B 'a)
(put 'B 'b 4)
(get 'B 'b)
(put 'B 'b 5)
(get 'B 'b)
```

Exercise 3.24: 给构造函数添加一个参数same-key?作为key的相等性测试程序

Exercise 3.25: 一般化一维两维表，可以有任意个键。lookup和insert!接收一个键的list

```scheme
(define (make-table)
  (let ((self (list '*table*)))
    ; internal
    (define (record-key record)
      (car record))
    (define (record-value record)
      (cdr record))
    (define (new-record key value)
      (cons key value))
    (define (set-record! record value)
      (set-cdr! record value))
    (define (empty-records? records)
      (null? records))
    (define (empty-records)
      '())
    (define (first-record records)
      (car records))
    (define (rest-records records)
      (cdr records))
    (define (insert-record! records record)
      (cons record records))
    (define (table-name table)
      (car table))
    (define (table-records table)
      (cdr table))
    (define (set-table-name! table name)
      (set-car! table name))
    (define (set-table-records! table records)
      (set-cdr! table records))

    (define (make-record lok value)
      (cond
        ((null? lok) (error "the list of keys is empty"))
        (else
         (new-record (car lok) (make-records (cdr lok) value)))))
    
    (define (make-records lok value)
      (define (iter lok)
        (cond
          ((null? lok) value)
          (else (insert-record!
                 (empty-records)
                 (new-record (car lok)
                             (iter (cdr lok)))))))
      (iter lok))
    
    (define (assoc records key)
      (cond ((empty-records? records) #false)
            ((equal? key (record-key (first-record records)))
             (first-record records))
            (else (assoc (rest-records records) key))))
    
    (define (lookup lok)
      (define (iter table lok)
        (let* ((key (car lok))
               (rest (cdr lok))
               (record (assoc (table-records table) key)))
          (if record
              (let ((value (record-value record)))
                (cond
                  ((null? rest) value)
                  ((pair? value) (iter record rest))
                  (else #f))) ; not a subtable
              #f)))
      (iter self lok))
    
    (define (insert! lok value)
      (define (iter table lok)
        (let* ((key (car lok))
               (rest (cdr lok))
               (records (table-records table))
               (record (assoc records key)))
          (if record
              (cond
                ((null? rest) (set-record! record value))
                ((pair? (record-value record)) (iter record rest))
                (else
                 (set-record! record (make-records rest value))))
              (set-table-records!
               table
               (insert-record!
                records
                (make-record lok value))))))
      (iter self lok)
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            ((eq? m 'print-proc) self)
            (else (error "Unknown operation: 
                          TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))
(define print (operation-table  'print-proc))

(put (list 'A) 1)
(get (list 'A))
(put (list 'B 'BB) 2)
(get (list 'B 'BB))
(get (list 'B))
(put (list 'B 'BB) 3)
(get (list 'B 'BB))
(put (list 'B) 4)
(get (list 'B))
(put (list 'A 'AA) 5)
(get (list 'A 'AA))
(put (list 'A 'AA 'AAA) 6)
(get (list 'A 'AA 'AAA))
(get (list 'A 'BB))
```

Exercise 3.26: 描述一种表实现，其中的记录用二叉树来组织（对比练习2.66）

```scheme
(define (make-table =? <?)
  (let ((self (list '*table* '())))
    ; internal
    (define (new-node key value)
      (list key value (empty-tree) (empty-tree)))
    (define (node-key node)
      (car node))
    (define (node-value node)
      (car (cdr node)))
    (define (node-left node)
      (car (cdr (cdr node))))
    (define (node-right node)
      (car (cdr (cdr (cdr node)))))
    (define (set-value! node value)
      (set-car! (cdr node) value))
    (define (set-left! node left)
      (set-car! (cdr (cdr node)) left))
    (define (set-right! node right)
      (set-car! (cdr (cdr (cdr node))) right))
    (define (empty-tree? root)
      (null? root))
    (define (empty-tree)
      '())
    (define (insert-node! root node)
      (define (iter root)
        (cond
          ((empty-tree? root) node)
          (else
           (let ((key (node-key root))
                 (left (node-left root))
                 (right (node-right root))
                 (nkey (node-key node)))
             (cond
               ((=? nkey key) (error "key" key "already exist"))
               ((<? nkey key) (set-left! root (iter left)) root)
               (else (set-right! root (iter right)) root))))))
      (iter root))
    
    (define (new-node* lok value)
      (define (iter lok)
        (cond
          ((null? lok) value)
          (else (new-node (car lok)
                          (iter (cdr lok))))))
      (iter lok))
    
    (define (assoc root key)
      (define (iter root)
        (cond
          ((empty-tree? root) #false)
          (else
           (let ((rkey (node-key root)))
             (cond
               ((=? key rkey) root)
               ((<? key rkey) (iter (node-left root)))
               (else (iter (node-right root))))))))
      (iter root))
    
    (define (lookup lok)
      (define (iter table lok)
        (let* ((key (car lok))
               (rest (cdr lok))
               (node (assoc (node-value table) key)))
          (if node
              (let ((value (node-value node)))
                (cond
                  ((null? rest) value)
                  ((pair? value) (iter node rest))
                  (else #f))) ; not a subtable
              #f)))
      (iter self lok))
    
    (define (insert! lok value)
      (define (iter table lok)
        (let* ((key (car lok))
               (rest (cdr lok))
               (root (node-value table))
               (node (assoc root key)))
          (if node
              (cond
                ((null? rest) (set-value! node value))
                ((pair? (node-value node)) (iter node rest))
                (else
                 (set-value! node (new-node* rest value))))
              (set-value!
               table
               (insert-node!
                root
                (new-node* lok value))))))
      (iter self lok)
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            ((eq? m 'print-proc) self)
            (else (error "Unknown operation: 
                          TABLE" m))))
    dispatch))

(define operation-table (make-table symbol=? symbol<?))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))
(define print (operation-table  'print-proc))

(put (list 'A) 1)
(get (list 'A))
(put (list 'B 'BB) 2)
(get (list 'B 'BB))
(get (list 'B))
(put (list 'B 'BB) 3)
(get (list 'B 'BB))
(put (list 'B) 4)
(get (list 'B))
(put (list 'A 'AA) 5)
(get (list 'A 'AA))
(put (list 'A 'AA 'AAA) 6)
(get (list 'A 'AA 'AAA))
(get (list 'A 'BB))
```



Exercise 3.27: memoization (tabulation) 是这样的一种技术，允许程序在局部表中记录之前已经计算的值。作为一个例子回忆1.2.2中计算斐波那契数。

```scheme
(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))
```

memoized版本

```scheme
(define memo-fib
  (memoize 
   (lambda (n)
     (cond ((= n 0) 0)
           ((= n 1) 1)
           (else 
            (+ (memo-fib (- n 1))
               (memo-fib (- n 2))))))))

(define (memoize f)
  (let ((table (make-table)))
    (lambda (x)
      (let ((previously-computed-result 
             (lookup x table)))
        (or previously-computed-result
            (let ((result (f x)))
              (insert! x result table)
              result))))))
```

解释memo-fib复杂度是O(n)，因为有保存每个最多计算一次。如果只是简单定义memo-fibw在(memoize fib)呢？

### A Simulator for Digital Circuits

本节将设计一个执行数字逻辑仿真的系统，该系统是事件驱动仿真event-driven simulation类程序的代表。

```scheme
;A----+---|OR |--D--------------|AND|--S
;  +--|---|   |               +-|   |
;  |  |                       |
;  |  +---|   |  +--|NOT|--E--+
;B-+------|AND|--+---------------------C

; construc wires
(define a (make-wire))
(define b (make-wire))
(define c (make-wire))
(define d (make-wire))
(define e (make-wire))
(define s (make-wire))

; wire together the half-adder
(or-gate a b d)
ok
(and-gate a b c)
ok
(inverter c e)
ok
(and-gate d e s)
ok

(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(define (full-adder a b c-in sum c-out)
  (let ((c1 (make-wire)) 
        (c2 (make-wire))
        (s  (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))
```

从本质上讲，我们的模拟器为我们提供了构建电路语言的工具。 如果我们采用我们在1.1中研究Lisp时对语言的一般看法，我们可以说原始函数框构成了语言的原始元素，将框连在一起提供了一种组合的手段，而将连线模式指定为程序则是一种抽象的手段。

**Primitive function boxes**

```scheme
(get-signal ⟨wire⟩)
(set-signal! ⟨wire⟩ ⟨new value⟩)
(add-action! ⟨wire⟩ ⟨procedure of no arguments⟩) ; 每当线上信号改变时执行的程序
after-delay ; 一段延迟后执行程序

(define (inverter input output)
  (define (invert-input)
    (let ((new-value 
           (logical-not (get-signal input))))
      (after-delay 
       inverter-delay
       (lambda ()
         (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)

(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
           (logical-and (get-signal a1)
                        (get-signal a2))))
      (after-delay
       and-gate-delay
       (lambda ()
         (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

(define (logical-and s1 s2)
  (cond ((and (= s1 1) (= s2 1)) 1)
        ((and (= s1 0) (= s2 1)) 0)
        ((and (= s1 1) (= s2 0)) 0)
        ((and (= s1 0) (= s2 0)) 0)
        (else (error "Invalid signal" s1 s2))))
```

Exercise 3.28: 定义一个或门原始函数box

```scheme
(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value
           (logical-or (get-signal a1)
                       (get-signal a2))))
      (after-delay
       or-gate-delay
       (lambda ()
         (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)

(define (logical-or s1 s2)
  (cond ((and (= s1 1) (= s2 1)) 1)
        ((and (= s1 0) (= s2 1)) 1)
        ((and (= s1 1) (= s2 0)) 1)
        ((and (= s1 0) (= s2 0)) 0)
        (else (error "Invalid signal" s1 s2))))
```

Exercise 3.29: 另一种构建or-gate的方法是由与门和非门组合。

```scheme
; A or B == (not (and (not A) (not B)))
(define (or-gate a1 a2 output)
  (let ((c1 (make-wire))
        (c2 (make-wire))
        (c3 (make-wire)))
    (invertor a1 c1)
    (invertor a2 c2)
    (and-gate c1 c2 c3)
    (invertor c3 output)))
```

延迟是两倍非门+1倍与门的延迟

Exercise 3.30: ripple-carry adder

```scheme
(define (ripple-carry-adder al bl sl c)
  (define (iter al bl sl cout)
    (if (null? al)
        (set-signal! cout 0)
        (let ((a (car al))
              (b (car bl))
              (s (car sl))
              (cin (make-wire)))
          (full-adder a b cin s cout)
          (iter (cdr al) (cdr bl) (cdr sl) cin))))
  (iter al bl sl c))
```



**Representing wires**

在我们的仿真中，线是一个由两个局部状态变量的计算对象：一个singal-value（初始为0）和一个集合action-procedures，当信号改变值时运行。我们用消息传递风格来实现线

```scheme
(define (make-wire)
  (let ((signal-value 0) 
        (action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (begin (set! signal-value new-value)
                 (call-each 
                  action-procedures))
          'done))
    (define (accept-action-procedure! proc)
      (set! action-procedures 
            (cons proc action-procedures))
      (proc))  ; run it once
    (define (dispatch m)
      (cond ((eq? m 'get-signal) 
             signal-value)
            ((eq? m 'set-signal!) 
             set-my-signal!)
            ((eq? m 'add-action!) 
             accept-action-procedure!)
            (else (error "Unknown operation: 
                          WIRE" m))))
    dispatch))

(define (call-each procedures)
  (if (null? procedures)
      'done
      (begin ((car procedures))
             (call-each (cdr procedures)))))

(define (get-signal wire)
  (wire 'get-signal))
(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))
(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))
```



**The agenda**

还剩下after-delay未完成。我们维护一个叫agenda的数据结构，它包含了要做的事务，支持以下操作：

```scheme
(make-agenda) ; 返回一个新的
(empty-agenda? <agenda>)
(first-agenda-item <agenda>)
(remove-first-agenda-item! <agenda>)
(add-to-agenda! <time> <action> <agenda>) ; 增加一个给定的action程序在指定的时间执行
(current-time <agenda>) ; 返回当前的仿真时间
```

然后实现after-delay

```scheme
(define (after-delay delay action)
  (add-to-agenda! 
   (+ delay (current-time the-agenda))
   action
   the-agenda))
```

仿真由程序propagate驱动，它操作the-agenda.

```scheme
(define (propagate)
  (if (empty-agenda? the-agenda)
      'done
      (let ((first-item 
             (first-agenda-item the-agenda)))
        (first-item)
        (remove-first-agenda-item! the-agenda)
        (propagate))))
```



**A sample simulation**

probe用于探测指定wire上信号的变化。

```scheme
(define (probe name wire)
  (add-action! 
   wire
   (lambda ()
     (newline)
     (display name)
     (display " ")
     (display (current-time the-agenda))
     (display "  New-value = ")
     (display (get-signal wire)))))
```



```scheme
; initialize the agenda and specify delays
(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

; 4 wires
(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))

(probe 'sum sum)
sum 0  New-value = 0
(probe 'carry carry)
carry 0  New-value = 0

; connect the wires in a half-adder
(half-adder input-1 input-2 sum carry)
ok

; set input-1
(set-signal! input-1 1)
done

(propagate)
sum 8  New-value = 1
done

; set input-2
(set-signal! input-2 1)
done

(propagate)
carry 11  New-value = 1
sum 16  New-value = 0
done
```

Exercise 3.31: make-wire中定义的accept-action-procedure!为什么要立即执行一次？

确保有正确的初始值，例如非门输出为1。每当一段新线连接之后，相邻线的状态就可能发生变化。



**Implementing the agenda**

agenda由时间段组成，每个时间段是一个pair，由一个表示时间的数和一个需要在该时间执行的程序的队列组成。agenda本身是一个时间段的一维表，关于时间增序。当前时间是上一个action处理的时间。

```scheme
; time-segment
(define (make-time-segment time queue)
  (cons time queue))
(define (segment-time s) (car s))
(define (segment-queue s) (cdr s))

; agenda
(define (make-agenda) (list 0))
(define (current-time agenda) (car agenda))
(define (set-current-time! agenda time)
  (set-car! agenda time))
(define (segments agenda) (cdr agenda))
(define (set-segments! agenda segments)
  (set-cdr! agenda segments))
(define (first-segment agenda) 
  (car (segments agenda)))
(define (rest-segments agenda) 
  (cdr (segments agenda)))
(define (empty-agenda? agenda)
  (null? (segments agenda)))

(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
    (or (null? segments)
        (< time 
           (segment-time (car segments)))))
  (define (make-new-time-segment time action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))
  (define (add-to-segments! segments)
    (if (= (segment-time (car segments)) time)
        (insert-queue! 
         (segment-queue (car segments))
         action)
        (let ((rest (cdr segments)))
          (if (belongs-before? rest)
              (set-cdr!
               segments
               (cons (make-new-time-segment 
                      time 
                      action)
                     (cdr segments)))
              (add-to-segments! rest)))))
  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
        (set-segments!
         agenda
         (cons (make-new-time-segment 
                time 
                action)
               segments))
        (add-to-segments! segments))))

(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue 
            (first-segment agenda))))
    (delete-queue! q)
    (if (empty-queue? q)
        (set-segments! 
         agenda 
         (rest-segments agenda))
        (void))))

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "Agenda is empty: 
              FIRST-AGENDA-ITEM")
      (let ((first-seg 
             (first-segment agenda)))
        (set-current-time! 
         agenda 
         (segment-time first-seg))
        (front-queue 
         (segment-queue first-seg)))))
```

Exercise 3.32: 每个时间段中的程序按照在队列中的顺序执行。即先加到agenda的先执行。解释一下为什么要按照这个顺序。

以与门为例，只有两个输入信号对应的action都执行了之后的输出才是正确的结果。（也即action中的状态是有时序的。）调换顺序之后，就把一种中间结果作为最终结果了。所以顺序是不能更改的。

完整代码

```scheme
#lang racket

(require rnrs/mutable-pairs-6)
(require compatibility/mlist)

(define cons mcons)
(define car mcar)
(define cdr mcdr)
(define list mlist)
(define pair? mpair?)
(define set-car! set-mcar!)
(define set-cdr! set-mcdr!)
(define memq mmemq)
(define length mlength)

; queue
; data abstract
(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) 
  (set-car! queue item))
(define (set-rear-ptr! queue item) 
  (set-cdr! queue item))

; constructor
(define (make-queue) (cons '() '()))
; selector
(define (empty-queue? queue) 
  (null? (front-ptr queue)))
(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an 
              empty queue" queue)
      (car (front-ptr queue))))
; mutator
(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else (set-cdr! (rear-ptr queue) 
                          new-pair)
                (set-rear-ptr! queue new-pair)
                queue))))
(define (delete-queue! queue)
  (cond
   ((empty-queue? queue)
    (error "DELETE called with an 
            empty queue" queue))
   ((eq? (front-ptr queue) (rear-ptr queue))
    (set-front-ptr! queue '())
    (set-rear-ptr! queue '())
    queue)
   (else
    (set-front-ptr! queue (cdr (front-ptr queue)))
    queue)))

; wire implemetation
(define (make-wire)
  (let ((signal-value 0) 
        (action-procedures '()))
    (define (call-each procedures)
      (if (null? procedures)
          'done
          (begin ((car procedures))
                 (call-each (cdr procedures)))))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (begin (set! signal-value new-value)
                 (call-each 
                  action-procedures))
          'done))
    (define (accept-action-procedure! proc)
      (set! action-procedures 
            (cons proc action-procedures))
      (proc))  ; run it once
    (define (dispatch m)
      (cond ((eq? m 'get-signal) 
             signal-value)
            ((eq? m 'set-signal!) 
             set-my-signal!)
            ((eq? m 'add-action!) 
             accept-action-procedure!)
            (else (error "Unknown operation: 
                          WIRE" m))))
    dispatch))

(define (get-signal wire)
  (wire 'get-signal))
(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))
(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))

; probe
(define (probe name wire)
  (add-action! 
   wire
   (lambda ()
     (newline)
     (display name)
     (display " ")
     (display (current-time the-agenda))
     (display "  New-value = ")
     (display (get-signal wire)))))

; primitive function boxes
(define (inverter input output)
  (define (invert-input)
    (let ((new-value 
           (logical-not (get-signal input))))
      (after-delay 
       inverter-delay
       (lambda ()
         (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)

(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
           (logical-and (get-signal a1)
                        (get-signal a2))))
      (after-delay
       and-gate-delay
       (lambda ()
         (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

(define (logical-and s1 s2)
  (cond ((and (= s1 1) (= s2 1)) 1)
        ((and (= s1 0) (= s2 1)) 0)
        ((and (= s1 1) (= s2 0)) 0)
        ((and (= s1 0) (= s2 0)) 0)
        (else (error "Invalid signal" s1 s2))))

(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value
           (logical-or (get-signal a1)
                       (get-signal a2))))
      (after-delay
       or-gate-delay
       (lambda ()
         (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)

(define (logical-or s1 s2)
  (cond ((and (= s1 1) (= s2 1)) 1)
        ((and (= s1 0) (= s2 1)) 1)
        ((and (= s1 1) (= s2 0)) 1)
        ((and (= s1 0) (= s2 0)) 0)
        (else (error "Invalid signal" s1 s2))))

; high level circuit
(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(define (full-adder a b c-in sum c-out)
  (let ((c1 (make-wire)) 
        (c2 (make-wire))
        (s  (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

(define (ripple-carry-adder al bl sl c)
  (define (iter al bl sl cout)
    (if (null? al)
        (set-signal! cout 0)
        (let ((a (car al))
              (b (car bl))
              (s (car sl))
              (cin (make-wire)))
          (full-adder a b cin s cout)
          (iter (cdr al) (cdr bl) (cdr sl) cin))))
  (iter al bl sl c))

; time-segment & agenda
(define (make-time-segment time queue)
  (cons time queue))
(define (segment-time s) (car s))
(define (segment-queue s) (cdr s))

(define (make-agenda) (list 0))
(define (current-time agenda) (car agenda))
(define (set-current-time! agenda time)
  (set-car! agenda time))
(define (segments agenda) (cdr agenda))
(define (set-segments! agenda segments)
  (set-cdr! agenda segments))
(define (first-segment agenda) 
  (car (segments agenda)))
(define (rest-segments agenda) 
  (cdr (segments agenda)))
(define (empty-agenda? agenda)
  (null? (segments agenda)))

(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
    (or (null? segments)
        (< time 
           (segment-time (car segments)))))
  (define (make-new-time-segment time action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))
  (define (add-to-segments! segments)
    (if (= (segment-time (car segments)) time)
        (insert-queue! 
         (segment-queue (car segments))
         action)
        (let ((rest (cdr segments)))
          (if (belongs-before? rest)
              (set-cdr!
               segments
               (cons (make-new-time-segment 
                      time 
                      action)
                     (cdr segments)))
              (add-to-segments! rest)))))
  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
        (set-segments!
         agenda
         (cons (make-new-time-segment 
                time 
                action)
               segments))
        (add-to-segments! segments))))

(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue 
            (first-segment agenda))))
    (delete-queue! q)
    (if (empty-queue? q)
        (set-segments! 
         agenda 
         (rest-segments agenda))
        (void))))

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "Agenda is empty: 
              FIRST-AGENDA-ITEM")
      (let ((first-seg 
             (first-segment agenda)))
        (set-current-time! 
         agenda 
         (segment-time first-seg))
        (front-queue 
         (segment-queue first-seg)))))

(define (after-delay delay action)
  (add-to-agenda! 
   (+ delay (current-time the-agenda))
   action
   the-agenda))

(define (propagate)
  (if (empty-agenda? the-agenda)
      'done
      (let ((first-item 
             (first-agenda-item the-agenda)))
        (first-item)
        (remove-first-agenda-item! the-agenda)
        (propagate))))

; initialize the agenda and specify delays
(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

; 4 wires
(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))

(probe 'sum sum)
(probe 'carry carry)

(half-adder input-1 input-2 sum carry)
(set-signal! input-1 1)

; set input-1
(propagate)

(set-signal! input-2 1)
(propagate)
```



### Propagation of Constraints

传统计算机程序是单向的计算。另一方面我们经常将系统关于不同量之间的关系进行建模。本节将起草一种语言设计，使我们能够从关系本身的角度来工作。这种语言的基本限成员是**primitive constraints**，表明量之间的某种关系。例如

```scheme
(adder a b c)      ; a + b = c
(multiplier x y z) ; xy=z
(constant 3.14 x)  ; x=3.14
```

通过构建限制网络来组合限制，constraint之间通过connectors聚合。

这样一个网络的计算是如下进行的：当一个connector被给予一个值的时候，它唤醒所有跟它相关的constraints。每个被唤醒的限制盒都会轮询其connectors，看是否有足够的信息来确定一个连接器的值。如果是，就设置该连接器，然后它又唤醒其他相关的限制，以此类推。

**Using the constraint system**

```scheme
; 9C = 5(F-32)
; create connectors
(define C (make-connector))
(define F (make-connector))
(celsius-fahrenheit-converter C F)
ok

(define (celsius-fahrenheit-converter c f)
  (let ((u (make-connector))
        (v (make-connector))
        (w (make-connector))
        (x (make-connector))
        (y (make-connector)))
    (multiplier c w u)
    (multiplier v x u)
    (adder v y f)
    (constant 9 w)
    (constant 5 x)
    (constant 32 y)
    'ok))

(probe "Celsius temp" C)
(probe "Fahrenheit temp" F)

; set value
(set-value! C 25 'user)  ; the third argument tells C that this directive comes from the user
Probe: Celsius temp = 25
Probe: Fahrenheit temp = 77
done

(set-value! F 212 'user)
Error! Contradiction (77 212) ; already set 77

(forget-value! C 'user)
Probe: Celsius temp = ?
Probe: Fahrenheit temp = ?
done

(set-value! F 212 'user)
Probe: Fahrenheit temp = 212
Probe: Celsius temp = 100
done
```

**Implementing the constraint system**

限制系统通过带局部状态的程序式对象来实现，跟上一节中数字电路的实现方式非常类类似。虽然限制系统的基本对象要更加复杂一点，系统总体上更简单，因为不用带薪agendas和逻辑延迟。

```scheme
; operations on connectors
(has-value? <connector>)
(get-value <connector>)
(set-value! <connector> <new-value> <informant>) ; 报告者正在请求设置值
(forget-value! <connector> <retractor>) ; 牵引器正在请求遗忘值
(connect <connector> <new-constraint>)  ;告诉连接器加入新的限制

; connector tells the constraint it has a value
(define (inform-about-value constraint)
  (constraint 'I-have-a-value))
; connector tells the constraint it lost its value
(define (inform-about-no-value constraint)
  (constraint 'I-lost-my-value))

(define (adder a1 a2 sum)
  (define (process-new-value)
    (cond ((and (has-value? a1) 
                (has-value? a2))
           (set-value! sum
                       (+ (get-value a1) 
                          (get-value a2))
                       me))
          ((and (has-value? a1) 
                (has-value? sum))
           (set-value! a2
                       (- (get-value sum) 
                          (get-value a1))
                       me))
          ((and (has-value? a2) 
                (has-value? sum))
           (set-value! a1
                       (- (get-value sum) 
                          (get-value a2))
                       me))))
  (define (process-forget-value)
    (forget-value! sum me)
    (forget-value! a1 me)
    (forget-value! a2 me)
    (process-new-value)) ; 传递给其他connector
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else (error "Unknown request: 
                        ADDER" request))))
  (connect a1 me)
  (connect a2 me)
  (connect sum me)
  me)

(define (multiplier m1 m2 product)
  (define (process-new-value)
    (cond
     ((and (has-value? m1)
           (= 0 (get-value m1)))
      (set-value! product 0 me))
     ((and (has-value? m2)
           (= 0 (get-value m2)))
      (set-value! product 0 me))
     ((and (has-value? m1)
           (has-value? m2))
      (set-value! product
                  (* (get-value m1)
                     (get-value m2))
                  me))
     ((and (has-value? m1)
           (has-value? product))
      (let ((v1 (get-value m1))
            (vp (get-value product)))
        (if (and (= v1 0)
                 (not (= vp 0)))
            (error "impossible 0*m1 =" vp)
            (set-value! m2 (/ vp v1) me))))
     ((and (has-value? m2)
           (has-value? product))
      (let ((v2 (get-value m2))
            (vp (get-value product)))
        (if (and (= v2 0)
                 (not (= vp 0)))
            (error "impossible m1*0 =" vp)
            (set-value! m1 (/ vp v2) me))))))
  (define (process-forget-value)
    (forget-value! product me)
    (forget-value! m1 me)
    (forget-value! m2 me)
    (process-new-value))
  (define (me request)
    (cond
     ((eq? request 'I-have-a-value)
      (process-new-value))
     ((eq? request 'I-lost-my-value)
      (process-forget-value))
     (else (error "Unknown request: 
                        ADDER" request))))
  (connect m1 me)
  (connect m2 me)
  (connect product me)
  me)

; simply set the value of the designated connector
(define (constant value connector)
  (define (me request)
    (error "Unknown request: CONSTANT" 
           request))
  (connect connector me)
  (set-value! connector value me)
  me)

; print a message about the setting and unsetting of the designated connector.
(define (probe name connector)
  (define (print-probe value)
    (newline) (display "Probe: ")
    (display name) (display " = ")
    (display value))
  (define (process-new-value)
    (print-probe (get-value connector)))
  (define (process-forget-value)
    (print-probe "?"))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else (error "Unknown request: 
                        PROBE" request))))
  (connect connector me)
  me)
```

**Representing connectors**

```scheme
(define (make-connector)
  (let ((value false) 
        (informant false) 
        (constraints '()))
    (define (set-my-value newval setter)
      (cond ((not (has-value? me))
             (set! value newval)
             (set! informant setter)
             (for-each-except 
              setter
              inform-about-value
              constraints))
            ((not (= value newval))
             (error "Contradiction" 
                    (list value newval)))
            (else 'ignored)))
    (define (forget-my-value retractor)
      (if (eq? retractor informant)  ; why only forget the value set by informant
          (begin (set! informant false)
                 (for-each-except
                  retractor
                  inform-about-no-value
                  constraints))
          'ignored))
    (define (connect new-constraint)
      (if (not (memq new-constraint 
                     constraints))
          (set! constraints
                (cons new-constraint 
                      constraints))
          (void))
      (if (has-value? me)
          (inform-about-value new-constraint)
          (void))
      'done)
    (define (me request)
      (cond ((eq? request 'has-value?)
             (if informant true false))
            ((eq? request 'value) value)
            ((eq? request 'set-value!) 
             set-my-value)
            ((eq? request 'forget) 
             forget-my-value)
            ((eq? request 'connect) connect)
            (else (error "Unknown operation: 
                          CONNECTOR"
                         request))))
    me))

(define (for-each-except exception 
                         procedure 
                         list)
  (define (loop items)
    (cond ((null? items) 'done)
          ((eq? (car items) exception) 
           (loop (cdr items)))
          (else (procedure (car items))
                (loop (cdr items)))))
  (loop list))

(define (has-value? connector)
  (connector 'has-value?))
(define (get-value connector)
  (connector 'value))
(define (set-value! connector 
                    new-value 
                    informant)
  ((connector 'set-value!) 
   new-value 
   informant))
(define (forget-value! connector retractor)
  ((connector 'forget) retractor))
(define (connect connector new-constraint)
  ((connector 'connect) new-constraint))
```

Exercise 3.33: 使用multiplier, adder和constant限制，定义一个averager，由3个连接器a,b,c作为输入，建立限制使c为a和b的平均数。

```scheme
(define (averager a b c)
  (let ((x (make-connector))
        (y (make-connector)))
    (adder a b x)
    (multiplier c y x)
    (constant 2 y)
    'ok))
```

Exercise 3.34: 如下平方constraint的严重缺点

```scheme
(define (squarer a b) (multiplier a a b))
```

只能通过a求b，不能通过b求a，因为multiplier需要两个输入有值。

Exercise 3.35: 避免这种问题的一种方法上将squarer定义为一个新的基本限制。

```scheme
(define (squarer a b)
  (define (process-new-value)
    (if (has-value? b)
        (if (< (get-value b) 0)
            (error "square less than 0: 
                    SQUARER" 
                   (get-value b))
            (set-value! a (sqrt (get-value b)) me))
        (set-value! b (sqr (get-value a)) me)))
  (define (process-forget-value)
    (forget-value! a me)
    (forget-value! b me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else (error "Unknown request: 
                        PROBE" request))))
  (connect a me)
  (connect b me)
  me)
```

Exercise 3.37: celsius-fahrenheit-converter程序相比于更加面向表达式风格的定义显得很笨重 

```scheme
(define (celsius-fahrenheit-converter x)
  (c+ (c* (c/ (cv 9) (cv 5))
          x)
      (cv 32)))

(define C (make-connector))
(define F (celsius-fahrenheit-converter C))
```

这里c+, c*, c/等都是限制版本的算术操作。例如c+接收两个connector作为参数，返回一个connector通过一个adder限制关联到参数上。

```scheme
(define (c+ x y)
  (let ((z (make-connector)))
    (adder x y z)
    z))
```

定义其他类似的操作

```scheme
(define (c* x y)
  (let ((z (make-connector)))
    (multiplier x y z)
    z))

(define (c/ x y)
  (let ((z (make-connector)))
    (multiplier y z x)
    z))

(define (cv v)
  (let ((x (make-connector)))
    (constant v x)
    x))
```

完整程序

```scheme
#lang racket

(require rnrs/mutable-pairs-6)
(require compatibility/mlist)

(define cons mcons)
(define car mcar)
(define cdr mcdr)
(define list mlist)
(define pair? mpair?)
(define set-car! set-mcar!)
(define set-cdr! set-mcdr!)
(define memq mmemq)
(define length mlength)

; connector
(define (make-connector)
  (let ((value false) 
        (informant false) 
        (constraints '()))
    (define (set-my-value newval setter)
      (cond ((not (has-value? me))
             (set! value newval)
             (set! informant setter)
             (for-each-except 
              setter
              inform-about-value
              constraints))
            ((not (= value newval))
             (error "Contradiction" 
                    (list value newval)))
            (else 'ignored)))
    (define (forget-my-value retractor)
      (if (eq? retractor informant)  ; why only forget the value set by informant
          (begin (set! informant false)
                 (for-each-except
                  retractor
                  inform-about-no-value
                  constraints))
          'ignored))
    (define (connect new-constraint)
      (if (not (memq new-constraint 
                     constraints))
          (set! constraints
                (cons new-constraint 
                      constraints))
          (void))
      (if (has-value? me)
          (inform-about-value new-constraint)
          (void))
      'done)
    (define (me request)
      (cond ((eq? request 'has-value?)
             (if informant true false))
            ((eq? request 'value) value)
            ((eq? request 'set-value!) 
             set-my-value)
            ((eq? request 'forget) 
             forget-my-value)
            ((eq? request 'connect) connect)
            (else (error "Unknown operation: 
                          CONNECTOR"
                         request))))
    me))

(define (for-each-except exception 
                         procedure 
                         list)
  (define (loop items)
    (cond ((null? items) 'done)
          ((eq? (car items) exception) 
           (loop (cdr items)))
          (else (procedure (car items))
                (loop (cdr items)))))
  (loop list))

(define (has-value? connector)
  (connector 'has-value?))
(define (get-value connector)
  (connector 'value))
(define (set-value! connector 
                    new-value 
                    informant)
  ((connector 'set-value!) 
   new-value 
   informant))
(define (forget-value! connector retractor)
  ((connector 'forget) retractor))
(define (connect connector new-constraint)
  ((connector 'connect) new-constraint))

; print a message about the setting and unsetting of the designated connector.
(define (probe name connector)
  (define (print-probe value)
    (newline) (display "Probe: ")
    (display name) (display " = ")
    (display value))
  (define (process-new-value)
    (print-probe (get-value connector)))
  (define (process-forget-value)
    (print-probe "?"))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else (error "Unknown request: 
                        PROBE" request))))
  (connect connector me)
  me)

; primitive constraints
; connector tells the constraint it has a value
(define (inform-about-value constraint)
  (constraint 'I-have-a-value))
; connector tells the constraint it lost its value
(define (inform-about-no-value constraint)
  (constraint 'I-lost-my-value))

(define (adder a1 a2 sum)
  (define (process-new-value)
    (cond ((and (has-value? a1) 
                (has-value? a2))
           (set-value! sum
                       (+ (get-value a1) 
                          (get-value a2))
                       me))
          ((and (has-value? a1) 
                (has-value? sum))
           (set-value! a2
                       (- (get-value sum) 
                          (get-value a1))
                       me))
          ((and (has-value? a2) 
                (has-value? sum))
           (set-value! a1
                       (- (get-value sum) 
                          (get-value a2))
                       me))))
  (define (process-forget-value)
    (forget-value! sum me)
    (forget-value! a1 me)
    (forget-value! a2 me)
    (process-new-value)) ; 传递给其他connector
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else (error "Unknown request: 
                        ADDER" request))))
  (connect a1 me)
  (connect a2 me)
  (connect sum me)
  me)

(define (multiplier m1 m2 product)
  (define (process-new-value)
    (cond
     ((and (has-value? m1)
           (= 0 (get-value m1)))
      (set-value! product 0 me))
     ((and (has-value? m2)
           (= 0 (get-value m2)))
      (set-value! product 0 me))
     ((and (has-value? m1)
           (has-value? m2))
      (set-value! product
                  (* (get-value m1)
                     (get-value m2))
                  me))
     ((and (has-value? m1)
           (has-value? product))
      (let ((v1 (get-value m1))
            (vp (get-value product)))
        (if (and (= v1 0)
                 (not (= vp 0)))
            (error "impossible 0*m1 =" vp)
            (set-value! m2 (/ vp v1) me))))
     ((and (has-value? m2)
           (has-value? product))
      (let ((v2 (get-value m2))
            (vp (get-value product)))
        (if (and (= v2 0)
                 (not (= vp 0)))
            (error "impossible m1*0 =" vp)
            (set-value! m1 (/ vp v2) me))))))
  (define (process-forget-value)
    (forget-value! product me)
    (forget-value! m1 me)
    (forget-value! m2 me)
    (process-new-value))
  (define (me request)
    (cond
     ((eq? request 'I-have-a-value)
      (process-new-value))
     ((eq? request 'I-lost-my-value)
      (process-forget-value))
     (else (error "Unknown request: 
                        ADDER" request))))
  (connect m1 me)
  (connect m2 me)
  (connect product me)
  me)

; simply set the value of the designated connector
(define (constant value connector)
  (define (me request)
    (error "Unknown request: CONSTANT" 
           request))
  (connect connector me)
  (set-value! connector value me)
  me)

(define (squarer a b)
  (define (process-new-value)
    (if (has-value? b)
        (if (< (get-value b) 0)
            (error "square less than 0: 
                    SQUARER" 
                   (get-value b))
            (set-value! a (sqrt (get-value b)) me))
        (set-value! b (sqr (get-value a)) me)))
  (define (process-forget-value)
    (forget-value! a me)
    (forget-value! b me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else (error "Unknown request: 
                        PROBE" request))))
  (connect a me)
  (connect b me)
  me)

; combine constaints
(define (averager a b c)
  (let ((x (make-connector))
        (y (make-connector)))
    (adder a b x)
    (multiplier c y x)
    (constant 2 y)
    'ok))

; constraint version operations
(define (c+ x y)
  (let ((z (make-connector)))
    (adder x y z)
    z))

(define (c* x y)
  (let ((z (make-connector)))
    (multiplier x y z)
    z))

(define (c/ x y)
  (let ((z (make-connector)))
    (multiplier y z x)
    z))

(define (cv v)
  (let ((x (make-connector)))
    (constant v x)
    x))

; test
(define (celsius-fahrenheit-converter x)
  (c+ (c* (c/ (cv 9) (cv 5))
          x)
      (cv 32)))

(define C (make-connector))
(define F (celsius-fahrenheit-converter C))

(probe "Celsius temp" C)
(probe "Fahrenheit temp" F)
(set-value! C 100 'userA)
(forget-value! C 'userB)
(forget-value! C 'userA)
(set-value! F 200 'userB)
(set-value! F 300 'userB) ; Contradiction
```



## 3.4 Concurrency: Time Is of the Essence

我们已经见识了带局部状态的计算对象作为建模工具的威力，其代价是损失了引用透明性，引起了相同性和变化的问题，以及需要抛弃评估的替换模型转而向更加复杂的环境模型。

潜伏在状态复杂度、相同性和改变下面的核心问题是，通过引入赋值我们被迫将时间纳入我们的计算模型中。由于并发的存在使得赋值引入的复杂度变得更加大。

### The Nature of Time in Concurrent Systems

**Correct behavior of concurrent program**

不同程度的限制

- 规定不允许两个修改任何共享状态变量的操作同时发生。
- 并发系统产生的结果，跟进程以某种顺序串行执行的结果相同。
- 更弱的要求，算法收敛到正确的结果，不依赖操作的顺序，不需要对共享值的并发使用做任何限制。例如，多个进程有自己的空间记录值，然后计算它们的平均值。

Exercise 3.38: 初始100，三者同时操作，执行以下命令

```scheme
Peter: (set! balance (+ balance 10))
Paul:  (set! balance (- balance 20))
Mary:  (set! balance (- balance 
                        (/ balance 2)))
```

假设银行强制三个进程串行执行，列出balance所有可能的结果。35，40，45，50

如果允许交叉，还有哪些其他可能的值。

Peter： R + W

Paul:     R - W

Mary:   R / R - W

这里总共有7次读写操作，它们总共有多少种可能的顺序？

首先考虑Peter和Paul，将Paul的操作和Peter的操作合并，|R|W|，Paul的第一次操作有3个可能的位置，Paul的第二次操作不能比前一次操作更前。所以有3+2+1种可能。或者用另一种描述方式，4个操作排序共有4!，因为Peter/Paul的读写顺序是固定的，所以4!/(2!*2!)=6

然后Peter和Pual合并之后共有4次操作，Mary有3次操作，7!/(4!*3!)=35

最后将6x35=210

直接计算是7!/(2!\*2\!*3!)=210

不过这210种顺序可能有重复的值。

一定要交叉才可能产生不同于串行的结果。所以Peter和Paul有4种可能的顺序，其中只有两个写的顺序是影响结果的。

所以RRWW和RRW进行交叉

如果是Mary最后W，那么她W之前的值总共有多少种可能？

```
100->110->90
100->80->90
100->80->110
100->110->80
```

所以Mary的两次R有几种可能，100/100，80/80，90/90，110/110，100/80，100/90，100/110，80/90，80/110，110/90，110/80分别对应结果50,40,45,55,30,40,60,50,70,35,25

如果Paul最后写，他W之前的值总共有如下可能 ？

```
; Peter先写，Mary的两次R有100/100,100/110,110/110三种可能
100->110->50
100->110->60
100->110->55
; 如果Mary先写，Peter要么读到100要么50
100->50->60
100->50->110
```

所以Paul读的值可能是50,55,60,100,110，对应结果30,35,40,80,90

如果是Peter最后写，他W之前的值总共有如下可能

```
; Paul先写，Mary的两次R有100/100,80/80,100/80三种可能
100->80->50
100->80->40
100->80->30
; 如果Mary先写，Paul要么读到100要么50
100->50->80
100->50->30
```

所以Peter可能读的值是30,40,50,80,100，对应结果40,50,60,90,110

全是汇总，25,30,35,40,45,50,55,60,70,80,90,110，除去35/40/45/50还有8种可能。

### Mechanisms for Controlling Concurrency

并发系统设计的一种更加实际的方法是，设计通用的机制以允许我们来限制并发进程的交错以使我们能够确保程序的行为是正确的。本节描述其中一种方法，**serializer**.

**Serializing access to shared state**

某些过程的集合一次只允许一个执行。

**Serializers in Scheme**

不限制有101,121,110,11,100五种可能的结果

```scheme
(define x 10)
(define s (make-serializer))
(parallel-execute 
 (s (lambda () (set! x (* x x))))
 (s (lambda () (set! x (+ x 1)))))
```

Exercise 3.39: 改成这样之后还有几种可能的结果

```scheme
(define x 10)
(define s (make-serializer))
(parallel-execute 
  (lambda () 
    (set! x ((s (lambda () (* x x))))))
  (s (lambda () (set! x (+ x 1)))))
```

101，121，100

Exercise 3.40: 修改前后x可能的结果

```scheme
(define x 10)
(parallel-execute 
 (lambda () (set! x (* x x)))
 (lambda () (set! x (* x x x))))
; 10^2,10^3,10^4,10^5,10^6

(define x 10)
(define s (make-serializer))
(parallel-execute 
 (s (lambda () (set! x (* x x))))
 (s (lambda () (set! x (* x x x)))))
; 10^6
```

Exercise 3.41: 读balance不需要保护，因为只有单个读操作。

```scheme
(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance 
                     (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((protected (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) 
             (protected withdraw))
            ((eq? m 'deposit) 
             (protected deposit))
            ((eq? m 'balance) 
             balance)
            (else (error "Unknown request: 
                          MAKE-ACCOUNT"
                         m))))
    dispatch))
```



Exercise 3.42: 改成这样是否安全？特别地，两个版本在允许什么并发上有什么区别么？

```scheme
(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance 
                     (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((protected (make-serializer)))
    (let ((protected-withdraw 
           (protected withdraw))
          (protected-deposit 
           (protected deposit)))
      (define (dispatch m)
        (cond ((eq? m 'withdraw) 
               protected-withdraw)
              ((eq? m 'deposit) 
               protected-deposit)
              ((eq? m 'balance) 
               balance)
              (else 
               (error "Unknown request: 
                       MAKE-ACCOUNT"
                      m))))
      dispatch)))
```

第一个版本，同时调用dispatch就可能会等待。第二个版本，允许dispatch并发，但是调用其返回的函数不允许并发。所以是安全的，粒度更小一点。

好像不是这样。前一个版本每次调用dispatch都会用serializer生成一个新函数，而第二个版本都是使用固定的两个函数。在不知道make-serializer实现的情况下，这是不安全的，因为不知道它是否允许同一个函数交叉执行。



**Complexity of using multiple shared resources**

像这样交换两个账户的操作，只是限制单个账户不能并发并不能保证结果正确。

```scheme
(define (exchange account1 account2)
  (let ((difference (- (account1 'balance)
                       (account2 'balance))))
    ((account1 'withdraw) difference)
    ((account2 'deposit) difference)))
```

导出serializer

```scheme
(define (make-account-and-serializer balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin 
          (set! balance (- balance amount))
          balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((balance-serializer 
         (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            ((eq? m 'balance) balance)
            ((eq? m 'serializer) 
             balance-serializer)
            (else (error "Unknown request: 
                          MAKE-ACCOUNT"
                         m))))
    dispatch))

(define (deposit account amount)
  (let ((s (account 'serializer))
        (d (account 'deposit)))
    ((s d) amount)))
; 然后我们可以实现序列化版本的exchange函数
(define (serialized-exchange account1 account2)
  (let ((serializer1 (account1 'serializer))
        (serializer2 (account2 'serializer)))
    ((serializer1 (serializer2 exchange))
     account1
     account2)))
```

Exercise 3.44: exchange的时候两个账户关联的，其具体操作同时取决于两个账户的状态。而transfer只需要自己账户有足够的余额就可以了，所以只要单个账户的操作是序列化的就可以。 

Exercise 3.45: Louis建议自动序列化withdraw和deposit，外部就可以直接像最初那样处理就可以了。

```scheme
(define 
  (make-account-and-serializer balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance 
                     (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((balance-serializer 
         (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) 
             (balance-serializer withdraw))
            ((eq? m 'deposit) 
             (balance-serializer deposit))
            ((eq? m 'balance) 
             balance)
            ((eq? m 'serializer) 
             balance-serializer)
            (else (error "Unknown request: 
                          MAKE-ACCOUNT"
                         m))))
    dispatch))

(define (deposit account amount)
  ((account 'deposit) amount))
```

这有什么问题。特别地，考虑当serialized-exchange被调用的时候会发生什么。

调用serialized-exchange时已经保护了一层，exchange内部调用withdraw和serializer时又会自动保护一层，于是就block住了。



**Implementing serializers**

我们使用一种更加基础的同步机制叫mutex来实现串行器。每个串行器都关联一个mutex，给定一个过程p，串行器返回一个过程。它获得mutex，跑p然后释放mutex。

```scheme
(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serialized-p . args)
        (mutex 'acquire)
        (let ((val (apply p args)))
          (mutex 'release)
          val))
      serialized-p)))

(define (make-mutex)
  (let ((cell (list false)))
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-mutex 'acquire))) ; retry
            ((eq? m 'release) (clear! cell))))
    the-mutex))
(define (clear! cell) (set-car! cell false))

; does not suffice as it stands, it must be atomic
; 对于单核抢占式处理器，可以通过禁用抢占。对于多核处理器，直接在硬件上提供了支持原子操作的指令
(define (test-and-set! cell)
  (if (car cell)
      true
      (begin (set-car! cell true)
             false)))
```

Exercise 3.46: 假设我们就像上面那样实现test-and-set!，没有使操作原子。画图演示mutex实现怎么失败。

Exercise 3.47: semaphore是mutex的一般化。分别用mutex和原子的test-and-set!操作来实现信号量。

```scheme
(define (make-semaphore n)
  (let ((mtx (make-mutex))
        (c 0))
    (define (semaphore m)
      (cond
       ((eq? m 'acquire)
        (mtx 'acquire)
        (if (< c n)
            (begin (set! c (+ c 1)) (mtx 'release))
            (begin (mtx 'release) (semaphore 'acquire))))
       ((eq? m 'release)
        (mtx 'acquire)
        (set! c (- c 1))
        (mtx 'release))))
    semaphore))

; use 2 mutex
(define (make-semaphore n)
  (let ((access-mtx (make-mutex))
        (full-mtx (make-mutex))
        (c 0))
    (define (semaphore m)
      (cond
       ((eq? m 'acquire)
        (full-mtx 'acquire)
        (access-mtx 'acquire)
        (set! c (+ c 1))
        (if (< c n)
            (full-mtx 'release)
            (void))   ; don't release if full
        (access-mtx 'release))
       ((eq? m 'release)
        (access-mtx 'acquire)
        (if (= c n)
            (full-mtx 'release)
            (void))
        (set! c (- c 1))
        (access-mtx 'release))))
    semaphore))

; b just substitute (mtx 'acquire) and (mtx 'release) with test-and-set! and clear!
```



**Deadlock**

防止死锁的一种方法是总是按照既定的顺序加锁。有其他情况需要更加高超的死锁防止技术，或者根本无法避免。

Exercise 3.49: 给出一种场景上面的死锁避免技术不工作。（提示：在exchange问题中事先知道需要访问哪个账户。考虑一种情况，进程必须在知道还需要其他什么共享资源之前就访问一些共享资源）

**Concurrency, time, and communication**

 

## 3.5 Streams

本节探索另一种建模状态的方法，它基于叫做streams的数据结构。streams可以减轻一些建模状态的复杂度。

流处理让我们使用赋值或可变数据就建模带状态的系统。但它也有它自己的困难。

### Streams Are Delayed Lists

2.2.3节中的我们构思了操作sequences序列的强大抽象，像map,filter,accmulate等，但是在时间和空间上效率都不高，每一步都需要构造和拷贝数据结构。可以对比下面两个程序

```scheme
(define (sum-primes a b)
  (define (iter count accum)
    (cond ((> count b) accum)
          ((prime? count)
           (iter (+ count 1)
                 (+ count accum)))
          (else (iter (+ count 1) accum))))
  (iter a 0))

(define (sum-primes a b)
  (accumulate 
   +
   0
   (filter prime? (enumerate-interval a b))))
```

下面这个程序就更明显了，为了寻找第二个质数，需要完整遍历好几遍。

```scheme
(car (cdr 
      (filter 
       prime?
       (enumerate-interval 10000 1000000))))
```

Streams是一种聪明的想法，允许我们使用序列操作而不招致将序列操作为lists的成本。我们既可以将程序高雅地构思为序列操作，又可以获得增量计算的效率。

表面上，其接口只是换了个名字，操作方式跟lists类似。

```scheme
(stream-car (cons-stream x y)) = x
(stream-cdr (cons-stream x y)) = y
the-empty-stream
stream-null?

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream 
       (proc (stream-car s))
       (stream-map proc (stream-cdr s)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin 
        (proc (stream-car s))
        (stream-for-each proc 
                         (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))
```

为了使流的实现自动和透明地将流的构建与它的使用交织在一起。我们将安排流的cdr在它被stream-cdr访问的时候才求值，而不是流在被cons-stream构建的时候求值。

dealy返回一个对象，承诺在未来某个时间求值表达式，force则接收delayed object执行求值。但是cons-stream必须是一种特殊的格式，如果是普通函数的话，就会自动求值。

```scheme
(cons-stream ⟨a⟩ ⟨b⟩)

; is equivalent to
(cons ⟨a⟩ (delay ⟨b⟩))

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
;     (lambda ()
;       expr))))

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
```



**The stream implementation in action**

现在再来看前面求第二个质数的例子

```scheme
(stream-car 
 (stream-cdr
  (stream-filter 
   prime? (stream-enumerate-interval 
           10000 1000000))))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1)
                                  high))))

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
```



**Implementing delay and force**

```scheme
(delay <exp>)
; is syntacitc sugar for
(lambda () <exp>)

(define (force delayed-object)
  (delayed-object))
```

在很多应用中，我们可能会force同一个delayed object多次。可以第一次force然后保存值，后面直接取值。

```scheme
(define (memo-proc proc)
  (let ((already-run? false) (result false))
    (lambda ()
      (if (not already-run?)
          (begin (set! result (proc))
                 (set! already-run? true)
                 result)
          result))))

; then delay is defined so that it is equivalent to
(memo-proc (lambda () ⟨exp⟩))
```

Exercise 3.50: 完成下面的定义，通用版本的stream-map

```scheme
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc
                    (map stream-cdr argstreams))))))
```

Exercise 3.51: 为了更近地观看delayed求值，我们用下面的函数

```scheme
(define (show x)
  (display-line x)
  x)
```

下面的表达式序列，解释器会打印什么？

```scheme
(define x 
  (stream-map 
   show 
   (stream-enumerate-interval 0 10)))

0
(stream-ref x 5) ; 0 - 5?

1
2
3
4
55
(stream-ref x 7) ; 0 - 7?

6
77
; 如果不是memoization版本则输出1到7
```

流处理可以使我们忽略时间实际在程序中发生的顺序。不幸的是，这恰恰是当赋值存在时我们不能做的事。赋值的存在，强迫我们关注时间和变化。

Exercise 3.52: 下面的代码每一步求值之后sum的值，stream-ref和display-stream打印的响应。如果是不保存结果版的delay，这些响应有什么变化？

```scheme
(define sum 0)
; sum: 0

(define (accum x)
  (set! sum (+ x sum))
  sum)
; sum: 0

(define seq 
  (stream-map 
   accum 
   (stream-enumerate-interval 1 20)))
; sum: 1
; '(1 . #<procedure>)

(define y (stream-filter even? seq))
; 1, 1+2, 1+2+3 even
; sum: 6
; '(6 . #<procedure>)

(define z 
  (stream-filter 
   (lambda (x) 
     (= (remainder x 5) 0)) seq))
; 1+2+3+4
; sum: 10
; '(10 . #<procedure>)

(stream-ref y 7)
136
; sum: 136
(display-stream z)

10
15
45
55
105
120
190
210'done
; sum: 210
```

如果没有memoization，主要区别y和z都用了seq，如果没有memoization，那么就算y已经求值过了的seq元素，z还是会去求值，所以sum的值会更大。

```scheme
(define sum 0)
; sum: 0

(define (accum x)
  (set! sum (+ x sum))
  sum)
; sum: 0

(define seq 
  (stream-map 
   accum 
   (stream-enumerate-interval 1 20)))
; sum: 1

(define y (stream-filter even? seq))
; 1+2+3 even
; sum: 6

(define z 
  (stream-filter 
   (lambda (x) 
     (= (remainder x 5) 0)) seq))
; 6+2+3+4
; sum: 15

(stream-ref y 7)
162
; 15+4+5+...+17=162
; sum: 162
(display-stream z)

15
180
230
305'done
; 162+5+6+...+20=362
; sum: 362
```



### Infinite Streams

可以表示无限长的序列。

```scheme
; 所有整数
(define (integers-starting-from n)
  (cons-stream 
   n (integers-starting-from (+ n 1))))
(define integers (integers-starting-from 1))

; 所有不能被7整除的
(define (divisible? x y) (= (remainder x y) 0))
(define no-sevens
  (stream-filter (lambda (x) 
                   (not (divisible? x 7)))
                 integers))

; fibonacci
(define (fibgen a b)
  (cons-stream a (fibgen b (+ a b))))
(define fibs (fibgen 0 1))

; prime numbers
(define (sieve stream)
  (cons-stream
   (stream-car stream)
   (sieve (stream-filter
           (lambda (x)
             (not (divisible? 
                   x (stream-car stream))))
           (stream-cdr stream)))))

(define primes 
  (sieve (integers-starting-from 2)))
```



**Defining streams implicitly**

```scheme
; 无穷1
(define ones (cons-stream 1 ones))

; 流的按元素加法
(define (add-streams s1 s2) 
  (stream-map + s1 s2))

; 整数
(define integers 
  (cons-stream 1 (add-streams ones integers)))

; Fibonacci
(define fibs 
  (cons-stream 
   0 (cons-stream
      1 (add-streams 
         (stream-cdr fibs) fibs))))

; 放缩
(define (scale-stream stream factor)
  (stream-map
   (lambda (x) (* x factor))
   stream))
```

另一种定义质数的方法。primes使用了prime?，prime?又使用了primes。因为任意时刻已经有足够的质数已经产生，可以用于判断当前数是否是质数。

```scheme
(define primes
  (cons-stream
   2 (stream-filter 
      prime? (integers-starting-from 3))))

(define (prime? n)
  (define (iter ps)
    (cond ((> (square (stream-car ps)) n) true)
          ((divisible? n (stream-car ps)) false)
          (else (iter (stream-cdr ps)))))
  (iter primes))
```

Exercise 3.53: 描述下面定义的流的元素。2的次方

```scheme
(define s (cons-stream 1 (add-streams s s)))
```

Exercise 3.54: 定义类似add-streams的mul-streams。然后用它和integers一起定义阶乘的流。

```scheme
(define (mul-streams s1 s2)
  (stream-map * s1 s2))

; 0!, 1!, ...
(define factorials 
  (cons-stream 1 (mul-streams factorials integers)))

; 1!, 2!, ...
(define factorials 
  (cons-stream 1 (mul-streams factorials (stream-cdr integers))))
```

Exercise 3.55: partial-sums接收一个流作为参数，返回一个流，计算累积和。

```scheme
(define (partial-sums s)
  (add-streams s (cons-stream 0 (partial-sums s))))
```

Exercise 3.56: 升序枚举所有只有2,3,5质因数的正整数。

- s从1开始
- s的元素乘以2还是s的元素
- 同样地，s的元素乘以3还是s的元素，s的元素乘以5还是s的元素
- 以上是所有s的元素

现在我们要做的事情就是合并三个有序流，消除重复。

```scheme
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
```

然后用merge构造s如下

```scheme
(define S (cons-stream 1 (merge (scale-stream S 2) (merge (scale-stream S 3) (scale-stream S 5)))))
```

Exercise 3.57:使用 基于add-streams函数定义的fibs计算第n个Fibonacci数需要执行多少次加法。从第0个开始数的话，n-1次。如果不用memo-proc优化的版本需要的次数会指数级增长。没有了保存的值，每次前两个值都要计算。A(n)=A(n-1)+A(n-2)

Exercise 3.58: 给出下面函数计算的流的一个解释。

```scheme
(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) 
           den 
           radix)))
```

(expand 1 7 10)产生什么元素，(expand 3 8 10)呢？

142857142857

375000

1/7的小数，3/8的小数

Exercise 3.59: 将级数表示成流，流的元素是级数的系数。定义一个函数integrate-series计算幂级数的积分。返回的流不包含常数项。

```scheme
(define (integrate-series s)
  (stream-map / s integers))

(define exp-series
  (cons-stream
   1 (integrate-series exp-series)))

(define cosine-series
  (cons-stream 1 (stream-map - (integrate-series sine-series))))

(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))
```

Exercise 3.60: 幂级数表示成系数的流之后，级数加法就可以用add-streams实现。完成级数乘法的定义

```scheme
(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1)
                  (stream-car s2))
               (add-streams (scale-stream (stream-cdr s2) (stream-car s1))
                            (mul-series (stream-cdr s1) s2))))
```

Exercise 3.61: 假设级数S的常数项是1，求S的倒数X=1/S。将S写成X=1-Sr * X

```scheme
(define (invert-unit-series s)
  (let ((c (stream-car s))
        (sr (stream-cdr s)))
    (if (= c 0)
        (error "constant term can't be zero")
        (cons-stream (/ 1 c)
                     (scale-stream (mul-series sr (invert-unit-series s))
                                   -1)))))
```

Exercise 3.62: 定义级数除法，然后生成tangent的幂级数

```scheme
(define (div-series s1 s2)
  (mul-series s1 (invert-unit-series s2)))

(define tangent-series
  (div-series sine-series cosine-series))
```



### Exploiting the Stream Paradigm

**Formulating iterations as stream processes**

我们现在知道了可以将状态表示为无时间的流，而不是表示为一个被更新的变量的集合。

将1.1.7中求平方根的方法改成用流

```scheme
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
```

同样地，可以讲pi的近似求法pi/4=1-1/3+1/5-1/7用流来表示

```scheme
(define (pi-summands n)
  (cons-stream 
   (/ 1.0 n)
   (stream-map - (pi-summands (+ n 2)))))

(define pi-stream
  (scale-stream 
   (partial-sums (pi-summands 1)) 4))
```

上面两个例子跟更新状态变量没有多大区别，但是流给了我们一个做一些trciks的机会。例如我们可以用一个sequence accelerator转换流，以使序列更快地收敛。

一个加速器的例子，欧拉的技术，Sn是原始和序列的第n项，加速序列可以表示为

```scheme
(define (euler-transform s)
  (let ((s0 (stream-ref s 0))     ; Sₙ₋₁
        (s1 (stream-ref s 1))     ; Sₙ
        (s2 (stream-ref s 2)))    ; Sₙ₊₁
    (cons-stream 
     (- s2 (/ (square (- s2 s1))
              (+ s0 (* -2 s1) s2)))
     (euler-transform (stream-cdr s)))))

(display-stream (euler-transform pi-stream))
```

我们还可以递归地加速加速序列

```scheme
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

(display-stream 
 (accelerated-sequence euler-transform
                       pi-stream))
4.
3.166666666666667
3.142105263157895
3.141599357319005
3.1415927140337785
3.1415926539752927
3.1415926535911765
3.141592653589778
```

第8项已经精确到第145位了，如果只用原始的序列，需要10^13量级的项数

虽然我们不用流也可以实现这样的加速技术，但是用流的表述特别地优雅和方便。因为整个状态序列可以作为一个数据结构提供给我们，可以用一套统一的操作来操作。

EXercise 3.63: Louis问为什么不直接写成下面这样，不用局部变量

```scheme
(define (sqrt-stream x)
  (cons-stream 
   1.0
   (stream-map (lambda (guess)
                 (sqrt-improve guess x))
               (sqrt-stream x))))
```

因为创建了一个新的序列，会执行多余的计算，效率更低。如果delay的实现没有memoization，那么两者一样。

Exercise 3.64: 写一个函数stream-limit，接收一个流和一个数字（容差）。测试知道两个连续的元素差值绝对值小于容差，返回第二个元素。

```scheme
(define (stream-limit s n)
  (let ((a1 (stream-ref 0))
        (a2 (stream-ref 1)))
    (if (< (abs (- a1 a2)) n)
        a2
        (stream-limit (stream-cdr s) n))))

(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))
```

Exercise 3.65: 使用上面计算pi的三种方法计算2的自然对数的近似值，收敛速度怎么样？

```scheme
(define (ln2-summands n)
  (cons-stream 
   (/ 1.0 n)
   (stream-map - (ln2-summands (+ n 1)))))

(define ln2-stream
  (partial-sums (ln2-summands 1)))

(display-stream ln2-stream)
(display-stream (euler-transform ln2-stream))
(display-stream (accelerated-sequence euler-transform ln2-stream))
```

**Infinite streams of pairs**

2.2.3节中序列范式用定义哉pairs的序列上的处理来操作传统的嵌套循环。将这种技术一般化到无限streams上，我们可以写哪些不容易表现为循环的程序。例如生成所有和对质数的整数对i<=j

```scheme
(stream-filter 
 (lambda (pair)
   (prime? (+ (car pair) (cadr pair))))
 int-pairs)
```

现在的问题就是生成int-pairs

```scheme
(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) 
                  (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s)
           (stream-cdr t)))))

; 各种合并方式对无限流不合适，因为第一个值永远不会变化
(define (stream-append s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream 
       (stream-car s1)
       (stream-append (stream-cdr s1) s2))))

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream 
       (stream-car s1)
       (interleave s2 (stream-cdr s1)))))
```

Exercise 3.66: pair (m, n)之前有多少个pair?                                                                                                                  

```
f(i,j) = 2^i - 2, i = j
f(i,j) = 2^i * (j-i) + 2^(i-1) - 2, i < j
```

Exercise 3.67: 修改pairs使(pairs integers integers)输出所有对(i,j)。(hint: 你需要混合一个额外的流)

```scheme
(define (pairs s t)
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
```



Exercise 3.68: Louis的定义有什么问题

```scheme
(define (pairs s t)
  (interleave
   (stream-map
    (lambda (x) 
      (list (stream-car s) x))
    t)
   (pairs (stream-cdr s)
          (stream-cdr t))))
```

正解：程序将无限循环，因为没有delay。

将产生如下流 (1, 1) (2, 2) (1, 2) (3, 3) (1, 3) (4, 4) (1, 4) (5, 5)...

Exercise 3.69: 实现triples，接收3个无限流，S，T，U，产生一个triples的流(Si, Tj, Uk)，其中i<=j<=k。然后使用triples生成满足i2+j2=k2的triples。

```scheme
(define (triples s t u)
  (cons-stream
   (list (stream-car s)
         (stream-car t)
         (stream-car u))
   (interleave
    (stream-map (lambda (x)
                  (cons (stream-car s) x))
                (stream-cdr (pairs t u)))
    (triples (stream-cdr s)
             (stream-cdr t)
             (stream-cdr u)))))

(define phythagorean-numbers
  (stream-filter (lambda (x)
                   (= (sqr (caddr x))
                      (+ (sqr (car x))
                         (sqr (cadr x)))))
                 (triples integers integers integers)))

; 一种优化？
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
            (rec (stream-cdr si) (1+ i) ptu top-i)))) 
     (rec s 1 pairs-tu first-of-integer-pair))) 
```



Exercise 3.70: 生成的流以某种有用的顺序出现更好，而不是临时的交错。使用类似练习3.56中merge的技术，定义merge-weighted，它接收一个额外的参数计算pair的权重。然后一般化pairs为weighted-pairs，接收两个流和一个计算权重的函数。然后使用这个函数生成，1所有正整数对(i<=j)的流，按i+j排序。2所有正整数对(i<=j)的流，i和j都不能整除2，3，5，且按2i+3j+5ij的和排序

```scheme
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
```

Exercise 3.71: 可以用不止一种方式表示为两个立方和的数有时叫做Ramanujan数。排序的对流提供了一种优雅的计算这些数的方法。搜索流上具有相同权重的连续pairs。

```scheme
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
```



Exercise 3.72: 用类似的方法生成所有这些数的流，这些数可以用3种不同的方式写成两个平方数的和。

```scheme
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
```



**Streams as signals**

Si=C+∑xj * dt,实现一个积分器。

```scheme
(define (integral integrand initial-value dt)
  (define int
    (cons-stream 
     initial-value
     (add-streams (scale-stream integrand dt)
                  int)))
  int)
```

Exercise 3.73: 我们可以使用来流建模数字电路，在一个时间序列表现电流或电压的值。例如一个并联的RC电路的电压值v关于注入的电流i可以用这个公式表示

v=v0+1/C ∫idt + Ri

写一个函数RC建模这个电路

```scheme
(define (RC R C dt)
  (lambda (i v0)
    (add-streams
     (integral (scale-stream i (/ 1 C)) v0 dt)
     (scale-stream i R))))
```

Exercise 3.74: Alysa正在设计一个系统处理从物理传感器来的信号，一个重要的特性是每当输入信号穿过0值时产生一个信号。输入信号从负变正+1，从正变负-1，否则0。（输入信号0当成正值）

```scheme
(define (make-zero-crossings
         input-stream last-value)
  (cons-stream
   (sign-change-detector 
    (stream-car input-stream) 
    last-value)
   (make-zero-crossings 
    (stream-cdr input-stream)
    (stream-car input-stream))))

(define zero-crossings 
  (make-zero-crossings sense-data 0))
```

近似等效于下面这个

```scheme
(define zero-crossings
  (stream-map sign-change-detector 
              sense-data 
              (cons-stream 0 sense-data)))

; 更优雅
(define (make-zero-crossings input-stream) 
   (stream-map sign-change-detector (stream-cdr sense-data) sense-data)) 
```

Exercise 3.75: 因为噪声的影响有很多假的值，先平滑下信号，跟前一个输入值平均。

```scheme
(define (make-zero-crossings input-stream last-value last-avpt) 
  (let ((avpt (/ (+ (stream-car input-stream) last-value) 2))) 
    (cons-stream (sign-change-detetor avpt last-avpt) 
                 (make-zero-crossings (stream-cdr input-stream) 
                                      (stream-car input-stream) 
                                      avpt))))

 (define (average-sign-change-detector x y z) 
   (let ((avg1 (average x y)) 
         (avg2 (average y z))) 
     (sign-change-detector avg2 avg1))) 
 
  (define (make-zero-crossings s) 
   (stream-map average-sign-change-detector s (stream-cdr s) (stream-cddr s))) 
```

Exercise 3.76: Eva批评Louis的写法没有模块化，将平滑操作和穿0提取揉合在一起。如果我们有了更好的处理软处信号的方法，extractor本身不用修改。

```scheme
(define (smooth input-stream)
  (stream-map (lambda(x y)(/ (+ x y) 2)) input-stream (stream-cdr input-stream)))
  
(define (zero-crossings input-stream)
  (stream-map sign-change-detector input-stream (stream-cdr input-stream)))
  
(define (smoothed-zero-crossing sense-data)
  (zero-crossings (smooth sense-data)))
```



### Streams and Delayed Evaluation

delay对使用流来建模包含循环的信号处理系统至关重要。否则解释器无法构建像integral那样的函数。

不幸的是，带循环系统的流模型可能要求显式使用delay，而不仅仅是cons-stream中隐藏的delay。

比如下面的solve解微分方程f(y)=dy/dt，在定义y的时候dy还没有定义。但是integral第一个元素是initial-value，其实不需要知道dy。当我们知道y的第一个元素之后，dy的第一个元素也就可以生成了。

```scheme
(define (solve f y0 dt)
  (define y (integral dy y0 dt))
  (define dy (stream-map f y))
  y)
```

我们重新定义integral，将期望integrand为delayed argument，然后在integral中force它。

```scheme
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
; y=e^t, t=1, y =2.71692
```



Exercise 3.77: 另一种版本的integral

```scheme
(define (integral
         delayed-integrand initial-value dt)
  (cons-stream 
   initial-value
   (if (stream-null? integrand)
       the-empty-stream
       (integral 
        (delay (stream-cdr integrand))
        (+ (* dt (stream-car integrand))
           initial-value)
        dt))))
```

Exercise 3.78: 同类二阶线性微分方程d2y/dt2 - ady/dt -by=0。写一个程序solve-2nd接收常数a，b，dt和y和dy/dt的初始值分别y0和dy0，生成y的连续值的流。

```scheme
(define (solve-2nd a b dt y0 dy0)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (add-streams (scale-stream dy a)
                           (scale-stream y b)))
  y)
```

Exercise 3.79: 一般化solve-2nd使其可以解一般的二阶微分方程d2y/dt2=f(dy/dt, y)

```scheme
(define (solve-2nd f dt y0 dy0)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (stream-map f dy y))
  y)
```

Exercise 3.80: 一个串联RLC电路

vR=iR * R

vL=L diL/dt

iC=C dvC/dt

iR=iL=-iC

vC=vL+vR

联立这几个等式，显示电路状态由微分方程对描述

dvC/dt=- iL/C

diL/dt=1/L vC - R/L * iL

写一个RLC程序接收参数R，L，C，dt，产生一个程序接收状态变量的初始值vC0和iL0产生一个状态vC和iL的流的对。然后使用RLC，生成一个RLC电路R=1，C=0.2，L=1，dt=0.1，初始值为iL0=0amps和vC0=10volts

```scheme
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

(displayn (car RLC-instance) 10)
```



**Normal-order evaluation**

使用delay/force提供了编程的灵活性，但是也使我们的程序更加复杂。例如前面的integral，我们必须记得integrand参数是dealyed的，其他使用integral的函数也要知道。实际上，我们创建了两类函数：普通的和带delayed参数的。一般地，创建不同的函数类迫使我们也创建不同的高阶函数类。

一种避免创建不同函数类的方法是使所有的函数都接收delayed参数。我们可以采用一种求值模型，所有的参数都是自动delay的，参数只有在实际需要的时候才force。这就将我们的语言转换成了normal-order evaluation。

不幸的是，在过程调用中包括延迟会破坏我们设计依赖事件顺序的程序的能力，例如，使用赋值、可变数据或执行输入或输出的程序。我们已经在练习3.51和3.52中看到了即使只是cons-stream中的单个delay就能引起很大的困惑。可变性和延迟求值在编程语言中还不能很好的混合，是一个热门的研究领域。

### Modularity of Functional Programs and Modularity of Objects

如我们在3.1.2中所见，引入赋值的一个最大的好处是可以通过封装增加我们系统的模块化，将一个大系统的部分状态封装在局部变量中。流模型可以不使用赋值提供一个等效的模块化。作为示例，我们重新实现pi的Monte Carlo估计。

```scheme
(define rand
  (let ((x random-init))
    (lambda ()
      (set! x (rand-update x))
      x)))
```

在流的构想中没有随机数发生器本身，只有一个连续调用rand-update产生的随机数流。

```scheme
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
```

Exercise 3.81: 就像练习3.6中一样，一般化随机数发生器使其允许reset随机数序列。生成一个和发生器相同的流的构思，它操作一个请求的输入流，generate一个新的随机数或reset序列。不要使用赋值。

```scheme
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

(define random-numbers (make-rand 0 (stream '(generate)
                                            '(generate)
                                            '(generate)
                                            '(reset 100)
                                            '(generate)
                                            '(generate))))
```

Exercise 3.82: 重做练习3.5将Monte Carlo积分用流实现，流版本的estimate-integral不需要制定trials次数。

```scheme
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
```



**A functional-programming view of time**

建模带局部状态的变量提供模块化的机制。我们也可以使用流来建模一个变量，表示一个连续状态的时间历史。本质上，我们使用流显式地表现时间，从而将在我们仿真世界中的时间从在求值过程中发生的事件的序列解耦。

为了比较两种建模方法，考虑取钱的例子

```scheme
(define (make-simplified-withdraw balance)
  (lambda (amount)
    (set! balance (- balance amount))
    balance))

(define (stream-withdraw balance amount-stream)
  (cons-stream
   balance
   (stream-withdraw 
    (- balance (stream-car amount-stream))
    (stream-cdr amount-stream))))
```

用对象建模是强大的且符合直觉的，因为它符合与真实世界的交互的感知。避免这些问题的可能性刺激了函数式编程的开发。

但是时间相关的问题还是会爬进函数式模型，还是联合银行账户的例子。对于对象模型，只要将交易请求发给一个共享的账户对象即可。而对于流模型，我们可以建模为将两个人的交易请求流合并。但是具体怎么合并是个问题，合并需要有某些真实时间的限制。所以又碰到跟之前一样的问题，之前需要通过引入显式同步来确保并发处理的事件的正确顺序。因此，为了支持函数式风格，需要合并来自不同代理的输入，重新引入了函数式风格旨在消除的同样问题。

所以说到底，两者都有优点，但是单独看又不完全令人满意。

# 4 Metalinguistic Abstraction

当我们面对不断复杂的问题，单个语言往往无法满足要求。我们经常需要采用一个新的语言来是我们用不同的方法来描述和思考问题。使用那些手头上特别适合问题的原语、组合方法、抽象方法。

元语言抽象在所有工程设计分支都扮演着重要的角色。在计算机编程中尤其重要，因为我们不但可以构思新语言，我们还可以通过构建evaluator来实现这些语言。一个语言的evaluator（interpreter）是一个过程，当它被应用一个语言的表达式，执行求值那个表达式需要的动作。

毫不夸张的说，可以把下面这句话是编程中最基本的思想：

>  The evaluator, which determines the meaning of expressions in a programming language, is just another program.

要理解这一点，就是要改变我们作为程序员对自己的印象。我们需要将自己视为语言的设计者，而不仅仅是语言的用户。

事实上，我们几乎可以将任何程序看成是一些语言的解释器。例如2.5.3节中的多项式操纵系统，我们只需要扩展下系统加上读取和打印多项式的函数，我们就有了一个解决符号数学问题的特殊目的语言。3.3.4节中数字电路模拟器和3.3.5节的限制传播器，都有自己的原语、组合和抽象。从这个角度看，处理大规模计算系统的技术合并了构建新计算机语言的技术，计算机科学本身不外乎是构建适当的描述性语言的学科。



## 4.1 The Metacircular Evaluator

自循环解释器 本质上是3.2节中描述的求值环境模型的一个Scheme公式化。eval-apply循环显示了一个计算机语言的本质。

### The Core of Evaluator

**Eval**

eval接收一个表达式参数和一个环境参数。它被构建为一个要评估的表达式语法类型的条件分析。为了保持过程通用的，我们抽象地表达表达式类型的确定。没种表达式类型有一个测试的谓词和选择它的部分的方法。

**Primitive expressions**

- 对于自求值表达式，例如数字，直接返回表达式本身
- 对于变量，必须在环境中查找值

**Special forms**

- 对于引用的表达式，返回被引用的表达式
- 赋值给一个变量或者定义一个变量，必须递归调用eval计算变量的新值。环境必须被修改以更新变量的binding
- if表达式需要特殊处理
- lambda表达式必须转换为一个可应用的过程，通过将参数和函数体与评估时的环境一起打包。
- begin表达式需要以表达式出现的顺序依次求值
- cond转换成一个嵌套的if语句，然后求值

**Combinations**

- 对于一个过程应用，eval必须递归地评估其operator和operands，将得到的过程和参数传给apply，由它执行实际的过程应用。

```scheme
(define (eval exp env)
  (cond ((self-evaluating? exp) 
         exp)
        ((variable? exp) 
         (lookup-variable-value exp env))
        ((quoted? exp) 
         (text-of-quotation exp))
        ((assignment? exp) 
         (eval-assignment exp env))
        ((definition? exp) 
         (eval-definition exp env))
        ((if? exp) 
         (eval-if exp env))
        ((lambda? exp)
         (make-procedure 
          (lambda-parameters exp)
          (lambda-body exp)
          env))
        ((begin? exp)
         (eval-sequence
          (begin-actions exp) 
          env))
        ((cond? exp) 
         (eval (cond->if exp) env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values 
                 (operands exp) 
                 env)))
        (else
         (error "Unknown expression 
                 type: EVAL" exp))))
```

这里用了eval实现为了case analysis。坏处是我们的程序只能处理一些可区别的表达式类型，加入新的类型需要修改eval的定义。大多数List实现中，表达式类型的分派是用数据导向风格来做的。

**Apply**

apply接收两个参数，一个过程和一个参数的list。它将过程分为两种，一种原始的，一种复合的。应用复合的过程通过按顺序评估过程body中的表达式。复合过程body的评估环境的构建通过扩展过程携带的base环境，加入一帧将实参绑定到过程的形参。

```scheme
(define (apply procedure arguments)
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
```

**Procedure arguments**

当eval处理过程应用时，它用了list-of-values去产生过程的参数list。（这里故意没有用高阶过程）

```scheme
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))
```



**Conditionals**

eval-if评估if表达式的谓词部分，根据结果是否为true，评估对应部分。这里true?的使用强调被实现语言和实现语言之间的连接。true?将被实现语言的结果翻译成实现语言可以测试的值。

```scheme
(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))
```

**Sequences**

eval-sequence在apply中用来评估过程body中的表达式序列

```scheme
(define (eval-sequence exps env)
  (cond ((last-exp? exps) 
         (eval (first-exp exps) env))
        (else 
         (eval (first-exp exps) env)
         (eval-sequence (rest-exps exps) 
                        env))))
```

**Assignments and definitions**

```scheme
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
```

Exercise 4.1: 注意我们无法判断自循环解释器评估参数的顺序是从左往右还是从右往左。它的评估顺序从底下的Lisp继承来：list-of-values中的cons的参数从左往右评估，list-of-values就从左往右评估参数。写一个版本的list-of-values不管底下的Lisp的评估顺序如何，总是从左往右评估参数，再写一个从右往左的版本。

```scheme
; from left to right
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let ((first (eval (first-operand exps) env)))
        (cons first (list-of-values (rest-operands exps) env)))))

; from right to left
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let ((rest (list-of-values (rest-operand exps) env)))
        (cons (eval (first-operands exps) env) rest))))
```



### Representing Expressions

解释器让我们想到了2.3.2中讨论的符号求导程序。两者都操作符号表达式，复合表达式的结果由递归地操作子表达式决定，然后根据表达式的类型用某种方式组合结果。两者都是用数据抽象解耦操作的一般规则和表达式如何表现的细节。对于求导程序这意味着带书表达式可以以前缀、中缀或后缀的形式。对于解释器意味着语言的语法只由区分和提取表达式的过程决定。

下面是我们的语言的规格

- 自评估项只有数字和字符串

  ```scheme
  (define (self-evaluating? exp)
    (cond ((number? exp) true)
          ((string? exp) true)
          (else false)))
  ```

- 变量表示为符号

  ```scheme
  (define (variable? exp) (symbol? exp))
  ```

- 引用的形式`(quote <text-of-quotation>)`

  ```scheme
  (define (quoted? exp)
    (tagged-list? exp 'quote))
  
  (define (text-of-quotation exp)
    (cadr exp))
  
  (define (tagged-list? exp tag)
    (if (pair? exp)
        (eq? (car exp) tag)
        false))
  ```

- 赋值的形式`(set! <var> <value>)`

  ```scheme
  (define (assignment? exp)
    (tagged-list? exp 'set!))
  
  (define (assignment-variable exp) 
    (cadr exp))
  
  (define (assignment-value exp) (caddr exp))
  ```

- 定义的形式

  ```scheme
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
  ```

- Lambda

  ```scheme
  (define (lambda? exp) 
    (tagged-list? exp 'lambda))
  (define (lambda-parameters exp) (cadr exp))
  (define (lambda-body exp) (cddr exp))
  
  ; constructor used by definition-value
  (define (make-lambda parameters body)
    (cons 'lambda (cons parameters body)))
  ```

- if 条件

  ```scheme
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
  ```

- begin

  ```scheme
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
  ```

- procedure application

  ```scheme
  (define (application? exp) (pair? exp))
  (define (operator exp) (car exp))
  (define (operands exp) (cdr exp))
  (define (no-operands? ops) (null? ops))
  (define (first-operand ops) (car ops))
  (define (rest-operands ops) (cdr ops))
  ```



**Derived expressions**

cond可以转成嵌套的if表达式

```scheme
(cond ((> x 0) x)
      ((= x 0) (display 'zero) 0)
      (else (- x)))

(if (> x 0)
    x
    (if (= x 0)
        (begin (display 'zero) 0)
        (- x)))
```



```scheme
(define (cond? exp) 
  (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) 
  (car clause))
(define (cond-actions clause)
  (cdr clause))
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
            (make-if (cond-predicate first)
                     (sequence->exp 
                      (cond-actions first))
                     (expand-clauses 
                      rest))))))
```

let表达式也是derived表达式。

Exercise 4.2: Louis重新排了cond子句的顺序，吧过程应用放到了赋值前面。理由是这会使解释器更加高效，因为通常程序中应用比赋值定义等要多。

1. 问题是会把其他类型的表达式当成是应用，因为我们判断应用仅仅通过`(pair? exp)`。

2. 修改过程应用语法，解决上面的问题

   ```scheme
   (define (application? exp) (tagged-list? exp 'call))
   (define (operator exp) (cadr exp))
   (define (operands exp) (cddr exp))
   ```

Exercise 4.3: 重写eval，用数据导向风格来做dispatch。

```scheme
(define lof '())
(define (put op tag func)
  (set! lof (cons (list op tag func) lof)))
(define (get op tag)
  (define (iter l)
    (cond
      ((null? l) (error "operation" op "for type" tag "not found!"))
      ((and (equal? (car (car l)) op)
            (equal? (cadr (car l)) tag))
       (caddr (car l)))
      (else (iter (cdr l)))))
  (iter lof))

(put 'eval 'quote (lambda (exp env)
                    (text-of-quotation exp)))
(put 'eval 'set! eval-assignment)
(put 'eval 'define eval-definition)
(put 'eval 'if eval-if)
(put 'eval 'lambda (lambda (exp env)
                     (make-procedure
                      (lambda-parameters exp)
                      (lambda-body exp)
                      env)))
(put 'eval 'begin (lambda (exp env)
                    (eval-sequence
                     (begin-actions exp)
                     env)))
(put 'eval 'cond (lambda (exp env)
                   (evaln (cond->if exp) env)))

(define (evaln exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp)
         (lookup-variable-value exp env))
        ((and (pair? exp) (get 'eval (car exp)))
         ((get 'eval (car exp)) exp env))
        ((application? exp)
         (applyn (eval (operator exp) env)
                (list-of-values 
                 (operands exp) 
                 env)))
        (else (error "Unknown expression 
                 type: EVAL" exp))))
```

Exercise 4.4: 回忆特殊形式and和or的定义

- and: 从左往右评估表达式。如果一个为false返回false，如果所有都为true值，返回最后表达式的值。如果没有表达式则返回true
- or: 从左往右评估表达式。如果一个为true返回其值，如果所有都为false值，或者没有表达式则返回false

安装and和or到解释器作为新的特殊形式，定义合适的语法过程和评估过程eval-and和eval-or。另一种方法，显示如何将and和or实现为推导表达式。

```scheme
(define (and? exp) 
  (tagged-list? exp 'and))
(define (and-exps exp) (cdr exp))
(define no-exps? null?)
(define (last-exp? exps) (null? (cdr exps)))
(define (first-exp exps) (car exps))
(define (rest-exps exps) (cdr exps))

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
```

Exercise 4.5: Scheme允许一种额外的cond子句语法，`(<test> => <recipient>)`。如果test求值为真值，然后将其值作为参数调用recipient过程，过程返回值作为cond表达式的值。修改cond处理逻辑，支持这种扩展语法。

```scheme
(define (make-combination operator operands)
  (cons operator operands))

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
                          (make-combination (actions-procedure actions)
                                            (list predicate)))
                         ((bad-syntax? actions)
                          (error "COND clause syntax error! => must be followed by a procedure of one argument"))
                         (else
                          (sequence->exp actions)))
                       (expand-clauses rest)))))))
```

Exercise 4.6: let表达式是派生表达式，下面两个是等效的

```scheme
(let ((⟨var₁⟩ ⟨exp₁⟩) … (⟨varₙ⟩ ⟨expₙ⟩))
  ⟨body⟩)

((lambda (⟨var₁⟩ … ⟨varₙ⟩)
   ⟨body⟩)
 ⟨exp₁⟩
 …
 ⟨expₙ⟩)
```

实现语法转换let->combination，并增加合适的子句到eval来处理let表达式

```scheme
(define (let? exp) 
  (tagged-list? exp 'let))
(define (let-vars exp)
  (map car (cadr exp)))
(define (let-inits exp)
  (map cadr (cadr exp)))
(define (let-body exp)
  (cddr exp))
(define (let->combination exp)
  (make-combination (make-lambda (let-vars exp) (let-body exp))
                      (let-inits exp)))
(define (make-let lop body) 
  (cons 'let (cons lop body)))     ;Note: body needs to be sequence of expressions
```



Exercise 4.7: `let*`类似于let，只不过每个binding都在一个环境中进行，前面的binding是可见的。解释为什么`let*`可以重写成嵌套的let表达式，然后写`let*->nested-lets`并修改eval。如果我们已经实现了let，是否只需要在eval中加一个条件就够了执行如下动作就够了`(eval (let*->nested-lets exp) env)`，还是必须显式扩展let\*为非派生的表达式？

前面的let值通过外层的lambda和参数带进去了，在应用lambda的时候碰到内层let表达式，又会进行eval如此往复，直到最里面body。如果已经实现了let，除了调用一下`let*->nested-lets`还要外面包一层`let->combination`，因为let本身也是派生类型。

```scheme
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
       ((null? lop) (sequence->exp body)) ; 统一转成非list，因为make-let返回的不是list
       (else (make-let (list (car lop))
                       (list (iter (cdr lop)))))))
    (iter lop)))
```

Exercise 4.8: 修改练习4.6，支持命名let。如下是命名let的例子

```scheme
(define (fib n)
  (let fib-iter ((a 1) (b 0) (count n))
    (if (= count 0)
        b
        (fib-iter (+ a b) 
                  a 
                  (- count 1)))))
```

修改let->combination

```scheme
(define (let? exp) 
  (tagged-list? exp 'let))
(define (named-let? exp)  ; (let <var> <bindings> <body>)
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

(define (make-define name exp)
  (list 'define name exp))
```

named let和普通的let还不一样，因为变量的值中又包含了变量名本身的引用，所以要么语言本身支持define，要么就要用Y-combination了。

Exercise 4.9: Scheme中经常将迭代表示为普通的过程调用，所以特殊的迭代构造没有能计算能力带来什么本质的增益。另一方面，这样的构造很容易实现。设计一些迭代器构造，给出使用例子，显示如何将它们实现为派生表达式。

派生为combination，其operator一个lambda，参数是迭代的计数器，body是原循环体的内容加上计数器的判断和更新代码，operands为初始计数。

Exercise 4.10: 通过数据抽象，我们可以写出独立于待解释语言特定语法的eval过程。为了阐明这个，设计和实现一个新的语法，只修改本节中的过程，不修改eval或apply。

### Evaluator Data Structures

除了定外部的表达式语法，解释器实现也必须定义解释器内部操作的数据结构，为程序执行的一部分，例如过程和环境的表示、true和false的表示。

**Testing of predicates**

对于条件，除了显示的false对象，其他都为真

```scheme
(define (true? x)
  (not (eq? x false)))

(define (false? x)
  (eq? x false))
```

**Representing procedures**

处理内部过程，我们假设下面这些过程可用

```scheme
(apply-primitive-procedure ⟨proc⟩ ⟨args⟩)
(primitive-procedure? ⟨proc⟩)
```

复合过程由构造器make-procedure来构造

```scheme
(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))
```

**Operations on Environments**

```scheme
(lookup-variable-value ⟨var⟩ ⟨env⟩)

(extend-environment ⟨variables⟩ ⟨values⟩ ⟨base-env⟩)

(define-variable! ⟨var⟩ ⟨value⟩ ⟨env⟩)

(set-variable-value! ⟨var⟩ ⟨value⟩ ⟨env⟩)
```



```scheme
; 为了实现这些操作，我们将环境表示为一个帧的list，一个环境的enclosing环境是这个list的cdr，空环境是空list。
(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

; 每个帧表达式为一个lists的pair，一个变量list和一个关联值的list。
(define (make-frame variables values)
  (cons variables values))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

; 扩展环境，加一帧
(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" 
                 vars 
                 vals)
          (error "Too few arguments supplied" 
                 vars 
                 vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop 
              (enclosing-environment env)))
            ((eq? var (car vars))
             (car vals))
            (else (scan (cdr vars) 
                        (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop 
              (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) 
                        (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable: SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! 
              var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) 
                        (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))
```

这只是一种简单的表现，并不高效。生产环境中解释器的环境操作对性能有很大的影响，尤其是变量的查找。

Exercise 4.11: 我们也可以将帧表示为一个bindings的list，重新写环境操作使用这种表示。

```scheme
; 每个帧表达式为一个lists的pair，一个变量list和一个关联值的list。
(define (make-frame variables values)
  (cons 'frame
        (map cons variables values)))
(define (frame-bindings frame)
  (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-cdr! frame (cons (cons var val)
                        (frame-bindings frame))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan bindings)
      (cond ((null? bindings)
             (env-loop 
              (enclosing-environment env)))
            ((eq? var (caar bindings))
             (cdar bindings))
            (else (scan (cdr bindings)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (scan (frame-bindings (first-frame env)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan bindings)
      (cond ((null? bindings)
             (env-loop 
              (enclosing-environment env)))
            ((eq? var (caar bindings))
             (set-cdr! (car bindings) val))
            (else (scan (cdr bindings)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable: SET!" var)
        (scan (frame-bindings (first-frame env)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan bindings)
      (cond ((null? bindings)
             (add-binding-to-frame! 
              var val frame))
            ((eq? var (caar bindings))
             (set-cdr! (car bindings) val))
            (else (scan (cdr bindings)))))
    (scan (frame-bindings frame))))
```

Exercise 4.12: 查找设置定义变量三个函数可以表达为更加抽象的过程用于遍历环境结构。定义抽象捕捉这个公共的模式，并且重新定义这三个过程。

```scheme
(define (env-loop env var next op)
  (define (iter env)
    (define (scan vars vals)
      (cond ((null? vars)
             (if next
                 (next env)
                 (iter (enclosing-environment env))))
            ((eq? var (car vars))
             (op vals))
            (else (scan (cdr vars)
                        (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (iter env))

(define (lookup-variable-value var env)
  (env-loop env var #false car))

(define (set-variable-value! var val env)
  (env-loop env var #false
            (lambda (vars) (set-car! vars val))))

(define (define-variable! var val env)
  (env-loop env var
            (lambda (env)
              (add-binding-to-frame! var val
                                     (first-frame env)))
            (lambda (vars)
              (set-car! vars val))))

; based on 4.11
(define (env-loop env var next op)
  (define (iter env)
    (define (scan bindings)
      (cond ((null? bindings)
             (if next
                 (next op)
                 (iter (enclosing-environment env))))
            ((eq? var (caar bindings))
             (op bindings))
            (else (scan (cdr bindings)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (scan (frame-bindings (first-frame env)))))
  (iter env))

(define (lookup-variable-value var env)
  (env-loop env var #false cdar))

(define (set-variable-value! var val env)
  (env-loop env var #false
            (lambda (bindings)
              (set-cdr! (car bindings) val))))

(define (define-variable! var val env)
  (env-loop env var
            (lambda (env)
              (add-binding-to-frame!
               var val (first-frame env)))
            (lambda (bindings)
              (set-cdr! (car bindings) val))))
```



Exercise 4.13: Scheme允许我们用define创建新绑定，但是没有提供解绑的方法。为解释器实现一个特殊形式make-unbound!从该表达式被评估的环境中移除一个给定符号的绑定。这个问题并没有完全明确。例如，我们是否应该只移除环境第一帧中的绑定。完成规格并证明你的选择是合理的。

```scheme
(define (unbound? exp)
  (tagged-list? exp 'make-unbound!))

(define (unbound-variable exp)
  (cadr exp))

(define (eval-unbound exp env)
  (unbound-variable!
   (unbound-variable exp)
   env)
  'ok)

(define (unbound-variable! var env)
  (define (unbound vars vals)
    (cond ((null? vars) (cons '() '()))
          ((eq? var (car vars))
           (cons (cdr vars) (cdr vals)))
          (else
           (let ((res (unbound (cdr vars) (cdr vals))))
             (cons (cons (car vars) (car res))
                   (cons (car vals) (cdr res)))))))
  (if (eq? env the-empty-environment)
      (void) ; don't raise error
      (let* ((frame (first-frame env))
             (res (unbound (frame-variables frame)
                           (frame-values frame))))
        (set-car! frame (car res))
        (set-cdr! frame (cdr res)))))

; based on 4.11
(define (unbound-variable! var env)
  (define (unbound bindings)
    (cond ((null? bindings) '())
          ((eq? var (caar bindings))
           (cdr bindings))
          (else
           (let ((res (unbound (cdr bindings))))
             (cons (car bindings) res)))))
  (if (eq? env the-empty-environment)
      (void) ; don't raise error
      (let* ((frame (first-frame env))
             (res (unbound (frame-bindings frame))))
        (set-cdr! frame res))))
```

只移除第一帧中的绑定，因为我们没有权利接触封闭环境中的绑定，因为就算我不想用它，别人可能还需要用它。另外我们之所以需要unbound是因为想引用外层的绑定。



### Running the Evaluator as a Program

对于原始过程，我们的解释器程序直接调用底层的Lisp。全局环境创建原始过程的绑定和true/false的绑定。

```scheme
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
        ⟨more primitives⟩ ))

(define (primitive-procedure-names)
  (map car primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) 
         (list 'primitive (cadr proc)))
       primitive-procedures))

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))
```

建模read-eval-print循环

```scheme
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
```

为了在racket环境支持可变list，需要最一些改动，frame相关操作用mlist/mcons

```scheme
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
  (env-loop env var #false first-variable))

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
      (void) ; don't raise error
      (let* ((frame (first-frame env))
             (res (unbound (frame-variables frame)
                           (frame-values frame))))
        (set-frame-variables! frame (frame-variables res))
        (set-frame-values! frame (frame-values res)))))
```

Exercise 4.14: Eva自己定义了map，跑了一些测试都ok。Louis安装了系统的map，但是测试结果却很糟糕。解释为什么。

因为我们自己定义的过程的内部数据格式跟系统的不一样，所以没法使用系统的高阶函数。传给系统的map函数的参数已经是经过我们eval解析的了。

### Data as Programs

我们可以将程序看成是一个机器的描述，而evaluator是一个特殊的机器，它接收一个机器的描述作为输入，然后配置它自己来模仿被描述的机器。从这个角度看，解释器可以看成是一个全能机。它是编程语言本身和编程语言操作的数据对象的桥梁。对于用户来说是程序，对于解释器来说是数据。当然这不是绝对的，如果语言提供给用户显式evaluate数据对象的能力。



Exercise 4.15: 写一个过程halts?对于任何过程p和对象a判断p是否halt在a上是不可能的。反例就是(try try)

```scheme
(define (run-forever)
  (run-forever))

(define (try p)
  (if (halts? p p)
      (run-forever)
      'halted))
```

### Internal Definitions

我们目前是顺序定义的，内部定义要同时定义的话，就先扫描body所有的内部定义在开头创建let，然后通过赋值设置值。例如

```scheme
(lambda ⟨vars⟩
  (define u ⟨e1⟩)
  (define v ⟨e2⟩)
  ⟨e3⟩)

; transformed into
(lambda ⟨vars⟩
  (let ((u '*unassigned*)
        (v '*unassigned*))
    (set! u ⟨e1⟩)
    (set! v ⟨e2⟩)
    ⟨e3⟩))
```

Exercise 4.16: 实现前面描述的内部定义

1. 修改lookup-variable-value当查找的值是`*unassigned*`时出错。
2. 写过程scan-out-defines接收过程body为参数返回等效的没用内部定义的body，通过上述的转换。（设置值需要放到最前面么，如果是的话为什么不直接在let的变量中设置呢？）
3. 安装scan-out-defines到解释器中，要么make-procedure要么procedure-body，哪个位置更好？

```scheme
(define (lookup-variable-value var env)
  (env-loop env var #false
            (lambda (vals)
              (let ((val (first-value vals)))
                (if (eq? '*unassigned* val)
                    (error "use variable before it's defined" var)
                    val)))))

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
          newbody
          (list (make-let vvs newbody))))))
```

make-procedure中更好，procedure-body是在应用的时候，一个过程可能会应用很多次。

这样修改之后，如果当前环境定义了一个变量，就无法再引用外层的同名变量了。

Exercise 4.17: 换成let之后多了一层环境，如何不构建额外的帧实现同时的scope rule。在最前面`(define var '*unassigned*) `，然后原来的define替换成set!。

Exercise 4.18: 下面的修改有问题

```scheme
(lambda ⟨vars⟩
  (let ((u '*unassigned*)
        (v '*unassigned*))
    (let ((a ⟨e1⟩)
          (b ⟨e2⟩))
      (set! u a)
      (set! v b))
    ⟨e3⟩))
```

考虑solve过程，在求值b的时候因为y还是未定义的。

```scheme
(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)
```

Exercise 4.19: 下面的程序结果是什么

```scheme
(let ((a 1))
  (define (f x)
    (define b (+ a x))
    (define a 5)
    (+ a b))
  (f 10))
```

按照最初的顺序定义，b是11，a是5，结果是11。按照修改后的再set! b时a还没定义，应该报错。另一种观点是，如果a和b真的是同时定义，那么结果应该是20。

对于函数定义没用关系，因为body的评估是延迟的。对于非函数定义，根据依赖关系进行排序。



Exercise 4.20: letrec的初始值是在包含所有binding的环境中评估的，所以允许递归。

```scheme
(define (f x)
  (letrec
      ((even?
        (lambda (n)
          (if (= n 0)
              true
              (odd? (- n 1)))))
       (odd?
        (lambda (n)
          (if (= n 0)
              false
              (even? (- n 1))))))
    ⟨rest of body of f⟩))
```



1. 实现letrec为派生表达式，转成let表达式。变量由let创建，然后用set!赋值。
2. Louis表示如果不喜欢在过程中用define，可以用let。画环境图阐明他推理的问题，(f 5)使用let和letrec时的环境图。

```scheme
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
```

let版本，even?和odd?的lambda是在f的body中评估创建的，其环境是f。而letrec版本，lambda要等到lambda的body中评估set!时才创建，所以其环境是let的lambda，能够查到变量的定义。

Exercise 4.21: Louis的直觉是正确的，不用letrec或define来定义递归函数的确是可能的。下面的表达式计算阶乘

````scheme
((lambda (n)
   ((lambda (fact) (fact fact n))
    (lambda (ft k)
      (if (= k 1)
          1
          (* k (ft ft (- k 1)))))))
 10)
````

**基本思想就是不直接给函数一个变量名，而是通过参数把lambda本身传进去。**

想出一个类似的计算Fibonacci数的表达式

```scheme
((lambda (n)
   ((lambda (fibon) (fibon fibon 0 0 0))
    (lambda (fib i a b)
      (cond
       ((> i n) b)
       ((< i 2) (fib fib (+ i 1) b 1))
       (else (fib fib (+ i 1) b (+ a b)))))))
 10)
```

对于下面的函数不使用内部定义或letrec完成定义

```scheme
(define (f x)
  (define (even? n)
    (if (= n 0)
        true
        (odd? (- n 1))))
  (define (odd? n)
    (if (= n 0)
        false
        (even? (- n 1))))
  (even? x))

(define (f x)
  ((lambda (even? odd?)
     (even? even? odd? x))
   (lambda (ev? od? n)
     (if (= n 0) 
         true 
         (od? ev? od? (- n 1))))
   (lambda (ev? od? n)
     (if (= n 0) 
         false 
         (ev? ev? od? (- n 1))))))
```

扩展到通用的例子（一个参数版本）

```scheme
 ;non-recursive factorial function 
 (define fact-once 
    (lambda (f) 
      (lambda (n) 
        (if (= n 0) 
            1 
            (* n (f (- n 1))))))) 
  
 ;y-combinator 
 (define Y  
   (lambda (f) 
     ((lambda (x) (x x)) 
      (lambda (x) (f (lambda (y) ((x x) y))))))) 
 ; fact-once多了一层lambda，用于传递递归函数
 ; 这是为了不让用户显式传递函数，将这个传递的动作封装在Y-combinator中
 ; (x x)就是去掉这个增加的一层，得到内部实际的lambda
 
 ; 假设用户自己传递参数，我们是不需要Y-combinator
  (define fact-once 
      (lambda (f n) 
        (if (= n 0) 
            1 
            (* n (f f (- n 1)))))) 
```



### Separating Syntactic Analysis from Execution

上面的解释器很简单，但是效率很低，因为表达式的语法分析和它们的执行交织在一起。所以如果一个程序执行很多次，它的语法也要分析很多次。

我们将接收表达式和环境的eval分成两部分，过程analyze只接收表达式，它执行语法分析返回一个新的过程，我们叫它执行过程。执行过程接收环境作为参数并完成解释。这样analyze只需执行一次，而执行过程可以重复调用。

```scheme
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
        (bproc (analyze-sequence 
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

; scan-out-defines和make-procedure需要修改一下，analyze-sequence放到scan-out-defines中
(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (scan-out-defines
                (lambda-body exp))))
    (lambda (env) 
      (make-procedure vars bproc env))))

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
```

Exercise 4.23: analyze-sequence为什么需要这么复杂，她觉得可以像下面这样？比较body中有一个和两个表达式时两者的区别。

```scheme
(define (analyze-sequence exps)
  (define (execute-sequence procs env)
    (cond ((null? (cdr procs)) 
           ((car procs) env))
          (else ((car procs) env)
                (execute-sequence 
                 (cdr procs) env))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence: 
                ANALYZE"))
    (lambda (env) 
      (execute-sequence procs env))))
```

前面文中的版本在分析时做了更多的工作，这里的版本分析了单个的表达式，但是没有分析序列本身，在执行时每个表达式都需要判断是否非空。

Exercise 4.24: 设计并执行一些实验比较两个版本解释器的速度。使用结果来评估不同过程在分析和执行所花时间的比值。

```scheme
 (define (loop n) 
     (if (> n 0) 
         (loop (- n 1)))) 
  
 (loop 1000000) 
 ; 优化前10s左右，优化后5s
 
  (define (fib n) 
     (if (< n 2) 
         1 
         (+ (fib (- n 1)) (fib (- n 2))))) 
  
 (fib 30) 
  ; 优化前30s左右，优化后15s
```



## 4.2 Variations on a Scheme — Lazy Evaluation

新语言经常通过写一个解释器嵌入到在现存的高级语言上来发明。除了方便测试、调试，也使得设计者可以直接偷现存语言中的特性。

### Normal Order and Applicative Order

过程在某个参数上strict或non-strict，基本跟normal order/applicative order同样的意思，除了当特指某个过程的时候。

将过程做成non-strict非常有用的一个突出的例子是cons（或者几乎任何数据结构的构造器）。即使你不知道元素的值，你也可以做有用的计算和组合元素形成数据结构并操作结果的数据结构。这是很有道理的。例如计算list的长度而不需要知道单个元素的值。

Exercise 4.25: 在应用序Scheme中我们定义factorial关于前面的define如下

```scheme
(define (unless condition 
                usual-value 
                exceptional-value)
  (if condition 
      exceptional-value 
      usual-value))

(define (factorial n)
  (unless (= n 1)
          (* n (factorial (- n 1)))
          1))
```

然后我们尝试评估(factorial 5)会发生什么？在正常序语言中定义能正常工作吗？

会无限循环，unless会先求值所有参数。正常序并不一定表示lazy，它只是展开过程到不能展开再进行解释，所以还是会无限循环。

Exercise 4.26: Ben和Alyssa在lazy evalution对实现像unless这样的函数上的重要性产生了分歧。Ben觉得可以在应用序中将unless实现为关键字。Alyssa反驳道，这样地话unless将仅仅是语法，而不是可以跟高阶过程一起使用的过程。补充两方的论点。展示如何实现unless为派生表达式。给出一个unless作为过程有用的一个例子。

可以实现为派生表达式，两个表达式只会在运行时执行一个。作为过程可以和map这种高阶函数配合进行过滤。

```scheme
;; (define select-y '(#t #f #t #t))
;; (define xs '(1 3 5 7))
;; (define ys '(2 4 6 8))
;; (define selected (map unless select-y xs ys))
```



### An Interpreter with Lazy Evaluation

本节实现正常序，除了复合函数对每个参数都是non-strict的，其他同Scheme。原生函数还是strict的。

基本思想是应用过程时，解释器必须决定是否需要评估参数。延迟的参数转换成叫做thunks的对象，它包含产生参数值所必须的信息，就像它已经在应用时被评估了一样。所以thunk需要保护参数的表达式和过程应用被评估的环境。评估thunk中的表达式叫做forcing，只有当它的值需要的时候才force（传给原生过程、条件的谓词、作为应用的operator）。另外一个设计选择是是否memoize thunk。

**Modifying the evaluator**

lazy解释器跟4.1节中的最大的不同是eval和apply中过程应用的处理。

```scheme
((application? exp)
 (apply (actual-value (operator exp) env)
        (operands exp)
        env))

(define (actual-value exp env)
  (force-it (eval exp env)))

(define (apply procedure arguments env)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure
          procedure
          (list-of-arg-values 
           arguments 
           env)))  ; changed
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           (list-of-delayed-args 
            arguments 
            env)   ; changed
           (procedure-environment procedure))))
        (else (error "Unknown procedure 
                      type: APPLY" 
                     procedure))))

(define (list-of-arg-values exps env)
  (if (no-operands? exps)
      '()
      (cons (actual-value 
             (first-operand exps)
             env)
            (list-of-arg-values 
             (rest-operands exps)
             env))))

(define (list-of-delayed-args exps env)
  (if (no-operands? exps)
      '()
      (cons (delay-it 
             (first-operand exps) 
             env)
            (list-of-delayed-args 
             (rest-operands exps)
             env))))

(define (eval-if exp env)
  (if (true? (actual-value (if-predicate exp) 
                           env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

(define input-prompt  ";;; L-Eval input:")
(define output-prompt ";;; L-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (actual-value 
                   input
                   the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))
```



**Representing thunks**

```scheme
(define (force-it obj)
  (if (thunk? obj)
      (actual-value (thunk-exp obj) 
                    (thunk-env obj))
      obj))

(define (delay-it exp env)
  (list 'thunk exp env))
(define (thunk? obj) (tagged-list? obj 'thunk))
(define (thunk-exp thunk) (cadr thunk))
(define (thunk-env thunk) (caddr thunk))
```

actual-value调force-it，force-it再调用actual-value是不是有点重复了。没有。

注意看delay-it的地方，thunk是被放到环境中作为实参了，所以在actual-value的时候是先eval形参获取到值。这个值可能是thunk也可能不是，需要force-it一下。返回值可能还是thunk，所以在force-it里也用了actual-value。

memoize thunk

```scheme
(define (force-it obj)
  (cond ((thunk? obj)
         (let ((result
                (actual-value 
                 (thunk-exp obj)
                 (thunk-env obj))))
           (set-thunk-evaluated! obj)
           ;; replace exp with its value:
           (set-thunk-value! obj result) 
           ;; forget unneeded env:
           (clear-thunk-env! obj) 
           result))
        ((evaluated-thunk? obj)
         (thunk-value obj))
        (else obj)))

(define (delay-it exp env)
  (mlist 'thunk exp env))
(define (thunk? obj) (tagged-mlist? obj 'thunk))
(define (thunk-exp thunk) (mcar (mcdr thunk)))
(define (thunk-env thunk) (mcar (mcdr (mcdr thunk))))
(define (evaluated-thunk? obj)
  (tagged-mlist? obj 'evaluated-thunk))
(define (thunk-value evaluated-thunk) 
  (mcar (mcdr evaluated-thunk)))
(define (set-thunk-evaluated! obj)
  (set-mcar! obj 'evaluated-thunk))
(define (set-thunk-value! obj val)
  (set-mcar! (mcdr obj) val))
(define (clear-thunk-env! obj)
  (set-mcdr! (mcdr obj) '()))
(define (tagged-mlist? exp tag)
  (if (mpair? exp)
      (eq? (mcar exp) tag)
      false))
```



Exercise 4.27: 假设我们输入下面的定义到lazy evaluator中。

```scheme
(define count 0)
(define (id x) (set! count (+ count 1)) x)
```

给出下面的交互中缺失的值，并解释。

```scheme
(define w (id (id 10)))

;;; L-Eval input:
count

;;; L-Eval value:
⟨response⟩

;;; L-Eval input:
w

;;; L-Eval value:
⟨response⟩

;;; L-Eval input:
count

;;; L-Eval value:
⟨response⟩

;;; L-Eval input:
w

;;; L-Eval value:
⟨response⟩

;;; L-Eval input:
count

;;; L-Eval value:
⟨response⟩

; no memoize answer
1  ;(list 'thunk '(id 10) env)
10 ;forced
2  ;
10 ;forced
3  

; memoize answer
1  ;(list 'thunk '(id 10) env)
10 ; forced
2  ;
10 ; return result directly
2
```

Exercise 4.28: eval使用了actual-value而不是eval来评估operator，以force operator的值。给一个例子演示这个forcing的必要性。

```scheme
(define (calculator op a b)
  (op a b))

(calculator + 1 1)
```

Exercise 4.29: 计算fib的没有memoization更慢。下面的交互，其中id是上一练习中id，count从0开始。分别给出有无memoization时的结果

```scheme
(define (square x) (* x x))

;;; L-Eval input:
(square (id 10))

;;; L-Eval value:
⟨response⟩

;;; L-Eval input:
count

;;; L-Eval value:
⟨response⟩

;有memoization
100
1
;没有memoization
100
2
```

Exercise 4.30: Cy担心有些副作用可能永远无法发生，因为lazy evaluation不会force一个sequence中的表达式。一个序列中除了最后一个的值，其他表达式的值都不会被用到，也就不会force。所以Cy认为在求值表达式的时候必须force序列中的所有表达式，除了最后一个。

```scheme
(define (eval-sequence exps env)
  (cond ((last-exp? exps) 
         (eval (first-exp exps) env))
        (else 
         (actual-value (first-exp exps) 
                       env)
         (eval-sequence (rest-exps exps) 
                        env))))
```

1. Ben觉得Cy不对。下面for-each可以正常工作。

```scheme
(define (for-each proc items)
  (if (null? items)
      'done
      (begin (proc (car items))
             (for-each proc 
                       (cdr items)))))

;;; L-Eval input:
(for-each
 (lambda (x) (newline) (display x))
 (list 57 321 88))
57
321
88

;;; L-Eval value:
done
```

这是因为这里，都是primitive过程，直接就force评估了。

2. Cy说这没错，但是不是他想得这种程序。

```scheme
(define (p1 x)
  (set! x (cons x '(2))) x)

(define (p2 x)
  (define (p e) e x)
  (p (set! x (cons x '(2)))))
```

评估(p1 1)和(p2 1)对于两个版本的eval-sequence将产生不同的结果。原始版本，'(1 2)和1。修改版都是'(1 2)

3. Cy也指出用他的版本的eval-sequence不影响1里面的结果。解释为什么？因为force一下不是thunk的结果也没什么影响
4. 你觉得序列应该怎么处理？你喜欢Cy的方法，文中的方法还是其他方法。

我觉得Cy的方法更好。因为序列中前面的表达式就是为了副作用存在的，应确保这些副作用发生。

Exercise 4.31: 本节方法的一个不太愉快的地方是它是一个不兼容Scheme的修改。可能将lazy evalutIon实现为一个upward-compatible扩展更加好。我们可以通过扩展过程声明的语法来让用户控制参数是否要delay以及是否要memoization。例如`(define (f a (b lazy) c (d lazy-memo))...)`。设计和实现这个修改，实现新的过程define语法，重新安装eval和apply觉得是否delay或force以及是否保存。

```scheme
; 每个参数多了一个对应的类型，保存到哪呢？
; 还是保留原来的过程parameters中，评估过程的时候提取，根据类型决定是否delay或delay-memo
; delay和force需要相应修改

(define (applyn procedure arguments env)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure
          procedure
          (list-of-arg-values 
           arguments 
           env)))  ; changed
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameter-names procedure)
           (list-of-delayed-args
            (procedure-parameter-types procedure)
            arguments 
            env)   ; changed
           (procedure-environment procedure))))
        (else (error "Unknown procedure 
                      type: APPLY" 
                     procedure))))

(define (list-of-delayed-args types exps env)
  (let ((lent (length types))
        (lene (length exps)))
    (define (iter types exps)
      (cond
       ((null? types) '())
       (else (let ((type (car types))
                   (rtypes (cdr types))
                   (exp (first-operand exps))
                   (rexps (rest-operands exps))
                   (arg #f))
               (cond
                ((eq? 'normal type)
                 (set! arg (actual-value exp env)))
                ((eq? 'lazy type)
                 (set! arg (delay-it exp env)))
                ((eq? 'lazy-memo type)
                 (set! arg (delay-memo-it exp env)))
                (else (error "unknown parameter type" type)))
               (cons arg (iter rtypes rexps))))))
    (cond
     ((= lent lene) (iter types exps))
     ((< lent lene) (error "Too many arguments supplied. Expected" lent ", Given" lene))
     (else (error "Too few arguments supplied. Expected" lent ", Given" lene)))))

(define (procedure-parameter-names p)
  (map (lambda (x) (if (pair? x) (car x) x))
       (procedure-parameters p)))

(define (procedure-parameter-types p)
  (map (lambda (x) (if (pair? x) (cadr x) 'normal))
       (procedure-parameters p)))

(define (force-it obj)
  (cond ((thunk? obj)
         (actual-value (thunk-exp obj) (thunk-env obj)))
        ((mthunk? obj)
         (let ((result (actual-value (mthunk-exp obj) (mthunk-env obj))))
           (set-mthunk-evaluated! obj)
           ;; replace exp with its value:
           (set-mthunk-value! obj result)
           ;; forget unneeded env:
           (clear-mthunk-env! obj)
           result))
        ((evaluated-thunk? obj) (evaluated-thunk-value obj))
        (else obj)))

(define (delay-it exp env)
  (list 'thunk exp env))
(define (thunk? obj) (tagged-list? obj 'thunk))
(define (thunk-exp thunk) (car (cdr thunk)))
(define (thunk-env thunk) (car (cdr (cdr thunk))))

(define (delay-memo-it exp env)
  (mlist 'mthunk exp env))
(define (mthunk? obj) (tagged-mlist? obj 'mthunk))
(define (mthunk-exp thunk) (mcar (mcdr thunk)))
(define (mthunk-env thunk) (mcar (mcdr (mcdr thunk))))
(define (set-mthunk-evaluated! obj)
  (set-mcar! obj 'evaluated-thunk))
(define (set-mthunk-value! obj val)
  (set-mcar! (mcdr obj) val))
(define (clear-mthunk-env! obj)
  (set-mcdr! (mcdr obj) '()))

(define (evaluated-thunk? obj)
  (tagged-mlist? obj 'evaluated-thunk))
(define (evaluated-thunk-value evaluated-thunk) 
  (mcar (mcdr evaluated-thunk)))

(define (tagged-mlist? exp tag)
  (if (mpair? exp)
      (eq? (mcar exp) tag)
      false))
```

测试

```scheme
(define x 0)
(define y 0)
(define z 0)

(define (foo a (b lazy) (c lazy-memo))
  (lambda (n)
    (cond
     ((= 0 n) a)
     ((= 1 n) b)
     ((= 2 n) c))))
; x,y,z: 0,0,0
(define result
  (foo (begin (set! x (+ x 1)) x)
       (begin (set! y (+ y 1)) y)
       (begin (set! z (+ z 1)) z)))
; x,y,z: 1,0,0
(result 0) ; 1
; x,y,z: 1,0,0
(result 1) ; 1
; x,y,z: 1,1,0
(result 2) ; 1
; x,y,z: 1,1,1
(result 0) ; 1
; x,y,z: 1,1,1
(result 1) ; 2
; x,y,z: 1,2,1
(result 2) ; 1
; x,y,z: 1,2,1
```



### Streams as Lazy Lists

之前3.5.1节中我们展示了如何实现流为延迟的lists，通过引入特殊形式的delay和cons-stream。但是这种方法很棘手。一方面特殊形式不是像过程一样的一等对象，没法和高阶函数一起使用。另一方面是我们必须创建流为类似但不同于lists的新数据对象类型，所以必须重新实现很多流版本普通的list操作。

有了lazy evaluation（所有参数都lazy的版本），流和list就可以相同了，不用特殊形式，也不同分离list和流操作。我们所要做的就是安排东西使cons是non-strict的。一种方法是扩展lazy evaluator允许non-strict的原语，并实现cons为其中之一。一个更简单的方法是，回忆我们2.1.3节中没有根本不需要将cons实现为原语。我们可以将pair表现为过程。

```scheme
(define (cons x y) (lambda (m) (m x y)))
(define (car z) (z (lambda (p q) p)))
(define (cdr z) (z (lambda (p q) q)))
```

基于这些基本操作，list操作的标准定义将可以和无限list（流）和有限list工作，并且流的操作可以实现为list操作。下面是几个例子。

```scheme
(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(define (map proc items)
  (if (null? items)
      '()
      (cons (proc (car items))
            (map proc (cdr items)))))

(define (scale-list items factor)
  (map (lambda (x) (* x factor))
       items))

(define (add-lists list1 list2)
  (cond ((null? list1) list2)
        ((null? list2) list1)
        (else (cons (+ (car list1) 
                       (car list2))
                    (add-lists
                     (cdr list1) 
                     (cdr list2))))))

(define ones (cons 1 ones))

(define integers 
  (cons 1 (add-lists ones integers)))
```

这里甚至比第三章的流更加lazy，因为连car也delay了。事实上即使访问一个lazy pair的car或cdr也不需要force一个list元素的值。值只有在真正被需要的时候才被force，例如作为primitive的参数或者作为结果打印。

lazy pairs也解决了3.5.4中显式delay操作往外扩散的问题。例如3.5.4中的积分和解微分方程的程序可以实现为

```scheme
(define (integral integrand initial-value dt)
  (define int
    (cons initial-value
          (add-lists (scale-list integrand dt) 
                     int)))
  int)

(define (solve f y0 dt)
  (define y (integral dy y0 dt))
  (define dy (map f y))
  y)

;;; L-Eval input:
(list-ref (solve (lambda (x) x) 1 0.001) 1000)

;;; L-Eval value:
2.716924
```

Exercise 4.32: 给出例子说明第三章的流和本节中更懒的lazy lists的区别。你能怎么利用这种额外的懒惰。

因为car也是lazy的，可以构建lazy tree。

Exercise 4.33: Ben发现上面的lazy list实现评估下面的表达式竟然出错了。思考之后他意识到从引用表达式读进来的list和用我们新定义的cons/car/cdr操作的list是不同的。修改解释器对引用表达式的处理以使在driver loop中输入引用lists将产生真正的lazy lists

```scheme
(car '(a b c))
```

构造嵌套的lambda，类似分析评估分离版解释器中对sequence的处理。

```scheme
;eval
((quoted? exp) 
         (text-of-quotation exp env))
((null? exp) '())

(define (text-of-quotation exp env)
  (let ((text (cadr exp)))
    (if (pair? text)
        (eval (pair->lambda text) env)
        text)))

; 可以改成cons，然后eval
(define (pair->lambda p)
  (let ((a (car p))
        (d (cdr p)))
    (cond
     ((pair? d)
      (make-lambda-pair (list 'm)
                        (list (list 'm a (pair->lambda d)))))
     (else
      (make-lambda-pair (list 'm)
                        (list (list 'm a d)))))))

; test
(driver-loop)
(define (cons x y) (lambda (m) (m x y)))
(define (car z) (z (lambda (p q) p)))
(define (cdr z) (z (lambda (p q) q)))
'(1 2 3)
(car '(1 2 3))
(cdr '(1 2 3))
(car (cdr '(1 2 3)))
(cdr (cdr (cdr '(1 2 3))))

(car '(1 . 2))
(cdr '(1 . 2))

(define ones (cons 1 ones))
(car ones)
(cdr ones)
```



Exercise 4.34: 修改driver-loop以使lazy pairs和lists将以合理的方式打印。（你打算怎么处理无限lists？）你可能也需要修改lazy pairs的表示以使解释器可以识别它们以打印它们。（因为实际上是用的lambda，所以需要和普通的函数区别开来）

```scheme
(define (text-of-quotation exp env)
  (let ((text (cadr exp)))
    (if (pair? text)
        (eval (pair->cons text) env)
        text)))

; 可以改成cons，然后eval
(define (pair->cons p)
  (define (iter p)
    (cond
      ((null? p) '())
      ((pair? p) (list 'cons (list 'quote (car p)) (iter (cdr p))))
      (else (list 'quote p))))
  (iter p))

(define (lazy-pair->pair lp)
  (define (first exp) (cadr exp))
  (define (rest exp) (caddr exp))
  (define (iter lp lexp count)
    (let ((proc (lazy-pair-proc lp)))
      (let ((exp (car (procedure-body proc)))
          (env (procedure-environment proc)))
      (let ((a (actual-value (first exp) env))
            (d (actual-value (rest exp) env))
            (rexp (thunk-exp (lookup-variable-value (rest exp) env))))
        ; need to traverse car if it's pair as well
        (if (lazy-pair? d)
            (let ((aa (if (lazy-pair? a)
                          (iter a #f 0)
                          a)))
              (if (eq? rexp lexp)
                  (if (= count 5)
                      (cons aa '...)
                      (cons aa (iter d rexp (+ count 1))))
                  (cons aa (iter d rexp 0))))
            (cons (if (lazy-pair? a)
                      (iter a #f 0)
                      a)
                  d))))))
  (iter lp #f 0))

(define primitive-procedures
  (list (list 'ocar car)  ; we need to save the primitive ones
        (list 'ocdr cdr)
        (list 'ocons cons)
        (list 'opair? pair?)
        (list 'null? null?)
        (list 'not not)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list '> >)
        (list '< <)
        (list '= =)
        (list 'eq? eq?)
        (list 'equal? equal?)
        ))

; standard procedures
(define standard-procedures
  (list
   '(define (cons x y)
      (ocons 'lazy-pair (lambda (m) (m x y))))
   '(define (car z)
      ((ocdr z) (lambda (p q) p)))
   '(define (cdr z)
      ((ocdr z) (lambda (p q) q)))
   '(define (pair? z)
      (and (opair? z) (eq? (ocar z) 'lazy-pair)))
   ))
; lazy-pair
(define (lazy-pair? expr) (tagged-list? expr 'lazy-pair))
(define (lazy-pair-proc p) (cdr p))

(map (lambda (exp)
       (eval exp the-global-environment))
     standard-procedures)

(define (user-print object)
  (cond
    ((lazy-pair? object)
     (display (lazy-pair->pair object)))
    ((compound-procedure? object)
     (display 
      (list 'compound-procedure
            (procedure-parameters object)
            (procedure-body object)
            '<procedure-env>)))
    (else (display object))))

; test
(driver-loop)

'(1 2)
'(1 . 2)
(define x 1)
(define y 2)
'(x y)

(cons 1 (cons 2 3))
(cons 1 (cons 2 '()))
(car (cons 1 (cons 2 3)))
(cdr (cons 1 (cons 2 3)))
(car (cons 1 (cons 2 '())))
(cdr (cons 1 (cons 2 '())))
(cdr (cdr (cons 1 (cons 2 '()))))
(cdr (cdr (cons 1 (cons 2 3))))

(define (add-lists list1 list2)
  (cond ((null? list1) list2)
        ((null? list2) list1)
        (else (cons (+ (car list1) 
                       (car list2))
                    (add-lists
                     (cdr list1) 
                     (cdr list2))))))

(define ones (cons 1 ones))
(define integers 
  (cons 1 (add-lists ones integers)))

ones
(car ones)
(cdr ones)
(cons 1 (cons 2 (cons 3 (cons 4 (cons 5 (cons 6 7))))))

integers

(define (fibgen a b)
  (cons a (fibgen b (+ a b))))
(define fibs (fibgen 0 1))
fibs

(cons (cons 1 2) (cons 3 4))
(cons ones ones)
(cons integers integers)
(cons (cons ones integers) (cons integers ones))
```

优化不要用lambda，而是用符号'cons表示。eval不用改，car/cdr实现中取对应lambda。打印的时候识别'cons。

car也需要遍历，count通过参数传递。





## 4.3 Variations on a Scheme — Nondeterministic Computing

非确定性计算，就像流处理，对于“generate and test”应用很有用。前面从两个正整数list中找到和是质数的例子，不管是第二章中对于有限序列先生成所有对的序列再过滤，还是第三章中对于无限流生成和过滤交叉进行，其关于计算是如何组织的本质图像是一样的。

非确定性方法提出了一种不同的图像。

```scheme
(define (prime-sum-pair list1 list2)
  (let ((a (an-element-of list1))
        (b (an-element-of list2)))
    (require (prime? (+ a b)))
    (list a b)))
```

看起来好像仅仅是在重述问题，而不是指定解决的方法。然后这是一个合法的非确定性程序。

这里的关键思想是非确定性语言中的表达式可以有不止一种可能的值。解释器会自动选择一个可能的值并追踪选择。如果接下来的要求没有满足，解释器会尝试一个不同的选择，它会一直尝试新选择直到评估成功或者用完了选择。就像lzay evaluation将程序员从值如何被delay和force的细节中解放出来，非确定性程序的解释器将程序员从选择如何做出的细节中解放出来。

对比一下非确定性评估和流处理所引起的不同的时间图像是很有启发的。lazy evaluation将可能结果的流被装配的时候同实际流元素生成的时间解耦。lazy evaluation给了我们这样一种错觉，所有可能的答案都在一个无时间的序列中摆在我们面前。非确定性解释器，一个表达式代表了对一组可能世界的探索，每个世界都由一组选择决定。非确定性程序解释器给了我这样的假象，时间分叉了，我们的程序有不同的可能执行历史。



### Amb and Search

`(amb <e1> <e2> .. <en>)`模糊地返回n个表达式中一个的值。例如

`(list (amb 1 2 3) (amb 'a 'b))`有6个可能的值。

只有一个选择的amb生成一个平凡值。没有选择的amb是没有可接受值的表达式，我们可以认为它在评估时会导致计算：计算终止且不产生值。

使用这个思想，可以将一个谓词表达式p必须为真的要求表示为

```scheme
(define (require p)
  (if (not p) (amb) (void)))

(define (an-element-of items)
  (require (not (null? items)))
  (amb (car items) 
       (an-element-of (cdr items))))

; infinite ranges of choices
(define (an-integer-starting-from n)
  (amb n (an-integer-starting-from (+ n 1))))
```

抽象地，我们可以想象评估amb表达式引起时间分支，计算在每个分支上以表达式的一个可能值继续。我们说amb补充一个**非确定性选择点**。我们本节将开发的amb解释器是这样实现系统性搜索的：当遇到一个amb应用时，初始选择第一个，如果选择的结果是失败，则自动回溯到最近的选择点，尝试下一个选择。如果所有选择用完了，再回到前一个选择点，从那里继续。这种搜索策略是深度优先搜索DFS而时间逻辑回溯chronological backtracking。



**Driver loop**

amb解释器的driver loop有一些不寻常的特性。它读一个表达式，打印第一次非失败执行的值。如果想让他继续生成下一次的尝试，输入try-again。如果输入其他表达式，则开始一个新的问题。

Exercise 4.35: 写一个程序an-integer-between返回一个位于两数之间的整数。它可以用于实现寻找毕达哥拉斯triples的过程。

```scheme
(define (an-integer-between low high)
  (require (<= low high))
  (amb low (an-integer-between (+ low 1) high)))

(define (a-pythagorean-triple-between low high)
  (let ((i (an-integer-between low high)))
    (let ((j (an-integer-between i high)))
      (let ((k (an-integer-between j high)))
        (require (= (+ (* i i) (* j j)) 
                    (* k k)))
        (list i j k)))))

(a-pythagorean-triple-between 1 20)
```

Exercise 4.36: 练习3.69讨论了如何生成所有毕达哥拉斯三元组的流。解释为什么简单地将an-integer-between替换为an-integer-starting-from是不够生成所有毕达哥拉斯三元组的。写一个可以实现这种功能的过程（即，如果一直输入try-again，理论上最终可以生成所有三元组）

因为会从回溯最近的选择点，所以会一直尝试k，而条件永远不可能满足了。 

```scheme
(define (an-integer-between low high)
  (require (<= low high))
  (amb low (an-integer-between (+ low 1) high)))

(define (an-integer-starting-from low)
  (amb low (an-integer-starting-from (+ low 1))))

(define (a-pythagorean-triple-starting-from low)
  (let ((j (an-integer-starting-from low)))
    (let ((i (an-integer-between low j)))
      (let ((ksq (+ (* i i) (* j j))))
        (let ((k (sqrt ksq)))
          (require (integer? k))
          (list i j k))))))

(a-pythagorean-triple-starting-from 1)
```

Exercise 4.37: Ben声称下面的方法比4.35更高效，他说的对吗？（考虑必须要探索的可能性的个数）

```scheme
(define (a-pythagorean-triple-between low high)
  (let ((i (an-integer-between low high))
        (hsq (* high high)))
    (let ((j (an-integer-between i high)))
      (let ((ksq (+ (* i i) (* j j))))
        (require (>= hsq ksq))
        (let ((k (sqrt ksq)))
          (require (integer? k))
          (list i j k))))))
```

是的，i和j的组合数还是永远，k可能不需要尝试或者只尝试一次。



### Examples of Nondeterministic Programs

非确定性编程的好处是我们可以抑制搜索如何执行的细节，因而在更高的抽象层次上表达我们的程序。

**Logic Puzzles**

Baker, Cooper, Fletcher, Miller, and Smith住在一个五层楼的不同层。Baker不住顶层，Cooper不住底层，Fletcher既不住顶层也不住底层。Millter比Cooper住的高，Smith不和Fletcher相邻。Fletcher和Cooper不相邻。大家分别住在哪一层？

直接枚举所有可能性加限制

```scheme
(define (distinct? items)
  (cond ((null? items) true)
        ((null? (cdr items)) true)
        ((member (car items) (cdr items)) false)
        (else (distinct? (cdr items)))))

(define (multiple-dwelling)
  (let ((baker (amb 1 2 3 4 5))
        (cooper (amb 1 2 3 4 5))
        (fletcher (amb 1 2 3 4 5))
        (miller (amb 1 2 3 4 5))
        (smith (amb 1 2 3 4 5)))
    (require
     (distinct? (list baker cooper fletcher 
                      miller smith)))
    (require (not (= baker 5)))
    (require (not (= cooper 1)))
    (require (not (= fletcher 5)))
    (require (not (= fletcher 1)))
    (require (> miller cooper))
    (require
     (not (= (abs (- smith fletcher)) 1)))
    (require 
     (not (= (abs (- fletcher cooper)) 1)))
    (list (list 'baker baker)
          (list 'cooper cooper)
          (list 'fletcher fletcher)
          (list 'miller miller)
          (list 'smith smith))))

(multiple-dwelling)
; ((baker 3) (cooper 2) (fletcher 4) (miller 5) (smith 1))
```

虽然这个简单的过程可以工作，但是它很慢。

Exercise 4.38: 省略Smith和Fletcher不相邻的要求。修改后有多少解答。

```scheme
((baker 1) (cooper 2) (fletcher 4) (miller 3) (smith 5))
((baker 1) (cooper 2) (fletcher 4) (miller 5) (smith 3))
((baker 1) (cooper 4) (fletcher 2) (miller 5) (smith 3))
((baker 3) (cooper 2) (fletcher 4) (miller 5) (smith 1))
((baker 3) (cooper 4) (fletcher 2) (miller 5) (smith 1))
```



Exercise 4.39: 这个过程中的限制的顺序影响结果吗？影响运行时间吗？

不影响结果，但是影响时间，把有更高的过滤比例的限制放到前面。

```scheme
(define (multiple-dwelling)
  (let ((baker (amb 1 2 3 4 5))
        (cooper (amb 1 2 3 4 5))
        (fletcher (amb 1 2 3 4 5))
        (miller (amb 1 2 3 4 5))
        (smith (amb 1 2 3 4 5)))
    (require
     (distinct? (list baker cooper fletcher 
                      miller smith))) ; 120/3125
    (require (> miller cooper))     ;1/2
    (require (not (= fletcher 5)))  ;3/5
    (require (not (= fletcher 1)))
    (require (not (= baker 5)))     ;4/5
    (require (not (= cooper 1)))    ;4/5
    (require
     (not (= (abs (- smith fletcher)) 1)))  ;112/120
    (require 
     (not (= (abs (- fletcher cooper)) 1))) ;112/120
    (list (list 'baker baker)
          (list 'cooper cooper)
          (list 'fletcher fletcher)
          (list 'miller miller)
          (list 'smith smith))))
(multiple-dwelling)
```

Exercise 4.40: 加层数不同的限制前后分别有多少种组合。生成所有可能的组合然后再回溯消除是很低效的。有些限制只依赖于一两个人的层数。写一个更高效的过程，只生成前面的限制没有过滤掉的可能。（提示：需要嵌套的let表达式）

前后分别是5^5和5!。

```scheme
(define (multiple-dwelling)
  (let ((fletcher (amb 1 2 3 4 5)))
    (require (not (= fletcher 1)))
    (require (not (= fletcher 5)))
    (let ((baker (amb 1 2 3 4 5)))
      (require (not (= baker fletcher)))
      (require (not (= baker 5)))
      (let ((cooper (amb 1 2 3 4 5)))
        ;(require (not (= cooper fletcher)))
        (require (not (= cooper baker)))
        (require (not (= cooper 1)))
        (require (> (abs (- fletcher cooper)) 1))
        (let ((miller (amb 1 2 3 4 5)))
          (require (not (= miller fletcher)))
          (require (not (= miller baker)))
          ;(require (not (= miller cooper)))
          (require (> miller cooper))
          (let ((smith (amb 1 2 3 4 5)))
            ;(require (not (= smith fletcher)))
            (require (not (= smith baker)))
            (require (not (= smith cooper)))
            (require (not (= smith miller)))
            (require (> (abs (- smith fletcher)) 1))
            (list (list 'baker baker)
                  (list 'cooper cooper)
                  (list 'fletcher fletcher)
                  (list 'miller miller)
                  (list 'smith smith))))))))
(multiple-dwelling)
```

Exercise 4.41: 写一个普通的Scheme程序来解决multiple dwelling puzzle

```scheme
(define (filter predicate sequence)
  (cond ((null? sequence) '())
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate 
                       (cdr sequence))))
        (else  (filter predicate 
                       (cdr sequence)))))

(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op 
                      initial 
                      (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (permutations s)
  (if (null? s)   ; empty set?
      (list '())  ; sequence containing empty set
      (flatmap (lambda (x)
                 (map (lambda (p) 
                        (cons x p))
                      (permutations 
                       (remove x s))))
               s)))

(define (distinct? items)
  (cond ((null? items) true)
        ((null? (cdr items)) true)
        ((member (car items) (cdr items)) false)
        (else (distinct? (cdr items)))))

(define (restrictions l)
  (apply
   (lambda (baker cooper fletcher miller smith)
     (and (> miller cooper)
          (not (= fletcher 1))
          (not (= fletcher 5))
          (not (= baker 5))
          (not (= cooper 1))
          (not (= (abs (- smith fletcher)) 1))
          (not (= (abs (- cooper fletcher)) 1))
          (distinct? (list baker cooper fletcher miller smith))))
   l))

(define (multiple-dwelling)
  (filter restrictions (permutations '(1 2 3 4 5))))
```



Exercise 4.42: 解决下面的Liars puzzle，每个人说的话都是一真一假

Betty：Kitty第二，我第三

Ethel：我第一，Joan第二

Joan：我第三，Ethel垫底

Kitty：我第二，Mary第四

Mary：我第四，Betty第一

```scheme
(define (xor a b)
  (if a (not b) b))

(define (distinct? items)
  (cond ((null? items) true)
        ((null? (cdr items)) true)
        ((member (car items) (cdr items)) false)
        (else (distinct? (cdr items)))))

(define (liars-puzzle)
  (let ((betty (amb 1 2 3 4 5))
        (ethel (amb 1 2 3 4 5))
        (joan (amb 1 2 3 4 5))
        (kitty (amb 1 2 3 4 5))
        (mary (amb 1 2 3 4 5)))
    (require (distinct? (list betty ethel joan kitty mary)))
    (require (xor (= kitty 2) (= betty 3)))
    (require (xor (= ethel 1) (= joan 2)))
    (require (xor (= joan 3) (= ethel 5)))
    (require (xor (= kitty 2) (= mary 4)))
    (require (xor (= mary 4) (= betty 1)))
    (list (list 'betty betty)
          (list 'ethel ethel)
          (list 'joan joan)
          (list 'kitty kitty)
          (list 'mary mary))))
(liars-puzzle)
; ((betty 3) (ethel 5) (joan 2) (kitty 1) (mary 4))
```



Exercise 4.43: 用amb解下面的问题，5个父亲各自以其他4人中一人的女儿命名自己的帆船

```scheme
(define (yacht)
  (define moore 1)
  (define downing 2)
  (define hall 3)
  (define hood 4)
  (define parker 5)
  (define mary 1)
  (define gabielle 2)
  (define rosalind 3)
  (define melissa 4)
  (define lorna 5)
  (define (father-of daughter fathers)
    (list-ref fathers (- daughter 1)))
  (define (daughter-of father fathers)
    (define (iter l i)
      (cond
        ((null? l) (error "non-exist"))
        ((= (car l) father) i)
        (else (iter (cdr l) (+ i 1)))))
    (iter fathers 1))
  (define (yacht-of father yachts)
    (list-ref yachts (- father 1)))
  (define (to-daughter-name num)
    (cond
      ((= num 1) 'mary)
      ((= num 2) 'gabielle)
      ((= num 3) 'rosalind)
      ((= num 4) 'melissa)
      ((= num 5) 'lorna)))
  (define (to-father-name num)
    (cond
      ((= num 1) 'moore)
      ((= num 2) 'downing)
      ((= num 3) 'hall)
      ((= num 4) 'hood)
      ((= num 5) 'parker)))
  (define (show-result fathers yachts)
    (map (lambda (fa)
           (list (to-father-name fa)
                 "daughter:"
                 (to-daughter-name (daughter-of fa fathers))
                 "yacht:"
                 (to-daughter-name (yacht-of fa yachts))))
         '(1 2 3 4 5)))
  (let ((f-mary (amb moore downing hall hood parker)))
    (require (= f-mary moore))
    (let ((f-melissa (amb moore downing hall hood parker))
          (y-downing (amb mary gabielle rosalind melissa lorna)))
          (require (= f-melissa hood))
          (require (= y-downing melissa))
          (let ((f-gabielle (amb moore downing hall hood parker))
                (y-hood (amb mary gabielle rosalind melissa lorna)))
            (require (not (= f-gabielle f-mary)))
            (require (not (= f-gabielle f-melissa)))
            (require (not (= f-gabielle hood)))  ; same as the above
            (require (= y-hood gabielle))
            (let ((f-lorna (amb moore downing hall hood parker))
                  (y-moore (amb mary gabielle rosalind melissa lorna)))
              (require (not (= f-lorna f-mary)))
              (require (not (= f-lorna f-melissa)))
              (require (not (= f-lorna f-gabielle)))
              (require (not (= f-lorna moore))) ; unnecessary
              (require (= y-moore lorna))
              (let ((f-rosalind (amb moore downing hall hood parker))
                    (y-hall (amb mary gabielle rosalind melissa lorna)))
                (require (not (= f-rosalind f-mary)))
                (require (not (= f-rosalind f-melissa)))
                (require (not (= f-rosalind f-gabielle)))
                (require (not (= f-rosalind f-lorna)))
                (require (not (= f-rosalind hall)))
                (require (= y-hall rosalind))
                (let ((y-parker (amb mary gabielle rosalind melissa lorna)))
                  (require (= y-parker mary))
                  (let ((fathers (list f-mary f-gabielle f-rosalind f-melissa f-lorna))
                        (yachts (list y-moore y-downing y-hall y-hood y-parker)))
                    (require (= (yacht-of f-gabielle yachts)
                                (daughter-of parker fathers)))
                    (show-result fathers yachts)))))))))
(yacht)
```

Lorna是Downing的女儿。

Exercise 4.44: 练习2.42描述了八皇后问题。写一个非确定性程序来解这个问题。

```scheme
(define (adjoin-position r c rest-of-queens)
  (cons (cons r c) rest-of-queens))

(define (safe? r c positions)
    (define (threaten-new? x)
      (let ((rx (car x))
            (cx (cdr x)))
        (or (= rx r)
            (= (- cx rx) (- c r))
            (= (+ cx rx) (+ c r)))))
    (not (ormap threaten-new? positions)))

(define empty-board '())

(define (queens size)
  (define (iter n l)
    (if (= n 0)
        l
        (let ((r (list-amb (range size)))
              (c n))
          (require (safe? r c l))
          (iter (- n 1) (adjoin-position r c l)))))
  (iter size empty-board))
```



**Parsing natural language**

```scheme
; classify various words
(define nouns 
  '(noun student professor cat class))

(define verbs 
  '(verb studies lectures eats sleeps))

(define articles '(article the a))

(define prepositions 
  '(prep for to in by with))

; grammars
(define (parse-sentence)
  (list 'sentence
         (parse-noun-phrase)
         (parse-verb-phrase)))

(define (parse-simple-noun-phrase)
  (list 'simple-noun-phrase
        (parse-word articles)
        (parse-word nouns)))

(define (parse-noun-phrase)
  (define (maybe-extend noun-phrase)
    (amb 
     noun-phrase
     (maybe-extend 
      (list 'noun-phrase
            noun-phrase
            (parse-prepositional-phrase)))))
  (maybe-extend (parse-simple-noun-phrase)))

(define (parse-verb-phrase)
  (define (maybe-extend verb-phrase)
    (amb 
     verb-phrase
     (maybe-extend 
      (list 'verb-phrase
            verb-phrase
            (parse-prepositional-phrase)))))
  (maybe-extend (parse-word verbs)))

(define (parse-prepositional-phrase)
  (list 'prep-phrase
        (parse-word prepositions)
        (parse-noun-phrase)))

(define (parse-word word-list)
  (require (not (null? *unparsed*)))
  (require (memq (car *unparsed*) 
                 (cdr word-list)))
  (let ((found-word (car *unparsed*)))
    (set! *unparsed* (cdr *unparsed*))
    (list (car word-list) found-word)))

(define *unparsed* '())
(define (parse input)
  (set! *unparsed* input)
  (let ((sent (parse-sentence)))
    (require (null? *unparsed*))
    sent))

(parse '(the professor lectures to 
         the student with the cat))
```

Exercise 4.45: 用上面的语法，下面的句子可以用五种不同的方式解析。the professor lectures to the student in the class with the cat.

```scheme
; class with cat, student in class
The professor (lectures (to (the student (in ((the class) (with the cat)))))).
; class with cat, lectures in class
The professor (lectures (to the student) ((in the class) (with the cat))).
; student with cat
The professor (lectures (to ((the student) (in the class) (with the cat)))).
; lectures with cat, student in class
The professor (lectures (to ((the student) in the class)) (with the cat)).
; lectures with cat, lectures in class
The professor (lectures (to the student) (in the class) (with the cat)).
```

Exercise 4.46: 4.1节和4.2节的解释器没有确定operands求值的顺序。我们将看到amb解释器从左往右解释参数。解释为什么如果参数以其他顺序评估我们的解析程序将不工作。

因为所有解析函数都是`*unparsed*`中读单词，解析顺序依赖参数的顺序。

Exercise 4.47: Louis建议既然verb phrase要么是一个verb要么是一个verb phrase后面跟一个介词phrase，可以定义成下面这样更直接

```scheme
(define (parse-verb-phrase)
  (amb (parse-word verbs)
       (list 
        'verb-phrase
        (parse-verb-phrase)
        (parse-prepositional-phrase))))

(define (parse-verb-phrase)
  (amb (list 
        'verb-phrase
        (parse-verb-phrase)
        (parse-prepositional-phrase))
       (parse-word verbs)))
```

这样可以么？如何交换amb表达式的顺序呢？

不行的，因为当读完单词之后`(parse-word verbs)`总是失败，所以总是执行amb的第二个分支，导致无限循环。交换表达式的顺序之后，还是无限循环，甚至不会去读单词。

Exercise 4.48: 扩展上面的语法来处理更复杂的句子。例如你可以扩展名词短语和动词短语以支持形容词和副词，或者可以处理复合句子。

```scheme
(define adjectives
  '(adj big small long short good bad))

(define (parse-simple-noun-phrase)
  (let ((art (parse-word articles))
        (tag 'simple-noun-phrase))
    (amb (list tag art (parse-word adjectives) (parse-word nouns))
         (list tag art (parse-word nouns)))))

(define adverbs
  '(adv quickly slowly happily))

(define (parse-simple-verb-phrase)
  (let ((tag 'simple-verb-phrase))
    (amb (list tag (parse-word adverbs) (parse-word verbs))
         (list tag (parse-word verbs)))))

(define (parse-verb-phrase)
  (define (maybe-extend verb-phrase)
    (amb 
     verb-phrase
     (maybe-extend 
      (list 'verb-phrase
            verb-phrase
            (parse-prepositional-phrase)))))
  (maybe-extend (parse-simple-verb-phrase)))

(define conjunctions
  '(conj while and but when))

(define (parse-simple-sentence)
  (list 'simple-sentence
         (parse-noun-phrase)
         (parse-verb-phrase)))

(define (parse-sentence)
  (let ((sent (parse-simple-sentence)))
    (amb sent (list 'compound-sentence sent (parse-word conjunctions)
                    (parse-sentence)))))
(parse '(the big cat slowly eats in the class))
```

Exercise 4.49: Alyssa对生成有趣的句子更感兴趣，而不是解析它们。她觉得只需要修改parse-word，让它忽略输入句子，而是生成一个合适的词，就可以变成生成器了。实现她的想法，然后显示6个左右的句子。

```scheme
(define (list-amb l)
  (if (null? l)
      (amb)
      (amb (car l) (list-amb (cdr l)))))

(define (parse-word word-list) 
   (require (not (null? *unparsed*))) 
   (require (memq (car *unparsed*) (cdr word-list))) 
   (let ((found-word (car *unparsed*))) 
     (set! *unparsed* (cdr *unparsed*)) 
     (list-amb (cdr word-list))))
```



### Implementing the Amb Evaluator

当表达式的评估碰到死胡同时，必须回溯到前一个选择点。我们修改4.1.7的分析解释器来构建非确定性Scheme的amb解释器。

**Execution procedures and continuations**

amb解释器的执行过程有3个参数：环境，以及两个continuation过程。如果表达式的评估结果得到一个值，则用这个值调用success continuation；否则调用failure continuation。构造和调用合适的continuation就是非确定性解释器实现回溯的机制。

success continuation的工作是接收一个值然后继续计算。同时会传另一个失败continuation给它。

failure continuation的工作是尝试另一个分支，它会选择最近的选择点选择下一个，如果没有其他选择了，就出发更早的一个选择点上的失败continuation。

try-again输入时，driver loop也会调用失败continuation。

另外，如果处理的一个分支导致了副作用（例如赋值），当这个分支走到死胡同的时候，需要undo这个副作用，然后再做下个选择。这个通过副作用操作产生一个失败continuation来撤销副作用并传递失败。



以下几种方式会构建失败continuations。

- amb表达式，提供一个机制来做选择，当前选择到死胡同时进行下一个
- 顶层driver，提供机制报告失败，当选择耗尽时
- 赋值，在回溯的时候拦截失败，撤销赋值操作

失败的初始化

- 用户程序执行`(amb)`
- 用户在driver-loop中输入try-again

失败延续在处理一个失败时也会被调用

- 赋值创建的失败延续在撤销完副作用之后，调用它拦截的失败延续，往上传递失败。
- 当amb的失败延续用完选择的时候，它调用最初给amb的失败延续，往上传递失败。



**Structure of the evaluator**

```scheme
(define (amb? exp) (tagged-list? exp 'amb))
(define (amb-choices exp) (cdr exp))

((amb? exp) (analyze-amb exp))

(define (ambeval exp env succeed fail)
  ((analyze exp) env succeed fail))
; 成功延续有两个参数：刚获取的值，另一个失败延续，如果这个值导致后续的失败时调用
; 失败延续没有参数.
; 所以执行过程的一般形式是：
(lambda (env succeed fail)
  ;; succeed is (lambda (value fail) …)
  ;; fail is (lambda () …)
  …)
; amb解释器的复杂度主要来自执行过程相互调用的时候传递延续的机制。
```



**Simple expressions**

简单类型的执行过程跟普通解释器在本质上是相同的，除了需要管理延续。

```scheme
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

(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (scan-out-defines 
                (lambda-body exp))))
    (lambda (env succeed fail)
      (succeed (make-procedure vars bproc env)
               fail))))

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
```

注意查找变量总是“成功”的。如果没有找到变量，触发错误。这种失败指示程序bug，而不是指示我们应该尝试下一个选择。



**Conditionals and sequences**

```scheme
; if 语句里也可能有amb，会失败。
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
```



**Definitions and assignments**

```scheme
; 我们不用担心撤销定义，因为我们假设内部定义已经被扫描出去了。
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

; 赋值是我们第一次真正使用延续的地方
(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze 
                (assignment-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)    ; *1*
               (let ((old-value
                      (lookup-variable-value 
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

; 只unbound当前帧的，否则在撤销的时候需要知道是在哪个帧
(define (analyze-unbound exp)
  (let ((var (unbound-variable exp)))
    (lambda (env succeed fail)
      (let ((old-value (lookup-variable-value var env)))
        (unbound-variable! var env)
        (succeed 'ok
                 (lambda ()
                   (define-variable! var old-value env)
                   (fail)))))))
```



**Procedure applications**

```scheme
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
```



**Evaluating amb expressions**

amb特殊形式是非确定性语言中的关键成员。这里我们将看到解释器进程的精华和追踪延续的原因。

```scheme
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
```



**Driver loop**

因为允许用户try again的机制使driver loop变得复杂。

```scheme
; read-eval-print loop
(define input-prompt  ";;; Amb-Eval input:")
(define output-prompt ";;; Amb-Eval value:")”

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
```

Exercise 4.50: 实现一个新的特殊形式ramb，类似amb除了以随机的顺序搜索可选项。展示这个可以帮助Alyssa在练习4.49中的问题。

不会永远在递归里面出不来。

```scheme
(define (ramb? exp) (tagged-list? exp 'ramb))
(define (ramb-choices exp) (cdr exp))

((ramb? exp) (analyze-ramb exp))

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
```

Exercise 4.51: 实现一种新的赋值permanent-set!，在时候的失败不会撤销。例如可以用于统计试了多少次才成功。

```scheme
(define count 0)
(let ((x (an-element-of '(a b c)))
      (y (an-element-of '(a b c))))
  (permanent-set! count (+ count 1))
  (require (not (eq? x y)))
  (list x y count))

((permanent-assignment? exp)
         (analyze-permanent-assignment exp))

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

(define (permanent-assignment? exp)
  (tagged-list? exp 'permenant-set!))
```

如果我们用了set!的话，结果将永远是1。

Exercise 4.52: 实现一个新的结构叫if-fail，它允许用户捕捉表达式的失败。它接收两个表达式，它评估第一个表达式，如果成功则正常返回值。如果评估失败，则返回第二个表达式。下面是一个例子：

```scheme
;;; Amb-Eval input:
(if-fail 
 (let ((x (list-amb '(1 3 5))))
   (require (even? x))
   x)
 'all-odd)

;;; Starting a new problem
;;; Amb-Eval value:
all-odd

;;; Amb-Eval input:
(if-fail
 (let ((x (list-amb '(1 3 5 8))))
   (require (even? x))
   x)
 'all-odd)

;;; Starting a new problem
;;; Amb-Eval value:
8
```

实现如下

```scheme
((if-fail? exp) 
         (analyze-if-fail exp))

(define (analyze-if-fail exp)
  (let ((proc1 (analyze (if-fail-exp1 exp)))
        (proc2 (analyze (if-fail-exp2 exp))))
    (lambda (env succeed fail)
      (proc1 env
             succeed
             (lambda ()
               (proc2 env succeed fail))))))

(define (if-fail? exp) (tagged-list? exp 'if-fail))
(define (if-fail-exp1 exp) (cadr exp))
(define (if-fail-exp2 exp) (caddr exp))
```

Exercise 4.53: 有了permanent-set!和if-fail，下面的结果是什么？所有和为质数的对。

```scheme
(let ((pairs '()))
  (if-fail 
   (let ((p (prime-sum-pair 
             '(1 3 5 8) 
             '(20 35 110))))
     (permanent-set! pairs 
                     (cons p pairs))
     (amb))
   pairs))
```

Exercise 4.54: 如果我们将require实现为特殊形式

```scheme
(define (require? exp) 
  (tagged-list? exp 'require))

(define (require-predicate exp) 
  (cadr exp))

((require? exp) (analyze-require exp))

(define (analyze-require exp)
  (let ((pproc (analyze 
                (require-predicate exp))))
    (lambda (env succeed fail)
      (pproc env
             (lambda (pred-value fail2)
               (if (false? pred-value)
                   (fail2)
                   (succeed 'ok fail2)))
             fail))))
```



## 4.4 Logic Programming

### Deductive Information Retrieval

**Simple Queries**

Exercise 4.55: 给出检索下列信息的简单查询语句

1. 所有由Ben Bitdiddle监管的人
2. 所有在accounting部门的人的名字和工作
3. 所有生活在Slumerville的人的名字和地址

```scheme
(supervisor ?x (Bitdiddle Ben))
(job ?x (accounting . ?y))
(address ?x (Slumerville . ?y))
```

**Compound Queries**

这里的and/or/not跟一般的不太一样，and其实并不表示交集，而是只需要两个子句不冲突就可以了，一般绑不同变量。or表示只需要满足其中之一，一般绑同一个变量。not是在前面的基础上过滤，如果是单独一个语句结果肯定为空。

Exercise 4.56: 构思检索下列信息的查询语句

1. 所有由Ben Bitdiddle监管的人的名字，以及他们的地址
2. 所有工资比Ben Ditdiddle低的人，以及他们和Ben的工资
3. 所有不是由计算机部门的人监管的人，以及他们的监管人的名字和工作

```scheme
(and (supervisor ?x (Bitdiddle Ben))
     (address ?x ?y))
(and (salary (Bitdiddle Ben) ?x)
     (salary ?name ?y)
     (lisp-value < ?y ?x))
(and (supervisor ?name ?super)
     (not (job ?super (computer . ?type)))
     (job ?super ?job))
```

**Rules**

`(rule <conclusion> <body>) `

Exercise 4.57: 定义一个rule判断person1是否可以替代person2的工作。当两个人做相同的工作，或者做person1工作的人也可以做person2的工作，且person1和person2是两个不同的人。使用这条规则，给出下面查找的查询语句

1. 所有可以替代Cy D. Fect工作的人
2. 所有可以替代某个比他工资高的人的人，以及两个人的工资

```scheme
(assert! (rule (can-replace ?p1 ?p2)
      (and (job ?p1 ?job1)
           (or (job ?p2 ?job1)
               (and (job ?p2 ?job2)
                    (can-do-job ?job1 ?job2)))
           (not (same ?p1 ?p2)))))
;output
(can-replace (Bitdiddle Ben) (Fect Cy D))
(can-replace (Hacker Alyssa P) (Fect Cy D))

(assert! (rule (can-replace ?person1 ?person2) 
                (and (job ?person1 ?job1) 
                     (job ?person2 ?job2) 
                     (or (same ?job1 ?job2) 
                         (can-do-job ?job1 ?job2)) 
                     (not (same ?person1 ?person2))))) 
;output
(can-replace (Bitdiddle Ben) (Fect Cy D))
(can-replace (Hacker Alyssa P) (Fect Cy D))

(can-replace ?x (Fect Cy D))
(and (can-replace ?p1 ?p2)
     (salary ?p1 ?x)
     (salary ?p2 ?y)
     (lisp-value < ?x ?y))
```

Exercise 4.58: 定义一个rule，当一个人在某个部门工作但是它的监管人不在这个部分，那么我们就说他是这个部分的一个big shot

```scheme
(rule (bigshot ?p ?div)
      (and (job ?p (?div . ?post))
           (not (and (supervisor ?p ?super)
                     (job ?super (?div . ?any))))))
```

Exercise 4.59: Ben将每周会议加到了数据库里

```scheme
; division-wide meetings
(meeting accounting (Monday 9am))
(meeting administration (Monday 10am))
(meeting computer (Wednesday 3pm))
(meeting administration (Friday 1pm))

; company-wide meeting
(meeting whole-company (Wednesday 4pm))

; query for all the meetings on Friday
(meeting ?x (Friday ?y))

; rule says all the meetings for a person
(rule (meeting-time ?person ?day-and-time)
      (or (meeting whole-company ?day-and-time)
          (and (meeting ?div ?day-and-time)
               (job ?person (?div . ?any)))))
; query on meetings Alyssa should attend on Wednesday
(meeting-time (Hacker Alyssa P) (Wednesday ?time))
```

Exercise 4.60: 用live-near?查相邻的人时，如果两个都是变量，每一对都会出现两次。为什么？有什么办法让每对只出现一次么？

```scheme
(rule (lives-near ?person-1 ?person-2)
      (and (address ?person-1 
                    (?town . ?rest-1))
           (address ?person-2 
                    (?town . ?rest-2))
           (not (same ?person-1 ?person-2))
           (alphabet< ?person-1 ?person-2)))

(rule (alphabet< ?x ?y)
      (lisp-value
       (lambda (x y)
         (define (list->string l)
           (foldr string-append ""
                  (map symbol->string l)))
         (string<? (list->string x) (list->string y)))
       ?x ?y))
```

因为两次变量是独立的。可以加一个限制，对名字进行排序。

**Logic as programs**

定义两条规则来表示一个关系

```scheme
(rule (append-to-form () ?y ?y))
(rule (append-to-form (?u . ?v) ?y (?u . ?z))
      (append-to-form ?v ?y ?z))
```

Exercise 4.61: 下面的规则实现next-to关系，来寻找list中的相邻元素

```scheme
(rule (?x next-to ?y in (?x ?y . ?u)))
(rule (?x next-to ?y in (?v . ?z))
      (?x next-to ?y in ?z))
```

下面的查询语句的响应是什么？

```scheme
(?x next-to ?y in (1 (2 3) 4))
; 1 (2 3), (2 3) 4
(?x next-to 1 in (2 1 3 1))
; 2 1, 3 1
```

Exercise 4.62: 定义规则来实现练习2.17中的last-pair操作，返回包含非空list中最后一个元素的list。确认你的规则在以下查询中是否正常工作

```scheme
(rule (last-pair (?u . ?v) (?x))
      (last-pair ?v (?x)))
(rule (last-pair (?x) (?x)))

(last-pair (3) ?x)
(last-pair (1 2 3) ?x)
(last-pair (2 ?x) (3))
(last-pair ?x (3)) ;这种呢，换一下顺序就工作了？？
;;; Query results:
(last-pair (3) (3))
(last-pair (?u-2 3) (3))
(last-pair (?u-2 ?u-6 3) (3))
(last-pair (?u-2 ?u-6 ?u-10 3) (3))
(last-pair (?u-2 ?u-6 ?u-10 ?u-14 3) (3))
(last-pair (?u-2 ?u-6 ?u-10 ?u-14 ?u-18 3) (3))
(last-pair (?u-2 ?u-6 ?u-10 ?u-14 ?u-18 ?u-22 3) (3))
(last-pair (?u-2 ?u-6 ?u-10 ?u-14 ?u-18 ?u-22 ?u-26 3) (3))
(last-pair (?u-2 ?u-6 ?u-10 ?u-14 ?u-18 ?u-22 ?u-26 ?u-30 3) (3))
(last-pair (?u-2 ?u-6 ?u-10 ?u-14 ?u-18 ?u-22 ?u-26 ?u-30 ?u-34 3) (3))

   pattern  conclusion  body   bindings 
1: (?x (3)) ((?x-1) (?x-1))  ==> ?x->(?x-1), ?x-1->3
(last-pair (3) (3))
2: (?x (3)) ((?u-2 . ?v-2) (?x-2)) (?v-2 (?x-2)) ==> ?x->(?u-2 . ?v-2), ?x-2->3
 3: (?v-2 (?x-2)) ((?x-3) (?x-3)) ==> ?v-2->(?x-3)->(3) ,?x-2->?x-3->3
(last-pair (?u-2 3) (3))
 4: other rules
 5: other rules
 6: (?v-2 (?x-2)) ((?u-6 . ?v-6) (?x-6)) (?v-6 (?x-6)) ==> ?v-2->(?u-6 . ?v-6), ?x-2->?x-6->3
  7: (?v-6 (?x-6)) ((?x-7) (?x-7)) ==> ?v-6->(?x-7)->(3), ?x-6->?x-7->3
(last-pair (?u-2 ?u-6 3) (3))
  8: (?v-6 (?x-6)) ((?u-8 . ?v-8) (?x-8)) (?v-8 (?x-8)) ==> ?v-6->(?u-8 . ?v-8), ?x-6->?x-8->3
  
如果交换顺序，一直在递归调用
   pattern  conclusion  body   bindings 
1: (?x (3)) ((?u-1 . ?v-1) (?x-1)) (?v-1 (?x-1)) ==> ?x->(?u-1 . ?v-1), ?x-1->3
 2: (?v-1 (?x-1)) ((?u-2 . ?v-2) (?x-2)) (?v-2 (?x-2)) ==> ?v-1->(?u-2 . ?v-2), ?x-1->?x-2->3
  3: (?v-2 (?x-2)) ((?u-3 . ?v-3) (?x-3)) (?v-3 (?x-3)) ==> ?v-2->(?u-3 . ?v-3), ?x-2->?x-3->3
```

因为同一个表中是cons的，后注册的rule在list的前面，会先被尝试到。

Exercise 4.63: 下面的数据库追踪Ada后代的族谱

```scheme
(son Adam Cain) (son Cain Enoch)
(son Enoch Irad) (son Irad Mehujael)
(son Mehujael Methushael)
(son Methushael Lamech)
(wife Lamech Ada) (son Ada Jabal)
(son Ada Jubal)
```

构思规则如“如果S是f的儿子，f是G的儿子，那么S是G的孙子”，“如果W是M的妻子，S是W的儿子，那么S也M的儿子”。这些规则使查询系统可以查找Cain的孙子，Lamech的儿子，Methushael的孙子。

```scheme
(rule (partner ?a ?b)
      (or (wife ?a ?b)
          (wife ?b ?a)))

(rule (partner-uniq ?a ?b)
      (and (partner ?a ?b)
           (alphabet< ?a ?b)))

(rule (hasson ?p ?s)
      (or (son ?p ?s)
          (and (partner ?p ?p2)
               (son ?p2 ?s))))

(rule (grandson ?g ?s)
      (and (hasson ?g ?p)
           (hasson ?p ?s)))

(grandson Cain ?x)
(hasson Lamech ?x)
(grandson Methushael ?x)
```

### How the Query System Works

本节只看系统的一般结构。query解释器必须执行一些搜索。一种方式是将查询系统实现为非确定性程序，使用4.3节中的amb解释器。另一种方式是通过流来管理搜索。我们的实现使用第二种方式。

查询系统围绕两个核心操作**pattern matching**和**unification**来组织。

**Pattern matching**

模式匹配器接收一个pattern一个datum和一个frame作为输入。其中frame指定各种模式变量的绑定。它检查数据是否匹配模式，并且跟已经在frame中的绑定一致。如果是，就将当前匹配确定的任何绑定加到frame中然后返回增大的frame。否则，它指示匹配失败。

模式匹配器是处理不涉及规则的简单查询所需要的所有机制。

**Streams of frames**

对frame的模式测试通过使用流来组织。给定一个帧，匹配处理一条一条遍历数据库条目。对于每个数据库条目，匹配器要么产生一个符号指示其错误，要么产生一个帧的扩展。结果将收集到一个流中，这个流然后通过过滤器。

在我们的系统中，一个query接收一个帧流作为输入，对流中的每个帧执行上述匹配操作。

**Compound queries**

这个帧流实现的真正高雅之处在我们处理复合查询的时候就变得明显了。

and操作可以看成是两个组件query的级联组合。or操作可以看成是并联组合。即使从高层描述，我们也可以看出复合查询可以很慢。从帧流的视角看，not操作就像一个过滤器，移除掉所有查询可以满足的帧。lisp-value也实现为类似的在帧流上的过滤器。

**Unification**

rule conclusions就像assertions，除了它们可以包含变量。所以我们需要一个模式匹配的一般化—叫unification，它里面pattern和datum都可以包含变量。

一个unifier接收两个patterns，每个都包含常量和变量，然后确定是否可以通过赋值给变量来使两个模式相等。例如unifying`(?x a ?y)`和`(?y ?z a)`，将会指定一个帧其三个变量都绑到a上。

unification算法是查询系统中技术最难的部分。对于复杂的模式，执行统一需要推导。而且一个成功的统一可能不需要完全确定变量的值，有些变量可以仍然是未绑定的。（另一种考虑unification的方式是，它产生两个输入模式专门化的最一般的模式）

**Applying rules**

统一是根据规则做推断的查询系统的组件的关键。例如处理这个涉及应用规则的查询

`(live-near ?x (Hacker Alyssa P))`

首先使用普通的模式匹配过程看数据库中是否有assertions匹配这个模式。接下来尝试统一查询模式和每条规则的conclusion，我们发现这个模式与这个条规则的结论统一

```scheme
(rule (lives-near ?person-1 ?person-2)
      (and (address ?person-1 
                    (?town . ?rest-1))
           (address ?person-2 
                    (?town . ?rest-2))
           (not (same ?person-1 ?person-2))))
```

这导致一个帧，指定?person-2绑定为`(Hacker Alyssa P)`，?x绑定?person-1。现在关于这个帧，我们评估规则body中的复合查询，成功的匹配将会通过绑定?person-1来扩展这个帧，也即?x的值，我们也就可以用它实例话最初的查询模式。

一般而言，查询解释器使用下面的方法来应用一个规则

- 统一查询与规则的conclusion，如果成功，形成一个原始帧的扩展
- 关于这个扩展到帧，评估规则的body中的查询

可以看到这跟我们前面解释器应用过程的方法是相似的

- 绑定过程形参到它的实惨，形成一个帧扩展原始的过程环境
- 关于这个扩展的环境，评估过程body中的表达式



**Simple queries**

**The query evaluator and the driver loop**

不管底下匹配操作的复杂度，系统被组织为像任何语言的解释器。

### Is Logic Programming Mathematical Logic?

**逻辑编程的目的是提供给编程者将一个计算问题分解成两个单独的问题的技术：要计算什么？如何计算？**

这通过选择一个数学逻辑语句的子集来实现，这个自己子集足够强大来描述任何可能想要计算的东西，但同时又足够弱来有一个可控制的过程解释器。

一方面，程序应该是一个有效的可以被计算机执行的程序。控制（How）体现在使用语言的评估顺序。通过安排子句和子目标的顺序来使计算有效和高效。同时，我们要能看到计算结果（What）。

我们的查询语言可以看成是这样一个过程可解释的数学逻辑的子集。一个assertion表示一个简单事实（原子的命题）。一个规则表示一个暗示，规则conclusion持有规则body所持有的所有情况。一个规则有一个自然的过程解释：要建立规则的结论，建立规则的body。所以规则指定了计算。然而因为规则也可以被看成是数学逻辑的陈述，我们可以通过论断相同的结果可以完全通过数学逻辑中的工作得到来证明任何逻辑程序完成的推断是合法的。

**Infinite loops**

逻辑程序的过程解释的一个后果就是它可能构建很低效的程序，极端的例子就是死循环。

相互关联的规则集合可以导致很难预期的循环，循环的出现可以取决于and中子句的顺序或者系统处理查询顺序的底层细节。

**Problems with not**

not子句的顺序不同，会导致结果不同。因为我们的not实现其实是作为一个变量值的过滤器。如果not子句在处理时有些变量是未绑定的，系统会产生非预期的结果。同样地，如果lisp-value的一些参数未绑定它将不能工作。

另外查询语言中的not跟数学逻辑的not也不一样。这里的not P并不是表示P不真，而是P无法从数据库的知识中推导出来。也就是说，逻辑编程语言中的not反应了所谓的封闭世界假设**closed world assumption**。

Exercise 4.64: Louis错误修改了outranked-by规则，然后有人来查谁在Ben Bitdiddle上面结果死循环了。

```scheme
(rule (outranked-by ?staff-person ?boss)
  (or (supervisor ?staff-person ?boss)
      (and (outranked-by ?middle-manager
                         ?boss)
           (supervisor ?staff-person 
                       ?middle-manager))))

(outranked-by (Bitdiddle Ben) ?who)
   pattern  conclusion  body   bindings 
1: ((Bitdiddle Ben) ?who) (?s-1 ?b-1) (?m-1 ?b-1) ==> ?s-1->(Bitdiddle Ben), ?b-1->?who
 2: (?m-1 ?b-1) (?s-2 ?b-2) (?m-2 ?b-2) ==> ?m-1->?s-2, ?b-1->?b-2
  2: (?m-2 ?b-2) (?s-3 ?b-3) (?m-3 ?b-3) ==> ?m-2->?s-3, ?b-2->?b-3

(outranked-by ?who (Warbucks Oliver))
1: (?who (Warbucks Oliver)) (?s-1 ?b-1) (?m-1 ?b-1) ==> ?s-1->?who, ?b-1->(Warbucks Oliver)
 2: (?m-1 ?b-1) (?s-2 ?b-2) (?m-2 ?b-2) ==> ?m-1->?s-2, ?b-1->?b-2
  3: (?m-2 ?b-2) (?s-3 ?b-3) (?m-3 ?b-3) ==> ?m-2->?s-3, ?b-2->?b-3
```

执行到body中的outraked-by时，?middle-manager和?boss都是未绑定状态，然后就一直递归。

所以，一般把不是递归的放到前面，这样可以提供更多绑定。

Exercise 4.65: Cy查找所有的wheels，结果Oliver出现了四次？

```scheme
(rule (wheel ?person)
      (and (supervisor ?middle-manager 
                       ?person)
           (supervisor ?x ?middle-manager)))

(wheel ?who)
;;; Query results:
(wheel (Warbucks Oliver))
(wheel (Bitdiddle Ben))
(wheel (Warbucks Oliver))
(wheel (Warbucks Oliver))
(wheel (Warbucks Oliver))
```

因为有四个人的监管人是他，结果显示的其实是这些条目。

Exercise 4.66: Ben在一般化查询系统以提供关于公司的统计。

```scheme
(sum ?amount
     (and (job ?x (computer programmer))
          (salary ?x ?amount)))

(accumulation-function ⟨variable⟩
                       ⟨query pattern⟩)
```

他觉得实现这个应该很简单。简单地将query pattern喂给qeval，将得到一个帧流，然后将流传给一个映射函数，将每个帧中变量的值喂给积累函数。可以当Cy给他看了4.65的问题之后他意识到这不可行。他意识到了什么？略述一个方法可以抢救这个状况。

同一条数据可能被统计很多次。对结果帧进行去重。

Exercise 4.67: 设计一个方法安装一个循环探测器，从而可以防止简单的循环（如文中和练习4.64中描述的）。总体思路是系统应该维护它当前推导链的一些历史，并且不应该处理一个已经在工作的查询。描述哪些信息（模式和帧）需要包含在这个历史中，以及如何进行检测。在学习了4.4.4之后，你可能想要修改系统以包含你的循环探测器。

同一条rule，如果pattern一样（绑定到相同值或未绑定）。多传一个hist参数。



```scheme
(define (apply-a-rule rule
                      query-pattern
                      query-frame
                      hist)
  (let* ((state (pattern-state query-pattern query-frame))
         (nhist (add-history rule state hist)))
    (if nhist
        (let ((clean-rule 
               (rename-variables-in rule))) ; avoid confusion
          (let ((unify-result
                 (unify-match query-pattern
                              (conclusion clean-rule)
                              query-frame)))
            (if (eq? unify-result 'failed)
                the-empty-stream
                (qeval (rule-body clean-rule)
                       (singleton-stream 
                        unify-result)
                       nhist))))
        the-empty-stream)))

; work history to avoid infinite loop
(define (pattern-state pat frame)
  (cond
    ((var? pat) (let ((binding (binding-in-frame pat frame)))
                  (if binding
;                      (pattern-state (binding-value binding) frame)
                      ;(binding-value binding)
                      binding)))
    ((pair? pat) (cons (pattern-state (car pat) frame)
                       (pattern-state (cdr pat) frame)))
    (else pat)))

(define (add-history rule state hist)
  (let ((key (conclusion rule)))
    (let ((exist (stream-ormap (lambda (x)
                                 (equal? x state))
                               hist)))
      (if exist
          #f
          (cons-stream state hist)))))
```

如何确定两条查询状态一致，首先需要有未绑定的变量，或者绑定到其他变量的变量。

历史存放，pattern中未绑定的变量（包括绑定到变量的）和pattern，rule？。比较该次unify之后，之前的历史是否有进展，未绑定的变成绑定了，对于pair/list绑定的部分增多了也算，或者更具体了也算，例如(?u-2 . ?v-2)变成(?u-2 ?u-6 . ?v-6)。一般一点，就是变量绑定到了其他变量也算。只要端层的任意free变量绑定了即可。?x-2绑定?x-4这种不算。?x绑到?y该算么

是所有历史都比较，还是只比较rule相同的。只要是有没有进展的循环，不管是直接循环，还是间接循环，总是会再次调用到相同的rule，且再次调用到时之前的历史没有进展。

```scheme
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
      (if (or (eq? unify-result 'failed)
              (in-progress? conclusion unify-result hist))
          the-empty-stream
          (qeval (rule-body clean-rule)
                 (singleton-stream 
                  unify-result)
                 (extend-history conclusion query-pattern unify-result hist))))))

(define (in-progress? conclusion frame hist)
  (let ((h (first-history conclusion hist))) ; only checking the first is enough?
    (and h (all-unbound? (history-freevars h) frame))));

(define (free-vars pat frame)
  (let ((result '()))
    (define (iter pat)
      (cond
       ; var? must be before pair?, because var is actually a pair
       ((var? pat)
        (let ((bind (binding-in-frame pat frame)))
          (if bind
              (iter (binding-value bind))
              (set! result (cons pat result)))))
       ((pair? pat)
        (begin (iter (car pat)) (iter (cdr pat))))
       (else (void))))
    (iter pat)
    result))

(define (all-unbound? vars frame)
  (andmap (lambda (var)
            (binding-in-frame var frame))
          vars))

(define (history-conclusion hist)
  (car hist))
(define (history-freevars hist)
  (cdr hist))
(define (make-history conclusion pattern frame)
  (let ((vars (free-vars pattern frame)))
    (if (null? vars)
        #f
        (cons conclusion vars))))

(define (extend-history conclusion pattern frame history)
  (let ((hist (make-history conclusion pattern frame)))
    (if hist
        (cons hist history)
        history)))

(define (first-history conclusion history)
  (ormap (lambda (hist)
           (if (equal? conclusion
                       (history-conclusion hist))
               hist
               #f)) history))
(free-vars '(outranked-by (Bitdiddle Ben) (? who))
'(((? who) ? 1 boss) ((? 1 staff-person) Bitdiddle Ben)))
```

The final solution combines the previous two.

```scheme
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


;; ========TESTS========
(assert! (rule (self-recursion ?x)
               (self-recursion ?x)))
(self-recursion ?x)

(assert! (rule (mutual-recursion-a ?x)
               (mutual-recursion-b ?x)))
(assert! (rule (mutual-recursion-b ?x)
               (mutual-recursion-a ?x)))
(mutual-recursion-a ?x)
(mutual-recursion-b ?x)

(assert! (rule (self-recursion-indirect ?x ?y)
               (self-recursion-indirect ?y ?x)))
(self-recursion-indirect ?x ?y)
(self-recursion-indirect 3 ?y)

(assert! (married Minnie Mickey))
(assert! (rule (married ?x ?y)
               (married ?y ?x)))
;;; Query input:
(married Mickey ?who)
;;; Query results:
(married Mickey Minnie)

;;; Query input:
(outranked-by (Bitdiddle Ben) ?who)
;;; Query results:
(outranked-by (Bitdiddle Ben) (Warbucks Oliver))

;;; Query input:
(outranked-by ?who (Warbucks Oliver))
;;; Query results:
(outranked-by (Aull DeWitt) (Warbucks Oliver))
(outranked-by (Scrooge Eben) (Warbucks Oliver))
(outranked-by (Bitdiddle Ben) (Warbucks Oliver))

;;; Query input:
(last-pair ?x (3))

;;; Query results:
(last-pair (3) (3))
(last-pair (?u-39 3) (3))
(last-pair (?u-39 ?u-43 3) (3))
(last-pair (?u-39 ?u-43 ?u-47 3) (3))

(reverse ?x (1 2 3))
(reverse (1 2 3) ?x) ; infinite loop
```



Exercise 4.68: 定义规则以实现练习2.18中的reverse操作，它返回一个相同元素的反序list。（提示：使用append-to-form）。你的规则可以回答`(reverse (1 2 3) ?x)`和`(reverse ?x (1 2 3))`么？

```scheme
(rule (reverse ?x ?y)
      (and (append-to-form (?first) ?rest ?x)
           (append-to-form ?rev-rest (?first) ?y) ;循环
           (reverse ?rest ?rev-rest)))
(rule (reverse () ()))
; 本质上是因为list是单向的，只能从前面获取，不能从后面获取
```



Exercise 4.69:  由练习4.63中的数据库和规则出发，设计一个规则用于给一个孙子关系增加greats，元孙子。（提示：表现为`((greate grandson) Adam Irad)`。写规则确定是否一个list由单词grandson结尾。使用这个规则表达一个规则，允许推导关系`((great . ?rel) ?x ?y`，其中?rel是以grandson结尾的list）。检查你的规则，`((great grandson) ?g ?ggs)`和`(?relationship Adam Irad)`

```scheme
(rule (end-in-grandson (?x . ?rest))
      (end-in-grandson ?rest))
(rule (end-in-grandson (grandson)))
(rule ((grandson) ?x ?y) (grandson ?x ?y)) 

; Note that the end-with-gs predicate has to be the last one 
(rule ((great . ?rel) ?x ?y)
      (and (hasson ?x ?z)
           (?rel ?z ?y)
           (end-in-grandson ?rel)))

((great grandson) ?g ?ggs)
(?relationship Adam Irad)
(?relationship Adam Jabal) 
```



### Implementing the Query System

Exercise 4.70:过程add-assertion!和add-rules!中的let绑定的目的是什么？下面的add-assertion!实现由什么问题。

因为延迟评估，THE-ASSERTIONS会先被设置，然后在实际用到cdr的时候才会评估，此时的cdr已经是新的THE-ASSERTIONS了，导致无限循环。

```scheme
(define (add-assertion! assertion)
  (store-assertion-in-index assertion)
  (set! THE-ASSERTIONS
        (cons-stream assertion 
                     THE-ASSERTIONS))
  'ok)
```



Exercise 4.71: Louis纳闷为什么simple-query和disjoin实现为显式使用delay，而不是像下面这样定义

```scheme
(define (simple-query 
         query-pattern frame-stream)
  (stream-flatmap
   (lambda (frame)
     (stream-append
      (find-assertions query-pattern frame)
      (apply-rules query-pattern frame)))
   frame-stream))

(define (disjoin disjuncts frame-stream)
  (if (empty-disjunction? disjuncts)
      the-empty-stream
      (interleave
       (qeval (first-disjunct disjuncts)
              frame-stream)
       (disjoin (rest-disjuncts disjuncts)
                frame-stream))))
```

你能给出查询的例子，这些更简单的定义将会导致非期望的行为。

可以节省时间提高效率，有些后面的计算可能是不必要的，例如过滤的时候。另外也可以推迟无限循环，甚至避免无限循环。例如下面的例子，查询`(son ?x ?y)`会无限循环，而`(not (son ?x ?y))`不会。因为过滤掉了。

```scheme
(rule (son ?x ?y) 
       (son ?x ?y)) 
```

Exercise 4.72: 为什么disjoin和stream-flatmap要交叉，给出例子说明交叉更好。（提示：3.5.3中为什么使用交叉）

如果有一个是无限的，那么另外一个永远得不到显示，或者一直得不到显示。

Exercise 4.73: 为什么flatten-stream显式使用delay

跟4.71一样的道理，防止无限循环。

Exercise 4.74: Alyssa提出哉negate、lisp-value和find-assertions中使用更简单版本的stream-flatmap。她观察到这些例子中的过程处理每个帧的时候要么返回空流要么返回singleton流，所以没有必要交叉。 1完成空缺的部分，2如果我们这么改查询系统的行为会改变么？

```scheme
(define (simple-stream-flatmap proc s)
  (simple-flatten (stream-map proc s)))

(define (simple-flatten stream)
  (stream-map stream-car
              (stream-filter (lambda (s)
                               (not (stream-null? s)))
                             stream)))
```

结果不会变，因为本来就是顺序的。

Exercise 4.75: 实现一个新的特殊形式unique。当数据库中正好有一个条目满足指定的查询unique就成功。

列出所有正好管一个人的人。

```scheme
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
(define (unique-query exps) (car exps))

; test
(unique (job ?x (computer wizard)))
;;; Query results:
(unique (job (Bitdiddle Ben) (computer wizard)))

(unique (job ?x (computer programmer)))

; list all the jobs that are filled by only one person
(and (job ?x ?j) 
     (unique (job ?anyone ?j)))
; list all people who supervise precisely one person
(and (supervisor ?x ?s)
     (unique (supervisor ?any ?s)))
;;; Query results:
(and (supervisor (Cratchet Robert) (Scrooge Eben)) (unique (supervisor (Cratchet Robert) (Scrooge Eben))))
(and (supervisor (Reasoner Louis) (Hacker Alyssa P)) (unique (supervisor (Reasoner Louis) (Hacker Alyssa P))))
```

Exercise 4.76: 我们的and实现优雅但是低效，因为在处理and的第二个查询时，我们必须为第一个查询产生的每个帧都扫描一遍数据库。如果数据库有n个元素，一次查询产生n/k个结果，对第一次查询产生的所有帧扫描数据库是是n^2/k复杂度。

另一个方法是单独处理and的子句，然后寻找结果帧兼容的对。如果每个查询产生n/k输出帧，这意味着我们必须执行n^2/k^2次兼容性检查。设计一个使用该策略的and实现。你必须实现一个过程接收两个帧作为输入，检查帧的绑定是否兼容，如果是就合并两个绑定的集合生成一个新帧。这个操作类似于unification

```scheme
(define (conjoin conjuncts frame-stream hist)
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
  (cond
    ((empty-frame? f1) (singleton-stream f2))
    (else (let ((binding (first-binding f1)))
            (let ((var (binding-variable binding))
                  (val (binding-value binding)))
              (let ((nf2 (extend-if-consistent var val f2)))
                (if (eq? nf2 'failed)
                    the-empty-stream
                    (merge-if-consistent (rest-bindings f1) nf2))))))))

(define (first-binding frame)
  (car frame))
(define (rest-bindings frame)
  (cdr frame))
(define (empty-frame? frame)
  (null? frame))

(wheel ?who)

;;; Query results:
(wheel (Warbucks Oliver))
(wheel (Warbucks Oliver))
(wheel (Bitdiddle Ben))
(wheel (Warbucks Oliver))
(wheel (Warbucks Oliver))
```

and改成并行之后，原先的not和lisp-value等过滤器的行为就变了，不过变得更加符合直觉了。

Exercise 4.77: 在4.4.3中我们看到了not和lisp-value会导致查询语言给出错误的答案，如果这些过程操作应用在变量没有绑定的帧上。设计一个方法来修复这个短处。一个想法是以延迟的方式执行过滤，通过给帧附加一个promise，只有当足够多的变量已经绑定了，使得操作成为可能时才执行过滤。我们可以等到其他所有操作都执行了之后再执行过滤。但是，为了效率的考虑，我们应该尽早执行过滤以减少产生的中间帧的数量。

```scheme
; frame增加promise，包含query，满足条件时的filter。帧中增加绑定时，进行检查。如果到最后都没绑定，在loop中过滤还是不过滤？
; 在哪里执行检查是个问题，因为无法在内部把当前帧过滤掉，根据extend的返回值来判断。
; promise实现为对外部透明的，外部无法感知其存在。
; 判断条件，怎么确定执行操作成为可能？对于assertion直接可以判断，rule需要unifymatch，复合语句则更加不好判断，需要递归执行。对于像not，lisp-value这样的filter总是需要所有的变量都绑定才能进行操作。
; hist应该也要用外面传进来的，这里简单起见就不改了。
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

(define (new-negate operands frame-stream hist)
  (let* ((query (negated-query operands))
         (promise (make-promise query
                                (lambda (query frame)
                                  (stream-null?
                                   (qeval query
                                          (singleton-stream frame)
                                          hist))))))
    (frame-stream-add-and-try-promise promise frame-stream)))

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

; type: 'once|'all|'force
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
                   ; evaluate the filter with frame without pr-
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

; other modification
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

; test
(and (supervisor ?x ?y)
     (not (job ?x (computer programmer))))
;;; Query results:
(and (supervisor (Aull DeWitt) (Warbucks Oliver)) (not (job (Aull DeWitt) (computer programmer))))
(and (supervisor (Cratchet Robert) (Scrooge Eben)) (not (job (Cratchet Robert) (computer programmer))))
(and (supervisor (Scrooge Eben) (Warbucks Oliver)) (not (job (Scrooge Eben) (computer programmer))))
(and (supervisor (Bitdiddle Ben) (Warbucks Oliver)) (not (job (Bitdiddle Ben) (computer programmer))))
(and (supervisor (Reasoner Louis) (Hacker Alyssa P)) (not (job (Reasoner Louis) (computer programmer))))
(and (supervisor (Tweakit Lem E) (Bitdiddle Ben)) (not (job (Tweakit Lem E) (computer programmer))))

(and (not (job ?x (computer programmer)))
     (supervisor ?x ?y))

(and (salary (Bitdiddle Ben) ?x)
     (salary ?name ?y)
     (lisp-value < ?y ?x))
;;; Query results:
(and (salary (Bitdiddle Ben) 60000) (salary (Aull DeWitt) 25000) (lisp-value < 25000 60000))
(and (salary (Bitdiddle Ben) 60000) (salary (Cratchet Robert) 18000) (lisp-value < 18000 60000))
(and (salary (Bitdiddle Ben) 60000) (salary (Reasoner Louis) 30000) (lisp-value < 30000 60000))
(and (salary (Bitdiddle Ben) 60000) (salary (Tweakit Lem E) 25000) (lisp-value < 25000 60000))
(and (salary (Bitdiddle Ben) 60000) (salary (Fect Cy D) 35000) (lisp-value < 35000 60000))
(and (salary (Bitdiddle Ben) 60000) (salary (Hacker Alyssa P) 40000) (lisp-value < 40000 60000))
(and (lisp-value < ?y ?x)
     (salary (Bitdiddle Ben) ?x)
     (salary ?name ?y))

; force evaluate promise
(not (job ?x (computer programmer)))
; expect to signal error
(and (lisp-value < ?y 60000) (salary (Bitdiddle Ben) 60000))
```



Exercise 4.78: 重新设计查询语言为一个非确定性程序而不是流处理，使用4.3节中的解释器。在这个方法中，每个查询将产生单个回答（而不是所有回答的流），并且用户可以输入try-again来看更多的结果。你应该发现，我们在本节中建立的大部分机制都被非确定性搜索和回溯所归纳。但是，你可能也会发现我们的新查询语言在行为上跟之前有着微妙的区别。你能找到例子来说明这种差异吗？

```scheme
; 因为amb本来已经有搜索和回溯功能，我们所有的函数都只需要处理一个帧，结果传递这个帧
; 如果帧不符合条件了不再表示为'failed，而是直接用(amb)进行下一个尝试
; 外层driver-loop不需要了，amb解释器已经有个loop了
; 不过需要封装一个逻辑解释器函数，把原先query-driver-loop里的逻辑塞进去
; simple-query原先是fetch-assertions和rules，现在变成amb整个list
; lisp-value可以直接require过滤
; not需要must-fail特殊形式支持
; unique需要count=特殊形式支持
; 定义error，在逻辑解释器层捕获错误，把continuation放到frame里面
; call/cc需要我们自己实现，因为不能直接用底层的高阶函数，好在amb中比较容易实现。
; 一些特殊形式不能直接使用，需要在amb解释器中用函数包一下。
; 这种方式不能进行交叉处理，不过可以用ramb

; get/put
(define lof '())
(define (put tag op func)
  (set! lof (cons (list op tag func) lof)))
(define (get tag op)
  (define (iter l)
    (cond
      ((null? l) false) ; automatically create table for database
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
(define (ndp query)
  (newline)
  (display input-prompt)
  (newline)
  (display query)
  (let ((q (query-syntax-process query)))
    (cond ((assertion-to-be-added? q)
           (add-rule-or-assertion! 
            (add-assertion-body q))
           (newline)
           (display 
            "Assertion added to data base."))
          (else
           (newline)
           (display output-prompt)
           (call/cc
            (lambda (throw)
              ; empty frame, empty hist
              (let ((frame (try-promises (qeval q (make-frame '() '() throw) '())
                                         'force)))
                (instantiate q frame
                             (lambda (v f)
                             (contract-question-mark v))))))))))

(define (error frame l)
  (newline)
  (display "ERROR: ")
  (display l)
  ((frame-throw frame) 'error))

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

(define (qeval query frame hist)
  (let ((qproc (get (type query) 'qeval)))    ; data-directed dispatch
    (if qproc
        (qproc (contents query) frame hist) ; special form
        (simple-query query frame hist))))

(define (simple-query query-pattern 
                      frame hist)
  (amb (find-assertions query-pattern frame)
       (apply-rules query-pattern frame hist)))

; compound queries
(define (conjoin conjuncts frame hist)
  (if (empty-conjunction? conjuncts)
      frame
      (conjoin (rest-conjuncts conjuncts)
               (qeval 
                (first-conjunct conjuncts)
                frame hist)
               hist)))

; another strategy for and
; parallely eval clauses and then merge
(define (new-conjoin conjuncts frame hist)
  (define (iter conjuncts result-frame)
    (if (empty-conjunction? conjuncts)
        result-frame
        (let ((new-frame (qeval (first-conjunct conjuncts)
                                frame
                                hist)))
          (iter (rest-conjuncts conjuncts)
                    (merge-if-consistent result-frame new-frame)))))
  (iter conjuncts frame))

(put 'and 'qeval conjoin)

(define (disjoin disjuncts frame hist)
  (require (not (empty-disjunction? disjuncts)))
  (amb (qeval (first-disjunct disjuncts)
              frame hist)
       (disjoin (rest-disjuncts disjuncts)
                frame hist)))

(put 'or 'qeval disjoin)

(define (uniquely-asserted operands frame hist)
  (count= 1 (qeval (unique-query operands)
                   frame
                   hist))
  (qeval (unique-query operands) frame hist))

(put 'unique 'qeval uniquely-asserted)

; filters
; need must-fail
(define (negate operands frame hist)
  (must-fail
   (qeval (negated-query operands)
          frame
          hist))
  frame)

(define (new-negate operands frame hist)
  (let* ((query (negated-query operands))
         (promise
          (make-promise query
                        (lambda (query frame)
                          (must-fail
                           (qeval query frame hist))
                          frame))))
    (add-and-try-promise promise frame)))

(put 'not 'qeval new-negate)

(define (lisp-value call frame hist)
  (require (execute
            (instantiate call frame
                         (lambda (v f)
                           (error f
                                  (list "Unknown pat var: LISP-VALUE" v))))))
  frame)

(define (new-lisp-value call frame hist)
  (let ((promise
         (make-promise call
                       (lambda (query frame)
                         (require
                          (execute
                           (instantiate query frame
                                        (lambda (v f)
                                          (display "in unbound handler")
                                         (error f
                                                (list "Unknown pat var: LISP-VALUE" v)
                                          )))))
                         frame))))
    (add-and-try-promise promise frame)))

(put 'lisp-value 'qeval new-lisp-value)

(define (add-and-try-promise promise frame)
  (try-promises
   (add-promise-to-frame promise frame) 'once)) ; only try the promise we just extend

; only eval the predicate
(define (execute exp)
  (apply (eval (predicate exp)
               (make-base-namespace))
         (args exp)))
;(define user-initial-environment '())

; used for rules without bodies
(define (always-true ignore frame hist) 
  frame)
(put 'always-true 'qeval always-true)

(define (find-assertions pattern frame)
  (let ((assertion (fetch-assertions pattern frame)))
    (check-an-assertion assertion pattern frame)))

(define (check-an-assertion 
         assertion query-pat query-frame)
  (pattern-match query-pat assertion query-frame))

(define (pattern-match pat dat frame)
  (cond ((equal? pat dat) frame)
        ((var? pat) 
         (extend-if-consistent 
          pat dat frame))
        ((and (pair? pat) (pair? dat))
         (pattern-match 
          (cdr pat)
          (cdr dat)
          (pattern-match
           (car pat) (car dat) frame)))
        (else (amb))))

(define (extend-if-consistent var dat frame)
  (let ((binding (binding-in-frame var frame)))
    (if binding
        (pattern-match 
         (binding-value binding) dat frame)
        (extend var dat frame))))

(define (merge-if-consistent f1 f2)
  (define (iter bindings f2)
    (cond
     ((empty-bindings? bindings) f2)
     (else (let ((binding (first-binding bindings)))
            (let ((var (binding-variable binding))
                  (val (binding-value binding)))
              (iter (rest-bindings bindings)
                    (extend-if-consistent var val f2)))))))
  (iter (frame-bindings f1) f2))

; rules and unification
(define (apply-rules pattern frame hist)
  (let ((rule (fetch-rules pattern frame)))
    (apply-a-rule rule pattern frame hist)))

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
      (let ((state (pattern-state query-pattern unify-result)))
        (if (in-progress? conclusion state unify-result hist)
            (amb)
            (qeval (rule-body clean-rule)
                   unify-result
                   (extend-history conclusion query-pattern state hist)))))))

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
  (cond ((equal? p1 p2) frame)
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
        (else (amb))))

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
           (amb))
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
       ((empty-history? hist) false)
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

; in addition to storing all assertions in one big list
; we store all assertions whose cars are constant symbols in separate lists,
; in a table indexed by the symbol
(define THE-ASSERTIONS '())

(define (fetch-assertions pattern frame)
  (if (use-index? pattern)
      (get-indexed-assertions pattern)
      (get-all-assertions)))

(define (get-all-assertions)
  (list-amb THE-ASSERTIONS))

(define (get-indexed-assertions pattern)
  (list-amb (get-list (index-key-of pattern)
                      'assertion-list)))

; return '() if not exist
(define (get-list key1 key2)
  (let ((s (get key1 key2)))
    (if s s '())))

; rules are stored using the car of the rule conclusion
; A pattern whose car is a constant symbol can match rules
; whose conclusions start with a variable as well as rules
; whose conclusions have the same car.
; we store all rules whose conclusions starts with a variable
; in a separate list in our table, indexed by the symbol ?
(define THE-RULES '())

(define (fetch-rules pattern frame)
  (if (use-index? pattern)
      (get-indexed-rules pattern)
      (get-all-rules)))

(define (get-all-rules)
  (list-amb THE-RULES))

(define (get-indexed-rules pattern)
  (list-amb
   (append (get-list (index-key-of pattern) 'rule-list)
           (get-list '? 'rule-list))))

(define (add-rule-or-assertion! assertion)
  (if (rule? assertion)
      (add-rule! assertion)
      (add-assertion! assertion)))

(define (add-assertion! assertion)
  (store-assertion-in-index assertion)
  (let ((old-assertions THE-ASSERTIONS))
    (set! THE-ASSERTIONS
          (cons assertion 
                old-assertions))
    'ok))

(define (add-rule! rule)
  (store-rule-in-index rule)
  (let ((old-rules THE-RULES))
    (set! THE-RULES
          (cons rule old-rules))
    'ok))

(define (store-assertion-in-index assertion)
  (if (indexable? assertion)
      (let ((key (index-key-of assertion)))
        (let ((current-assertion-list
               (get-list 
                key 'assertion-list)))
          (put key
               'assertion-list
               (cons 
                assertion
                current-assertion-list))))
      (void)))

(define (store-rule-in-index rule)
  (let ((pattern (rule-conclusion rule)))
    (if (indexable? pattern)
        (let ((key (index-key-of pattern)))
          (let ((current-rule-list
                 (get-list 
                  key 'rule-list)))
            (put key
                 'rule-list
                 (cons
                  rule
                  current-rule-list))))
        (void))))

(define (indexable? pat)
  (or (constant-symbol? (car pat))
      (var? (car pat))))

(define (index-key-of pat)
  (let ((key (car pat)))
    (if (var? key) '? key)))

(define (use-index? pat)
  (constant-symbol? (car pat)))

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
(define (make-frame bindings promises throw)
  (list bindings promises throw))   

(define (frame-bindings frame)
  (car frame))
(define (frame-promises frame)
  (cadr frame))
(define (frame-throw frame)
  (caddr frame))

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
              (frame-promises frame)
              (frame-throw frame)))

(define (add-promise-to-frame promise frame)
  (make-frame (frame-bindings frame)
              (extend-promises promise
                               (frame-promises frame))
              (frame-throw frame)))

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
    (else true)))

(define (try-promises frame type)
  (let ((bindings (frame-bindings frame))
        (promises (frame-promises frame))
        (throw (frame-throw frame)))
    (let ((frame-w/o-promises (make-frame bindings '() throw)))
      (define (iter promises delayed-promises count)
        (cond
          ((or (empty-promises? promises)
               (and (eq? 'once type) (= count 1)))
           (make-frame bindings delayed-promises throw))
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
                       (amb))
                   (iter (rest-promises promises)
                         (extend-promises promise delayed-promises)
                         (+ count 1))))))))
      (iter promises '() 0))))

; 数据库初始化
(define rules-and-assertions
'(
  (assert! (address (Bitdiddle Ben) 
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
(for-each ndp rules-and-assertions)
```

测试

```scheme
; amb evaluator special forms tests
; must-fail
(begin (must-fail (amb)) 2)
2
(begin (must-fail 1) 2)
;;; There are no more values of

; count=
(begin (count= 3 (amb 1 2 3)) 'succeed)
succeed
(begin (count= 3 (amb 1 2)) 'succeed)
;;; There are no more values of
(begin (count= 3 (amb 1 2 3 4)) 'succeed)
;;; There are no more values of

; call/cc
(call/cc (lambda (k) 'succeed))
succeed
(call/cc (lambda (k) (k 'fail)))
fail

; for-each
(for-each display '(1 2 3))
                 
; simple query
(ndp '(supervisor ?x (Bitdiddle Ben)))
; Result
(supervisor (Tweakit Lem E) (Bitdiddle Ben))
(supervisor (Fect Cy D) (Bitdiddle Ben))
(supervisor (Hacker Alyssa P) (Bitdiddle Ben))

; and/or/not/lisp-value/unique
(ndp '(and (supervisor ?x (Bitdiddle Ben))
     (address ?x ?y)))
; Result
(and (supervisor (Tweakit Lem E) (Bitdiddle Ben)) (address (Tweakit Lem E) (Boston (Bay State Road) 22)))
(and (supervisor (Fect Cy D) (Bitdiddle Ben)) (address (Fect Cy D) (Cambridge (Ames Street) 3)))
(and (supervisor (Hacker Alyssa P) (Bitdiddle Ben)) (address (Hacker Alyssa P) (Cambridge (Mass Ave) 78)))

(ndp '(or (supervisor ?x (Bitdiddle Ben)) (supervisor (Bitdiddle Ben) ?y)))

(ndp '(and (salary (Bitdiddle Ben) ?x)
     (salary ?name ?y)
     (lisp-value < ?y ?x)))
(and (salary (Bitdiddle Ben) 60000) (salary (Aull DeWitt) 25000) (lisp-value < 25000 60000))
(and (salary (Bitdiddle Ben) 60000) (salary (Cratchet Robert) 18000) (lisp-value < 18000 60000))
(and (salary (Bitdiddle Ben) 60000) (salary (Reasoner Louis) 30000) (lisp-value < 30000 60000))
(and (salary (Bitdiddle Ben) 60000) (salary (Tweakit Lem E) 25000) (lisp-value < 25000 60000))
(and (salary (Bitdiddle Ben) 60000) (salary (Fect Cy D) 35000) (lisp-value < 35000 60000))
(and (salary (Bitdiddle Ben) 60000) (salary (Hacker Alyssa P) 40000) (lisp-value < 40000 60000))

(ndp '(and (supervisor ?name ?super)
     (not (job ?super (computer . ?type)))
     (job ?super ?job)))
(and (supervisor (Aull DeWitt) (Warbucks Oliver)) (not (job (Warbucks Oliver) (computer . ?type))) (job (Warbucks Oliver) (administration big wheel)))
(and (supervisor (Cratchet Robert) (Scrooge Eben)) (not (job (Scrooge Eben) (computer . ?type))) (job (Scrooge Eben) (accounting chief accountant)))
(and (supervisor (Scrooge Eben) (Warbucks Oliver)) (not (job (Warbucks Oliver) (computer . ?type))) (job (Warbucks Oliver) (administration big wheel)))
(and (supervisor (Bitdiddle Ben) (Warbucks Oliver)) (not (job (Warbucks Oliver) (computer . ?type))) (job (Warbucks Oliver) (administration big wheel)))

(ndp '(unique (supervisor ?x (Bitdiddle Ben))))
(ndp '(unique (supervisor (Bitdiddle Ben) ?x)))

; promise
(ndp '(and (lisp-value < ?y ?x)
     (salary (Bitdiddle Ben) ?x)
     (salary ?name ?y)))
;same

(ndp '(and (not (job ?super (computer . ?type)))
     (supervisor ?name ?super)    
     (job ?super ?job)))
;same

; rule
(ndp '(can-replace ?x (Fect Cy D)))
(can-replace (Hacker Alyssa P) (Fect Cy D))
(can-replace (Bitdiddle Ben) (Fect Cy D))

(ndp '(bigshot ?p ?div))
(bigshot (Scrooge Eben) accounting)
(bigshot (Warbucks Oliver) administration)
(bigshot (Bitdiddle Ben) computer)

(ndp '(lives-near ?person-1 ?person-2))
(lives-near (Aull DeWitt) (Reasoner Louis))
(lives-near (Aull DeWitt) (Bitdiddle Ben))
(lives-near (Fect Cy D) (Hacker Alyssa P))
(lives-near (Bitdiddle Ben) (Reasoner Louis))

(ndp '(?x next-to ?y in (1 (2 3) 4)))
; 1 (2 3), (2 3) 4
(ndp '(?x next-to 1 in (2 1 3 1)))
; 2 1, 3 1

(ndp '(grandson Cain ?x))
(ndp '(hasson Lamech ?x))
(ndp '(grandson Methushael ?x))

(ndp '(wheel ?who))

; error
(ndp '(lisp-value < ?x ?y))

; inifite loop
(ndp '(assert! (rule (self-recursion ?x)
               (self-recursion ?x))))
(ndp '(self-recursion ?x))

(ndp '(assert! (rule (mutual-recursion-a ?x)
               (mutual-recursion-b ?x))))
(ndp '(assert! (rule (mutual-recursion-b ?x)
               (mutual-recursion-a ?x))))
(ndp '(mutual-recursion-a ?x))
(ndp '(mutual-recursion-b ?x))

(ndp '(assert! (rule (self-recursion-indirect ?x ?y)
               (self-recursion-indirect ?y ?x))))
(ndp '(self-recursion-indirect ?x ?y))
(ndp '(self-recursion-indirect 3 ?y))

(ndp '(assert! (married Minnie Mickey)))
(ndp '(assert! (rule (married ?x ?y)
               (married ?y ?x))))
;;; Query input:
(ndp '(married Mickey ?who))
;;; Query results:
(married Mickey Minnie)

(ndp '(last-pair (3) ?x)))
(ndp '(last-pair (1 2 3) ?x))
(ndp '(last-pair (2 ?x) (3)))
(ndp '(last-pair ?x (3)))

(ndp '(outranked-by (Bitdiddle Ben) ?who))
(ndp '(outranked-by ?who (Warbucks Oliver)))
```



amb解释器修改

```scheme
        ((must-fail? exp) (analyze-must-fail exp))
        ((count? exp) (analyze-count exp))
        ((call/cc? exp) (analyze-call/cc exp))
        
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

(define (must-fail? exp) (tagged-list? exp 'must-fail))
(define (must-fail-exp exp) (cadr exp))
(define (count? exp) (tagged-list? exp 'count=))
(define (count-number exp) (cadr exp))
(define (count-exp exp) (caddr exp))
(define (call/cc? exp) (tagged-list? exp 'call/cc))
(define (call/cc-lambda exp) (cadr exp))

; some list procedures
```



Exercise 4.79: 当我们在4.1节实现Lisp解释器的时候，我们看到如何使用局部环境来避免过程的参数名字冲突。在查询系统中，我们使用了一种不同的策略来避免名字冲突。每次我们应用一个规则时我们重命名变量为保证唯一的新名字。Lisp评估器的类似策略是取消局部环境，只是在我们每次应用程序时对程序主体中的变量进行重命名。

为查询语言实现一个规则应用方法，使用环境而不是重命名。看看你是否能在你的环境结构基础上，在查询语言中创建处理大型系统的结构，例如块状结构程序的规则模拟。你能把这些与在某个上下文中进行推理的问题联系起来，作为解决问题的一种方法吗？（例如：如果我假设P是真的，那么我就可以推导A和B）。这是一个开放的问题，一个好的回答可能值得一个ph.D.

Implement for the query language a rule-application method that uses environments rather than renaming.  See if you can build on your environment structure to create constructs in the query language for dealing with large systems, such as the rule analog of block-structured procedures.  Can you relate any of this to the problem of making deductions in a context (e.g., “If I supposed that P were true, then I would be able to deduce A and B.”) as a method of problem solving?  (This problem is open-ended.  A good answer is probably worth a Ph.D.)” 



# 5 Computing with Register Machines

My aim is to show that the heavenly machine is not a kind of divine, live being, but a kind of clockwork (and he who believes that a clock has soul attributes the maker’s glory to the work), insofar as nearly all the manifold motions are caused by a most simple and material force, just as all motions of the clock are caused by a single weight.

—Johannes Kepler (letter to Herwart von Hohenburg, 1605)

## 5.1 Designing Register Machines

Exercise 5.1: 设计一个注册机用迭代方法计算阶乘，画出数据路径和控制器图。

```scheme
(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product)
              (+ counter 1))))
  (iter 1 1))
```

### A Language for Describing Register Machines

为了使处理复杂机器成为可能，我们将创建一个语言，用文本的形式表现数据路径和控制器图的所有信息。

```scheme
(data-paths
 (registers
  ((name a)
   (buttons ((name a<-b) 
             (source (register b)))))
  ((name b)
   (buttons ((name b<-t)
             (source (register t)))))
  ((name t)
   (buttons ((name t<-r)
             (source (operation rem))))))
 (operations
  ((name rem)
   (inputs (register a) (register b)))
  ((name =)
   (inputs (register b) (constant 0)))))

(controller
 test-b                ; label
   (test =)            ; test
   (branch 
    (label gcd-done))  ; conditional branch
   (t<-r)              ; button push
   (a<-b)              ; button push
   (b<-t)              ; button push
   (goto 
    (label test-b))    ; unconditional branch
 gcd-done)             ; label
```

合并两个描述的信息到一起，将任意的button和操作名字替换为它们的行为的定义

```scheme
(controller
 test-b
   (test (op =) (reg b) (const 0))
   (branch (label gcd-done))
   (assign t (op rem) (reg a) (reg b))
   (assign a (reg b))
   (assign b (reg t))
   (goto (label test-b))
 gcd-done)
```

缺点：

重复描述数据路径元素，使得机器的实际数据路径结构变得模糊。

因为看起来像Lisp表达式，容易让鸡它们不是任意的Lisp表达式。

Exercise 5.2: 使用注册机语言描述练习5.1中的迭代阶乘机器。

```scheme
(controller
   (assign p (const 1))
   (assign c (const 1))
 test-c
   (test (op >) (reg c) (reg n))
   (branch (label fac-done))
   (assign p (op prod) (reg p) (reg c))
   (assign c (op sum) (reg c) (const 1))
   (goto (label test-c))
 fac-done)

(define factorial-machine
  (make-machine
   '(p c n)
   (list (list 'prod *)
         (list 'sum +)
         (list '> >))
   '( (assign p (const 1))
      (assign c (const 1))
     test-c
      (test (op >) (reg c) (reg n))
      (branch (label fac-done))
      (assign p (op prod) (reg p) (reg c))
      (assign c (op sum) (reg c) (const 1))
      (goto (label test-c))
     fac-done)))

(set-register-contents! factorial-machine 'n 5)
(start factorial-machine)
(get-register-contents factorial-machine 'p)
```



**Actions**

read from outside of the machine, print to outside of the machine

### Abstraction in Machine Design

Exercise 5.3: 设计一个机器使用Newton法计算平方根，如1.1.7中描述的那样

```scheme
(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))
```

假设good-enough?和improve已经是基本操作，然后展示怎么扩展为算术操作。给每个版本画一个数据路径图，写一个控制器定义。

```scheme
(controller
  (assign g (const 1.0))
 test-g
  (test (op good-enough?) (reg g))
  (branch (label sqrt-done))
  (assign g (op improve) (reg g))
  (goto (label test-g))
 sqrt-done
  (perform (op print) (reg g)))

(define sqrt-machine
  (make-machine
   '(x g t)
   (list (list '- -)
         (list '< <)
         (list '> >)
         (list '/ /)
         (list '* *)
         (list '+ +)
         (list 'print print))
   '( (assign g (const 1.0))
     test-g
      (assign t (op *) (reg g) (reg g))
      (assign t (op -) (reg t) (reg x))
      (test (op >) (reg t) (const 0))
      (branch (label jump))
      (assign t (op -) (reg t))
     jump
      (test (op <) (reg t) (const 0.001))
      (branch (label sqrt-done))
      (assign t (op /) (reg x) (reg g))
      (assign t (op +) (reg g) (reg t))
      (assign t (op /) (reg t) (const 2.0))
      (assign g (reg t))
      (goto (label test-g))
     sqrt-done
      (perform (op print) (reg g)))))

(set-register-contents! sqrt-machine 'x 2)
(start sqrt-machine)
```

### Subroutines

使用子程序使得数据路径元件和控制器序列得到重用。continue寄存器已经放有当子程序执行结束时控制器序列继续执行的入口点。实现这个策略需要在数据路径和控制器之间有一种新的连接。必须有一种方法将控制器序列中的标签复制给寄存器（sp）

```scheme
gcd
 (test (op =) (reg b) (const 0))
 (branch (label gcd-done))
 (assign t (op rem) (reg a) (reg b))
 (assign a (reg b))
 (assign b (reg t))
 (goto (label gcd))
gcd-done
 (goto (reg continue))
  …
;; Before calling gcd, 
;; we assign to continue the label
;; to which gcd should return.
 (assign continue (label after-gcd-1))
 (goto (label gcd))
 after-gcd-1
  …
;; Here is the second call to gcd,
;; with a different continuation.
 (assign continue (label after-gcd-2))
 (goto (label gcd))
after-gcd-2
```

共享continue寄存器的话，在连续调用的时候，需要保存上一个的值。

### Using a Stack to Implement Recursion

实现递归进程，要求额外的机制。不像迭代，下一个子问题直接替换上一个问题。递归不能直接替换，所以就嵌套包含机器。不过同一时间只有一个是active状态的，所以我们仍然可以重复同一个机器，推迟主问题，重用工作来解决子问题，然后继续被悬置的计算。所以机器必须保存那些在子问题解决后还需要使用的寄存器的内容。因为要后进先出所以用**栈**来保存。我们扩展语言加入save和restore指令。

控制器序列的重用则通过自程序来完成。

```scheme
(controller
  (assign continue (label fact-done))
 fact-loop
  (test (op =) (reg n) (const 1))
  (branch (label base-case))
  (save continue)
  (save n)
  (assign n (op -) (reg n) (const 1))
  (assign continue (label after-fact))
  (goto (label fact-loop))
 after-fact
  (restore n)
  (restore continue)
  (assign val (op *) (reg n) (reg val))
  (goto (reg continue))
 base-case
  (assign val (const 1))
  (goto (reg continue))
 fact-done)
```



**A double recursion**

```scheme
(controller
   (assign continue (label fib-done))
 fib-loop
   (test (op <) (reg n) (const 2))
   (branch (label immediate-answer))
   ;; set up to compute Fib(n − 1)
   (save continue)
   (assign continue (label afterfib-n-1))
   (save n)           ; save old value of n
   (assign n 
           (op -)
           (reg n)
           (const 1)) ; clobber n to n-1
   (goto 
    (label fib-loop)) ; perform recursive call
 afterfib-n-1 ; upon return, val contains Fib(n − 1)
   (restore n)
   (restore continue)
   ;; set up to compute Fib(n − 2)
   (assign n (op -) (reg n) (const 2))
   (save continue)
   (assign continue (label afterfib-n-2))
   (save val)         ; save Fib(n − 1)
   (goto (label fib-loop))
 afterfib-n-2 ; upon return, val contains Fib(n − 2)
   (assign n 
           (reg val)) ; n now contains Fib(n − 2)
   (restore val)      ; val now contains Fib(n − 1)
   (restore continue)
   (assign val        ; Fib(n − 1) + Fib(n − 2)
           (op +) 
           (reg val)
           (reg n))
   (goto              ; return to caller,
    (reg continue))   ; answer is in val
 immediate-answer
   (assign val 
           (reg n))   ; base case: Fib(n) = n
   (goto (reg continue))
 fib-done)
```

Exercise 5.4: 指定实现给下面每个程序的寄存器机器。给每个机器写一个控制器指令序列并画一个数据路径图。

```scheme
(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))

(define (expt b n)
  (define (expt-iter counter product)
    (if (= counter 0)
        product
        (expt-iter (- counter 1)
                   (* b product))))
  (expt-iter n 1))
```

控制指令

```scheme
(controller
  (assign continue (label expt-done))
 expt-loop
  (test (op =) (reg n) (const 0))
  (branch (label base-case))
  (save continue)
  (assign continue (label after-expt))
  (assign n (op -) (reg n) (const 1))
  (goto (label expt-loop))
 after-expt
  (restore continue)
  (assign val (op *) (reg b) (reg val))
  (goto (reg continue))
 base-case
  (assign val (const 1))
  (goto (reg continue))
 expt-done)

(controller
  (assign p (const 1))
 expt-iter
  (test (op =) (reg n) (const 0))
  (branch (label final-case))
  (assign n (op -) (reg n) (const 1))
  (assign p (op *) (reg b) (reg p))
  (goto (label expt-iter))
 final-case
  (assign val (reg p))
 expt-done)

(define expt-rec-machine
  (make-machine
   '(continue b n val)
   (list (list '= =)
         (list '- -)
         (list '* *)
         (list 'print print))
   '( (assign continue (label expt-done))
     expt-loop
      (test (op =) (reg n) (const 0))
      (branch (label base-case))
      (save continue)
      (assign continue (label after-expt))
      (assign n (op -) (reg n) (const 1))
      (goto (label expt-loop))
     after-expt
      (restore continue)
      (assign val (op *) (reg b) (reg val))
      (goto (reg continue))
     base-case
      (assign val (const 1))
      (goto (reg continue))
     expt-done
      (perform (op print) (reg val)))))

(set-register-contents! expt-rec-machine 'b 2)
(set-register-contents! expt-rec-machine 'n 3)
(start expt-rec-machine)

(define expt-iter-machine
  (make-machine
   '(b n p val)
   (list (list '= =)
         (list '- -)
         (list '* *)
         (list 'print print))
   '( (assign p (const 1))
     expt-iter
      (test (op =) (reg n) (const 0))
      (branch (label final-case))
      (assign n (op -) (reg n) (const 1))
      (assign p (op *) (reg b) (reg p))
      (goto (label expt-iter))
     final-case
      (assign val (reg p))
     expt-done
      (perform (op print) (reg val)))))

(set-register-contents! expt-iter-machine 'b 2)
(set-register-contents! expt-iter-machine 'n 3)
(start expt-iter-machine)
```

Exercise 5.5: 手动仿真阶乘和fibonacci机器，使用一些非琐碎的输入，展示在执行过程中在每个有意义点栈中的内容。

Exercise 5.6: Ben观察到Fibonacci机器的控制器序列有一个额外的save和restore，可以删除它们使机器更快。找出它们。

中间continue不用restore再save。

### Instructions Summary

```scheme
(assign ⟨register-name⟩ (reg ⟨register-name⟩))
(assign ⟨register-name⟩ 
        (const ⟨constant-value⟩))
(assign ⟨register-name⟩ 
        (op ⟨operation-name⟩) 
        ⟨input₁⟩ … ⟨inputₙ⟩)
(perform (op ⟨operation-name⟩) 
         ⟨input₁⟩ 
         … 
         ⟨inputₙ⟩)
(test (op ⟨operation-name⟩) 
      ⟨input₁⟩ 
      … 
      ⟨inputₙ⟩)
(branch (label ⟨label-name⟩))
(goto (label ⟨label-name⟩))

(assign ⟨register-name⟩ (label ⟨label-name⟩))
(goto (reg ⟨register-name⟩))

(save ⟨register-name⟩)
(restore ⟨register-name⟩)
; constant values include numbers, strings, symbols and lists
```



## 5.2 A Register-Machine Simulator

Exercise 5.7: 使用模拟器测试练习5.4中设计的机器。

### The Machine Model

### The Assembler

Exercise 5.8: 下面的代码有歧义，因为标签here出现了不止一次。

```scheme
start
  (goto (label here))
here
  (assign a (const 3))
  (goto (label there))
here
  (assign a (const 4))
  (goto (label there))
there
```

用我们前面写的模拟器，当控制流到大there的时候，寄存器a中的内容是什么？修改extract-labels函数，使得汇编器遇到同一个标签名字指示不同位置时触发错误。

因为是cons的，所以查找先找到前面的here，所以a的值是3

```scheme
(define (extract-labels text receive)
  (if (null? text)
      (receive '() '())
      (extract-labels 
       (cdr text)
       (lambda (insts labels)
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
                     labels)))
               (receive 
                (cons (make-instruction 
                       next-inst)
                      insts)
                labels)))))))
```

### Generating Execution Procedures for Instructions

Exercise 5.9: 上面对机器操作的处理允许参数为标签、常数和寄存器内容。修改表达式处理函数强制操作只能与寄存器和常数使用。

```scheme
(define (make-operation-exp
         exp machine labels operations)
  (let ((op (lookup-prim 
             (operation-exp-op exp)
             operations))
        (aprocs
         (map (lambda (e)
                (if (label-exp? e)
                    (error "operations can't be used with labels: MAKE-OPERATION-EXP" e)
                    (make-primitive-exp 
                      e machine labels)))
              (operation-exp-operands exp))))
    (lambda () (apply op (map (lambda (p) (p))
                              aprocs)))))
```

Exercise 5.10: 给寄存器机器指令设计一个新语法，修改模拟器使用新的语法。你能实现你的新语法，只需要修改本节中的语法函数而不用动模拟器的其他任何部分吗？

Exercise 5.11: restore的几种合理的可能

1. 不限制，将栈中最后一个值放入

   ```scheme
      (assign n 
              (reg val)) ; n now contains Fib(n − 2)
      (restore val)      ; val now contains Fib(n − 1)”
   ; 改成 
   (restore n)
   ```

   

2. 将栈中最后一个值放入y，但是必须是之前从y存的，否则报错

   ```scheme
   ; save the reg name to stack
   (define (make-save inst machine stack pc)
     (let ((reg (get-register 
                 machine
                 (stack-inst-reg-name inst))))
       (lambda ()
         (push stack (cons (stack-inst-reg-name inst)
                           (get-contents reg)))
         (advance-pc pc))))
   
   ; check the reg name
   (define (make-restore inst machine stack pc)
     (let* ((reg-name (stack-inst-reg-name inst))
            (reg (get-register machine reg-name)))
       (lambda ()
         (let ((top (pop stack)))
           (if (eq? (car top) reg-name)
               (error "the value was not saved from" reg-name)
               (begin (set-contents! reg (pop stack))
                      (advance-pc pc)))))))
   ```

   

3. 将最后一个从y保存的值放到y中，不管y保存了之后是否存了其他寄存器。每个寄存器需要关联一个单独的栈。然后用initialize-stack操作初始化所有寄存器的栈。

   ```scheme
   (define (make-register name)
     (let ((contents '*unassigned*)
           (s '()))
       (define (set value)
         (set! contents value))
       (define (push)
         (set! s (cons contents s)))
       (define (pop)
         (if (null? s)
             (error "Empty stack: POP")
             (let ((top (car s)))
               (set! s (cdr s))
               (set top))))
       (define (initialize)
         (set! s '())
         'done)
       (define (dispatch message)
         (cond ((eq? message 'get) contents)
               ((eq? message 'set) set)
               ((eq? message 'push) (push))
               ((eq? message 'pop) (pop))
               ((eq? message 'initialize)
                (initialize))
               (else
                (error "Unknown request: 
                        REGISTER"
                       message))))
       dispatch))
   
   (define (pop reg) (reg 'pop))
   (define (push reg) (reg 'push))
   (define (initialize reg)
     (reg 'initialize)))
   
   ; remove stack in make-new-machine
   (define (make-new-machine)
     (let ((pc (make-register 'pc))
           (flag (make-register 'flag))
           (the-instruction-sequence '()))
       (let* (register-table
              (list (list 'pc pc) 
                    (list 'flag flag)))
             (the-ops
              (list
               (list 'initialize-stack
                     (for-each
                      (lambda (item)
                        (initialize
                         (cadr item)))
                      register-table)))))
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
                 (error "Unknown register:" 
                        name))))
         (define (execute)
           (let ((insts (get-contents pc)))
             (if (null? insts)
                 'done
                 (begin
                   ((instruction-execution-proc 
                     (car insts)))
                   (execute)))))
         (define (dispatch message)
           (cond ((eq? message 'start)
                  (set-contents! 
                   pc
                   the-instruction-sequence)
                  (execute))
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
                 ((eq? message 'operations) 
                  the-ops)
                 (else (error "Unknown request: 
                               MACHINE"
                              message))))
         dispatch)))
   
   (define (make-machine register-names 
                         ops 
                         controller-text)
     (let ((machine (make-new-machine)))
       (for-each (lambda (register-name)
                   ((machine 'allocate-register) 
                    register-name))
                 register-names)
       ((machine 'install-operations) ops)
       ((machine 'install-instruction-sequence)
        (assemble controller-text machine))
       ((machine 'initialize-stack))
       machine))
   
   (define (update-insts! insts labels machine)
     (let ((pc (get-register machine 'pc))
           (flag (get-register machine 'flag))
           (ops (machine 'operations)))
       (for-each
        (lambda (inst)
          (set-instruction-execution-proc!
           inst
           (make-execution-procedure
            (instruction-text inst) 
            labels
            machine
            pc
            flag
            ops)))
        insts)))
   
   (define (make-execution-procedure 
            inst labels machine pc flag ops)
     (cond ((eq? (car inst) 'assign)
            (make-assign 
             inst machine labels ops pc))
           ((eq? (car inst) 'test)
            (make-test 
             inst machine labels ops flag pc))
           ((eq? (car inst) 'branch)
            (make-branch 
             inst machine labels flag pc))
           ((eq? (car inst) 'goto)
            (make-goto inst machine labels pc))
           ((eq? (car inst) 'save)
            (make-save inst machine pc))
           ((eq? (car inst) 'restore)
            (make-restore inst machine pc))
           ((eq? (car inst) 'perform)
            (make-perform
             inst machine labels ops pc))
           (else (error "Unknown instruction 
                         type: ASSEMBLE"
                        inst))))
   
   (define (make-save inst machine pc)
     (let ((reg (get-register 
                 machine
                 (stack-inst-reg-name inst))))
       (lambda ()
         (push reg)
         (advance-pc pc))))
   
   (define (make-restore inst machine stack pc)
     (let ((reg (get-register
                 machine
                 (stack-inst-reg-name inst))))
       (lambda ()
         (pop reg)
         (advance-pc pc))))
   ```

Exercise 5.12: 模拟器可以帮助确定实现用一个给定的控制器实现一个机器所需的数据路径。扩展汇编器来存储信息到机器模型中。

- 所有指令的列表，消除重复之后以指令类型排序
- 用于保存入口点的寄存器的列表（goto指令所引用的）
- 被保存或恢复的寄存器的列表
- 对于每个寄存器，一个赋值源的列表

扩展消息传递接口提供访问新信息的接口。为了测试你的分析器，定义图5.12的Fibonacci机器，检查你构造的lists。(下面的没有进行排序)

```scheme
(define (inst-type inst)
  (car inst))

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '())
        (inst-infos '())
        (entry-point-infos '())
        (saved-reg-infos '())
        (reg-source-infos '()))
    (let ((the-ops
           (list 
            (list 'initialize-stack
                  (lambda () 
                    (stack 'initialize)))))
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
              (error "Unknown register:" 
                     name))))
      (define (execute)
        (let ((insts (get-contents pc)))
          (if (null? insts)
              'done
              (begin
                ((instruction-execution-proc 
                  (car insts)))
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
         (sort inst-infos (lambda (a b)
                            (string<? (symbol->string (inst-type a))
                                      (symbol->string (inst-type b))))))
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
      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents! 
                pc
                the-instruction-sequence)
               (execute))
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
              ((eq? message 'display-infos) (display-infos))
              (else (error "Unknown request: 
                            MACHINE"
                           message))))
      dispatch)))

(define (update-insts! insts labels machine)
  (let ((pc (get-register machine 'pc))
        (flag (get-register machine 'flag))
        (stack (machine 'stack))
        (ops (machine 'operations)))
    (for-each
     (lambda (inst)
       (gather-info (instruction-text inst) machine);change
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
```

测试

```scheme
(define fib-machine
  (make-machine
   '(continue n val)
   (list (list '+ +)
         (list '- -)
         (list '< <)
         (list 'print print))
   '( (assign continue (label fib-done))
 fib-loop
   (test (op <) (reg n) (const 2))
   (branch (label immediate-answer))
   ;; set up to compute Fib(n - 1)
   (save continue)
   (assign continue (label afterfib-n-1))
   (save n)           ; save old value of n
   (assign n 
           (op -)
           (reg n)
           (const 1)) ; clobber n to n-1
   (goto 
    (label fib-loop)) ; perform recursive call
 afterfib-n-1 ; upon return, val contains Fib(n - 1)
   (restore n)
   (restore continue)
   ;; set up to compute Fib(n - 2)
   (assign n (op -) (reg n) (const 2))
   (save continue)
   (assign continue (label afterfib-n-2))
   (save val)         ; save Fib(n - 1)
   (goto (label fib-loop))
 afterfib-n-2 ; upon return, val contains Fib(n - 2)
   (assign n 
           (reg val)) ; n now contains Fib(n - 2)
   (restore val)      ; val now contains Fib(n - 1)
   (restore continue)
   (assign val        ; Fib(n - 1) + Fib(n - 2)
           (op +) 
           (reg val)
           (reg n))
   (goto              ; return to caller,
    (reg continue))   ; answer is in val
 immediate-answer
   (assign val 
           (reg n))   ; base case: Fib(n) = n
   (goto (reg continue))
 fib-done)))

(set-register-contents! fib-machine 'n 5)
(start fib-machine)
(display-infos fib-machine)
```

Exercise 5.13: 修改模拟器使其使用控制器序列确定机器有哪些寄存器，而不是通过make-machine参数指定。不用预先分配寄存器，而是在第一次看到的时候分配。

```scheme
(define (make-machine ops 
                      controller-text)
  (let ((machine (make-new-machine)))
    ((machine 'install-operations) ops)
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine))
    machine))

      (define (lookup-register name)
        (let ((val
               (assoc name register-table)))
          (if val
              (cadr val)
              (begin (allocate-register name)
                     (lookup-register name)))))
```



### Monitoring Machine Performance

仿真不仅可以验证提示的机器设计的正确性，也可以测量机器的性能。例如，我们在程序中安装一个尺用来测量计算中的栈操作数量。我们修改栈追踪寄存器被保存的次数以及到达的最大深度，并增加一个栈接口的消息打印统计信息。

```scheme
; make-new-machine
(the-ops
           (list
            (list 'initialize-stack
                  (lambda ()
                    (stack 'initialize)))
            (list 'print-stack-statistics
                  (lambda ()
                    (stack 'print-statistics)))))

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
```

Exercise 5.14: 测量阶乘机器计算n!所需的压栈次数和最大深度。

Exercise 5.15: 给寄存器机器增加指令计数，给计算增加接口打印和重置计数。

```scheme
                ((instruction-execution-proc
                  (car insts)))
                (set! count (+ count 1))
                
              ((eq? message 'get-count) count)
              ((eq? message 'reset-count) (set! count 0))
```

Exercise 5.16: 扩展模拟器提供指令追踪，在每个指令执行前，模拟器打印指令的文本。使机器模型接收trace-on和trace-off消息进行开关。

```scheme
      (define (execute)
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
```



Exercise 5.17: 扩展指令追踪，在打印指令前，模拟器打印在当前指令前的标签。注意不要干扰练习5.15中的指令计数。你需要使模拟器维护必要的标签信息。

```scheme
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
                    (cons (make-label-entry insts next-inst) rlabels)))
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

(define (set-rlabels! machine rlabels)
  ((machine 'set-rlabels) rlabels))

      (define (execute)
        (let ((insts (get-contents pc)))
          (if tracing
              (let ((rlabel (lookup-rlabel rlabels insts)))
                (if rlabel
                    (begin (display rlabel) (newline)))))
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
      
(define (make-rlabel-entry insts label-name)
  (cons insts label-name))

(define (lookup-rlabel rlabels insts)
  (let ((val (assoc insts rlabels)))
    (if val
        (cdr val)
        false)))
```

把标签放在指令里有两个问题，把调试信息和代码信息混到一起了。而且，对于在代码末尾的标签，没有对应的指令。



Exercise 5.18: 修改make-register函数使得寄存器可以被追踪。寄存器应该接收消息打开和关闭追踪。当一个寄存器被追踪时，给寄存器赋值应该打印寄存器名字，旧值和新值。扩展机器模型接口允许打开和关闭指定寄存器的追踪。

```scheme
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

(define (reg-trace-on register)
  (register 'trace-on))

(define (reg-trace-off register)
  (register 'trace-off))

(define (trace-on-register machine register-name)
  (reg-trace-on (get-register machine register-name)))

(define (trace-off-register machine register-name)
  (reg-trace-off (get-register machine register-name)))
```



Exercise 5.19: Alyssa想写一个断点特性，帮助她调试机器的设计。她想要能够指定一个控制器序列的位置让模拟器停在那里，然后允许检查机器的状态。你将实现一个函数

`(set-breakpoint ⟨machine⟩ ⟨label⟩ ⟨n⟩)`

例如`(set-breakpoint gcd-machine 'test-b 4)`在给定的标签后面第4个指令前设置一个断点。当模拟器到达断点时，应该打印标签和断点的offset并停止执行指令。然后可以用get-register-contents和set-register-contents!操纵机器的状态。通过`proceed-machine <machine>`继续执行。也要能移除断点。

```scheme
(cancel-breakpoint <machine> <label> <n>)
(cancel-all-breakpoints <machine>)
```



```scheme
((eq? message 'proceed) (proceed))

(define (proceed-machine machine)
  (machine 'proceed))

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

; make-new-machine
        (pos (make-position 'start 0))
        (bps '())
        
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
      
              ((eq? message 'set-breakpoint) set-breakpoint)
              ((eq? message 'cancel-breakpoint) cancel-breakpoint)
              ((eq? message 'cancel-all-breakpoints) (cancel-all-breakpoints))
              
(define (set-breakpoint machine label n)
  ((machine 'set-breakpoint) label n))

(define (cancel-breakpoint machine label n)
  ((machine 'cancel-breakpoint) label n))

(define (cancel-all-breakpoints machine)
  (machine 'cancel-all-breakpoints))
```

测试

```scheme
(define fib-machine
  (make-machine
   (list (list '+ +)
         (list '- -)
         (list '< <)
         (list 'print print))
   '( (assign continue (label fib-done))
 fib-loop
   (test (op <) (reg n) (const 2))
   (branch (label immediate-answer))
   ;; set up to compute Fib(n - 1)
   (save continue)
   (assign continue (label afterfib-n-1))
   (save n)           ; save old value of n
   (assign n 
           (op -)
           (reg n)
           (const 1)) ; clobber n to n-1
   (goto 
    (label fib-loop)) ; perform recursive call
 afterfib-n-1 ; upon return, val contains Fib(n - 1)
   (restore n)
   (restore continue)
   ;; set up to compute Fib(n - 2)
   (assign n (op -) (reg n) (const 2))
   (save continue)
   (assign continue (label afterfib-n-2))
   (save val)         ; save Fib(n - 1)
   (goto (label fib-loop))
 afterfib-n-2 ; upon return, val contains Fib(n - 2)
   (assign n 
           (reg val)) ; n now contains Fib(n - 2)
   (restore val)      ; val now contains Fib(n - 1)
   (restore continue)
   (assign val        ; Fib(n - 1) + Fib(n - 2)
           (op +) 
           (reg val)
           (reg n))
   (goto              ; return to caller,
    (reg continue))   ; answer is in val
 immediate-answer
   (assign val 
           (reg n))   ; base case: Fib(n) = n
   (goto (reg continue))
 fib-done)))

(set-register-contents! fib-machine 'n 5)
;(trace-on fib-machine)
;(trace-on-register fib-machine 'n)
(set-breakpoint fib-machine 'fib-loop 1)
(set-breakpoint fib-machine 'afterfib-n-1 1)
(set-breakpoint fib-machine 'afterfib-n-2 2)
(set-breakpoint fib-machine 'afterfib-n-2 3)
(set-breakpoint fib-machine 'immediate-answer 1)
(start fib-machine)
(get-register-contents fib-machine 'n)
(cancel-breakpoint fib-machine 'fib-loop 1)
(cancel-breakpoint fib-machine 'afterfib-n-1 1)
(proceed-machine fib-machine)
(cancel-all-breakpoints fib-machine)
(proceed-machine fib-machine)
;(display-infos fib-machine)
```



## 5.3 Storage Allocation and Garbage Collection

实现list结构有两个考虑。一个纯粹是表现的问题。另一个是随着计算的进行的内存管理问题。



### Memory as Vectors

```scheme
; car/cdr
(assign ⟨reg₁⟩ (op car) (reg ⟨reg₂⟩))
(assign ⟨reg₁⟩ (op cdr) (reg ⟨reg₂⟩))

(assign ⟨reg₁⟩ 
        (op vector-ref)
        (reg the-cars)
        (reg ⟨reg₂⟩))
(assign ⟨reg₁⟩
        (op vector-ref)
        (reg the-cdrs)
        (reg ⟨reg₂⟩))

; set-car!/set-cdr!
(perform (op set-car!) (reg ⟨reg₁⟩) (reg ⟨reg₂⟩))
(perform (op set-cdr!) (reg ⟨reg₁⟩) (reg ⟨reg₂⟩))

(perform (op vector-set!)
         (reg the-cars)
         (reg ⟨reg₁⟩)
         (reg ⟨reg₂⟩))
(perform (op vector-set!)
         (reg the-cdrs)
         (reg ⟨reg₁⟩)
         (reg ⟨reg₂⟩))

; cons
(assign ⟨reg₁⟩
        (op cons)
        (reg ⟨reg₂⟩)
        (reg ⟨reg₃⟩))

(perform (op vector-set!)
         (reg the-cars)
         (reg free)
         (reg ⟨reg₂⟩))
(perform (op vector-set!)
         (reg the-cdrs)
         (reg free)
         (reg ⟨reg₃⟩))
(assign ⟨reg₁⟩ (reg free))
(assign free (op +) (reg free) (const 1))

; tests the equality of all fields in the registers
(op eq?) (reg ⟨reg₁⟩) (reg ⟨reg₂⟩)

; pair?, null?, symbol?, number? need only check the type field

; stack
; (save <reg>)
(assign the-stack 
        (op cons)
        (reg ⟨reg⟩)
        (reg the-stack))

; (restore <reg>)
(assign ⟨reg⟩ (op car) (reg the-stack))
(assign the-stack (op cdr) (reg the-stack))

; (perform (op initialize-stack))
(assign the-stack (const ()))
```



Exercise 5.20: box-and-pointer图

```scheme
(define x (cons 1 2))
(define y (list x x))

x     y  free
1  2  3  4
n1 p1 p1
n2 e0 p2
```

Exercise 5.21: 给下面的函数实现寄存器机器。假设list结构内存操作已经是机器基本操作

```scheme
(define (count-leaves tree)
  (cond ((null? tree) 0)
        ((not (pair? tree)) 1)
        (else 
         (+ (count-leaves (car tree))
            (count-leaves (cdr tree))))))

(define count-leaves-machine
  (make-machine
   (list (list 'null? null?)
         (list 'pair? pair?)
         (list 'car car)
         (list 'cdr cdr)
         (list '+ +)
         (list 'print print))
   '( (assign continue (label done))
     loop
      (test (op null?) (reg tree))
      (branch (label empty-case))
      (test (op pair?) (reg tree))
      (branch (label left-subtree))
      (goto (label leaf-case))
     left-subtree
      (save continue)
      (save tree)
      (assign tree (op car) (reg tree))
      (assign continue (label after-left-subtree))
      (goto (label loop))
     after-left-subtree 
      (restore tree)
      (restore continue) ; ***
      (save continue)    ; ***
      (save tree)        ; ***
      (save val)         ; count of left subtree
      (assign tree (op cdr) (reg tree))
      (assign continue (label after-right-subtree))
      (goto (label loop))
     after-right-subtree
      (restore t)        ; count of left subtree
      (restore tree)     ; ***
      (restore continue)
      (assign val (op +) (reg t) (reg val))
      (goto (reg continue))
     empty-case
      (assign val (const 0))
      (goto (reg continue))
     leaf-case
      (assign val (const 1))
      (goto (reg continue))
     done
      (perform (op print) (reg val)))))

(set-register-contents! count-leaves-machine 'tree '())
(set-register-contents! count-leaves-machine 'tree 1)
(set-register-contents! count-leaves-machine 'tree (cons 1 2))
(set-register-contents! count-leaves-machine 'tree '((1 2) (3 4) 5))
(start count-leaves-machine)
```

带显式计数器的递归版本

```scheme
(define (count-leaves tree)
  (define (count-iter tree n)
    (cond ((null? tree) n)
          ((not (pair? tree)) (+ n 1))
          (else 
           (count-iter 
            (cdr tree)
            (count-iter (car tree) 
                        n)))))
  (count-iter tree 0))

(define count-leaves-machine2
  (make-machine
   (list (list 'null? null?)
         (list 'pair? pair?)
         (list 'car car)
         (list 'cdr cdr)
         (list '+ +)
         (list 'print print))
   '( (assign continue (label done))
      (assign n (const 0))
     loop
      (test (op null?) (reg tree))
      (branch (label empty-case))
      (test (op pair?) (reg tree))
      (branch (label left-subtree))
      (goto (label leaf-case))
     left-subtree
      (save continue)
      (save tree)
      (assign tree (op car) (reg tree))
      (assign continue (label after-left-subtree))
      (goto (label loop))
     after-left-subtree 
      (restore tree)
      (restore continue) ; ***
      (save continue)    ; ***
      (save tree)        ; ***
      (assign n (reg val)) ; count of left subtree
      (assign tree (op cdr) (reg tree))
      (assign continue (label after-right-subtree))
      (goto (label loop))
     after-right-subtree
      (restore tree)     ; ***
      (restore continue)
      (goto (reg continue))
     empty-case
      (assign val (reg n))
      (goto (reg continue))
     leaf-case
      (assign val (op +) (reg n) (const 1))
      (goto (reg continue))
     done
      (perform (op print) (reg val)))))

(set-register-contents! count-leaves-machine2 'tree '())
(set-register-contents! count-leaves-machine2 'tree 1)
(set-register-contents! count-leaves-machine2 'tree (cons 1 2))
(set-register-contents! count-leaves-machine2 'tree '((1 2) (3 4) 5))
(start count-leaves-machine2)
```

Exercise 5.22: 3.3.1中的练习3.12给了两种append。给每个函数设计一个寄存器机器，假设list结构内存操作已经是基本操作。

```scheme
; 创建一个新pair
(define (append x y)
  (if (null? x)
      y
      (cons (car x) (append (cdr x) y))))

(define append-machine
  (make-machine
   (list (list 'null? null?)
         (list 'pair? pair?)
         (list 'car car)
         (list 'cdr cdr)
         (list 'cons cons)
         (list '+ +)
         (list 'print print))
   '( (assign continue (label done))
     loop
      (test (op null?) (reg x))
      (branch (label empty-case))
      (save continue)
      (save x)
      (assign x (op cdr) (reg x))
      (assign continue (label remaining))
      (goto (label loop))
     remaining
      (restore x)
      (restore continue)
      (assign x (op car) (reg x))
      (assign val (op cons) (reg x) (reg val))
      (goto (reg continue))
     empty-case
      (assign val (reg y))
      (goto (reg continue))
     done
      (perform (op print) (reg val)))))

(set-register-contents! append-machine 'x '(1 2))
(set-register-contents! append-machine 'y '(3 4))
(start append-machine)
(get-register-contents append!-machine 'x)

; 直接修改x的cdr
(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define append!-machine
  (make-machine
   (list (list 'null? null?)
         (list 'pair? pair?)
         (list 'car car)
         (list 'cdr cdr)
         (list 'cons cons)
         (list 'set-cdr! set-cdr!)
         (list '+ +)
         (list 'print print))
   '( (assign continue (label done))
      (save x)
     last-pair
      (assign t (op cdr) (reg x))
      (test (op null?) (reg t))
      (branch (label final-case))
      (assign x (reg t))
      (goto (label last-pair))
     final-case
      (assign val (reg x))
      (goto (reg continue))
     done
      (perform (op set-cdr!) (reg val) (reg y))
      (restore x)
      (assign val (reg x))
      (perform (op print) (reg val)))))

(set-register-contents! append!-machine 'x '(1 2))
(set-register-contents! append!-machine 'y '(3 4))
(start append!-machine)
(get-register-contents append!-machine 'x)
```

### Maintaining the Illusion of Infinite Memory

垃圾回收基于这样的观察，在Lisp解释器的任意时刻，能对计算的将来产生影响的对象只有那些，可以从当前正在机器寄存器中的指针开始通过连续的car和cdr操作访问的对象。

gc有很多方法。这里我们检查的方法是stop-and-copy，将内存分成working和free两部分。回收时把还有用的对拷贝到free memory，然后交换working和free的角色。

另一种常见的gc技术时mark-sweep。

stop-and-copy算法时大内存系统的主导算法，因为它只检查有用部分的内存。而mark-sweep在sweep阶段需要检查所有内存。另外stop-and-copy还是一个compacting的gc，当它gc结束有用的数据都被移到了连续的内存位置。

**stop-and-copy实现**

假设root寄存器包含的指针可以最终访问到所有数据。可以通过把所有机器寄存器内存保存在一个预分配的list中，然后root指向该list（不包括root，the-cars、the-cdrs等寄存器）。当前working memory由基址在寄存器the-cars和the-cdrs中的vectors组成，free memory则在寄存器new-cars和new-cdrs中。

当working memory用完时触发gc，即cons操作尝试增加free指针超出了vector的尾部。当gc完成时，root指针将指向新内存，所有可以通过root访问的对象都被移到了新内存，free指针也指向了新位置。此外，working memory和free memory也互换了。

gc处理的状态由两个指针free和scan控制。初始它们都指向新内存的开头。算法开始把root指向的pair重新定位的新内存的开头，一个pair拷贝之后，root指针调整指向新位置，free指针则递增。此外，pair的老位置做一下标记表示已经被移动了：在其car位置，放上一个特殊的tag表示已经移动，在cdr位置放一个forwarding address指向新的位置。

在重定位root之后，gc进入基础循环。每一步scan指针（初始指向重定位的root）指向一个pair，其已经被移到了新内存，但是它的car和cdr还引用旧内存中的对象。重定位这些对象，然后递增scan指针。重定位一个对象，我们检查对象是否已经被移动了，如果还没有，就拷贝到free指示的位置并更新free，设置broken heart标志，更新对象指针为新位置。如果对象已经被移动了，那么它的forwarding地址就用于替换正在被扫描的对中的指针。

最终所有可访问对象都被移动和扫描了，此时scan会赶上free，处理结束。

```scheme
; relocate root
begin-garbage-collection
  (assign free (const 0))
  (assign scan (const 0))
  (assign old (reg root))
  (assign relocate-continue 
          (label reassign-root))
  (goto (label relocate-old-result-in-new))
reassign-root
  (assign root (reg new))
  (goto (label gc-loop))

; main loop
gc-loop
  (test (op =) (reg scan) (reg free))
  (branch (label gc-flip))
  (assign old 
          (op vector-ref)
          (reg new-cars)
          (reg scan))
  (assign relocate-continue 
          (label update-car))
  (goto (label relocate-old-result-in-new))

  update-car
  (perform (op vector-set!)
           (reg new-cars)
           (reg scan)
           (reg new))
  (assign  old 
           (op vector-ref)
           (reg new-cdrs)
           (reg scan))
  (assign  relocate-continue
           (label update-cdr))
  (goto (label relocate-old-result-in-new))
update-cdr
  (perform (op vector-set!)
           (reg new-cdrs)
           (reg scan)
           (reg new))
  (assign  scan (op +) (reg scan) (const 1))
  (goto (label gc-loop))

; relocate old to new
; old and new are all index
relocate-old-result-in-new
  (test (op pointer-to-pair?) (reg old))
  (branch (label pair))
  (assign new (reg old))
  (goto (reg relocate-continue))
pair
  (assign  oldcr 
           (op vector-ref)
           (reg the-cars) ; working memory
           (reg old))
  (test (op broken-heart?) (reg oldcr))
  (branch  (label already-moved))
  (assign  new (reg free)) ; new location for pair
  ;; Update free pointer.
  (assign free (op +) (reg free) (const 1))
  ;; Copy the car and cdr to new memory.
  (perform (op vector-set!)
           (reg new-cars)
           (reg new)
           (reg oldcr))
  (assign  oldcr 
           (op vector-ref)
           (reg the-cdrs)
           (reg old))
  (perform (op vector-set!)
           (reg new-cdrs)
           (reg new)
           (reg oldcr))
  ;; Construct the broken heart.
  (perform (op vector-set!)
           (reg the-cars)
           (reg old)
           (const broken-heart))
  (perform (op vector-set!)
           (reg the-cdrs)
           (reg old)
           (reg new))
  (goto (reg relocate-continue))
already-moved
  (assign  new
           (op vector-ref)
           (reg the-cdrs)
           (reg old))
  (goto (reg relocate-continue))

; gc complete, interchange the role
gc-flip
  (assign temp (reg the-cdrs))
  (assign the-cdrs (reg new-cdrs))
  (assign new-cdrs (reg temp))
  (assign temp (reg the-cars))
  (assign the-cars (reg new-cars))
  (assign new-cars (reg temp))
```



## 5.4 The Explicit-Control Evaluator

显式控制解释器将展示解释过程中使用的过程调用和参数传递如何可以表示为关于寄存器和栈的操作。此外，它也可以作为一个用类似计算机原生机器语言的语言写的Scheme解释器的实现。

### The Core of the Explicit-Control Evaluator

```scheme
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
  (save env)
  (assign unev (op operands) (reg exp))
  (save unev)
  (assign exp (op operator) (reg exp))
  (assign
   continue (label ev-appl-did-operator))
  (goto (label eval-dispatch))

ev-appl-did-operator
  (restore unev)             ; the operands
  (restore env)
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
```

### Sequence Evaluation and Tail Recursion

```scheme
ev-begin
  (assign unev
          (op begin-actions)
          (reg exp))
  (save continue)
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
```

### Conditionals, Assignments, and Definitions

```scheme
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
```

Exercise 5.23: 扩展解释器处理派生表达式如cond, let等。你可以作弊，假设假设像cond->if这样的语法转换器已经是可用的机器操作。

```scheme
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
```



Exercise 5.24: 实现cond为一个新的基本特殊形式，而不是推导为if。你将要构造一个循环测试连续的cond子句谓词，直到找到一个真的，然后使用ev-sequence来求值子句的actions。

```scheme
ev-cond
 (assign unev (op cond-clauses) (reg exp))
loop
 (test (op null?) (reg unev))
 (branch (label no-else))
 (assign exp (op car) (reg unev)) ; first clause
 (test (op cond-else-clause?) (reg exp))
 (branch (label else-clause))
 
 (save continue)
 (save env)
 (save unev)  ; clauses
 (save exp)   ; save clause for later
 (assign continue (label after-predicate))
 (assign exp (op cond-predicate) (reg exp))
 (goto (label eval-dispatch))
after-predicate
 (restore exp) ; restore clause
 (restore unev)
 (restore env)
 (restore continue)
 (test (op true?) (reg val))       ; test predicate
 (branch (label actions))
 (assign unev (op cdr) (reg unev)) ; rest clauses
 (goto (label loop))
actions
 (assign unev (op cond-actions) (reg exp))
 (test (op apply-procedure?) (reg unev))
 (branch (label procedure-action))
 (test (op bad-syntax?) (reg unev))
 (branch (label bad-syntax))
 (save continue)  ; because in ev-sequnce it doesn't save continue
 (goto (label ev-sequence))       ; directly eval the actions
bad-syntax
 (assign val (const "COND clause syntax error!"))
 (goto (label signal-error))
procedure-action
 (assign argl (op list) (reg val))
 (save continue)
 (save env)
 (save argl)
 (assign exp (op actions-procedure) (reg unev))
 (assign continue (label after-operator))
 (goto (label eval-dispatch))
after-operator
 (restore argl)             ; the operands
 (restore env)
 ;(restore continue)        ; save again
 (assign proc (reg val))    ; the operator
 (goto (label apply-dispatch))
else-clause
 (assign unev (op cdr) (reg unev)) ;rest clause
 (test (op null?) (reg unev))
 (branch (label else-actions))
 (assign val (const "ELSE clause isn't last: COND"))
 (goto (label signal-error))
else-actions
 (assign unev (op cond-actions) (reg exp))
 (save continue)  ; because in ev-sequnce it doesn't save continue
 (goto (label ev-sequence))
no-else
 (assign val (const false))
 (goto (reg continue))
```



Exercise 5.25: 修改解释器使其使用normal-order求值，基于4.2节的lazy解释器。

应用复合过程时对参数delay-it，对operator、谓词条件以及序列除了最后一个，都使用actual-value

```scheme
read-eval-print-loop
  (perform (op initialize-stack))
  (perform (op prompt-for-input)
           (const ";;; EC-Eval input:"))
  (assign exp (op read))
  (assign env (op get-global-environment))
  (assign continue (label print-result))
  (goto (label actual-value)) ; actual-value **

actual-value
  (save continue)
  (assign continue (label force-it))
  (goto (label eval-dispatch))
force-it
  (restore continue)
  (test (op thunk?) (reg val))
  (branch (label force-thunk))
  (goto (reg continue))     ; not thunk, return directly
force-thunk
  (assign exp (op thunk-exp) (reg val))
  (assign env (op thunk-env) (reg val))
  (goto (label actual-value))
  
ev-application
  (save continue)
  (save env)
  (assign unev (op operands) (reg exp))
  (save unev)
  (assign exp (op operator) (reg exp))
  (assign
   continue (label ev-appl-did-operator))
  (goto (label actual-value)) ; actual-value **

ev-appl-did-operator
  (restore unev)             ; the operands
  (restore env)
  (assign argl (op empty-arglist))
  (assign proc (reg val))
  (test (op primitive-procedure?) (reg proc)) ; **
  (branch (label ev-appl-operand))            ; **
  (test (op compound-procedure?) (reg proc))  ; **
  (branch (label delay-appl-operand))         ; **
  (goto (label unknown-procedure-type))       ; **

ev-appl-operand                           ; **
  (test (op no-operands?) (reg unev))     ; **
  (branch (label primitive-apply))        ; **
  (save proc)                             ; **
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
  (goto (label actual-value)) ; actual-value **
  
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
  (goto (label actual-value)) ; ; actual-value **
ev-appl-accum-last-arg
  (restore argl)
  (assign argl
          (op adjoin-arg)
          (reg val)
          (reg argl))
  (restore proc)
  (goto (label primitive-apply))          ; **

delay-appl-operand
  (test (op no-operands?) (reg unev))
  (branch (label compound-apply))         ; **
  (assign val (op first-operand) (reg unev))
  (assign val (op delay-it) (reg val) (reg env))    ; delay-it
  (assign argl
          (op adjoin-arg)
          (reg val)
          (reg argl))
  (assign unev
          (op rest-operands)
          (reg unev))
  (goto (label delay-appl-operand))
  
ev-sequence
  (assign exp (op first-exp) (reg unev))
  (test (op last-exp?) (reg unev))
  (branch (label ev-sequence-last-exp))
  (save unev)
  (save env)
  (assign continue
          (label ev-sequence-continue))
  (goto (label actual-value)) ; actual-value **
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

ev-if
  (save exp)   ; save expression for later
  (save env)
  (save continue)
  (assign continue (label ev-if-decide))
  (assign exp (op if-predicate) (reg exp))
  ; evaluate the predicate:
  (goto (label actual-value))  ; actual-value **
```

另外增加所需的操作，thunk?, thunk-exp, thunk-env, delay-it

```scheme
(define count 0)
(define (id x) (set! count (+ count 1)) x)
(define w (id (id 10)))
count ;1
w ;10
count ;2
w ;10
count ;3
```



### Running the Evaluator

```scheme
read-eval-print-loop
  (perform (op initialize-stack))
  (perform (op prompt-for-input)
           (const ";;; EC-Eval input:"))
  (assign exp (op read))
  (assign env (op get-global-environment))
  (assign continue (label print-result))
  (goto (label eval-dispatch))
print-result
  ;(perform (op print-stack-statistics))
  (perform (op announce-output)
           (const ";;; EC-Eval value:"))
  (perform (op user-print) (reg val))
  (goto (label read-eval-print-loop))

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
  (goto (label read-eval-print-loop))

(define eceval-operations
  (list (list 'self-evaluating? 
              self-evaluating)
        ⟨complete list of operations 
         for eceval machine⟩))

(define eceval
  (make-machine
   '(exp env val proc argl continue unev)
   eceval-operations
   '(read-eval-print-loop
     ⟨entire machine controller 
      as given above⟩)))

(define the-global-environment
  (setup-environment))

(define (get-global-environment)
  the-global-environment)

(start eceval)
```

Exercise 5.26 使用监控的栈探索解释器的尾递归特性。启动解释器定义如下迭代factorial函数。

```scheme
(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product)
              (+ counter 1))))
  (iter 1 1))
```

你会发现最大栈深度跟n无关。这个深度是什么？最大深度是10。

给出总push操作数与n的关系。35n+29

Exercise 5.27: 为了对比，探索下面递归版本的函数

```scheme
(define (factorial n)
  (if (= n 1)
      1
      (* (factorial (- n 1)) n)))
```

看下最大深度和总push数的关系。

最大深度5n+3，总push数32n-16。

Exercise 5.28: 修改解释器的定义eval-sequence如5.4.2中描述的那样，不再尾递归。然后重新跑前面两个实验，验证现在两个版本所需的空间都是线性增长。

迭代版：最大深度3n+14，总push数37n+33

递归版：最大深度8n+3，总push数34n-16

Exercise 5.29: 监控树递归Fibonacci计算的栈操作

```scheme
(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))
```

给出栈最大深度关于n的公式。（线性）5n+3

给出总push数的公式。（指数）。给出S(n)关于S(n-1)和S(n-2)加上常数k。然后展示S(n)可以表示为a Fib(n+1) + b，给出a和b的值

```
 1  2  3   4   5   6   7
 16 72 128 240 408 688 1136
d   56 56  112 168 280 
v1  1  2   3   5   8   13
```

S(n)=S(n-1)+S(n-2)+40

S(n)=56 Fib(n+1) - 40

Exercise 5.30: 我们的解释器现在只处理两个错误—未知表达式类型和未知过程类型。其他错误将终止repl循环。

要使一个实际的错误系统工作是个大工程，但是为了了解这里涉及什么是值得努力的。

1. 发生在评估过程中的操作，例如访问一个没有绑定的变量，可以通过修改查询操作来捕捉。使其返回一个不同的条件码，它不能是任何用户变量的可能值。解释器可以测试这个条件码，然后触发signal-error。找出解释器中所有需要这样修改的地方。这要很多工作。
2. 更难的是处理当应用基本过程时发生的错误。对于一个专业的高质量系统，每个基本应用都会被检查安全性，作为原语的一部分。例如，每个car的对用可以先检查参数是一个pair。如果参数不是一个pair，应用将返回一个不同的条件码给解释器。我们可以在寄存器模拟器中安排这个，通过使每个原始程序检查适用性，并在失败时返回一个适当的区分条件代码。然后每个解释器中的primitive-apply可以检查条件码。这是个大项目。

## 5.5 Compilation

解释和编译。解释器将源语言的原始过程实现为一个用机器语言写的子程序的库。解释器遍历源程序，它通过从库中调用适当的原始子程序来模拟源程序想要的行为。编译器，直接将源程序翻译成用机器语言写的一个等效的程序。本节要实现的编译器，将Scheme程序翻译成使用显式控制解释器机器的数据路径执行的指令序列。

编译器提供了更快地程序执行效率，而解释器为交互式程序开发和调试提供了一个更强大的环境，因为正在执行的源程序在运行时可以被检查和修改。现代的程序开发环境追求一个混合的策略。

**An overview of the compiler**

遍历表达式的方式跟解释器一样，只不过最后我们不是执行指令而是积累到一个序列中。有了编译器，表达式只需要被分析一次，就像我们在4.1.7中分析解释器中实现的优化一样。编译码可以有其他提升效率的机会。解释器运行时，它跟随的进程必须要能应用于语言中的任何表达式。而一个给定的编译代码段是为了执行一些特定的表达式。这可以造成很大的差别，例如在保存寄存器的栈使用上，当解释器评估一个表达式，它必须准备好应对任何突发事件，在评估子表达式前解释器保存所有后面会用到的寄存器。而编译器可以利用它所处理的特定表达式的结构来生成代码，以避免不必要的堆栈操作。

主要区别就是解释器是实时的执行，而编译器会全部完整地编译一次，因此可以利用更多的上下文。除了寄存器和栈优化，还可以优化环境的访问。

### Structure fo the compilter

Exercise 5.31: 在解释一个过程应用时，显式控制解释器总是会保存和恢复env寄存器，除了最后一个参数。对每个参数评估保存和恢复argl和proc。对于以下组合，哪些save和restore操作是多余的，可以通过编译器的preserving机制消除。

```scheme
(f 'x 'y)    ; 所有操作都多余
((f) 'x 'y)  ; 所有操作都多余，参数不是变量用不到env，且(f)不会扩展环境
(f (g 'x) y) ; 评估y需要env，且(g 'x)会扩展环境，所以需要保存env，proc,argl
(f (g 'x) 'y); env不用保存，只需保存proc/argl
```



Exercise 5.32: operator是符号的话就不需要保存env。1扩展5.4节中的ec解释器将这种识别为特殊的表达式组合类型。2Alassa建议扩展解释器以识别更多的特殊情况，将所有编译器的优化集成进来，这将完全消除编译的优势。你对这个想法怎么看？

```scheme
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
```



不可能将所有编译器的优化全部集成进来，编译的时候具有完整的上下文，解释的时候只有当前的。

### Compiling Expressions

### Compiling Combinations

### Combining Instruction Sequences

### An Example of Compiled Code

Exercise 5.33: 下面这个稍有不同的阶乘过程，编译之后的结果有什么不同，是否有一个程序执行效率会更高一点。

```scheme
(define (factorial-alt n)
  (if (= n 1)
      1
      (* n (factorial-alt (- n 1)))))
```

之前是先评估n，这个是先评估递归的阶乘。原先在评估递归阶乘前需要保存argl，现在在评估递归阶乘前需要保存env，因为会修改env且n还要用到。

Exercise 5.34: 编译迭代版本的阶乘过程。注释结果代码，展示迭代版本和递归版本的本质区别。一个会积累栈空间，另一个则是常数栈空间。

```scheme
(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product)
              (+ counter 1))))
  (iter 1 1))
```

递归版本在跳到递归调用之前，需要设置continue，在执行完之后还要回来。而迭代版本之前恢复上层的continue，执行完之后直接回去上层。

Exercise 5.35: 下面的编译器输出是什么表达式生成的？

```scheme
(define (f x)
  (+ x (g (+ x 2))))
```



Exercise 5.36: 我们的编译器评估组合的参数的顺序是？从右往左。由`construct-arglist`中的参数代码的顺序决定。修改参数的评估顺序如何影响构建参数列表的代码的效率？如果从左往右的话，就必须在运行时对arglist做一次反序，原先是在编译时反序。

Exercise 5.37: 一种理解编译器优化栈使用的preserving机制的方法看看如果不使用它的话会产生什么额外的操作。修改preserving总是生成save和restore，然后编译一些简单的表达式看看有哪些不需要的栈操作。

Exercise 5.38:编译原始函数的调用时不够聪明。如果编译器支持原始函数的open coding那么就不需要生成一般的过程应用代码。我们扩展机器加两个特殊的参数寄存器arg1和arg2，原始的算数运算将从这两个寄存器中取值，结果可能放入val，arg1或arg2。编译器必须要能识别open-coded原始过程的应用，我们将扩展compile过程的分发以识别这些原语。我们将为open-coded原语构造一族代码生成器。

1. open-coded原语，不像特殊形式，都需要评估参数。写一个代码生成器spread-arguments给所有的open-coded代码生成器使用。spread-arguments应该接收一个参数list并编译给的参数到目标连续的参数寄存器上。注意参数也可能包含对open-coded原语的调用，所以在参数评估期间参数寄存器需要保存。

   ```scheme
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
   ```

   

2. 对每个原始过程=,*-+写一个代码生成器，接收一个combination, 一个target和一个linkage描述符，生成代码将参数传进寄存器并执行操作，用给定的target和linkage。你只需要处理两个参数的表达式。使compile分发这些代码生成器。

   ```scheme
   (define (open-code-application? exp)
     (and (application? exp)
          (memq (operator exp) '(+ - * / =))))
   
   ((open-code-application? exp)
            (compile-open-code exp target linkage))
   ```

   

3. 用新的编译器试下factorial。比较生成的代码

   生成的代码短了许多。

4. 扩展加法和乘法的代码生成器使他们能够处理任意数量的参数。多于两个参数的表达式，将编译成一个操作的序列，每次两个输入。

   ```scheme
   (define (open-code-application-arbi? exp)
     (and (application? exp)
          (memq (operator exp) '(+ *))))
   
   ((open-code-application-arbi? exp)
            (compile-open-code-arbi exp target linkage))
   
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
   ```

   

### Lexical Addressing

优化变量查找。编译时记录一个变量环境，追踪变量的位置。

Exercise 5.39: 写一个过程lexical-address-lookup实现新的查找操作。接收两个参数一个词法地址和一个运行时环境，返回指定词法地址处的变量的值。如果变量的值为`*unassigned*`就触发错误。

```scheme
; use list-ref and list-set!
(define (scan addr env fn)
  (let ((frame (frame-number addr))
        (pos (displacement-number addr)))
    (define (scan-frame vals i)
      (if (= i 0)
          (fn vals)
          (scan-frame (rest-values vals) (- i 1))))
    (define (scan-env env i)
      (if (= i 0)
          (scan-frame (frame-values (first-frame env)) pos)
          (scan-env (enclosing-environment env) (- i 1))))
    (scan-env env frame)))

(define (lexical-address-lookup addr env)
  (scan addr env (lambda (vals)
                   (let ((val (first-value vals)))
                     (if (eq? '*unassigned* val)
                         (error "use variable before it's defined")
                         val)))))

(define (lexical-address-set! addr val env)
  (scan addr env (lambda (vals)
                   (set-car! vals val))))

(define (make-lexical-address frame displacement)
  (list frame displacement))

(define (frame-number addr)
  (car addr))

(define (displacement-number addr)
  (cadr addr))

; assume the initial frame already exsits
(define (define-lexical-variable! env var)
  (let* ((f-num (length env))
         (d-num (length (car env)))
         (addr (make-lexical-address f-num d-num)))
    (define-variable! var addr env)))
```



Exercise 5.40: 修改编译器维护一个编译时环境。即给compile和各个代码生成器添加一个编译时环境参数，并在compile-lambda-body中扩展它。

```scheme
; target: the register in which the compiled code
; is to return the value of the expression
; linkage: describes how the code resulting from
; the compilation of the expression should proceed
; when it has finished its execution.
; next|return|label
(define (compile exp target linkage env)
  (cond ((self-evaluating? exp)
         (compile-self-evaluating 
          exp target linkage))
        ((quoted? exp) 
         (compile-quoted exp target linkage))
        ((variable? exp)
         (compile-variable 
          exp target linkage env))
        ((assignment? exp)
         (compile-assignment
          exp target linkage env))
        ((definition? exp)
         (compile-definition
          exp target linkage env))
        ((if? exp)
         (compile-if exp target linkage env))
        ((lambda? exp)
         (compile-lambda exp target linkage env))
        ((begin? exp)
         (compile-sequence 
          (begin-actions exp) target linkage env))
        ((cond? exp) 
         (compile 
          (cond->if exp) target linkage env))
        ((open-code-application-arbi? exp)
         (compile-open-code-arbi exp target linkage env))
        ((open-code-application? exp)
         (compile-open-code exp target linkage env))
        ((application? exp)
         (compile-application 
          exp target linkage env))
        (else
         (error "Unknown expression type: 
                 COMPILE" 
                exp))))

(define (compile-if exp target linkage env)
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
                      'next
                      env))
            (c-code
             (compile (if-consequent exp) 
                      target 
                      consequent-linkage
                      env))
            (a-code
             (compile (if-alternative exp)
                      target
                      linkage
                      env)))
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

(define (compile-sequence seq target linkage env)
  (if (last-exp? seq)
      (compile (first-exp seq) target linkage env)
      (preserving '(env continue)
       (compile (first-exp seq) target 'next env)
       (compile-sequence (rest-exps seq)
                         target
                         linkage env))))

(define (compile-lambda exp target linkage env)
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
        (compile-lambda-body exp proc-entry env))
       after-lambda))))

(define (compile-lambda-body exp proc-entry env)
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
                       'return
                       (extend-compile-env env formals)))))

(define (extend-compile-env env vars)
  (let* ((new-env (cons (make-frame '() '()) env)))
    (define (iter l)
      (if (null? l)
          (void)
          (begin (define-lexical-variable! new-env (car l))
                 (iter (cdr l)))))
    (iter vars)))
```



Exercise 5.41: 写一个步骤find-variable接收一个变量和一个编译时环境作为参数，返回一个变量关于该环境的词法地址。

```scheme
(define the-empty-cenv '())

(define (extend-cenv frame base-cenv)
  (cons frame base-cenv))

(define (cenv-first-frame cenv) (car cenv))

(define (enclosing-cenv cenv) (cdr cenv))

(define (find-variable var cenv)
  (define (env-loop frame-num cenv)
	(define (scan displacement vars)
	  (cond ((null? vars)
			 (env-loop (+ frame-num 1)
					   (enclosing-cenv cenv)))
			((eq? var (car vars))
			 (make-lexical-address frame-num
								   displacement))
			(else
			 (scan (+ 1 displacement)
				   (cdr vars)))))
	(if (eq? cenv the-empty-cenv)
		'not-found
		(scan 0 (cenv-first-frame cenv))))
  (env-loop 0 cenv))
```



Exercise 5.42: 使用find-variable重写compile-variable和compile-assignment以输出词法地址指令。对于find-variable返回not-found的情况，你应该让代码生成器像之前那样用解释器操作来搜索绑定。变量在编译时找不到的唯一情况是在全局环境中，这是运行时环境的一部分，但不是编译时环境的一部分。所以可以直接操作全局环境，而不是env。测试修改的编译器，例如本节开头的嵌套的lambda组合

```scheme
(define (compile-variable
         exp target linkage env)
  (let ((addr (find-variable exp env)))
    (end-with-linkage 
     linkage
     (make-instruction-sequence 
      '(env)
      (list target)
      (if (eq? addr 'not-found)
          `((assign ,target
                    (op lookup-variable-value)
                    (const ,exp)
                    (reg env))) ; get-global-environment
          `((assign ,target
                    (op lexical-address-lookup)
                    (const ,addr)
                    (reg env))))))))

(define (compile-assignment 
         exp target linkage env)
  (let ((var (assignment-variable exp))
        (get-value-code
         (compile (assignment-value exp) 
                  'val
                  'next
                  env)))
    (let ((addr (find-variable var env)))
      (end-with-linkage 
       linkage
       (preserving 
        '(env)
        get-value-code
        (make-instruction-sequence
         '(env val)
         (list target)
         (if (eq? addr 'not-found)
             `((perform (op set-variable-value!)
                        (const ,var)
                        (reg val)
                        (reg env)) ; get-global-environment
               (assign ,target (const ok)))
             `((perform (op lexical-address-set!)
                        (const ,addr)
                        (reg val)
                        (reg env))
               (assign ,target (const ok))))))))))    
```



Exercise 5.43: 4.1.6节中我们认为块结构内部的定义不应该被认为是真正的定义。而应该被解释为定义了一个内部变量，然后用set!初始化为lambda。练习4.16修改了自循环解释器。修改编译器对过程body执行相同的转换。

```scheme
(define (compile-lambda-body exp proc-entry env)
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
     (compile-sequence (scan-out-defines
                        (lambda-body exp))
                       'val
                       'return
                       ))))
```



Exercise 5.44: 本节中我们关注使用编译时环境来生成词法地址。但是编译时环境也可以做其他用途。例如练习5.38中我们通过open-coding promitive过程来提升编译的代码的效率。如果我们重新绑定了这些名字，练习5.38中的机制还是会open-code它为一个原语。例如：

```scheme
(lambda (+ * a b x y)
  (+ (* a x) (* b y)))
```



修改open-coding编译器，使其能够咨询编译时环境，以便为涉及原始程序名称的表达式编译出正确的代码。（代码将能正确工作，只要程序不define或set!这些名字）

```scheme
(define (open-code-application? exp env)
  (and (application? exp)
       (memq (operator exp) '(+ - * / =))
       (eq? 'not-found (find-variable (operator exp) env))))

(define (open-code-application-arbi? exp env)
  (and (application? exp)
       (memq (operator exp) '(+ *))
       (eq? 'not-found (find-variable (operator exp) env))))
```



### Interfacing Compiled Code to the Evaluator

实现一个过程compile-and-go编译Scheme表达式，加载结果目标码进解释器机器，然后让机器在解释器全局环境运行目标码，打印结果，然后进入解释器的驱动循环。我们也会修改解释器使得，解释的表达式可以可以调用解释的过程以及编译的过程。

```scheme
apply-dispatch
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-apply))
  (test (op compound-procedure?) (reg proc))
  (branch (label compound-apply))
  (test (op compiled-procedure?) (reg proc))
  (branch (label compiled-apply))
  (goto (label unknown-procedure-type))

compiled-apply
  (restore continue)
  (assign val
          (op compiled-procedure-entry)
          (reg proc))
  (goto (reg val))
```

为了使我们在开始解释器机器后可以跑编译的代码，我们在解释器机器的开头增加一个branch指令，如果flag置位了就跳到一个新的入口点

```scheme
;; branches if flag is set:
(branch (label external-entry)) 
read-eval-print-loop
  (perform (op initialize-stack))
  …
  
external-entry
  (perform (op initialize-stack))
  (assign env (op get-global-environment))
  (assign continue (label print-result))
  (goto (reg val))
```

为了将编译器生成的目标码转换成解释器注册机的执行指令，我们用assemble过程。

```scheme
(define (compile-and-go expression)
  (let ((instructions
         (assemble 
          (statements
           (compile 
            expression 'val 'return))
          eceval)))
    (set! the-global-environment
          (setup-environment))
    (set-register-contents! 
     eceval 'val instructions)
    (set-register-contents! 
     eceval 'flag true)
    (start eceval)))
```

另外需要修改make-operation-exp支持标签参数，因为

```scheme
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
```



测试，编译版本

```scheme
(compile-and-go
 '(define (factorial n)
    (if (= n 1)
        1
        (* (factorial (- n 1)) n))))

(total-pushes = 0 maximum-depth = 0)
;;; EC-Eval value:
ok

;;; EC-Eval input:
(factorial 5)

(total-pushes = 11 maximum-depth = 8)
;;; EC-Eval value:
120
```

解释版本

```scheme
(factorial 5)

(total-pushes = 108 maximum-depth = 28)
;;; EC-Eval value:
120
```

**解释和编译**

解释器将机器提升到用户程序层，编译器将用户程序降低到机器语言层。

Exercise 5.45: 比较编译代码和解释器使用的栈操作。比较特殊目的机器，解释器和编译器三者

解释器： 最大深度5n+3，总push数32n-16。

编译器： 最大深度2n-2，总push数2n+1

Exercise 5.46: 再对fib过程做类似上一练习的分析。

```scheme
(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))
; 1 2  3  4  5  6
; 5 12 19 33 54 89
; 1 1  2  3  5  8

; 1 2  3  4  5  6
; 3 6  9  15 24 39
; 1 1  2  3  5  8
```

解释器：深度5n+3，push数S(n)=56 Fib(n+1) - 40

编译器：深度2n，push数7fib(n+1)-2

编译器（<）：深度2n-2，push数3fib(n+1)

Exercise 5.47: 本节描述了如何修改显示控制解释器，以使解释的代码可以调用编译的过程。展示如何修改编译器使编译的过程也可以调用解释的过程。这需要修改compile-procedure-call来处理复合过程的情况。确保处理所有跟compile-proc-appl中相同的target和linkage组合。要做实际的过程应用，代码需要调到解释器的compound-apply入口。这个标签无法直接在目标码中引用，所以我们将给解释器机器添加一个叫compapp的寄存器来保存这个入口点，并增加一个指令来初始化它

```scheme
  (assign compapp (label compound-apply))
  ;; branches if flag is set:
  (branch (label external-entry))
read-eval-print-loop …
```

测试你的代码，先定义一个过程f调用过程g。使用compile-and-go编译f的定义并启动解释器。然后在解释器中定义g，尝试调用f。

```scheme
(define (compile-procedure-call
         target linkage)
  (let ((primitive-branch
         (make-label 'primitive-branch))
        (compound-branch
         (make-label 'compound-branch))
        (compiled-branch
         (make-label 'compiled-branch))
        (after-call
         (make-label 'after-call)))
    (let ((compiled-linkage
           (if (eq? linkage 'next)
               after-call
               linkage)))
      (append-instruction-sequences
       (make-instruction-sequence
        '(proc)
        '()
        `((test
           (op primitive-procedure?)
           (reg proc))
          (branch
           (label ,primitive-branch))
          (test (op compound-procedure?)
                (reg proc))
          (branch (label ,compound-branch))))
       (parallel-instruction-sequences
        (parallel-instruction-sequences
         (append-instruction-sequences
          compiled-branch
          (compile-proc-appl
           target
           compiled-linkage))
         (append-instruction-sequences
          compound-branch
          (compound-proc-appl
           target
           compiled-linkage)))
        (append-instruction-sequences
         primitive-branch
         (end-with-linkage
          linkage
          (make-instruction-sequence
           '(proc argl)
           (list target)
           `((assign
              ,target
              (op apply-primitive-procedure)
              (reg proc)
              (reg argl)))))))
       after-call))))

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
```



Exercise5.48: 本节实现的compile-and-go非常糟糕，因为编译器只能被调用一次。扩展编译器-解释器接口，提供一个compile-and-run原语，可以在显式控制解释器中调用。例如：

```scheme
;;; EC-Eval input:
(compile-and-run
 '(define (factorial n)
    (if (= n 1)
        1
        (* (factorial (- n 1)) n))))

;;; EC-Eval value:
ok

;;; EC-Eval input:
(factorial 5)

;;; EC-Eval value:
120
```



```scheme
;primitive-apply
;  (assign val (op apply-primitive-procedure)
;              (reg proc)
;              (reg argl))
;  (restore continue)
;  (goto (reg continue))

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
```



Exercise 5.49: 作为显式控制解释器repl的替代，设计一个寄存器机器执行read-compile-execute-print loop。这在我们的模拟设置中很容易，因为我们可以将compile和assemble安排为寄存器机器的操作。

```scheme
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

read-compile-execute-print-loop
  (perform (op initialize-stack))
  (perform (op prompt-for-input)
           (const ";;; EC-Eval input:"))
  (assign exp (op read))
  (assign val (op compile) (reg exp) (const val) (const return)
           (const ,the-empty-cenv)
           )
  (assign val (op statements) (reg val))
  (assign val (op assemble) (reg val) (const ,eceval))
  (assign env (op get-global-environment))
  (assign continue (label print-result))
  (goto (reg val))
```



Exercise 5.50: 使用编译器编译4.1节的自循环解释器，使用寄存器机器模拟器运行这个程序。（要一次编译多个定义可以使用begin）。结果的解释器会跑的非常慢，但是让所有的细节工作是一个有意义的练习。



```scheme
; #f/#t改成false和true
; 把void去掉，因为#lang sicp中没有
; apply需要在5.4-eceval-operations.rkt中实现，调用eceval
(define ambeval-exp '(begin
;...
(driver-loop)
))
```

amb解释器的primitives到eceval解释器的primitives如何转换？需要解一层套。

5.4-eceval-operations.rkt中添加primitive-procedures，包含所有amb解释器用到的primitives。apply比较特殊，因为primitives实际是两层的tag，需要再去掉一层。map需要实现为解释器的库函数，因为proc可能是primitive也可能是compiled。

编译器需要增加let的支持。



```scheme
(define stdlib
  '(begin
    (define (map proc l)
      (if (null? l)
          '()
          (cons (proc (car l)) (map proc (cdr l)))))
    ))

(define lib-and-exp
  `(begin ,stdlib ,mceval-exp))

(define (start-mceval)
  (compile-and-go lib-and-exp))

; ---OR---
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

(define (start-mceval)
  (start mceval-machine))
```



Exercise 5.51: 用C语言（或其他低层语言）开发一个Scheme的初步实现，通过翻译5.4节中的显式控制解释器。为了跑这个代码你也需要提供合适的内存分配程序以及其他运行时支持。

See [catbro66/scheme](https://github.com/catbro666/scheme).

Exericse 5.52: 作为练习5.51的对应物，修改编译器使其编译Scheme程序为C的指令序列。编译4.1节的自循环解释器生成一个用C写的Scheme解释器。
