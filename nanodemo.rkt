#lang at-exp nanopass

(define-language Lsrc
  (terminals
   (number (n))
   (boolean (b))
   (symbol (x)))
  (Expr (e)
        n
        x
        b
        (+ e1 e2)
        (if e1 e2 e3)
        (when e1 e2)
        (λ (x) e)
        (e1 e2))
  (entry Expr))

(define-language L1
  (extends Lsrc)
  (Expr (e)
        (- (when e1 e2))))

(define-language L2
  (extends L1)
  (Expr (e)
        (- (λ (x) e))
        (+ (λ (x) fe)))
  (FreeVars-Expr (fe)
                 (+ (free (x ...) e))))

(define-language L3
  (extends L2)
  (terminals
   (+ (exact-nonnegative-integer (nat))))
  (Expr (e)
        (- (λ (x) fe)
           (e1 e2))
        (+ (closure (x (x1 x2) e) (x* ...))
           (env-get x1 nat)
           (closure-func x)
           (closure-env x)
           (let ([x e])
             e*)
           (e1 e2 e3)))
  (FreeVars-Expr (fe)
                 (- (free (x ...) e))))

(define-language L4
  (extends L3)
  (Program (p)
           (+ (program ([x (x1 x2) e*] ...)
                       e)))
  (Expr (e)
        (+ (make-closure x (x* ...)))
        (- (closure (x (x1 x2) e) (x* ...))))
  (entry Program))

(define-language L5
  (extends L4)
  (Expr (e)
        (- (+ e1 e2)
           (e1 e2 e3)
           (if e1 e2 e3))
        (+ (+ x1 x2)
           (x1 x2 x3)
           (if x1 e2 e3))))

(define-language L6
  (extends L5)
  (Program (p)
           (- (program ([x (x1 x2) e*] ...)
                       e))
           (+ (program ([x (x1 x2) le*] ...)
                       le)))
  (Expr (e)
        (- (let ([x e])
             e*)
           (if x1 e2 e3))
        (+ (if x1 le2 le3)))
  (Let-Expr (le)
            (+ e
               (let ([x e])
                 le))))

(define-pass desugar-when : Lsrc (e) -> L1 ()
  (Expr : Expr (e) -> Expr ()
        [(when ,e1 ,e2)
         `(if ,e1 ,e2 #f)]))

(define-pass identify-free-variables : L1 (e) -> L2 ()
  (Expr : Expr (e) -> Expr ('())
        [,x (values x '(x))]
        [(+ ,[e1 a1] ,[e2 a2])
         (values `(+ ,e1 ,e2)
                 (set-union a1 a2))]
        [(if ,[e1 a1] ,[e2 a2] ,[e3 a3])
         (values `(if ,e1 ,e2, e3)
                 (set-union e1 e2 e3))]
        [(λ (,x) ,[e1 a1])
         (define a* (set-remove a1 x))
         (values `(λ (,x) (free (,a* ...) ,e1))
                 a*)]
        [(,[e1 a1] ,[e2 a2])
         (values `(,e1 ,e2)
                 (set-union a1 a2))])
  (let-values ([(res _) (Expr e)])
    res))

(define-pass make-closures : L2 (e) -> L3 ()
  (Expr : Expr (e [env #f] [fv '()]) -> Expr ()
        [(,e1 ,e2)
         (define clo-name (gensym 'clo))
         `(let ([,clo-name ,e1])
            ((closure-func ,clo-name) ,e2 (closure-env ,clo-name)))]
        [,x
         (if (dict-has-key? fv x)
             `(env-get ,env ,(dict-ref x))
             x)]
        [(λ (,x) (free (,x* ...) ,e))
         (define lambda-name (gensym 'func))
         (define env-name (gensym 'env))
         (define e*
           (Expr e env-name
                 (for/list ([i (in-list x*)]
                            [j (in-range (length x*))])
                   (list i j))))
         `(closure (,lambda-name (,x ,env-name) ,e*) (,x* ...))]))

(define-pass raise-closures : L3 (e) -> L4 ()
  (definitions
    (define lamb-name '())
    (define lamb-arg  '())
    (define lamb-env  '())
    (define lamb-body '()))
  (Expr : Expr (e) -> Expr ()
        [(closure (,x1 (,x2 ,x3) ,[e]) (,x* ...))
         (set! lamb-name (cons x1 lamb-name))
         (set! lamb-arg (cons x2 lamb-arg))
         (set! lamb-env (cons x3 lamb-env))
         (set! lamb-body (cons e lamb-body))
         `(make-closure ,x1 (,x* ...))])
  (let ([e* (Expr e)])
    `(program ([,lamb-name (,lamb-arg ,lamb-env) ,lamb-body] ...)
              ,e*)))

(define-pass simplify-calls : L4 (e) -> L5 ()
  (Expr : Expr (e) -> Expr ()
        [(,[e1] ,[e2] ,[e3])
         (define x1 (gensym 'app))
         (define x2 (gensym 'app))
         (define x3 (gensym 'app))
         `(let ([,x1 ,e1])
            (let ([,x2 ,e2])
              (let ([,x3 ,e3])
                (,x1 ,x2 ,x3))))]
        [(+ ,[e1] ,[e2])
         (define x1 (gensym '+))
         (define x2 (gensym '+))
         `(let ([,x1 ,e1])
            (let ([,x2 ,e2])
              (+ ,x1 ,x2)))]
        [(if ,[e1] ,[e2] ,[e3])
         (define x1 (gensym 'if))
         `(let ([,x1 ,e1])
            (if ,x1 ,e2 ,e3))]))

(define-pass raise-lets : L5 (e) -> L6 ()
  (Expr : Expr (e) -> Expr ())
  (Let-Expr : Expr (e [var #f] [next-expr #f]) -> Let-Expr ()
        [,n (if var
                `(let ([,var ,n])
                   ,next-expr)
                n)]
        [,b (if var
                `(let ([,var ,b])
                   ,next-expr)
                b)]
        [(let ([,x ,e])
           ,e*)
         (if var
             (Expr e x (Expr e* var next-expr))
             (Expr e x e*))]
        [else
         (if var
             `(let ([,var ,(Expr e)])
                ,next-expr)
             (Expr e))])
  (Program : Program (p) -> Program ()
           [(program ([,x (,x1 ,x2) ,[e #f #f -> e]] ...)
                     ,[e* #f #f -> e*])
            `(program ([,x (,x1 ,x2) ,e] ...)
                      ,e*)]))

(define-pass generate-c : L6 (e) -> * ()
  (definitions
    (define (build-func name x1 x2 body)
      @~a{
 Racket_Object @name(Racket_Object @x1,
                     Racket_Object @x2) {
  @(Let-Expr body)
 }
 }))
  (Program : Program (e) -> * ()
           [(program ([,x (,x1 ,x2) ,le*] ...)
                     ,le)
            @~a{#include <stdio.h>
             #include <stdarg.h>
             #include <stdlib.h>

             struct Int;
             struct Bool;
             struct Closure;
             union Racket_Object;

             typedef union Racket_Object (*Lambda)();

             enum Tag {INT, BOOL, CLOSURE};

             typedef struct Int {
              enum Tag t;
              int v;
             } Int;

             typedef struct Bool {
              enum Tag t;
              unsigned int v;
             } Bool;

             typedef struct Closure {
              enum Tag t;
              Lambda l;
              union Racket_Object * e;
             } Closure;

             typedef union Racket_Object {
              enum Tag t;
              Int i;
              Bool b;
              Closure c;
             } Racket_Object;

             Racket_Object __make_int(int i) {
              Racket_Object o;
              o.t = INT;
              o.i.v = i;
              return o;
             }

             Racket_Object __make_bool(int b) {
              Racket_Object o;
              o.t = BOOL;
              o.b.v = b;
              return o;
             }

             Racket_Object __make_closure(Lambda name, int argc, ...) {
              /* Allocate space for env */
              Racket_Object* env = malloc(sizeof(Racket_Object) * argc);

              /* Fill env */
              va_list lp;
              va_start(lp, argc);
              for(int i = 0; i < argc; i++) {
               env[i] = va_arg(lp, Racket_Object);
              }

              /* Return closure */
              Racket_Object o;
              o.t = CLOSURE;
              o.c.l = name;
              o.c.e = env;
              return o;
             }
             
             Racket_Object  __prim_plus(Racket_Object a, Racket_Object b) {
              return __make_int(a.i.v + b.i.v);
             }

             int __prim_if(Racket_Object a) {
              return a.b.v;
             }

             @(apply ~a
                     (for/list ([x (in-list x)]
                                [x1 (in-list x1)]
                                [x2 (in-list x2)]
                                [le* (in-list le*)])
                       (build-func x x1 x2 le*)))

             Racket_Object __racket_main() {
              @Let-Expr[le]
             }

             int main () {
              Racket_Object ret = __racket_main();
              if(ret.t == CLOSURE) {
               printf("ans = #<procedure>\n");
              } else if(ret.t == INT) {
               printf("ans = %d\n", ret.i.v);
              } else {
               printf("ans = %s", ret.b.v ? "#t" : "#f");
              }
              return 0;
             }
            }])
  (Expr : Expr (e) -> * ()
        [,n @~a{__make_int(@n)}]
        [,x (symbol->string x)]
        [,b @~a{__make_bool(@(if b "1" "0"))}]
        [(+ ,x1 ,x2)
         @~a{__prim_plus(@x1, @x2)}]
        [(if ,x1 ,le2 ,le3)
         @~a{if(__prim_if(@x1)) {
           @Let-Expr[le2]
          } else {
           @Let-Expr[le3]
          }}]
        [(,x1 ,x2 ,x3)
         @~a{@x1(@x2, @x3)}]
        [(closure-env ,x)
         @~a{((Closure*)@x).e}]
        [(closure-func ,x)
         @~a{((Closure*)@x).l}]
        [(env-get ,x ,nat)
         @~a{__env_get(@x, @nat)}]
        [(make-closure ,x (,x* ...))
         @~a{__make_closure(@x,
                            @(length x*)
                            @(apply ~a
                                    (for/list ([i (in-list x*)])
                                      @~a{, @i})))}])
  (Let-Expr : Let-Expr (e) -> * ()
            [(let ([,x ,e]) ,le)
             @~a{
              struct Racket_Object @x = (@(Expr e));
              @Let-Expr[le]}]
            [else
             @~a{return @(Expr e);}]))

(define compile
  (compose generate-c
           raise-lets
           simplify-calls
           raise-closures
           make-closures
           identify-free-variables
           desugar-when))

(define x
  (compile
   (with-output-language (Lsrc Expr)
     `(λ (x) 5))))

(displayln x)
(with-output-to-file "temp.c"
  #:exists 'replace
  (λ () (displayln x)))