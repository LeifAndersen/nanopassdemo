#lang at-exp nanopass

(provide (except-out (all-defined-out)
                     define-language
                     define-pass))

(require (prefix-in nanopass: nanopass/base)
         (for-syntax racket/syntax
                     syntax/parse)
         (for-label racket/base
                    racket/match
                    racket/format
                    nanopass/base))
(define-syntax (define-language stx)
  (syntax-parse stx
    [(define-language name . rest)
     #:with name-code (format-id stx "~a-code" #'name)
     #`(begin
         (define name-code (quote-syntax #,stx))
         (nanopass:define-language name . rest))]))
(define-syntax(define-pass stx)
  (syntax-parse stx
    [(define-pass name . rest)
     #:with name-code (format-id stx "~a-code" #'name)
     #`(begin
         (define name-code (quote-syntax #,stx))
         (nanopass:define-pass name . rest))]))

(define (int64? x)
  (and (integer? x)
       (<= (- (expt 2 63)) x (- (expt 2 63) 1))))

(define-language Lsrc
  (terminals
   (int64 (n))
   (boolean (b))
   (symbol (x)))
  (Expr (e)
        n x b
        (= e1 e2)
        (+ e1 e2)
        (if e1 e2 e3)
        (cond [e1 e2] ... [e3])
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
        (- (cond [e1 e2] ... [e3]))))

(define-language L3
  (extends L2)
  (Expr (e)
        (- (λ (x) e))
        (+ (λ (x) fe)))
  (FreeVars-Expr (fe)
                 (+ (free (x ...) e))))

(define-language L4
  (extends L3)
  (terminals
   (+ (exact-nonnegative-integer (nat))))
  (Var (v)
       (+ x
          (env-get x nat)))
  (Expr (e)
        (- x
           (λ (x) fe)
           (e1 e2))
        (+ v
           (closure (x (x1 x2) e) (v ...))
           (closure-func x)
           (closure-env x)
           (let ([x e])
             e*)
           (e1 e2 e3)))
  (FreeVars-Expr (fe)
                 (- (free (x ...) e))))

(define-language L5
  (extends L4)
  (Program (p)
           (+ (program ([x (x1 x2) e*] ...)
                       e)))
  (Expr (e)
        (+ (make-closure x (v ...)))
        (- (closure (x (x1 x2) e) (v ...))))
  (entry Program))

(define-language L6
  (extends L5)
  (Expr (e)
        (- (+ e1 e2)
           (= e1 e2)
           (e1 e2 e3)
           (if e1 e2 e3))
        (+ (+ x1 x2)
           (= x1 x2)
           (x1 x2 x3)
           (if x1 x2 x3))))

(define-language L7
  (extends L6)
  (Program (p)
           (- (program ([x (x1 x2) e*] ...)
                       e))
           (+ (program ([x (x1 x2) le*] ...)
                       le)))
  (Expr (e)
        (- (let ([x e])
             e*)))
  (Let-Expr (le)
            (+ e
               (let ([x e])
                 le))))

(define-pass parse : * (e) -> Lsrc ()
  (Expr : * (e) -> Expr ()
        (match e
          [`(= ,(app Expr e1) ,(app Expr e2))
           `(= ,e1 ,e2)]
          [`(+ ,(app Expr e1) ,(app Expr e2))
           `(+ ,e1 ,e2)]
          [`(if ,(app Expr e1) ,(app Expr e2) ,(app Expr e3))
           `(if ,e1 ,e2 ,e3)]
          [`(when ,(app Expr e1) ,(app Expr e2))
           `(when ,e1 ,e2)]
          [`(cond [,(app Expr e1) ,(app Expr e2)] ... [,(app Expr e3)])
           `(cond [,e1 ,e2] ... [,e3])]
          [`(λ (,x) ,(app Expr e1))
           `(λ (,x) ,e1)]
          [`(,(app Expr e1) ,(app Expr e2))
           `(,e1 ,e2)]
          [else e]))
  (Expr e))

(define-pass desugar-when : Lsrc (e) -> L1 ()
  (Expr : Expr (e) -> Expr ()
        [(when ,[e1] ,[e2])
         `(if ,e1 ,e2 #f)]))

(define-pass desugar-cond : L1 (e) -> L2 ()
  (Expr : Expr (e) -> Expr ()
        [(cond [,[e1]])
         e1]
        [(cond [,[e1] ,[e1*]] [,e2 ,e2*]  ...  [,e3])
         `(if ,e1  ,e1*  ,(with-output-language (L1 Expr)
                            (Expr `(cond [,e2 ,e2*] ... [,e3]))))]))

(define-pass delay-if : L2 (e) -> L2 ()
  (Expr : Expr (e) -> Expr ()
        [(if ,[e1] ,[e2] ,[e3])
         (define x2 (gensym 'trash))
         (define x3 (gensym 'trash))
         `((if ,e1 (λ (,x2) ,e2) (λ (,x3) ,e3)) #f)]))

(define-pass identify-free-variables : L2 (e) -> L3 ()
  (Expr : Expr (e) -> Expr ('())
        [,x (values x (list x))]
        [(+ ,[e1 a1] ,[e2 a2])
         (values `(+ ,e1 ,e2)
                 (set-union a1 a2))]
        [(= ,[e1 a1] ,[e2 a2])
         (values `(= ,e1 ,e2)
                 (set-union a1 a2))]
        [(if ,[e1 a1] ,[e2 a2] ,[e3 a3])
         (values `(if ,e1 ,e2, e3)
                 (set-union a1 a2 a3))]
        [(λ (,x) ,[e1 a1])
         (define a* (set-remove a1 x))
         (values `(λ (,x) (free (,a* ...) ,e1))
                 a*)]
        [(,[e1 a1] ,[e2 a2])
         (values `(,e1 ,e2)
                 (set-union a1 a2))])
  (let-values ([(res _) (Expr e)])
    res))

(define-pass make-closures : L3 (e) -> L4 ()
  (Expr : Expr (e [env #f] [fv '()]) -> Expr ()
        [(,[e1] ,[e2])
         (define clo-name (gensym 'clo))
         `(let ([,clo-name ,e1])
            ((closure-func ,clo-name) ,e2 (closure-env ,clo-name)))]
        [,x
         (if (dict-has-key? fv x)
             `(env-get ,env ,(dict-ref fv x))
             x)]
        [(λ (,x) (free (,x* ...) ,e))
         (define lambda-name (gensym 'func))
         (define env-name (gensym 'env))
         (define e*
           (Expr e env-name
                 (for/list ([i (in-list x*)]
                            [j (in-range (length x*))])
                   (cons i j))))
         `(closure (,lambda-name (,x ,env-name) ,e*) (,(for/list ([i (in-list x*)])
                                                         (Expr i env fv)) ...))]))

(define-pass raise-closures : L4 (e) -> L5 ()
  (definitions
    (define lamb-name '())
    (define lamb-arg  '())
    (define lamb-env  '())
    (define lamb-body '()))
  (Expr : Expr (e) -> Expr ()
        [(closure (,x1 (,x2 ,x3) ,[e]) (,[v*] ...))
         (set! lamb-name (cons x1 lamb-name))
         (set! lamb-arg (cons x2 lamb-arg))
         (set! lamb-env (cons x3 lamb-env))
         (set! lamb-body (cons e lamb-body))
         `(make-closure ,x1 (,v* ...))])
  (let ([e* (Expr e)])
    `(program ([,lamb-name (,lamb-arg ,lamb-env) ,lamb-body] ...)
              ,e*)))

(define-pass simplify-calls : L5 (e) -> L6 ()
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
         (define x1 (gensym 'plus))
         (define x2 (gensym 'plus))
         `(let ([,x1 ,e1])
            (let ([,x2 ,e2])
                (+ ,x1 ,x2)))]
        [(= ,[e1] ,[e2])
         (define x1 (gensym 'eq))
         (define x2 (gensym 'eq))
         `(let ([,x1 ,e1])
            (let ([,x2 ,e2])
              (= ,x1 ,x2)))]
        [(if ,[e1] ,[e2] ,[e3])
         (define x1 (gensym 'if))
         (define x2 (gensym 'if))
         (define x3 (gensym 'if))
         `(let ([,x1 ,e1])
            (let ([,x2 ,e2])
              (let ([,x3 ,e3])
                (if ,x1 ,x2 ,x3))))]))

(define-pass raise-lets : L6 (e) -> L7 ()
  (Expr : Expr (e) -> Expr ())
  (Let-Expr : Expr (e [var #f] [next-expr #f]) -> Let-Expr ()
            [(let ([,x ,e])
               ,e*)
             (Let-Expr e x (Let-Expr e* var next-expr))]
            [else
             (if var
                 `(let ([,var ,(Expr e)])
                    ,next-expr)
                 (Expr e))])
  (Program : Program (p) -> Program ()
           [(program ([,x (,x1 ,x2) ,[Let-Expr : e #f #f -> e]] ...)
                     ,[Let-Expr : e* #f #f -> e*])
            `(program ([,x (,x1 ,x2) ,e] ...)
                      ,e*)]))

(define runtime
  @~a{#include <stdio.h>
 #include <stdarg.h>
 #include <stdlib.h>
 #include <inttypes.h>
 
 struct Int;
 struct Bool;
 struct Closure;
 union Racket_Object;
 
 typedef union Racket_Object (*Lambda)();
 enum Tag {INT, BOOL, CLOSURE};
 
 typedef struct Int {
  enum Tag t;
  int64_t v;
  } Int;
  
 typedef struct Bool {
  enum Tag t;
  int64_t v;
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
   
 Racket_Object __make_int(int64_t i) {
  Racket_Object o;
  o.t = INT;
  o.i.v = i;
  return o;
 }
 
 Racket_Object __make_bool(int64_t b) {
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
 
 Racket_Object __env_get(Racket_Object *env, unsigned int id) {
  return env[id];
 }
 
 Racket_Object  __prim_plus(Racket_Object a, Racket_Object b) {
  return __make_int(a.i.v + b.i.v);
 }
 
 Racket_Object __prim_equal(Racket_Object a, Racket_Object b) {
  return __make_bool(a.i.v == b.i.v);
 }
 
 Racket_Object __prim_if(Racket_Object a, Racket_Object b, Racket_Object c) {
  return a.b.v ? b : c;
 }})

(define-pass generate-c : L7 (e) -> * ()
 (definitions
    (define (c s)
      (list->string
       (cons #\_
             (for/list ([i (in-string (symbol->string s))])
               (cond
                 [(or (char-alphabetic? i)
                      (char-numeric? i))
                  i]
                 [else #\_])))))
    (define (build-func-decl name x1 x2)
      @~a{Racket_Object @c[name](Racket_Object @c[x1], Racket_Object* @c[x2]);})
    (define (build-func name x1 x2 body)
      @~a{Racket_Object @c[name](Racket_Object @c[x1], Racket_Object* @c[x2]) {
  @(Let-Expr body)}}))
  (Program : Program (e) -> * ()
           [(program ([,x (,x1 ,x2) ,le*] ...)
                     ,le)
            @~a{@runtime
             @(apply ~a (for/list ([x (in-list x)]
                                   [x1 (in-list x1)]
                                   [x2 (in-list x2)])
                          (build-func-decl x x1 x2)))
             @(apply ~a (for/list ([x (in-list x)]
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
               printf("ans = %" PRId64 "\n", ret.i.v);
              } else {
               printf("ans = %s", ret.b.v ? "#t" : "#f");
              }
              return 0;
             }
            }])
  (Expr : Expr (e) -> * ()
        [,n @~a{__make_int(@n)}]
        [,b @~a{__make_bool(@(if b "1" "0"))}]
        [(+ ,x1 ,x2)
         @~a{__prim_plus(@c[x1], @c[x2])}]
        [(= ,x1 ,x2)
         @~a{__prim_equal(@c[x1], @c[x2])}]
        [(if ,x1 ,x2 ,x3)
         @~a{__prim_if(@c[x1],@c[x2],@c[x3])}]
        [(,x1 ,x2 ,x3)
         @~a{@c[x1](@c[x2], @c[x3])}]
        [(closure-env ,x)
         @~a{@c[x].c.e}]
        [(closure-func ,x)
         @~a{@c[x].c.l}]
        [(make-closure ,x (,v ...))
         @~a{__make_closure(@c[x],
                            @(length v)
                            @(apply ~a (for/list ([i (in-list v)])
                                         @~a{, @Var[i]})))}])
  (Var : Var (e) -> * ()
       [,x @c[x]]
       [(env-get ,x ,nat)
        @~a{__env_get(@c[x], @nat)}])
  (Let-Expr : Let-Expr (e) -> * ()
            [(let ([,x (closure-func ,x*)]) ,le)
             @~a{Lambda @c[x] = @c[x*].c.l;
              @Let-Expr[le]}]
            [(let ([,x (closure-env ,x*)]) ,le)
             @~a{Racket_Object* @c[x] = @c[x*].c.e;
              @Let-Expr[le]}]
            [(let ([,x ,e]) ,le)
             @~a{Racket_Object @c[x] = @(Expr e);
              @Let-Expr[le]}]
            [else @~a{return @(Expr e);}]))

(define compiler
  (compose generate-c
           raise-lets
           simplify-calls
           raise-closures
           make-closures
           identify-free-variables
           delay-if
           desugar-cond
           desugar-when
           parse))

(module+ test
  (define x
    (compiler
     #;`(((λ (x)
            (λ (x)
              x)) 1) 2)
     '(((λ (x)
          (λ (y)
            (cond [(= 6 (+ x y)) x]
                  [y]))) 4) 2)))
  
  (displayln x)
  (with-output-to-file "temp.c"
    #:exists 'replace
    (λ () (displayln x))))