#lang scribble/manual

@require[(except-in scribble/manual cite)
         scriblib/footnote
         scribble/examples
         scriblib/autobib
         nanopass/base
         "bib.rkt"
         "nanodemo.rkt"
         @for-label[racket/base
                    racket/match
                    racket/format
                    nanopass/base]]

@title{Writing a Compiler with Nanoapss}
@author{Leif Andersen}

@(define nano-eval (make-base-eval))
@examples[#:eval nano-eval
          #:hidden
          (require racket/list
                   nanopass/base
                   "nanodemo.rkt")]

@section{Introduction}

@section{Prologue: Installing Racket and Nanopass}

@section{Defining the Source Language}

As with other Racket based languages, the first line of a
Nanopass program is the @tt{#lang}. For this compiler, we will use:

@codeblock{#lang at-exp nanopass}

The @racket[nanopass] language both provides Nanopass
specific constructs reifies the bindings in the 
@racket[racket] language. Modules can get Nanopass bindings
without anything provided by @racket[racket] by requiring 
@racket[nanopass/base].

The @racket[at-exp] language installs the @"@"-reader, which
makes code generation easier. While @"@"-reader does help
format string, using them is not strictly necessary.

@racket[define-language] creates new languages in Nanopass.
The following code defines the source language, 
@racket[Lsrc], for our compiler:

@racketblock[#,Lsrc-code]

Terminals in Nanopass are defined by predicates. Any value
that satisfies a predicate can be a terminal of that type.
In the above example, @racket[b] is anything that is a 
@racket[boolean?]. The predicates @racket[symbol?] and 
@racket[boolean?] are provided by Nanopass. However, 
@racket[int64] is a user created predicate:

@racketblock[
 (define (int64? x)
   (and (integer? x)
        (<= (- (expt 2 63)) x (- (expt 2 63) 1))))]

@; <============================
In this compiler, @racket[Expr] is a non-terminal. It can be
a combination of terminals and other non-terminals. These
combinations are called production rules. Each production
rule may contain a label, as well as several meta-variables.
For our source language, a @racket[Expr] can be an integer
(@racket[n]), variable (@racket[x]), boolean (@racket[b]),
arithmetic expression (@racket[(= e1 e2)], 
@racket[(+ e1 e2)]), branching expression @;

(@racket[(if e1 e2 e3)], @racket[(when e1 e2)]), function
(@racket[(λ (x) e)]), and function application @;

(@racket[(e1 e2)]).
@; ============================>

Every meta-variable in a production rule must be unique and
have a name matching a terminal or non-terminal. Numbers
can be appended onto the end of a meta-variable without
changing its type. In the expression @racket[(+ e1 e2)]: 
@racket[+] is a tag and @racket[e1] and @racket[e2] are
meta-variables that can contain expressions.

The ellipsis (@racket[...]) in the @racket[cond] production
rule indicates that the pattern before it is a list that can
occur zero or more times. In this case, there can be
multiple pairs of @racket[(,e1 ,e2)] pairs. Only one
ellipsis is allowed for each level of parenthesis ( 
@racket[()]) in a production rule.

For example, the following is a valid pattern:

@racketblock[(let ([x e] ...) e2 ...) (code:comment "A valid pattern")]

However, the following is not a valid pattern:

@racketblock[(let [x e] ... e2 ...) (code:comment "Not a valid pattern")]

Finally, the @racket[entry] clause tells Nanopass which
non-terminal is the top most non-terminal. This is 
@racket[Expr] in this compiler.

@subsection{Building Source Expressions}

Rather than building a parser, for now, we will use 
@racket[with-output-language] to build programs in 
@racket[Lsrc]. @racket[with-output-language] rebinds 
@racket[quasiquote] to create a nanopass record.@note{
 @racket[with-racket-quasiquote] rebinds 
 @racket[quasiquote] back to the normal Racket version.}

@racket[with-output-language] additionally takes a language
and a non-terminal in that language. It uses this
information to determine which records to construct.

@examples[
 #:eval nano-eval
 (with-output-language (Lsrc Expr)
   `5)
 (with-output-language (Lsrc Expr)
   `(+ 4 6))
 (with-output-language (Lsrc Expr)
   `((λ (x) (x x)) (λ (x) (x x))))]

Although @racket[quasiquote] appears to be creating a list,
it is actually creating records with a fixed arity. Thus,
it will error if the expression does not match a pattern in
the language. Finally, only @racket[quasiquote] is rebound,
so other list creating constructs such a @racket[quote] are
unchanged.

@examples[
 #:eval nano-eval
 (eval:error (with-output-language (Lsrc Expr)
               `(+ 5 6 7)))
 (with-output-language (Lsrc Expr)
   '(+ 1 2))]

If a production has an ellipses (@racket[...]), then the
pattern prior to it may occur zero ore more times.

@examples[
 #:eval nano-eval
 (with-output-language (Lsrc Expr)
   `(cond [(= 5 4) 3]
          [(= 2 1) 0]
          [42]))
 (with-output-language (Lsrc Expr)
   `(cond [84]))]

@subsection[#:tag "deflangscale"]{Notes on Scaling Up}

The source language used in this tutorial is clearly a
small one that is designed to make it easy to learn how to
write simple compilers using Nanopass. There are a few
design choices that make it non trivial (although still
possible) to scale up to a production quality language.

First, this source language is missing any form of
mutation. This feature is lacking because handing it
requires the compiler to reason about assigned variables,
and requires the runtime to create mutable cells in a heap
to store these boxes. Doing so additionally necessitates
creating a garbage collector. We have omitted this as
implementing this is straightforward, and adds little
understanding to how to use the framework. Interested
readers can read about how to implement the runtime for
these cells in @hyperlink[plai-link]{ Programming Languages:
 Application and Interpretation}
@cite[plai]. Additionally, techniques used in this tutorial
can be used to detect assigned variables, making it
possible to determine when a mutable cell must be used. 
@note{TODO: Source for faster assigned variable detection.}

Second, in this compiler, primitives such as @racket[=] and
@racket[+] are encoded directly in the language. While this
makes sense for primitives that significantly differ
syntactically, variables with similar syntax will benefit
from having a @racket[prim?] predicate and terminal. This is
because the vast majority of the rules that apply to
variables are identical to each other. We have used
primitives directly in our source language for simplicity.
Separating out primitives, however, is a straightforward task.

Third, our source language only contains 64 bit integers
and booleans as datums. Larger languages will have other
types of datums such as arbitrarily large integers, floats,
strings, lists, and structs. We did not include these in
this compiler because adding them is a straightforward task.
Additionally, adding more data types requires adding more
primitives to handle these data types.

Fourth, all functions take exactly one argument. All of the
transformations shown in this tutorial can be trivially
extended to multi-arity functions. Doing so complicates the
passes shown in this tutorial too quickly. Rather, we first
introduce passes that operate over expressions with a fixed
number of size. Later in this compiler, however, we begin to
show examples of operations on expressions that have an
arbitrary number of arguments. This is because many
intermediate forms require expressions with a variadic
number of arguments.

Finally, this language is lacking common expressions such as
@racket[let] and @racket[letrec]. We omitted these because
they complicate the implementation of the compiler, and add
little value on learning to use Nanopass.

@section{A Simple Pass: Desugaring @racket[when] Forms}

Languages created with @racket[define-language] can be
extensions of other languages. These so called extensions
are indicated with the @racket[extends] keyword.

The following language extends @racket[Lsrc]:

@racketblock[#,L1-code]

The @racket[+] form adds new expressions to non-terminals,
and the @racket[-] form removes production rules. These
forms can also be used inside of a @racket[terminals] form.
In this case, it adds and remove terminals.

Nanopass uses @racket[define-pass] to create new passes.
Unlike languages, passes are functions that transforms
expressions from one language to another. The following pass
converts expressions from @racket[Lsrc] to @racket[L1]:

@racketblock[#,desugar-when-code]

Because @racket[when] is not a production in @racket[L1],
the @racket[desugar-when] pass converts uses of 
@racket[when] into @racket[if]. Unlike @racket[if], 
@racket[when] expressions only contain a condition and a
body. When a @racket[when] condition in our language is 
@racket[#f], the entire expression evaluates to @racket[#f],
without evaluating the body.

@examples[
 #:eval nano-eval
 (with-output-language (Lsrc Expr)
   (desugar-when `(when #f 42)))
 (with-output-language (Lsrc Expr)
   (desugar-when `(λ (x) (when x (λ (y) y)))))]

A pass constructed with @racket[define-pass] is composed of
a signature, a body, and a list of processors. In the above
pass, this signature is:

@racketblock[desugar-when : Lsrc (e) -> L1 ()]

The name of this pass is @racket[desugar-when]. It is
followed by @racket[Lsrc], which indicates that the source
its language, and @racket[L1] indicates the target language
for the pass. The @racket[(e)] is a list of the arguments
the pass takes. In this example, it is only one, which is
the source expression. The empty list @racket[()] is a list
of any extra return values that the pass may give. This pass
only returns an expression in the target language, and is
thus empty.

The remainder of the above pass is a processor, and is
discussed below in @secref{processors}.

@subsection[#:tag "processors"]{Processors and Catamorphisms}

The following is a processor in the @racket[desugare-when]
pass shown above:

@racketblock[
 (Expr : Expr (e) -> Expr ()
       [(when ,[e1] ,[e2])
        `(if ,e1 ,e2 #f)])]

Like passes, processors are functions and begin with a
signature:

@racketblock[Expr : Expr (e) -> Expr ()]

The first @racket[Expr] in this process is the name of this
process. While this name is arbitrary, @racket[Expr] is a
reasonable first name as it transforms expressions. The
second @racket[Expr] indicates that the input for this
processor is an @racket[Expr] in @racket[Lsrc]. This
information is determined by input language of the pass.
Analogously, the last @racket[Expr] indicates that the
output for this processor is an @racket[Expr] in 
@racket[L1]. Finally, like the pass itself, @racket[(e)]
means that this processor takes in one argument, an
expression, and has no additional return values besides the
output expression.

After the signature, a processor is composed of a series of
patterns and templates. Like Racket's @racket[match] form,
the processor selects the first pattern to match the given
expression. If none of the patterns match, Nanopass will
convert the expression to an equivalent one in the target
language and recursively match on all subpatterns in the
expression. This automatic behavior is how Nanopass
compilers reduce the amount of boilerplate.

The above processor contains one pattern:

@racketblock[
 [(when ,[e1] ,[e2])
  `(if ,e1 ,e2 #f)]]

This pattern does the actual transformation of @racket[if]
forms to @racket[when] forms. The first line is the pattern
itself. Unlike match (but like @racket[syntax-parse]),
patterns begin already in a @racket[quasiquote], and must
use @racket[unquote] (@tt{,}) to escape.

Using @racket[unquote] means to match a subexpression, and
bind it to the variable given. In this pattern, however,
these variables are surrounded by square bracket (@tt{[]}).
Bracket are for a feature of Nanopass called catamorphisms.
@note{The term catamorphism comes from category theory.
 While related, catamorphisms in this setting are used
 slightly differently and are more closely related to the
 @hyperlink[iumatch-link]{IU Pattern Matcher}
 or @tt{app} forms in @racket[match].}

These so-called catamorphisms further reduce boilerplate by
handling recursion automatically. The processor determines
input and output non-terminals by using the location in the
pattern and the name of the pattern variable. If the pass
contains a processor that matches this signature, it is used
to transform the variable. Otherwise, a default processor
that translates the expression to a similar one in the
target is used. Finally, the output is bound to the variable
inside of the brackets.

In this example, @racket[e1] is the first variable in a 
@racket[when] clause, which indicates that it is an 
@racket[Expr]. Next, because the variables name begins with
@racket[e], its output is also an @racket[Expr]. The
process named @racket[Expr] matches this signature, and is
used to process the variable. The result is bound to the
variable @racket[e1]. An analogous process happens for 
@racket[e2].

An equivalent pattern that does not use catamorphisms would
be:

@racketblock[
 [(when ,e1 ,e2)
  `(if ,(Expr e1) ,(Expr e2) #f)]]

Here, @racket[e1] and @racket[e2] are expressions in 
@racket[Lsrc], and thus must be passed into the 
@racket[Expr] processor to be converted into @racket[L1]
expression.

Note that the recursion for expressions not listed in a
processor is important. Even if an expression does not need
to be transformed, it may contain subexpressions that do.

@examples[
 #:eval nano-eval
 (with-output-language (Lsrc Expr)
   (desugar-when `(+ 5 (when #t 6))))]

@subsection[#:tag "whenifscale"]{Notes on Scaling Up}

Converting @racket[when] expressions to @racket[if]
expressions serves as a simple example to illustrate the
benefits of using Nanopass to write compilers, while also
showing the basics of how to use it. This particular
transformation, however, is simple enough that it is
generally implemented in a language's macro expander or
during its parsing pass.

@section{Desugaring @racket[cond] and recursive passes}

Sometimes a pass or a processor will recursively call
itself with on new expressions. When this happens, we need
to make sure that the new expression is in the input
language for the pass. By default @racket[define-pass] binds
@racket[quasiquote] to construct expressions in the output
language for the pass. We use @racket[with-output-language]
to rebind @racket[quasiquote] to the input language.

The following is the language is the result of desugaring 
@racket[cond]:

@racketblock[#,L2-code]

Similarly to how @racket[L1] removed @racket[when]
expressions from @racket[L2], this language removes 
@racket[cond] expressions from @racket[L1].

The following pass does the actual desugaring:

@racketblock[#,desugar-cond-code]

This pass is similar to the @racket[desugar-when] pass
before it, with two major differences. First, this pass uses
ellipses (@racket[...]) to match on lists. Second, this pass uses 
@racket[with-output-language] to construct expressions in @racket[L1].

@examples[
 #:eval nano-eval
 (with-output-language (L1 Expr)
    (desugar-cond `(cond [(= 5 6) 7]
                         [(= 8 9) 10]
                         [42])))]

@subsection{Complex patterns and pattern matching}

Ellipses in patterns bind the variables before it to a
list.@note{Pattern variables can occur before an arbitrarily
 deep level of ellipses. For example, if a pattern is two levels of
 ellipses deep, it will be a list of lists. If the pattern
 is three levels of ellipses deep it will be a list of list
 of lists.} In this case, both @racket[e2] and @racket[e2*]
are bound to lists that match the relevant input expression
given to the processor. The pattern causes them to look like
they are zipped together, but they are distinct lists.

The following code uses @racket[nanopass-case] to show that
@racket[e2] and @racket[e2*] are different lists:

@examples[
 #:eval nano-eval
 #:label #f
 (define cond-example
   (with-output-language (L1 Expr)
     `(cond [(= 1 2) 3]
            [(= 4 5) 6]
            [(= 7 8) 9]
            [10])))
 (nanopass-case (L1 Expr) cond-example
                [(cond [,e1 ,e1*] [,e2 ,e2*] ... [,e3])
                 e2*])]

First, we create a @racket[cond] expression and name it 
@racket[cond-example]. We then use @racket[nanopass-case] to
destruct that expression, returning only @racket[e2*].
Notice that the result is a list. Returning @racket[e2]
would have similar results. If, however, we returned 
@racket[e1], @racket[e1*], or @racket[e3], the result would
have been a single expression, rather than a list of
expressions.

Lists can also be used in templates wherever an ellipsis is
allowed.

@examples[
 #:eval nano-eval
 (nanopass-case (L1 Expr) cond-example
                [(cond [,e1 ,e1*] ... [,e3])
                 (with-output-language (L1 Expr)
                   `(cond [,e1* ,e1] ... [,e3]))])]

In this example we reverse the test and body of each of the
expressions in the @racket[cond] expression. While this does
change the semantics of what we would expect from a 
@racket[cond], it is syntactically valid. Additionally, both
@racket[e1] and @racket[e1*] are both lists of expressions.
Even though they appear to be zipped by Nanopass, they are
still distinct lists.

Note that because variables bound with ellipses in the
pattern are just lists, a lot of common idioms in other
pattern languages are not possible. This limitation becomes
particularly obvious when trying to duplicate a single
element to match the length of a list.

@examples[
 #:eval nano-eval
 (eval:error
  (nanopass-case (L1 Expr) cond-example
                 [(cond [,e1 ,e1*] ... [,e3])
                  (with-output-language (L1 Expr)
                   `(cond [,e1 ,e3] ... [5]))]))
 (nanopass-case (L1 Expr) cond-example
                [(cond [,e1 ,e1*] ... [,e3])
                 (with-output-language (L1 Expr)
                   `(cond [,e1 ,(make-list (length e1) e3)] ... [5]))])]

The first example causes an error because @racket[e1] is a
list and @racket[e3] is a single expression. Using 
@racket[make-list], however, to generate a list of 
@racket[e3] expressions of the correct length however
achieves the desired behavior.

@subsection{Recursive templates}

Inside of a processor, @racket[quasiquote] is rebound to
construct an expression in the output language. Normally,
this is the correct behavior, but sometimes we want to
construct an expression in a different language, as in the
pass above.

More specifically, the following is the code that rebinds 
@racket[quasiquote]:

@racketblock[
 [(cond [,[e1] ,[e1*]] [,e2 ,e2*]  ...  [,e3])
  `(if ,e1  ,e1*  ,(with-output-language (L1 Expr)
                     (Expr `(cond [,e2 ,e2*] ... [,e3]))))]]

In this expression, the outer @racket[quasiquote]
constructs an expression in the output language for the
pass. The inner @racket[quasiquote], however, is
constructing an expression in @racket[L1], the input
language for the pass. Finally, the @racket[Expr] is the
name of the processor, and runs itself on the newly created
expression.

@subsection[#:tag "condscale"]{Notes on Scaling Up}

Many desugaring operations performed by the compiler are
fairly simple. As such, it is often easier for programmers
to implement them together in one pass. Merging these passes
can help reduce the boilerplate code surrounding the pass,
while not making the passes themselves any more
complicated.

The following is an alternate version of the desugar pass
that combines both @racket[desugar-when] and 
@racket[desugar-cond]:

@racketblock[
 (define-pass desugar-alt : Lsrc (e) -> L2 ()
   (Expr : Expr (e) -> Expr ()
         [(when ,[e1] ,[e2])
          `(if ,e1 ,e2 #f)]
         [(cond [,[e1]])
          e1]
         [(cond [,[e1] ,[e1*]] [,e2 ,e2*]  ...  [,e3])
          `(if ,e1  ,e1*  ,(with-output-language (L1 Expr)
                             (Expr `(cond [,e2 ,e2*] ... [,e3]))))]))]

This particular pass can be constructed simply by merging
the two passes together. Doing this merging is
straightforward for simple passes such as these.
Unfortunately, this process gets significantly more
complicated as passes themselves become more complicated.
For this reason, many front end passes of a compiler will be
merged like above. This so-called merging works because the
passes themselves are simple, and separating them out

The second approach to desugaring expressions is to do them
in the language's macro system. This makes it easier for
programmers to create there own macros that act as syntactic
sugar.@note{Racket's @racket[when] and @racket[cond] forms
 are desugared in Racket's
 @tech[#:key "macro"
       #:doc '(lib "scribblings/guide/macros.scrbl")]{
  macro system}.}

@section{Delaying @racket[if] Forms}

Unlike function application, the body and alternate body of
@racket[if] expressions should only be evaluated on the
result of the conditional expression. Worse still, @tt{if}
expressions in C are statements rather than expressions.

Ternary operators, however, are expressions. Using ternary
operators directly is still problematic because expressions
in C are not as expressive as ones in our source. For
example, expressions in our source can create closures apply
them to a new variable, and call that closure at a later
time, all in one expression. We will eventually need to
translate some of these operations into statements.

Translating expressions into statements is problematic with
the delayed nature of @racket[if] expressions. Specifically,
we want to first evaluate the condition, and then evaluate
either the body or alternative.@note{If our source was
 effect free, we could evaluate all subexpressions of 
 @racket[if]. This language does, however, have one major
 effect, non-termination. We only want an @racket[if]
 expression to not terminate if the appropriate
 subexpressions do not terminate.}

One way to delay the values of @racket[if] expressions is
to wrap them in function expressions, and apply the whole
expression to a dummy variable. After this transformation
the entire expression can be evaluated eagerly, and the
functions themselves will give the condition body delayed
semantics.

The following pass transforms delayed @racket[if]
expressions to equivalent eager expressions:

@racketblock[#,delay-if-code]

Both the source and target languages for this pass are 
@racket[L2]. It is possible to create a new language that
statically enforces if expressions to store only functions.
Doing so in this case does not prevent further
optimizations, but does help programmers find bugs in their
compilers.

The following is an example of a language that enforces 
@racket[f] expressions to store functions in their body:

@racketblock[
 (define-language L2-alt
   (extends L2)
   (Expr (e)
         (- (λ (x) e)
            (if e1 e2 e3))
         (+ l
            (if e l2 l3)))
   (Lambda (l)
           (+ (λ (x) e))))]

All functions in this language take exactly one argument.
The ones in this pass, however, are thunks that do not
require an argument. To accommodate this, we generate to two
unused variables, and apply the result of the @racket[if]
expression to @racket[#f].

@examples[
 #:eval nano-eval
 (with-output-language (L2 Expr)
   (delay-if `(if #f 42 84)))]

After this transformation, we can treat @racket[if] as an
entirely eager expression.

@subsection[#:tag "ifscale"]{Notes on Scaling up}

TODO:

First: Other means of delaying

Second: Better generation of temporary variables.

Third: Actual thunks

@section{Closure Conversion}

Unlike our source language, C does not have closures. It
does, however, support higher order functions through the
use of function pointers. Unfortunately function pointers do
not store their own environments. Thus, we use closure
conversion@cite[appelcont] as our first step to supporting closures.

Closure conversion is the process of removing all free
variables from functions, and passing them in explicitly in
the form of an environment. The function associated with the
closure will eventually be lifted to the top level, but the
environment remains in the functions plaice. This is
possible because environment mappings are first class values
in C.

For example, if a function has one free variable @racket[y],
the transformation would look like:

@racketblock[
 (lambda (x) .... y ....)
 (code:comment "=>")
 (lambda (x env) .... (env-get env y) ....)]

Unfortunately, this transformation is not enough to create
a closure object. When a lambda occurs we need to also
explicitly create the environment associated with it. Doing
so allows the closure to bind to the variables as the
lambda's definition, rather then whatever they happen to be
at the call site. In other words, we want to preserve 
@hyperlink[lexicalscope-link]{lexical scoping} in our target
language.

Applying this idea to the transformation above gives the
following transformation:

@racketblock[
 (lambda (x env) .... (env-get env y) ....)
 (code:comment "=>")
 (closure (name (x env) ... (env-get env y) ...) (y))]

Here, @racket[closure] is a piece of syntax to describe the
@racket[closure] object. The first argument is the function
expression, which has now been given the name
@racket[name]. The second argument is the variables that
this closure's environment binds, in this case @racket[y].

Now that functions take two arguments, we also need to
modify all of the function call sites to also pass in the
function's environment. This transformation is simple to do
here because closure objects contain their environments.

The following is the transformation that happens at each
function's call site:

@racketblock[
 (f x)
 (code:comment "=>")
 ((closure-func f) x (closure-env f))]

In this example, @racket[closure-func] and 
@racket[closure-env] are special syntax that retrieves the
function and environment objects from a closure. A later
pass transforms @racket[closure-func] to retrieve a function
pointer. For now, however, the closure contains the literal
function itself.

We perform closure conversion in two passes. First, we
create a pass to identify all free variables in each
function. This pass enables us to transform free variables
into environment lookups, as well as determine which
variables should be passed in as part of the closure
environment. The second pass creates the actual explicit
closure structures. These structures still contain the
function and environment, while a later pass will lift them
to the top.

@subsection{Free Variable Identification}

The first step to closure conversion is to identify all of
the free variables in every function. This transformation
allows us to convert free variables into lookups in a later pass.

The following language modifies functions to store free variables:

@racketblock[#,L3-code]

In this language @racket[FreeVars-Expr] is a new
non-terminal that stores an expression and a list of
variables. Function expressions now store an expression with
free variables for their body. The main effect of this
transformation is that functions now have constant time
access to all of their free variables.

@examples[
 #:eval nano-eval
 (with-output-language (L3 Expr)
   `(λ (x) (free (y z) (+ x (+ y z)))))]

The following pass does the actual transformation:

@racketblock[#,identify-free-variables-code]

Unlike the previous passes, this pass uses extra return
values in its processors.

@subsection{Explicit Closure Creation}

@racketblock[#,L4-code]

@racketblock[#,make-closures-code]

@subsection[#:tag "ccscale"]{Notes on Scaling Up}

TODO:

First, intermediate passes.

Second, data structure for linear time

Third, Lambda Lifting@cite[lambdalifting].

@section{Turning Closures to Function Pointers}

@racketblock[#,L5-code]

@racketblock[#,raise-closures-code]

@section{Converting Expressions into Statements}

@racketblock[
 #,L6-code

 #,L7-code]

@racketblock[
 #,simplify-calls-code

 #,raise-lets-code]
 
@section{The Runtime}

@codeblock[#:keep-lang-line? #f]|{#lang at-exp nanopass
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
  
  Racket_Object __prim_if(Racket_Object a,
            Racket_Object b,
            Racket_Object c) {
   return a.b.v ? b : c;
   }})}|

@section{Code Generation}

@codeblock[#:keep-lang-line? #f]|{#lang at-exp nanopass
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
       @~a{Racket_Object @c[name](Racket_Object @c[x1],
            Racket_Object* @c[x2]);})
     (define (build-func name x1 x2 body)
       @~a{Racket_Object @c[name](Racket_Object @c[x1],
            Racket_Object* @c[x2]) {
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
                 printf("ans = " PRId64 "\n", ret.i.v);
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
             [else @~a{return @(Expr e);}]))}|

@section{Parsing}

@racketblock[#,parse-code]

@section{Tying Everything Together}

@racketblock[
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
            parse))]

@section{Further Reading}

@section{Epilogue: Other useful Nanopass Constructs}

@subsection{Pattern Matching Languages}

@subsection{Viewing Expanded Languages}

@examples[
 #:eval nano-eval
 (language->s-expression L7)]

@section{Bonus: Creating a #lang}

@generate-bibliography[]
