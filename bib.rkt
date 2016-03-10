#lang racket/base

(provide (all-defined-out))
(require scriblib/autobib)

(define-cite cite citet generate-bibliography)

(define appelcont
  (make-bib #:title "Compiling With Continuations"
            #:author "Andrew W. Appel"
            #:is-book? #t
            #:date 2007
            #:location (book-location #:edition "revised"
                                      #:publisher "Cambridge University Press")
            #:url "http://www.amazon.com/Compiling-Continuations-Andrew-W-Appel/dp/052103311X"))

(define plai
  (make-bib #:title "Programming Language Application and Interpretation"
            #:author "Shriram Krishnamurthi"
            #:date 2003
            #:is-book? #t
            #:location (book-location #:edition "first")
            #:url "https://cs.brown.edu/~sk/Publications/Books/ProgLangs/2007-04-26/"))

(define lambdalifting
  (make-bib
   #:title "Lambda Lifting: Transforming Programs to Recursive Equations"
   #:author "Thomas Johnson"
   #:date 1985
   #:location (proceedings-location
               "Conference on Functional Programming Languages and Computer Architecture")
   #:url "http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.48.4346"))

(define lexicalscope-link
   "https://en.wikipedia.org/wiki/Scope_%28computer_science%29#Lexical_scoping")