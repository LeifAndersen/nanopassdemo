#lang racket/base

(provide (all-defined-out))
(require scriblib/autobib)

(define-cite cite citet generate-bibliography)

(define appelcont
  (make-bib #:title "Compiling With Continuations"
            #:author "Andrew W. Appel"
            #:is-book? #t
            #:date 2007
            #:location (book-location #:edition "Revised Edition"
                                      #:publisher "Cambridge University Press")
            #:url "http://www.amazon.com/Compiling-Continuations-Andrew-W-Appel/dp/052103311X"))
