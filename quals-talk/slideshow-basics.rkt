#lang slideshow

(require (for-syntax syntax/parse)
         (rename-in slideshow (slide orig-slide)))

(define slide-count 0)

(define-syntax slide
  (syntax-parser
    [(_ arg ...)
     #'(begin
         (set! slide-count (add1 slide-count))
         (orig-slide arg ...))]))

(current-page-number-adjust
 (λ (str n)
   (format "~a / ~a" n slide-count)))

(slide
 #:title "How to Say Hello"
 (t "Hello World!"))

(slide
 #:title "How to Say Hello"
 (t "Hello World!")
 (t "Goodbye Dlrow!"))

(slide
 #:title "How to Say Hello"
 (item "If you want to create an example, you"
      "can always do something with" (bt "Hello World!"))
 (t "In between")
 (item "It's a bit silly, but a follow-up example"
       "could be" (bt "Goodbye Dlrow!")))

(slide
 #:title "How to Say Hello"
 (item "If you want to create an example, you"
      "can always do something with" (bt "Hello World!"))
 'next
 (comment "Talk about things")
 (item "It's a bit silly, but a follow-up example"
       "could be" (bt "Goodbye Dlrow!")))

(slide
 #:title "Example"
 (item "First step")
 'next
 (item "Second step")
 'next
 'alts
 (list (list (item "Tentative third step")
             'next
             (item "This isn't working... back up"))
       (list (item "Third step that works")))
 'next
 (item "Fourth step"))