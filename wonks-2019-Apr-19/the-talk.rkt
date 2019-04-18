#lang slideshow

(require (for-syntax syntax/parse)
         (rename-in slideshow (slide orig-slide))
         slideshow/code
         slideshow/base
         pict
         pict/face
         racket/draw
         "utils.rkt")

(set-spotlight-style! #:size 20 #:color (make-object color% 255 0 0 0.6))

(current-slide-assembler pycket-default-assembler)

(define outline
  (let ([sub-para
         (lambda l
           (para #:width (* 3/4 (current-para-width)) l))])
    (make-outline
     'one "Part I: How does meta-tracing work?"
     #f

     'two "Part II: What happens when we increase the meta-ness?"
     #f

     'three "Part III: How to solve these?"
     #f)))

(slide
 #:title "Why self-hosting makes meta-tracing difficult?"
 (scale (t "Caner Derici") 1)
 (scale (t "19 Apr 2019") 1)
 (blank)
 ;; (comment "Girizgah")
 ;; (scale (it "Commitee:") 0.5)
 ;; (scale (it "Dr. Sam Tobin-Hochstadt") 0.5)
 ;; (scale (it "Dr. Jeremy Siek") 0.5)
 ;; (scale (it "Dr. Daniel Leivant") 0.5)
 )

(outline 'one)

(slide
 #:title "Meta-tracing"
 (para #:width 1000
       (it "meta-tracing")
       (it "is")
       (it "cool")))


(outline 'two)

(slide
 #:title "Meta-tracing a self-Hosting interpreter"
 (para #:width 1000
       (it "self-hosting")
       (it "is")
       (it "cool")))

(outline 'three)

(slide
 #:title "Stackful + Generalizing Hints"
 (para #:width 1000
       (it "")
       (it "")
       (it "")))


(current-slide-assembler empty-assembler)

(slide
 #:title "Thanks!"
 (table 2 ; two columns
        (list
         (para #:align 'right (it "https://github.com/pycket/pycket.git"))
         (scale (bitmap (build-path ".." "images" "pycket.png")) 0.3))
         (list* lc-superimpose  ; left-align first column
                cc-superimpose) ; h-center the rest
         cc-superimpose ; v-center all rows
         gap-size  ; separate all columns by gap-size
         gap-size)
 (blank)
 (blank)
 (para #:align 'left (it "References"))
 (item #:bullet (colorize (tt ">") "darkred")
       (para (t "Pycket ref")))
 (item #:bullet (colorize (tt ">") "darkred")
       (para (t "PyPy ref"))))
