#lang slideshow

(require mred)

(provide (all-defined-out))

(define fade-bg
  (let ([w (+ (* 2 margin) client-w)]
        [h (+ (* 2 margin) client-h)]
        [trans (make-object brush% "white" 'transparent)]
        [inside (make-object brush% "white" 'solid)])
    (inset (dc (lambda (dc x y)
                 (let ([b (send dc get-brush)]
                       [p (send dc get-pen)]
                       [draw-one
                        (lambda (i)
                          (send dc draw-rectangle
                                (+ x i) (+ y i)
                                (- w (* 2 i)) (- h (* 2 i))))])
                   (send dc set-brush trans)
                   (color-series 
                    dc margin 1
                    (make-object color% "black")
                    (make-object color% "white")
                    draw-one
                    #t #t)
                   (send dc set-brush inside)
                   (draw-one margin)
                   (send dc set-pen p)
                   (send dc set-brush b)))
               w h 0 0)
           (- margin))))

(define outline 
  (let ([sub-para (lambda l
                    (para #:width (* 3/4 (current-para-width)) l))])
    (make-outline
     'one "Part I: Racket Bytecode" 
     #f
     
     'two "Part II: Expanding Bytecode -> Pycket AST"
     #f
     #;(lambda (tag)
       (sub-para "Using" (code make-outline) "and more..."))
     
     'three "Part III: Optimizations & Results"
     #f
     #;(lambda (tag)
       (sub-para "Creating interesting graphics"))
     
     )))
