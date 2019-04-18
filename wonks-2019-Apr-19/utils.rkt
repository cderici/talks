#lang slideshow

(require racket/draw)

(provide pycket-default-assembler empty-assembler)

(define empty-assembler
 (lambda (s v-sep c)
   (lt-superimpose
    fade-bg
    (let ([c (colorize c "darkred")])
      (if s
          (vc-append v-sep
                     ;; left-aligns the title:
                     (ghost (scale (titlet s) 2))
                     (titlet s)
                     c)
          c))
      )))

(define pycket-default-assembler
  (lambda (s v-sep c)
    (lt-superimpose
     (lbl-superimpose
      fade-bg
      (scale (bitmap (build-path ".." "images" "pycket.png")) 0.3))
     (let ([c (colorize c "darkred")])
       (if s
           (vc-append v-sep
                     ;; left-aligns the title:
                      #;(ghost (scale (titlet s) 2))
                      (inset (titlet s) 20)
                      c)
           c))
     )))

;; A pict to use behind the main content
(define fade-bg
  (let ([w (+ (* 2 margin) client-w)]
        [h (+ (* 2 margin) client-h)]
        [trans (make-object brush% "white" 'transparent)]
        [inside (make-object brush% (make-object color% 153 187 204) 'solid)])
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
                    (make-object color% (make-object color% 120 153 169))
                    draw-one
                    #t #t)
                   (send dc set-brush inside)
                   (draw-one margin)
                   (send dc set-pen p)
                   (send dc set-brush b)))
               w h 0 0)
           (- margin))))
