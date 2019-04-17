#lang slideshow

(require "utils.rkt" slideshow/code)

(define orig-assembler (current-slide-assembler))

(define my-assembler (lambda (s v-sep c)
                       (if s
                           (vc-append v-sep (inset (t s) (* 1 gap-size)) c)
                           c)))

(current-slide-assembler my-assembler)

(slide (titlet "Pycket Uses Racket Bytecode!")
       (tt "Caner Derici")
       (blank)(blank)
       (let ([p (t "The Team:")])
         (refocus (vc-append p (hline (pict-width p) 1)) p))
       (colorize (t "Sam Tobin-Hochstadt") "darkred")
       (colorize (t "Jeremy Siek") "darkred")
       (colorize (t "Spenser Bauman") "darkred")
       (colorize (t "Rajan Walia") "darkred"))

(slide #:title "Pycket is a jit by rpython"
       #:name "0.1"
       (bitmap (build-path (current-directory) "pycket-rpython.png"))
       (para "our idea is to replace with Racket bytecode"))

(slide #:title "replacing the expander"
       (bitmap (build-path (current-directory) "pycket-regular.png"))
       'next
       (bitmap (build-path (current-directory) "pycket-bytecode.png")))


(current-slide-assembler orig-assembler)
(outline 'one)
(current-slide-assembler my-assembler)

(define (bound-frame p)
  (frame p #:color "green"))

(slide #:title "Racket Bytecode"
       (scale (code (define a 999) (define b 666) (define (caner (x y) (+ a b x y))) (caner 5 6)) 0.5)
       (hline (* 2 gap-size) gap-size)
       (scale (code
               ((def-values form 0 zo 0) (((toplevel expr 0 form 0 zo 0) 0 0 #f #f)) 999)
               ((def-values form 0 zo 0) (((toplevel expr 0 form 0 zo 0) 0 1 #f #f)) 666)
               (def-values
                 (((toplevel expr 0 form 0 zo 0) 0 2 #f #f))
                 (lam
                  '#(caner <path:/home/caner/fun/pycket/simple.rkt> 6 0 46 33 #f)
                  '(preserves-marks single-result)
                  2
                  '(val val)
                  #f
                  '#(0)
                  '(val/ref)
                  (set 1 0)
                  12
                  ((application expr 0 form 0 zo 0)
                   ((primval expr 0 form 0 zo 0) 248)
                   (((toplevel expr 0 form 0 zo 0) 4 1 #f #f)
                    ((toplevel expr 0 form 0 zo 0) 4 0 #f #f)
                    ((localref expr 0 form 0 zo 0) #f 5 #f #f #f)
                    ((localref expr 0 form 0 zo 0) #f 6 #f #f #f)))))
               ((apply-values expr 0 form 0 zo 0)
                ((toplevel expr 0 form 0 zo 0) 0 3 #t #t)
                ((application expr 0 form 0 zo 0) ((toplevel expr 0 form 0 zo 0) 2 2 #f #f) (5 6)))) 0.5)
       )

(slide #:title "Racket Bytecode"
       (cb-superimpose
        (ct-superimpose
         (scale titleless-page 0.9)
         (vc-append 
          (scale (code (λ (x) (begin
                                x
                                (let ([y x])
                                  (begin y x))))) 0.7)
          (hline (* 2 gap-size) (* 4 gap-size))
          (scale (code
                  (seq (localref 0) 'x
                       (let-one
                        (localref 0) 'x
                        (seq (localref 0) 'y
                             (localref 1)))) 'x) 0.7)))
        (scale (t "[Klein, C., Flatt, M., & Findler, R. B. (2013)]") 0.8)))

(slide #:title "Racket Bytecode"
       (cb-superimpose
        (ct-superimpose
         (scale titleless-page 0.9)
         (vc-append 
          (scale (code (λ (x y)
                         (let ([z x])
                           (begin (set! z y) z)))) 0.7)
          (hline (* 2 gap-size) (* 3 gap-size))
          (scale (code
                  (let-void 1
                    (install-value 0 (localref 1)
                      (boxenv 0
                        (install-value-box 0 (localref 2)
                          (localref 0 #:boxes true))))))
                 0.7)
          (hline (* 2 gap-size) (* 3 gap-size))
          (scale (bitmap (build-path (current-directory) "stack1.png")) 0.7)))
        (scale (t "[Klein, C., Flatt, M., & Findler, R. B. (2013)]") 0.8)))

        
(outline 'two)

(slide #:title "Bytecode -> Pycket AST"
       (scale (code (define a 999) (define caner (λ (x y) (+ a x y)))) 0.7)
       'next
       (hline (* 2 gap-size) gap-size)
       (hc-append (* 4 gap-size)
                  (vc-append (* 2 gap-size)
                             (scale (code
                                     (def-values caner
                                       (lam 2
                                         (application
                                          (primval +)
                                          (toplevel 0)
                                          (localref 4)
                                          (localref 3))))) 0.6)
                             (tt "Bytecode"))
                  (vc-append (* 2 gap-size)
                             (scale (code
                                     (define-values-names a 999)
                                     (define-values-names b 666)
                                     (define-values-names caner
                                       (lambda (lexical lam.val.1 lam.val.2)
                                         (operator +
                                           (operands
                                            (a b lam.val.1 lam.val.2)))))) 0.6)
                             (tt "Pycket"))))

(slide #:title "Bytecode -> Pycket AST"
       (scale (code (define a 999) (define caner (λ (x y) (+ a x y)))) 0.7)
       (hline (* 2 gap-size) gap-size)
       (hc-append (* 2 gap-size)
                  (vc-append (* 2 gap-size)
                             (scale (code
                                     (def-values caner
                                       (lam 2
                                         (application
                                          (primval 248)
                                          (toplevel 0)
                                          (localref 4)
                                          (localref 3))))) 0.6)
                             (tt "Bytecode"))
                  (bitmap (build-path (current-directory) "stack2.png"))
                  (vc-append (* 2 gap-size)
                             (scale (code
                                     (define-values-names a 999)
                                     (define-values-names b 666)
                                     (define-values-names caner
                                       (lambda (lexical lam.val.1 lam.val.2)
                                         (operator +
                                           (operands
                                            (a b lam.val.1 lam.val.2)))))) 0.6)
                             (tt "Pycket"))))

(slide #:title "Transform Challenge 1"
       (vc-append (* 2 gap-size)
                  (scale (code (let-void 2
                                 (install-value 0 (lam0 ...)
                                   (install-value 1 (lam1 ...)
                                     body)))) 0.7)
                   (tt "Bytecode")))

(slide #:title "Transform Challenge 1"
       (vc-append (* 2 gap-size)
                  (vc-append (* 2 gap-size)
                             (scale (code (let-void 2
                                            (install-value 0 (lam0 ...)
                                              (install-value 1 (lam1 ...)
                                                body)))) 0.6)
                             (tt "Bytecode"))
                  (vc-append (* 2 gap-size)
                             (scale (code (let ([inst.val.1 (lambda0 ...)])
                                            (let ([inst.val2 (lambda1 ...)])
                                              body))) 0.6)
                             (tt "Pycket"))))

(slide #:title "Transform Challenge 1"
       (vc-append (* 2 gap-size)
                  (vc-append (* 2 gap-size)
                             (scale (code (let-void 2
                                            (install-value 0 (lam0 ... '(localref 1 #:boxes? true) ...)
                                              (install-value 1 (lam1 ...)
                                                body)))) 0.6)
                             (tt "Bytecode"))
                  (vc-append (* 2 gap-size)
                             (scale (code (let ([inst.val.1 (lambda0 ... 'uninitialized ...)])
                                            (let ([inst.val2 (lambda1 ...)])
                                              body))) 0.6)
                             (tt "Pycket"))))

(slide #:title "Transform Challenge 1"
       (vc-append (* 2 gap-size)
                  (vc-append (* 2 gap-size)
                             (scale (code (let-void 2
                                            (install-value 0 (lam0 ... '(localref 1 #:boxes? true) ...)
                                              (install-value 1 (lam1 ...)
                                                body)))) 0.6)
                             (tt "Bytecode"))
                  (vc-append (* 2 gap-size)
                             (scale (code (let ([void1 uninit][void2 uninit])
                                            (let ([inst.val.1 (lambda0 ... void2 ...)])
                                              (set! void1 inst.val1)
                                              (let ([inst.val2 (lambda1 ...)])
                                                (set! void2 inst.val2)
                                                body)))) 0.6)
                             (tt "Pycket"))))

(slide #:title "Transform Challenge 2"
       (para "Not enough to expand only the given source"
             "All the required modules need to be expanded using the bytecode")
       (para "#"(code lang racket) " for instanece has a total of 184 module dependencies"))

(slide (bitmap (build-path (current-directory) "pycket-bytecode.png")))

(outline 'three)

(slide #:title "Racket Bytecode Compiler"
       (bitmap (build-path (current-directory) "compiler.png"))
       (blank)
       (tt "raco make input.rkt")
       (tt "raco expand input.rkt"))



(slide #:title "Optimizations"
       (para "figure"))

(slide #:title "Performance Results"
       (para "figure"))

(slide #:title "Thanks"
       (para "figure"))
