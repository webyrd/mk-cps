(load "mk.scm")
(load "pmatch.scm")

(define-syntax test-check
  (syntax-rules ()
    ((_ title tested-expression expected-result)
     (begin
       (printf "Testing ~s\n" title)
       (let* ((expected expected-result)
              (produced tested-expression))
         (or (equal? expected produced)
             (errorf 'test-check
               "Failed: ~a~%Expected: ~a~%Computed: ~a~%"
               'tested-expression expected produced)))))))

(define rember*-tests
  (lambda (rember* name)
    (let ()

      (test-check (string-append "rember*-1-" name)
        (rember* 'y '())
        '())

      (test-check (string-append "rember*-2-" name)
        (rember* 'y '(a z y x y y z y))
        '(a z x z))

      (test-check (string-append "rember*-3-" name)
        (rember* 'y '(a z x z))
        '(a z x z))

      (test-check (string-append "rember*-4-" name)
        (rember* 'y '(y y a z x y z y))
        '(a z x z))

      (test-check (string-append "rember*-5-" name)
        (rember* 'y '(a (z y ((x y) z y) y) () (()) ((x)) z (((y)))))
        '(a (z ((x) z)) () (()) ((x)) z ((()))))

      (test-check (string-append "rember*-6-" name)
        (rember* 'y '((y)))
        '(()))      

      (test-check (string-append "rember*-7-" name)
        (rember* 'y '(x (w y) z))
        '(x (w) z))

      (test-check (string-append "rember*-8-" name)
        (rember* 'y '(() (())))
        '(() (())))

      (test-check (string-append "rember*-9-" name)
        (rember* 'y '(()))
        '(()))

      )))

(define rember*o-simple-tests
  (lambda (rember*o name)
    (let ()

      (test-check (string-append "rember*o-0-" name)
        (run 1 (q) (rember*o 'y '() '()))
        '(_.0))
      
      (test-check (string-append "rember*o-1-" name)
        (run 1 (q) (rember*o 'y '() q))
        '(()))

      (test-check (string-append "rember*o-2-" name)
        (run 1 (q) (rember*o 'y '(a z y x y y z y) q))
        '((a z x z)))

      (test-check (string-append "rember*o-3-" name)
        (run 1 (q) (rember*o 'y '(a z x z) q))
        '((a z x z)))

      (test-check (string-append "rember*o-4-" name)
        (run 1 (q) (rember*o 'y '(y y a z x y z y) q))
        '((a z x z)))

      (test-check (string-append "rember*o-9-" name)
        (run 1 (q) (rember*o 'y '(()) q))
        '((())))

      (test-check (string-append "rember*o-8-" name)
        (run 1 (q) (rember*o 'y '(() (())) q))
        '((() (()))))
      
      (test-check (string-append "rember*o-7-" name)
        (run 1 (q) (rember*o 'y '(x (w y) z) q))
        '((x (w) z)))
      
      (test-check (string-append "rember*o-6-" name)
        (run 1 (q) (rember*o 'y '((y)) q))
        '((())))
      
      (test-check (string-append "rember*o-5-" name)
        (run 1 (q) (rember*o 'y '(a (z y ((x y) z y) y) () (()) ((x)) z (((y)))) q))
        '((a (z ((x) z)) () (()) ((x)) z ((())))))

      (test-check (string-append "rember*o-10-" name)
        (run 1 (q) (rember*o q '(a z y x y y z y) '(a z x z)))
        '(y))

      (test-check (string-append "rember*o-11-" name)
        (run 1 (q) (rember*o 'y '(a z y x y y z y) `(a z x z . ,q)))
        '(()))

      (test-check (string-append "rember*o-13-" name)
        (run 1 (q) (rember*o 'y q '()))
        '(()))

      (test-check (string-append "rember*o-13b-" name)
        (run 2 (q) (rember*o 'y q '()))
        '(() (y)))

      (test-check (string-append "rember*o-13c-" name)
        (run 1 (q) (rember*o 'y q '(a)))
        '((a)))

      (test-check (string-append "rember*o-13d-" name)
        (run 2 (q) (rember*o 'y q '(a)))
        '((a) (a y)))

      (test-check (string-append "rember*o-13e-" name)
        (run 1 (q) (rember*o 'y q '((a))))
        '(((a))))

;;;      

      (test-check (string-append "rember*o-0b-" name)
        (run* (q) (rember*o 'y '() '()))
        '(_.0))

      (test-check (string-append "rember*o-1b-" name)
        (run* (q) (rember*o 'y '() q))
        '(()))

      (test-check (string-append "rember*o-2b-" name)
        (run* (q) (rember*o 'y '(a z y x y y z y) q))
        '((a z x z)))

      (test-check (string-append "rember*o-3b-" name)
        (run* (q) (rember*o 'y '(a z x z) q))
        '((a z x z)))

      (test-check (string-append "rember*o-4b-" name)
        (run* (q) (rember*o 'y '(y y a z x y z y) q))
        '((a z x z)))

      (test-check (string-append "rember*o-9b-" name)
        (run* (q) (rember*o 'y '(()) q))
        '((())))

      (test-check (string-append "rember*o-8b-" name)
        (run* (q) (rember*o 'y '(() (())) q))
        '((() (()))))
      
      (test-check (string-append "rember*o-7b-" name)
        (run* (q) (rember*o 'y '(x (w y) z) q))
        '((x (w) z)))
      
      (test-check (string-append "rember*o-6b-" name)
        (run* (q) (rember*o 'y '((y)) q))
        '((())))
      
      (test-check (string-append "rember*o-5b-" name)
        (run* (q) (rember*o 'y '(a (z y ((x y) z y) y) () (()) ((x)) z (((y)))) q))
        '((a (z ((x) z)) () (()) ((x)) z ((())))))

      (test-check (string-append "rember*o-10b-" name)
        (run* (q) (rember*o q '(a z y x y y z y) '(a z x z)))
        '(y))      

      (test-check (string-append "rember*o-11b-" name)
        (run* (q) (rember*o 'y '(a z y x y y z y) `(a z x z . ,q)))
        '(()))  

      (test-check (string-append "rember*o-12b-" name)
        (run* (q) (rember*o 'y '(a z ((y) x) y y z y) `(a z (() x) z . ,q)))
        '(()))

;;; ordering of answers can change, based on goal ordering      
      ;; (test-check (string-append "rember*o-13f-" name)
      ;;   (run 10 (q) (rember*o 'y q '((a))))
      ;;   '(((a))
      ;;     ((a) y)
      ;;     ((a) y y)
      ;;     (y (a))
      ;;     ((a) y y y)
      ;;     ((a) y y y y)
      ;;     ((a y))
      ;;     (y (a) y)
      ;;     ((a) y y y y y)
      ;;     ((a) y y y y y y)))      
      
      )))

(define rember*o-divergent-tests
  (lambda (rember*o name)
    (let ()

      (test-check (string-append "rember*o-tricky-1-" name)        
        (run* (q)
          (fresh (ra rb)
            (rember*o 'y `(x . ,ra) `(z . ,rb))))
        '())
      
      (test-check (string-append "rember*o-tricky-2-" name)
        (run* (q)
          (fresh (ra rb)
            (rember*o 'y `((x) . ,ra) `((z) . ,rb))))
        '())
      
      )))

(define rember*o-long-tests
  (lambda (rember*o name)
    (let ()

      (test-check (string-append "rember*o-15-" name)
        (run 1 (q) (rember*o 'y q '(a z z)))
        '((a z z)))

      (test-check (string-append "rember*o-16-" name)
        (run 1 (q) (rember*o 'y q '(a z () z)))
        '((a z () z)))      

      (test-check (string-append "rember*o-17-" name)
        (run 1 (q) (rember*o 'y q '(a z (()) z)))
        '((a z (()) z)))

      (test-check (string-append "rember*o-18-" name)
        (run 1 (q) (rember*o 'y q '(a z (() x))))
        '((a z (() x))))

      (test-check (string-append "rember*o-14-" name)
        (run 1 (q) (rember*o 'y q '(a z (() x) z)))
        '((a z (() x) z)))
      
;;; seems like a potentially unfair test, since it may take less or
;;; more time depending on goal ordering.
      (test-check (string-append "rember*o-14b-" name)
        (run 10 (q) (rember*o 'y q '(a z (() x) z)))
        '((a z (() x) z)
          (a z (() x) z y)
          (a z (() x) z y y)
          (a z (() x) y z)
          (a z y (() x) z)
          (a z (() x) z y y y)
          (a z (() x) z y y y y)
          (a z (y () x) z)
          (a z y (() x) z y)
          (a z (() x) z y y y y y)))
      
      )))

(define rember*o-tests
  (lambda (rember*o name)

    (rember*o-simple-tests rember*o (string-append name "-simple"))

    (rember*o-divergent-tests rember*o (string-append name "-divergent"))
    
    (rember*o-long-tests rember*o (string-append name "-long"))
    
    ))

;; direct style Scheme
(let ()

  (define rember*
    (lambda (x t)
      (cond
        [(null? t) '()]
        [(pair? (car t))
         (cons (rember* x (car t)) (rember* x (cdr t)))]
        [(eq? (car t) x) (rember* x (cdr t))]
        [else (cons (car t) (rember* x (cdr t)))])))  

  (rember*-tests rember* "direct-style")
  
  )

;;; CPS rember* in Scheme, car first
(let ()

  (define rember*-cps
    (lambda (x t k)
      (cond
        [(null? t) (k '())]
        [(pair? (car t))
         (rember*-cps x (car t)
                      (lambda (v)
                        (rember*-cps x (cdr t)
                                     (lambda (v^)
                                       (k (cons v v^))))))]
        [(eq? (car t) x)
         (rember*-cps x (cdr t) k)]
        [else
         (rember*-cps x (cdr t)
                      (lambda (v)
                        (k (cons (car t) v))))])))  

  (define rember*
    (lambda (x t)
      (rember*-cps x t (lambda (v) v))))
  
  (rember*-tests rember* "Scheme-CPS-car-first")
  
  )

;;; CPS rember* in Scheme, cdr first
(let ()

  (define rember*-cps
    (lambda (x t k)
      (cond
        [(null? t) (k '())]
        [(pair? (car t))
         (rember*-cps x (cdr t)
                      (lambda (v^)
                        (rember*-cps x (car t)
                                     (lambda (v)
                                       (k (cons v v^))))))]
        [(eq? (car t) x)
         (rember*-cps x (cdr t) k)]
        [else
         (rember*-cps x (cdr t)
                      (lambda (v)
                        (k (cons (car t) v))))])))  

  (define rember*
    (lambda (x t)
      (rember*-cps x t (lambda (v) v))))
  
  (rember*-tests rember* "Scheme-CPS-cdr-first")
  
  )

;; miniKanrenizing direct-style rember*
(let ()

  (define rember*o
    (lambda (x t out)
      (conde
        [(== '() t) (== '() out)]
        [(fresh (a d aa da res-a res-d)
           (== `(,a . ,d) t)
           (== `(,res-a . ,res-d) out)           
           (conde
             [(== `(,aa . ,da) a)]
             [(== '() a)])
           (rember*o x a res-a)
           (rember*o x d res-d))]
        [(fresh (y d)
           (== `(,y . ,d) t)
           (symbolo y)
           (conde
             [(== x y) (rember*o x d out)]
             [(=/= x y)
              (fresh (res)
                (== `(,y . ,res) out)
                (rember*o x d res))]))])))
  
 (rember*o-tests rember*o "miniKanrenizing direct-style rember*")
  
  )

;; CPS in miniKanren w/shortcuts
(let ()

  (define rember*o-cps
    (lambda (x t k)
      (conde
        [(== '() t) (k '())]
        [(fresh (a d aa da res-a res-d)
           (== `(,a . ,d) t)
           (conde
             [(== `(,aa . ,da) a)]
             [(== '() a)])

           ; trick
           (k `(,res-a . ,res-d))
           
           (rember*o-cps x a (lambda (v)
                               (fresh ()

                                 (== v res-a)
                                 
                                 (rember*o-cps x d (lambda (v^)
                                                     (fresh ()
                                                       
                                                       (== v^ res-d)
                                                       
                                                       (k `(,res-a . ,res-d)))))))))]
        [(fresh (y d)
           (== `(,y . ,d) t)
           (symbolo y)
           (conde
             [(== x y) (rember*o-cps x d k)]
             [(=/= x y)
              (fresh (res)
                ; trick

                (k `(,y . ,res))
                
                (rember*o-cps x d (lambda (v)
                                    (fresh ()

                                      (== v res)
                                    
                                      (k `(,y . ,res))))))]))])))

  (define rember*o
    (lambda (x t out)
      (rember*o-cps x t (lambda (v) (== out v)))))
  
  (rember*o-simple-tests rember*o (string-append "CPS in miniKanren, with shortcuts" "-simple"))

  (rember*o-divergent-tests rember*o (string-append "CPS in miniKanren, with shortcuts" "-divergent"))

  ; long tests take a looong time
;  (rember*o-long-tests rember*o (string-append "CPS in miniKanren, with shortcuts" "-long"))
      
  )


;; CPS in miniKanren, no shortcuts
(let ()

  (define rember*o-cps
    (lambda (x t k)
      (conde
        [(== '() t) (k '())]
        [(fresh (a d aa da)
           (== `(,a . ,d) t)
           (conde
             [(== `(,aa . ,da) a)]
             [(== '() a)])
           (rember*o-cps x a (lambda (v)
                               (rember*o-cps x d (lambda (v^)
                                                   (k `(,v . ,v^)))))))]
        [(fresh (y d)
           (== `(,y . ,d) t)
           (symbolo y)
           (conde
             [(== x y) (rember*o-cps x d k)]
             [(=/= x y)
              (rember*o-cps x d (lambda (v)
                                  (k `(,y . ,v))))]))])))

  (define rember*o
    (lambda (x t out)
      (rember*o-cps x t (lambda (v) (== out v)))))
  

  (rember*o-simple-tests rember*o (string-append "CPS in miniKanren, no shortcuts" "-simple"))

  ; divergent tests diverge!!
;  (rember*o-divergent-tests rember*o (string-append "CPS in miniKanren, no shortcuts" "-divergent"))

  ; long tests take a looong time, and might even diverge
;  (rember*o-long-tests rember*o (string-append "CPS in miniKanren, no shortcuts" "-long"))
  
  )


;; miniKanrenizing direct-style rember*, original goal ordering
(let ()

  (define rember*o
    (lambda (x t out)
      (conde
        [(== '() t) (== '() out)]
        [(fresh (a d aa da res-a res-d)
           (== `(,a . ,d) t)
           (conde
             [(== `(,aa . ,da) a)]
             [(== '() a)])
           (rember*o x a res-a)
           (rember*o x d res-d)
           (== `(,res-a . ,res-d) out))]
        [(fresh (y d)
           (== `(,y . ,d) t)
           (symbolo y)
           (conde
             [(== x y) (rember*o x d out)]
             [(=/= x y)
              (fresh (res)
                (rember*o x d res)
                (== `(,y . ,res) out))]))])))

  (rember*o-simple-tests rember*o (string-append "miniKanrenizing direct-style rember*, original goal ordering" "-simple"))

  ; divergent tests diverge!!
;  (rember*o-divergent-tests rember*o (string-append "miniKanrenizing direct-style rember*, original goal ordering" "-divergent"))

  ; long tests take a looong time, and might even diverge
;  (rember*o-long-tests rember*o (string-append "miniKanrenizing direct-style rember*, original goal ordering" "-long"))
  
  )
