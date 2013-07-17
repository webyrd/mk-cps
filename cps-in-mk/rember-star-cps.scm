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

(define rember*o-tests
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


      (test-check (string-append "rember*o-0-" name)
        (run* (q) (rember*o 'y '() '()))
        '(_.0))

      (test-check (string-append "rember*o-1-" name)
        (run* (q) (rember*o 'y '() q))
        '(()))

      (test-check (string-append "rember*o-2-" name)
        (run* (q) (rember*o 'y '(a z y x y y z y) q))
        '((a z x z)))

;;; *** TODO add more tests to make code diverge when not using reconnecting trick
      
      
      )))

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
 (rember*o-tests rember*o "miniKanrenizing direct-style rember*, original goal ordering")
  
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
              (rember*o-cps x d (lambda (v)
                                  ;; **** TODO do I need to reconnect the wires here as well?
                                  (k `(,y . ,v))))]))])))

  (define rember*o
    (lambda (x t out)
      (rember*o-cps x t (lambda (v) (== out v)))))
  
  (rember*o-tests rember*o "CPS in miniKanren, with shortcuts")
  
  )


;; CPS in miniKanren
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
  
  (rember*o-tests rember*o "CPS in miniKanren")
  
  )
