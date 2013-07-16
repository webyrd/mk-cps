;;; quines interpreter, CPSed in miniKanren, 1st order continuations

(load "mk.scm")

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

(let ()

  (define apply-ko
    (lambda (k^ v)
      (conde
        [(fresh (val)
           (== `(empty-k ,val) k^)
           (== val v))]

        ;; wanted an example of a non-trivial initial continuation      
        ;; eval once
        [(fresh (val)
           (== `(eval-once-k ,val) k^)
           (eval-expo-cps v '() (empty-k val)))]
;;;
      
        [(fresh (body x env^ k)
           (== `(app-inner-k ,body ,x ,env^ ,k) k^)
           (eval-expo-cps body `((,x . ,v) . ,env^) k))]
        [(fresh (x body env^ rand env k)
           (== `(app-outer-k ,x ,body ,env^ ,rand ,env ,k) k^)         
           (== v `(closure ,x ,body ,env^))                            
           (eval-expo-cps rand env (app-inner-k body x env^ k)))]
        [(fresh (v-d v-a k)
           (== `(proper-listo-inner-k ,v-d ,v-a ,k) k^)
           (== v v-d)                                                              
           (apply-ko k `(,v-a . ,v-d)))]
        [(fresh (v-a d env v-d k)
           (== `(proper-outer-inner-k ,v-a ,d ,env ,v-d ,k) k^)
           (== v v-a) 
           (proper-listo-cps d env (proper-listo-inner-k v-d v-a k)))])))

  (define empty-k
    (lambda (val)
      `(empty-k ,val)))

;;;
  (define eval-once-k
    (lambda (val)
      `(eval-once-k ,val)))
;;;

  (define app-inner-k
    (lambda (body x env^ k)
      `(app-inner-k ,body ,x ,env^ ,k)))

  (define app-outer-k
    (lambda (x body env^ rand env k)
      `(app-outer-k ,x ,body ,env^ ,rand ,env ,k)))

  (define proper-listo-inner-k
    (lambda (v-d v-a k)
      `(proper-listo-inner-k ,v-d ,v-a ,k)))

  (define proper-outer-inner-k
    (lambda (v-a d env v-d k)
      `(proper-outer-inner-k ,v-a ,d ,env ,v-d ,k)))



  (define lookupo-cps
    (lambda (x env k)
      (fresh (y v rest)
        (== `((,y . ,v) . ,rest) env)
        (conde
          ((== y x)
           (apply-ko k v))
          ((=/= y x)
           (lookupo-cps x rest k))))))

;;; presumably this predicate would be CPSed by adding a k argument
;;; and passing it through, without applying k in the base case.
  (define not-in-envo
    (lambda (x env)
      (conde
        ((== '() env))
        ((fresh (y v rest)
           (== `((,y . ,v) . ,rest) env)
           (=/= y x)
           (not-in-envo x rest))))))
  
  (define proper-listo-cps
    (lambda (exp env k)
      (conde
        ((== '() exp)
         (apply-ko k '()))
        ((fresh (a d v-a v-d)
           (== `(,a . ,d) exp)

                                        ; trick (still works!)
           (apply-ko k `(,v-a . ,v-d))
           
           (eval-expo-cps a env (proper-outer-inner-k v-a d env v-d k)))))))

  (define eval-expo-cps
    (lambda (exp env k)
      (conde
        ((fresh (v)
           (== `(quote ,v) exp)
           (absento 'closure v)
           (not-in-envo 'quote env)
           (apply-ko k v)))
        ((fresh (a*)
           (== `(list . ,a*) exp)
           (absento 'closure a*)
           (not-in-envo 'list env)
           (proper-listo-cps a* env k)))
        ((symbolo exp)
         (lookupo-cps exp env k))
        ((fresh (rator rand x body env^ a)
           (== `(,rator ,rand) exp)
           (eval-expo-cps rator env (app-outer-k x body env^ rand env k))))
        ((fresh (x body)
           (== `(lambda (,x) ,body) exp)
           (symbolo x)
           (not-in-envo 'lambda env)
           (apply-ko k `(closure ,x ,body ,env)))))))

  (define eval-expo
    (lambda (exp env val)
      (eval-expo-cps exp env (empty-k val))))




;;;

  (define replace*
    (lambda (al x)
      (cond
        [(null? x) '()]
        [(symbol? x)
         (cond
           [(assq x al) => cdr]
           [else x])]                         
        [(pair? x)
         (cons (replace* al (car x))
               (replace* al (cdr x)))]
        [(boolean? x) x]
        [(string? x) x]
        [else (error 'replace* (format "unknown expression type: ~a\n" x))])))

  (define replace-respect-quote*
    (lambda (al x)
      (cond
        [(null? x) '()]
        [(symbol? x)
         (cond
           [(assq x al) => cdr]
           [else x])]                         
        [(pair? x)
         (if (eq? (car x) 'quote)
             x
             (cons (replace-respect-quote* al (car x))
                   (replace-respect-quote* al (cdr x))))]
        [(boolean? x) x]
        [(string? x) x]
        [else (error 'replace-respect-quote* (format "unknown expression type: ~a\n" x))])))

  (define quinec 
    '((lambda (x)
        (list x (list (quote quote) x)))
      (quote
       (lambda (x)
         (list x (list (quote quote) x))))))

  (test-check "var-1"
    (run* (q)
      (eval-expo 'y '((y . (closure z z ()))) q))
    '((closure z z ())))

  (test-check "lambda-1"
    (run* (q)
      (eval-expo '(lambda (x) x) '() q))
    '((closure x x ())))

  (test-check "lambda-2"
    (run* (q)
      (eval-expo '(lambda (x) (lambda (y) (x y))) '() q))
    '((closure x (lambda (y) (x y)) ())))

  (test-check "quote-1"
    (run* (q)
      (eval-expo '(quote (lambda (x) x)) '() q))
    '((lambda (x) x)))

  (test-check "app-0"
    (run 1 (q)
      (eval-expo '((lambda (x) x) (lambda (y) y)) '() q))
    '((closure y y ())))
    
  (test-check "app-1"
    (run* (q)
      (eval-expo '((lambda (x) x) (lambda (y) y)) '() q))
    '((closure y y ())))
    
  (test-check "extend-3"
    (run* (q) (eval-expo '((lambda (quote) (quote (lambda (x) x))) (lambda (y) y)) '() q))
    '((closure x x ((quote . (closure y y ()))))))

  (test-check "quinec-1"
    (run 1 (q) (eval-expo quinec '() q))
    '(((lambda (x) (list x (list 'quote x))) '(lambda (x) (list x (list 'quote x))))))

  (test-check "quinec-2"
    (run* (q) (eval-expo quinec '() q))
    '(((lambda (x) (list x (list 'quote x))) '(lambda (x) (list x (list 'quote x))))))

  (test-check "quinec-3"
    (run 1 (q) (eval-expo q '() quinec))
    '('((lambda (x) (list x (list 'quote x))) '(lambda (x) (list x (list 'quote x))))))
    
  (test-check "intro-2"
    (equal? (replace* '((_.0 . x)) (car (car (run 1 (q) (eval-expo q '() q))))) quinec)
    #t)


  (test-check "quine-eval-once-k-1"
    ;; why is this so slooooow????    
    (run 1 (q)
      (== quinec q)
      (eval-expo-cps q '() (eval-once-k q)))
    '(((lambda (x) (list x (list 'quote x))) '(lambda (x) (list x (list 'quote x))))))

  (test-check "quine-eval-once-k-2"
    ;; takes several minutes, but does return      
    (run 1 (q) (eval-expo-cps q '() (eval-once-k q)))
    '(('((lambda (_.0) (list 'quote (list _.0 (list 'quote _.0)))) '(lambda (_.0) (list 'quote (list _.0 (list 'quote _.0))))) (=/= ((_.0 closure)) ((_.0 list)) ((_.0 quote))) (sym _.0))))


;;;

  (test-check "apply-ko-1"
    (run 8 (q)
      (fresh (k v)
        (apply-ko k v)
        (== `(,k ,v) q)))
    '(((empty-k _.0) _.0)
      (((eval-once-k _.0) '_.0) (absento (closure _.0)))
      ((eval-once-k ()) (list))
      ((proper-listo-inner-k _.0 _.1 (empty-k (_.1 . _.0))) _.0)
      (((eval-once-k (closure _.0 _.1 ())) (lambda (_.0) _.1))
       (sym _.0))
      (((app-inner-k '_.0 _.1 () (empty-k _.0)) _.2)
       (=/= ((_.1 quote)))
       (absento (closure _.0)))
      (((app-inner-k '_.0 _.1 ((_.2 . _.3)) (empty-k _.0)) _.4)
       (=/= ((_.1 quote)) ((_.2 quote)))
       (absento (closure _.0)))
      (((app-inner-k _.0 _.0 _.1 (empty-k _.2)) _.2) (sym _.0))))

  (test-check "lambda-any-k-1"
    (run 3 (q)
      (eval-expo-cps '(lambda (y) y) '() q))
    '((empty-k (closure y y ()))
      (proper-listo-inner-k (closure y y ()) _.0 (empty-k (_.0 closure y y ())))
      ((app-inner-k '_.0 _.1 () (empty-k _.0)) (=/= ((_.1 quote))) (absento (closure _.0)))))

  (test-check "var-any-k-1"
    (run 3 (q)
      (eval-expo-cps 'y '((y . (closure z z ()))) q))
    '((empty-k (closure z z ()))
      (proper-listo-inner-k (closure z z ()) _.0 (empty-k (_.0 closure z z ())))
      ((app-inner-k '_.0 _.1 () (empty-k _.0)) (=/= ((_.1 quote))) (absento (closure _.0)))))

;;;




  (test-check "punchline from intro"
    (equal? (replace* '((_.0 . x)) (caar (run 1 (q) (eval-expo q '() q)))) quinec)
    #t)

  (test-check "quine-gen-1"
    (run 1 (q) (eval-expo q '() q))
    '((((lambda (_.0) (list _.0 (list 'quote _.0)))
        '(lambda (_.0) (list _.0 (list 'quote _.0))))
       (=/= ((_.0 closure)) ((_.0 list)) ((_.0 quote)))
       (sym _.0))))

  (test-check "quine-gen-2"
    (replace* '((_.0 . x)) (car (car (run 1 (q) (eval-expo q '() q)))))
    quinec)

  (test-check "quine-gen-3"
    (run 1 (x)
      (fresh (p q)
        (=/= p q)
        (eval-expo p '() q) (eval-expo q '() p)
        (== `(,p ,q) x)))
    '((('((lambda (_.0)
            (list 'quote (list _.0 (list 'quote _.0))))
          '(lambda (_.0) (list 'quote (list _.0 (list 'quote _.0)))))
        ((lambda (_.0) (list 'quote (list _.0 (list 'quote _.0))))
         '(lambda (_.0) (list 'quote (list _.0 (list 'quote _.0))))))
       (=/= ((_.0 closure)) ((_.0 list)) ((_.0 quote)))
       (sym _.0))))

  (test-check "quine-gen-4-a"
    (run 1 (x)
      (fresh (p q r)
        (== '''((lambda (_.0)
                  (list 'quote (list 'quote (list _.0 (list 'quote _.0)))))
                '(lambda (_.0)
                   (list 'quote (list 'quote (list _.0 (list 'quote _.0))))))
            p)
        (== ''((lambda (_.0)
                 (list 'quote (list 'quote (list _.0 (list 'quote _.0)))))
               '(lambda (_.0)
                  (list 'quote (list 'quote (list _.0 (list 'quote _.0))))))
            q)
        (== '((lambda (_.0)
                (list 'quote (list 'quote (list _.0 (list 'quote _.0)))))
              '(lambda (_.0)
                 (list 'quote (list 'quote (list _.0 (list 'quote _.0))))))
            r)          
        (=/= p q) (=/= q r) (=/= r p)
        (eval-expo p '() q) (eval-expo q '() r) (eval-expo r '() p)
        (== `(,p ,q ,r) x)))
    '((''((lambda (_.0) (list 'quote (list 'quote (list _.0 (list 'quote _.0))))) '(lambda (_.0) (list 'quote (list 'quote (list _.0 (list 'quote _.0)))))) '((lambda (_.0) (list 'quote (list 'quote (list _.0 (list 'quote _.0))))) '(lambda (_.0) (list 'quote (list 'quote (list _.0 (list 'quote _.0)))))) ((lambda (_.0) (list 'quote (list 'quote (list _.0 (list 'quote _.0))))) '(lambda (_.0) (list 'quote (list 'quote (list _.0 (list 'quote _.0)))))))))
    
  (test-check "quine-gen-4-b"
    (run 1 (x)
      (fresh (p q r)
        (=/= p q) (=/= q r) (=/= r p)
        (eval-expo p '() q) (eval-expo q '() r) (eval-expo r '() p)
        (== `(,p ,q ,r) x)))
    '(((''((lambda (_.0)
             (list 'quote (list 'quote (list _.0 (list 'quote _.0)))))
           '(lambda (_.0)
              (list 'quote (list 'quote (list _.0 (list 'quote _.0))))))
        '((lambda (_.0)
            (list 'quote (list 'quote (list _.0 (list 'quote _.0)))))
          '(lambda (_.0)
             (list 'quote (list 'quote (list _.0 (list 'quote _.0))))))
        ((lambda (_.0)
           (list 'quote (list 'quote (list _.0 (list 'quote _.0)))))
         '(lambda (_.0)
            (list 'quote (list 'quote (list _.0 (list 'quote _.0)))))))
       (=/= ((_.0 closure)) ((_.0 list)) ((_.0 quote)))
       (sym _.0))))

  (test-check "backwards-1"
    (run 3 (q)
      (eval-expo q '() '(closure y y ())))
    '((lambda (y) y)
      (((lambda (_.0) _.0) (lambda (y) y)) (sym _.0))
      (((lambda (_.0) (_.0 _.0)) (lambda (y) y)) (sym _.0))))

  (test-check "backwards-2"
    (run 10 (q)
      (fresh (exp env val)
        (eval-expo exp env val)
        (== `(,exp ,env ,val) q)))
    '((('_.0 () _.0) (absento (closure _.0)))
      (('_.0 ((_.1 . _.2)) _.0) (=/= ((_.1 quote))) (absento (closure _.0)))
      (('_.0 ((_.1 . _.2) (_.3 . _.4)) _.0) (=/= ((_.1 quote)) ((_.3 quote))) (absento (closure _.0)))
      ((list) () ())
      ((_.0 ((_.0 . _.1) . _.2) _.1) (sym _.0))
      (('_.0 ((_.1 . _.2) (_.3 . _.4) (_.5 . _.6)) _.0) (=/= ((_.1 quote)) ((_.3 quote)) ((_.5 quote))) (absento (closure _.0)))
      (('_.0 ((_.1 . _.2) (_.3 . _.4) (_.5 . _.6) (_.7 . _.8)) _.0) (=/= ((_.1 quote)) ((_.3 quote)) ((_.5 quote)) ((_.7 quote))) (absento (closure _.0)))
      (('_.0 ((_.1 . _.2) (_.3 . _.4) (_.5 . _.6) (_.7 . _.8) (_.9 . _.10)) _.0) (=/= ((_.1 quote)) ((_.3 quote)) ((_.5 quote)) ((_.7 quote)) ((_.9 quote))) (absento (closure _.0)))
      (('_.0 ((_.1 . _.2) (_.3 . _.4) (_.5 . _.6) (_.7 . _.8) (_.9 . _.10) (_.11 . _.12)) _.0) (=/= ((_.1 quote)) ((_.11 quote)) ((_.3 quote)) ((_.5 quote)) ((_.7 quote)) ((_.9 quote))) (absento (closure _.0)))
      ((_.0 ((_.1 . _.2) (_.0 . _.3) . _.4) _.3) (=/= ((_.0 _.1))) (sym _.0))))

  )

