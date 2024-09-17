(module interp (lib "eopl.ss" "eopl")
  
  ;; interpreter for the EXPLICIT-REFS language

  (require "drscheme-init.scm")

  (require "lang.scm")
  (require "data-structures.scm")
  (require "environments.scm")
  (require "store.scm")
  (require racket/trace)
  
  (provide value-of-program value-of instrument-let instrument-newref)

  ;;;;;;;;;;;;;;;; switches for instrument-let ;;;;;;;;;;;;;;;;

  (define instrument-let (make-parameter #f))

  ;; say (instrument-let #t) to turn instrumentation on.
  ;;     (instrument-let #f) to turn it off again.

  ;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

  ;; value-of-program : Program -> ExpVal
  ;; Page: 110
  (define value-of-program 
    (lambda (pgm)
      (initialize-store!)               ; new for explicit refs.
      (cases program pgm
        (a-program (exp1)
                   (value-of exp1 (init-env))))))

  ;; value-of : Exp * Env -> ExpVal
  ;; Page: 113
  (define value-of
    (lambda (exp env)
      (cases expression exp

        ;\commentbox{ (value-of (const-exp \n{}) \r) = \n{}}
        (const-exp (num) (num-val num))

        ;\commentbox{ (value-of (var-exp \x{}) \r) = (apply-env \r \x{})}
        (var-exp (var) (apply-env env var))

        ;\commentbox{\diffspec}
        (diff-exp (exp1 exp2)
                  (let ((val1 (value-of exp1 env))
                        (val2 (value-of exp2 env)))
                    (let ((num1 (expval->num val1))
                          (num2 (expval->num val2)))
                      (num-val
                       (- num1 num2)))))
      
        ;\commentbox{\zerotestspec}
        (zero?-exp (exp1)
                   (let ((val1 (value-of exp1 env)))
                     (let ((num1 (expval->num val1)))
                       (if (zero? num1)
                           (bool-val #t)
                           (bool-val #f)))))
              
        ;\commentbox{\ma{\theifspec}}
        (if-exp (exp1 exp2 exp3)
                (let ((val1 (value-of exp1 env)))
                  (if (expval->bool val1)
                      (value-of exp2 env) 
                      (value-of exp3 env))))

        ;\commentbox{\ma{\theletspecsplit}}
        (let-exp (var exp1 body)       
                 (let ((val1 (value-of exp1 env)))
                   (value-of body
                             (extend-env var val1 env))))
        
        (proc-exp (var body)
                  (proc-val (procedure var body env)))

        (call-exp (rator rand)
                  (let ((proc (expval->proc (value-of rator env)))
                        (arg (value-of rand env)))
                    (apply-procedure proc arg)))

        (letrec-exp (p-names b-vars p-bodies letrec-body)
                    (value-of letrec-body
                              (extend-env-rec* p-names b-vars p-bodies env)))

        (begin-exp (exp1 exps)
                   (letrec 
                       ((value-of-begins
                         (lambda (e1 es)
                           (let ((v1 (value-of e1 env)))
                             (if (null? es)
                                 v1
                                 (value-of-begins (car es) (cdr es)))))))
                     (value-of-begins exp1 exps)))

        (newref-exp (exp1)
                    (let ((v1 (value-of exp1 env)))
                      (ref-val (newref v1))))

        (deref-exp (exp1)
                   (let ((v1 (value-of exp1 env)))
                     (let ((ref1 (expval->ref v1)))
                       (deref ref1))))

        (setref-exp (exp1 exp2)
                    (let ((ref (expval->ref (value-of exp1 env))))
                      (let ((v2 (value-of exp2 env)))
                        (begin
                          (setref! ref v2)
                          (num-val 23)))))

        ; #####################################################
        ; ###### ENTER YOUR CODE HERE
        ; ###### value-of cases for new expressions, remember
        ; ###### that you need to use memory functionalities. 
        ; #####################################################

        (new-vec-exp (exp1 exp2)
                     (let ((length (expval->num (value-of exp1 env))))
                       (let ((value (value-of exp2 env)))
                         (vec-val (make-vector length value)))))

        (update-vec-exp (exp1 exp2 exp3)
                        (let ((vector (value-of exp1 env))
                              (index (expval->num (value-of exp2 env)))
                              (value (value-of exp3 env)))
                          (setref! (list-ref (expval->vec vector) index) value)))

        (read-vec-exp (exp1 exp2)
                      (let ((vector (value-of exp1 env))
                            (index (expval->num (value-of exp2 env))))
                        (deref (list-ref (expval->vec vector) index))))

        (len-vec-exp (exp1)
                     (let ((vector (expval->vec (value-of exp1 env))))
                       (num-val(length vector)))
                             
                     )

        (swap-vec-exp (exp1 exp2 exp3)
                      (let ((vector (value-of exp1 env))
                            (index1 (expval->num (value-of exp2 env)))
                            (index2 (expval->num (value-of exp3 env))))
                        (let ((value (deref (list-ref (expval->vec vector) index1))))
                          (begin
                            (setref! (list-ref (expval->vec vector) index1) (deref (list-ref (expval->vec vector) index2)))
                            (setref! (list-ref (expval->vec vector) index2) value))))
                      )
        

        
        (copy-vec-exp (exp1)
                      (let ((vector (value-of exp1 env)))
                        (let ((copy (make-vector (length (expval->vec vector)) 1)))
                          (map update-helper  copy  (map deref (expval->vec vector)))   
                          (vec-val copy)))
                                                          
                      )

        (vec-mult-exp (exp1 exp2)
                      (let ((vec1-refs (expval->vec (value-of exp1 env)))  ; Extract vector from expression 1
                            (vec2-refs (expval->vec (value-of exp2 env)))) ; Extract vector from expression 2
                        (if (not (eq? (length vec1-refs) (length vec2-refs)))  ; Check if vectors have the same length
                            (eopl:error "size did not match")                  ; If not, raise an error
                            (let ((vec1-values (map deref vec1-refs))            ; Dereference all elements of vector 1
                                  (vec2-values (map deref vec2-refs)))           ; Dereference all elements of vector 2
                              (let ((result-refs (make-vector (length vec1-refs) 0)))  ; Create result vector initialized with 0
                                (map update-helper                                     ; Update each reference of result vector
                                     result-refs 
                                     (map num-val 
                                          (map (lambda (x y) (* x y)) 
                                               (map expval->num vec1-values) 
                                               (map expval->num vec2-values))))
                                (vec-val result-refs))))))                           ; Return result vector



         


        (new-queue-exp (exp1)
                       (let ((size (value-of exp1 env)))
                         (let ((size-num (expval->num size)))
                           (let ((new-queue (make-vector (+ size-num 2) '())))
                             (begin
                               (setref! (list-ref new-queue 0) 0)  ; Set front to 0
                               (setref! (list-ref new-queue 1) 0)  ; Set rear to 0
                               (vec-val new-queue))))))

        (enqueue-exp (exp1 exp2)
                     (let ((queue (value-of exp1 env))
                           (value (value-of exp2 env)))
                       (let ((new_queue (expval->vec queue)))
                         (let ((front (deref (list-ref new_queue 0)))
                               (rear (deref (list-ref new_queue 1))))
                           (if (= rear (length new_queue))
                               (eopl:error "queue-overflow")
                               (begin
                                 (setref! (list-ref new_queue (+ rear 2)) value)
                                 (setref! (list-ref new_queue 1) (+ rear 1))))))))

        (queue-size-exp (exp1)
                        (let ((queue (value-of exp1 env)))
                          (let ((new_queue (expval->vec queue)))
                            (let ((front (deref (list-ref new_queue 0)))
                                  (rear (deref (list-ref new_queue 1))))
                              (num-val (- rear front))))))

        (dequeue-exp (exp1)
                     (let ((queue (value-of exp1 env)))
                       (let ((new_queue (expval->vec queue)))
                         (let ((front (deref (list-ref new_queue 0)))
                               (rear (deref (list-ref new_queue 1))))
                           (if (= front rear)
                               (num-val -1)
                               (begin
                                 (let ((value (deref (list-ref new_queue (+ front 2)))))
                                   (setref! (list-ref new_queue 0) (+ front 1))
                                   value)))))))
        
        (peek-queue-exp (exp1)
                        (let ((queue (value-of exp1 env)))
                          (let ((new_queue (expval->vec queue)))
                            (let ((front (deref (list-ref new_queue 0)))
                                  (rear (deref (list-ref new_queue 1))))
                              (if (= front rear)
                                  (eopl:error "queue is empty")
                                  (deref (list-ref new_queue (+ front 2))))))))
        
        (queue-empty-exp (exp1)
                         (let ((queue (value-of exp1 env)))
                           (let ((new_queue (expval->vec queue)))
                             (let ((front (deref (list-ref new_queue 0)))
                                   (rear (deref (list-ref new_queue 1))))
                               (bool-val (= front rear))))))




    (print-queue-exp (exp1)

                     (let ((queue (value-of exp1 env)))
                       (let ((new_queue (expval->vec queue)))
                         (let ((front (deref (list-ref new_queue 0)))
                               (rear (deref (list-ref new_queue 1))))
          
                           (begin
                             (display "(")
                             (display-element new_queue front rear)
                             (display ")"))))))




    

        
        )))


  
  ; ###### YOU CAN WRITE HELPER FUNCTIONS HERE



  
  (define (update-helper index value)
    (setref! index value))


  (define (display-element queue front rear)
    (if (= front rear)
        (display "")
        (begin
          (display (expval->num (deref (list-ref queue front))))
          (display ", ")
          (display-element queue (+ front 1) rear))))

  


 





   
  ;; instrumented version
  (define apply-procedure
    (lambda (proc1 arg)
      (cases proc proc1
        (procedure (var body saved-env)
                   (let ((r arg))
                     (let ((new-env (extend-env var r saved-env)))
                       (when (instrument-let)
                         (begin
                           (eopl:printf
                            "entering body of proc ~s with env =~%"
                            var)
                           (pretty-print (env->list new-env))
                           (eopl:printf "store =~%")
                           (pretty-print (store->readable (get-store-as-list)))
                           (eopl:printf "~%")))
                       (value-of body new-env)))))))


    


  ;; store->readable : Listof(List(Ref,Expval)) 
  ;;                    -> Listof(List(Ref,Something-Readable))
  (define store->readable
    (lambda (l)
      (map
       (lambda (p)
         (cons
          (car p)
          (expval->printable (cadr p))))
       l)))
 
  )
  


  
