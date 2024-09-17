#lang eopl

;; interpreter for the LET language.  The \commentboxes are the
;; latex code for inserting the rules into the code in the book.
;; These are too complicated to put here, see the text, sorry.

(require "lang.rkt")
(require "data-structures.rkt")
(require "environments.rkt")

(provide value-of-program value-of)

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

;; value-of-program : Program -> ExpVal
;; Page: 71
(define value-of-program 
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
                 (value-of exp1 (init-env))))))



;; value-of : Exp * Env -> ExpVal
;; Page: 71
(define value-of
  (lambda (exp env)
    (cases expression exp

      (if-exp (cond1 exp1 conds exps else-exp)
              (let ((val1 (value-of cond1 env)))
                (cond
                  ((expval->bool val1)  (value-of exp1 env))
                  ((null? conds) (value-of else-exp env))
                  (else (value-of (if-exp (car conds) (car exps) (cdr conds) (cdr exps) else-exp) env)))))
         
              

           

      (let-exp (var exp1 body)       
               (let ((val1 (value-of exp1 env)))
                 (value-of body
                           (extend-env var val1 env))))

      
      
      (const-exp (num) (num-val num))

      (rational-exp (num1 num2)
                    (if (zero? num2) ;; eopl pre built.
                        (eopl:error "Error: Division by zero!")
                        (rational-val (cons num1 num2))))

      (var-exp (var) (apply-env env var))


        
      
      (op-exp (exp1 exp2 op)
              (let ((val1 (value-of exp1 env))
                    (val2 (value-of exp2 env)))
                  (let ((num1 (expval->rational val1))
                        (num2 (expval->rational val2)))
                      (cond
                        
                        ((and (number? num1) (number? num2))
                          (num-val
                            (cond 
                              ((= op 1) (+ num1 num2)) ;; add
                              ((= op 2) (* num1 num2)) ;; mult
                              ((= op 3) (/ num1 num2));; division
                              (else (- num1 num2)) ;; substract
                                 
                              )))
                        
                        ((and (number? num1) (not (number? num2)))
                          (rational-val
                          (let ((num2top (car num2))
                                (num2bot (cdr num2)))
                            (cond 
                              ((= op 1) (cons (+ (* num1 num2bot) num2top) num2bot)) ;; add
                              ((= op 2) (cons (* num1 num2top) num2bot)) ;; mult
                              ((= op 3) (cons (* num1 num2bot) num2top)) ;; div
                              (else (cons (- (* num1 num2bot) num2top) num2bot)) ;; sub
                         
                              
                              ))))

                        ((and (number? num2) (not (number? num1)))
                          (rational-val
                          (let ((num1top (car num1))
                                (num1bot (cdr num1)))
                            (cond 
                              ((= op 1) (cons (+ (* num1bot num2) num1top) num1bot)) ;; add
                              ((= op 2) (cons (* num1top num2) num1bot)) ;;; 
                              ((= op 3) (cons num1top (* num1bot num2)))
                              (else (cons (- num1top (* num1bot num2)) num1bot))
                              
                              ))))

                       
                        (else
                          (rational-val
                          (let ((num1top (car num1))
                                (num1bot (cdr num1))
                                (num2top (car num2))
                                (num2bot (cdr num2)))
                            (cond 
                              ((= op 1) (cons (+ (* num1top num2bot) (* num1bot num2top)) (* num1bot num2bot))) ;; add
                              ((= op 2) (cons (* num1top num2top) (* num1bot num2bot))) ;; multiply
                              ((= op 3) (cons (* num1top num2bot) (* num1bot num2top))) ; divide
                              (else (cons (- (* num1top num2bot) (* num1bot num2top)) (* num1bot num2bot)))
                              
                            ))))
                        ))))

      
      (zero?-exp (exp1)
               (let ((val1 (value-of exp1 env)))
                 (cases expval val1
                   (num-val (if-num)
                            (let ((num1 (expval->num val1)))
                              (if (= num1 0)
                                  (bool-val #t)
                                  (bool-val #f))))
                   (rational-val (if-rat)
                            (let ((rat1 (expval->rational)))
                              (if (= (car rat1) 0)
                                  (bool-val #t)
                                  (bool-val #f))))
                   (else (eopl:error "NOT THE RIGHT TYPE!")))))



      (list-exp ()
                (list-val '()))

      (sum-exp (lst)
              (let ((listval (value-of lst env)))
                (let ((listreal (expval->list listval)))
                  (num-val (sumrec listreal)))))

      
            

             (simpl-exp (exp)
                        (let ((val1 (value-of exp env)))
                          (let ((numb (expval->rational val1)))
                            (if (number? numb) numb
                            (rational-val (let ((nominator (car numb))
                                                      (denominator (cdr numb)))
                                                  (cons (/ nominator (euclidian nominator denominator)) (/ denominator (euclidian nominator denominator)))))))))

                                         
                           
                  
              
          (cons-exp (exp1 lst)
                (let ((val1 (value-of exp1 env))
                     (val2 (value-of lst env)))
                      (list-val
                      (let ((num1 (expval->num val1)))
                        (let ((lst (expval->list val2)))
                          (cons num1 lst))))) ;; add front.
                  

                          
                     
                  

      


      ))))

(define sumrec (lambda (lst)
                       (if (null? lst)
                           0
                           (+ (car lst) (sumrec (cdr lst))))))
(define euclidian (lambda (a b)
                    (cond ((= b 0) a)
                          ((= a 0) b)
                          ((< a b) (euclidian b a))
                          (else (euclidian b (modulo a b)))))) ;;; modulo is predefined in lang eopl




                                   


                  