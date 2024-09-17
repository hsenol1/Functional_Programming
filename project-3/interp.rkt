#lang eopl

;; interpreter for the PROC language, using the procedural
;; representation of procedures.



(require "lang.rkt")
(require "data-structures.rkt")
(require "environments.rkt")

(provide value-of-program value-of)

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

;; run : String -> ExpVal
(define run
  (lambda (s)
    (value-of-program (scan&parse s))))

;; value-of-program : Program -> ExpVal
(define value-of-program 
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
                 (value-of exp1 (init-env))))))

;; value-of : Exp * Env -> ExpVal
(define value-of
  (lambda (exp env)
    (cases expression exp
      
      
      (const-exp (num) (num-val num))
      
      
      (var-exp (var) (apply-env env var))
      
     
      (diff-exp (exp1 exp2)
                (let ((val1 (value-of exp1 env))
                      (val2 (value-of exp2 env)))
                  (let ((num1 (expval->num val1))
                        (num2 (expval->num val2)))
                    (num-val
                     (- num1 num2)))))
      
      
      (zero?-exp (exp1)
                 (let ((val1 (value-of exp1 env)))
                   (let ((num1 (expval->num val1)))
                     (if (zero? num1)
                         (bool-val #t)
                         (bool-val #f)))))
      
     
      (if-exp (exp1 exp2 exp3)
              (let ((val1 (value-of exp1 env)))
                (if (expval->bool val1)
                    (value-of exp2 env)
                    (value-of exp3 env))))
      
      
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
      
      ;;----------------------------------------------------
      ; INSERT YOUR CODE HERE
      ; Write the required expressions starting from here

      ;;-------------------------------------------------
      
     (queue-exp ()
            (queue-val (empty-queue)))

      (queue-push-exp (q exp)
                      (let ((queue (expval->queue (value-of q env)))
                            (num (expval->num (value-of exp env))))

                        
                            (queue-val (cons num queue)))
                      )
      
      (queue-pop-exp (q)
               (let ((queue (expval->queue (value-of q env))))
                 (if (null? queue)
                     (queue-val (empty-queue))
                     (queue-val (queue-pop-helper queue)))))

         (queue-peek-exp (e)
                      (let ((queue (expval->queue (value-of e env))))
                        (if (null? queue)
                           (num-val 2000)
                                  (num-val (queue-peek-helper queue)))))
      

      (queue-push-multi-exp (q args)
        (if (null? args)
            (value-of q env)
            (let ((num-exp (car args)))
              (let ((new-queue-exp (queue-push-exp q num-exp)))
                 (value-of (queue-push-multi-exp new-queue-exp (cdr args)) env)))))

      (queue-pop-multi-exp (q arg)
         (if (zero? arg)
             (value-of q env)
             (let ((new-queue-exp (queue-pop-exp q)))
               (value-of (queue-pop-multi-exp new-queue-exp (- arg 1)) env))))
      
      (queue-merge-exp (q1 q2)
         (let ((q1-val (expval->queue (value-of q1 env)))
               (q2-val (expval->queue (value-of q2 env))))
           (if (null? q2-val)
               (value-of q1 env)
               (let* ((pop-elt (queue-peek-exp q2))
                      (pop-list (queue-pop-exp q2))
                      (q1-after-push (queue-push-exp q1 pop-elt)))
                 (value-of (queue-merge-exp q1-after-push pop-list) env)))))
      





              
                               


      

     
      
      
      )))

;;-----------------------------------------
; INSERT YOUR CODE HERE
; you may use this area to define helper functions
;;-----------------------------------------

(define (empty-queue)
  '())

(define (queue-pop-helper lst)
                   (if (null? (cdr lst))
                       '()
                       (cons (car lst) (queue-pop-helper (cdr lst)))))

(define (queue-peek-helper lst)
  (if (null? (cdr lst))
      (car lst)
      (queue-peek-helper (cdr lst))))



(define (multi-push-helper queue args)
  (if (null? args)
      queue
      (multi-push-helper (cons (expval->num (car args)) queue) (cdr args))))








;;-----------------------------------------

(define apply-procedure
  (lambda (proc1 val)
    (cases proc proc1
      (procedure (var body saved-env)
                 (value-of body (extend-env var val saved-env))))))
