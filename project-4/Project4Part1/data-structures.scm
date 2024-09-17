(module data-structures (lib "eopl.ss" "eopl")

  ;; data structures for letrec-lang.

  (require "lang.scm")             ; for expression?

  (provide (all-defined-out))      ; too many things to list

;;;;;;;;;;;;;;;; expressed values ;;;;;;;;;;;;;;;;

;;; an expressed value is either a number, a boolean or a procval.

  (define-datatype expval expval?
    (num-val
      (value number?))
    (bool-val
      (boolean boolean?))
    (proc-val 
      (proc proc?)))

;;; extractors:

  ;; expval->num : ExpVal -> Int
  (define expval->num
    (lambda (v)
      (cases expval v
	(num-val (num) num)
	(else (expval-extractor-error 'num v)))))

  ;; expval->bool : ExpVal -> Bool
  (define expval->bool
    (lambda (v)
      (cases expval v
	(bool-val (bool) bool)
	(else (expval-extractor-error 'bool v)))))

  ;; expval->proc : ExpVal -> Proc
  (define expval->proc
    (lambda (v)
      (cases expval v
	(proc-val (proc) proc)
	(else (expval-extractor-error 'proc v)))))
  
;; var-exp? : Exp -> Bool
(define var-exp?
  (lambda (exp)
    (cases expression exp
      (var-exp (var) #t)
      (else #f))))

  (define expval-extractor-error
    (lambda (variant value)
      (eopl:error 'expval-extractors "Looking for a ~s, found ~s"
	variant value)))

;;;;;;;;;;;;;;;; procedures ;;;;;;;;;;;;;;;;

  ;; proc? : SchemeVal -> Bool
  ;; procedure : Var * Exp * Env -> Proc
  (define-datatype proc proc?
    (procedure
      (bvar symbol?)
      (body expression?)
      (env environment?))
      
    ; #####################################################
    ; ###### ENTER YOUR CODE HERE
    ; ###### you need to define a variant to the procedure; 
    ; ###### the nested-procedure
    ; ###### this variant takes 2 additional parameters than 
    ; ###### the original version of procedure, the count 
    ; ###### variable and the name of the procedure.
    ; #####################################################

    (nested-procedure
     (bvar symbol?)
     (body expression?)
     (env environment?)
     (count symbol?)
     (name symbol?))



    ; #####################################################
  )
  (define (nested-procedure? p)
  (and (proc? p) (nested-procedure? (expval->proc p))))



  ;; Page: 86
  (define-datatype environment environment?
    (empty-env)
    (extend-env 
      (bvar symbol?)
      (bval expval?)
      (saved-env environment?))
    (extend-env-rec
      (id symbol?)
      (bvar symbol?)
      (body expression?)
      (saved-env environment?))
    
    ; ########################################################
    ; ###### ENTER YOUR CODE HERE
    ; ###### you need to define a variant to the environment; 
    ; ###### extend-env-rec-nested
    ; ###### this variant is the nested version of the 
    ; ###### extend-env-rec. It takes 1 additional parameter as 
    ; ###### the count variable.
    ; ########################################################s

    (extend-env-rec-nested
     (id symbol?)
     (bvar symbol?)
     (body expression?)
     (saved-env environment?)
     (count symbol?))


    ; #####################################################
      
  )

)
;; drscheme-init.scm - compatibility file for DrScheme
;; usage: (require "drscheme-init.scm")

;;; makes structs printable, and provides basic functionality for
;;; testing.  This includes pretty-printing and tracing.