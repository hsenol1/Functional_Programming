(module lang (lib "eopl.ss" "eopl")                

  ;; grammar for the LETREC language

  (require "drscheme-init.scm")
  
  (provide (all-defined-out))

  ;;;;;;;;;;;;;;;; grammatical specification ;;;;;;;;;;;;;;;;
  
  (define the-lexical-spec
    '((whitespace (whitespace) skip)
      (comment ("%" (arbno (not #\newline))) skip)
      (identifier
       (letter (arbno (or letter digit "_" "-" "?")))
       symbol)
      (number (digit (arbno digit)) number)
      (number ("-" digit (arbno digit)) number)
      ))
  
  (define the-grammar
    '((program (expression) a-program)

      (expression (number) const-exp)
      (expression
        ("-" "(" expression "," expression ")")
        difference-exp)
      
      (expression
       ("zero?" "(" expression ")")
       zero-exp)

      (expression
       ("if" expression "then" expression "else" expression)
       if-exp)

      (expression (identifier) var-exp)

      (expression
       ("let" identifier "=" expression "in" expression)
       lett-exp)   

      (expression
       ("proc" "(" identifier ")" expression)
       proc-exp)

      (expression
       ("(" expression expression ")")
       call-exp)

      (expression
        ("letrec"
          identifier "(" identifier ")" "=" expression
           "in" expression)
        letrec-exp)
      
      ; #####################################################
      ; ###### ENTER YOUR CODE HERE
      ; ###### you need to add the expression variants; 
      ; ###### proc-nested, call-nested, letrec-nested as 
      ; ###### described in the pdf file.
      ; #####################################################
      ; proc-nested-exp
       (expression
       ("proc-nested" "(" identifier "," identifier "," identifier ")" expression )
       proc-nested-exp)

      ; call-nested-exp
      (expression
       ("call-nested" "(" expression expression "," expression ")")
       call-nested-exp)

      ; letrec-nested-exp
      (expression
       ("letrec-nested" identifier "(" identifier "," identifier ")" "=" expression "in" expression)
       letrec-nested-exp)

      ; #####################################################

    ))
  
  ;;;;;;;;;;;;;;;; sllgen boilerplate ;;;;;;;;;;;;;;;;
  
  (sllgen:make-define-datatypes the-lexical-spec the-grammar)
  
  (define show-the-datatypes
    (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))
  
  (define scan&parse
    (sllgen:make-string-parser the-lexical-spec the-grammar))
  
  (define just-scan
    (sllgen:make-string-scanner the-lexical-spec the-grammar))
  
  )