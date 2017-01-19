;;evaluates the given expression in a given environment.
(defun lisp2li (expr env)
  (if (atom expr) ;;if the expression is an atom...
      (if (constantp expr) ;;...and a constant...
          (cons :LIT expr) ;;...simply append :LIT to the constant's value.
        (let ((pos (position expr env))) ;...and a variable
          (if pos ;;if it is found in the environment, append :VAR to its position
              (cons :VAR (+ 1 pos))
            ;;(warn "~s absent de l'environnement" expr)))) ;;otherwise send a warning
            (cons :UNKNOWN expr))))
    (let ((fun (car expr)) ;;if the expression isn't an atom
          (args (cdr expr)))
      (cond
       ((eq 'defun fun) ;;defun case
        (list
         :CALL 'set-li-defun
         (cons ':LIT (first args))
         (list ':LIT ':LAMBDA (length (second args)) (list* (lisp2li (third args) (second args))))))
       ((eq 'setf fun) ;;setf case
        (set-li-defun fun env))
       ((eq 'loop fun) ;;loop macro case
        (let ((var (list (list (cadr args) (cadddr args)))))
          (list
           :LET
           1
           (let-lisp2li var (addtoenv var env))
           (list (loop2li (cdr args) (addtoenv var env))))))
       ((macro-function fun) ;;macro case
        (lisp2li (macroexpand-1 expr) env))
       ((eq 'quote fun) ;;quote case
        (list 
         :CONST (first args)))
       ((eq 'if fun) ;;if case
        (cons
         :IF
         (map-lisp2li args env)))
       ((eq 'progn fun) ;;progn case
        (list
         :PROGN
         (map-lisp2li args env)))      
       ((eq 'let fun) ;;let case
        (list
         :LET
         (length (car args))
         (let-lisp2li (car args) (addtoenv (car args) env))
         (map-lisp2li (rest args) (addtoenv (car args) env))))
       ((eq 'let* fun) ;;let* case
        (list
         :LET*
         (length (car args))
         (let-lisp2li (car args) (addtoenv (car args) env))
         (map-lisp2li (rest args) (addtoenv (car args) env))))
       ((get-li-defun fun) ;;already known user function case
        (list*
         :MCALL
         fun
         (map-lisp2li args env)))
       ((fboundp fun) ;;function defined in CLisp case
        (list*
         :CALL 
         fun
         (map-lisp2li args env)))
       ((not (fboundp fun)) ;;unknown function or first appearance in the environment case
        (list 
         :UNKNOWN 
         (cons fun args) env))))))

;;redefines let
(defun let-lisp2li (lexpr env)
  (let* ((expr (car lexpr)) (pos (position (car expr) env)))
    (if (atom lexpr)
        ()
      (cons
       (list
        :SET-VAR
        (+ pos 1)
        (lisp2li (cadr expr) env))
       (let-lisp2li (cdr lexpr) env)))))

;;allows to analyze a list of expressions by ana lazing each of the expressions composing it
(defun map-lisp2li (lexpr env)
  (if (atom lexpr)
      ()
    (cons
     (lisp2li (first lexpr) env)
     (map-lisp2li (rest lexpr) env))))

;;
(defun addtoenv (lexpr env)
  (let ((expr (car lexpr)))
    (if (atom lexpr)
        env
      (addtoenv (cdr lexpr) (append env (list (car expr)))))))

;;redefines set
(defun set-li-defun (expr env)
  (setf (get expr :DEFUN) env))

;;checks if fun is known in the environment
(defun get-li-defun (fun)
  (get fun :DEFUN)) 

;ne gere que le cas "loop for"
;loop for instr in code do (print instr)
;(defun loop_test (li n) macroexpand-1 (loop for cel in li do (+ 1 n)))))
(defun loop2li (lexpr env)
  (let ((val (caddr lexpr)) (args (cddddr lexpr)))
    ;(print args)
    (list
     :LOOP     
     (lisp2li val env)
     (random (random 1000000))
     (lisp2li (car args) env)
     (lisp2li (append (list 'setf (car lexpr) (list 'cdr (car lexpr)))) env))))




;tests
;(lisp2li '(if (< a 0) 1 0) '(a))
;(lisp2li '(defun fact (n) (if (<= n 0) 1 (* n (fact (- n 1))))) ())
;(lisp2li '(defun fibo (n) (if (<= n 0) 0 (if (= 1 n) 1  (+ (fibo (- n 1)) (fibo (- n 2)))))) ())
;(lisp2li '(defun cc (a) (cond ((< a 1) 0) ((> a 1) 1))) '(a))
;(lisp2li '(defun fibonacci (n) (cond ((> n 1) (+ (fibonacci (- n 1)) (fibonacci (- n 2)))) ((= n 1) 1) ((= n 0) 0))) ())
;(lisp2li '(cond ((< a 1) 0) ((> a 1) 1)) '(a))
;(lisp2li '(cond ((< 0 1) 0) ((> 0 1) 1)) ())
;(butlast (macroexpand '(cond ((< a 1) 0) ((> a 1) 1))))
;(lisp2li '(defun t27 (n) (if (<= n 0) (+ n n) (* (t27 (- n 1)) n))) ())
