;;evaluates an expression with its environnment
(defun eval-li (expr env)
  (if (atom expr)
      (error "pas une expression li")
    (ecase (car expr)
	   (:LIT (cdr expr))
           (:CONST (cdr expr))
	   (:VAR (aref env (cdr expr)))
	   (:SET-VAR (setf (aref env (cadr expr))
			   (eval-li (cddr expr) env)))
	   (:IF (if (eval-li (second expr) env)
		    (eval-li (third expr) env)
		  (eval-li (fourth expr) env)))
           (:LET ())
           (:LET* ())
	   (:CALL (apply (second expr) (map-eval-li (cddr expr) env)))
	   (:MCALL (let* ((fun (get-li-defun (second expr)))
			  (nenv (make-array (+ 1 (second fun)))))
		     (eval-li (cddr fun) (make-fill-env-eval-li (cddr expr) env nenv 1))))
	   (:PROGN (eval-li-progn (cdr expr) env))
	   (:UNKNOWN (let ((nexpr (lisp2li (second expr)(third expr))))
		       (if (eq (car nexpr) :UNKNOWN)
			   (error "unknown ~s" expr)
			   (eval-li nexpr env)))))))

;;allows us to run eval-li step by step 
(defun map-eval-li (lexpr env)
  (if (null lexpr)
      ()
    (cons (eval-li (first lexpr) env) (map-eval-li (rest lexpr) env))))

;;evaluates all the expressions in sequence and returns the value of the last
(defun eval-li-progn (lexpr env)
  (if (null (cdr lexpr))
      (eval-li (car lexpr) env)
    (cons (eval-li (car lexpr) env) (eval-li-progn (cdr lexpr) env))))

;;evaluates the expressions one by one and makes the last expr the cdr of the last cons
(defun map-eval-li* (lexpr env)
  (if (null (cdr lexpr))
      (eval-li (car lexpr) env)
    (cons (eval-li (car lexpr) env) (map-eval-li (cdr lexpr) env))))

;;takes an int, a list of expr and an environment and creates an array of the successive calls to eval-li
(defun map-eval-li-make-env (nbexpr lexpr env)
  (if (eq nbexpr 0)
      ()
    (lambda (nbexpr) (lambda (nbexpr) (setf eval-arr (make-array nbexpr
                                                                :initial-contents (eval-li (car lexpr) env)))
                       (concatenate 'array eval-arr (map-eval-li-make-env (- nbexpr 1) (cdr lexpr) env))))))

(defun make-fill-env-eval-li (args env nenv index)
  (if
    (null args)
    nenv
    (progn
      (setf (aref nenv index) (eval-li (car args) env))
      (make-fill-env-eval-li (cdr args) env nenv (+ 1 index)))))

;tests
;(eval-li (lisp2li '(defun f (x) (+ 2 x)) ()) ())
;(eval-li (lisp2li '(if (< 0 1) 0 1) ()) ())
;(eval-li (lisp2li '(defun fact (n) (if (<= n 0) 1 (* n (fact (- n 1))))) ()) ())
;(eval-li (lisp2Li '(fact 4) ()) ())
;(eval-li '(:IF ((:CALL <) (:CONST 1) (:CONST 0)) (:CONST 1) (:CONST 0)) '())
