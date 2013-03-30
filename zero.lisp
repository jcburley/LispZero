; TODO: Consider making p_environment a true assoc list (a binding is
; '(key value) instead of '(key . value)).

; (onelet ((var exp)) body)
; E.g. (onelet (('x 'y)) '(car (curenv))) => (x . y)
; E.g. (onelet (('x 'y)) 'x) => y
; TODO: Pull into caller(s) so we leave nothing in the namespace?
(defglobal 'onelet
  '((me formlist env)
    (eval
     (eval (car (cdr formlist)) env)
     (cons
      (cons
       (eval (car (car (car formlist))) env)
       (eval (car (cdr (car (car formlist)))) env))
      env))
    ))

(defglobal 'null
  '((me formlist env)
    (onelet (('arg (eval (car formlist) env)))
	    '(cond ((atom arg) (eq arg '()))
		   ('t '())))))

; Returns a list consisting of the (evaluated) arguments (supports any number of args)
(defglobal 'list
  '((me formlist env)
    (cond ((null formlist) '())
	  ('t (cons (eval (car formlist) env)
		    (apply me me (cdr formlist) env))))))

; (applied params argsname envname body)
; TODO: Pull into caller(s)
(defglobal 'applied
  '((me formlist env)
    (onelet (('params (eval (car formlist) env)))
	    '(onelet (('argsname (eval (car (cdr formlist)) env)))
		     '(onelet (('envname (eval (car (cdr (cdr formlist))) env)))
			      '(onelet (('body (eval (car (cdr (cdr (cdr formlist)))) env)))
				       '(cond ((null params) body)
					      ('t (list 'onelet
							(list (list (list 'quote
									  (car params))
								    (list 'eval
									  (list 'car
										argsname)
									  envname)))
							(list 'quote
							      (applied (cdr params)
								       (list 'cdr
									     argsname)
								       envname
								       body)))))))))))

; (defun name params body)
(defglobal 'defun
  '((me formlist env)
    (defglobal
      (car formlist)
      (list '(me formlist env)
	    (applied (car (cdr formlist))
		     'formlist
		     'env
		     (car (cdr (cdr formlist))))))))

(defun caar (x)
  (car (car x)))

(defun cadr (x)
  (car (cdr x)))

(defun cadar (x)
  (car (cdr (car x))))

(defun caddr (x)
  (car (cdr (cdr x))))

(defun caddar (x)
  (car (cdr (cdr (car x)))))

; After this point, defun should be reasonably expected to create lambdas.
;
; However, there are two issues with this:
;   1.  The defuns must create forms that Lisp Zero itself can process,
;       since the evaluation loop relies on their being supported via
;       the underlying engine.  For examples, evcon. and evlis. recursively
;       invoke themselves directly, not via eval. -- and of course the
;       user is expected to make this all useful by typing "(eval. ...)".
;   2.  The original Lisp ("Lisp One"?) does not itself define how defun
;       is to work.  In particular, it doesn't define the concept of a
;       global namespace to which new function definitions are added.
;       This means the following defuns are "unreachable" via any particular
;       assocation list and so cannot be expected to be "reached" via
;       any invocation of eval., although they are reached by its
;       implementation (thus representing an unexposed buildup of the
;       underlying engine).
;
; So it seems reasonable to make Lisp One a bit easier to use by
; offering a defun. that works like defun but adds it to a global list
; as a lambda, rather than to the Lisp Zero global list as a zedba.
;
; See below the jmc.lisp portion of this file for that code

; jmc.lisp with edits

; The Lisp defined in McCarthy's 1960 paper, translated into CL.
; Assumes only quote, atom, eq, cons, car, cdr, cond.
; Bug reports to lispcode@paulgraham.com.

; Cannot use eq since that is defined solely for atoms.
(defun null. (x)
  (cond (x '())
	('t 't)))

(defun and. (x y)
  (cond (x (cond (y 't) ('t '())))
        ('t '())))

; Syntactic sugar for null.
(defun not. (x)
  (cond (x '())
        ('t 't)))

(defun append. (x y)
  (cond ((null. x) y)
        ('t (cons (car x) (append. (cdr x) y)))))

(defun list. (x y)
  (cons x (cons y '())))

(defun pair. (x y)
  (cond ((and. (null. x) (null. y)) '())
        ((and. (not. (atom x)) (not. (atom y)))
         (cons (list. (car x) (car y))
               (pair. (cdr x) (cdr y))))))

(defun assoc. (x y)
  (cond ((eq (caar y) x) (cadar y))
        ('t (assoc. x (cdr y)))))

(defun eval. (e a)
  (cond
    ((atom e) (assoc. e a))  ; 't must evaluate to an atom (not (()) as I originally tried)
    ((atom (car e))
     (cond
       ((eq (car e) 'quote) (cadr e))
       ((eq (car e) 'atom)  (atom   (eval. (cadr e) a)))
       ((eq (car e) 'eq)    (eq     (eval. (cadr e) a)
                                    (eval. (caddr e) a)))
       ((eq (car e) 'car)   (car    (eval. (cadr e) a)))
       ((eq (car e) 'cdr)   (cdr    (eval. (cadr e) a)))
       ((eq (car e) 'cons)  (cons   (eval. (cadr e) a)
                                    (eval. (caddr e) a)))
       ((eq (car e) 'cond)  (evcon. (cdr e) a))
       ('t (eval. (cons (assoc. (car e) a)
                        (cdr e))
                  a))))
    ((eq (caar e) 'label)
     (eval. (cons (caddar e) (cdr e))
            (cons (list. (cadar e) (car e)) a)))
    ((eq (caar e) 'lambda)
     (eval. (caddar e)
            (append. (pair. (cadar e) (evlis. (cdr e) a))
                     a)))))

(defun evcon. (c a)
  (cond ((eval. (caar c) a)
         (eval. (cadar c) a))
        ('t (evcon. (cdr c) a))))

(defun evlis. (m a)
  (cond ((null. m) '())
        ('t (cons (eval.  (car m) a)
                  (evlis. (cdr m) a)))))

; End jmc.lisp portion

; ======== Tests ========

(defglobal 'mylist '((a b c) (d e f) (g h i)))
(defglobal 'myassoc (list '(x 2) '(y 3) '(z 4) (list 'mylist mylist)))

(caar mylist)  ; => a
(cadr mylist)  ; => (d e f)
(cadar mylist)  ; => b
(caddar mylist)  ; => c

(assoc. 'y myassoc)  ; => 3

(eval. 'x myassoc)  ; => 2

(eval. 'y myassoc)  ; => 3

(list (eval. 'z myassoc) 'should 'be '4)

(list (eval. '(quote hey) myassoc) 'should 'be 'hey)

(list (eval. '(atom (quote hey)) myassoc) 'should 'be 't)

(list (eval. '(atom 'hey) myassoc) 'should 'be 't)

(list (eval. '(eq 'hey 'you) myassoc) 'should 'be '())

(list (eval. '(eq 'hey 'hey) myassoc) 'should 'be 't)

(list (eval. '(car mylist) myassoc) 'should 'be '(a b c))

(list (eval. '(cdr mylist) myassoc) 'should 'be '((d e f) (g h i)))

(list (eval. '(cons (car mylist) (cdr (cdr mylist))) myassoc) 'should 'be '((a b c) (g h i)))

(list 'cond 'test ': (eval. '(cond ((atom mylist) 'fail) ('t 'pass)) myassoc))

(eval. '((lambda (x) x) '3) '())

(eval. '((lambda (x y) (cons 'x (cons 'is (cons x (cons 'while (cons 'y (cons 'is (cons y (cons 'and (cons 'yet (cons 'mylist (cons 'is (cons mylist '()))))))))))))) 'i-am-x 'i-am-y) myassoc)

(defglobal 'global-list-name 'globals)

(defglobal global-list-name '())

(defglobal 'defun.
  '((me formlist env)
     (defglobal
       global-list-name
       (cons (list (car formlist)
		   (list 'lambda
			 (car (cdr formlist))
			 (car (cdr (cdr formlist)))))
	     (eval global-list-name)))))

(defun. foo () 'bar)

(eval. '(foo) globals)

(defun. null (x) (cond (x '()) ('t 't)))

(eval. '(null '()) globals)

(eval. '(null null) globals)

(defun. list (first second) (cons first (cons second '())))

(eval. '(list 'a 'b) globals)

(defglobal 'defglob.
  '((me formlist env)
     (defglobal
       global-list-name
       (cons (list (car formlist)
		   (car (cdr formlist)))
	     (eval global-list-name)))))

(defglob. someglob 'i-am-someglob)

(eval. '((lambda (x y) (list x y)) 'i-am-x 'i-am-y) globals)

(eval. '((lambda (someglob null) (list someglob null)) 'i-am-dummy-someglob 'i-am-dummy-null) globals)

(eval. '((label lab (lambda (x y) (list x y))) 'i-am-x 'i-am-y) globals)

(eval. '((label append (lambda (x y) (cond ((null x) y) ('t (cons (car x) (append (cdr x) y)))))) (list 'x-1 'x-2) (list 'y-1 'y-2)) globals)


; list doesn't take arbitrary arglist due to Lisp Zero not yet implementing something
; like &rest:
; (eval. '((lambda (x y) (list 'x 'is x 'while 'y 'is y 'and 'yet 'someglob 'is someglob)) 'i-am-x 'i-am-y) globals)

; Broken:
; (eval. '((label myassoc ((lambda (x y) (cons 'x (cons 'is (cons x (cons 'while (cons 'y (cons 'is (cons y (cons 'and (cons 'yet (cons 'mylist (cons 'is (cons mylist '()))))))))))))))) 'i-am-x 'i-am-y) myassoc)

; Hmm, cond is undefined when no conditions evaluate true!
; (list (eval. '(cond ((atom mylist) 'fail) ('() 'fail)) myassoc) 'should 'be '())

; This recurses forever:
; (list 'basic 'label 'test ': (eval. '((label lab lab)) myassoc))

; This just prints (basic label test : ()):
; (list 'basic 'label 'test ': (eval. '((label lab (list. 'hey mylist))) myassoc))

;(list 'basic 'label 'test ': (eval. '((label lab ((lambda (a) (list 'lab 'is ': lab 'while 'a 'is ': a)))) 'pass) myassoc))

; ======== Older stuff ========

(defglobal 'applied-noevalofargsname
   '((me formlist env)
     (onelet (('params (eval (car formlist) env)))
	     '(onelet (('argsname (eval (car (cdr formlist)) env)))
		      '(onelet (('body (eval (car (cdr (cdr formlist))) env)))
			       '(cond ((null params) body)
				      ('t (list 'onelet
						(list (list (list 'quote
								  (car params))
							    (list 'car
								  argsname)))
						(list 'quote
						      (applied (cdr params)
							       (list 'cdr
								     argsname)
							       body))))))))))

(defglobal 'applied-whatever
   '((me formlist env)
     (onelet (('params (eval (car formlist) env)))
	     '(onelet (('body (eval (car (cdr formlist)) env)))
		      '(cond ((null params) body)
			     ('t (list 'onelet
				       (list (list (list 'quote (car params))
						   (list 'quote 'whatever)))
				       (list 'quote
					     (applied (cdr params) body)))))))))

(defglobal 'applied-norecurse
   '((me formlist env)
     (cond ((null (eval (car formlist) env)) (eval (car (cdr formlist)) env))
	   ('t (list 'onelet
		     '(('x 'y))
		     (list 'quote
			   (eval (car (cdr formlist)) env)))))))

(defglobal 'defun-new
   '((me formlist env)
     (defglobal
       (car formlist)
       (list '(me formlist env)
	     (applied-whatever (car (cdr formlist))
		      (car (cdr (cdr formlist))))))))

(defglobal 'defun-to-defglobal
   '((me formlist env)
     (defglobal
       (car formlist)
       (list '(me formlist env)
	     (list 'onelet	; TODO: generalize this
		   '(('x 'y))	; TODO: generalize this
		   (list 'quote
			 (car (cdr (cdr formlist)))))))))

(defun try () 'body)
; => (try (me formlist env) (onelet (('x 'y)) (quote 'body)))
(defglobal 'a '((me formlist env) (onelet (('x 'whatever)) (quote 'body))))
(defglobal 'b '((me formlist env) (onelet (((quote x) (quote whatever))) (quote 'body))))
(defglobal 'blah '((me formlist env) (onelet (((quote x) (quote whatever))) (quote (list 'x 'is x)))))
(defglobal 'c '((me formlist env) (onelet (((quote x) (car formlist))) (quote (list 'x 'is x)))))

; (defun name params body)
; => (defglobal 'name '(lambda params body))
(defglobal 'defun-to-lambda
   '((me formlist env)
     (defglobal
       (car formlist)
       '(lambda () 'mybody))))
;       (list (quote
;	      (list 'lambda
;		    (car (cdr formlist))
;		    (car (cdr (cdr formlist)))))))))

; TODO: defun generates lambda, lambda does apply, lambdap -> applicablep, apply() supports
;       alternate form specifying arbitrary applicable function to yield applicable params

; ======== Handy tools ========

; Returns the current evaluation environment (bindings)
(defglobal 'curenv
  '((me formlist env)
     env))

; ======== Primitive/incomplete versions of some functions ========

(defglobal 'list-exactlytwoargs
  '((me formlist env)
    (cons
     (eval (car formlist) env)
     (cons (eval (car (cdr formlist)) env) '()))))

(defglobal 'list-wrongnamespacep
  '((me formlist env)
    (cond ((null formlist) '())
	  ('t (cons (eval (car formlist) env)
		    (eval (cons 'me
				(cdr formlist))))))))

(defglobal 'list-never-quite-worked
  '((me formlist env)
    (cond ((null formlist) '())
	  ('t (cons (eval (car formlist) env)
		    (eval (cons (eval 'me) (cdr formlist)) env))))))

; (cons 'quote (cons (eval me) '()))
;		    ((eval me) (cdr formlist)))))))

(defglobal 'list-wrong
  '((me formlist env)
    (cond ((null formlist) '())
	  ('t (cons (eval (car formlist) env)
		    (eval (cons 'me
				(cdr formlist))))))))

(defglobal 'defun-newnil
   '((me formlist env)
     (defglobal
       'newnil
       '((me formlist env)
	 ()))))

(defglobal 'defun-newnull
   '((me formlist env)
     (defglobal
       'newnull
       '((me formlist env)
	 (onelet (('arg (eval (car formlist) env)))
		 '(cond ((atom arg) (eq arg '()))
			('t '())))))))

(defglobal 'defun-ignoreparams
   '((me formlist env)
     (defglobal
       (car formlist)
       (cons '(me formlist env)
	     (cons (car (cdr (cdr formlist)))
		   '())))))

(defglobal 'defun-bodynotsufficientlyquoted
   '((me formlist env)
     (defglobal
       (car formlist)
       (cons '(me formlist env)
	     (cons (cons 'onelet
			 (cons '(('x 'y))
			       (cons (car (cdr (cdr formlist)))
				     '())))
		   '())))))

(defglobal 'defun-nolist
   '((me formlist env)
     (defglobal
       (car formlist)
       (cons '(me formlist env)
	     (cons (cons 'onelet
			 (cons '(('x 'y))
			       (cons (cons 'quote
					   (cons (car (cdr (cdr formlist)))
						 '()))
				     '())))
		   '())))))

; ======== Other notes ========

; (defglobal 'showme '((formlist env) (cons formlist env)))
; (showme a b c)
; => ((a b c) (showme (formlist env) (cons formlist env)) (undefglobal . *COMPILED*) ...
; (eval 'c (cons (cons 'c 'd) (defglobal)))
; => d
;
; (defun sym (params) body)
;
; => "compiled" form of sym
;
; evaluate the params in the context of the original environment
; push bindings for the list of evaluated params onto a new environment stack
; evaluate the body using the new environment
;
; 
; (eval (car formlist) env)
;
; (defun sym () body)
;
; => (defglobal 'sym '((formlist env) body))
;
