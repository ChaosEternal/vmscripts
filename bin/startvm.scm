#!/usr/bin/guile
!#
(use-modules 
 (scsh scsh)
 (scsh syntax)
 (scsh run-extras)
 (srfi srfi-8)
 (rnrs lists)
 (ice-9 match)
 (ice-9 getopt-long)
 (ice-9 pretty-print)
 (srfi srfi-11))
(define (kvm xvmdef)
   (fold-right
    (lambda (x y)
      (let ((rdopt (lambda (v)
		     (string-join 
		      (map (lambda (x)
			     (match x
			       ((i)
				(format #f "~a" i))
			       ((i j)
				(format #f "~a=~a" i j))))
			   v)
		      ","))))
	(match x
	  (('tags v)
	   y)
	  ((p) 
	   (cons (format #f "-~a" p) y))
	  ((p v)
	   (cons (format #f "-~a" p)
		 (match v
		   ((? list? xx)
		    (cons (rdopt xx) y))
		   (_
		    (cons (format #f "~a" v) y))))))))
    '()
    xvmdef))
(define (get-vm-def file-name)
  (match (port->sexp-list (open-input-file file-name))
    [(_ *** ('define 'vmdef ('quote x) ))  x]
    [(('vmdef x) . _)  x]
    [(('vmdef . x) . _)  x]
    [_ #f]));;=> #<unspecified>
(let* ((cmd-line (command-line))
       (working-dir (dirname (car cmd-line)))
       (file-name (cadr cmd-line))
       (vmdef (get-vm-def file-name))
       (is-disk (match vmdef
		  [(('machine (('type 'diskonly) _)) . _) #t]
		  [_ #f])))
  (with-cwd 
   working-dir
   (let ()
     (run (qemu-system-x86_64 ,@(kvm vmdef)))
     (run (ssvncviewer ./vncsock))
     )
   ))
