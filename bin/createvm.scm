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



(define guile-magic "#!/usr/bin/guile
!#
")
(define (modulesuse)
  "The trunk used to load necessary module in startvm.scm"
  '(use-modules  (scsh scsh)
		 (scsh syntax)
		 (scsh run-extras)
		 (srfi srfi-8)
		 (srfi srfi-11)
		 (ice-9 match)
		 (rnrs lists)
		 (ice-9 pretty-print))
  )
(define (xkvmf)
  "The trunk used to define procedure of kvm in generated startvm.scm"
  '(define (kvm xvmdef)
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
      xvmdef
      )))

(define (xvmdef hda-name macaddr memsize vmisopath vmuuid vmname diskonly)
  "the trunk used to define vm parameters in generated startvm.scm"
  (let ((nicname (string-append "hst-nic-" vmname))
	(cdrom (if vmisopath `((drive ((file ,vmisopath)
					(index 1)
					(if ide)
					(media cdrom))))
		   '()
		   ))
	(pc (if diskonly 'diskonly 'pc)))
   `(define vmdef
      '((M ,pc)
	(cpu kvm64)
	(name ,vmname)
	(uuid ,vmuuid)
	(daemonize)
	(pidfile "kvm.pid")
	(drive ((file ,hda-name)
		(index 0)
		(if ide)
		(media disk)))
	(net ((tap)
	      ;; (ifname ,nicname)
	      ;; (script no)
	      (helper /usr/lib/qemu/qemu-bridge-helper)
	      ))
	(net ((nic)
	      (name "nic1")
	      (macaddr ,macaddr)))
	(m ,memsize)
	(vnc "unix:vncsock,server")
	(chardev (
		  (socket)
		  (id mnt1)
		  (nowait)
		  (path mntrsock)
		  (server)))
	(mon ((chardev mnt1)
	      (mode readline)))
	(chardev (
		  (socket)
		  (id mnt2)
		  (nowait)
		  (path qmpsock)
		  (server)))
	(mon ((chardev mnt2)
	      (mode control)))
	(chardev (
		  (socket)
		  (id mnt3)
		  (nowait)
		  (path serialsock)
		  (server)))
	(serial "chardev:mnt3"
	      )

	(usb)
	(usbdevice tablet)
	,@cdrom)))
  )


(define (optbuilder* ol)
  "usage: (optbuilder* '((info) (text \"hello world\")))"
  (let ((o->s 
	 (lambda (x)
	   (let* ((s (stringify (car x)))
		  (l? (> (string-length s) 1))
		  (p (if l?
			 "--"
			 "-"))
		  (m (if l?
			 "="
			 ""))
		  (ls (if (null? (cdr x))
			  (list p s)
			  (list p s m (stringify (cadr x))))))
	     (string-join ls "")))))
    (map o->s ol)))


(define (zenity zdef)
  `(zenity ,@(optbuilder* zdef)))

(define (createvm repo vmname uuid memsize disksize disktype diskonly vmisopath)

  
  (random 255 (random-state-from-platform ))

  (let ((repo repo)
	(macaddr (string-join 
		  (cons "00" 
			(map 
			 (lambda (x) 
			   (format #f "~X" (random 255 ))) 
			 (iota 5))) 
		  ":")))
    (&& (test "!" -d ,repo)
	(begin (run (mkdir "-p" repo))
	       (run (mkdir ,(string-join (list repo "vde1") "/")))))
    

    (with-cwd 
     repo
     (let-values 
	 (
	  ((vmname memsize disksize disktype)
	   (values vmname memsize disksize disktype))
	  ((vmisopath)
	   (values vmisopath))
	  )
       (let* ((vmpath (string-append repo "/" vmname))
	      (script-path (string-append vmpath "/startvm.scm"))
	      (hda-name "hda.img")
	      (hda-path (string-append vmpath "/" hda-name))
					;(script-port (open-file script-path "w"))
	      (vmuuid (if uuid uuid (car (run/strings (uuidgen)))))
	      )
	 (if (> (run (mkdir ,vmpath)) 0)
	     (throw `repo-exists))
	 (run (qemu-img create -f ,disktype ,hda-path ,disksize))
	 (with-output-to-file 
	     script-path
	   (lambda () 
	     (display guile-magic)
	     (pretty-print (modulesuse))
	     (pretty-print (xkvmf))
	     (pretty-print (xvmdef hda-name macaddr memsize vmisopath vmuuid vmname diskonly))
	     (if (not diskonly)
		 (pretty-print '(with-cwd 
				 (dirname 
				  (car (command-line)))
				 (let ()
				   (run (qemu-system-x86_64 ,@(kvm vmdef)))
				   (run (ssvncviewer ./vncsock))
				   )
				 )
			       ))))
					;(list repo vmname memsize disksize disktype vmisopath)
	 ))))
  )

(define (parse-args)
  (define option-spec 
    '((version (value #f))
      (help (value #f))
      (repo (single-char #\r) (value #t))
      (name (single-char #\n) (value #t))
      (uuid (single-char #\U) (value #t))
      (memory-size (single-char #\m) (value #t))
      (disk-only (value #f))
      (disk-size (single-char #\d) (value #t))
      (disk-type (value #t))
      (iso-path (value #t))
      ))

  (define options (getopt-long (command-line) option-spec))
  (let  ([help-wanted (option-ref options 'help #f)]
  	[version-wanted (option-ref options 'version #f)]
  	[repo (option-ref options 'repo (string-append (getenv "HOME") "/kVMs"))]
  	[name (option-ref options 'name #f)]
	[uuid (option-ref options 'uuid #f)]
	[disk-only (option-ref options 'disk-only #f)]
  	[memory-size (option-ref options 'memory-size "1024M")]
  	[disk-size (option-ref options 'disk-size "8192G")]
	[disk-type (option-ref options 'disk-type "qcow2")]
	[iso-path (option-ref options 'iso-path #f)]
  	 )
    (begin
      (if help-wanted (begin (display "createvm --repo --name --memory-size --disk-size
createvm --repo --name --disk-only --disk-size
") (exit)))
      (if version-wanted (begin (display "version 0.0.1\n") (exit) ))
      (display repo )
      (values repo (if uuid (string-append "vm-" uuid) name) uuid memory-size disk-size disk-type disk-only iso-path))
    )
)

(define (main)
  (let-values (((repo name uuid memory-size disk-size disk-type disk-only vmisopath)
		 (parse-args)))
    (createvm repo name uuid memory-size disk-size disk-type disk-only vmisopath) 
    )

  )

(main)
