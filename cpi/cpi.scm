#!/usr/bin/guile
!#

(use-modules (scsh scsh)
	     (scsh syntax)
	     (scsh glob)
	     (json)
	     (scsh run-extras)
	     (srfi srfi-8)
	     (rnrs lists)
	     (ice-9 match)
	     (ice-9 getopt-long)
	     (ice-9 pretty-print)
	     (srfi srfi-11))

(define REPO (string-append (getenv "HOME") "kVMs"))

(define (create-stemcell path options)
  (define stemcell-uuid
    (car (run/strings (uuidgen))))
  (begin 
    (run (createvm.scm --uuid ,@uuid --disk-only))
    (let* 
	([vm-arg (car  (run/sexp (lsvm --uuid ,@uuid)))]
	 [vm-name (match vm-arg [(_ *** ('name name)) name])]
	 [vm-uuid (match vm-arg [(_ *** ('uuid uuid)) uuid])])
      (run (cp ,path ,(string-join (list REPO vm-name "hda.img") "/")))))
  stemcell-uuid)


(define (delete-stemcell stemcell-uuid)
  (define stemcell-defs 
    (car
     (run/sexp (lsvm --uuid ,stemcell-uuid))))
  (let ([vm-name (match stemcell-defs [(_ *** ('name name)) name])])
    (run (rm "-fr" ,(string-join (list REPO vm-name) "/")))))

(define (apply-netwok repo networks vmmac agent-id)
  (let* ([net0 (car  (hash-map->list (lambda (x y) y)))]
	 [ipaddr (hash-ref net0 "ip")])
    (begin 
      (run (echo ,@(string-join (list vmmac ipaddr ,agent-id) "," )) (>> (string-append REPO "/dhcp-host.lst")))
      (run (echo ,ipadd ,agent_id) (>> (string-append REPO "/dhcp-host.lst"))))))

(define (create-vm agent-id stemcell-id resource-pool networks)
  (begin 
    (run (createvm --name ,agent-id))
    (define vm-arg (car (run/sexp (lsvm --name ,agent-id))))
    (let* ([vm-name (match vm-arg [(_ *** ('name name)) name])]
	   [vm-uuid (match vm-arg [(_ *** ('uuid uuid)) uuid])]
	   [vm-mac (match vm-arg [(_ *** ('net ("nic1" mac))) mac])]
	   [ip-count (string->number (car (run/list (wc -l (string-append REPO "/dhcp-host.lst")))))])
      
      ))
  
  )
