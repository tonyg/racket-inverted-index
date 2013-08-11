#lang racket/base

(require racket/match)
(require racket/fasl)

(require "inifile.rkt")
(require "ngrams.rkt")
(require "mail-message.rkt")

(define (first-mail-profile-path [profile-dir "/Users/tonyg/Library/Thunderbird"])
  (let search ((profiles (call-with-input-file (build-path profile-dir "profiles.ini")
			   read-ini-file)))
    (match profiles
      ['() #f]
      [(cons (cons "General" _) rest) (search rest)]
      [(cons (cons _ fields) _)
       (define is-relative? (positive? (string->number
					(cadr (or (assoc "IsRelative" fields) '("0"))))))
       (define partial-path (cadr (assoc "Path" fields)))
       (define path (if is-relative?
			(build-path profile-dir partial-path)
			(build-path "/" partial-path)))
       path])))

(define (slurp-mbox [p (current-input-port)])
  (let loop ((acc '())
	     (items '()))
    (define (finish-item)
      (printf "Read ~a items; most recent item is ~a lines long\n"
      	      (+ (length items) 1)
      	      (length acc))
      (if (null? acc)
	  items
	  (cons (analyze-mime (analyze-mailbox-item (reverse acc))) items)))
    (define line (read-line p 'any))
    (cond
     [(eof-object? line) (reverse (finish-item))]
     [(and (>= (string-length line) 5)
	   (string=? (substring line 0 5) "From "))
      (loop '() (finish-item))]
     [else
      (loop (cons line acc) items)])))

(define (message->ngrams+frequencies message)
  (string->ngrams+frequencies (mail-message-searchable-text message)))

(define (build-index messages)
  (let loop ((counter 0)
	     (index (hash))
	     (messages messages))
    (printf "Indexed ~a messages; ~a ngrams so far...\n" counter (hash-count index))
    (flush-output)
    (match messages
      ['() index]
      [(cons message rest)
       (loop (+ counter 1)
	     (for/fold ((index index))
		 ([item (message->ngrams+frequencies message)])
	       (match-define (cons word frequency) item)
	       (hash-set index word (cons (cons counter frequency) (hash-ref index word '()))))
	     rest)])))

(module+ main
  (require racket/pretty)
  (require (only-in racket/string string-join))

  (define messages
    (call-with-input-file
  	(build-path (first-mail-profile-path) "ImapMail/imap.ccs.neu.edu/Archives.sbd/2012")
      slurp-mbox))
  ;; (define messages
  ;;   (list
  ;;    (string->mail-message
  ;;     (string-join (list "Subject: test this"
  ;; 			 ""
  ;; 			 "Hello world")
  ;; 		   "\r\n"))))

  (define index (build-index messages))

  (printf "Press enter to write out the database files.\n")
  (flush-output)
  (read-line)

  (call-with-output-file "MESSAGES"
    #:exists 'replace
    (lambda (p) (s-exp->fasl (list->vector messages) p)))

  (call-with-output-file "INDEX"
    #:exists 'replace
    (lambda (p) (s-exp->fasl index p))))
