#lang racket/base

(require racket/match)
(require (only-in racket/string string-trim string-split string-join))
(require (only-in racket/list take-right drop-right))

(provide (struct-out mail-message)
	 analyze-mailbox-item
	 string->mail-message
	 analyze-mime
	 string->lines
	 mail-message-header
	 mail-message-content-type
	 text-body
	 first-non-compound-body
	 mail-message-searchable-text)

(struct mail-message (headers body) #:prefab)

(define (analyze-mailbox-item lines)
  (let split-headers ((header "")
		      (headers '())
		      (lines lines))
    (define (extend-header h) (string-append header h))
    (define (finish-header) (if (string=? header "")
				headers
				(let ((pieces (string-split header ":")))
				  (cons (list (string->symbol (string-downcase (car pieces)))
					      (car pieces)
					      (string-trim (string-join (cdr pieces) ":")))
					headers))))
    (match lines
      ['() (mail-message (reverse (finish-header)) "")] ;; shouldn't happen
      [(cons "" rest) (mail-message (reverse (finish-header)) (string-join rest "\r\n"))]
      [(cons h rest)
       (if (and (> (string-length h) 1) (char-whitespace? (string-ref h 0)))
	   (split-headers (extend-header h) headers rest)
	   (split-headers h (finish-header) rest))])))

(define (string->lines s)
  (define p (open-input-string s))
  (let loop ((acc '()))
    (define line (read-line p 'any))
    (if (eof-object? line)
	(reverse acc)
	(loop (cons line acc)))))

(define (mail-message-header header message)
  (match (assq header (mail-message-headers message))
    [#f #f]
    [(list _ _ t) t]))

(define (mail-message-content-type message)
  (mail-message-header 'content-type message))

(define (analyze-mime message)
  (match (mail-message-content-type message)
    [#f message]
    [t
     (match (regexp-match "^(multipart/[^;]*).*; +boundary=\"?([^\";]+)" t)
       [#f message]
       [(list _ type boundary)
	(define mid-boundary (string-append "\r\n--"boundary"\r\n"))
	(define end-boundary (string-append "\r\n--"boundary"--"))
	(define all-pieces (string-split (string-append "\r\n" (mail-message-body message))
					 ;; ^ some buggy mailers have the initial boundary sans
					 ;; leading \r\n
					 mid-boundary
					 #:trim? #f))
	(when (< (length all-pieces) 2)
	  (local-require racket/pretty)
	  (pretty-print `((content-type ,t)
			  (mid-boundary ,mid-boundary)
			  (body ,(mail-message-body message))
			  (all-pieces ,all-pieces)))
	  (flush-output))
	(define final-piece (car (take-right all-pieces 1)))
	(define pieces (append (drop-right (cdr all-pieces) 1)
			       (list (car (string-split final-piece end-boundary #:trim? #f)))))
	(mail-message (mail-message-headers message)
		      (cons type (map string->mail-message pieces)))])]))

(define (string->mail-message s)
  (analyze-mime (analyze-mailbox-item (string->lines s))))

(define (text-body message)
  (if (regexp-match "^text/" (or (mail-message-content-type message) "text/plain"))
      (mail-message-body message)
      (match (mail-message-body message)
	[(cons type subparts)
	 (ormap text-body subparts)]
	[_ #f])))

(define (first-non-compound-body message)
  (if (string? (mail-message-body message))
      (mail-message-body message)
      (ormap first-non-compound-body (cdr (mail-message-body message)))))

(define (mail-message-searchable-text message)
  (string-append (or (mail-message-header 'from message) "") "\n"
		 (or (mail-message-header 'subject message) "") "\n"
		 (or (text-body message) "")))
