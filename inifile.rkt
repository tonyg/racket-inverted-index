#lang racket/base

(require (only-in racket/string string-trim string-split string-join))

(provide read-ini-file)

(define (read-ini-file [p (current-input-port)])
  (let loop ((name #f)
	     (acc '())
	     (sections '()))
    (define (finish-section)
      (if name
	  (cons (cons name (reverse acc)) sections)
	  sections))
    (define line (read-line p 'any))
    (if (eof-object? line)
	(reverse (finish-section))
	(let ((line (string-trim line)))
	  (cond
	   [(equal? line "") (loop name acc sections)]
	   [(eqv? (string-ref line 0) #\[) (loop (substring line 1 (- (string-length line) 1))
						 '()
						 (finish-section))]
	   [else
	    (define pieces (string-split line "="))
	    (loop name
		  (cons (list (car pieces) (string-join (cdr pieces) "=")) acc)
		  sections)])))))
