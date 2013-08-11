#lang racket/base

(require racket/match)
(require racket/fasl)
(require (only-in racket/string string-trim string-split string-join))
(require (only-in racket/list take-right drop-right))
(require (only-in srfi/13 string-contains))

(require "inifile.rkt")
(require "ngrams.rkt")
(require "mail-message.rkt")

(module+ main
  (require racket/pretty)

  (printf "Loading database...\n")
  (flush-output)
  (define index (time (call-with-input-file "INDEX" fasl->s-exp)))
  (define messages
    (let ((m (time (call-with-input-file "MESSAGES" fasl->s-exp))))
      (if (list? m)
	  (list->vector m)
	  m)))
  (printf "Loaded.\n")
  (flush-output)

  (define (eval-query q)
    (define ngrams (map car (string->ngrams+frequencies q)))
    (define counts
      (for/fold ([counts (hash)])
	  ([matches (map (lambda (ngram) (hash-ref index ngram '())) ngrams)])
	(foldl (lambda (match counts)
		 (hash-set counts (car match) (+ (cdr match) (hash-ref counts (car match) 0))))
	       counts
	       matches)))
    (define raw-articles (map car (sort (hash->list counts) > #:key cdr)))
    (printf "~a raw articles matched.\n" (length raw-articles))
    (define words (string-split (string-downcase q)))
    (define (relevant? i)
      (define t (string-downcase (mail-message-searchable-text (vector-ref messages i))))
      (andmap (lambda (w) (string-contains t w)) words))
    (define final-articles (filter relevant? raw-articles))
    (printf "~a articles remain after relevancy filter.\n" (length final-articles))
    final-articles)

  (define (print-query-results r)
    (pretty-print (map (lambda (i)
			 (map (lambda (h) (mail-message-header h (vector-ref messages i)))
			      '(subject from to date)))
		       r))
    (printf "============================================================\n")
    (printf "============================================================\n")
    (printf "============================================================\n"))

  (let loop ()
    (display "Enter a query: ")
    (define query (read-line))
    (if (eof-object? query)
	(printf "Goodbye!\n")
	(begin (print-query-results (time (eval-query query)))
	       (loop)))))
