#lang racket/base

(require racket/match)
(require (only-in racket/string string-split))

(provide string->ngrams+frequencies)

(define (string->ngrams+frequencies input [ngram-size 3] [ngram-normalizer string-downcase])
  (define max-i (- (string-length input) ngram-size))
  (let loop ((table (hash))
	     (i 0))
    (if (> i max-i)
	(hash->list table)
	(match (string-split (substring input i (+ i ngram-size)))
	  [(list candidate) ;; no whitespace in the candidate
	   (define ngram (ngram-normalizer candidate))
	   (if (= (string-length ngram) ngram-size)
	       (loop (hash-set table ngram (+ (hash-ref table ngram 0) 1)) (+ i 1))
	       (loop table (+ i 1)))]
	  [_
	   (loop table
		 (+ i 1))]))))
