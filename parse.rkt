#! /usr/bin/env racket

#lang racket

(require racket/match)

;; Does Racket come with `butlast` or `subseq`?
(define (split-last xs)
  (if (null? xs)
      (values #f '())
      (let loop ((xs xs) (others '()))
        (if (null? (cdr xs))
            (values (car xs) (reverse others))
            (loop (cdr xs) (cons (car xs) others))))))

;;

(struct c-file (functions flagsets)
  #:transparent)

(define (split-arg string-arg)
  ;; Split into words, but so that each group of asterisks is its own
  ;; word. Asterisks separated only by whitespace are put together in
  ;; the same group and the whitespace between them is removed.
  (map (lambda (token) (string-replace token #px"\\s+" ""))
       (regexp-match* #px"\\*[\\s\\*]*\\*|\\*|\\S+" string-arg)))

(define (parse-arg string-arg)
  (let-values (((argname argtype) (split-last (split-arg string-arg))))
    (list argname argtype)))

(define (parse-args string-args)
  (map parse-arg (string-split string-args ",")))

(define (parse-doc-comment whole)
  (list 'doc whole))

(define (parse-hot-comment whole)
  (match (string-split whole)
    ((list "flagset:" set-name)
     (list 'flagset set-name))
    ((list "flag:" flag-name)
     (list 'flag flag-name))))

(define (parse-things input)
  (map (lambda (x)
         (match x
           ((list whole fun-name fun-args comment-marker comment-text)
            (cond ((and fun-name fun-args)
                   (list 'fun fun-name (parse-args fun-args)))
                  ((and comment-marker comment-text)
                   (case comment-marker
                     (("///") (parse-doc-comment comment-text))
                     (("//!") (parse-hot-comment comment-text))))))))
       (regexp-match*
        #px"(?m:^(?:static )?struct err \\*([A-Za-z0-9_]+)\\(([^)]*)\\)|^(//[/!])\\s*(.*?)\\s*$)"
        input #:match-select values)))

(define (append-if items new-item)
  (if new-item (append items (list new-item)) items))

(define (parse input)
  (let loop ((things (parse-things input))
             (functions '())
             (flagsets '())
             (flagset #f)
             (docstring #f))
    (match things
      ((list-rest (list 'doc doc) things)
       (loop things
             functions
             flagsets
             flagset
             (string-append (or docstring "") doc)))
      ((list-rest (list 'flagset name) things)
       (loop things
             functions
             (append-if flagsets flagset)
             (list name docstring)
             #f))
      ((list-rest (list 'flag name) things)
       (unless flagset (error "Flag before flagset"))
       (loop things
             functions
             flagsets
             (append flagset (list (list name docstring)))
             #f))
      ((list-rest (list 'fun name args) things)
       (loop things
             (append functions (list (list* name docstring args)))
             (append-if flagsets flagset)
             #f
             #f))
      ((list)
       (c-file functions (append-if flagsets flagset))))))

(writeln (parse (port->string (current-input-port))))
