#! /usr/bin/env chibi-scheme

(import (scheme base) (scheme write) (scheme process-context))

(define id64-char-set
  " _0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz")

(define (string-char-offset string char)
  (let loop ((i 0))
    (cond ((>= i (string-length string)) #f)
          ((equal? char (string-ref string i)) i)
          (else (loop (+ i 1))))))

(define (char->id64-char char)
  (or (string-char-offset id64-char-set char)
      (error "Character cannot be represented in id64")))

(define (id64-char->char id64-char)
  (string-ref id64-char-set id64-char))

(define (string->id64-string string)
  (let loop ((mul 1) (ps 0) (cs (string->list string)))
    (if (null? cs)
        ps
        (loop (* mul 64)
              (+ ps (* mul (char->id64-char (car cs))))
              (cdr cs)))))

(define (id64-string->string id64-string)
  (let loop ((ps id64-string) (cs '()))
    (if (<= ps 0)
        (list->string (reverse cs))
        (let-values (((ps id64-char) (truncate/ ps 64)))
          (loop ps (cons (id64-char->char id64-char) cs))))))

(define (display-id64 string)
  (display "#define ID64_")
  (display string)
  (display " 0x")
  (display (number->string (string->id64-string string) 16))
  (newline))

(for-each display-id64 (cdr (command-line)))
