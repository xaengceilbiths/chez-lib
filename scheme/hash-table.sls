#!chezscheme
;; Implementation of hash-table by Peter Lane, 2017
;; -- attempts to build directly on (srfi 69), which is well supported by implementations
;; -- ignores deprecated functions and forbids implementation-specific optional args
;; -- hashtables assumed to be mutable
;; -- as with other implementations of this library, hash-table-set! does not check the 
;;    type of key against the original comparator
;; Both these last could be done with an internal record to hold the hash-table, comparator
;; and mutable flag

;; Parts marked Reference Implementation:
;;; Copyright 2015 William D Clinger.
;;;
;;; Permission to copy this software, in whole or in part, to use this
;;; software for any lawful purpose, and to redistribute this software
;;; is granted subject to the restriction that all copies made of this
;;; software must include this copyright and permission notice in full.
;;;
;;; I also request that you send me a copy of any improvements that you
;;; make to this software so that they may be incorporated within it to
;;; the benefit of the Scheme community.
;;;

(library
  (scheme hash-table)
  (export 
    make-hash-table
    hash-table
    hash-table-unfold
    alist->hash-table 

    hash-table?
    hash-table-contains?
    hash-table-empty?
    hash-table=?
    hash-table-mutable? 

    hash-table-ref
    hash-table-ref/default 

    hash-table-set!
    hash-table-delete!
    hash-table-intern!
    hash-table-update!
    hash-table-update!/default
    hash-table-pop!
    hash-table-clear! 

    hash-table-size
    hash-table-keys
    hash-table-values
    hash-table-entries
    hash-table-find
    hash-table-count

    hash-table-map
    hash-table-for-each
    hash-table-map!
    hash-table-map->list
    hash-table-fold
    hash-table-prune!

    hash-table-copy
    hash-table-empty-copy
    hash-table->alist 

    hash-table-union!
    hash-table-intersection!
    hash-table-difference!
    hash-table-xor!
    )
  (import (scheme base)
          (scheme case-lambda)
          (scheme comparator)
          #;(only (srfi 27) random-integer)
          (only (surfage s27 random-bits) random-integer)
          #;(prefix (srfi 69) s69:) ; uses srfi 69 functions, with prefix s69:
          (prefix (surfage s69 basic-hash-tables) s69:) ; uses srfi 69 functions, with prefix s69:
          (only (scheme list) every))

  (begin

    ;; **** Constructors ****

    (define (make-hash-table comparator)
      (s69:make-hash-table 
        (comparator-equality-predicate comparator) 
        ; NB: srfi 69 hash-function takes two arguments, 
        ; (scheme comparator) hash-function takes only one
        ; -- . args used as hash-table copying mixes the source of comparators
        (lambda (obj . args) ((comparator-hash-function comparator) obj))))

    ;; Uses args as pair-wise key-value items to initialise hash-table
    (define (hash-table comparator . args)
      (let ((ht (make-hash-table comparator)))
        (do ((kvs args (cddr kvs)))
          ((< (length kvs) 2) ht)
          (hash-table-set! ht (car kvs) (cadr kvs)))))

    ;; Reference implementation
    (define (hash-table-unfold stop? mapper successor seed comparator)
      (let ((ht (make-hash-table comparator)))
        (let loop ((seed seed))
          (if (stop? seed)
            ht
            (call-with-values
              (lambda () (mapper seed))
              (lambda (key val)
                (hash-table-set! ht key val)
                (loop (successor seed))))))))

    (define (alist->hash-table alist comparator)
      (let ((ht (make-hash-table comparator)))
        (for-each (lambda (kv) (hash-table-set! ht (car kv) (cdr kv)))
                  alist)
        ht))

    ;; **** Predicates ****

    (define hash-table? s69:hash-table?)

    (define hash-table-contains? s69:hash-table-exists?)

    (define (hash-table-empty? ht)
      (zero? (s69:hash-table-size ht)))

    ;; From reference implementation, but uses list/every and own ht functions
    (define (hash-table=? value-comparator ht1 ht2)
      (let ((val=? (comparator-equality-predicate value-comparator))
            (n1 (hash-table-size ht1))
            (n2 (hash-table-size ht2)))
        (and (= n1 n2)
             (every (lambda (key val1)
                      (and (hash-table-contains? ht2 key)
                           (val=? val1
                                  (hash-table-ref ht2 key 'ignored))))
                    (hash-table-keys ht1)
                    (hash-table-values ht1))
             (every (lambda (key val2)
                      (and (hash-table-contains? ht1 key)
                           (val=? val2
                                  (hash-table-ref ht1 key 'ignored))))
                    (hash-table-keys ht2)
                    (hash-table-values ht2)))))

    (define (hash-table-mutable? ht)
      #t) ; all hash-tables are mutable

    ;; **** Accessors ****

    (define hash-table-ref
      (case-lambda 
        ((ht key)
         (s69:hash-table-ref ht key))
        ((ht key failure)
         (s69:hash-table-ref ht key failure))
        ((ht key failure success)
         (if (hash-table-contains? ht key)
           (success (s69:hash-table-ref ht key))
           (failure)))))

    (define hash-table-ref/default s69:hash-table-ref/default)

    ;; **** Mutators ****

    ;; -- specification requires we check keys meet type check of comparator
    ;;    but no-one does this
    (define (hash-table-set! ht . args)
      (do ((kvs args (cddr kvs)))
        ((< (length kvs) 2) )
        (s69:hash-table-set! ht (car kvs) (cadr kvs))))

    (define (hash-table-delete! ht . keys)
      (let ((cnt 0))
        (for-each (lambda (k) 
                    (when (hash-table-contains? ht k)
                      (set! cnt (+ 1 cnt))
                      (s69:hash-table-delete! ht k)))
                  keys)
        cnt))

    ;; From reference implementation
    (define (hash-table-intern! ht key failure)
      (if (hash-table-contains? ht key)
        (hash-table-ref ht key)
        (let ((val (failure)))
          (hash-table-set! ht key val)
          val)))

    (define hash-table-update! 
      (case-lambda
        ((ht key function)
         (hash-table-set! ht key (function (hash-table-ref ht key))))
        ((ht key function thunk)
         (hash-table-set! ht key
                          (function (hash-table-ref ht key thunk))))))

    (define (hash-table-update!/default ht key function default)
      (hash-table-update! ht key function (lambda () default)))

    ;; Returns an arbitrary value - requires random-integer from srfi 27
    (define (hash-table-pop! ht)
      (unless (zero? (hash-table-size ht)) ; ignore if empty
        (let* ((keys (hash-table-keys ht))
               (key (list-ref (hash-table-keys ht) (random-integer (length keys))))
               (val (hash-table-ref ht key)))
          (hash-table-delete! ht key)
          (values key val))))

    (define (hash-table-clear! ht)
      (for-each (lambda (key)
                  (hash-table-delete! ht key))
                (hash-table-keys ht)))

    ;; **** The Whole Hash-Table ****

    (define hash-table-size s69:hash-table-size)

    (define hash-table-keys s69:hash-table-keys)

    (define hash-table-values s69:hash-table-values)

    (define (hash-table-entries ht)
      (values (hash-table-keys ht) 
              (hash-table-values ht)))

    (define (hash-table-find proc ht failure)
      (let loop ((keys (hash-table-keys ht)))
        (if (null? keys)
          (failure)
          (let ((res (proc (car keys) (hash-table-ref ht (car keys)))))
            (if res
              res
              (loop (cdr keys)))))))

    (define (hash-table-count pred ht)
      (do ((keys (hash-table-keys ht) (cdr keys))
           (cnt 0 (if (pred (car keys) (hash-table-ref ht (car keys)))
                    (+ 1 cnt)
                    cnt)))
        ((null? keys) cnt)))

    ;; **** Mapping and Folding ****

    ;; From reference implementation
    (define (hash-table-map proc comparator ht)
      (let ((new-ht (make-hash-table comparator)))
        (hash-table-for-each
          (lambda (k v) 
            (hash-table-set! new-ht k (proc v)))
          ht)
        new-ht))

    (define (hash-table-for-each proc ht)
      (s69:hash-table-walk ht proc))

    ;; From reference implementation
    (define (hash-table-map! proc ht)
      (hash-table-for-each
        (lambda (k v) (hash-table-set! ht k (proc k v)))
        ht))

    (define (hash-table-map->list proc ht)
      (map proc (hash-table-keys ht) (hash-table-values ht)))

    (define (hash-table-fold proc seed ht)
      (s69:hash-table-fold ht proc seed))

    ;; From reference implementation
    (define (hash-table-prune! proc ht)
      (hash-table-for-each
        (lambda (k v)
          (when (proc k v) (hash-table-delete! ht k)))
        ht))

    ;; **** Copying and Conversion ****

    (define hash-table-copy
      (case-lambda
        ((ht)
         (hash-table-copy ht #t))
        ((ht mutable?)  ; mutable? flag is ignored (always #t)
         (s69:hash-table-copy ht))))

    (define (hash-table-empty-copy ht)
      (let ((new-ht (hash-table-copy ht)))
        (hash-table-clear! new-ht)
        new-ht))

    (define (hash-table->alist ht)
      (hash-table-map->list cons ht))

    ;; **** Hash Tables as Sets ****

    ;; From reference implementation
    (define (hash-table-union! ht1 ht2)
      (hash-table-for-each
        (lambda (k v)
          (unless (hash-table-contains? ht1 k)
            (hash-table-set! ht1 k v)))
        ht2)
      ht1)

    ;; From reference implementation
    (define (hash-table-intersection! ht1 ht2)
      (hash-table-for-each
        (lambda (k v)
          (unless (hash-table-contains? ht2 k)
            (hash-table-delete! ht1 k)))
        ht1)
      ht1)

    ;; From reference implementation
    (define (hash-table-difference! ht1 ht2)
      (hash-table-for-each
        (lambda (k v)
          (when (hash-table-contains? ht2 k)
            (hash-table-delete! ht1 k)))
        ht1)
      ht1)

    ;; From reference implementation
    (define (hash-table-xor! ht1 ht2)
      (hash-table-for-each
        (lambda (k v) 
          (if (hash-table-contains? ht1 k)
            (hash-table-delete! ht1 k)
            (hash-table-set! ht1 k v)))
        ht2)
      ht1)

    ))

