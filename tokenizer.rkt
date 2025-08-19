#lang racket/base

(require json
         racket/port
         racket/dict
         racket/string)

#|

Basic Llama-type (BPE) tokenizer. The general approach is described
in, for example:

  Vilém Zouhar et al. “A formal perspective on byte-pair
  encoding”. In: arXiv preprint arXiv:2306.16837 (2023)

This tokenizer takes Unicode scalars as primitive (as opposed to
bytes).

A tokenizer is a vocabulary and a merge list. A vocabulary is a
dictionary, string? -> integer?, and the inverse dictionary integer?
-> string? A merge list is a list of merge rules.

|#

(struct merge-rule (fst snd out) #:transparent)
(struct tokeniser (vocab vocab-inverse merges) #:transparent)

;; read-tokeniser/json : port? -> tokeniser?
;;
;; Parse a JSON representation of a tokeniser. The JSON must be of the form:
;;
;; { ...,
;;   "model" : { "vocab" : <dictionary>, 
;;               "merges": <array>
;;             }  
;; }
;;
;; All fields in "...", except those shown, are ignored. <dictionary>
;; is a map from strings to numbers (token ids); <array> is an array
;; of strings, where each string is two parts separated by a space.
;; Furthermore, the set of token ids must be precisely the set of
;; integers in the range 0 to one less than the number of tokens.
;;
(define (read-tokeniser/json in)
  ;; Convert a string "aa bbb" into a merge-rule
  (define (parse-merge-rule r vocab)
    (let ([rs (string-split r " ")])
      (merge-rule (hash-ref vocab (car rs))
                  (hash-ref vocab (cadr rs))
                  (hash-ref vocab (string-append (car rs) (cadr rs))))))
  
  (let* ([tokeniser/json (read-json in)]
         [model          (dict-ref tokeniser/json 'model)])
    (let ([vocab/jsexpr  (dict-ref model 'vocab)]    ; a hash?  
          [merges/jsexpr (dict-ref model 'merges)]) ; a list?
      (let* ([vocab/assoc ; Convert symbol-valued keys to strings
              (hash-map vocab/jsexpr (λ (k v) (cons (symbol->string k) v)))]
             [vocab
              (make-immutable-hash vocab/assoc)]
             [vocab-inverse
              (list->vector (map car (sort vocab/assoc < #:key cdr)))]
             [merges
              (map (λ (m) (parse-merge-rule m vocab)) merges/jsexpr)])
        (tokeniser vocab vocab-inverse merges)))))



(module+ test

;; picked pickled pickles, from the reference above
  (define *input* #<<EOF
{
"model": {
"vocab": {
"▁" : 0,
"p" : 1,
"i" : 2,
"c" : 3,
"k" : 4,
"l" : 5,
"e" : 6,
"d" : 7,
"s" : 8,
"pi" : 9,
"ck" : 10,
"ed" : 11,
"pick" : 12,
"pickl" : 13
},
"merges" : [
"p i",
"c k",
"e d",
"pi ck",
"pick l"
]
}
}
EOF
)

  (define tkzr
    (call-with-input-string *input* read-tokeniser/json))


)
