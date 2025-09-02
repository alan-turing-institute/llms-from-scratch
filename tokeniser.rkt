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

The tokeniser in this file tries to emulate the "Llama" tokeniser from
the transfomers library. In particular, this tokeniser:

- takes Unicode scalars as primitive (as opposed to bytes).
- decodes U+2581 to a space

A tokeniser is a vocabulary and a merge list. A vocabulary is a
dictionary, string? -> integer?, and the inverse dictionary, integer?
-> string? implemented as a vector (and used for decoding). A merge
list is a list of merge rules.

|#

(struct merge-rule (fst snd out) #:transparent)
(struct tokeniser (vocab vocab-inverse merges) #:transparent)

;; read-tokeniser/json : port? -> tokeniser?
;;
;; Parse a JSON representation of a tokeniser. The JSON must be of the
;; following form, which is my understanding of the tokenisers
;; available from Huggingface:
;;
;; { ...,
;;   "model" : { "vocab" : <dictionary>, 
;;               "merges": <array>
;;             }  
;; }
;;
;; All fields in "...", except those shown, are ignored. <dictionary>
;; is a map from strings to numbers (that is, token ids); <array> is
;; an array of strings, where each string is two parts separated by a
;; space. Furthermore, the set of token ids must be precisely the set
;; of integers in the range 0 to one less than the number of tokens.
;;
(define (read-tokeniser/json in)
  ;; Helper: Convert a string such as "aa bbb" into a merge rule
  (define (parse-merge-rule r vocab)
    (let ([rs (string-split r " ")])
      (merge-rule (hash-ref vocab (car rs))
                  (hash-ref vocab (cadr rs))
                  (hash-ref vocab (string-append (car rs) (cadr rs))))))
  
  (let* ([tokeniser/json (read-json in)]
         [model          (dict-ref tokeniser/json 'model)])
    (let ([vocab/jsexpr  (dict-ref model 'vocab)]    ; hash?  
          [merges/jsexpr (dict-ref model 'merges)])  ; list?
      (let* ([vocab/assoc ; Convert symbol-valued keys to strings
              (hash-map vocab/jsexpr (λ (k v) (cons (symbol->string k) v)))]
             [vocab
              (make-immutable-hash vocab/assoc)]
             [vocab-inverse
              (list->vector (map car (sort vocab/assoc < #:key cdr)))]
             [merges
              (map (λ (m) (parse-merge-rule m vocab)) merges/jsexpr)])
        (tokeniser vocab vocab-inverse merges)))))


;; tokeniser-decode : tokeniser? [list-of integer?] -> string?
;;
;; Decode the list of tokens into a string.
;; The algorithm is straightfoward:
;; 1. All tokens are converted from their numerical to string form;
;; 2. The resulting strings are concatenated.
;; 3. If there is a leading U+2581, it is removed.
;; 4. All other instances of U+2581 are replaced with a space

(define (tokeniser-decode tkner tokens)
  (define (token->string token)
    (vector-ref (tokeniser-vocab-inverse tkner) token))
  (let ([out (apply string-append
                    (map token->string tokens))])
    (cond
      [(zero? (string-length out))
       ""]
      [(char=? (string-ref out 0) #\▁)
       (string-replace (substring out 1) "▁" " ")]
      [else 
       (string-replace               out "▁" " ")])))


(module+ test
  (require rackunit)

  ;; "picked pickled pickles". Example from Zouhar et al., but the
  ;; merge rules and tokens are excerpted from BritLLM's tokeniser
  ;; (which looks like a standard Llama one, not a special one for
  ;; BritLLM). In that tokeniser, for example, "▁p i" is not a merge
  ;; rule; but "i c" and "▁p ic" are.
  (define *input-tokeniser* #<<EOF
{
"model": {
"vocab": {
"▁"       : 0,
"p"       : 1,
"i"       : 2,
"c"       : 3,
"k"       : 4,
"l"       : 5,
"e"       : 6,
"d"       : 7,
"s"       : 8,
"▁p"      : 9,
"ic"      : 10,
"ick"     : 11,
"ed"      : 12,
"▁pick"   : 13,
"▁picked" : 14,
"le"      : 15, 
"led"     : 16,
"les"     : 17
},
"merges" : [
"▁ p",
"i c",
"ic k",
"▁p ick",
"e d",
"▁pick ed",
"l e",
"le d",
"le s"
]
}
}
EOF
    )

  (define tkzr
    (call-with-input-string *input-tokeniser* read-tokeniser/json))

  (check-equal?
   (tokeniser-decode tkzr '(14 13 16 13 17))
   "picked pickled pickles")
  
  (check-equal?
   (tokeniser-decode tkzr '())
   "")

  (check-equal?
   (tokeniser-decode tkzr '(0))
   "")

  (check-equal?
   (tokeniser-decode tkzr '(0 0))
   " ")

  
  )
