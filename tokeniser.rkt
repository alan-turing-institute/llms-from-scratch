#lang racket/base

(require json
         racket/port
         racket/dict
         racket/string
         racket/match)

#|
----------------------------------------------------------------------

Basic Llama-type (BPE) tokenizer. The general approach is described
in, for example:

Vilém Zouhar et al. “A formal perspective on byte-pair
encoding”. In: arXiv preprint arXiv:2306.16837 (2023)

Only encoding and decoding are implemented: the merge rules are given,
not derived from text.

This tokeniser tries to emulate the "Llama" tokeniser from
the transfomers library. In particular, it:

- takes Unicode scalars as primitive (as opposed to bytes).
- decodes U+2581 to a space

TODO: However, this tokeniser currently fails if an input character is
not in the list of known tokens

----------------------------------------------------------------------
|#

;; A tokeniser is a vocabulary and a merge list. A vocabulary is a
;; dictionary, string? -> integer?, and the inverse dictionary, integer?
;; -> string? implemented as a vector (and used for decoding). A merge
;; list is a list of merge rules.

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
;; 1. All tokens are converted from token-ids to string;
;; 2. The resulting strings are concatenated.
;; 3. If there is a leading U+2581, it is removed.
;; 4. All other instances of U+2581 are replaced by a space

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
       (string-replace  out              "▁" " ")])))

;; tokeniser-encode : tokeniser? string? -> [list-of integer?]
;;
;; Encode a string as a list of token ids.
;; 
(define (tokeniser-encode tkner str)
  (let* ([cs (string->list str)]                      ; cs is a list-of char?
         [cs (map space-to-special cs)]               ; replace spaces by special
         [cs (if (null? cs) null (cons #\▁ cs))]      ; maybe prefix with special
         [toks
          (let ([vocab (tokeniser-vocab tkner)])
            (for/list ([c (in-list cs)])
              (hash-ref vocab (string c))))])
    (merge-tokens (tokeniser-merges tkner) toks)))

(define (space-to-special c)
  (if (char=? c #\space)
      #\▁
      c))

;; merge-tokens : [list-of merge-rule?] [list-of integer?] 
(define (merge-tokens rules toks)
  (for/fold ([toks toks])
            ([rule (in-list rules)])
    (merge-tokens/rule rule toks)))

;; TODO: Avoid reverse each time
(define (merge-tokens/rule rule toks)
  (match rule
    [(merge-rule fst snd out)
     (let merge-tokens/rule/recurse ([left '()]
                                     [right toks])
       (if (null? right)
           (reverse left)
           (let ([a (car right)]
                 [bs (cdr right)])
             (if (null? bs)
                 (reverse (cons a left))
                 (let ([b (car bs)])
                   (if (and (= a fst)
                            (= b snd))
                       (merge-tokens/rule/recurse (cons out left) (cdr bs))
                       (merge-tokens/rule/recurse (cons a left) bs)))))))]))


;;; ------------------------------------------------------------------ 

(module+ test
  (require rackunit)

  ;; "picked pickled pickles". Example from Zouhar et al., 
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
"▁pi"    : 10,
"ck"     : 11,
"▁pick"  : 12,
"ed"     : 13,
"▁pickl" : 14
},
"merges" : [
"▁ p",
"▁p i",
"c k",
"▁pi ck",
"e d",
"▁pick l"
]
}
}
EOF
    )

  ;; --------------------------------------------------
  ;; Simple example
  
  (define tkzr
    (call-with-input-string *input-tokeniser* read-tokeniser/json))

  (check-equal?
   (tokeniser-decode tkzr '(12 13 14 13 14 6 8))
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

  ;; --------------------------------------------------
  ;; Example using BritLLM's tokeniser definition

  
  
  
  )
