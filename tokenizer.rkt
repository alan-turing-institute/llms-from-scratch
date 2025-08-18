#lang racket/base

(require json
         racket/port
         racket/dict)

;; Basic Llama 2-type tokenizer
;;
;; The general approach is described in, for example:
;;
;; Vilém Zouhar et al. “A formal perspective on byte-pair
;; encoding”. In: arXiv preprint arXiv:2306.16837 (2023)

;; Further details
;;
;; This tokenizer takes Unicode scalars as primitive (as opposed to
;; bytes).

;; A tokenizer is a vocabulary and a merge list
;; A vocabulary is a dictionary, string? -> integer?
;; A merge list is a list of merge rules

(struct merge-rule (fst snd out) #:transparent)

(struct tokeniser (vocab merges) #:transparent)

;; read-tokeniser/json : port? -> tokeniser?
;; Read a tokeniser represented as JSON. The JSON must be of the form:
;;
;; { ...,
;;   "model" : { "vocab" : <dictionary>, 
;;               "merges": <array>
;;             }  
;; }
;;
;; All fields in "...", except those shown, are ignored. <dictionary>
;; is a map from strings to numbers; <array> is an array of strings,
;; where each string is two parts separated by a space.
;; 
(define (read-tokeniser/json in)
  (define (string->merge-rule r vocab)
    (void))
  
  (let* ([tokeniser/json (read-json in)]
         [model          (dict-ref tokeniser/json 'model)])
    (let* ([vocab/jsexpr  (dict-ref model 'vocab)]    ; a hash?  
           [merges/jsexpr (dict-ref model 'merges)]) ; a list?
      ;; Convert symbol-valued keys to strings,
      ;; parse the merge rules, and replace strings in the
      ;; merge rules with the token ids
      (let ([vocab (hash-map/copy (λ (k v) (values (symbol->string k) v)) vocab)])
        
        ))
    (tokeniser
     
     (map (λ (m) (string->merge-rule m)) merges)
     ) (displayln (dict-count vocab))
    (displayln (length merges)))
  )



(module+ test

  ;; picked pickled pickles, from the reference above
  (define *tkzr* #<<EOF
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



  (println *tkzr*)
  
  )
