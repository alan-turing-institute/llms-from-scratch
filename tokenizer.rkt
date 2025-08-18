#lang racket/base

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




(module+ test
  (define *tkzr* #<<EOF

{ model: 
  { vocab: 
    }}
EOF
)

  (println *tkzr*)
  
  )
