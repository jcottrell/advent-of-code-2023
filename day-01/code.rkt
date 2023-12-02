#lang racket

(require rackunit
         threading)

(define (squeeze-for-calibration mixed-line)
  (letrec ([squeeze (lambda (left right left-value right-value)
                      (cond [(or (and left-value right-value)
                                 (> left right))
                             (let ([left-value (or left-value right-value)]
                                   [right-value (or right-value left-value)])
                               (string->number (string-append (number->string left-value)
                                                              (number->string right-value))))]
                            [else
                             (squeeze (if left-value left (add1 left))
                                      (if right-value right (sub1 right))
                                      (or left-value
                                          (string->number (substring mixed-line left (add1 left))))
                                      (or right-value
                                          (string->number (substring mixed-line right (add1 right)))))]))])
    (squeeze 0 (sub1 (string-length mixed-line)) #f #f)))

;; part one
#;(define (file->calibration-total file-name)
  (~> file-name
      (file->lines #:mode 'text)
      (map squeeze-for-calibration _)
      ;((lambda (v) (display v) v))
      (apply + _)))

;; part two
(define (file->calibration-total file-name #:conversion-map [key-value-pairs '()])
  (~> file-name
      (file->lines #:mode 'text)
      (map (convert-word-to-number key-value-pairs) _)
      ;((lambda (v) (display v) v))
      (map squeeze-for-calibration _)
      ;((lambda (v) (display v) v))
      (apply + _)))

(define word-to-number
  '(("one" "1")
    ("two" "2")
    ("three" "3")
    ("four" "4")
    ("five" "5")
    ("six" "6")
    ("seven" "7")
    ("eight" "8")
    ("nine" "9")))

(define ((convert-word-to-number mapping-pairs) original)
  (~> original
      (replace-left-to-right mapping-pairs _)
      #;(replace-first-found mapping-pairs _)
      #;(replace-last-found mapping-pairs _)))

(define (replace-first-found replacement-pairs original)
  (let ([original-length (string-length original)])
    (letrec ([replace (lambda (left right)
                        (let* ([current (substring original left (- right left))]
                               [converted (foldl (lambda (pair so-far) (string-replace so-far (first pair) (second pair)))
                                                 current
                                                 replacement-pairs)]
                               [was-converted (not (string=? current converted))])
                          (cond [was-converted (string-append converted (substring original right))]
                                [(> (add1 right) original-length) original]
                                [else (replace left (add1 right))])))])
      (replace 0 1))))

(define (replace-last-found replacement-pairs original)
  (let* ([pairs-with-reversed-keys (map (lambda (key-value)
                                          (list (string-reverse (first key-value)) (second key-value)))
                                        replacement-pairs)]
         [reversed-original (string-reverse original)]
         [result (replace-first-found pairs-with-reversed-keys reversed-original)])
    (string-reverse result)))

(define (string-reverse original)
  (~> original
      (string->list)
      (reverse)
      (list->string)))

(define (replace-left-to-right replacement-pairs original)
  (let ([original-length (string-length original)])
    (letrec ([replace (lambda (left right so-far)
                        (let* ([current (substring original left right)]
                               [converted (foldl (lambda (pair so-far) (string-replace so-far (first pair) (second pair)))
                                                 current
                                                 replacement-pairs)]
                               [was-converted (not (string=? current converted))])
                          (cond [was-converted (if (> (add1 right) original-length)
                                                   (string-append so-far converted)
                                                   (replace right (add1 right) (string-append so-far converted)))]
                                [(> (add1 right) original-length) (string-append so-far current)]
                                [else (replace left (add1 right) so-far)])))])
      (replace 0 1 ""))))

;; part one
(check-equal? (squeeze-for-calibration "1abc2") 12)
(check-equal? (squeeze-for-calibration "aa2abbbccc") 22)
(check-equal? (file->calibration-total "example-part1.txt") 142)
(check-equal? (squeeze-for-calibration "65bnhfgcgfxgqflblvjdhhtvzpfoureightfive") 65)
;(file->calibration-total "input.txt") ;; => 53334

;; part two
(check-equal? (replace-first-found word-to-number "eightwothree") "8wothree")
(check-equal? (replace-first-found word-to-number "abceightwothreetwone") "abc8wothreetwone")
(check-equal? (replace-first-found word-to-number "abcdefghijkone") "abcdefghijk1")
(check-equal? (replace-last-found word-to-number "abceightwothreetwone") "abceightwothreetw1")
(check-equal? (replace-first-found word-to-number "1abc2") "1abc2")
(check-equal? (replace-left-to-right word-to-number "two1nine") "219")
(check-equal? (replace-left-to-right word-to-number "4nineeightseven2") "49872")
;(check-equal? (file->calibration-total "example-part2.txt" #:conversion-map word-to-number) 281)
;(file->calibration-total "input.txt" #:conversion-map word-to-number) ;; => 52834
