#lang racket

(require rackunit
         threading)

(struct game (number red green blue) #:transparent) ;; game hold the max number of cubes
(struct bag (red green blue) #:transparent) ;; a possible bag, maxes of each color

(define (line->game line)
  (~> line
      (string-split _ ":")
      ((lambda (game-throws)
        (list (~> (first game-throws)
                  (string-split _ " ")
                  (second)
                  (string->number))
              (~> (second game-throws)
                  (string-split _ ";")
                  (map (lambda~>
                        (string-split _ ",")
                        (map string-trim _)
                        (map (lambda~> (string-split _ " ")) _))
                       _)
                  (apply append _)))))
      ((lambda (game+number-color-pairs)
         (game (first game+number-color-pairs)
               (number-color-pairs-and-color->color-max "red" (second game+number-color-pairs))
               (number-color-pairs-and-color->color-max "green" (second game+number-color-pairs))
               (number-color-pairs-and-color->color-max "blue" (second game+number-color-pairs))
               )))))

(check-equal? (line->game "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green") (game 1 4 2 6))

(define (number-color-pairs-and-color->color-max color pairs)
  (foldl (lambda (pair max-value)
           (let ([current-value (string->number (first pair))])
             (if (and (string=? color (second pair))
                      (> current-value max-value))
                 current-value
                 max-value)))
         0
         pairs))

(check-equal? (number-color-pairs-and-color->color-max "blue" (apply append '((("3" "blue") ("4" "red")) (("1" "red") ("2" "green") ("6" "blue")) (("2" "green"))))) 6)

(define ((game<bag bag-contents) game-contents)
  (and (<= (game-red game-contents)
           (bag-red bag-contents))
       (<= (game-green game-contents)
           (bag-green bag-contents))
       (<= (game-blue game-contents)
           (bag-blue bag-contents))))

(check-true ((game<bag (bag 8 8 10)) (game 1 7 8 9)))
(check-false ((game<bag (bag 4 0 12)) (game 1 7 8 9)))

(define (filter-games-by-bag->game-numbers games possible-bag)
  (~> games
      (filter (game<bag possible-bag) _)
      (map (lambda (possible-game) (game-number possible-game)) _)))

;; part one
(define (file-name->possible-game-total file-name possible-bag)
  (~> file-name
      (file->lines)
      (map line->game _)
      (filter-games-by-bag->game-numbers _ possible-bag)
      (apply + _)))

;; part two
(define (game->power game-contents)
  (* (game-red game-contents)
     (game-green game-contents)
     (game-blue game-contents)))

(define (line->game-power line)
  (~> line
      (line->game)
      (game->power)))

(define (file-name->total-game-power file-name)
  (~> file-name
      (file->lines)
      (map line->game-power _)
      (apply + _)))

(check-equal? (file-name->possible-game-total "example.txt" (bag 12 13 14)) 8)
;(file-name->possible-game-total "input.txt" (bag 12 13 14)) ;; 2617
(check-equal? (line->game-power "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green") 48)
(check-equal? (file-name->total-game-power "example.txt") 2286)
;(file-name->total-game-power "input.txt") ;; 59795
