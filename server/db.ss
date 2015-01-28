;; Copyright (C) 2013 Dave Griffiths
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.
;;
;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

#lang racket
(require (planet jaymccarthy/sqlite:5:1/sqlite))
(provide (all-defined-out))
(require "logger.ss")

(define (setup db)
  (exec/ignore db "CREATE TABLE player ( id INTEGER PRIMARY KEY AUTOINCREMENT, played_before INTEGER, age_range INTEGER, score INTEGER)")
  (exec/ignore db "CREATE TABLE eaten ( id INTEGER PRIMARY KEY AUTOINCREMENT, player_id INTEGER, morph TEXT, toxic INTEGER, time_stamp INTEGER )")
  (exec/ignore db "CREATE TABLE morph ( id INTEGER PRIMARY KEY AUTOINCREMENT, texture_name TEXT, probability INTEGER, active INTEGER, can_be_toxic INTEGER, wing_shape INTEGER )")
  (exec/ignore db "CREATE TABLE player_name ( id INTEGER PRIMARY KEY AUTOINCREMENT, player_id INTEGER, player_name TEXT )")
  )

(define (insert-player db played_before age_range)
  (insert db "insert into player values (NULL, ?, ?, 0)"
          played_before
          age_range))

(define (insert-player-name db player_id player_name)
  (log "player name " player_id " " player_name)
  (insert db "insert into player_name VALUES (NULL, ?, ?)"
          player_id player_name ))

(define (set-player-score db player-id score)
  (exec/ignore
   db "update player set score = ? where id = ?" score player-id))

(define (get-hiscores db)
  (map
   (lambda (i)
     (list (vector-ref i 0) (vector-ref i 1)))
   (cdr (select db "select n.player_name, p.score from player as p join player_name as n on p.id=n.player_id order by p.score limit 100;"))))

(define (insert-eaten db player_id morph toxic time_stamp)
  (insert db "INSERT INTO eaten VALUES (NULL, ?, ?, ?, ?)"
          player_id morph toxic time_stamp))

(define (insert-morph db texture_name probability active can_be_toxic wing_shape)
  (insert db "INSERT INTO morph VALUES (NULL, ?, ?, ?, ?, ?)"
          texture_name probability active can_be_toxic wing_shape))

(define (update-morph db id probability active can_be_toxic wing_shape)
  (exec/ignore
   db "update morph set probability = ?, active = ?, can_be_toxic = ?, wing_shape = ? where id = ?"
   probability active can_be_toxic wing_shape id))

(define (delete-morph db id)
  (exec/ignore db "delete from morph where id = ?" id))

(define (get-morphs db)
  (let ((s (select db "SELECT id, texture_name, probability, active, can_be_toxic, wing_shape from morph")))
    (if (null? s)
        '()
        (map
         (lambda (i)
           (list
            (vector-ref i 0)
            (vector-ref i 1)
            (vector-ref i 2)
            (vector-ref i 3)
            (vector-ref i 4)
            (vector-ref i 5)
            ))
         (cdr (select db "SELECT id, texture_name, probability, active, can_be_toxic, wing_shape from morph"))))))

;(define (get-player-averages db)
;  (let ((players (cdr (select db "SELECT * from player"))))
;    (filter
;     (lambda (av)
;       (not (false? av)))
;     (map
;      (lambda (player)
;        (get-player-average-min db (vector-ref player 0)))
;      players))))

(define (get-player-averages db)
  (map
   (lambda (i) (vector-ref i 0))
   (cdr (select db "SELECT score from player"))))

(define (get-player-average db player-id)
  (let ((v (cadr
            (select db (string-append
                        "SELECT avg(time_stamp), count(time_stamp) from click where success = 1 and player_id = "
                        (number->string player-id))))))
    (when (> (vector-ref v 1) 5)
          (exec/ignore
           db (string-append
               "UPDATE player SET score = "
               (number->string (vector-ref v 0))
               " where id = " (number->string player-id))))
    (vector-ref v 0)))

(define (get-player-count db player-id)
  (let ((v (cadr (select db (string-append
                             "SELECT count(time_stamp) from click where success = 1 and player_id = "
                             (number->string player-id))))))
    (vector-ref v 0)))

(define (get-position v ol)
  (define (_ n l)
    (cond
      ((null? l) n)
      ((> (car l) v) n)
      (else (_ (+ n 1) (cdr l)))))
  (_ 1 ol))

(define (get-player-rank db av)
  (if av
      (let ((rank (sort (get-player-averages db) <)))
        (get-position av rank))
      999))
