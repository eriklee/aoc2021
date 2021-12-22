(local fennel (require :fennel))
(local utils (require :utils))

(fn parse-inp [inp-file]
  (var res {:scores [] :rolls 0 :spaces []})
  (each [line (io.lines inp-file)]
    (let [(_ _ pl pos) 
          (string.find line "^Player (%d) starting position: (%d+)$")]
      (table.insert res.spaces (tonumber pos))
      (table.insert res.scores 0)))
  res)

(fn move [cp roll]
  "return the new position after rolling"
  (+ 1 (% (+ cp roll -1) 10)))

(fn mk-det-die [] {:state 1})

(fn roll-det [die]
  "rolls the deterministic die"
  (var res die.state)
  (tset die :state (+ 1 (% die.state 100)))
  res)

(fn take-turn [st die pl]
  (var cp (. st.spaces pl))
  (for [i 1 3]
    (set cp (move cp (roll-det die))))

  (local ns (+ (. st.scores pl) cp))
  (tset st.scores pl ns)
  (tset st :rolls (+ 3 st.rolls))
  (tset st.spaces pl cp))
  ;(print (.. "After " st.rolls " rolls Player " pl " is on space " cp " with a total score of " ns)))

(fn other-p [p] (+ 1 (% p 2)))

(fn play-game-pt1 [st]
  (var die (mk-det-die))
  (var pl 1)
  (while (and (< (. st.scores 1) 1000) 
              (< (. st.scores 2)  1000))
    (take-turn st die pl)
    (set pl (other-p pl))))

;; part 2 - disappointingly little reuse...
(fn key [player pp1 pp2 sp1 sp2]
  "Turns a set of state into a key we can store in a table"
  (+ (* pp1 10 10 22 22 2)
     (* pp2 10 22 22 2)
     (* sp1 22 2)
     (* sp2 2)
     (- player 1)))

(fn key-state [p board-st]
  (key p (. board-st.spaces 1) (. board-st.spaces 2)
       (math.min 21 (. board-st.scores 1))
       (math.min 21 (. board-st.scores 2))))

(fn merge-res [x y] (utils.tab-inc x 1 (. y 1)) (utils.tab-inc x 2 (. y 2)))

(fn play-game-pt2 [board-st dp-st p]
  (local k (key-state p board-st))
  ;(print (.. "entered p="p
  ;             " pp1=" (. board-st.spaces 1) " pp2=" (. board-st.spaces 2) 
  ;             " sp1=" (. board-st.scores 1) " sp2=" (. board-st.scores 2)) " key="k)
  (var res [0 0])
  (if (not= nil (. dp-st k)) (set res (. dp-st k))
    ; otherwise, we need to recursively find it
    (do
      (for [i 1 3]
        (for [j 1 3]
          (for [k 1 3]
            (local new-sp (move (. board-st.spaces p) (+ i j k)))
            (local new-sc (+ (. board-st.scores p) new-sp))
            (if (> new-sc 20) (utils.tab-inc res p 1)
              (let [sub-board (utils.deepclone board-st)]
                (tset sub-board.scores p new-sc)
                (tset sub-board.spaces p new-sp)
                (local sub-res (play-game-pt2 sub-board dp-st (other-p p)))
                (merge-res res sub-res))))))
      (tset dp-st k res)))
  ;(print (.. "result of p="p 
  ;           " pp1=" (. board-st.spaces 1) " pp2=" (. board-st.spaces 2) 
  ;           " sp1=" (. board-st.scores 1) " sp2=" (. board-st.scores 2)
  ;           " == " (fennel.view res)))
  res)
    

;;;;;;;;;;;;;;;
(local inp-file "inp/day21.txt")
;(local inp-file "inp/day21_test.txt")
(local st (parse-inp inp-file))

(play-game-pt1 st)
(local min-score (math.min (. st.scores 1) (. st.scores 2)))
(local prod (* min-score st.rolls))
(print (.. "Day21: Part1: score (" min-score ") * die-rolls (" st.rolls ") = " (utils.bold prod)))

(var dp-st [])
(local st (parse-inp inp-file))
(local wins (play-game-pt2 st dp-st 1))
(local part2 (math.max (. wins 1) (. wins 2)))
(print (.. "Day21: Part2: player wins = " (utils.bold (string.format "%.0f" part2))))
