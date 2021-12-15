(local fennel (require :fennel))
(local utils (require :utils))

(fn key [{: x  : y}] (.. x "," y))
(fn parse-pt [line]
  (let [(_ _ x y) (string.find line "^(%d+),(%d+)$")]
    {:x (tonumber x) :y (tonumber y)}))
;(parse-pt "123,456")

(fn parse-fold [line]
  (let [(_ _ xy v) (string.find line "fold along ([xy])=(%d+)")]
    {:axis xy :v (tonumber v)}))
;(parse-fold "fold along y=7")
;(parse-fold "fold along x=5")

(fn parse-inp [file-name]
  (var res {:pts [] :folds []})
  (let [f (assert (io.open file-name))]
    (var line (f:read))
    (while (not= "" line)
      (do (table.insert res.pts (parse-pt line))
      (set line (f:read))))
    (var line (f:read))
    (while (not= nil line)
      (do (table.insert res.folds (parse-fold line))
      (set line (f:read)))))
  res)

; x never changes
; if y < yl no change
; if y > yl -> y = yl - (y - yl) (mirrors across the axis)
(fn flip-y [x y yl]
 (if (> y yl) 
    (values x (- yl (- y yl)))
    (values x y)))
;(flip-y 10 14 7)

(fn flip-x [x y xl]
 (if (> x xl)
   (values (- xl (- x xl)) y)
   (values x y)))
;(flip-x 10 14 7)

(fn count-pts [inp]
  "this is terrible, but nevermind"
 (var tbl {})
 (each [_ p (ipairs inp)]
     (tset tbl (key p) true))
 (utils.tlength tbl))

(fn fold [pts f]
  (var npts [])
  (if (= f.axis :x)
    (each [_ pt (ipairs pts)]
      (let [(x y) (flip-x pt.x pt.y f.v)]
      (table.insert npts {: x : y})))
    (each [_ pt (ipairs pts)]
      (let [(x y) (flip-y pt.x pt.y f.v)]
      (table.insert npts {: x : y}))
      ))
  npts)

(fn fold-all [inp]
  (var res (utils.clone inp.pts))
  (each [_ f (ipairs inp.folds)]
    (set res (fold res f)))
  res)

; this is awful, but whatever...
(fn pp [pts]
  (var rows {})
  (for [i 0 5] (tset rows i []))
  (each [_ pt (ipairs pts)]
    (tset (. rows pt.y) pt.x true))
  (for [i 0 5] 
    (var line {})
    (for [j 0 38]
      (if (= true (. (. rows i) j))
        (table.insert line "#") 
        (table.insert line ".")))
    (print (table.concat line ""))))

;;;;;;;;;;;;;;
;(local inp "inp/day13_test.txt")
(local inp "inp/day13.txt")
(local inputs (parse-inp inp))
(count-pts inputs.pts)
(local npts (fold inputs.pts (. inputs.folds 1)))
(print (.. "Day13: Part1: after 1 fold there are " (utils.bold (count-pts npts)) " visible dots"))
(print (.. "Day13: Part2: after all the folds there are some letters:" ))
(pp (fold-all inputs))
