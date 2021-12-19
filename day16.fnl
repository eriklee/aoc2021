(local fennel (require :fennel))
(local utils (require :utils))
(local bits (require :bits))

(fn parse-inp [inp-file]
  (with-open [f (io.open inp-file)]
             (local line ((f:lines)))
             (local bs (bits.to-bit-string line))
             (bits.parse-packet bs)))

(fn parse-lit-inp [line]
  (bits.parse-packet (bits.to-bit-string line)))

(fn sum-vsns [pkt]
  (match pkt.pkts
    nil pkt.vsn
    _ (accumulate [tot pkt.vsn
                   _ sp (ipairs pkt.pkts)]
                  (+ tot (sum-vsns sp)))))

(fn eval-lit [lit] lit)
(fn eval-sum [eval pkts] 
  (accumulate [res 0
               _ sp (ipairs pkts)]
              (+ res (eval sp))))
(fn eval-prod [eval pkts] 
  (accumulate [res 1
               _ sp (ipairs pkts)]
              (* res (eval sp))))
(fn eval-min [eval pkts] 
  (accumulate [res 1e99
               _ sp (ipairs pkts)]
              (math.min res (eval sp))))
(fn eval-max [eval pkts] 
  (accumulate [res -1
               _ sp (ipairs pkts)]
              (math.max res (eval sp))))

(fn eval-gt [eval pkts] 
  (local pkt1 (eval (. pkts 1)))
  (local pkt2 (eval (. pkts 2)))
  (if (> pkt1 pkt2) 1 0))
(fn eval-lt [eval pkts] 
  (local pkt1 (eval (. pkts 1)))
  (local pkt2 (eval (. pkts 2)))
  (if (< pkt1 pkt2) 1 0))
(fn eval-eq [eval pkts] 
  (local pkt1 (eval (. pkts 1)))
  (local pkt2 (eval (. pkts 2)))
  (if (= pkt1 pkt2) 1 0))


(fn evaluate [pkt]
  (local f evaluate)
  (match pkt.typ
    4 (eval-lit pkt.lit)
    0 (eval-sum f pkt.pkts)
    1 (eval-prod f pkt.pkts)
    2 (eval-min f pkt.pkts)
    3 (eval-max f pkt.pkts)
    5 (eval-gt f pkt.pkts)
    6 (eval-lt f pkt.pkts)
    7 (eval-eq f pkt.pkts)))

;;;;;;;;;;;;;;;
(local inp-file "inp/day16.txt")
;(local inp-file "inp/day16_test.txt")
(local pkt (parse-inp inp-file))
;(print (fennel.view pkt))

;(assert (= 16 (sum-vsns (parse-lit-inp "8A004A801A8002F478"))) "test 1")
;(assert (= 12 (sum-vsns (parse-lit-inp "620080001611562C8802118E34"))) "test 2")
;(assert (= 23 (sum-vsns (parse-lit-inp "C0015000016115A2E0802F182340"))) "test 3")
;(assert (= 31 (sum-vsns (parse-lit-inp "A0016C880162017C3686B18A3D4780"))) "test 4")

(local part1 (sum-vsns pkt))
(print (.. "Day16: Part1: sum of versions is: " (utils.bold part1)))

;C200B40A82 finds the sum of 1 and 2, resulting in the value 3.
;(assert (= 3 (evaluate (parse-lit-inp "C200B40A82"))))
;04005AC33890 finds the product of 6 and 9, resulting in the value 54.
;(assert (= 54 (evaluate (parse-lit-inp "04005AC33890"))))
;880086C3E88112 finds the minimum of 7, 8, and 9, resulting in the value 7.
;(assert (= 7 (evaluate (parse-lit-inp "880086C3E88112"))))
;CE00C43D881120 finds the maximum of 7, 8, and 9, resulting in the value 9.
;(assert (= 9 (evaluate (parse-lit-inp "CE00C43D881120"))))
;D8005AC2A8F0 produces 1, because 5 is less than 15.
;(assert (= 1 (evaluate (parse-lit-inp "D8005AC2A8F0"))))
;F600BC2D8F produces 0, because 5 is not greater than 15.
;(assert (= 0 (evaluate (parse-lit-inp "F600BC2D8F"))))
;9C005AC2F8F0 produces 0, because 5 is not equal to 15.
;(assert (= 0 (evaluate (parse-lit-inp "9C005AC2F8F0"))))
;9C0141080250320F1802104A08 produces 1, because 1 + 3 = 2 * 2.
;(assert (= 1 (evaluate (parse-lit-inp "9C0141080250320F1802104A08"))))

(local part2 (evaluate pkt))
(print (.. "Day16: Part2: result of evaluating the pkt is: " (utils.bold part2)))
