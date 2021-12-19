(local fennel (require :fennel))
(local utils (require :utils))
(fn fv [x] (fennel.view x))

(local M {})

(local hex-expansions
  { :0 "0000" :1 "0001" :2 "0010" :3 "0011"
    :4 "0100" :5 "0101" :6 "0110" :7 "0111"
    :8 "1000" :9 "1001" :A "1010" :B "1011"
    :C "1100" :D "1101" :E "1110" :F "1111" })
(fn hex2bin [inp]
  (var res [])
  (for [i 1 (length inp)]
    (table.insert res (. hex-expansions (string.sub inp i i))))
  (table.concat res ""))

(comment (
(assert (= (hex2bin "D2FE28")
           "110100101111111000101000")
        "hex2bin simple case")
(assert (= (hex2bin "38006F45291200")
           "00111000000000000110111101000101001010010001001000000000")
        "hex2bin slightly longer")))

; bitstring - table with 
;   :bits (string)
;   :read-idx (number)
(fn M.to-bit-string [inp]
  { :bits (hex2bin inp)
   :read-idx 1} )

(fn M.ppbs [bs]
  (.. (utils.boldred (string.sub bs.bits 1 (- bs.read-idx 1)))
      (string.sub bs.bits bs.read-idx)))

(fn bs-ize-bin [bin] {:bits bin :read-idx 1})

; should make this a fancy metatable thing like the heap...
(fn read-n [bs n]
  "Reads n bits from the bitstring and returns them"
  (assert (>= (length bs.bits) (+ (- n 1) bs.read-idx)) (.. "asked to read too many bytes! bitslen=" (length bs.bits) " read-idx=" bs.read-idx " n=" n))
  (local res (string.sub bs.bits bs.read-idx (+ bs.read-idx (- n 1))))
  (tset bs :read-idx (+ bs.read-idx n))
  res)


(fn parse-header [bs]
  (let [vsns (read-n bs 3)
        typs (read-n bs 3)]
    {:vsn (tonumber vsns 2)
     :typ (tonumber typs 2)}))


(fn parse-literal [bs]
  (var nib (read-n bs 5))
  (var ress [(string.sub nib 2 5)])
  (while (= "1" (string.sub nib 1 1))
    (set nib (read-n bs 5))
    (table.insert ress (string.sub nib 2 5)))
  (tonumber (table.concat ress "") 2))

(comment (= 2021 
           (parse-literal 
             (M.to-bit-string "D2FE28")))
        "Parse literal seems broken...")

(fn parse-length [bs]
  (match (read-n bs 1)
    "0" (values :bitl (tonumber (read-n bs 15) 2))
    "1" (values :pktcnt (tonumber (read-n bs 11) 2))))

(assert (= (values :bitl 27) 
           (parse-length (bs-ize-bin "0000000000011011")))
        "parse-length ltid 0 broken")

(fn parse-n-pkts [bs n]
  (var res [])
  (for [i 1 n]
    (table.insert res (M.parse-packet bs)))
  res)

(fn parse-bitl-pkts [bs n]
  (var res [])
  (local tgt (+ bs.read-idx n))
  (while (< bs.read-idx tgt)
    (local pkt (M.parse-packet bs))
    (table.insert res pkt))
  res)

(fn M.parse-packet [bs]
  (local res (parse-header bs))
  (match res.typ
    4 (tset res :lit (parse-literal bs))
    _ (match (parse-length bs)
        (:bitl n) (tset res :pkts (parse-bitl-pkts bs n))
        (:pktcnt n) (tset res :pkts (parse-n-pkts bs n))))
  res)

;(parse-header (M.to-bit-string (tonumber 0xD2FE28)))
;(local bs (M.to-bit-string "D2FE28"))
;(print (fv (M.parse-packet bs)))

;(print "bigger example1")
;(local bs (M.to-bit-string "38006F45291200"))
;(print (fv (M.parse-packet bs)))

;(print "biggest example1")
;(local bs (M.to-bit-string "EE00D40C823060"))
;(print (fv (M.parse-packet bs)))

M
