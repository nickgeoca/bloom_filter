#require "extlib";;

(* -------------------------------------------------- 
   Util functions *) 

let rec getHashes (obj : 'a) (m : int) (salt : int) (k : int) : int list
                   = let h        = Hashtbl.hash (salt + Hashtbl.hash obj) in  (* hash salt and hashed value. Ensures hash changes every time *)
                     let toMask x = truncate (2.0 ** (float (1 + truncate (log (float x) /. log 2.0))) -. 1.0) in  (* This was tested for correctness *)
                     let bitmask  = toMask m in
                     let index    = bitmask land h in
                     let test     = index < m in           (* index should be less than m... i.e. 'm'=10, so 'index' is 0 to 9 *)
                     if k == 0 then []
                     else if test then index :: getHashes obj m (salt + 1) (k-1) (* Can use hash, so decrement k *)
                          else                  getHashes obj m (salt + 1) k     (* Case where can't use hash, so increase salt *)
;;

let getIndexes (obj : 'a) (k : int) (m : int) : int list
                = getHashes obj m 0 k  (* Initialize salt value to zero *)
;;

let round (x : float) : int  
           = int_of_float (floor (x +. 0.5))
;;

(* -------------------------------------------------- 
   Either type *) 

type ('a, 'b) either = Left of 'a | Right of 'b;;

let getLeft (eith : ('a,'b) either) : 'a list
      = match eith with
      | Left x  -> x :: []
      | Right y -> []
;;

let getRight (eith : ('a,'b) either) : 'b list
      = match eith with
      | Left x  -> []
      | Right y -> y :: []
;;

let fmapEither (fn : 'b -> 'c) (eith : ('a,'b) either) : ('a,'c) either
      = match eith with
      | Left x  -> Left x
      | Right y -> Right (fn y)
;;


(* -------------------------------------------------- 
   Bloom filter *) 

module BloomFilter = 
  struct
    type bloomFilterT = { n    : int;         (* n  Expected number of elements *)
                          p    : float;       (* p  Probability of false positive *) 
                          k    : int;         (* k  Number of hashes *)
                          m    : int;         (* m  Number of bins *) 
                          bf   : BitSet.t; }  (* Bit array (bloom filter) *)

    let create (n : int) (p : float) : (string,bloomFilterT) either
                = let m = round (-1.0 *. (float n) *. (log p) /. ((log 2.0) ** 2.0)) in
                  let k = round ((round m) /. (float n) *. (log 2.0)) in
                  let bFT = { n = n;
                              p = p;
                              m = m;
                              k = k;
                              bf = (BitSet.create m) } in
                  if      m < 2              then Left "m value too small"
                  else if k < 1              then Left "k value less than one"
                  else if p < 0.0 || p > 1.0 then Left "p value out of range"
                  else Right bFT

    let insert (b : bloomFilterT) (obj : 'a) : unit list
                = let bf = b.bf in
                  let k  = b.k in
                  let m  = b.m in
                  let ks = getIndexes obj k m
                  and setBf = BitSet.set bf in
                  List.map setBf ks     (* Map over hash *) 
                 
    let test (b : bloomFilterT) (obj : 'a) : bool 
              = let bf = b.bf in
                let k  = b.k in
                let m  = b.m in
                let ks = getIndexes obj k m in
                let isSetBf = BitSet.is_set bf in              
                let result = List.fold_left (&&) true (List.map isSetBf ks) in
                result
end
