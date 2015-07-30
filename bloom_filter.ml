#require "extlib";;

(* -------------------------------------------------- 
   Util functions *) 

(* 0b0100 => 0b0111 ; 0b101 => 0b111 *)
let setAllBitsRightOfLeadingOne (x : int) : int
    = List.fold_right (fun a b -> b lor (b lsr a)) [1;2;4;8;16] x;;

let createIndex (h : int) (m : int) : int
    = let bitmask = setAllBitsRightOfLeadingOne m in
      let index   = bitmask land h in
      index;;

let hashWithSalt (obj : 'a) (salt : int) : int = Hashtbl.hash (salt + Hashtbl.hash obj);;  (* Hash object and salt *)

(* Hash value must map to index value. Hash is much bigger than range of the index (0 to m-1), and
 * this creates a challenge. Taking modulo of hash is one method, but is erroneous b/c it doesn't
 * produce a uniform distribution of index values. 
 * Solution is to only take hash values from 0 to m-1. Optimize by masking out bits larger than (m-1).
 * Example, probability of valid hash: if m = 1000, then the odds of a 32 bit hash being less than 'm' is about 1000 / 2^32 
 *)
let rec getIndexes' (obj : 'a) (m : int) (salt : int) (k : int) : int list =
    match k with 
      0 -> []
    | _ -> let h     = hashWithSalt obj salt in
           let index = createIndex h m in
           match (index < m) with                                  (* index should be in bounds... i.e. 'm'=10, so valid indexes are 0 to 9 *)
             true  -> index :: getIndexes' obj m (salt + 1) (k-1)  (* Can use index. Decrement k *)
           | false ->          getIndexes' obj m (salt + 1) k      (* Can't use index, so try again *)
;;

let getIndexes (obj : 'a) (k : int) (m : int) : int list
    = getIndexes' obj m 0 k  (* Initialize salt value to zero *)
;;

let round (x : float) : int = int_of_float (floor (x +. 0.5));;

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
                = let m' = -1.0 *. (float n) *. (log p) /. ((log 2.0) ** 2.0) in
                  let m  = round m' in
                  let k  = round (m' /. (float n) *. (log 2.0)) in
                  let bFT = { n = n;
                              p = p;
                              m = m;
                              k = k;
                              bf = (BitSet.create m) } in
                  if      m < 2              then Left "m value too small"
                  else if k < 1              then Left "k value less than one"
                  else if p < 0.0 || p > 1.0 then Left "p value out of range"
                  else Right bFT

    let insert (b : bloomFilterT) (obj : 'a) : unit
                = let bf = b.bf in
                  let k  = b.k in
                  let m  = b.m in
                  let ks = getIndexes obj k m
                  and setBf = BitSet.set bf in
                  List.map setBf ks;     (* Map over hash *) 
                  ()
                 
    let test (b : bloomFilterT) (obj : 'a) : bool 
              = let bf = b.bf in
                let k  = b.k in
                let m  = b.m in
                let ks = getIndexes obj k m in
                let isSetBf = BitSet.is_set bf in              
                let result = List.fold_left (&&) true (List.map isSetBf ks) in
                result
end
