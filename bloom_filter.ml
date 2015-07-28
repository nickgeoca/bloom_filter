let rec getHashes (obj : 'a) (m : int) (salt : int) (k : int) : int list
                   = let h = Hashtbl.hash (salt + Hashtbl.hash obj) in           (* hash salt and hashed value. Ensures hash changes every time *)
                     let toMask x = truncate (2.0 ** (float (1 + truncate (log (float x) /. log 2.0))) -. 1.0) in  (* This was tested for correctness *)
                     let bitmask = toMask m in
                     let test = bitmask land h
                     in if k == 0 then []
                        else if test >= m then  getHashes obj m (salt + 1) k     (* Case where can't use hash, so increase salt *)
                             else test :: getHashes obj m (salt + 1) (k-1) (* Can use hash, so decrement k *)
;;

let getIndexes (obj : 'a) (k : int) (m : int) : int list
                = getHashes obj m 0 k  (* Initialize salt value to zero *)
;;


module BloomFilter = 
  struct
    type bloomFilterT = { n    : int;         (* n  Expected number of elements *)
                          p    : float;       (* p  Probability of false positive *) 
                          k    : int;         (* k  Number of hashes *)
                          m    : int;         (* m  Number of bins *) 
                         bf   : BitSet.t; }  (* Bit array (bloom filter) *)

    type ('a, 'b) either = Left of 'a | Right of 'b

    let fmapEither (fn : 'b -> 'c) (eith : ('a,'b) either) : ('a,'c) either
          = match eith with
          | Left x  -> Left x
          | Right y -> Right (fn y)
 
    let round (x : float) : int  
               = int_of_float (floor (x +. 0.5))

    let create (n : int) (p : float) : (string,bloomFilterT) either
                = let m = round (-1.0 *. (float n) *. (log p) /. ((log 2.0) ** 2.0)) in
                  let k = round ((float m) /. (float n) *. (log 2.0)) in
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


(*

Equations:
m = filter bin count
n = elements
k = hash count
p = false positive probability
 * Prob bit is not set
   (1 - (1/m)) ^ (kn)
 * Prob of false positive
   p = (1 - e ^ (-kn/m)) ^ k

-- Optimal hash assumption
 * Optimal hash count
   k = m / n * (ln 2)
 * Length of bloom filter given optimal hash count
   m = - (n * ln p) / (ln 2) ^ 2
 * Items in bloom filter
   n* = - (m * ln (1 - X/m)) / k
 
input params:
 * nc = expected number of elements to hold
 * p  = probability of false positive
 * file = file name to restore filter

Implementation should not depend on specific type of input values.
Prioritize robustness and maintainability

Length of bloom filter given optimal hash count
m = - (n * ln p) / (ln 2) ^ 2
Optimal hash count
k = m / n * (ln 2)

 *)

(*
-- Create
let bF = create 100 0.1;;
  Right {n = 100; p = 0.1; k = 3; m = 479; bf = <abstr>}
-- Test element not in filter
fmapEither (fun bf -> test bf "hey") bF;;
  - : (string, bool) either = Right false
-- Test element added to filter
fmapEither (fun bf -> insert bf "hey") bF
fmapEither (fun bf -> test bf "hey") bF;;
  - : (string, bool) either = Right true
-- Test number of bits set
fmapEither (fun bf -> BitSet.count bf.bf ) bF;;
  - : (string, int) either = Right 3
 *)