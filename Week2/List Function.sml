fun swap (pr: int*bool) =
  (#2 pr, #1 pr)

(* (int * int) * (int * int) -> int *)     
fun sum_two_pairs (pr1: int * int, pr2 : int * int) =
  (#1 pr1) + (#2 pr1) + (#1 pr2) + (#2 pr2)

(* int * int -> (int * int) *)				       
fun div_mod (x: int, y: int) =
  (x div y, x mod y)

fun sort_pair(pr : int * int) =
  if (#1 pr) < (#2 pr)
  then pr
  else (#2 pr, #1 pr)

fun sum_list (xs : int list) =
  if null xs
  then 0
  else hd xs + sum_list(tl xs)

fun list_product ( xs : int list) =
  if null xs
  then 1
  else hd xs * list_product(tl xs)

fun countdown (x: int) = 
  if x = 0
  then []
  else x::countdown(x-1)

fun append (xs : int list, ys : int list) =
  if null xs
  then ys
  else hd xs::append((tl xs),ys)

fun sum_pair_list (xs : (int * int) list) =
  if null xs
  then 0
  else #1(hd xs) + #2(hd xs) + sum_pair_list(tl xs)

fun firsts (xs : (int * int) list) = 
  if null xs
  then []
  else #1(hd xs)::firsts(tl xs)

fun seconds (xs : (int * int) list) = 
  if null xs
  then []
  else #2(hd xs)::seconds(tl xs)

fun sum_pair_list2 (xs : (int * int) list) =
  (sum_list(firsts xs)) + (sum_list(seconds xs)) 




