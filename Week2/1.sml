fun list_product ( xs : int list) =
  if null xs
  then 1
  else hd xs * list_product(tl xs)

fun countdown (x: int) = 
  if x = 0
  then []
  else x::countdown(x-1)



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

