
(* This function takes two dates and evaluates to true or false. 
   It evaluates to true if the first argument is a date that comes before the second argument. *)
fun is_older (date1: int * int * int, date2: int * int * int) = 
  if #1 date1 < #1 date2
  then true
  else if #1 date1 = #1 date2 andalso #2 date1 < #2 date2
  then true
  else if #1 date1 = #1 date2 andalso #2 date1 = #2 date2 andalso #3 date1 < #3 date2
  then true
  else false

(* This function takes a list of dates and a month (i.e., an int) and returns how many dates in the list are in the given month. *)
fun number_in_month (date: (int * int * int) list, month: int) =
  if null date
  then 0
  else if #2(hd date) = month
  then 1 + number_in_month((tl date), month)
  else number_in_month((tl date), month)

(* takes a list of dates and a list of months (i.e., an int list) and returns the number of dates in the list of dates that are in any of the months in the list of months.
   Assume the list of months has no number repeated. *)
fun number_in_months (date: (int * int * int) list, month: int list) =
  if null month
  then 0
  else number_in_month (date, hd month) + number_in_months(date, tl month)

