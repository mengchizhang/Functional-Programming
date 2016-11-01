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

(* This function takes a list of dates and a list of months (i.e., an int list) and returns the number of dates in the list of dates that are in any of the months in the list of months.
   Assume the list of months has no number repeated. *)
fun number_in_months (date: (int * int * int) list, month: int list) =
  if null month orelse null date
  then 0
  else number_in_month (date, hd month) + number_in_months(date, tl month)

(* This function takes a list of dates and a month (i.e., an int) and returns a list holding the dates from the argument list of dates that are in the month.
   The returned list should contain dates in the order they were originally given. *)
fun dates_in_month (date: (int *int * int) list, month: int) = 
  if null date
  then []
  else if #2(hd date) = month
  then hd date::dates_in_month(tl date, month)
  else dates_in_month(tl date, month)

(* This functiion takes a list of dates and a list of months (i.e., an int list) and returns a list holding the dates from the argument list of dates that are in any of the months in the list of months. 
Assume the list of months has no number repeated. *)
fun dates_in_months (date: (int * int * int) list, month: int list) = 
  if null month orelse null date
  then []
  else dates_in_month(date, hd month) @ dates_in_months(date, tl month)

(* This function takes a list of strings and an int n and returns the nth element of the list where the head of the list is 1st *)
fun get_nth (s: string list, n: int) = 
  if null s
  then NONE
  else if n = 1
  then SOME(hd s)
  else get_nth(tl s, n-1)

(* This function takes a date and returns a string *)
fun date_to_string (date: int * int * int) = 
  let val month = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
      val day_year = Int.toString(#3 date) ^ (", ") ^ Int.toString(#1 date)
  in valOf(get_nth(month, #2 date)) ^ (" ") ^ day_year
  end

(* This function takes an int called sum, which you can assume is positive, and an int list, which you can assume contains all positive numbers, and returns an int. It will return an int n such that the first n elements of the list add to less than sum, but the first n + 1 elements of the list add to sum or more. 
   Assume the entire list sums to more than the passed in value *)
fun number_before_reaching_sum (sum: int, l: int list) =
  let val hd_sum = 0
      fun list_sum (hdsum: int, xl: int list) = 
	 if hdsum >= sum
	 then 0
	 else 1 + list_sum (hdsum + hd xl, tl xl)
  in list_sum (hd_sum + hd l, tl l)
  end



