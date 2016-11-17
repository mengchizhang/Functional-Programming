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

(* Alternative *)
fun is_older2 (date1 : int * int * int, date2 : int * int * int) =
    let 
        val y1 = #1 date1
        val m1 = #2 date1
        val d1 = #3 date1
        val y2 = #1 date2
        val m2 = #2 date2
        val d2 = #3 date2
    in
        y1 < y2 orelse (y1=y2 andalso m1 < m2)
                orelse (y1=y2 andalso m1=m2 andalso d1 < d2)
    end 

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
  let
      fun list_sum (hdsum: int, xl: int list) = 
	 if hdsum >= sum
	 then 0
	 else 1 + list_sum (hdsum + hd xl, tl xl)
  in list_sum (hd l, tl l)
  end

(* Alternative *)
fun number_before_reaching_sum2 (sum: int, l: int list) =
  if hd l >= sum
  then 0
  else 1 + number_before_reaching_sum2 (sum - hd l, tl l)

(* This function takes a day of year (i.e., an int between 1 and 365) and returns what month that day is in (1 for January, 2 for February, etc.). *)
fun what_month (day: int) =
  let val days = [31,28,31,30,31,30,31,31,30,31,30,31]
  in 1 + number_before_reaching_sum (day, days)
  end

(* This function takes two days of the year day1 and day2 and returns an int list [m1,m2,...,mn] where m1 is the month of day1, m2 is the month of day1+1, ..., and mn is the month of day day2. *)
fun month_range (day1: int, day2: int) = 
  if day1 > day2
  then []
  else what_month(day1)::month_range(day1+1,day2)


(* This function takes a list of dates and evaluates to an (int*int*int) option. 
   It evaluates to NONE if the list has no dates and SOME d if the date d is the oldest date in the list. *)
fun oldest (date: (int * int * int) list) =
  if null date
  then NONE
  else let fun oldest_nonempty (date: (int * int * int) list) =
	     if null (tl date)
	     then hd date
	     else let val oldest_tl = valOf(oldest (tl date))
		  in if is_older (hd date, oldest_tl)
		     then hd date
		     else oldest_tl
		  end
       in SOME (oldest_nonempty date)
       end

(* Arguably Better Alternative *)
fun oldest2 (dates : (int * int * int) list) =
    if null dates
    then NONE
    else let fun f dates =
	       if null (tl dates)
	       then hd dates
	       else 
		   let val ans = f (tl dates)
		   in 
		       if is_older(ans, hd dates)
		       then ans
		       else hd dates
		   end
	 in 
             SOME(f dates) 
	 end
