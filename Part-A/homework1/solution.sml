(* 1 *)
fun is_older (date1: int * int * int , date2: int*int*int) =
    if #1 date1 <> #1 date2
    then #1 date1 < #1 date2
    else
	if #2 date1 <> #2 date2
	then #2 date1 < #2 date2
	else
	    #3 date1 < #3 date2

(* 2 *)			  
fun number_in_month (dates: (int*int*int) list, month: int) =
    let
	fun count_number(date: (int*int*int) list) =
	    if null date
	    then 0
	    else
		if #2 (hd date) = month
		then 1 + count_number(tl date)
		else 0 + count_number(tl date)
    in
	count_number(dates)
    end

fun number_in_months (dates: (int * int* int) list, months: int list) =
    if null months
    then 0
    else number_in_month(dates, hd months) + number_in_months(dates, tl months)

fun dates_in_month (dates: (int * int *int) list, month: int) =
    if null dates
    then []
    else if #2 (hd dates) = month
    then hd dates::dates_in_month(tl dates, month)
    else dates_in_month(tl dates, month)

fun dates_in_months (dates: (int*int*int) list, month: int list) =
    if null month
    then []
    else dates_in_month(dates, hd month) @ dates_in_months(dates, tl month)

							  
fun get_nth (words:string list, index: int) =
    let
	fun return_word(word: string list, iter: int) =
	    if iter = index
	    then hd word
	    else return_word(tl word, iter+1)
    in
	return_word(words, 1)
    end


fun  date_to_string (date: int*int*int)=
     let val date_string = ["January ", "February ", "March ", "April ","May ", "June ", "July ", "August ", "September ", "October ", "November ", "December "]
     in
	 get_nth(date_string, #2 date) ^ Int.toString (#3 date) ^ ", " ^ Int.toString(#1 date)
     end
	 

fun number_before_reaching_sum1 (sum: int, nums: int list) =
    if hd nums > sum then 0
    else let
	fun finding_number(sm: int, num_list: int list) =
	    let
		val total = sm + hd(tl num_list)
	    in
		if total >= sum then 0
		else 1 + finding_number(total, tl num_list)
	    end
    in
	1 + finding_number(hd nums, nums)
    end

fun number_before_reaching_sum (sum: int, nums: int list) =
    if hd nums > sum
    then 0
    else 1 + number_before_reaching_sum (sum - (hd nums), nums )
	


fun what_month(day: int) =
    let val month_days = [31,28,31,30,31,30,31,31,30,31,30,31];
    in
	if day <= 31 then 1
	else 1 + number_before_reaching_sum (day, month_days)
    end

fun month_range(day1: int, day2: int)=
    if day1>day2 then []
    else
	what_month(day1)::month_range(day1+1, day2)
	
    
fun oldest(days: (int*int*int) list) =
    if null days then NONE
    else let
	fun max(xs: (int * int* int) list) =
	    if null (tl xs)
	    then hd xs
	    else let val tl_ans = max(tl xs)
		 in
		     if is_older(hd xs, tl_ans)
		     then hd xs
		     else tl_ans
		 end
    in
	SOME (max days)
    end


fun remove_dups(vals: int list) =
    if null (tl vals) then hd vals::[]
    else
	let
	    val tl_vals = remove_dups(tl vals)
	in
	    let
		fun search_list(values: int list, item: int) =
		    if null values then false
		    else if item = hd values then true
		    else search_list(tl values, item)
	    in
		if search_list (tl_vals, hd vals)
		then tl_vals
		else hd vals :: tl_vals
	    end
	end

	    
fun number_in_months_challenge(dates: (int * int* int) list, months: int list) =
    if null months then 0
    else
	number_in_months(dates,remove_dups(months))

fun dates_in_months_challenge(dates: (int*int*int) list, months: int list) =
    if null months then []
    else
	dates_in_months(dates, remove_dups(months))

fun reasonable_date(date: int * int * int) =
    if #1 date <= 0 then false
    else if #2 date <= 0 orelse #2 date > 12 then false
    else
	(* handle dates now *)
	let val month = #2 date;
	    val day = #3 date
	    val year = #1 date
	in
	    if month = 4 orelse month = 6 orelse month = 9 orelse month = 11 then
		day > 0 andalso day < 30
	    else if month <> 2 then
		day > 0 andalso day<31
	    else
		let
		    fun check_leap_year(year: int) =
			if year mod 400 = 0 then true
			else if year mod 4  = 0 andalso year mod 100 <> 0
			then true
			else false
		in
		    if check_leap_year(year)
		    then day >0 andalso day <= 29
		    else day >0 andalso day <= 28
		end
	end
	    
	
		   
