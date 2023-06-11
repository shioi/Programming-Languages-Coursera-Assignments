fun same_string(s1 : string, s2 : string) =
    s1 = s2

datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove


(* Question 1 *)

fun all_except_option (str, lst) =
    case lst of
	[] => NONE
      | x::xs => if same_string (x,str)
		 then SOME(xs)
		 else let val res = all_except_option(str,xs)
		      in
			  case res of
			      SOME(xs') => SOME(x::xs')
			    | NONE => NONE
		      end

			  
fun get_substitutions1 (lst, str) =
    case lst of
	[] => []
      | x::xs =>  let val res = all_except_option (str, x)
		  in
		      case res of
			  SOME(xs') => xs'@get_substitutions1 (xs,str)
			| _ => get_substitutions1 (xs,str)
		  end



(* question 3: tail recursion*)
fun get_substitutions2 (lst, str) =
    let fun aux (lst, acc) =
	    case lst of
		[] => acc
	      | x::xs => case all_except_option(str,x) of
			     SOME(x) => aux(xs,x@acc)
			   | _ => aux(xs,acc)
    in
	aux(lst,[])
    end

(* question 4*)
fun similar_names (lst, {first=x, middle= y, last= z}) =
    let
	val subs = get_substitutions1(lst, x)
	fun aux (lst) =
	    case lst of
		[] => []
	      | x'::xs => {first= x', middle=y, last= z} :: aux (xs)
    in	{first=x,middle=y,last=z}::aux (subs)
    end
	
(* Question 5 *)
	
fun card_color (st , rank) =
    case st of
	Spades => Black
      | Clubs => Black
      | _ => Red

		 
fun card_value (_, rank) =
    case rank of
	Ace => 11
      | Num n => n
      | _ => 10


fun remove_card (cs, c, e) =
    case cs of
	[] => raise e
      | x::xs => if x=c then xs else x::remove_card(xs,c,e)

fun all_same_color cardlist =
    case cardlist of
	[] => true
      | _::[] => true
      | x::y::xs => card_color(x) = card_color(y) andalso all_same_color(y::xs)
	
	    
fun sum_cards cardlist =
    let
	fun aux (cardlist, acc) =
	    case cardlist of
		[] => acc
	      | x::xs => aux(xs,card_value(x)+acc)
    in
	aux (cardlist, 0)
    end

fun score (cardlist, goal) =
    let val sum = sum_cards(cardlist)
    in
	if sum > goal then
	    if all_same_color(cardlist) then 3*(sum-goal) div 2
	    else 3*(sum - goal)
	else if all_same_color(cardlist) then
	    (goal - sum) div 2
	else (goal - sum)

    end
	  
(* 2.g *)

fun officiate (cardlist, movelist, goal) =
    let
	fun game (cardlist, moves,heldlist) =
	    case moves of
		[] => score(heldlist,goal)
	      | Discard c::xs => game(cardlist,xs,remove_card(heldlist, c, IllegalMove))
	      | Draw::xs => case cardlist of
			    [] => score (heldlist,goal)
			     | x::xs' => let val newlist = x::heldlist
					 in
					     if sum_cards(newlist) > goal
					     then score(newlist,goal)
					     else game(xs',xs,newlist)
			     end
    in
	game(cardlist, movelist, [])
    end
	
(*------------------------------------------------------------------------------------------*)

	


	  	


	
