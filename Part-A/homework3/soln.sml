(* Question 1 *)			       

fun only_capitals xs = List.filter (fn s => Char.isUpper(String.sub(s,0))) xs

(* Question 2*)				   
fun longest_string1 xs =
    foldl (fn (s1,s2) => if String.size s1 > String.size s2 then s1 else s2) "" xs

(* Question 3*)	  
fun longest_string2 xs =
    foldl (fn (s1,s2) => if String.size s1 >= String.size s2 then s1 else s2) "" xs



(* Question 4*)	  
fun longest_string_helper f xs =
    foldl (fn (s1,s2) =>  if f(String.size s1, String.size s2) then s1 else s2) "" xs

val longest_string3 = longest_string_helper (fn (l1,l2) => l1 > l2)
val longest_string4 = longest_string_helper (fn (l1,l2) => l1 >= l2) 	      	       

(* Question 5*)	  			       
val longest_capitalized = longest_string1 o only_capitals

(* Question 6*)
val rev_string =  implode o List.rev o explode 


(* Question 7 *)
exception NoAnswer					   
fun first_answer f [] = raise NoAnswer
  | first_answer f (x::xs) =
    case f x of
	NONE => first_answer f xs
      | SOME v => v


(* Question 8 *)
		      
fun all_answers f xs =
    let
	fun aux (xs,acc) =
	    case xs of
		[] => SOME acc
	      | x::xs' => case f x of
			      SOME v => aux(xs',acc@v)
			    | NONE => NONE
    in
	aux(xs,[])
    end


datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu


fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end
	    
(* Question 9.a *)
val count_wildcards  = g (fn _ => 1) (fn x => 0) 


(* Question 9.b*)			       
val count_wild_and_variable_lengths  = g (fn _ => 1) String.size

(* Question 9.c *)					 
fun count_some_var (s,v) = g (fn _ => 0) (fn s' => if s'=s then 1 else 0) v
			     
(*Question 10 *)
val check_pat =
    let
	fun extract_v p =
	    case p of
		Wildcard => []
	      | Variable x => [x]
	      | TupleP ps => List.foldl (fn (x,y) => extract_v(x)@y) [] ps
	      | ConstructorP (_,p) => extract_v p
	      | _ => []

	fun does_repeat xs =
	    case xs of
		[] => true
	      | x::xs' => if List.exists (fn s => s = x) xs'
			  then false
			  else does_repeat xs'
    in
	does_repeat o extract_v
    end

(* Question 11 *)

fun match xs =
    case xs of
	(Constructor(s2,v),ConstructorP(s1,p)) => if s1 <> s2 then NONE
						  else match(v,p)
      | (v,Variable s) => SOME [(s,v)]
      | (Unit,UnitP) => SOME []
      | (_,Wildcard) => SOME []
      | (Const v, ConstP s) => if v = s then SOME[] else NONE
      | (Tuple vs, TupleP ps) => if length vs <> length ps
				 then NONE
				 else all_answers match (ListPair.zip(vs,ps))
      | _ => NONE

(* Question 12 *)
fun first_match v pl =  (SOME o first_answer (fn z => match (v,z))) pl
			handle NoAnswer => NONE

