use "soln.sml";

val test1 = only_capitals ["A","B","C"] = ["A","B","C"]

val test2 = longest_string1 ["A","bc","C"] = "bc"

val test3 = longest_string2 ["A","bc","C"] = "bc"

val test4a = longest_string3 ["A","bc","C"] = "bc"

val test4b = longest_string4 ["A","B","C"] = "C"

val test5 = longest_capitalized ["A","bc","C"] = "A"

val test6 = rev_string "abc" = "cba"

val test7 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4

val test8 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE

val test9a = count_wildcards Wildcard = 1

val test9b = count_wild_and_variable_lengths (Variable("a")) = 1

val test9c = count_some_var ("x", Variable("x")) = 1

						       
val test10 = check_pat (Variable("x")) = true

val test11 = match (Const(1), UnitP) = NONE

val test12 = first_match Unit [UnitP] = SOME []

val test13 = first_match (Tuple[Const 17,Unit,Const 4,Constructor ("egg",Const 4),Constructor ("egg",Constructor ("egg",Const 4)),Tuple[Const 17,Unit,Const 4,Constructor ("egg",Const 4),Constructor ("egg",Constructor ("egg",Const 4))],Tuple[Unit,Unit],Tuple[Const 17,Const 4],Tuple[Constructor ("egg",Const 4),Constructor ("egg",Const 4)]]) [ConstP 17,ConstP 4,ConstructorP ("egg",ConstP 4),ConstructorP ("egg",ConstructorP ("egg",ConstP 4)),TupleP[ConstP 17,Wildcard,ConstP 4,ConstructorP ("egg",ConstP 4),ConstructorP ("egg",ConstructorP ("egg",ConstP 4))],TupleP[Wildcard,Wildcard],TupleP[ConstP 17,ConstP 4],TupleP[ConstructorP ("egg",ConstP 4),ConstructorP ("egg",ConstP 4)],TupleP[ConstP 17,Wildcard,ConstP 4,ConstructorP ("egg",ConstP 4),ConstructorP ("egg",ConstructorP ("egg",ConstP 4)),TupleP[ConstP 17,Wildcard,ConstP 4,ConstructorP ("egg",ConstP 4),ConstructorP ("egg",ConstructorP ("egg",ConstP 4))],TupleP[Wildcard,Wildcard],TupleP[ConstP 17,ConstP 4],TupleP[ConstructorP ("egg",ConstP 4),ConstructorP ("egg",ConstP 4)]]] = SOME([])					     


