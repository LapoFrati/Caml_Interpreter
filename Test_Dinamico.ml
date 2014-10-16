
#use "Prog_Ocaml_Dinamico.ml";;

#use "assert.ml";;

print_endline ("Testing:");;

let r = bindlist(emptyenv(Int(0)),["x";"y";"t";"f";"l";"f1";"g1"],[Int(4);Int(5);True;False;List(Cons(3,Cons(2,Cons(1,Nil))));Fun( fun b -> Op("+",Den(b), Eval(Int(1))));Fun( fun b -> Op("+",Den(b), Den("x")))]);;

print_endline ("Env:");;
print_endline ("x -> 4");;
print_endline ("y -> 5");;
print_endline ("t -> True");;
print_endline ("f -> False");;
print_endline ("l -> [3;2;1]");;
print_endline ("f1(b) -> b+1");;
print_endline ("g1(b) -> b+x");;

let closure = (Fun( fun b -> Op("+",Den(b), Eval(Int(1)))), r);;

let test () : bool = sem(Den("x"),r) = Int(4)
;; run_test " Sem Den(x) -> 4 " test

let test () : bool = sem(Den("t"),r) = True
;; run_test " Sem Den(t) -> True " test

let test () : bool = sem(Den("l"),r) = List(Cons(3,Cons(2,Cons(1,Nil))))
;; run_test " Sem Den(l) -> [3;2;1] " test

let test () : bool = sem(Eval(Int(6)),r) = Int(6)
;; run_test " Sem Eval(6) -> 6 " test

let test () : bool = sem(And(Eval(True),Den("t")),r) = True 
;; run_test " True and True -> True " test

let test () : bool = sem(And(Eval(True),Eval(False)),r) = False 
;; run_test " True and False -> False " test

let test () : bool = sem(Or(Eval(True),Den("t")),r) = True 
;; run_test " True or True -> True " test

let test () : bool = sem(Or(Eval(True),Eval(False)),r) = True 
;; run_test " True or False -> True " test

let test () : bool = sem(Or(Eval(False),Eval(False)),r) = False 
;; run_test " False or False -> False " test

let test () : bool = sem(Not(Eval(True)),r) = False 
;; run_test " not True -> False " test

let test () : bool = sem(Op("+", Eval(Int(2)), Den("x")),r) = Int(6) 
;; run_test " Op: 2 + 4 -> 6 " test

let test () : bool = sem(Op("*", Eval(Int(2)), Eval(Int(3))),r) = Int(6) 
;; run_test " Op: 2 * 3 -> 6 " test

let test () : bool = sem(Op("-", Eval(Int(3)), Eval(Int(2))),r) = Int(1) 
;; run_test " Op: 3 - 2 -> 1 " test

let test () : bool = sem(Op("=", Eval(Int(3)), Eval(Int(3))),r) = True 
;; run_test " Op: 3 = 3 -> True " test

let test () : bool = sem(Op("<=", Den("l"), Eval(List(Cons(3,Cons(2,Nil))))),r) = True
;; run_test " Op: [3;2] <= [3;2;1] -> True " test

let test () : bool = sem(Op("<=", Den("l"), Eval(List(Cons(4,Cons(5,Nil))))),r) = False
;; run_test " Op: [4;5] <= [3;2;1] -> False " test

let test () : bool = sem(Eq( Den("l"), Eval(List(Cons(3,Cons(2,Nil))))),r) = False
;; run_test " [3;2] == [3;2;1] -> False " test

let test () : bool = sem(Eq( Den("l"), Den("l")),r) = True
;; run_test " [3;2;1] == [3;2;1] -> True " test

let test () : bool = sem(IsEmpty(Eval(List(Nil))),r) = True
;; run_test " isEmpty [] -> True " test

let test () : bool = sem(IsEmpty(Den("l")),r) = False
;; run_test " isEmpty [3;2;1] -> False " test

let test () : bool = sem(Append( Eval(List(Cons(0,Nil))), Den("l")),r) = List(Cons(0,Cons(3,Cons(2,Cons(1,Nil)))))
;; run_test " [0]@[3;2;1] -> [0;3;2;1] " test

let test () : bool = sem(IfThenElse(Eval(True),Den("x"), Den("y")),r) = Int(4)
;; run_test " if True then 4 else 5 -> 4 " test

let test () : bool = sem(IfThenElse(Eval(False),Den("x"), Den("y")),r) = Int(5)
;; run_test " if False then 4 else 5 -> 4 " test

let test () : bool = sem( LetIn ( "z", Eval(Int(6)) , Op("+", Den("z"), Den("x"))),r) = Int(10)
;; run_test " Let z = 6 in z + 4 -> 10 " test

let test () : bool = sem( Apply( Den("f1"), Op("+", Eval(Int(2)), Eval(Int(39)))), r) = Int(42)
;; run_test " f1( 39+2 ) -> 42 " test

let test () : bool = sem( Apply( Den("g1"), Eval(Int(38))), r) = Int(42)
;; run_test "{x = 4} g1( 38 ) -> 42 " test

let test () : bool = sem( LetIn ( "x", Eval(Int(100)) , Apply( Den("g1"), Eval(Int(38)))),r) = Int(138)
;; run_test "let x = 100 in g1( 38 ) -> 138 " test

let test () : bool = sem(MapOn(Den("f1"),Den("l")),r) = List(Cons(4,Cons(3,Cons(2,Nil))))
;; run_test " Map f1 on [3;2;1] -> [4;3;2] " test
