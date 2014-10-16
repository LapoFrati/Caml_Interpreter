
let execCom (c:com) = semc(c,(emptyenv Unbound),(emptystore Undefined));;
(*  blocchi annidati : 
{ DECL: int x = 1; BODY: x := x +1; 
  	{ DECL: int y = x; BODY: x := y+1 }; 
 x:= x + 1  } *)

let (block2:com) =
Block([("x", Newloc (Eint 1))],                          (* {int x = 1; *) 
[Assign(Den "x",Sum(Val (Den "x"),Eint 1));              (* x := x +1; *)
Block ([("y", Newloc(Val(Den "x")));("z", Newloc(Val(Den "x")))],                     (* {int y = x; *)
[Assign(Den "x",Sum(Val (Den "y"), Eint 1))]);              (* x := y+1 }  *)	
Assign(Den "x",Sum(Val (Den "x"), Eint 5))]);;            (* x:= x + 1  } *)

let (st2: mval store) = execCom(block2);;

applystore(st2, 0);;
applystore(st2, 1);;
(*  - : mval = Mint 4 *)
(* - : mval = Mint 2 *)


let en = emptyenv(Unbound);;
let en1 = bind(en, "x", Dint(4));;
let st = emptystore(Undefined);;
let st1 = update(st, 0, Mint(5));;

Assign(Den "x",Sum(Val (Den "x"),Eint 1));;


let (r,s) = semdv([("x", Newloc (Eint 1))], en, st);;
let (v1,s1) = semden((Den "x"), r, s);;

let loc = match v1 with
	     | Dloc(n) -> n
	     | _ -> failwith "";;

let res = update(s, loc, evaltomval(sem(Sum(Val (Den "x"),Eint 41), r, s)));;

let r = bindlist(emptyenv(Int(0)),["x";"y";"t";"f";"l";"f1"],[Int(4);Int(5);True;False;List(Cons(3,Cons(2,Cons(1,Nil))));Fun( fun b -> Op("+",Den(b), Den("x")))]);;



sem( LetIn ( "x", Eval(Int(10)) , Apply( Den("f1"), Eval(Int(100)))),r);;
sem( Apply( Den("f1"), Eval(Int(100))), r);;

let funzenv = bindlist(emptyenv(Int(0)),["x";"y";"t";"f";"l"],[Int(4);Int(5);True;False;List(Cons(3,Cons(2,Cons(1,Nil))))]);;
let funz = fun b -> Op("+",Den(b), Den("x"));;
let r = bindlist(emptyenv(Int(0)),["x";"y";"t";"f";"l";"f1"],[Int(4);Int(5);True;False;List(Cons(3,Cons(2,Cons(1,Nil)))); Fun(funz, funzenv)]);;

sem( Apply( Den("f1"), Eval(Int(100))), r);;
sem( LetIn ( "x", Eval(Int(10)) , Apply( Den("f1"), Eval(Int(100)))),r);;
