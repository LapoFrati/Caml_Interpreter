(* Ambiente e operazioni *)

     type 't env = string -> 't 
     exception WrongBindlist 
     let emptyenv(x) = function (y:string) -> x
     let applyenv(x,(y:string)) = x y
     let bind((r: 'a env) , (l:string),  (e:'a)) = 
          function lu -> if lu = l then e else applyenv(r,lu)
     let rec bindlist(r, il, el) = match (il,el) with
        | ([],[]) -> r
	| i::il1, e::el1 -> bindlist (bind(r, i, e), il1, el1)
	| _ -> raise WrongBindlist

(* SYNTACTIC DOMAINS *)

    type ide = string

    and intList = Nil | Cons of int * intList

(*
	(Espressioni)
	E ::= ide
	| Val (Valori)
	| E and E | E or E | not E (Espressioni Booleane)
	| OP(E,E) (Espressioni su interi con OP∈{"+","-","*","=","<="})
	| E <= E (operatore di confronto tra intList: "sottolista iniziale")
	| E  == E (Operatore di uguaglianza per intList)
	| isEmpty(E) (Operatore logico su intList: controlla se la lista E è vuota)
	| E @ E (Append di intList)
	| if E then E else E (Condizionale)
	| let Ide = E in E (Blocco let)
	| E(E) (Applicazione Funzionale)
	| map E on E (Applica la funzione E a tutti gli elementi della lista)
*)
	and exp = Den of ide
	 | Eval of eval
	 | And of exp * exp | Or of exp * exp | Not of exp
   | Op of string * exp * exp
   | Subl of exp * exp
   | Eq of exp * exp
   | IsEmpty of exp
	 | Append of exp * exp
	 | IfThenElse of exp * exp * exp
	 | LetIn of ide * exp * exp
	 | Apply of exp * exp
	 | MapOn of exp * exp 

(* (Valori)
   Val::= Int (Interi)  	
   | intList (Liste	di interi)	
   | True | False (Valori	Booleani)	
   | fun Ide-­‐>E (Funzioni con un solo parametro, non ricorsive)	
*)  
	and closure = (ide -> exp) * eval env

    and eval = Int of int
     | List of intList
     | True
     | False
     | Fun of closure

     (* Operations on Eval *)
    let myand (x,y) =
   	(match (x,y) with
   	| (True, True) -> True
   	| (False, False) -> False
    | (True, False) -> False
    | (False, True) -> False
    |_ -> failwith ("type error"))

    let myor (x,y) =
   	(match (x,y) with
   	| (True, True) -> True
   	| (False, False) -> False
    | (True, False) -> True
    | (False, True) -> True
    |_ -> failwith ("type error"))
      
    let mynot x =
   	(match x with
   	| True -> False
   	| False -> True 
    |_ -> failwith ("type error"))

    let equ (x,y) =
   	(match (x,y) with
   	| (Int(u), Int(w)) -> if(u=w) then True else False
   	| (True, True) -> True
   	| (False, False) -> True
    | (True, False) -> False
    | (False, True) -> False
    | _ -> failwith ("type error"))
 
    let plus (x,y) =
   	(match (x,y) with
   	| (Int(u), Int(w)) -> Int(u+w)
    | _ -> failwith ("type error"))

      
    let diff (x,y) =
   	(match (x,y) with
   	| (Int(u), Int(w)) -> Int(u-w)
    | _ -> failwith ("type error"))

      
    let mult (x,y) =
   	(match (x,y) with
   	| (Int(u), Int(w)) -> Int(u*w)
    |_ -> failwith ("type error"))

    (*verifica ricorsivamente che l2 sia sottolista iniziale di l1*)
   	let rec subl(l1, l2) = match (l1, l2) with
   		| (Cons(x,xs), Cons(y,ys)) when x = y -> subl( xs, ys)
   		| (Cons(x,xs), Cons(y,ys)) when x <> y -> False
   		| (Cons(x,xs), Nil)                -> True
   		| _ -> failwith ("error")
    
    (* controlla che i tipi siano corretti*)
   	let checksubl (x,y) =
   	(match (x,y) with
    | (List(l1),List(l2)) -> subl(l1,l2)
    | _ -> failwith("type error"))

    (* Controlla che le liste l1 ed l2 siano uguali elemento per elemento*)
    let rec equList (l1, l2) = match (l1, l2) with
    	| (Nil,Nil) -> True
    	| (Nil, Cons(y,ys)) -> False
    	| (Cons(x,xs), Nil) -> False
      | (Cons(x,xs), Cons(y,ys)) when x <> y -> False
    	| (Cons(x,xs), Cons(y,ys)) when x = y -> equList(xs,ys)
      | _ -> failwith("error")

    (* controlla che i tipi siano corretti*)
    let checkequList (e1, e2) = match (e1, e2) with
    	| (List(l1), List(l2)) -> equList(l1, l2)
    	| _ -> failwith ("type error")

    (*Scorre l1 fino al termine e poi aggiunge l2*)
    let rec myappend(l1,l2) = match (l1, l2) with
    	| (_, Nil) -> l1
    	| (Nil,_) -> l2
    	| (Cons(x,Nil), _) -> Cons(x,l2)
    	| (Cons(x,xs),_) -> myappend(xs,l2) 

let rec  sem ((e:exp), (r)) =
      match e with
      | Den(i) -> applyenv(r,i)
      | Eval(n) -> n
      | And(e1,e2) -> myand(sem(e1,r), sem(e2,r))
      | Or(e1,e2) -> myor(sem(e1,r), sem(e2,r))
      | Not(e1) -> mynot(sem(e1,r))
      | Op(op, e1, e2) -> ( match op with
                						| ("+") -> plus(sem(e1,r), sem(e2,r))
                						| ("*") -> mult(sem(e1,r), sem(e2,r))
                						| ("-") -> diff(sem(e1,r), sem(e2,r))
                						| ("=") -> equ(sem(e1,r), sem(e2,r))
                						| ("<=") -> checksubl(sem(e1,r), sem(e2,r))
                						| _ -> failwith("operation not supported"))
      | Subl(e1, e2) -> checksubl(sem(e1,r), sem(e2,r))
      | Eq(e1,e2) -> checkequList(sem(e1,r), sem(e2,r))
      | IsEmpty(e1) -> (match sem(e1,r) with
              					| List(Nil) -> True
              					| List(_) -> False
          						  | _ -> failwith ("type error"))
      | Append(e1, e2) -> (match (sem(e1,r), sem(e2,r)) with
            							| (List(l1), List(l2)) -> List( myappend(l1,l2)) 
            							| _ -> failwith("type error"))
      | IfThenElse(e1, e2, e3) -> (match sem(e1,r) with (*controlla che il tipo nei due rami coincida*)
                                    | True -> sem(e2,r)
                                    | False -> sem(e3,r)
                                    | (_) -> failwith("branch type mismatch"))
      | LetIn(id, e1, e2) -> let newenv = bind(r, id, sem(e1,r) ) in sem( e2, newenv) (*aggiungo il nuovo bind ed eseguo la sem di e2 nel nuovo ambiente*)
      | Apply(e1, e2) -> ( match (sem(e1,r)) with (*controllo che e1 sia una Fun*)
                          | Fun(f1, env) -> let newenv = bind(env, "par", sem(e2,r)) (* Bindo la sem di e2 al paramentro formale par nell'ambiente env della chiusura*)
                                              in sem(f1 "par", newenv) (*Valuto la sem della funzione applicata al paramentro par, nell'ambiente in cui par è associato alla sem di e2*)
                          | _ -> failwith("type error"))
	    | MapOn(e1,e2) -> (let rec mymap(e1, l) = match l with
                          					    	      | Nil -> Nil
                          							        | Cons(x,xs) -> match sem(Apply(e1,Eval(Int(x))),r) with
                          											| Int(z) -> Cons(z, mymap(e1,xs))
                          											| _ -> failwith ("type error") 
              						in
              							match (sem(e1,r),sem(e2,r)) with
              		  						| (Fun(f), List(l)) -> List(mymap( e1, l))
              		  						| _ -> failwith("type error"))
