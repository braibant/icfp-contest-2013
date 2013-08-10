open Term

open Camlp4.PreCast
module LambdaGram = MakeGram(Lexer)
 
let term_eoi = LambdaGram.Entry.mk "term"
let prog = LambdaGram.Entry.mk "full_program"

let () = EXTEND LambdaGram
  GLOBAL: term_eoi prog;
  prog: [[ "("; "lambda"; "("; x = id; ")"; e = term; ")"; `EOI -> `Lam (x, e) ]];
  term_eoi: [[ t = term; `EOI -> t ]];
  term: [[
       `INT(0, _) -> `C0
     | `INT(1, _) -> `C1
     | x = id -> `Var x
     | "("; op = op1; e = term; ")" -> `Op1 (op, e)
     | "("; op = op2; e1 = term; e2 = term; ")" -> `Op2 (op, e1, e2)
     | "("; "if0"; p = term; a = term; b = term; ")" -> `If0 (p, a, b)
     | "("; "fold"; e0 = term; e1 = term;
                   "("; "lambda"; "("; x = id; y = id; ")"; e2 = term; ")";
       ")" -> `Fold (e0, e1, `Lam2 (x, y, e2))
  ]];
  id: [[ v = LIDENT -> v ]];
  op1: [[
    "not" -> Not
  | "shl1" -> Shl1
  | "shr1" -> Shr1
  | "shr4" -> Shr4
  | "shr16" -> Shr16
  ]];
  op2: [[
    "and" -> And
  | "or" -> Or
  | "xor" -> Xor
  | "plus" -> Plus
  ]];
END;;

type vars = { 
  arg: string option ref;
  fold_acc: string option ref;
  fold_arg: string option ref;
}

let set v id = match !v with
  | None -> v := Some id
  | Some id' -> failwith "Parser.set"

let rec collect v = function
  | `C0 -> ()
  | `C1 -> ()
  | `Var _ -> ()
  | `If0 (e,a,b) -> collect v e; collect v a; collect v b
  | `Op1 (_op, e) -> collect v e
  | `Op2 (_op, e1, e2) -> collect v e1; collect v e2
  | `Fold (e0, e1, `Lam2 (x, y, e2)) ->
    set v.fold_arg x;
    set v.fold_acc y;
    collect v e0;
    collect v e1;
    collect v e2;
    ()

let collect_prog (`Lam (x, e)) =
  let v = { arg = ref (Some x); fold_acc = ref None; fold_arg = ref None } in
  collect v e;
  v

let eq v id = match !v with
  | None -> false
  | Some id' -> id = id'

let bind v t =
  let get_id x =
    if eq v.arg x then Constants.arg
    else if eq v.fold_acc x then Constants.fold_acc
    else if eq v.fold_arg x then Constants.fold_arg
    else failwith "Parser.get_id" 
  in
  let rec bind =
    let open Notations in
    function
    | `C0 -> c0
    | `C1 -> c1
    | `Var x ->
	if get_id x = Constants.arg then mk_arg
	else if get_id x = Constants.fold_acc then mk_facc
	else if get_id x = Constants.fold_arg then mk_farg
	else assert false
    | `If0 (e1, e2, e3) -> if0 (bind e1) (bind e2) (bind e3)
    | `Fold (e0, e1, `Lam2 (_x, _y, e2)) -> fold (bind e0) (bind e1) (bind e2)
    | `Op1 (op, e) -> op1 op (bind e)
    | `Op2 (op, e1, e2) -> op2 op (bind e1) (bind e2)
  in bind t

let bind_prog v (`Lam (_x, e)) = bind v e

let prog_of_string str =
  try
    let term_with_vars = LambdaGram.parse_string prog (Loc.mk "lalala") str in
    let vars = collect_prog term_with_vars in
    bind_prog vars term_with_vars
  with Loc.Exc_located (loc, exn) ->
    Loc.print Format.err_formatter loc;
    Format.eprintf "\nError %S in the parser\n%s%!" (Printexc.to_string exn) str;
    raise exn
