open Term

open Camlp4.PreCast
module LambdaGram = MakeGram(Lexer)
 
let term = LambdaGram.Entry.mk "term"
let prog = LambdaGram.Entry.mk "full_program"

let () = EXTEND LambdaGram
  GLOBAL: term prog;
  prog: [[ "("; "lambda"; "("; x = id; ")"; e = term; ")"; `EOI -> ((* x, *) e) ]];
  term: [[
       "0" -> C0
     | "1" -> C1
     | x = id -> Var x
     | "("; op = op1; e = term; ")" -> Op1 (op, e)
     | "("; op = op2; e1 = term; e2 = term; ")" -> Op2 (op, e1, e2)
     | "("; "if0"; p = term; a = term; b = term; ")" -> If0 (p, a, b)
     | "("; "fold"; e0 = term; e1 = term;
                   "("; "lambda"; "("; x = id; y = id; ")"; e2 = term; ")";
       ")" -> Fold (e0, e1, ((* x, y, *) e2))
  ]];
  id: [[ v = LIDENT -> ignore v; 0 ]];
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
