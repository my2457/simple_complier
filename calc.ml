open Ast

let pt= Array.make 10 0;;
let rec eval = function 
    Lit(x) -> x
  | Par(e1) -> eval e1
  | Seq(e1, e2) -> (ignore(eval e1); let v2= eval e2 in v2)
  | Var(u) -> pt.(u)
  | Asn(e1, e2) -> (let v2=eval e2 in (pt.(e1)<-v2; v2))
  | Binop(e1, op, e2) ->
      let v1 = eval e1 and v2 = eval e2 in
      match op with
	Add -> v1 + v2
      | Sub -> v1 - v2
      | Mul -> v1 * v2
      | Div -> v1 / v2


let _ =
  let lexbuf = Lexing.from_channel stdin in
  let expr = Parser.expr Scanner.token lexbuf in
  let result = eval expr in
  print_endline (string_of_int result)
