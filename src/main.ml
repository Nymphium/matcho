open Ast
open Eval

let () =
  (*
   * match [3; 4] with
   * | [p; x] when p = 2 -> x
   * | [3; x] -> x
   * | x :: _ -> x
   * | [] -> 0
   *)
  print_endline @@ show_v @@ eval [] @@
  Match(Data("Cons", [Int 3; Data("Cons", [Int 4; Data("Nil", [])])]), [
      Pwhen(Pdata("Cons", [Pvar "p"; Pdata("Cons", [Pvar "x"; Pdata("Nil", [])])]), BinOp(`Eq, Var "p", Int 2),  Var "x");
      P(Pdata("Cons", [Pint 3; Pdata("Cons", [Pvar "x"; Pdata("Nil", [])])]),  BinOp(`Add, Var "x", Int 10));
      P(Pdata("Cons", [Pvar "x"; Pvar "_"]), Var "x");
      P(Pdata("Nil", []), Int 0)
    ])
