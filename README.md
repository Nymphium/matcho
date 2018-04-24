matcho: lambda calculus, pattern match, adt, and *first class pattern*
===

#thought

```
e :: = ...
    | "match" e "with" "|"? pattern {"|" pattern}*
    | e ("when" e)? "->" e

pattern ::= ...
    | "+" e
```

```ocaml
let h c f =
  match f () with
  | None -> 0
  | +c
  | Some x -> x

let c : (int option, int) case =
  Some i when i > 3 -> i + i

let _ =
  h c @@ fun () -> Some 4
  (*
   * ~>
   *   match Some 4 with
   *   | None -> 0
   *   | Some i when i > 3 -> i + i
   *   | Some x -> x
   *
   * ~> 4 + 4 ~> 8
  *)
```

