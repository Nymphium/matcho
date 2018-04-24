matcho: lambda calculus, pattern match, adt, and *first class pattern*
===

# Thought: first class pattern

```
e :: = ...
    | "match" e "with" "|"? pattern {"|" pattern}* ["|" ".."]
    | e ("when" e)? "->" e
    | "{<" e+ ">}"

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

## extensible pattern-match
```ocaml
let e : t = (* something *)
let b : u = (* something *)
let m : (t, u) cases -> u =
  match e with
  | p -> b
  | ..

let ps : (t, u) cases = {<
 | p1 -> (* something has type u *)
 | p2 -> (* ... *)
>}
in m extend by ps
(*
 * equilvalent to:
 * match e with
 * | p -> b
 * | p1 -> ...
 * | p2 -> ...
*)
```

... think concrete syntax later

