# MiniFP

### A syntactically simple interpreted expression-based functional programming language written in Scala


Each MiniFP file is one contiguous expression which uses OCaml's `let ... in` syntax to assign data
```
let double(x) = x * 2 in 
let square(x) = x * x in 
let final =
    square(double(double(100))) in
if final > 10 
    then (final + 2) * 20 
    else 0
```

MiniFP also has features commonly found in functional languages such as pattern matching:

```
let x = "a string" in

let check_if_valid(x) = 
    match x with
    | "some string" -> false
    | "a string" -> true
    | "b string" -> false 
    | _ -> false

in check_if_valid(x)
```
