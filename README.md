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