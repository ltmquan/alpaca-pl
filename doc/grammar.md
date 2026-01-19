# Alpaca Grammar Documentation

```ebnf
prog = { proc_def } ;

proc_def = id local_proc "=" proc ;

local_proc = "[" id { "," id } "]" ;

proc = par_proc | seq_proc ;

par_proc = "|" seq_proc { "|" seq_proc } ;

seq_proc = "{" { stmt } "}" ;

stmt = ( wait_op | id ) expr "." ;

wait_op = "$" [ id ] [ ":" id] ;

expr = cast_op expr | "(" expr ")" | id | lit ;

cast_op = "int" | "str" | "id" ;

id = ( alpha | "_" ) { alnum | "_" } ;

lit = list_lit | int_lit | char_lit | str_lit | "nil" ;

list_lit = "[" expr { "," expr } "]" ;

int_lit = digit { digit } ;

char_lit = "\'" str_char "\'" ;

str_lit = "\"" { str_char } "\"" ;

str_char = ~["\"" | "\\"] | "\\" escape_char ;

escape_char = "n" | "t" | "r" | "\\" | "\"" | "'" ;


```

add_mult [r0, r1] =
| {
    $ [a, b, c, rep] .
    + [a, b, r0] .
    r0 [c, rep] .
}
| {
    $r0:add_mult [c, rep] .
    $ res .
    * [res, c, r1] .
    r1 rep .
}
| {
    $r1:r0 rep
    $r1 res .
    rep res .
}

main [c] = {
    + [a, b, c] .
}
| {
    $c res .
    << res .
}
