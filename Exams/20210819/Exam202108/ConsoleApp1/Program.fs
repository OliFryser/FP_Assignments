//Q1.1
type binList<'a, 'b> =
    | Nil
    | Cons1 of 'a * binList<'a, 'b>
    | Cons2 of 'b * binList<'a, 'b>

let rec length =
    function
    | Nil -> 0
    | Cons1 (_,lst') | Cons2 (_,lst') ->
        1+(length lst')

//Q1.2
let split blst =
    let rec aux lst1 lst2 =
        function
        | Nil -> (List.rev lst1, List.rev lst2)
        | Cons1 (a,blst') -> 
            aux (a::lst1) lst2 blst'
        | Cons2 (b,blst') ->
            aux lst1 (b::lst2) blst'
    
    aux [] [] blst

let length2 blst =
    let rec aux aes bes =
        function
        | Nil -> (aes,bes)
        | Cons1 (_,lst') -> aux (aes+1) bes lst'
        | Cons2 (_,lst') -> aux aes (bes+1) lst'

    aux 0 0 blst

//Q1.3
let rec map f g =
    function
    | Nil -> Nil
    | Cons1 (a,blst') -> Cons1 (f a, map f g blst')
    | Cons2 (b,blst') -> Cons2 (g b, map f g blst')

//Q1.4
let rec filter f g =
    function
    | Nil -> Nil
    | Cons1 (a,blst') -> 
        if (f a) 
        then (Cons1 (a, filter f g blst')) 
        else (filter f g blst')
    | Cons2 (b,blst') -> 
        if (g b) 
        then (Cons2 (b, filter f g blst')) 
        else (filter f g blst')

//Q1.5
let rec fold f g acc =
    function
    | Nil -> acc
    | Cons1 (a,blst') -> fold f g (f acc a) blst'
    | Cons2 (b,blst') -> fold f g (g acc b) blst'


//Q2.1
(*

foo : 'a list -> 'a list -> 'a list
bar : 'a list -> 'a list

bar sorts a list.

*)