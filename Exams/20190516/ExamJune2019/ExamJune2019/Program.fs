type Peano =
| O
| S of Peano

// Q1.1
let toInt =
    let rec aux acc =
        function
        | O -> acc
        | S p -> aux (acc + 1) p

    aux 0

let fromInt =
    let rec aux acc =
        function
        | 0 -> acc
        | n -> aux (S acc) (n-1)

    aux O

// Q1.2
let rec add (p1 : Peano) (p2 : Peano) : Peano =
    match p1 with
    | O -> p2
    | S p' -> add p' (S p2)

let rec mult p1 p2 =
    match p1 with
    | O -> O
    | S O -> p2 
    | S p' -> mult p' (add p2 p2)

let rec pow p1 p2 =
    match p2 with
    | O -> S O
    | S O -> p2 
    | S p' -> pow (mult p1 p1) p'



// Quesiton 2

// Q2.1

// Q2.1.1 
// f : 'a -> 'a list -> 'a list option
// g : 'a list -> 'a list -> bool

// Q2.1.2
// f removes the element x from a list, if the element is the first or second element, and returns it as an option. If not it returns None.
// g 