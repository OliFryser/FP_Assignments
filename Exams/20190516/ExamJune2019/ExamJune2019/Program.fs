type Peano =
| O
| S of Peano

// Q1.1
let toInt : Peano -> uint32 =
    let rec aux acc =
        function
        | O -> acc
        | S p -> aux (acc + 1u) p

    aux 0u

let fromInt =
    let rec aux acc =
        function
        | 0u -> acc
        | n -> aux (S acc) (n-1u)

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
    | S p' -> add p2 (mult p' p2)

let rec pow p1 p2 =
    match p2 with
    | O -> S O
    | S O -> p1
    | S p' -> mult p1 (pow p1 p')


// Q1.3
let tailAdd a (b : Peano) =
    let rec aux a' acc =
        match a' with
        | O -> acc
        | S p' -> aux p' (S acc)
    
    aux a b

let tailMult a b =
    let rec aux a' acc =
        match a' with
        | O -> O
        | S O -> tailAdd b acc
        | S p' -> aux p' (tailAdd b acc)

    aux a O
  
let tailPow a b =
    let rec aux b' acc =
        match b' with
        | O -> S O
        | S O -> acc
        | S p' -> aux p' (tailMult a acc)

    aux b a


// Q1.4
let rec loop (f : ('a -> 'a)) (acc : 'a) (p : Peano) : 'a =
   match p with
   | O -> acc
   | S p' -> loop f (f acc) p'
   
// Q1.5
let loopAdd a b =
    loop (fun p -> S p) a b

let loopMult a b =
    loop (fun p -> loopAdd p b) O a 

let loopPow a b =
    if b = O then S O 
    else loop (fun p -> loopMult p a) (S O) b

// Quesiton 2

// Q2.1

// Q2.1.1 
// f : 'a -> 'a list -> 'a list option
// g : 'a list -> 'a list -> bool

// Q2.1.2
// f removes the element x from a list, if the element is the first or second element, and returns it as an option. If not it returns None.
// g 