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
// f removes the first occurence of element x in a list and returns 
// Some [list without x] or None if x does not exist in the list.

// g checks if two lists have the same elements in any order. Returns true or false.


// Q2.1.3
// f could be called removeElemFromList
// g could be called checkSameElems


// Q2.2
// The compiler does not know if the list has all cases covered.
// It does, however, but it does not know it, because it is in when clauses.
// The second when clause is however redundant, because they can either be equal or not.

let rec f2 x =
    function
    | []                -> None
    | y::ys when x = y  -> Some ys
    | y::ys             -> 
        match f2 x ys with
        | Some ys' -> Some (y::ys')
        | None     -> None

// Q2.3
let rec fOpt x =
    function
    | []            -> None
    | y::ys when x = y  -> Some ys
    | y::ys             -> 
        fOpt x ys |> Option.map (fun o -> y::o)

let rec gOpt xs =
    function
    | []    -> xs = []
    | y::ys -> 
        fOpt y xs 
        |> Option.map (fun o -> gOpt o ys) 
        |> Option.defaultValue false


// Q2.4

(*

g is the tail-recursive function. Say we are comparing the two lists xs [1;2;] and ys [2;1]

g [1;2] [2;1] 
-> g (f 2 [1;2]) [1]
-> g [1] [1]
-> g (f 1 [1]) []
-> g [] []
-> true

We always call g after all other computations, meaning no calls of g are waiting on other 
calls of g.

*)

let rec fTail (x : 'a) (ys : 'a list) =
    let rec aux (c : 'a list option -> 'a list option) x =
        function
        | [] -> None
        | y::ys when x = y -> c (Some ys)
        | y::ys -> 
            aux (fun lst -> c (y::(Option.get lst) |> Some)) x ys

    aux id x ys
    

// Q3.1
let rec calculatePi (x : uint64) : decimal =
    let rec aux (acc : decimal) =
        function
        | 0UL                  -> 3M+acc
        | x' when x' % 2UL = 0UL  -> 
            aux (acc-(4M/(decimal) ((2UL*x')*(2UL*x'+1UL)*(2UL*x'+2UL)))) (x'-1UL)
        | x' -> 
            aux (acc+(4M/(decimal) ((2UL*x')*(2UL*x'+1UL)*(2UL*x'+2UL)))) (x'-1UL)

    aux 0M x

let piSeq : seq<decimal> =
    Seq.initInfinite (fun n -> calculatePi ((uint64) n))

let circleArea (x : float) =
    let r = (decimal x)
    Seq.map (fun pi -> pi*r*r) piSeq

let sphereVolume (x : float) =
    let r = (decimal x)
    Seq.map (fun pi -> (4M/3M)*pi*r*r*r) piSeq

let circleSphere (r : float) =
    seq {
        (yield (circleArea r))
        (yield (sphereVolume r))
    }