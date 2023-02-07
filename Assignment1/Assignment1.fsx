// Exercise 1.1
let sqr n = n * n;;

// Exercise 1.2
let pow x n = System.Math.Pow(x,n);;

// Exercise 1.3
let rec sum = function 
| 0 -> 0
| n -> n + sum(n-1);;

// Exercise 1.4
let rec fib = function
| 0 -> 0
| 1 -> 1
| n -> fib(n-1) + fib(n-2);;

// Exercise 1.5
let dup (s : string) = s + s;;

// Exercise 1.6
let rec dupn (s : string) = function 
| 0 -> ""
| n -> s + (dupn s (n-1));;

// Exercise 1.7
let rec bin = function
| (n,k) when k = n || k = 0 -> 1
| (n,k) -> bin(n-1, k-1) + bin(n-1, k);;

// Exercise 1.8
let timediff (h1,m1) (h2,m2) = (h2*60+m2) - (h1*60+m1);;

// Exercise 1.9
let minutes t = timediff (0,0) t;;

// Exercise 1.10
let curry f x y = f (x,y);;
let uncurry f (x,y) = f x y;;

// Exercise 1.11
let empty (letter, pointvalue) pos = (letter, pointvalue);;

// let theLetterA : int -> char * int = empty ('A', 1);;
// theLetterA 0;;


// Exercise 1.12
let add newPos cv word pos = 
    match pos with
    | n when newPos = n -> cv
    | _ -> word pos;;

// Exercise 1.13
let hello pos =
     (empty (char 0, 0) |> add 0 ('H',4) |> add 1 ('E', 1) |> add 2 ('L', 1) |> add 3 ('L', 1) |> add 4 ('O', 1)) pos;;


// Exercise 1.14
let singleLetterScore word pos = 
    let (_, pointvalue) = word pos
    pointvalue;;
let doubleLetterScore word pos = 
    let (_, pointvalue) = word pos
    pointvalue * 2;;
let trippleLetterScore word pos = 
    let (_, pointvalue) = word pos
    pointvalue * 3;;