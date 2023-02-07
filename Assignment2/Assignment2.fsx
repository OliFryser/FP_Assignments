// Exercise 2.1
let downto1 n = 
    if n = 0 then []
    else [n .. -1 .. 1];;

downto1 5;;

let downto2 = function
    | 0 -> []
    | n -> [n .. -1 .. 1];;


// Exercise 2.2
let rec removeOddIdx = function
    | x0::_::xs -> x0::(removeOddIdx xs)
    | x -> x;;

removeOddIdx ([] : int list);;
removeOddIdx [true];;
removeOddIdx ["Marry"; "had"; "a"; "little"; "lamb"; "its"; "fleece"; "was"; "white"; "as"; "snow"];;

// Execise 2.3
let rec combinePair = function
    | [] -> []
    | x0::x1::xs -> (x0, x1)::(combinePair xs)
    | _ -> [];;

combinePair ([] : int list);;
combinePair [true; false];;
combinePair ["Marry"; "had"; "a"; "little"; "lamb"; "its"; "fleece"; "was"; "white"; "as"; "snow"];;


// Exercise 2.4
type complex = { number : float * float };;
let mkComplex a b = { number = (a,b) };;
let complexToPair c = c.number;;

let (|+|) c1 c2 = 
    let (a, b) = complexToPair c1
    let (c, d) = complexToPair c2
    mkComplex (a+c) (b+d);;

let (|*|) c1 c2 = 
    let (a, b) = complexToPair c1
    let (c, d) = complexToPair c2
    mkComplex (a*c-b*d) (b*c+a*d);;

let (~-.) c1 = 
    let (a, b) = complexToPair c1
    mkComplex -a -b;;

let (~&) c1 = 
    let (a, b) = complexToPair c1
    if a=0.0 || b=0.0 then failwith("a or b is zero") else
    mkComplex ((a/(a*a+b*b)) : float) ((-b/(a*a+b*b)) : float);;

let (|-|) c1 c2 = c1 |+| -.c2;;

let (|/|) c1 c2 = c1 |*| &c2;;

let c1 = {number = (1.0, 2.0)};;
let c2 = {number = (5.0, 4.0)};;


// Exercise 2.5
let explode1 (s : string) = List.ofArray (s.ToCharArray());;
let rec explode2 = function
    | "" -> []
    | s -> s.[0]::(explode2 (s.Remove(0,1)));;


explode1 "";;
explode1 "Hello World!";;
explode2 "";;
explode2 "Hello World!";;

// Exercise 2.6
let implode (cs : char list) = List.foldBack (fun x s -> string x + s) cs "";;
implode [];;
implode ['H'; 'e'; 'l'; 'l'; 'o'; ' '; 'W'; 'o'; 'r'; 'l'; 'd'; '!'];;

let implodeRev (cs : char list) = List.fold (fun x s -> string x + s) "" cs;;