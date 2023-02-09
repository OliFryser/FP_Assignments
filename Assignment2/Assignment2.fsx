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
type complex = float * float;;
let mkComplex a b = complex(a,b);;
let complexToPair ((a,b) : complex) = (a,b);;

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

let c1 = (1.0, 2.0);;
let c2 = (5.0, 4.0);;


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

let implodeRev (cs : char list) = List.fold (fun s x -> string x + s) "" cs;;
implodeRev ['H'; 'e'; 'l'; 'l'; 'o'; ' '; 'W'; 'o'; 'r'; 'l'; 'd'; '!'];;

// Exercise 2.7
let toUpper1 s = s |> explode2 |> List.map System.Char.ToUpper |> implode;;
let toUpper = explode2 >> List.map System.Char.ToUpper >> implode;;


toUpper "Hello";;
toUpper1 "Hello";;

// Exercise 2.8
let rec ack = function
    | (m,n) when m = 0 -> n + 1
    | (m,n) when m > 0 && n = 0 -> ack(m-1,1)
    | (m,n) when m > 0 && n > 0 -> ack(m-1, ack(m, n-1))
    | (_,_) -> failwith("ack is only defined for non-negative numbers");;


// Exercise 2.9
let time f =
    let start = System.DateTime.Now
    let res = f ()
    let finish = System.DateTime.Now
    (res, finish - start);;

//time (fun () -> ack(3,11));;

let timeArg1 f a = time (fun () -> f a);;

// Exercise 2.10
let rec downto3 f = fun n e ->
    match n with
    | n' when n' > 0 -> downto3 f (n'-1) (f n' e)
    | _ -> e;;

let fac n = downto3 ( * ) n 1;;
let range g n = downto3 (fun elem acc -> (g elem)::acc) n [];;


// Exercise 2.11
type word = (char * int) list;;
let hello = ('H',4)::('E',1)::('L',1)::('L',1)::('O',1)::[];;


// Exercise 2.12
type squareFun = word -> int -> int -> int;;
let singleLetterScore word pos acc = snd(List.item pos word) + acc;;
let doubleLetterScore word pos acc = snd(List.item pos word) * 2 + acc;;
let tripleLetterScore word pos acc = snd(List.item pos word) * 3 + acc;;

singleLetterScore hello 4 0;;
doubleLetterScore hello 4 0;;
tripleLetterScore hello 4 0;;
singleLetterScore hello 4 42;;
doubleLetterScore hello 4 42;;
tripleLetterScore hello 4 42;;


// Exercise 2.13
let doubleWordScore _ _ acc = acc * 2;;
let tripleWordScore _ _ acc = acc * 3;;

doubleWordScore hello 4 0;;
tripleWordScore hello 4 0;;
doubleWordScore hello 12345 42;;
tripleWordScore hello 12345 42;;


// Exercise 2.14
let consonants = ['B';'C';'D';'F';'G';'H';'J';'K';'L';'M';'N';'P';'Q';'R';'S';'T';'V';'W';'Y';'X';'Z'];;

let oddConsonants word _ acc = 
    if ((List.fold (fun acc elem -> 
        if (List.exists ((=) (fst(elem))) consonants)
        then (acc + 1) else acc) 0 word) % 2 = 0) then acc else -acc;;

let helo = ('H',4)::('E',1)::('L',1)::('O',1)::[];;
oddConsonants helo 0 10;;
oddConsonants hello 0 10;;


// Exercise 2.15
type square = (int * squareFun) list

let SLS : square = [(0, singleLetterScore)];;
let DLS : square = [(0, doubleLetterScore)];;
let TLS : square = [(0, tripleLetterScore)];;
let DWS : square = SLS @ [(1, doubleWordScore)];;
let TWS : square = SLS @ [(1, tripleWordScore)];;

let calculatePoints (squares : square list) w = 
    (List.mapi (fun i square -> 
        List.map (fun pair -> (fst(pair), (snd(pair) w i))) square) squares |>
    List.fold (fun acc elem -> elem @ acc) [] |>
    List.sortBy (fun pair -> fst(pair)) |>
    List.map (fun pair -> snd(pair)) |>
    List.fold (fun acc elem -> elem << acc) id) 0;;

calculatePoints [DLS; SLS; TLS; SLS; DWS] hello;;
calculatePoints [DLS; DWS; TLS; TWS; DWS] hello;;