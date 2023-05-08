// For more information see https://aka.ms/fsharp-console-apps
//Exerise 5.1
let sum m n = 
    let rec sumA acc = 
        function
        | 0 -> acc + m
        | n' -> sumA (n' + m + acc) (n'-1)
    sumA 0 n

//Exercise 5.2
let length lst = 
    let rec lengthA acc =
        function
        | [] -> acc
        | x::xs -> lengthA (acc + 1) xs
    lengthA 0 lst

//Exercise 5.3
let foldBack f lst acc =
    let rec foldBackC lst c =
        match lst with
        | [] -> c acc
        | x::xs -> foldBackC xs (fun n -> c ((f x) n))
    foldBackC lst id

printfn "%A" (foldBack (-) [1..10] 0)

// Exercise 5.4
let factA x =
    let rec aux acc =
        function
        | 0 -> acc
        | x -> aux (x * acc) (x - 1)
    aux 1 x

let factC x =
    let rec aux c =
        function
        | 0 -> c 1
        | x -> aux (fun n -> c (x*n)) (x-1)
    aux id x

// Using the number 100000000 to compare the two, I get the running times:
// factA = Real: 00:00:00.121, CPU: 00:00:00.062, GC gen0: 0, gen1: 0, gen2: 0
// factC = Real: 00:00:10.269, CPU: 00:00:04.671, GC gen0: 389, gen1: 388, gen2: 6
// factA uses constant space, as can be seen in the garbage collection
// factC uses linear space, but we do not get stack overflow, since the memory is in the heap, rather than in the stack


// Exercise 5.5
let fibW x =
    let mutable res1 = 0
    let mutable res2 = 1
    let mutable i = 1
    while (i <= x) do
        let temp = res1
        res1 <- res2
        res2 <- temp + res2
        i <- i + 1
    res1

let fibA x =
    let rec aux acc1 acc2 = 
        function
        | 0 -> 0
        | 1 -> acc1
        | x -> aux (acc2) (acc1 + acc2) (x-1)
    aux 1 1 x

let fibC x =
    let rec aux c = 
        function
        | 0 -> c 0
        | 1 -> c 1
        | x -> aux (fun n -> aux (fun n' -> c (n+n')) (x-1)) (x-2)
    aux id x

printfn "%A" (fibW 10)
printfn "%A" (fibA 10)
printfn "%A" (fibC 10)

// The running times for the three functions are as follows using the number 10000000 (eight zeroes):
// fibW = Real: 00:00:00.078, CPU: 00:00:00.062, GC gen0: 0, gen1: 0, gen2: 0
// fibA = Real: 00:00:00.082, CPU: 00:00:00.062, GC gen0: 0, gen1: 0, gen2: 0
// fibC = Real: 00:00:10.341, CPU: 00:00:04.281, GC gen0: 389, gen1: 388, gen2: 6

// Using the number 1000000000 (nine zeroes), we get:
// fibW = Real: 00:00:00.905, CPU: 00:00:00.484, GC gen0: 0, gen1: 0, gen2: 0
// fibA = Real: 00:00:00.871, CPU: 00:00:00.406, GC gen0: 0, gen1: 0, gen2: 0
// fibC = did not finish

// It can be seen that the imperative and iterative function using accumulators are pretty much identical in terms of running times.
// I expected to see that the imperative function would be slower, since it would have to evaluate the condition every iteration.
// This is however not the case in my tests.
// fibC is still slower, due to the garbage collection.


// Exercise 5.6

// While the big recursive function is tail-recursive, the continuation function within it is not. The c is called on res.
// Then 1 is "cons" onto that list, but we do not yet know the value of res. Therefore this call is stored on the stack, not the heap,
// meaning that it is not tail-recursive, and therefore not imperative.


// Exercise 5.7
type word = (char * int) list

type aExp =
    | N of int (* Integer literal *)
    | V of string (* Variable reference *)
    | WL (* Word length *)
    | PV of aExp (* Point value lookup at word index *)
    | Add of aExp * aExp (* Addition *)
    | Sub of aExp * aExp (* Subtraction *)
    | Mul of aExp * aExp (* Multiplication *)
    | CharToInt of cExp (* NEW: Cast to integer *)
and cExp =
    | C of char (* Character literal *)
    | CV of aExp (* Character lookup at word index *)
    | ToUpper of cExp (* Convert character to upper case *)
    | ToLower of cExp (* Convert character to lower case *)
    | IntToChar of aExp (* NEW: Cast to character *)


let rec arithEvalSimple a w s : int = 
    match a with
    | V v -> Map.tryFind v s |> Option.defaultValue 0
    | N x -> x
    | WL -> List.length w
    | PV x -> 
        w |> List.item (arithEvalSimple x w s) |> snd
    | Add(aExp1,aExp2) -> (arithEvalSimple aExp1 w s) + (arithEvalSimple aExp2 w s)
    | Sub(aExp1,aExp2) -> (arithEvalSimple aExp1 w s) - (arithEvalSimple aExp2 w s)
    | Mul(aExp1,aExp2) -> (arithEvalSimple aExp1 w s) * (arithEvalSimple aExp2 w s)
    | CharToInt c -> int (charEvalSimple c w s)
and charEvalSimple c w s = 
    match c with
    | C c -> c
    | ToUpper c -> System.Char.ToUpper (charEvalSimple c w s)
    | ToLower c -> System.Char.ToLower (charEvalSimple c w s)
    | CV a -> w |> List.item (arithEvalSimple a w s) |> fst
    | IntToChar a -> char (arithEvalSimple a w s)


// Exercise 5.8
let rec arithEvalTail (a : aExp) (w : word) (s : Map<string,int>) (con : int -> 'a) : 'a = 
    match a with
    | V v -> con (Map.tryFind v s |> Option.defaultValue 0)
    | N x -> con x
    | WL -> con (List.length w)
    | PV x -> 
        arithEvalTail x w s (fun y -> w |> List.item y |> snd |> con)
    | Add(aExp1,aExp2) -> 
        arithEvalTail aExp1 w s (fun n -> arithEvalTail aExp2 w s (fun n' -> con (n + n')))
    | Sub(aExp1,aExp2) -> 
        arithEvalTail aExp1 w s (fun n -> arithEvalTail aExp2 w s (fun n' -> con (n - n')))
    | Mul(aExp1,aExp2) -> 
        arithEvalTail aExp1 w s (fun n -> arithEvalTail aExp2 w s (fun n' -> con (n * n')))
    | CharToInt c -> 
        charEvalTail c w s (fun char -> con (int char))

and charEvalTail (c : cExp) (w : word) (s : Map<string,int>) con = 
    match c with
    | C c -> con c
    | ToUpper c -> charEvalTail c w s (fun char -> con (System.Char.ToUpper char))
    | ToLower c -> charEvalTail c w s (fun char -> con (System.Char.ToLower char))
    | CV a -> arithEvalTail a w s (fun y -> w |> List.item y |> fst |> con)
    | IntToChar a -> arithEvalTail a w s (fun n -> con (char n))


let arithEval a w s = arithEvalTail a w s id
let charEval c w s = charEvalTail c w s id

let s = Map.empty<string, int> |> Map.add "var" 2
printf "%A" (arithEval (V "var") [] s)
arithEval (PV (N 1)) (['a', 2;'b',2]) (Map.empty<string,int>)