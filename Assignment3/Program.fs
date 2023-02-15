//Setup
type aExp =
| N of int // Integer value
| V of string // Variable
| WL // Length of the word
| PV of aExp // Point value of character at specific word index
| Add of aExp * aExp // Addition
| Sub of aExp * aExp // Subtraction
| Mul of aExp * aExp // Multiplication

type cExp =
| C of char (* Character value *)
| ToUpper of cExp (* Converts lower case to upper case character,
non-letters are unchanged *)
| ToLower of cExp (* Converts upper case to lower case character,
non-letters are unchanged *)
| CV of aExp (* Character lookup at word index *)

let (.+.) a b = Add (a, b)
let (.-.) a b = Sub (a, b)
let (.*.) a b = Mul (a, b)

type bExp =
| TT (* true *)
| FF (* false *)
| AEq of aExp * aExp (* numeric equality *)
| ALt of aExp * aExp (* numeric less than *)
| Not of bExp (* boolean not *)
| Conj of bExp * bExp (* boolean conjunction *)
| IsDigit of cExp (* check for digit *)
| IsLetter of cExp (* check for letter *)
| IsVowel of cExp (* check for vowel *)

let (~~) b = Not b
let (.&&.) b1 b2 = Conj (b1, b2)
let (.||.) b1 b2 = ~~(~~b1 .&&. ~~b2) (* boolean disjunction *)

let (.=.) a b = AEq (a, b)
let (.<.) a b = ALt (a, b)
let (.<>.) a b = ~~(a .=. b) (* numeric inequality *)
let (.<=.) a b = a .<. b .||. ~~(a .<>. b) (* numeric less than or equal to *)
let (.>=.) a b = ~~(a .<. b) (* numeric greater than or equal to *)
let (.>.) a b = ~~(a .=. b) .&&. (a .>=. b) (* numeric greater than *)

type stmnt =
| Skip (* does nothing *)
| Ass of string * aExp (* variable assignment *)
| Seq of stmnt * stmnt (* sequential composition *)
| ITE of bExp * stmnt * stmnt (* if-then-else statement *)
| While of bExp * stmnt (* while statement *)

//Exercise 3.1
let rec arithEvalSimple = function
    | N x -> x
    | Add(aExp1,aExp2) -> (arithEvalSimple aExp1) + (arithEvalSimple aExp2)
    | Sub(aExp1,aExp2) -> (arithEvalSimple aExp1) - (arithEvalSimple aExp2)
    | Mul(aExp1,aExp2) -> (arithEvalSimple aExp1) * (arithEvalSimple aExp2)


//Exercise 3.2
let rec arithEvalState a s = 
    match a with
    | V v -> 
        match (Map.tryFind v s) with
        | None -> 0
        | x -> Option.get(x)
    | N x -> x
    | Add(aExp1,aExp2) -> (arithEvalState aExp1 s) + (arithEvalState aExp2 s)
    | Sub(aExp1,aExp2) -> (arithEvalState aExp1 s) - (arithEvalState aExp2 s)
    | Mul(aExp1,aExp2) -> (arithEvalState aExp1 s) * (arithEvalState aExp2 s)


//Exercise 3.3
let arithSingleLetterScore = PV (V "_pos_") .+. (V "_acc_")
let arithDoubleLetterScore = ((N 2) .*. PV (V "_pos_")) .+. (V "_acc_")
let arithTripleLetterScore = ((N 3) .*. PV (V "_pos_")) .+. (V "_acc_")

let arithDoubleWordScore = N 2 .*. V "_acc_"
let arithTripleWordScore = N 3 .*. V "_acc_"

type word = (char * int) list;;
let hello = ('H',4)::('E',1)::('L',1)::('L',1)::('O',1)::[];;

let rec arithEval a w s = 
    match a with
    | V v -> Map.tryFind v s |> Option.defaultValue 0
    | N x -> x
    | WL -> List.length w
    | PV x -> 
        w |> List.item (arithEval x w s) |> snd
    | Add(aExp1,aExp2) -> (arithEval aExp1 w s) + (arithEval aExp2 w s)
    | Sub(aExp1,aExp2) -> (arithEval aExp1 w s) - (arithEval aExp2 w s)
    | Mul(aExp1,aExp2) -> (arithEval aExp1 w s) * (arithEval aExp2 w s)


//Exercise 3.4
let rec charEval c w s = 
    match c with
    | C c -> c
    | ToUpper c -> System.Char.ToUpper (charEval c w s)
    | ToLower c -> System.Char.ToLower (charEval c w s)
    | CV a -> w |> List.item (arithEval a w s) |> fst


//Exercise 3.5
let isVowel c =
    let upper = System.Char.ToUpper c
    match upper with
    | 'A' | 'E' | 'I' | 'O' | 'U' -> true
    | _ -> false

let rec boolEval b w s = 
    match b with
    | TT -> true
    | FF -> false

    | AEq (aExp1, aExp2) -> (arithEval aExp1 w s) = (arithEval aExp2 w s)
    | ALt (aExp1, aExp2) -> (arithEval aExp1 w s) < (arithEval aExp2 w s)

    | Not x -> not (boolEval x w s)
    | Conj (x1, x2) -> (boolEval x1 w s) && (boolEval x2 w s)

    | IsDigit c -> System.Char.IsDigit (charEval c w s)
    | IsLetter c -> System.Char.IsLetter (charEval c w s)
    | IsVowel c -> isVowel (charEval c w s)


//Exercise 3.6
let isConsonant c = ~~(IsVowel c)


//Exercise 3.7
let rec evalStmnt stm w s =
    match stm with
    | Skip -> s
    | Ass (x, a) -> Map.add x (arithEval a w s) s
    | Seq (stm1, stm2) -> evalStmnt stm1 w s |> evalStmnt stm2 w
    | ITE (guard, stm1, stm2) -> 
        if (boolEval guard w s) 
        then (evalStmnt stm1 w s) else (evalStmnt stm2 w s)
    | While (guard, stm) ->
        if boolEval guard w s
        then (evalStmnt stm w s) |> evalStmnt (While (guard, stm)) w
        else s

//Exercise 3.8
type squareFun = word -> int -> int -> int

let stmntToSquareFun stm w pos acc =
    let s = Map [("_pos_", pos); ("_acc_", acc)] 
    Map.find "_result_" (evalStmnt stm w s)

let singleLetterScore = stmntToSquareFun (Ass ("_result_", arithSingleLetterScore))
let doubleLetterScore = stmntToSquareFun (Ass ("_result_", arithDoubleLetterScore))
let tripleLetterScore = stmntToSquareFun (Ass ("_result_", arithTripleLetterScore))
let doubleWordScore = stmntToSquareFun (Ass ("_result_", arithDoubleWordScore))
let tripleWordScore = stmntToSquareFun (Ass ("_result_", arithTripleWordScore))

let containsNumbers =
    stmntToSquareFun
        (Seq (Ass ("_result_", V "_acc_"),
            While (V "i" .<. WL,
                ITE (IsDigit (CV (V "i")),
                    Seq (
                        Ass ("_result_", V "_result_" .*. N -1),
                        Ass ("i", WL)),
                    Ass ("i", V "i" .+. N 1)))))


//Exercise 3.9
let oddConsonants = 
    (Seq (Ass ("_result_", V "_acc_"),
        While (V "i" .<. WL, 
            Seq(
                ITE (isConsonant (CV (V "i")),
                        Ass ("_result_", V "_result_" .*. N -1),
                        Skip),
                Ass ("i", V "i" .+. N 1)))))


//Exercise 3.10
type square = (int * squareFun) list
type square2 = (int * stmnt) list

let SLS = [(0, Ass ("_result_", arithSingleLetterScore))]
let DLS = [(0, Ass ("_result_", arithDoubleLetterScore))]
let TLS = [(0, Ass ("_result_", arithTripleLetterScore))]
let DWS = [(1, Ass ("_result_", arithDoubleWordScore))] @ SLS
let TWS = [(1, Ass ("_result_", arithTripleWordScore))] @ SLS

let calculatePoints (squares : square list) w = 
    (List.mapi (fun i -> 
        List.map (fun (p, f) -> p, (f w i))) squares |>
    List.fold (fun acc elem -> elem @ acc) [] |>
    List.sortBy (fun (p, _) -> p) |>
    List.map (fun (_, f) -> f) |>
    List.fold (fun acc elem -> acc >> elem) id) 0

let calculatePoints2 (squares : square2 list) w = 
    List.map (List.map (fun (p, s) -> (p, stmntToSquareFun s))) squares
    |> calculatePoints <| w

