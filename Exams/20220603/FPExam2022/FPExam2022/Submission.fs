module Exam2022
(* If you are importing this into F# interactive then comment out
   the line above and remove the comment for the line bellow.

   Do note that the project will not compile if you do this, but 
   it does allow you to work in interactive mode and you can just remove the '=' 
   to make the project compile again.

   You will also need to load JParsec.fs. Do this by typing
   #load "JParsec.fs" 
   in the interactive environment. You may need the entire path.

   Do not remove the module declaration (even though that does work) because you may inadvertently
   introduce indentation errors in your code that may be hard to find if you want
   to switch back to project mode. 

   Alternative, keep the module declaration as is, but load ExamInteractive.fsx into the interactive environment
   *)
(*
 module Exam2022 = 
 *)

(* 1: Grayscale images *)

    type grayscale =
    | Square of uint8
    | Quad of grayscale * grayscale * grayscale * grayscale
    
    let img = 
      Quad (Square 255uy, 
            Square 128uy, 
            Quad(Square 255uy, 
                 Square 128uy, 
                 Square 192uy,
                 Square 64uy),
            Square 0uy)
    
(* Question 1.1 *)
    let countWhite img =
        let rec aux img' acc = 
            match img' with
            | Square value -> 
                match value with
                | 255uy -> acc + 1
                | _ -> acc
            | Quad (g1,g2,g3,g4) ->  
                acc + aux g1 0 + aux g2 0 + aux g3 0 + aux g4 0
        aux img 0
    
(* Question 1.2 *)
    let rec rotateRight = 
        function
        | Square value -> Square value
        | Quad (g1,g2,g3,g4) -> Quad (rotateRight g4, rotateRight g1, rotateRight g2, rotateRight g3)

(* Question 1.3 *)
    let rec map (mapper : uint8 -> grayscale) =
        function
        | Square value -> mapper value
        | Quad (g1,g2,g3,g4) -> Quad (map mapper g1, map mapper g2, map mapper g3, map mapper g4)
    
    let bitmap img = 
        map (fun x ->
            match x with
            | _ when x < 128uy -> Square 0uy
            | _ when x >= 128uy -> Square 255uy
            | _ -> failwith "value out of bounds"
            ) img
            

(* Question 1.4 *)

    let rec fold folder acc img =
        match img with
        | Square x -> folder acc x
        | Quad (g1,g2,g3,g4) -> fold folder (fold folder (fold folder (fold folder acc g1) g2) g3) g4
    
    let countWhite2 img = fold (fun acc x ->
        if x = 255uy then acc + 1 else acc) 0 img

(* 2: Code Comprehension *)
    let rec foo =
        function
        | 0 -> ""
        | x when x % 2 = 0 -> foo (x / 2) + "0"
        | x when x % 2 = 1 -> foo (x / 2) + "1"

    let rec bar =
        function
        | []      -> []
        | x :: xs -> (foo x) :: (bar xs)

(* Question 2.1 *)

    (* 
    
    Q: What are the types of functions foo and bar?

    A: 
        foo : int -> string
        bar : int list -> string list


    Q: What does the function bar do.
       Focus on what it does rather than how it does it.

    A: foo takes an integer and returns a string of the integer in binary.
       bar takes a list of integers and applies foo on them (similar to List.map)
    
    Q: What would be appropriate names for functions 
       foo and bar?

    A: foo could be decimalToBinary and bar could be manyDecimalToBinary
        
    Q: The function foo does not return reasonable results for all possible inputs.
       What requirements must we have on the input to foo in order to get reasonable results?
    
    A: Input cannot be less than 1. If it is zero, 
       it will produce the empty string, and if it is negative, 
       it will produce the same result as the positive number.
    *)
        

(* Question 2.2 *)

 
    (* 
    The function foo compiles with a warning. 

    
    Q: What warning and why?

    A: The compiler is not sure we have covered all cases in pattern matching.
       This is true, since we do not cover the case for negative odd numbers,
       since [any odd number] % 2 = -1

    *)

    let foo2 x = 
        match x with
        | 0 -> ""
        | x when x % 2 = 0 -> foo (x / 2) + "0"
        | x when x % 2 = 1 -> foo (x / 2) + "1"
        | _ -> failwith "Function failed as it should"

(* Question 2.3 *) 

    let bar2 xs = List.map (fun x -> foo x) xs

(* Question 2.4 *)

    (*

    Q: Neither foo nor bar is tail recursive. Pick one (not both) of them and explain why.
       To make a compelling argument you should evaluate a function call of the function,
       similarly to what is done in Chapter 1.4 of HR, and reason about that evaluation.
       You need to make clear what aspects of the evaluation tell you that the function is not tail recursive.
       Keep in mind that all steps in an evaluation chain must evaluate to the same value
       ((5 + 4) * 3 --> 9 * 3 --> 27, for instance).

    A: bar is not tail recursive, because the resolved value of (foo x) will be kept on the stack until we reach that evaluation again.
       This is because we cannot concatenate that value onto the (bar xs) element, since that value is yet to be computed.
    
    Q: Even though neither `foo` nor `bar` is tail recursive only one of them runs the risk of overflowing the stack.
       Which one and why does  the other one not risk overflowing the stack?

    A: bar runs the risk because all the computed values with foo are waiting to be concetenated to the list. //TODO: Why does foo not overflow the stack

    *)
(* Question 2.5 *)

    let fooTail x = 
        let rec aux acc =
            function
            | 0 -> acc
            | x when x % 2 = 0 -> aux ("0"+acc) (x / 2)
            | x when x % 2 = 1 -> aux ("1"+acc) (x / 2)
            | _ -> failwith "unexpected value"
        aux "" x

(* Question 2.6 *)
    let barTail lst =
        let rec aux lst' c =
            match lst' with
            | []      -> c []
            | x :: xs -> aux xs (fun n -> c ((foo x)::n))

        aux lst id

(* 3: Matrix operations *)

    type matrix = int[,]

    let init f rows cols = Array2D.init rows cols f

    let numRows (m : matrix) = Array2D.length1 m
    let numCols (m : matrix) = Array2D.length2 m

    let get (m : matrix) row col = m.[row, col]
    let set (m : matrix) row col v = m.[row, col] <- v

    let print (m : matrix) =
        for row in 0..numRows m - 1 do
            for col in 0..numCols m - 1 do
                printf "%d\t" (get m row col)
            printfn ""

(* Question 3.1 *)

    let failDimensions a b = 
        failwith (
            sprintf "Invalid matrix dimensions: m1 rows = %d, m1 columns = %d, m2 rows = %d, m2 columns = %d"
                (numRows a) (numCols a) (numRows b) (numCols b))

(* Question 3.2 *)

    let add a b = 
        if (numRows a <> numRows b || numCols a <> numCols b |> not)
        then failDimensions a b
        else 
             init (fun i j -> (get a i j) + (get b i j)) 
                (numRows a) (numCols a)

(* Question 3.3 *)
    
    let m1 = (init (fun i j -> i * 3 + j + 1) 2 3) 
    let m2 = (init (fun j k -> j * 2 + k + 1) 3 2)

    let dotProduct a b row col = 
        let rec aux acc =
            function
            | 0 -> acc
            | n -> 
                aux ((get a row (n-1)) * (get b (n-1) col) + acc) (n-1)

        aux 0 (numCols a)

    let mult a b = 
        if (numRows a <> numCols b) then failDimensions a b
        else
            init (fun i j -> dotProduct a b i j) (numRows a) (numCols b)

(* Question 3.4 *)

    let parInit f row col = 
        let startM = init (fun _ _ -> 0) row col
        
        [for i in [0..row-1] do
            for j in [0..col-1] do yield (i, j)] |>

        List.map (fun (i,j) -> async { do set startM i j (f i j) }) |>
        Async.Parallel |> 
        Async.RunSynchronously |> ignore

        
        startM


(* 4: Stack machines *)

    type cmd = Push of int | Add | Mult
    type stackProgram = cmd list

(* Question 4.1 *)

    type stack = int list

    let emptyStack () : stack = List<int>.Empty

(* Question 4.2 *)
    
    let runCmd cmd stack = 
        match cmd with
        | Push x -> x::stack
        | Add -> 
            if List.length stack < 2 then failwith "empty stack" else
            let a = List.head stack
            let tempRest = List.tail stack
            let b = List.head tempRest
            let rest = List.tail tempRest
            (a+b)::rest
        | Mult ->
            if List.length stack < 2 then failwith "empty stack" else
            let a = List.head stack
            let tempRest = List.tail stack
            let b = List.head tempRest
            let rest = List.tail tempRest
            (a*b)::rest

    let runStackProg prog = 
        let stack = emptyStack ()
        let rec aux prog stack =
            match prog with
            | [] -> if List.isEmpty stack then failwith "empty stack" else List.head stack
            | c::cs -> aux cs (runCmd c stack)                    

        aux prog stack

(* Question 4.3 *)
    
    type StateMonad<'a> = SM of (stack -> ('a * stack) option)

    let ret x = SM (fun s -> Some (x, s))
    let fail  = SM (fun _ -> None)
    let bind f (SM a) : StateMonad<'b> = 
        SM (fun s -> 
            match a s with 
            | Some (x, s') -> 
                let (SM g) = f x             
                g s'
            | None -> None)
        
    let (>>=) x f = bind f x
    let (>>>=) x y = x >>= (fun _ -> y)

    let evalSM (SM f) = f (emptyStack ())

    let push (x : int) = 
        SM (fun s -> Some ((), x::s))

    let pop = 
        SM (fun s -> 
            if List.isEmpty s then None else 
                Some ((List.head s), List.tail s))

(* Question 4.4 *)

    type StateBuilder() =

        member this.Bind(f, x)    = bind x f
        member this.Return(x)     = ret x
        member this.ReturnFrom(x) = x
        member this.Combine(a, b) = a >>= (fun _ -> b)

    let state = new StateBuilder()

    let runCmd2 cmd = state {
        match cmd with
            | Push x -> 
                do! push x
            | Add ->
                let! a = pop
                let! b = pop
                do! (push (a+b))
            | Mult ->
                let! a = pop
                let! b = pop
                do! (push (a*b))
        }

    let rec runStackProg2 prog = state {
        match prog with
        | [] -> return! pop
        | c::cs -> 
            do! runCmd2 c
            return! (runStackProg2 cs)
        }

(* Question 4.5 *)
    
    open JParsec.TextParser

    let whitespaceChar = satisfy (fun c -> System.Char.IsWhiteSpace c) <?> "whitespace"

    let spaces         = many whitespaceChar <?> "spaces"
    let spaces1        = many1 whitespaceChar <?> "space1"
    
    let pPush = spaces >>. pstring "PUSH" .>> spaces1
    let pAdd = spaces >>. pstring "ADD" .>> spaces
    let pMult = spaces >>. pstring "MULT" .>> spaces

    let PushParse = pPush >>. pint32 |>> Push <?> "Push"
    let AddParse = pAdd |>> (fun _ -> Add) <?> "Add"
    let MultParse = pMult |>> (fun _ -> Mult) <?> "Mult"
    let StackParse = choice [PushParse; AddParse; MultParse]

    let parseStackProg : Parser<stackProgram> = 
        many (StackParse)