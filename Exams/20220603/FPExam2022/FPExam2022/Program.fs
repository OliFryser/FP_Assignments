open System
open Exam2022

let testQ1 () =
    (* Testsfor Q1.1 *)
    printfn "Testing Question 1"
    (*
    printfn "%A" (countWhite (Square 123uy))
    printfn "%A" (countWhite img)
    *)

    (* Tests for Q1.3*)
    printfn "%A" (map (fun x -> Quad (Square (x + 10uy), Square (x + 20uy), Square (x + 30uy), Square (x + 40uy))) (Square (123uy)))
    ()

let testQ2 () =
    printfn "Testing Question 2"
    // place debug prints for Q2 here
    ()

let testQ3 () =
    printfn "Testing Question 3"
    // place debug prints for Q3 here
    ()

let testQ4 () =
    printfn "Testing Question 4"
    printfn "%A" (
        "PUSH 5\nPUSH 4 \nADD \n PUSH 8\nMULT \n" |> 
        JParsec.TextParser.run parseStackProg |> 
        JParsec.TextParser.getSuccess |>
        runStackProg2 |>
        evalSM |>
        Option.map fst)
    ()

[<EntryPoint>]
let main argv =
    //testQ1 ()
    testQ4 ()
    0 // return an integer exit code
