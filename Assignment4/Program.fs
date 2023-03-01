// For more information see https://aka.ms/fsharp-console-apps
//printfn "%A" (Dictionary.lookup "HELLO" (Dictionary.empty () |> Dictionary.insert "HE" |> Dictionary.insert "HELLO"))
printfn "%A" (Dictionary.empty () |> Dictionary.insert "HELLO")

let d = Dictionary.empty () |> Dictionary.insert "HELLO"

let flatmap f = Option.map f >> Option.flatten

let testGaddag str gdag =
    let rec lookup =
        function
        | [] -> fun _ -> failwith "This can never happen"
        | [x] -> Dictionary.step x
        | x :: xs ->
        Dictionary.step x >>
        flatmap (snd >> lookup xs)
    let rec lookups acc back =
        function
        | [] -> fun _ -> Some false
        | [x] ->
            lookup (x :: back) >>
            flatmap (snd >> Dictionary.reverse) >>
            Option.map (fun (b, _) -> acc && b)
        | x :: xs ->
            lookup (x :: back) >>
            flatmap (snd >> Dictionary.reverse) >>
            flatmap (snd >> lookup xs) >>
            flatmap (fun (b, _) -> lookups (acc && b) (x :: back) xs gdag)

    gdag |>
    lookups true [] [for c in str -> c] |>
    (=) (Some true)

printfn "%A" (testGaddag "HELLO" d)