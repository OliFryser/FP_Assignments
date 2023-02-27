// For more information see https://aka.ms/fsharp-console-apps
printfn "%A" (Dictionary.lookup "HE" (Dictionary.empty () |> Dictionary.insert "HE" |> Dictionary.insert "HELLO"))
