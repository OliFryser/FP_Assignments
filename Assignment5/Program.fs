// For more information see https://aka.ms/fsharp-console-apps
let sum m n = 
    let rec sumA acc = 
        function
        | 0 -> acc + m
        | n' -> sumA (n' + m + acc) (n'-1)
    sumA 0 n

let length lst = 
    let rec lengthA acc =
        function
        | [] -> acc
        | x::xs -> lengthA (acc + 1) xs
    lengthA 0 lst

let foldBack f lst acc =
    let rec foldBacKC f lst acc c =
        match lst with
        | [] -> c acc
        | x::xs -> foldBackC f xs 