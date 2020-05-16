type 'a IList = 'a System.Collections.Generic.IList

let findMissingWithoutSpecialCaseForFirst (items : int IList) =
    let folder acc r =
       match acc with
       | []        -> [(r, 0)]
       | [(l, 0)]  -> [(r, l - r)]
       | (l, d)::_ -> (r, l - r) :: acc
    Seq.fold folder [] items |>
    Seq.groupBy (fun (_, d) -> d) |>
    Seq.map (fun (_, s) -> s |> Array.ofSeq) |>
    Seq.map (fun a -> match a with
                      | [|x|] -> x |> fst
                      | _ -> a |> Array.head |> snd) |>
    Seq.sum

let findMissing (items : int IList) =
    Seq.fold (fun acc r -> match acc with
                           | []        -> [(r, 0)]
                           | [(l, 0)]  -> [(r, l - r)]
                           | (l, d)::_ -> (r, l - r) :: acc) [] items |>
    Seq.groupBy (fun (_, d) -> d) |>
    Seq.map (fun (_, s) -> s |> Array.ofSeq) |>
    Seq.sortBy (fun a -> a |> Array.length) |>
    Seq.fold (fun acc a -> match acc, a with
                           | 0, [|x|] -> x |> fst
                           | _, [|x|] -> (x |> snd) + acc
                           | _, array -> acc + (array |> Array.head |> snd)) 0

[<EntryPoint>]
let main _ =
    findMissing [|1; 3; 5; 9; 11|] |> printfn "7:%A"
    findMissing [|0; 5; 10; 20; 25|] |> printfn "15:%A"
    findMissing [|0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 11|] |> printfn "10:%A"
    findMissing [|1040; 1220; 1580|] |> printfn "1400:%A"
    findMissing [|32; 40; 44; 48|] |> printfn "36:%A"
    findMissing [|32; 64; 80; 96|] |> printfn "48:%A"
    0
