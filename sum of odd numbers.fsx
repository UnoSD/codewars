// With seq
let rowSumOddNumbers n = 
    let rec sumToOne n =
      match n with
      | 0 | 1 -> n
      | _ -> n + (sumToOne (n - 1))
    seq { for x in 1 .. System.Int32.MaxValue do if (x % 2) <> 0 then yield x } |>
    Seq.skip (sumToOne (n - 1)) |>
    Seq.take n |>
    Seq.sum

// Pure math
let rowSumOddNumbers n = n * n * n
    
[1..10] |> List.map rowSumOddNumbers |> printfn "%A"
