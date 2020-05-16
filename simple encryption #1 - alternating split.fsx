let rec encrypt (str:string) (n:int) = 
  match str, n with
  | "", _ -> str
  | null, _ -> str
  | _, n when n < 1 -> str
  | _, _ -> 
  Seq.fold (fun acc c -> match acc with
                         | (true , odds, evens) -> (false, odds @ [c], evens      )
                         | (false, odds, evens) -> (true , odds      , evens @ [c]))
           (false, [], [])
           str |> (function | (_, o, e) -> encrypt (System.String.Concat(o @ e)) (n-1))

let rec decrypt (str:string) (n:int) =
  match str, n with
  | "", _ -> str
  | null, _ -> str
  | _, n when n < 1 -> str
  | _, _ -> 
  Seq.zip (Seq.take (str.Length / 2) str) (Seq.skip (str.Length / 2) str) |>
  Seq.fold (fun a (e, o) -> a + string o + string e) "" |>
  (fun s -> match (String.length str) % 2 with | 0 -> s | 1 -> s + string (Seq.last str)) |>
  (fun s -> decrypt s (n-1))
  
decrypt "hsi  etTi sats!" 2;;
