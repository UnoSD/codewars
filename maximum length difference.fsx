open System

let mxdiflg(a1: String[]) (a2: String[]): int Option =
  let maxMin a =
    a |>
    Array.map String.length |> (fun x -> (Array.max x, Array.min x))
  match a1, a2 with
  | [||], _ | _, [||] -> None
  | _ -> a1 |> maxMin |> (fun (mx, mi) -> a2 |> maxMin |> (fun (mx', mi') -> List.max [ mx - mi'; mx' - mi ])) |> Some
  
# TODO: attempt with applicatives

let (<*>) fs l = fs |> List.map (fun f -> l |> List.map f);;
[ Array.map String.length >> Array.max; Array.map String.length >> Array.min ] <*> [ [| "ab"; "a" |]; [| "aba"; "a" |] ];;
