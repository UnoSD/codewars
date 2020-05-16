// With lists
let josephusSurvivor n k =
    let rec without ix lst =
      match ((ix-1) % (List.length lst)), lst with
      | _, [x] -> x
      | i, lst -> lst |> List.splitAt i |> (fun (l, r) -> (r |> List.skip 1) @ l) |> without ix
    [1..n] |> without k;;

// Pure math        
let rec josephusSurvivor n k = match n with | 1 -> 1 | _ -> ((josephusSurvivor (n - 1) k) + k-1) % n + 1

josephusSurvivor 7 3;; //4
josephusSurvivor 11 19;; //10
josephusSurvivor 40 3;; //28
josephusSurvivor 14 2;; //13
josephusSurvivor 100 1;; //100
josephusSurvivor 1 300;; //1
josephusSurvivor 2 300;; //1
josephusSurvivor 5 300;; //1
josephusSurvivor 7 300;; //7
josephusSurvivor 300 300;; //265
