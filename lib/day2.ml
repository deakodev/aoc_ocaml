open! Imports

module M = struct
  (* Type to parse the input into *)
  type t = int list list

  let int_list_of_string r = 
    String.split_on_char ' ' r 
    |> List.filter (fun c -> String.trim c <> "")
    |> List.map int_of_string

  (* Parse the input to type t, invoked for both parts *)
  let parse ( _inputs : string ) = 
    String.split_on_char '\n' _inputs
    |> List.map (int_list_of_string)

  let condition1 (record : int list) damp =
    let aux op = List.fold_left_map (fun acc b -> if acc <> [] 
      then let a = List.hd (List.rev acc) in acc, (op a b) else b :: acc, true) []
  in
    let increasing = 
      snd (aux ( < ) record) |> List.filter (fun x -> x) |> List.length |> ( <= ) ((List.length record) - damp)
  in
    let decreasing = 
      snd (aux ( > ) record) |> List.filter (fun x -> x) |> List.length |> ( <= ) ((List.length record) - damp)
  in
    increasing || decreasing

  let condition2 (record : int list) damp = 
    let rec aux acc = function
      | a :: (b :: _ as l) -> 
        let diff = abs (a - b)
      in
        if diff >= 1 && diff <= 3 then aux (true :: acc) l else aux (false :: acc) l
      | _ :: [] -> acc
      | [] -> acc
    in
  aux [] record |> List.filter (fun x -> x) |> List.length |> ( <= ) ((List.length record) - damp)

  let check cond1 cond2 (records : int list list) damp = 
    List.fold_left (fun acc r -> if cond1 r damp && cond2 r damp then acc + 1 else acc) 0 records

  (* Run part 1 with parsed inputs *)
  let part1 (records : int list list) = 
    print_endline_int (check condition1 condition2 records 0)

  (* Run part 2 with parsed inputs *)
  let part2 (records : int list list) =
    print_endline_int (check condition1 condition2 records 1) 



end

(* 
1 ) The levels are either all increasing or all decreasing.
2 ) Any two adjacent levels differ by at least one and at most three.
*)

include M
include Day.Make (M)

(* Example input *)
let example = "
7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9
"

(* Expect test for example input *)
let%expect_test _ = run example ; [%expect {|
  2
  4
|}]
