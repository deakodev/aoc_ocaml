open! Imports

module M = struct
  (* Type to parse the input into *)
  type t = int list * int list (* sorted *)

  (* Parse the input to type t, invoked for both parts *)
  let parse input = 
    let lines = String.split_on_char '\n' input in
    let aux (left, right) line = match Str.split (Str.regexp " +") line with
    | [a; b] -> int_of_string a :: left, int_of_string b :: right
    | _ -> left, right
  in 
    let left, right = List.fold_left aux ([], []) lines 
  in
    List.sort compare left, List.sort compare right


  (* Run part 1 with parsed inputs *)
  let part1 (left, right) = List.map2 (fun x y -> abs (x - y)) left right 
  |> List.fold_left ( + ) 0 
  |> print_endline_int


  (* Run part 2 with parsed inputs *)
  let part2 (left, right) =
    let rec count x = function
    | [] -> 0
    | h :: t -> (if h = x then 1 else 0) + count x t
  in List.map (fun x -> (count x right) * x ) left
  |> List.fold_left ( + ) 0 
  |> print_endline_int
end

include M
include Day.Make (M)

(* Example input *)
let example = "
  3 4
  4 3
  2 5
  1 3
  3 9
  3 3
"

(* Expect test for example input *)
let%expect_test _ = run example ; [%expect {|
  11
  31
|}]
