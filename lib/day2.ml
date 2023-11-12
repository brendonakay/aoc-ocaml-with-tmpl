open! Imports
open Base

module M = struct
  type moves = Rock [@value 1] | Paper | Scissors [@@deriving enum]

  type outcome = Win | Draw | Loss

  type strategy1 = {opp: moves; player: moves}

  type strategy2 = {opp: moves; res: outcome}

  (*TODO rename these to make more sense*)
  let parse_part1 (str : string) : strategy1 =
    let o, p = (String.get str 0, String.get str 2) in
    let parse c =
      match c with
      | 'A' | 'X' -> Rock
      | 'B' | 'Y' -> Paper
      | 'C' | 'Z' -> Scissors
      | _ -> Rock
    in
    {opp= parse o; player= parse p}

  (*TODO rename these to make more sense*)
  let parse_part2 (str : string) : strategy2 =
    let o, p = (String.get str 0, String.get str 2) in
    let parse_o c =
      match c with 'A' -> Rock | 'B' -> Paper | 'C' -> Scissors | _ -> Rock
    in
    let parse_r c =
      match c with 'X' -> Loss | 'Y' -> Draw | 'Z' -> Win | _ -> Loss
    in
    {opp= parse_o o; res= parse_r p}

  let outcome_score (x : outcome) : int =
    match x with Loss -> 0 | Draw -> 3 | Win -> 6

  (* TODO is there a better way to determine winners and losers with a
     different data type?*)
  let round_outcome (opp : moves) (you : moves) : outcome =
    match (opp, you) with
    | Rock, Paper | Paper, Scissors | Scissors, Rock -> Win
    | Rock, Rock | Paper, Paper | Scissors, Scissors -> Draw
    | _ -> Loss

  let round_score (opp : moves) (you : moves) : int =
    (round_outcome opp you |> outcome_score) + moves_to_enum you

  (* Type to parse the input into *)
  type t = Input of (strategy1 list * strategy2 list)

  (* Parse the input to type t, invoked for both parts *)
  let parse _inputs =
    _inputs |> String.split_lines
    |> List.filter ~f:(String.( <> ) "")
    |> List.map ~f:(fun x -> (parse_part1 x, parse_part2 x))
    |> List.unzip
    |> fun x -> Input x

  (* Run part 1 with parsed inputs *)
  let part1 (Input (i, _) : t) =
    let answer =
      i
      |> List.map ~f:(fun ({opp; player} : strategy1) ->
             round_score opp player )
      |> List.fold ~init:0 ~f:( + )
      |> fun x -> x
    in
    Out_channel.output_string Stdlib.stdout
      (Printf.sprintf "Part 1: %d\n" answer)

  (* Run part 2 with parsed inputs *)
  let part2 _ = ()
end

include M
include Day.Make (M)

(* Example input *)
let example = "A Y\nB X\nC Z"

(* Expect test for example input *)
let%expect_test _ = run example ; [%expect {| Part 1: 15 |}]

(* let solve_part1 (Input (i, _)) : answer = i |> List.map ~f:(fun ({opp;
   you}:strategy1) -> round_score opp you) |> List.fold ~init:0 ~f:(+) |>
   (fun x -> Answer x)

   let solve_part2 (Input (_, i)) : answer = i |> List.map ~f:(fun ({opp;
   res}:strategy2) -> round_score opp (your_shape opp res)) |> List.fold
   ~init:0 ~f:(+) |> (fun x -> Answer x)

   let answer_to_text = function | Answer x -> Int.to_string x | Unknown ->
   "Solution not yet implemented"

   (* end-to-end functions *)

   let part1 (input_text: string) : (string) = input_text |> text_to_input |>
   solve_part1 |> answer_to_text

   let part2 (input_text: string) : (string) = input_text |> text_to_input |>
   solve_part2 |> answer_to_text *)
