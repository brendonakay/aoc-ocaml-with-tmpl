open! Imports
open Base

module M = struct
  (* Type to parse the input into *)
  type t = Input of int list list (* Multi-dimensional list of ints *)

  (* function that extracts integer number from the string *)
  let extract_int = function "" -> None | t -> Some (Int.of_string t)

  (* Parse the input to type t, invoked for both parts *)
  let parse _inputs =
    _inputs |> String.split_lines
    (* Some lines are empty and we use it as a separator to create list of
       lists*)
    |> List.group ~break:(fun x _ -> String.(x = ""))
    |> List.map ~f:(fun x -> List.filter_map x ~f:extract_int)
    |> fun x -> Input x

  (* Run part 1 with parsed inputs *)
  let part1 (Input i : t) =
    let answer =
      i (* Sum elements of nested lists *)
      |> List.map ~f:(List.fold ~init:0 ~f:( + )) (*Iterate over the input*)
      |> List.max_elt ~compare:Int.compare (*Sort list by max*)
      |> fun x -> match x with None -> 0 | Some n -> n
    in
    Out_channel.output_string Stdlib.stdout (Printf.sprintf "%d" answer)

  (* Run part 2 with parsed inputs *)
  let part2 _ = ()
end

include M
include Day.Make (M)

(* Example input *)
let example = ""

(* Expect test for example input *)
let%expect_test _ = run example ; [%expect {| |}]

(* (* Solution for part 1 *) let solve_part1 (Input i : input) : answer = i
   (* Sum elements of nested lists *) |> List.map ~f:(List.fold ~init:0
   ~f:((+))) (* select biggest number *) |> List.max_elt
   ~compare:(Int.compare) (* biggest number is returned as optional type, use
   0 as default when None *) |> (fun x -> match x with | None -> Answer 0 |
   Some n -> Answer n)

   (* Solution for part 2 *) let solve_part2 (Input i : input) : answer = i
   (* Sum elements of nested lists *) |> List.map ~f:(List.fold ~init:0
   ~f:(+)) (* sort list descending *) |> List.sort ~compare:(Int.compare) |>
   List.rev (* take biggest 3 elements - they go first after sorting *) |>
   (fun x -> List.take x 3) (* sum elements *) |> List.fold ~init:0 ~f:(+) |>
   (fun x -> Answer x)

   let answer_to_text = function | Answer x -> Int.to_string x | Unknown ->
   "Solution not yet implemented"

   (* end-to-end functions *)

   let part1 (input_text: string) : (string) = input_text |> text_to_input |>
   solve_part1 |> answer_to_text

   let part2 (input_text: string) : (string) = input_text |> text_to_input |>
   solve_part2 |> answer_to_text *)
