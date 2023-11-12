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
       (* This is kind of like nested for loops *)
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
    Out_channel.output_string Stdlib.stdout
      (Printf.sprintf "Part 1: %d\n" answer)

  (* Run part 2 with parsed inputs *)
  let part2 (Input i : t) =
    let answer =
      i
      |> List.map ~f:(List.fold ~init:0 ~f:( + ))
      |> List.sort ~compare:Int.compare
      |> List.rev
      |> fun x -> List.take x 3 |> List.fold ~init:0 ~f:( + ) |> fun x -> x
    in
    Out_channel.output_string Stdlib.stdout
      (Printf.sprintf "Part 2: %d\n" answer)
end

include M
include Day.Make (M)

(* Example input *)
let example = ""

(* Expect test for example input *)
let%expect_test _ = run example ; [%expect {|
  Part 1: 0
  Part 2: 0 |}]
