
(* This file was auto-generated based on "sexpParserMessages". *)

(* Please note that the function [message] can raise [Not_found]. *)

let message =
  fun s ->
    match s with
    | 51 ->
        "<+ should be followed by a number>\n"
    | 3 ->
        "<- should be followed by a number>\n"
    | 0 ->
        "<Unexpected )>\n"
    | 17 ->
        "<Unexpected EOF>\n"
    | 15 ->
        "<+ should be followed by a number>\n"
    | 9 ->
        "<- should be followed by a number>\n"
    | 7 ->
        "<Unexpected EOF>\n"
    | 40 ->
        "<Misplaced .>\n"
    | 38 ->
        "<Misplaced .>\n"
    | 24 ->
        "<Misplaced .>\n"
    | 22 ->
        "<Misplaced .>\n"
    | 27 ->
        "<Misplaced .>\n"
    | 30 ->
        "<Misplaced .>\n"
    | 36 ->
        "<Misplaced .>\n"
    | 46 ->
        "<Misplaced .>\n"
    | 34 ->
        "<Spurious . after .>\n"
    | 12 ->
        "<EOF in list>\n"
    | 21 ->
        "<Misplaced .>\n"
    | 13 ->
        "<@ should be followed by an sexpr>\n"
    | 49 ->
        "<@ should be followed by an sexpr>\n"
    | _ ->
        raise Not_found
