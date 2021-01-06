
let pp_pos  out { Ppxlib.pos_lnum; pos_cnum; pos_bol; _} = Format.fprintf out "%d:%d"  pos_lnum (pos_cnum - pos_bol)

let pp_fpos out { Ppxlib.pos_lnum; pos_cnum; pos_bol; pos_fname; _} = Format.fprintf out "%s %d:%d"  pos_fname pos_lnum (pos_cnum - pos_bol)

type loc = Ppxlib.position * Ppxlib.position

let pp_loc out loc = Format.fprintf out "%a-%a" pp_fpos (fst loc) pp_pos (snd loc)

let rec showList show = function [] -> "" | [s] -> show s | s::ss -> show s^" "^showList show ss

let rec pp_List pp out = function 
    | []    -> () 
    | [s]   -> pp out s 
    | s::ss -> pp out s; Format.pp_print_string out " "; pp_List pp out ss

type  sexp = 
  | Id          of string  [@printer fun fmt s -> Format.pp_print_string fmt s]
  | String      of string  [@printer fun fmt s -> Format.fprintf fmt "\"%s\"" s]
  | Int         of int     [@printer fun fmt s -> Format.fprintf fmt "%d" s]
  | Pair        of sexp*sexp  
    [@printer fun fmt (a,d) -> Format.fprintf fmt "(%a . %a)" pp_sexp a pp_sexp d]
  | List        of sexp list  
    [@printer fun fmt ss -> Format.fprintf fmt "(%a)" (pp_List pp_sexp) ss]
  | At          of loc * sexp [@printer fun fmt (l,s) -> Format.fprintf fmt "%a@%a" pp_sexp s pp_loc l]
  | EndFile
  [@@deriving show]



