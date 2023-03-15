open Parsing;;
open Lexing;;

open Lambda;;
open Parser;;
open Lexer;;

open String;;
open Str;;

let rec process_line line =
  let r = regexp ";;" in
    if string_match r line (length line - 2) then string_before line (length line)
    (* if contains line ';' then sub line 0 (1+index line ';') *)
    else begin
      print_string "  ";
    
      flush stdout;
      process_line (line ^ " " ^ read_line ())
  end
;;


let top_level_loop () =
  print_endline "Evaluator of lambda expressions...";
  let rec loop (vctx, tctx) =
    print_string ">> ";
    flush stdout;
    try
      let line = trim (read_line ()) in
      if length line = 0 then loop (vctx, tctx)
      else let whole_line = string_before (process_line line) (length line - 2) in
      let c = s token (from_string(whole_line)) in
      loop (execute (vctx, tctx) c)
    with
       Lexical_error ->
         print_endline "lexical error";
         loop (vctx, tctx)
     | Parse_error ->
         print_endline "syntax error";
         loop (vctx, tctx)
    
     | Type_error e ->
         print_endline ("type error: " ^ e);
         loop (vctx, tctx)
     | End_of_file ->
         print_endline "...bye!!!"
  in
    loop (emptyctx, emptyctx)
  ;;

top_level_loop ()
;;

