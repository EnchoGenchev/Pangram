let pangram (inFile : string) (outFile : string) : unit =

  let ic = open_in inFile in

  let oc = open_out outFile in 

  let rec loop_read acc =
      (* We use try with to catch the End_of_file exception. *)
      try 
          (* Read a line from ic. Build a new list with l::acc
             and pass to next recursive call. *)
          let l = input_line ic in loop_read (l::acc)
      with
        (* At the end of file, we will reverse the string so it's in correct order*)
      | End_of_file -> List.rev acc in

   (* writes out output file *)
  let file_write bool_val = Printf.fprintf oc "%b\n" bool_val in

  (* list of the strings *)
  let ls_str = loop_read [] in () ;;

  (* ***** Code From Here, Replace () above and write your code ***** *)

(* Make sure to include this in your submission *)
pangram "input.txt" "output.txt" 