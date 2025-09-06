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
  let ls_str = loop_read [] in 
  
  (*defining all the letters to check for*)
  let letters = ['a';'b';'c';'d';'e';'f';'g';'h';'i';'j';'k';'l';'m';'n';'o';'p';'q';'r';'s';'t';'u';'v';'w';'x';'y';'z'] in

  let rec check_lines lines = 
    match lines with 
    | [] -> () (*stop at the end of the list of strings*)
    |line::next ->
      let rec check_letters letters = 
        match letters with
        | [] -> true
        | x :: xs -> 
          if not (String.contains line x) then (*checks if the letter is in the line*)
            false
          else
            check_letters xs (*if letter in line, then goes to tail of list*)
      in
      if check_letters letters then (*if all the letters are in the line*)
        file_write true
      else
        file_write false;

      check_lines next (*going to the next line*)
  in

  check_lines ls_str (*calling the function with the list of strings*)
;;

(* Make sure to include this in your submission *)
pangram "input.txt" "output.txt" 