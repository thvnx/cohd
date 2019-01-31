
let idx = ref 0
let new_id (id:int ref) =  
  id := !id + 1;
  string_of_int (!id - 1)


let cohd_tmp = "__cohd_"


let warnwith str = prerr_string ("warning: " ^ str ^ "\n")














(** get a string list of a string
 @param str string to split
 @param sep separator used to split the string *)
let string_to_list str sep =
  Str.split (Str.regexp sep) str
;;


let insert_before pos ins str =
  let s_beg = String.sub str 0 pos in
  let s_end = String.sub str pos ((String.length str - pos)) in
  s_beg ^ ins ^ s_end



let remove_spaces s =
  let res = ref "" in
  String.iter (fun i -> 
    if i <> ' ' then res := !res ^ (Char.escaped i)
  ) s;
  !res

let remove_character s c =
  let res = ref "" in
  String.iter (fun i -> 
    if i <> c then res := !res ^ (Char.escaped i)
  ) s;
  !res

let replace_character s c1 c2 =
  let res = ref "" in
  String.iter (fun i -> 
    if i <> c1 then res := !res ^ (Char.escaped i)
    else res := !res ^ (Char.escaped c2)
  ) s;
  !res


let conserve_special_characters s =
  let res = ref "" in
  String.iter (fun i -> 
    match i with
    | '\t' -> res := !res ^ "\\t"
    | '\n' -> res := !res ^ "\\n"
    | _ -> res := !res ^ (Char.escaped i)
  ) s;
  !res

