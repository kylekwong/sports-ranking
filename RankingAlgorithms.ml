open Core.Std
open Array

let sample_data = [[("Brown", 7); ("Harvard", 24)]; 
		   [("Cornell", 17); ("Yale", 37)]; 
		   [("Penn", 22); ("Dartmouth", 20)]; 
		   [("Columbia", 21); ("Princeton", 24)]; 
		   [("Dartmouth", 0); ("Yale", 30)]; 
		   [("Harvard", 41); ("Cornell", 31)];
		   [("Princeton", 0); ("Brown", 34)]; 
		   [("Penn", 27); ("Columbia", 20)]]

(* places all unique teams found in a data set into a list *)
let rec team_list (teams: (string * int) list list) 
	: string list =
  (* removes duplicates *)
  let rec clean_list (teams: string list) : string list = 
  match teams with
  | [] -> []
  | team :: league -> if not (List.mem league team) 
		      then team :: clean_list league
		      else clean_list league 
  in
  match teams with
  | [] -> []
  | game :: season -> let [(t1,s1); (t2,s2)] = game in
		      clean_list (t1 :: t2 :: (team_list season))
;;

let teams_list = team_list sample_data

(* calculates the point spread vector of a data set and
 * stores it in matrix format for later use *)
let rec point_spread (stats: (string * int) list list) 
	: float array array =
  match stats with
  | [] -> [||]
  | game :: season -> let [(t1,s1); (t2,s2)] = game in
		      let ps1 = float(s1) -. float(s2) in 
		      Array.append [|[|ps1|]|] (point_spread season)
;;

let points_vector = point_spread sample_data

(* assigns each team a unique matrix index *)
let rec assignment (teams: string list) (index: int) 
	: (string * int) list =
  match teams with
  | [] -> []
  | team :: league -> (team, index) :: assignment league (index + 1)
  
let index_list = assignment unique_team_list 0
;;

(* pulls a (team * index) tuple from a list given a search string *)
let rec pull_string (index_list: (string * int) list) 
		    (search: string) : string * int =
  match index_list with 
  | [] -> failwith "We know our search exists..."
  | first :: rest -> let (string, index) = first in
		     if search = string
		     then first 
		     else (pull_string rest search)

(* creates a new dataset, with each original tuple replaced
 * by the new assigned tuple, which contains information 
 * regarding matrix indecies. in other words, now that we've
 * calculated the point spreads, we can throw out the scores 
 * and instead store information about where a team should go 
 * in our matrix *)
let rec update_index (stats: (string * int) list list) 
                     (index_list: (string * int) list)
		     (swap_index: int)
	: (string * int) list list =
  let isolate = List.nth stats swap_index in 
  match isolate with
  | None -> []
  | Some [(t1,s1); (t2,s2)] -> 
     [(pull_string index_list t1); (pull_string index_list t2)] 
       :: (update_index stats index_list (swap_index + 1))

let updated_data = update_index sample_data index_list 0;;

(* now that we have our updated index list, we can create
 * a float array array that reflects each game played! *)
let games = List.length updated_data
let matrix_x = Array.make_matrix ~dimx:(games) ~dimy:(games) 0.
let rec populate_massey (index_stats: (string * int) list list)
			(game_number: int)
	: float array array =
  match index_stats with
  | [] -> matrix_x
  | game :: season ->
     let [(t1, id_1); (t2, id_2)] = game in 
      matrix_x.(game_number).(id_1) <- 1.;
      matrix_x.(game_number).(id_2) <- -1.;
      populate_massey season (game_number + 1)

(*let populate_massey (index_stats: (string * int) list list)
	: float array array =
  let games = List.length index_stats in
  let matrix_x = Array.make_matrix ~dimx:(games) ~dimy:(games) 0. in
    let rec help_populate_massey (index_stats: (string * int) list list)
				 (game_number: int)
	: float array array =
  match index_stats with
  | [] -> matrix_x
  | game :: season ->
     let [(t1, id_1); (t2, id_2)] = game in 
      matrix_x.(game_number).(id_1) <- 1.;
      matrix_x.(game_number).(id_2) <- -1.;
      populate_massey season (game_number + 1) in
  help_populate_massey index_stats 0*)
;;

let v = [|[|3.|];[|5.|];[|1.|];[|2.|];[|7.|];[|8.|];[|9.|];[|11.|]|];;
let m = [|[|9.; 3.|]; [|4.; 6.|]|];;

(* pulls the team associated with a given index *)
let rec pull_team (index_list: (string * int) list) (index : int) 
	: string = 
  match index_list with 
  | [] -> failwith "We won't be dealing with this case!"
  | team :: league -> let (string, number) = team in 
		      if number = index 
		      then string
		      else pull_team league index
;;

(* associates a team with the number of ranking "points" it has,
 * which is equal to Transpose(matrix) * point_spread vector *)
let rec associate_value (index_list: (string * int) list) 
			(point_spread: float array array) 
	: (string * float) list = 
  match index_list with
  | [] -> []
  | team :: league -> let (string, index) = team in 
		     (string, point_spread.(index).(0)) ::
		       associate_value league point_spread
;;

(* sort our (string * float) list in ascending order by floats *)
let rec sort_teams (list: (string * float) list) 
	: (string * float) list =
  match list with
  | [] -> []
  | first :: rest -> insert first (sort_teams rest)
  and 
  insert elt list = 
    match list with
    | [] -> [elt]
    | first :: rest -> let (_, float) = first in
		       let (_, float2) = elt in
		       if float2 <= float
		       then elt :: list
		       else first :: (insert elt rest)
;;

(* prints the results in a semi-pretty format *)
let print_results (hierarchy: (string * float) list) = 
  let rankings = List.rev (hierarchy) in
  List.iter rankings 
    (fun x -> let (string, _) = x in print_string string; 
				     print_newline ())
