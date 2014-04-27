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

(* calculates the massey point spread vector of a data set and
 * stores it in matrix format for later use *)
let rec massey_point_spread (stats: (string * int) list list) 
	: float array array =
  match stats with
  | [] -> [||]
  | game :: season -> let [(t1,s1); (t2,s2)] = game in
		      let ps1 = float(s1) -. float(s2) in 
		      Array.append [|[|ps1|]|] 
				   (massey_point_spread season)
;;

let points_vector = massey_point_spread sample_data

(* assigns each team a unique matrix index *)
let rec assignment (teams: string list) (index: int) 
	: (string * int) list =
  match teams with
  | [] -> []
  | team :: league -> (team, index) :: assignment league (index + 1)
  
let index_list = assignment teams_list 0
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
let populate_massey (index_stats: (string * int) list list)
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
       help_populate_massey season (game_number + 1) in
  help_populate_massey index_stats 0
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
 * which is found by solving the linear system of 
 * (A^T)(t)(x) = (A^T)(b) *)
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
  let rec insert elt list = 
    match list with
    | [] -> [elt]
    | first :: rest -> let (_, float) = first in
		       let (_, float2) = elt in
		       if float2 <= float
		       then elt :: list
		       else first :: (insert elt rest)
  in 
  match list with
  | [] -> []
  | first :: rest -> insert first (sort_teams rest)
;;

(* strips the ranking vector from a simplified, augmented matrix *)
let strip_results (matrix : float array array) : float array array =
  let mat_size = Array.length matrix in
  let row_size = Array.length matrix.(0) in
  let vector = Array.make_matrix ~dimx:mat_size ~dimy:1 0. in
  for i = 0 to (mat_size - 1) do 
    vector.(i).(0) <- matrix.(i).(row_size - 1) 
  done;
  vector
;;

(* prints the results in a semi-pretty format *)
let print_results (hierarchy: (string * float) list) = 
  let rankings = List.rev (hierarchy) in
  List.iter rankings 
    (fun x -> let (string, _) = x in print_string string; 
				     print_newline ())

(* all of massey's functionality, compressed to 1 function *)
let calculate_massey () =
  (* first entry point for passed in data *)
  let teams_list = team_list sample_data in
  let indexed_list = assignment teams_list 0 in
  (* second entry point for passed in data *)
  let updated_data = update_index sample_data indexed_list 0 in
  let massey_matrix = populate_massey updated_data in
  (* third entry point for passed in data *)
  let points = massey_point_spread sample_data in
  let left_side = multiply (transpose massey_matrix) massey_matrix in
  let right_side = multiply (transpose massey_matrix) points in
  let augmented = augment left_side right_side in
  let simplified = rref augmented in
  let rankings = strip_results simplified in
  let teams_and_rating = sort_teams 
			   (associate_value indexed_list rankings) in
    print_results teams_and_rating
;;

calculate_massey ()
;;

(* Now we can move on to implementing Minton's method.
 * We first need to write a function that returns the number of times 
 * a given string occurs in a (string * int) list list...
 * then we update the corresponding index of that string to that number, 
 * and use the populate_massey method to turn the other indecies into -1...
 * then we just augment, row_reduce, strip_results, sort teams, and print! *)

(* finds the number of games a given team has played *)
let rec num_games (stats : (string * int) list list) (team : string) : int = 
  match stats with 
  | [] -> 0
  | game :: season -> let [(t1, _); (t2, _)] = game in
		      if t1 = team || t2 = team
		      then 1 + num_games season team 
		      else num_games season team
;;
		      
(* populates the minton matrix *)
let populate_minton (indexed_stats: (string * int) list list)
	: float array array =
  let games = List.length indexed_stats in
  let matrix_x = Array.make_matrix ~dimx:(games) ~dimy:(games) 0. in
  let rec help_populate_minton (index_stats: (string * int) list list)
				 (game_number: int)
	  : float array array =
    match index_stats with
    | [] -> matrix_x
    | game :: season ->
       let [(t1, id_1); (t2, id_2)] = game in 
       matrix_x.(id_1).(id_1) <- (float (num_games indexed_stats t1));
       matrix_x.(id_2).(id_2) <- (float (num_games indexed_stats t2));
       matrix_x.(id_1).(id_2) <- -1.;
       matrix_x.(id_2).(id_1) <- -1.;
       help_populate_minton season (game_number + 1) in
  help_populate_minton indexed_stats 0
;;

(* creates a float * float type that stores the number of points 
 * a given team has scored and then number of points that have 
 * been scored against it *)
(* make sure to initialize with team_point and opponent_points = 0. *)
let rec create_vars (stats: (string * int) list list) 
  		    (team : string) (team_points : float) 
		    (opponent_points : float) : float * float =
  let rec pull_games (stats: (string * int) list list) (team: string) : 
	             (string * int) list list = 
    match stats with
    | [] -> []
    | game :: season -> let [(t1, s1); (t2, s2)] = game in
			if team = t1 || team = t2 
		        then game :: (pull_games season team)
		        else pull_games season team in
  let stat_list = pull_games stats team in
  let rec vars_helper (stats1: (string * int) list list)
		      (team1: string) (team_points1: float)
		      (opponent_points1: float) : (float * float) =
    match stats1 with
    | [] -> (team_points1, opponent_points1)
    | game ::  season -> 
       let [(t1, s1); (t2, s2)] = game in
       if t1 = team 
       then let team_points = team_points1 +. float(s1) in
	    let opponent_points = opponent_points1 +.
				    float(s2) in
	    vars_helper season team1 team_points opponent_points
       else let team_points = team_points1 +. float(s2) in
	    let opponent_points = opponent_points1 +. 
				    float(s1) in
	    vars_helper season team1 team_points opponent_points in
  vars_helper stat_list team team_points opponent_points 
;;
    
(* creates the actual point spread for a given team! *)
let rec create_spread (stats: (string * int) list list) 
  		      (team : string) (team_points : float) 
		      (opponent_points : float) : float = 
  let (s1, s2) = (create_vars stats team 0. 0.)
  in s1 -. s2
;;
					        
(* calculates the massey point spread vector of a data set and
 * stores it in matrix format for later use *)
let rec minton_point_spread (stats: (string * int) list list)
	: float array array =
  (* we are going to iterate through the indexed list so we can keep 
   * track of which slots the point spreads should be stored *)
  let indexed_teams = assignment (team_list stats) 0 in
  let rec mps_helper (teams: (string * int) list) :
	    float array array = 
    match teams with
    | [] -> [|[||]|]
    | cur_team :: league -> let (team, _) = cur_team in 
			    let ps1 = (create_spread stats team 0. 0.) in
			    Array.append [|[|ps1|]|] (mps_helper league) in
  mps_helper indexed_teams
;;

(* all of massey's functionality, compressed to 1 function *)
let calculate_minton () =
  (* first entry point for passed in data *)
  let teams_list = team_list sample_data in
  let indexed_list = assignment teams_list 0 in
  (* second entry point for passed in data *)
  let updated_data = update_index sample_data indexed_list 0 in
  let minton_matrix = populate_minton updated_data in
  (* third entry point for passed in data *)
  let points = minton_point_spread sample_data in
  let augmented = augment minton_matrix points in
  let simplified = rref augmented in
  let rankings = strip_results simplified in
  let teams_and_rating = sort_teams 
			   (associate_value indexed_list rankings) in
    print_results teams_and_rating
;;

calculate_minton ()
;;
