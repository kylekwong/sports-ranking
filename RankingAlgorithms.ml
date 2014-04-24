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

let sample_vector = [|[|1.|];[|2.|]|];;

(* places all teams found in a data set into a list *)
let rec team_list (teams: (string * int) list list) 
	: string list =
  match teams with
  | [] -> []
  | game :: season -> let [(t1,s1); (t2,s2)] = game in
		      t1 :: t2 :: (team_list season)
;;

let teams_list = team_list sample_data
;;

(* removes duplicates from team lists *)
let rec clean_list (teams: string list) : string list = 
  match teams with
  | [] -> []
  | team :: league -> if not (List.mem league team) 
		      then team :: clean_list league
		      else clean_list league

let unique_team_list = clean_list (team_list sample_data)
;;

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
;;

(* assigns each team a unique matrix index *)
let rec assignment (teams: string list) (index: int) 
	: (string * int) list =
  match teams with
  | [] -> []
  | team :: league -> (team, index) :: assignment league (index + 1)
  
let index_list = assignment unique_team_list 0
;;

(* finds the nth element of a list--
 * IMPORTANT NOTE: 1-INDEXED *)
let rec find_nth (list: (string * int) list) (index: int) : 'a = 
  match list with
  | [] -> []
  | elt :: rest -> if index = 1 then elt
		   else find_nth rest (index - 1)


(* creates a new dataset, with each original tuple replaced
 * by the new assigned tuple, which contains information 
 * regarding matrix indecies *)

(* update indices of the matrix with regards 
 * to the numbers in the data set tuples *)

let rec populate_colley (stats: (string * int) list list) 
	: float array array =
  let rows = List.length stats in 
  let matrix = Array.make_matrix ~dimx:(rows) ~dimy:(rows + 1) 0. in
  match stats with
  | [] -> matrix
  | game :: season -> let [(t1, s1); (t2, s2)] = game in 
      
;;
