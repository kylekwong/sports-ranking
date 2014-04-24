open Core.Std
open Array

module type MATRIX =
sig 
  
  type m

  (* takes the transpose of the matrix but effectively swapping the "i" and 
   * and "j" coordinates of all elements in our matrix, given by 
   * array.(i).(j). used primarily in Massey's ranking algorithm.
   * can operate on any nxm matrix, but we'll only be using it for square matrices *)
  val transpose: m -> m

  (* multiplies two matrices for use in each of our ranking algorithms. 
   * most of our ranking algorithms will only require a matrix times a vector,
   * which is a matrix with 1-column. Massey's algorithm, though, will require 
   * us to multiply two nxn matrices *)
  val multiply: m -> m

  (* as an alternative to invert, we'll implement the functionality for
   * row_reduce if we find some meta problems in inverting a matrix. we might
   * run into problems with trying to invert non-invertible matrices, but we're 
   * not yet sure if we'll ever be passing in matrices for which the determinant
   * is equal to 0. we'll meet with faculty in the math department to discuss 
   * this week *)
  val row_reduce: m -> m

end

(*
module Matrix : MATRIX =
struct
 *)

  (*type m = float array array*)
  
  let transpose (matrix : float array array(*m*)) : float array array(*m*) =  
    let new_height = Array.length matrix.(0) in
    let new_width = Array.length matrix in
    let new_matrix = (make_matrix ~dimx:new_height ~dimy:new_width 1.) in
    let rec ihelper (c : int) : float array array(*m*) =
      let rec jhelper (c2 : int) : float array array (*m*) =
	Array.set new_matrix.(c) c2 matrix.(c2).(c);
	if c2 < new_width-1 then jhelper (c2 + 1)
	else ihelper (c + 1) in
      if c < new_height then jhelper (0)
      else new_matrix in
    ihelper (0)

  let v = [|[|1.|];[|2.|]|];;
  let m = [|[|9.; 3.|]; [|4.; 6.|]|];;
  let m_t = transpose [|[|9.; 3.|]; [|4.; 6.|]|];;

  (* dummy functions *)
  let multiply m = m
  let invert m = m    

  let row_reduce (matrix : float array array(*m*)) : float array array (*m*) = 
    [|[|9.; 3.|]; [|4.; 6.|]|];;

(*    
end
 *)

let swap_rows m i j =
  let tmp = m.(i) in
  m.(i) <- m.(j);
  m.(j) <- tmp;
;;

(* augments a matrix for rrefing *)
let augment (matrix : float array array(*m*)) (vector : float array array(*m*)) 
  : float array array (*m*) =
  let len = Array.length matrix.(0) in 
  let new_matrix = Array.make_matrix ~dimx:len ~dimy:(len + 1) 0. in
  for i = 0 to len-1 do
    new_matrix.(i) <- append matrix.(i) vector.(i)
  done;
  new_matrix

;;

(* rref *)
let rref m =
    let lead = ref 0 in
    let rows = Array.length m in 
    let cols = Array.length m.(0) in
    for r = 0 to rows - 1 do
      if cols <= !lead then
        raise Exit;
      let i = ref r in
      (* if our diagonal is 0 *)
      while m.(!i).(!lead) = 0. do
        incr i;
        if rows = !i then begin
          i := r;
          incr lead;
          if cols = !lead then
            raise Exit;
        end
      done;
      swap_rows m !i r;
      let lv = m.(r).(!lead) in
      m.(r) <- Array.map (fun v -> v /. lv) m.(r);
      for i = 0 to rows - 1 do
        if i <> r then
          let lv = m.(i).(!lead) in
          m.(i) <- Array.mapi (fun i iv -> iv -. lv *. m.(r).(i)) m.(i);
      done;
      incr lead;
    done
;;

let ericmap f xs ys =
  let n = length xs in
  if length ys <> n then raise (Invalid_argument "Array.map2");
  Array.init n (fun i -> f xs.(i) ys.(i))

let rec row_reduce m index =
  (* calculate the number of rows *)
  let rows = Array.length m in 
  (* calculate the number of columns *)
  let cols = Array.length m.(0) in 
  if m.(index).(index) <> 0. && m.(index).(index) <> 1. then 
    let new_row = Array.map (/.) m.(index) m.(index).(index) in
    ericmap (-) m.(index + 1) (ericmap ( * ) new_row m.(index + 1).(index))
  (*  row_reduce m (index + 1) *)
  (* we are at 0 *)
  else if m.(index).(index) = 0. then row_reduce m (index + 1)
  (* we are at 1 *)
  else ericmap (-) m.(index + 1) (ericmap ( * ) m.(index) m.(index + 1).(index));
  row_reduce m (index + 1)


;;


(* multiply *)
let ericmap f xs ys =
  let n = length xs in
  if length ys <> n then raise (Invalid_argument "Array.map2");
  Array.init n (fun i -> f xs.(i) ys.(i))

let a = ericmap (-) [|1;2;3|] [|6;3;1|] = [|-5;-1;2|] ;;
let b = ericmap (-) [|2;4;6|] [|1;2;3|] = [|1;2;3|]

let ma = [|[|1.; 2.; 3.|]; [|4.; 5.; 6.|]; [|7.; 8.; 9.|]|];;
let mb = [|[|1.;2.|]; [|3.;4.|]|]

let multiply (mat1 : float array array) (mat2 : float array array) : float array array = 
  let tmat2 = transpose mat2 in
  let length = Array.length mat1 in
  let nmat = (make_matrix ~dimx:(Array.length mat1) ~dimy:(Array.length tmat2) 1.) in
  let rec column (c1 : float array) (c2 : float array) (cc : int) : float array array =
    let rec row (r1 : float array) (r2 : float array) (rc : int) : float array array = 
      Array.set nmat.(cc) rc (Array.fold_right ~f:(+.) (ericmap ( *. ) r1 r2) ~init:(0.));
      if rc < length-1 then row mat1.(cc) tmat2.(rc + 1) (rc+1)
      else column mat1.(cc) tmat2.(0) (cc+1) in
    if cc < length then row mat1.(cc) c2 0
    else nmat in
  column mat1.(0) tmat2.(0) (0)
    
let mc = multiply ma ma

let make_rows (teams: (string * string) list) (scores: (int * int) list) : int =
  let (t1, t2) = teams in
  let (s1, s2) = scores in
  let counter_t1 = 0 in 
  let counter_t2 = 0 in 
  if s1 > s2 
  then counter_t1 + 1
  else counter_t2 + 1
    
let x = format_data [("Harvard", "Brown");("Brown, Yale");("Yale","Harvard")] 
		    [(100,0);(100,0);(100,0)];;