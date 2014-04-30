open Core.Std
open Array

module type MATRIX =
sig 
  
  type m

  (* takes the transpose of the matrix but effectively swapping the "i" and 
   * and "j" coordinates of all elements in our matrix, given by 
   * array.(i).(j). used primarily in Massey's ranking algorithm.
   * can operate on any nxm matrix, but we'll only be using it for 
   * square matrices *)
  val transpose: m -> m

  (* multiplies two matrices for use in each of our ranking algorithms. 
   * most of our ranking algorithms will only require a matrix times a vector,
   * which is a matrix with 1-column. Massey's algorithm, though, will require 
   * us to multiply two nxn matrices *)
  val multiply: m -> m -> m

  (* as an alternative to invert, we'll implement the functionality for
   * row_reduce if we find some meta problems in inverting a matrix. we might
   * run into problems with trying to invert non-invertible matrices, but we're 
   * not yet sure if we'll ever be passing in matrices for which the determinant
   * is equal to 0. we'll meet with faculty in the math department to discuss 
   * this week *)
  val echelon: m -> m

  val augment: m -> m -> m

  val empty_matrix: unit -> m

  val empty_float_array: unit -> m

  val append: m -> m -> m

  val fill: float -> m

  val make_matrix: int -> int -> float -> m

  val fix_elt: m -> int -> int -> float -> unit

  val find_elt: m -> int -> int -> float 

  val matrix_length: m -> int

  val array_length: m -> int

  val init: int -> (int -> 'a) -> 'a array

  val num_columns: m -> int
 
end


module Matrix : MATRIX =
struct

  open Array

  type m = float array array
  
  let empty_matrix () = [|[||]|]
  let empty_float_array () = [||]
  let append m1 m2 = Array.append m1 m2
  let fill float = [|[|float|]|]
  let make_matrix dim1 dim2 f = Array.make_matrix dim1 dim2 f
  let fix_elt matrix i j replacement = matrix.(i).(j) <- replacement
  let find_elt matrix i j = matrix.(i).(j)
  let matrix_length matrix = Array.length matrix
  let array_length matrix = Array.length matrix.(0)
  let init int func = Array.init int func 
  let num_columns mat = Array.length mat.(0)

  let transpose (matrix : m) : m  =  
    let new_height = Array.length matrix.(0) in
    let new_width = Array.length matrix in
    let new_matrix = (Array.make_matrix ~dimx:new_height ~dimy:new_width 1.) in
    let rec ihelper (c : int) : m =
      let rec jhelper (c2 : int) : m =
	Array.set new_matrix.(c) c2 matrix.(c2).(c);
	if c2 < new_width-1 then jhelper (c2 + 1)
	else ihelper (c + 1) in
      if c < new_height then jhelper (0)
      else new_matrix in
    ihelper (0)

  let swap_rows m i j =
    let tmp = m.(i) in
    m.(i) <- m.(j);
    m.(j) <- tmp;
;;

(* augments a matrix for rrefing *)
  let augment (matrix : m) 
	      (vector : m) 
      : m =
    let len = Array.length matrix.(0) in 
    let new_matrix = Array.make_matrix ~dimx:len ~dimy:(len + 1) 0. in
    for i = 0 to len-1 do
      new_matrix.(i) <- append matrix.(i) vector.(i)
    done;
    new_matrix

  let v = [|[|1.|];[|2.|]|];;
  let m = [|[|9.; 3.|]; [|4.; 6.|]|];;
  let m_t = transpose [|[|9.; 3.|]; [|4.; 6.|]|];;

let ericmap f xs ys =
  let n = Array.length xs in
  if Array.length ys <> n then raise (Invalid_argument "Array.map2");
  Array.init n (fun i -> f xs.(i) ys.(i))


let multiply (mat1 : m) (mat2 :m) : m =
  let tmat2 = transpose mat2 in
  let height = Array.length mat1 in
  let width = Array.length tmat2 in 
  let nmat = (Array.make_matrix ~dimx:(height) ~dimy:(width) 1.) in
  let rec column (c1 : float array) (c2 : float array) (cc : int) : m =
    let rec row (r1 : float array) (r2 : float array) (rc : int) : m = 
      Array.set nmat.(cc) rc (Array.fold_right ~f:(+.) (ericmap ( *. ) r1 r2) ~init:(0.));
      if rc < width-1 then row mat1.(cc) tmat2.(rc + 1) (rc+1)
      else column mat1.(cc) tmat2.(0) (cc+1) in
    if cc < height then row mat1.(cc) c2 0
    else nmat in
  column mat1.(0) tmat2.(0) (0)


let echelon (matx: m)  =
    let lead = ref 0
    and rows = Array.length matx
    and columns = Array.length matx.(0) in
    for r = 0 to rows - 1 do
      if columns <= !lead then
        raise Exit;
      let entry = ref r in
      while matx.(!entry).(!lead) = 0. do
        entry := !entry+1;
        if rows = !entry then begin
          entry := r;
          lead := !lead+1;
          if columns = !lead then
            raise Exit;
        end
      done;
      swap_rows matx !entry r;
      let part1 = matx.(r).(!lead) in
      matx.(r) <- Array.map (fun v -> v /. part1) matx.(r);
      for entry = 0 to rows-1 do
        if entry <> r then
          let part2 = matx.(entry).(!lead) in
          matx.(entry) <- Array.mapi (fun a b -> b -. part2 *. matx.(r).(a)) matx.(entry);
      done;
      lead := !lead+1;
    done;
    matx
 
end

(*
let a = ericmap (-) [|1;2;3|] [|6;3;1|] = [|-5;-1;2|] ;;
let b = ericmap (-) [|2;4;6|] [|1;2;3|] = [|1;2;3|]

let ma = [|[|1.; 2.; 3.|]; [|4.; 5.; 6.|]; [|7.; 8.; 9.|]|];;
let mb = [|[|1.;2.|]; [|3.;4.|]|]
    
let mc = multiply ma ma
 *)
