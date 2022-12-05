open Base

let xlen = 3840 
let ylen = 2160

let get_rock_vert prob =
  let open Float.O in
  Array.init xlen ~f:(fun _ -> Array.init ylen ~f:(fun _ -> Random.float 1.0 <= prob))

let get_rock_edge prob =
  let open Float.O in
  Array.init xlen ~f:(fun _ -> Array.init ylen ~f:(fun _ ->
    Array.init 2 ~f:(fun _ -> Array.init 2 ~f:(fun _ -> Random.float 1.0 <= prob))))
  
let pour_water_vert_4n rock =
  let water = Array.init xlen ~f:(fun _ -> Array.create ~len:ylen false) in
  let rec propagate = function
  | [] -> ()
  | (i, j)::rest ->
      if i >= 0 && j >= 0 && i < xlen && j < ylen && not rock.(i).(j) && not water.(i).(j) then (
        water.(i).(j) <- true;
        propagate ((i - 1, j)::(i + 1, j)::(i, j - 1)::(i, j + 1)::rest)
      ) else propagate rest in
  for i = 0 to xlen - 1 do propagate [i, 0] done;
  water

let pour_water_vert_8n rock =
  let water = Array.init xlen ~f:(fun _ -> Array.create ~len:ylen false) in
  let rec propagate = function
  | [] -> ()
  | (i, j)::rest ->
      if i >= 0 && j >= 0 && i < xlen && j < ylen && not rock.(i).(j) && not water.(i).(j) then (
        water.(i).(j) <- true;
        let more = List.concat_map [-1; 0; 1] ~f:(fun di -> List.map [-1; 0; 1] ~f:(fun dj ->
          (i + di, j + dj))) in
        propagate (more @ rest)
      ) else propagate rest in
  for i = 0 to xlen - 1 do propagate [i, 0] done;
  water

let pour_water_edge_8n rock =
  let water = Array.init xlen ~f:(fun _ -> Array.create ~len:ylen false) in
  let rec propagate = function
  | [] -> ()
  | (i, j)::rest ->
        if i >= 0 && j >= 0 && i < xlen && j < ylen && not water.(i).(j) then (
          water.(i).(j) <- true;
          let more = List.concat_map [-1; 0; 1] ~f:(fun di -> List.concat_map [-1; 0; 1] ~f:(fun dj ->
            if i + di < 0 || j + dj < 0 then []
            else if rock.(i + Int.min di 0).(j + Int.min dj 0).(Int.abs di).(Int.abs dj) then []
            else [i + di, j + dj])) in
          propagate (more @ rest)
        ) else propagate rest in
  for i = 0 to xlen - 1 do propagate [i, 0] done;
  water
  
let has_propagated water =
  Array.exists water ~f:(fun column -> column.(ylen - 1))

let main get_rock pour_water prob0 delta0 =
  let prob = ref prob0 in
  let delta = ref delta0 in
  for picture = 1 to 50 do
    (* let rock = get_rock !prob in *)
    let total = ref 0 in
    let percolated = ref 0 in
    let colormap = Array.init xlen ~f:(fun _ -> Array.init ylen ~f:(fun _ -> Color.({r=0; g=0; b=0}))) in
    List.iteri [(85, 0, 0); (0, 85, 0); (0, 0, 85); (85, 85, 0); (85, 0, 85); (0, 85, 85)] ~f:(
      fun step (r, g, b) ->
      let rock = get_rock Float.(!prob + Float.of_int step * !delta) in
      let water = pour_water rock in
      Int.incr total;
      (if has_propagated water then Int.incr percolated);
      Array.iteri water ~f:(fun i water_column -> Array.iteri water_column ~f:(
        fun j has_water ->
        let color = colormap.(i).(j) in
        if has_water then Int.(
          color.r <- color.r + r; color.g <- color.g + g; color.b <- color.b + b
    ))));
    let rgb_image = Rgb24.create xlen ylen in
    for i = 0 to xlen - 1 do
      for j = 0 to ylen - 1 do
        Rgb24.set rgb_image i j colormap.(i).(j)
      done
    done;
    Stdio.printf "\n%i: prob=%F delta=%F\n%!" picture !prob !delta;
    let open Float.O in
    (* delta := !delta * 0.9; *)
    delta := delta0 / Float.of_int picture;
    if Int.(2 * !percolated > !total) then prob := !prob + !delta else prob := !prob - !delta;
    Jpeg.save ("images/Percolation_no_" ^ Int.to_string Int.(picture+50) ^ ".jpg") [] (Images.Rgb24 rgb_image);
  done

let () = (* main get_rock_vert *) ignore (pour_water_vert_4n, 0.5, 0.1)

let () = (* main *) ignore (get_rock_vert, pour_water_vert_8n, 0.5, 0.1)

(* let () = main get_rock_edge pour_water_edge_8n 0.5 0.4 *)

let () = main get_rock_edge pour_water_edge_8n 0.7 0.01
