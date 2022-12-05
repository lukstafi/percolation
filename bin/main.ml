open Base

let xlen = 3840 
let ylen = 2160

let get_rock prob =
  let open Float.O in
  Array.init xlen ~f:(fun _ -> Array.init ylen ~f:(fun _ -> Random.float 1.0 <= prob))

let pour_water rock =
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

let has_propagated water =
  Array.exists water ~f:(fun column -> column.(ylen - 1))

let () =
  let prob = ref 0.5 in
  let delta = ref 0.1 in
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
      (* rock.(Random.int ylen).(Random.int xlen) <- Random.float 1.0 <= !prob; *)
      Array.iteri rock ~f:(fun i rock_column -> Array.iteri rock_column ~f:(
        fun j has_rock ->
        let color = colormap.(i).(j) in
        if not has_rock && water.(i).(j) then Int.(
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
    delta := 0.1 / Float.of_int picture;
    if Int.(2 * !percolated > !total) then prob := !prob + !delta else prob := !prob - !delta;
    Jpeg.save ("images/Percolation_no_" ^ Int.to_string picture ^ ".jpg") [] (Images.Rgb24 rgb_image);
  done
