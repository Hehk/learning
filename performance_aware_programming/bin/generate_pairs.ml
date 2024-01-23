open Base

type coord = { x : float; y : float }
type pair = coord * coord
type bounds = { x : float; y : float; x_range : float; y_range : float }
type sampling = Uniform | Cluster

type settings = {
  sampling : sampling;
  n : int;
  json : bool;
  log : bool;
  seed : int;
}

let default_settings =
  { sampling = Uniform; n = 1000; json = true; log = false; seed = 0 }

let help = {|
Usage: ./generate_pairs [uniform|cluster] [n]

  Flags:
    -no-json: turn off json output
    -log: output log
    -seed: random seed (example: -seed=1234)
|}

let parse_args argv =
  let fail_with_usage () =
    Stdio.prerr_endline "Invalid arguments";
    Stdio.prerr_endline help;
    Stdlib.exit 1
  in
  let flags, args =
    Array.partition_tf argv ~f:(fun arg -> phys_equal arg.[0] '-')
  in
  let settings =
    Array.fold flags ~init:default_settings ~f:(fun s flag ->
        let parts = String.split flag ~on:'=' in
        match parts with
        | [ "-no-json" ] -> { s with json = false }
        | [ "-log" ] -> { s with log = true }
        | [ "-seed"; n ] -> { s with seed = Int.of_string n }
        | ["-help"] | ["--help"] | ["-h"] -> Stdio.prerr_endline help; Stdlib.exit 0
        | _ -> fail_with_usage ())
  in
  if Array.length args <> 3 then fail_with_usage ();
  let pos_args = Array.sub args ~pos:1 ~len:2 in
  let settings =
    match pos_args with
    | [| sampling; n |] ->
        let sampling =
          match sampling with
          | "uniform" -> Uniform
          | "cluster" -> Cluster
          | _ -> fail_with_usage ()
        in
        let n = Int.of_string n in
        { settings with sampling; n }
    | _ -> fail_with_usage ()
  in
  settings

let generate_pairs bounds n =
  let x () =
    Random.float_range (bounds.x -. bounds.x_range) (bounds.x +. bounds.x_range)
  in
  let y () =
    Random.float_range (bounds.y -. bounds.y_range) (bounds.y +. bounds.y_range)
  in
  let rec aux acc = function
    | 0 -> acc
    | n ->
        let a = { x = x (); y = y () } in
        let b = { x = x (); y = y () } in
        aux ((a, b) :: acc) (n - 1)
  in
  aux [] n

let pair_json ((a, b) : pair) =
  Printf.sprintf {|{"x0": %f, "y0": %f, "x1": %f, "y1": %f}|} a.x a.y b.x b.y

let pairs_json (pairs : pair list) =
  let pairs_json = List.map pairs ~f:pair_json in
  Printf.sprintf "[%s]" (String.concat ~sep:"," pairs_json)

let gen_clusters bounds =
  let columns = 8. in
  let rows = 8. in
  let n = Int.of_float @@ (columns *. rows) in
  let clusters = Array.create ~len:n bounds in
  let x_step = bounds.x_range /. columns in
  let y_step = bounds.y_range /. rows in
  let x_min = bounds.x -. bounds.x_range in
  let y_min = bounds.y -. bounds.y_range in
  let rec aux i =
    if i = n then ()
    else
      (* The 0.5 is added to put the center in the middle of the coord *)
      let x_offset = Float.of_int (i % Int.of_float columns) +. 0.5 in
      let y_offset = Float.round_down (Float.of_int i /. columns) +. 0.5 in
      let x = x_min +. (x_offset *. x_step) in
      let y = y_min +. (y_offset *. y_step) in
      let bounds = { x; y; x_range = x_step /. 2.; y_range = y_step /. 2. } in
      clusters.(i) <- bounds;
      aux (i + 1)
  in
  aux 0;
  clusters

let haversine ?(radius = 6372.8) ((a, b) : pair) =
  let x0 = a.x in
  let y0 = a.y in
  let x1 = b.x in
  let y1 = b.y in
  let open Float in
  let dlat = (x1 - x0) * pi / 180.0 in
  let dlon = (y1 - y0) * pi / 180.0 in
  let a =
    (sin (dlat / 2.0) ** 2.0)
    + (cos (x0 * pi / 180.0) * cos (x1 * pi / 180.0) * (sin (dlon / 2.0) ** 2.0))
  in
  let c = 2.0 * asin (sqrt a) in
  radius * c

let () =
  let settings = Sys.get_argv () |> parse_args in
  Random.init settings.seed;
  let full_bounds = { x = 0.0; y = 0.0; x_range = 180.0; y_range = 90.0 } in
  let bounds =
    match settings.sampling with
    | Uniform -> full_bounds
    | Cluster ->
        let clusters = gen_clusters full_bounds in
        let n_clusters = Array.length clusters in
        clusters.(Random.int n_clusters)
  in
  let pairs = generate_pairs bounds settings.n in
  let () =
    if settings.log then
      let average =
        List.map pairs ~f:haversine |> List.fold ~init:0.0 ~f:( +. )
        |> fun sum -> sum /. (Float.of_int @@ List.length pairs)
      in
      let message = Printf.sprintf "Average distance: %f\n" average in
      Stdio.prerr_endline message
    else ()
  in
  if settings.json then
    let json = pairs_json pairs in
    Stdio.print_endline json
  else ()
