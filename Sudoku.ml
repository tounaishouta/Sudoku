open Printf

let flip f x y = f y x

let range n =
  let rec loop i xs =
    if i = 0 then
      xs
    else
      loop (i - 1) ((i - 1) :: xs)
  in
  loop n []

let minimum_with_index arr =
  let l = Array.length arr in
  let rec loop (i, m) j =
    if j = l then
      (i, m)
    else if arr.(j) < m then
      loop (j, arr.(j)) (j + 1)
    else
      loop (i, m) (j + 1)
  in
  if l = 0 then
    failwith "minimum_with_index called on empty array"
  else
    loop (0, arr.(0)) 1

let read_line_option () =
  try Some (read_line ())
  with End_of_file -> None

type sudoku = { admit : bool array; count : int array }

exception No_solution

let box_size = 3
let size     = box_size * box_size
let grd      = 0
let row      = 1
let col      = 2
let box      = 3
let view     = 4
let n_coord  = size * size * size
let n_block  = view * size * size
let defined  = 0xDEF
let digits   = "123456789"

let coord i j k = (i * size + j) * size + k
let block v p q = (v * size + p) * size + q

let parents =
  Array.init n_coord (fun c ->
    let i = c / size / size in
    let j = c / size mod size in
    let k = c mod size in
    let p = i / box_size * box_size + j / box_size in
    [block grd i j; block row i k; block col j k; block box p k]
  )

let children =
  let children' = Array.make n_block [] in
  let () =
    flip Array.iteri parents (fun c bs ->
      flip List.iter bs (fun b ->
        children'.(b) <- c :: children'.(b)
      )
    )
  in
  Array.init n_block (fun c -> List.rev children'.(c))

let init () =
  { admit = Array.make n_coord true; count = Array.make n_block size }

let copy s =
  { admit = Array.copy s.admit; count = Array.copy s.count }

let assign s c =
  let q = Queue.create () in
  let () = Queue.add c q in
  let rec loop () =
    if Queue.is_empty q then
      s
    else (
      let c0 = Queue.take q in
      if not s.admit.(c0) then raise No_solution;
      flip List.iter parents.(c0) (fun b1 ->
        s.count.(b1) <- defined;
        flip List.iter children.(b1) (fun c2 ->
          if c2 <> c0 && s.admit.(c2) then (
            s.admit.(c2) <- false;
            flip List.iter parents.(c2) (fun b3 ->
              s.count.(b3) <- s.count.(b3) - 1;
              if s.count.(b3) = 0 then raise No_solution;
              if s.count.(b3) = 1 then (
                flip List.iter children.(b3) (fun c4 ->
                  if s.admit.(c4) then Queue.add c4 q
                )
              )
            )
          )
        )
      );
      loop ()
    )
  in
  loop ()

let read input' =
  let s = init () in
  let input =
    if String.length input' > size * size then
      String.sub input' 0 (size * size)
    else
      input'
  in
  let () =
    flip String.iteri input (fun ij ch ->
      try
        let i = ij / size in
        let j = ij mod size in
        let k = String.index digits ch in
        ignore (assign s (coord i j k))
      with Not_found -> ()
    )
  in
  s

let show s =
  let buf = Buffer.create (size * size) in
  let () =
    flip List.iter (range size) (fun i ->
      flip List.iter (range size) (fun j ->
        match List.filter (fun k -> s.admit.(coord i j k)) (range size) with
        | [k] -> Buffer.add_char buf (String.get digits k)
        | _   -> Buffer.add_char buf '.'
      )
    )
  in
  Buffer.contents buf

let rec search s =
  let (b, m) = minimum_with_index s.count in
  if m = defined then
    s
  else
    let rec loop = function
      | [] ->
          raise No_solution
      | c :: cs ->
          try search (assign (copy s) c)
          with No_solution -> loop cs
    in
    loop children.(b)

let solve input =
  try show (search (read input))
  with No_solution -> "NO SOLUTION"

let () =
  let rec loop () =
    match read_line_option () with
    | None       -> ()
    | Some input -> printf "%s\n%!" (solve input); loop ()
  in
  loop ()
