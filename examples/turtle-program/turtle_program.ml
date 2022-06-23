type direction = Up | Down | Left | Right

let turn_right = function Up -> Left | Left -> Down | Down -> Right | Right -> Up
let turn_left = function Left -> Up | Down -> Left | Right -> Down | Up -> Right

let move n (x,y) = function
  | Up -> (x, y + n)
  | Down -> (x, y - n)
  | Left -> (x - n, y)
  | Right -> (x + n, y)

let pen_down = ref false
let direction = ref Up

let set_pen_down v =
  if not @@ Guile.Bool.is_bool v then
    failwith "expected boolean argument";
  let v = Guile.Bool.from_raw v in
  pen_down := v;
  Guile.eol

let turn_left _ =
  direction := turn_left !direction;
  Guile.eol

let turn_right _ =
  direction := turn_right !direction;
  Guile.eol

let move_by n =
  if not @@ Guile.Number.is_integer n then
    failwith "expected numeric arg";
  let n = Guile.Number.int_from_raw n in
  let x, y =
    let cur_pos = Graphics.current_point () in
    move n cur_pos !direction in
  if !pen_down then
    Graphics.lineto x y;
  Graphics.moveto x y;
  Guile.eol

let move_to x y =
  if (not @@ Guile.Number.is_integer x) ||
     (not @@ Guile.Number.is_integer y) then
    failwith "expected numeric position";
  let x, y = 
    Guile.Number.int_from_raw x,
    Guile.Number.int_from_raw y in
  if !pen_down then
    Graphics.lineto x y;
  Graphics.moveto x y;
  Guile.eol

let () =
  Graphics.open_graph " 400x400+50-0";
  Graphics.auto_synchronize true;
  Graphics.moveto 200 200;
  Guile.init ();
  ignore @@ Guile.Functions.register_fun1 "pen-down" set_pen_down;
  ignore @@ Guile.Functions.register_fun1 ~no_opt:1 "turn-left" turn_left;
  ignore @@ Guile.Functions.register_fun1 ~no_opt:1 "turn-right" turn_right;
  ignore @@ Guile.Functions.register_fun1 "move-by" move_by;
  ignore @@ Guile.Functions.register_fun2 "move-to" move_to;
  Guile.shell ()

