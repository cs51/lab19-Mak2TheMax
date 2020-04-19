
(* Customer account identifiers *)
type id = int ;;

(* cDBSIZE -- Initial size of the database hash table *)
let cDBSIZE = 100 ;;

(* The account database itself, a hash table mapping ids to pairs of
name and balance, initially an empty hash table. *)
let db : (int, (string * int)) Hashtbl.t =  Hashtbl.create cDBSIZE ;;

(* See .mli file for documentation on individual functions *)
let create (id : id) (name : string) : unit =
  Hashtbl.add db id (name, 0) ;;

let find (id : id) : string * int =
  Hashtbl.find db id ;;

let exists (id : id) : bool =
  try
    ignore (find id);
    true
  with
  | Not_found -> false ;;

let balance (id : id) : int =
  let _name, bal = find id  in bal ;;

let name (id : id) : string =
    let name, _bal = find id  in name ;;

let update (id : id) (value : int) : unit =
  let nam = name id in  Hashtbl.replace db id (nam, value) ;;

let close (id : id) : unit =
  Hashtbl.remove db id ;;

let dump () =
  db
  |> Hashtbl.iter (fun i (nam, bal) ->
                  Printf.printf "[%d] %s -> %d\n" i nam bal) ;;
