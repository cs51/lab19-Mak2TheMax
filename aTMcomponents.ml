
open Printf ;;
open Scanf ;;

(* Make use of an account database *)
module DB = Database ;;

(* Customer account identifiers *)
type id = int ;;

(* Possible actions that an ATM customer can perform *)
type action =  | Balance  (* balance inquiry *)
               | Withdraw of int   (* withdraw an amount *)
               | Deposit of int    (* deposit an amount *)
               | Next              (* finish this customer and move on to the next one *)
               | Finished          (* shut down the ATM and exit entirely *);;

(* A specification of a customer name and initial balance *)
type account_spec = {name : string; id : id; balance : int} ;;
(*....................................................................
ATM component behaviors. See .mli file for further documentation. *)
  let initialize (initial : account_spec list) : unit =
  initial
    |> List.iter (fun {name; id; balance}
                  -> DB.create id name;
                  DB.update id balance) ;;

  let rec acquire_id () : id =
    printf "Enter customer id: ";
    try
      let id = read_int () in
      ignore (DB.exists id); id
    with
    | Not_found
    | Failure _ -> printf "Invalid id \n";
                  acquire_id () ;;

  let rec acquire_amount () : int =
    printf "Enter amount: ";
    try
      let amount = read_int () in
      if amount <= 0 then raise (Failure "amount is non-positive");
      amount
    with
    | Failure _ -> printf "Invalid amount \n";
                   acquire_amount () ;;

    let rec acquire_act () : action =
      printf "Enter action: (B) Balance (-) Withdraw (+) Deposit \
              (=) Done (X) Exit: %!";
      scanf " %c"
            (fun char -> match char with
                        | 'b' | 'B'        -> Balance
                        | '/' | 'x' | 'X'  -> Finished
                        | '='              -> Next
                        | 'w' | 'W' | '-'  -> Withdraw (acquire_amount ())
                        | 'd' | 'D' | '+'  -> Deposit (acquire_amount ())
                        | _                -> printf "  invalid choice\n";
                                              acquire_act () ) ;;

    let get_balance : id -> int = DB.balance ;;

    let get_name : id -> string = DB.name ;;

    let update_balance : id -> int -> unit = DB.update ;;

    let present_message (msg : string) : unit =
      printf "%s\n%!" msg ;;

    let deliver_cash (amount : int) : unit =
      printf "Here's your cash: ";
      (* dispense some "20's" *)
      for _i = 1 to (amount / 20) do
        printf "[20 @ 20]"
      done;
      (* dispense the rest of the cash *)
      printf " and %d more\n" (amount mod 20) ;;


(*
Attempt from lab
type id = int

type action =
  | Balance           (* balance inquiry *)
  | Withdraw of int   (* withdraw an amount *)
  | Deposit of int    (* deposit an amount *)
  | Next              (* finish this customer and move on to the next one *)
  | Finished          (* shut down the ATM and exit entirely *)
;;


type account_spec = {name : string; id : id; balance : int} ;;

(* initialize accts -- Establishes a database of accounts, each with a
   name, aribtrary id, and balance. The names and balances are
   initialized as per the `accts` provided. *)
let data = ref []

let initailize (alst: account_spec list) : unit =
  List.iter (fun x -> data := [x] @ !data) alst ;;

(* acquire_id () -- Requests from the ATM customer and returns an id
   (akin to entering one's ATM card), by prompting for an id number
   and reading an id from stdin. *)
let acquire_id unit : id =
  print_string ("Enter Customer ID: ");
  read_int () ;;

let acquire_amount unit : int =
  print_string ("Enter Amount: ");
  read_int () ;;

let acquire_act unit : action =
  print_string ("Enter Action, (B) Balance (-) Withdraw (+) Deposit (=) Done (X) Exit:");
  match read_line () with
  | "B" -> Balance
  | "-" -> Withdraw (acquire_amount ())
  | "+" -> Deposit (acquire_amount ())
  | "=" -> Next
  | "X" -> Finished
  | _ -> raise (Invalid_argument ("Must enter an Action")) ;;

let get_balance (i: id) : int =
  (List.find (fun x -> x.id = i) !data).balance ;;

let get_name (i: id) : string =
  (List.find (fun x -> x.id = i) !data).name ;;

let update_balance (i: id) (n: int) : unit =
  let (el, lst) = List.partition (fun x -> x.id = i) !data in
  let e = List.hd el in
  if el = [] then raise Not_found
  else data := lst @ [{name = e.name; id = e.id; balance = n}] ;;

let present_message (s: string) : unit =
  print_string s;
  print_newline () ;;

let deliver_cash (n : int) : unit =
  print_string ("Here's your cash: ");
  for i = 0 to n / 20  do
    print_string ("[20 @ 20]")
  done;
  Printf.printf ("and %i more") (n - n/20) ;; *)
