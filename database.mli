
(* Customer account identifiers *)
type id = int ;;

(* create id name -- Creates a new account in the database with the
  given name and zero balance (which can be updated using the update
  function). Returns the new account's id. *)
val create : id -> string -> unit ;;

(* exists id -- Returns true if there is an account with the given
  id. *)
val exists : id -> bool ;;

(* balance id -- Returns the balance associated with the given account
   id. Raises Not_found if no account with that name is in the
   database. *)
val balance : id -> int ;;

(* name id -- Returns the name associated with the given account
  id. Raises Not_found if no account with that name is in the
  database. *)
val name : id -> string ;;

(* update id value -- Updates the balance associated with the given id
to the given balance. Raises Not_found if there is no account with
the given id. *)
val update : id -> int -> unit ;;

(* close id -- Removes the account associated with the given
id. Raises Not_found if there is no account with the given id. *)
val close : id -> unit ;;

(* dump () -- Prints to stdout a representation of the
database. Useful for debugging. *)
val dump : unit -> unit ;;
