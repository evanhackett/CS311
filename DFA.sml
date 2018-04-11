(* DFA string checker. Construct a DFA then check if a given string is in the language defined by the DFA. *)

(* states will be constructed with an int to distinguish them from each other *)
datatype state = State of int
       | DeadState

(* A DFA is defined as a 5-tuple *)
(* (set of states, alphabet, transition function, start state, set of accept states) *)
type DFA = state list * char list * state -> char -> state * state * state list


fun isAcceptState states x =
  isSome (List.find (fn (st) => case st of State y => y = x
                                         | _ => false) states)

fun check (states, alphabet, transition_func, start, accept_states) s =
  let
      val chars = String.explode s
      (* trans is transisition_func with reordered arguments to work with fold *)
      val trans = fn (ch, st) => transition_func st ch
  in
      case List.foldl trans start chars of
          DeadState => false
        | State x => isAcceptState accept_states x
  end


(* example DFA. This DFA only accepts strings that start with "a". Alphabet is: {a, b}  *)
val starts_with_a =
    let
        val states = [State 0, State 1]
        val alphabet = [#"a", #"b"]
        val start = State 0
        val accept_states = [State 1]
        fun transition_func st ch =
          case (st, ch) of
              (State 0, #"a") => State 1
            | (State 1, #"a") => State 1
            | (State 1, #"b") => State 1
            | (_, _) => DeadState
    in
        check (states, alphabet, transition_func, start, accept_states)
    end

(* Another Example. This DFA accepts binary strings that are divisible by 3 *)
val divisible_by_3 =
    let
        val states = [State 0, State 1, State 2]
        val alphabet = [#"0", #"1"]
        val start = State 0
        val accept_states = [State 0]
        fun transition_func st ch =
          case (st, ch) of
              (State 0, #"0") => State 0
            | (State 0, #"1") => State 1
            | (State 1, #"0") => State 2
            | (State 1, #"1") => State 0
            | (State 2, #"0") => State 1
            | (State 2, #"1") => State 2
            | (_, _) => DeadState
    in
        check (states, alphabet, transition_func, start, accept_states)
    end
