(*
                          CS 51 Problem Set
                 Simulation of an Infectious Disease

                     Increment/Decrement Counters
 *)

class type counter_type =
  object
    (* set n -- Sets the running count to `n`. *)
    method set : int -> unit
    (* reset -- Resets the running count to zero. *)
    method reset : unit
    (* bump -- Increments the count. *)
    method bump : unit
    (* debump -- Decrements the count. *)
    method debump : unit
    (* count -- Returns the current count, initially zero. *)
    method count : int
  end ;;

(*....................................................................
Place your implementation of the `counter` class of class type
`counter_type` here.
....................................................................*)

class counter : counter_type = 
  object
    val mutable count = 0

    method set (n : int) : unit = 
      count <- n
    
    method reset : unit =
      count <- 0

    method bump : unit =
      count <- count + 1
    
    method debump : unit = 
      count <- max 0 (count - 1)
    
    method count : int =
      count
  end ;;

(* Now you should be able to test the partially implemented simulation
with

      % ocamlbuild run.byte
      % ./run.byte

Once that is working, you should extend the simulation in the file
`people.ml`.  

......................................................................
When you've completed the assignment and made a video demonstrating
your simulation, define the function `recording_url` below to return
the URL where the recording can be found.
....................................................................*)

let recording_url () : string = "https://youtu.be/8nfqf-Kq7Ww" ;;
   
(*====================================================================
Reflection on the problem set

After each problem set, we'll ask you to reflect on your experience.
We care about your responses and will use them to help guide us in
creating and improving future assignments.

......................................................................
Please give us an honest (if approximate) estimate of how long (in
minutes) this problem set (in total, not just this file) took you to
complete.
....................................................................*)

let minutes_spent_on_pset () : int =
  200 ;;

(*....................................................................
It's worth reflecting on the work you did on this problem set, where
you ran into problems and how you ended up resolving them. What might
you have done in retrospect that would have allowed you to generate as
good a submission in less time? Please provide us your thoughts in the
string below.
....................................................................*)

let reflection () : string =
  "This PSet was much easier than the rest, and was a great synthesis of the" ^
  "course material this year. I had fun playing around with the simulation once" ^
  "it was implemented. Overall, fun and easy PSet." ;;
