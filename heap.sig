signature HEAP = 
sig

  type loc
  type 'a heap

  val fresh_loc: 'a heap -> loc
  val get_loc: 'a heap -> loc -> 'a
  val set_loc: 'a heap -> loc -> 'a -> unit
  val empty: unit -> 'a heap
  val print_heap: 'a heap -> ('a -> unit) -> unit
end