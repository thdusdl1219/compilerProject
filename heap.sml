 structure Heap:HEAP = 
   struct 
     type loc = int
     structure Mem = IntHashTable
     type 'a heap = (loc ref * 'a Mem.hash_table)
       
     fun fresh_loc (next_loc,_) = 
       let val l = !next_loc 
       in next_loc := l + 1;l
       end

     (* raises Mem.Not_found if loc is not in heap *)
     fun get_loc (_,m) loc = Mem.lookup m loc
     fun set_loc (_,m) loc value = Mem.insert m (loc,value)
 
     fun empty() = (ref 0, Mem.mkTable(512,Fail "Bad Table")) 

     fun print_heap (heap as (next_loc,mem)) printalpha = 
       let 
         fun printloc (loc, v) = 
           (print "L";
            print (Int.toString loc);
            print ": ";
            printalpha v;
            print "  ")
           
         val limit = !next_loc

         fun loop(i:int) = 
           if i<limit then 
             (printloc(i, get_loc heap i); loop(i+1))
           else ()
             
       in
          if limit=0 then print "empty" else loop(0)
       end
end