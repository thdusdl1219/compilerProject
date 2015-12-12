fun main(argc:int):int = 
(let	i=ref 2 in
  let j=ref 1 in
 	while (!i)<10 do(
		j := 1;
		(
			while (!j)<10 do(
				printint (!i*!j);
				j := !j +1
			)
		);
	  i := !i + 1
	)
);0
