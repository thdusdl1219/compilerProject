fun main (x:int):int =
	let sum = ref 0 in
	let a = ref 10 in
	while !a do
		(let v = !a in
		a := v-1;
		let sumv = !sum 
		in
			sum := sumv+v
		);
		!sum