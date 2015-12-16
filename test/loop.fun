fun main (x:int):int =
	let sum = ref 0 in
	let a = ref 10 in
	let i = 3 in
	let j = 2 in
	while !a do
		(let v = !a in
		a := v-1;
		let sumv = !sum in
		let const = 3 in
		let addition = i + j
		in
			if !a < const
			then
				sum := sumv+v+i
			else
				sum := sumv+addition
		);
		!sum