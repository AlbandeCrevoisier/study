fn main() {
	'outer_loop: loop {
		let x = loop {
			println!("forever!");
			break 42
		};
		println!("{x}");
		loop {
			break 'outer_loop
		}
	}
	while 42 == 1337 {
		println!("lol");
	}
	let a = [1, 2, 3];
	for el in a {
		println!("{el}");
	}
	for n in (1..3).rev() {
		println!("{n}");
	}
}
