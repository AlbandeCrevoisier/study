use std::io;

fn main() {
	let mut n = String::new();
	println!("Input a number > 1:");
	io::stdin()
		.read_line(&mut n)
		.expect("Could not read line.");
	let n: i32 = n.trim().parse().expect("NaN");
	if n < 2 {
		println!("dumb idiot");
	}
	let mut u_2 = 0;
	let mut u_1 = 1;
	for _ in (2..n) {
		let u_n = u_2 + u_1;
		u_2 = u_1;
		u_1 = u_n;
	}
	println!("{}", u_2 + u_1);
}
