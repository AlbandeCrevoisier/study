fn main() {
	const HOURS_IN_SECONDS: u32 = 60 * 60;
	println!("const: {HOURS_IN_SECONDS}");
	let x = 5;
	let x = x + 1;
	println!("x = {x}");
	{
		let x = x * 2;
		println!("x = {x}");
	}
	println!("x = {x}");
	let spaces = "   ";
	let spaces = spaces.len();
	println!("spaces: {spaces}");
}
