use std::io;

fn main() {
	let mut temp = String::new();
	println!("Input Fahrenheit temperature:");
	io::stdin()
		.read_line(&mut temp)
		.expect("Could not read line.");
	let mut temp: i32 = temp.trim().parse().expect("NaN");
	temp = 5 * (temp - 32) / 9;
	println!("{temp}°C");
}
