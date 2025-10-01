fn main() {
	println!("Hello, world!");
	foo(42, '.');
}

fn foo(x: i32, y: char) -> i32 {
	println!("foo {x}{y}");
	42
}
