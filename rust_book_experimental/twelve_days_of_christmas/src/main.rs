fn main() {
	let gifts = [
		"And a partridge in a pear tree",
		"Two turtle doves",
		"Three French hens",
		"Four calling birds",
		"Five gold rings",
		"Six geese a-laying",
		"Seven swans a-swimming",
		"Eight maids a-milking",
		"Nine ladies dancing",
		"Ten lords a-leaping",
		"Eleven pipers piping",
		"Twelve drummers drumming"
	];
	let days = [
		"first",
		"second",
		"third",
		"fourth",
		"fifth",
		"sixth",
		"seventh",
		"eighth",
		"ninth",
		"tenth",
		"eleventh",
		"twelfth"
	];
	for i in 0..12 {
		println!("\nOn the {} day of Christmas my true love sent me", days[i]);
		if i == 0 {
			println!("A partridge in a pear tree");
			continue
		}
		for j in (0..=i).rev() {
			println!("{}", gifts[j]);
		}
	}
}
