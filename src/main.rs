use crate::lang::{parsing::parse_string, lexing::Code, lexing::CodeOrigin::Interactive};

mod lang;

fn main() {
    println!("parsed: {:?}", parse_string(&Code { origin: Interactive, text: "record Point(x: int, y: int)" }))
}
