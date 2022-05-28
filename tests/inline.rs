use leggevm::testing::interpret;
use leggevm::vm::Value::*;

#[test]
fn run() {
    let val = interpret!({
        exit(1);
    });
    assert_eq!(val, Int(1))
}
