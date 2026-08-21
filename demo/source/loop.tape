Output ./demo/output/loop.gif

Set FontSize 25
Set Width 1600
Set Height 900
Set TypingSpeed 35ms

Type "bat -P crates/crust/samples/compiler/loop.crust"
Enter
Sleep 2s
Type "cargo run --quiet -- run crates/crust/samples/compiler/loop.crust"
Enter
Sleep 5s
