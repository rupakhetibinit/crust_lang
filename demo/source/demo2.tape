# Where should we write the GIF?
Output ./demo/output/demo2.gif

Set FontSize 25
Set Width 1600
Set Height 900

Type "bat -P .\interpreter\factorial.crust"
Enter
Sleep 5s

Type "cargo run --quiet .\interpreter\factorial.crust"
Enter
Sleep 5s
