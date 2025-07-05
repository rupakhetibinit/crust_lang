# Where should we write the GIF?
Output ./demo/output/disassemble.gif

Set FontSize 25
Set Width 1600
Set Height 900

Type "bat -P .\crust\samples\compiler\vm_test.crust"
Enter
Sleep 5s

Type "cargo run --quiet -- disassemble .\crust\samples\compiler\vm_test.crust"
Enter
Sleep 10s

