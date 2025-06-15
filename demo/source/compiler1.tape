# Where should we write the GIF?
Output ./demo/output/let_statement.gif

Set FontSize 25
Set Width 1600
Set Height 900

Type "bat -P .\crust\samples\compiler_samples\let_statement.crust"
Enter
Sleep 5s

Type "cargo run --quiet -- run .\crust\samples\compiler_samples\let_statement.crust"
Enter
Sleep 10s

