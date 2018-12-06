Node.Fs.readFileAsUtf8Sync("./src/day-1-input.txt")
|> Js.String.split("\n")
|> Array.map(int_of_string)
|> Array.fold_left((+), 0)
|> Js.log;
