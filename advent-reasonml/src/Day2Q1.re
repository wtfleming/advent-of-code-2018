let letterFrequencies = str =>
  Js.String.split("", str)
  |> Js.Array.reduce(
       (acc, value) => {
         let count =
           switch (Js.Dict.get(acc, value)) {
           | Some(l) => l + 1
           | None => 1
           };

         Js.Dict.set(acc, value, count);
         acc;
       },
       Js.Dict.empty(),
     );

let hasTwoOrThree = (x: Js.Dict.t(int)): (bool, bool) =>
  Js.Dict.values(x)
  |> Js.Array.reduce(
       (acc, value) => {
         let (hasTwo, hasThree) = acc;
         switch (value) {
         | 2 => (true, hasThree)
         | 3 => (hasTwo, true)
         | _ => acc
         };
       },
       (false, false),
     );

let calculateChecksum = x => {
  let (totalNumTwos, totalNumThrees): (int, int) =
    Js.Array.reduce(
      (acc, value) => {
        let (numTwos, numThrees) = acc;
        switch (value) {
        | (false, false) => acc
        | (true, true) => (numTwos + 1, numThrees + 1)
        | (true, false) => (numTwos + 1, numThrees)
        | (false, true) => (numTwos, numThrees + 1)
        };
      },
      (0, 0),
      x,
    );
  totalNumTwos * totalNumThrees;
};

let result =
  Node.Fs.readFileAsUtf8Sync("./src/day-2-input.txt")
  |> Js.String.split("\n")
  |> Js.Array.map(letterFrequencies)
  |> Js.Array.map(hasTwoOrThree)
  |> calculateChecksum;

Js.log(result);
