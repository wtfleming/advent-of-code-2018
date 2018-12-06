let data =
  Node.Fs.readFileAsUtf8Sync("./src/day-1-input.txt")
  |> Js.String.split("\n")
  |> Array.map(int_of_string)
  |> Array.to_list;

let rec findDuplicateFreq =
        (seenFreqs: Belt.Set.Int.t, currentFreq: int, freqs: list(int)) =>
  switch (freqs) {
  | [] => findDuplicateFreq(seenFreqs, currentFreq, data) /* start the cycle again at the beginning of the list */
  | [h, ...t] =>
    let nextFreq = currentFreq + h;
    if (Belt.Set.Int.has(seenFreqs, nextFreq) == true) {
      nextFreq;
    } else {
      findDuplicateFreq(Belt.Set.Int.add(seenFreqs, nextFreq), nextFreq, t);
    };
  };

findDuplicateFreq(Belt.Set.Int.fromArray([|0|]), 0, data)->Js.log;
