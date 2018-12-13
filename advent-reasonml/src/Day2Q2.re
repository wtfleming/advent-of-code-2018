let differsByOne = (l: string, r: string) => {
  let left = Js.String.split("", l);
  let right = Js.String.split("", r);

  let numLettersInCommon =
    Belt.Array.reduceWithIndex(left, 0, (acc, _x, idx) =>
      if (left[idx] == right[idx]) {
        acc + 1;
      } else {
        acc;
      }
    );

  switch (Belt.Array.length(left) - numLettersInCommon) {
  | 1 => true
  | _ => false
  };
};

let doFooFindCorrectIds = (x: string, l: list(string)) => {
  let result =
    Belt.List.keep(
      l,
      a => {
        ();
        if (Js.String.length(a) > 0) {
          differsByOne(x, a);
        } else {
          false;
        };
      },
    );

  switch (Belt.List.size(result)) {
  | 1 => (Some(x), Belt.List.head(result))
  | _ => (None, None)
  };
};

let rec doFindCorrectIds = (l: list(string), acc) =>
  switch (l) {
  | [] => acc
  | [h, ...t] =>
    if (Belt.List.length(t) > 0) {
      switch (doFooFindCorrectIds(h, t)) {
      | (Some(left), Some(right)) => (left, right)
      | _ => doFindCorrectIds(t, acc)
      };
    } else {
      acc;
    }
  };

let findCorrectIds = l => doFindCorrectIds(Array.to_list(l), ("", ""));

let result =
  Node.Fs.readFileAsUtf8Sync("./src/day-2-input.txt")
  |> Js.String.split("\n")
  |> findCorrectIds;

Js.log(result);
