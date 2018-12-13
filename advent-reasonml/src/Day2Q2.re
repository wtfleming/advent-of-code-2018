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

let findCorrectIds = (x: string, l: list(string)) => {
  let result =
    Belt.List.keep(l, a =>
      if (Js.String.length(a) > 0) {
        differsByOne(x, a);
      } else {
        false;
      }
    );

  switch (Belt.List.size(result)) {
  | 1 => (Some(x), Belt.List.head(result))
  | _ => (None, None)
  };
};

let rec doFindCorrectIds = (acc, l: list(string)) =>
  switch (l) {
  | [] => acc
  | [h, ...t] =>
    if (Belt.List.length(t) > 0) {
      switch (findCorrectIds(h, t)) {
      | (Some(left), Some(right)) => (left, right)
      | _ => doFindCorrectIds(acc, t)
      };
    } else {
      acc;
    }
  };

let result =
  Node.Fs.readFileAsUtf8Sync("./src/day-2-input.txt")
  |> Js.String.split("\n")
  |> Array.to_list
  |> doFindCorrectIds(("", ""));

Js.log(result);
