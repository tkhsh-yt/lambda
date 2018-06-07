let counter = ref 1
let genid s =
  incr counter;
  (s, !counter)
