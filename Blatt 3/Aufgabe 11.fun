fun curry f = fn x => fn y => f (x, y);

fun uncurry f = fn (x, y) => f x y;

