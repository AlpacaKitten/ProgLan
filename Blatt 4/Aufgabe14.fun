(*Man verwende h¨ohere Funktionen auf Listen zur Realisierung der folgenden Funktionen
in SML.
a) Die Funktion add l1 l2 berechnet die elementweise Summe zweier Listen ganzer
Zahlen l1 und l2. Das Suffix der l¨angeren Liste soll, sofern vorhanden, verworfen
werden.
b) Die Funktion iselem e l pr¨uft, ob das Element e in der Liste l passenden Typs
vorkommt.*)

fun add l1 l2 = zipWidth (op+)(l1,l2);
fun iselem e l = foldl (fn (x, acc) => (x = e) orelse acc) false l;