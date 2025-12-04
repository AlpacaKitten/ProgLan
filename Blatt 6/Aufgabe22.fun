(*Man entwerfe h¨ohere Funktionen auf B¨aumen in SML. Dazu sei folgende Datenstruktur
f¨ur B¨aume gegeben:
datatype ’a tree = Node of ’a * ’a tree list
a) treemap f wendet eine Funktion f: ’a -> ’b auf alle Knoten eines Baums an und
l¨asst die Struktur unver¨andert.
b) treefold f faltet einen Baum mit der Funktion f, indem die Funktion f ¨uber alle
Teilb¨aume jedes Knotens und den Knoteninhalt selbst angewendet wird.
Mit Hilfe dieser h¨oheren Funktionen auf B¨aumen realisiere man in SML folgende
Funktionen. Zum Testen kann die Anzeigetiefe rekursiver Datenstrukturen in SML
mit der Eingabe
Control.Print.printDepth := 10
ver¨andert werden, hier zum Beispiel auf 10 erh¨oht werden.
c) Die Funktion abstree ¨andert alle Zahlen in einem Baum vom Type int tree in
ihren Absolutbetrag um.
d) Die Funktion sumtree bestimmt die Summe aller Knotenmarkierungen in einem
Baum vom Typ int tree.
e) Die Funktion countNodes bestimmt die Anzahl aller Knoten in einem Baum vom
Typ ’a tree.*)

datatype 'a tree = Node of 'a * 'a tree list;

fun treemap f (Node (n, tl)) = Node (f n, map (treemap f) tl);

fun treefold f (Node (n, tl)) =  f (n, map (treefold f) tl);

fun abstree (t: int tree) = treemap(fn x => if x>0 then x else ~x)t;

fun sumtree t = treefold (fn (n, cs) => n + foldl op+ 0 cs) t;

fun countNodes t = treefold (fn (n, cs) => 1 + foldl op+ 0 cs) t;

val t =
  Node (3,
    [ Node (~2, []),
      Node (5, [
        Node (1, []),
        Node (4, [])
      ])
    ]);

sumtree t;
countNodes t;
abstree t;