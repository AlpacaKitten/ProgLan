(*Aufgabe 10
Man implementiere folgende h¨ohere Funktionen in SML. Man gebe ferner f¨ur jede der
Funktionen den allgemeinsten (polymorphen) Typ an und begr¨unde die Antwort genau.


a) twice f wendet die Funktion f zweimal auf ihr Argument an.*)
fun twice f x = f (f x);

(*
f wird auf x angewendet und muss als Rückgabewert wieder den Selben Typ wie x haben, damit die Ausgabe von f eine gültige Eingabe für f ist:
f: 'a -> 'a. Daraus folgt, dass x auch vom Typ 'a ist und die Ausgabe von f(f x) auch 'a ist. Für twice ergibt sich also folgendes:

twice: ('a -> 'a) -> 'a -> 'a

*)

(*b) compose f g wendet zuerst die Funktion f und dann die Funktion g auf ihr Argu-
ment an.*)

fun compose f g x = g (f x);


(* Compose wendet erst f auf das Argument x an und verwendet das Ergebnis dann als Eingabe für g. Der Ausgabetyp von f muss also der Eingabetyp von g sein:
f: 'a -> 'b  mit x vom Typ 'a
g: 'b -> 'c
Das Gesamtergebnis von compose hat also den Typ 'c:
compose: ('b -> 'c) -> ('a -> 'b) -> 'a -> 'b


*)

(*c) zipWith f l m wendet die zweistellige Funktion f auf die jeweils korrespondieren-
den Elemente der Listen l und m passenden Typs an und liefert die Liste der Re-
sultate. Das ¨uberz¨ahlige Suffix der l¨angeren Argumentliste soll, sofern vorhanden,
verworfen werden.*)

fun zipWith f (x::l) (y::m) = f x y :: zipWith f l m
  | zipWith f _ _ = [];

(*f wird auf das jeweilige Element der Listen m und l angewendet und in einer neuen Liste gespeichert. Da die in m und l gespeicherten Elemente nicht weiter spezifiziert sind und unterschiedliche
Typen haben können, und die Elemente der neuen Liste auch mit keiner dieser Typen übereinstimmen muss gilt 
zipWidth:  ('a → 'b → 'c) → 'a list → 'b list → 'c list

 *)



(*d) scan f e l liefert die Liste, die herauskommt, wenn man die Funktion foldl f e
auf alle nichtleeren Pr¨afixe der Liste l anwenden w¨urde.
Beispiel: scan (op+) 42 [1,2,3,4] = [43,45,48,52]
foldl braucht bei dieser Teilaufgabe nicht verwendet werden*)

fun scan f e [] = []
  | scan f e (x::l) = f (e, x) :: scan f (f (e, x)) l;


(* f bekommt als Eingabe  zwei Typen a' und b'. Weil wir f rekursiv aufrufen, muss der Rückgabetyp a' sein. Die funktion scan bekommt einen Wert vom Typen a' und eine Liste mit Elementen vom Typen b'.
Als Rückgabe erwarten wir eine Liste vom Typ a', da wir die Zwischenwerte der Anwendung f auf die Liste gespeichert angezwigt bekommen wollen:


scan: ('a * 'b → 'a) → 'a → 'b list → 'a list

*)