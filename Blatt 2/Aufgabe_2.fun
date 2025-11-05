(*Idee: nehme Liste(oder String), 
führe für den String eine Suche nach a,b,... durch, falls nicht null hänge String (#"zeichen", Anzahl) an Liste l an 

alternativ: fange beim ersten Zeichen an, zähle und remove (unique Aufgabe 1), Dann sortiere.

 compare (s, t)
    does a lexicographic comparison of the two strings using the ordering Char.compare on the characters. It returns LESS, EQUAL, or GREATER, if s is less than, equal to, or greater than t, respectively. 
    *)

    (* htab : string -> (char * int) list
   Erzeugt aus einer Zeichenfolge eine Häufigkeitstabelle der vorkommenden Zeichen,
   aufsteigend nach Zeichen sortiert, ohne Verwendung von let.
*)

(* htab : string -> (char * int) list
   Erstellt eine Häufigkeitstabelle der vorkommenden Zeichen, sortiert nach Zeichen.
   Keine Verwendung von let.
*)

(* fügt ein Zeichen in eine Häufigkeitstabelle ein *)
fun insert (c, []) = [(c,1)]
  | insert (c, (x,n)::xs) =
      if c = x then (x, n+1)::xs
      else (x,n)::insert(c, xs)

(* baut eine unsortierte Häufigkeitstabelle auf *)
fun countChars [] acc = acc
  | countChars (c::cs) acc = countChars cs (insert(c, acc))

(* sortiert die Häufigkeitstabelle nach Zeichen *)
fun sortTab [] = []
  | sortTab ((c,n)::xs) =
      sortTab (List.filter (fn (d,_) => Char.<(d, c)) xs)
      @ [(c,n)]
      @ sortTab (List.filter (fn (d,_) => Char.>(d, c)) xs)

(* Hauptfunktion *)
fun htab s = sortTab (countChars (String.explode s) [])

(* Beispiele *)
val test1 = htab "";             (* [] *)
val test2 = htab "abba";         (* [(#"a",2),(#"b",2)] *)
val test3 = htab "banane";       (* [(#"a",2),(#"b",1),(#"e",1),(#"n",2)] *)
