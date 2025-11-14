(*Aufgabe 9
Gegeben sei folgende SML-Funktion zum Revertieren von Listen:
fun rev(nil) = nil
| rev(x::xs) = rev(xs) @ [x]
Man beweise durch strukturelle Induktion über den Aufbau von Listen die folgende Eigenschaft:
rev(L@ R) = rev(R) @ rev(L)*)

Wir beweisen die Eigenschaft rev(L@ R) = rev(R) @ rev(L) über eine strukturelle Induktion über L
Induktionsanfang:
Sei L = nil. Damit gilt für ein beliebiges R: rev(nil@R) = rev(R) und rev(R)@rev(nil) = rev(R)@nil = rev(R)

Induktionsvoraussetzung: 
Für ein beliebiges aber festes L gelte rev(L@R) = rev(R)@rev(L) für alle Listen R. 

Induktionsschluss: 
Wir zeigen nun, dass die Eigenschaft auch für L' = x::L gilt.
rev(L' @ R) = rev(x::L@R) = rev(x::(L@R))
            = rev(L@R) @ [x]                (*Def von rev()*)
            = rev(R)@rev(L)@[x]             (*Induktionsvoraussetzung*)
            = rev(R)@rev(x::L)              (*Def von rev()*)
            = rev(R)@rev(L')
            
Mit vollständiger Induktion folgt nun das für beliebige Listen L und R gilt: rev(L@ R) = rev(R) @ rev(L)