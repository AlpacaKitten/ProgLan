(*5a Die Funktion remAll(a,l) entfernt alle Vorkommen eines Elements a aus einer
Liste l passenden Typs und liefert die verbleibende Liste als Resultat.*)

fun remAll(a,nil)= nil
    remAll(a,x::l)= if a=x then remAll(a,l)
                    else x::remAll(a,l);




(*b Die Funktion ith(l,i) liefert das i-te Element einer Liste l der Mindestl¨ange i+1,
wobei die Elemente einer Liste beginnend mit null gez¨ahlt werden.*)

fun ith(nil,i) = nil
    ith(x::l,0) = x
    ith(x::l,i) = ith(l,i-1);

(*c Die Funktion isScatteredSublist pr¨uft, ob eine Liste als verstreute Teilliste einer
anderen Liste gleichen Typs auftritt. Eine Liste al heißt verstreute Teilliste einer
Liste bl, wenn die Elemente von al in der Reihenfolge und Vielfachheit ihres Auf-
tretens in bl vorkommen. Verstreute Teillisten der Liste [3,3,1,5,3,2] sind zum
Beispiel die Listen [3,1,5], [3,3], [3,1,2] und [2]; keine verstreuten Teillisten
sind [3,5,6], [2,1] und [1,1].*)

fun isScatteredSublist(nil, bl) = true
    isScatteredSublist(al, nil) = false
    isScatteredSublist(x::al,y::bl) = if x=y then isScatteredSublist(al,bl)
                                        else isScatteredSublist(x::al, bl);



(*d Die Funktion unique entfernt alle Duplikate aus einer Liste*)
fun unique(nil)=nil
    unique(x::l)=x::unique(remAll(x,l));




