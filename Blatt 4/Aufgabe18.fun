(*Auf den folgenden ¨Ubungsbl¨attern soll in der Sprache SML ein Interpreter f¨ur die funk-
tionale Sprache aus der Vorlesung entwickelt werden. Als Grundlage daf¨ur definiere man
in dieser Aufgabe die Datenstruktur der Ausdr¨ucke.

a) Man definiere zuerst einen Datentyp etype f¨ur die Typen von Ausdr¨ucken so, dass
die Definition des Datentyps dem Aufbau von Typen T gem¨aß Kap. 3.1 der Vorle-
sung entspricht. Als Grundtypen ber¨ucksichtige man ganze Zahlen und Wahrheits-
werte.*)

type intType = int;
type boolType = bool;
datatype etype = intType 
    | boolType
    | eTupel of etype list
    | eFun of etype * etype;

(*b) Man schreibe eine Funktion toStringType, die einen Ausdruck vom Typ etype in
einen String umwandelt gem¨aß der Definition von Typen T aus Kap. 3.1 der Vorle-
sung. Dabei kann die SML-Funktion String.concatWith f¨ur Tupeltypen n¨utzlich
sein.*)

fun toStringType intType = "int"
  | toStringType boolType = "bool"
  | toStringType (eTupel ts) =
        "(" ^ String.concatWith " * " (map toStringType ts) ^ ")"
  | toStringType (eFun (t1, t2)) =
        "(" ^ toStringType t1 ^ " -> " ^ toStringType t2 ^ ")";

(*c) Man definiere einen Datentyp expr f¨ur die abstrakten Syntaxb¨aume der Ausdr¨ucke
vom Typ EXP R. Die Definition des Datentyps soll m¨oglichst genau die Struktur
der Ausdr¨ucke aus Kap. 3.1 der Vorlesung widerspiegeln. Die Kontextbedingungen
zu Typen von Ausdr¨ucken brauchen noch nicht beachtet zu werden.
Als Funktionssymbole ber¨ucksichtige man die zweistelligen arithmetischen Opera-
tionen Addition, Substraktion, Multiplikation, ganzzahlige Division, Modulo, als
einstellige arithmetische Operation die Negation, als einstellige logische Operati-
on die Verneinung, sowie die Vergleichsoperationen gleich, ungleich, gr¨oßer, kleiner,
kleiner gleich und gr¨oßer gleich.*)

(*d) Man schreibe eine Funktion toString, die einen Ausdruck vom Typ expr in einen
String umwandelt gem¨aß der funktionalen Kernsprache EXP R aus Kap. 3.1 der
Vorlesung. Dabei kann die SML-Funktion Int.toString zur Umwandlung einer
ganzen Zahl in einen String n¨utzlich sein.
Der Ausdruck muss (noch) nicht typkorrekt sein und soll (noch) nicht ausgewertet werden!*)