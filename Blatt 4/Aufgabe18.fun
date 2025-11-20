(*Auf den folgenden ¨Ubungsbl¨attern soll in der Sprache SML ein Interpreter f¨ur die funk-
tionale Sprache aus der Vorlesung entwickelt werden. Als Grundlage daf¨ur definiere man
in dieser Aufgabe die Datenstruktur der Ausdr¨ucke.

a) Man definiere zuerst einen Datentyp etype f¨ur die Typen von Ausdr¨ucken so, dass
die Definition des Datentyps dem Aufbau von Typen T gem¨aß Kap. 3.1 der Vorle-
sung entspricht. Als Grundtypen ber¨ucksichtige man ganze Zahlen und Wahrheits-
werte.*)

datatype etype = intType of int
    | boolType of bool
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

datatype expr = 
    (*Bezeichner*)
    Bez of string

    (*Konstanten*)
    | IntConst of int
    | BoolConst of bool
    
    (*arithmetische Funktionssymbole*)
    | Add of expr * expr 
    | Sub of expr * expr
    | Mult of expr * expr
    | Div of expr * expr
    | Mod of expr * expr
    | Neg of expr 

    (*logische Funktionssymbole*)
    | Not of expr 
    | Equal of expr * expr
    | NotEq of expr * expr
    | Lower of expr * expr
    | Greater of expr * expr
    | LowEq of expr * expr
    | GreEq of expr * expr

    (*Tupel*)
    | Tupel of expr list

    (*Selektion*)
    | Sel of int * expr

    (*Konditionale*)
    | If of expr * expr * expr

    (*Appplikation*)
    | Appl of expr * expr

    (*Abstraktion*)
    | Abstr of string * etype * expr

    (*Rekursion*)
    | Rec of string * etype * expr


(*d) Man schreibe eine Funktion toString, die einen Ausdruck vom Typ expr in einen
String umwandelt gem¨aß der funktionalen Kernsprache EXP R aus Kap. 3.1 der
Vorlesung. Dabei kann die SML-Funktion Int.toString zur Umwandlung einer
ganzen Zahl in einen String n¨utzlich sein.
Der Ausdruck muss (noch) nicht typkorrekt sein und soll (noch) nicht ausgewertet werden!*)

fun toString (Bez x) = x

    | toString (IntConst i) = Int.toString i
    | toString (BoolConst b) = if b then "true" else "false"

    | toString (Add (e1, e2)) = "(" ^ toString e1 ^ " + " ^ toString e2 ^ ")"
    | toString (Sub (e1, e2)) = "(" ^ toString e1 ^ " - " ^ toString e2 ^ ")"
    | toString (Mult (e1, e2)) = "(" ^ toString e1 ^ " * " ^ toString e2 ^ ")"
    | toString (Div (e1, e2)) = "(" ^ toString e1 ^ " / " ^ toString e2 ^ ")"
    | toString (Mod (e1, e2)) = "(" ^ toString e1 ^ " mod " ^ toString e2 ^ ")"
    | toString (Neg t) = "(-" ^ toString t ^ ")"

    | toString (Not t) = "(not " ^ toString t ^ ")"
    | toString (Equal (e1, e2)) = "(" ^ toString e1 ^ " = " ^ toString e2 ^ ")"
    | toString (NotEq (e1, e2)) = "(" ^ toString e1 ^ " <> " ^ toString e2 ^ ")"
    | toString (Lower (e1, e2)) = "(" ^ toString e1 ^ " < " ^ toString e2 ^ ")"
    | toString (Greater (e1, e2)) = "(" ^ toString e1 ^ " > " ^ toString e2 ^ ")"
    | toString (LowEq (e1, e2)) = "(" ^ toString e1 ^ " <= " ^ toString e2 ^ ")"
    | toString (GreEq (e1, e2)) = "(" ^ toString e1 ^ " >= " ^ toString e2 ^ ")"

    | toString (Tupel ts) = "(" ^ String.concatWith ", " (map toString ts) ^ ")"
    
    | toString (Sel (i, e)) = "(π_" ^ Int.toString i ^ toString e ^ ")"

    | toString (If (e1, e2, t3)) = "(if " ^ toString e1 ^ " then " ^ toString e2 ^ " else " ^ toString t3 ^ ")"

    | toString (Appl (e1, e2)) = "(" ^ toString e1 ^ " " ^ toString e2 ^ ")"

    | toString (Abstr (x, t, e)) = "(fn " ^ x ^ " : " ^ toStringType t ^ " => " ^ toString e ^ ")"

    | toString (Rec (x, t, e)) = "(fix " ^ x ^ " : " ^ toStringType t ^ " => " ^ toString e ^ ")"