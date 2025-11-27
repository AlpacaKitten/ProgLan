(*Aufgabe 19
Lösung mit SML:
(* enumeration*)
datatype value = true3
               | false3
               | unknown3;

(* vier Alternativen, rek. Datenstruktur term
   Fall 1: Terminierung, Fall 3 und 4 mit direktem Produkt *)
datatype term = Const of value
              | Neg of term
              | Conj of term * term
              | Disj of term * term;*)

(*Java:*)
public enum Value {
    true3,
    false3,
    unknown3
}

public interface Term { 
}

public class Const extends Term {
    public Value val;
    public Const(Value val) {
        this.val = val;
    }
}

public class Neg extends Term {
    public term term;
    public Neg(Term term) {
        this.term = term;
    }
}

public class Conj extends Term {
    public Term left
    public Term right
    public Conj(Term left right) {
        this.left = left;
        this.right = right;
    }
}

public class Disj extends Term {
    public Term left
    public Term right
    public Conj(Term left right) {
        this.left = left;
        this.right = right;
    }
}

(*Python*)

from enum inport Enum
from abc import ABC

class Value(Enum):
    TRUE3 = "true"
    FALSE3 = "false"
    UNKNOWN3 = "unknown"

class Term(ABC):
    pass

class Const(Term):
    val: Value
    def __init__(self, val):
        self.val = val

class Neg(Term):
    term: Term
    def __init__(self, term):
        self.term = term

class Disj(Term):
    left: Term
    right: Term
    def __init__(self, left, right):
        self.left = left
        self.right = right

class Disj(Term):
    left: Term
    right: Term
    def __init__(self, left, right):
        self.left = left
        self.right = right

