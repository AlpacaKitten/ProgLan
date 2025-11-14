(*Aufgabe 13*)
datatype threeValue = true | false | unknown;

datatype term = Const of threeValue
            | Neg of term
            | Conj of term * term
            | Disj of term * term;