(*Aufgabe 20
a) siehe Overleaf
b)*)
type pnat = int;

datatype Mobile = mkWeight of pnat
    | MkRod of Mobile * pnat * Mobile * pnar

fun weight (mkWeight) = mkWeight
    | weight (mkRod(left, _, right _)) = weight(left) + weight(right)

fun isBalanced (mkWeight) = true
    | isBalanced (mkRod(left, leLen, right, riLen)) = 
        leLen * weight(left) = reLen * weight(right)
        andalso isBalanced(left)
        andalso isBalanced(right)