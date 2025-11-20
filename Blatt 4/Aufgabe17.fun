(*Das n-te Element der Fibonaccifolge kann mit folgender Formel berechnet werden:
fib(n) =
{ 1 wenn n ≤ 1
fib(n − 1) + fib(n − 2) sonst
Man entwerfe einen Ausdruck in der funktionalen Kernsprache EXP R gem¨aß der Syntax
aus der Vorlesung (Kap. 3.1), der das n-te Element der Fibonaccifolge berechnet. Da-
bei gebe man f¨ur die verwendeten Bezeichner sowie Konstanten- und Funktionssymbole
jeweils die Signatur an.

seien $1 \in C^{nat}, pred \in F^{(nat\rightarrow nat)}, add\in F^{((nat\times nat)\rightarrow nat)}, iszero \in F^{(nat\rightarrow bool)}$ und $fib \in V^{(nat\rightarrow nat)} 
(fix fib: nat -> nat =>(fn n: nat=>(if (iszero(pred n)) then 1 else (if (iszero(pred(pred n)) then 1 else (add(fib(pred n),(pred(pred(n))))))))))

*)