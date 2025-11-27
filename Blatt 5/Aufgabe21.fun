exception TypeError of string;

(*Hilfsfunktion zum Prüfen, ob Bezeichner in env und Rückgabe von Typ oder Fehler, wenn Bezeichner unbekannt*)
fun lookup nil x = raise TypeError ("Unbekannter Bezeichner: " ^ x)
  | lookup ((y,t)::env) x = if x = y then t else lookup env x;


(*Hilfsfunktion zum Zuweisen von Typen für Operationssymbole*)
fun typeOfOper Add = FunctionType(TupelType([IntType, IntType]), IntType)
  | typeOfOper Sub = FunctionType(TupelType([IntType, IntType]), IntType)
  | typeOfOper Mul = FunctionType(TupelType([IntType, IntType]), IntType)
  | typeOfOper Div = FunctionType(TupelType([IntType, IntType]), IntType)
  | typeOfOper Mod = FunctionType(TupelType([IntType, IntType]), IntType)

  | typeOfOper Eq = FunctionType(TupelType([IntType, IntType]), BoolType)
  | typeOfOper Neq = FunctionType(TupelType([IntType, IntType]), BoolType)
  | typeOfOper Lt = FunctionType(TupelType([IntType, IntType]), BoolType)
  | typeOfOper Gt = FunctionType(TupelType([IntType, IntType]), BoolType)
  | typeOfOper Ge = FunctionType(TupelType([IntType, IntType]), BoolType)
  | typeOfOper Le = FunctionType(TupelType([IntType, IntType]), BoolType)

  | typeOfOper Not = FunctionType(BoolType, BoolType)
  | typeOfOper Neg = FunctionType(IntType, IntType);


fun typecheck env (Id i) = lookup env i 
  | typecheck env (Const(IntConst c)) = IntType 
  | typecheck env (Const(BoolConst c)) = BoolType 

(*Funktionssymbole*)
  | typecheck env (Funsymb oper) = typeOfOper oper

(*Tupel*)
  | typecheck env (Tupel es) = TupelType (map (typecheck env) es)

(*Selektion*)
  | typecheck env (Sel(i, e)) =
      (case typecheck env e of
           TupelType ts =>
               if i < 1 then
                   raise TypeError "Selektion benötigt Index größer 0"
               else if i > length ts then
                   raise TypeError "Index darf bei Selektion nicht größer als Tupellänge sein"
               else
                   List.nth(ts, i - 1)
         | other =>
               raise TypeError "Selektion benötigt als Tupeltyp")

(*Konditional*)
  | typecheck env (Cond(e1,e2,e3)) =
    if typecheck env e1 <> BoolType then
        raise TypeError "Bedingung muss Typ bool haben"
    else if typecheck env e2 <> typecheck env e3 then
        raise TypeError "Beide Zweige müssen bei Konditional gleichen Typ haben"
    else
        typecheck env e2

(*Applikation*)
  | typecheck env (App(e1,e2)) =
      (case typecheck env e1 of
           FunctionType(argT, retT) =>
               if typecheck env e2 = argT then
                   retT
               else
                   raise TypeError "Argumenttyp ist falsch"
         | other =>
               raise TypeError "linke Seite von Applikation muss eine Funktion sein") 
(*Abstraktion*)
  | typecheck env (Abstr(x, t, e)) = 
    FunctionType(t, typecheck ((x,t)::env) e)

(*Fixpunktbildung*)
  | typecheck env (Fix (x, t, e)) =
      (case typecheck env (Abstr(x, t, e)) of
           FunctionType(t1, t2) =>
               if t1 = t2 then
                   t1
               else
                   raise TypeError "Fix benötigt Funktion vom Typ t→t"
         | other =>
               raise TypeError "Fix benötigt eine Funktion");
