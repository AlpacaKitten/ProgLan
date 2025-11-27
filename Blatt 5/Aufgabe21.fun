fun typecheck env i = IntType 
  | typecheck env const = IntType 

(*TUPEL*)
  |typecheck env (Tupel es) = TupelType (map (typecheck env) es)

(*KONDITIONAL*)
  | typecheck env (Cond(e1,e2,e3)) =
    if typecheck env e1 <> BoolType then
        raise TypeError "Bedingung muss Typ bool haben"
    else if typecheck env e2 <> typecheck env e3 then
        raise TypeError "Beide Zweige müssen gleichen Typ haben"
    else
        typecheck env e2