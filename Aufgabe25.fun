(* bound e bestimmt die Menge der gebundenen Bezeichner im Ausdruck e *)
fun bound(Id id)        = nil
  | bound(Const _)      = nil
  | bound(Funsymb _)    = nil
  | bound(Tupel el)     = foldr union nil (map bound el)
  | bound(Sel(i,e))     = bound(e)
  | bound(Cond(b,t,e))  = union(bound(b), union(bound(t),bound(e)))
  | bound(App(f,a))     = union(bound(f), bound(a))
  | bound(Abstr(x,t,e)) = union(bound(e), [x])
  | bound(Fix(x,t,e))   = union(bound(e), [x])