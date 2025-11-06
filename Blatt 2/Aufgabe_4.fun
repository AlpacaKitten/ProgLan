(*
a)
Sorten: a' (Mengen)
        a'' (Elemente von Mengen)

Konstanten:
        empty: a'

Operationen:
        addElement: a', a'' -> a'
        remElement: a', a'' -> a'
        isElementInSet: a', a'' -> boolean
        isEmptySet: a' -> boolean
        mergeSets: a', a' -> a'

*)
structure Set :> SET = struct
  type 'a set= 'a list
  val empty =[]
  fun addElement s v= 
        if (List.exists(fn x=> x = v) s) then s 
        else v :: s;
  fun remElement s v=List.filter(fn x => x<>v)s;
  fun isElementInSet s v = List.exists (fn x => x = v) s
  fun isEmptySet s =List.null s;
  fun mergeSets (a, b) =
        foldl (fn (x, acc) => addElement acc x) a b
end
