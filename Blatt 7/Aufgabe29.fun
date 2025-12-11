(******************************)
(*           S E T            *)
(******************************)

val emptySet = nil;

fun isEmptySet(s) = null(s);

fun isElem( nil, e) = false
  | isElem(a::s, e) = a=e orelse isElem(s,e);

fun insert( nil, e) = [e]
  | insert(a::l, e) = if isElem(a::l,e) then    a::l
                                        else e::a::l;

fun union(  nil, bl) = bl
  | union(a::al, bl) = insert(union(al,bl), a);

fun delete( nil, e) = nil
  | delete(a::l, e) = if a=e then l
                             else a::delete(l,e);

fun unique    nil  = true
  | unique (i::il) = (foldr (fn (i2,b) => i <> i2 andalso b) true il)
                     andalso unique(il);

fun last(a::nil) = a
  | last(a::  l) = last(l);

fun lead(a::nil) = nil
  | lead(a::  l) = a::lead(l);

fun unzip(     nil) = (nil,nil)
  | unzip((a,b)::l) = let val (al,bl) = unzip(l)
                      in (a::al,b::bl)
                      end;



(******************************)
(*          E X P R           *)
(******************************)

datatype etype = IntType
               | BoolType
               | TupelType    of etype list
               | FunctionType of etype * etype
               | Unknown
               | ErrorType    of string;

datatype oper = Add | Sub | Mul | Div | Mod 
              | Eq | Neq | Lt | Gt | Ge | Le 
              | Not | Neg;

datatype const =  IntConst of int
               | BoolConst of bool;

type ident = string;

datatype expr = Id      of ident
              | Const   of const
              | Funsymb of oper
              | Tupel   of expr list
              | Sel     of int * expr
              | Cond    of expr * expr * expr
              | App     of expr * expr
              | Abstr   of ident * etype * expr
              | Fix     of ident * etype * expr;



(************************)
(*   A N A L Y S E      *)
(************************)

exception TypeError of string


(* lookup env id liefert den Typ des Bezeichners id in der Umgebung env 
   bzw eine Exception, wenn der Bezeichner id in der Umgebung nicht 
   vorhanden ist. 
*)
fun lookup          nil id = raise TypeError ("Unknown identifier " ^ id)
  | lookup ((i,t)::env) id = if i = id then t else lookup env id;


(* makefresh(x,ids) liefert einen zum Bezeichner x passenden neuen Namen, 
   der nicht in der Bezeichnermenge ids vorkommt.
*)
fun makefresh(x, ids) =
  let
    fun try base =
      if isElem(ids, base) then try (base ^ "'") else base
  in
    try x
  end




(**************************************
*   freie und gebundene Bezeichner    *
***************************************)
(* bound e bestimmt die Menge der gebundenen Bezeichner im Ausdruck e *)
and bound(Id id)       = nil
  | bound(Const _)     = nil
  | bound(Funsymb _)   = nil
  | bound(Tupel el)    = foldr union nil (map bound el)
  | bound(Sel(i,e))    = bound(e)
  | bound(Cond(b,t,e)) = union(bound(b), union(bound(t),bound(e)))
  | bound(App(f,a))    = union(bound(f), bound(a))
  | bound(Abstr(x,t,e))= insert(bound(e),x)
  | bound(Fix(x,t,e))  = insert(bound(e),x)

(* free e bestimmt die Menge der freien Bezeichner im Ausdruck e *)
and free(Id id)        = [id]
  | free(Const _)      = nil
  | free(Funsymb _)    = nil
  | free(Tupel el)     = foldr union nil (map free el)
  | free(Sel(i,e))     = free(e)
  | free(Cond(b,t,e))  = union(free(b), union(free(t),free(e)))
  | free(App(f,a))     = union(free(f), free(a))
  | free(Abstr(x,t,e)) = delete(free(e),x)
  | free(Fix(x,t,e))   = delete(free(e),x)


(**************************************
*            Typbestimmung            *
***************************************)
(* typeof fs bestimmt den Typ des Funktionssymbols fs vom Datentyp oper *)
and typeOf Add = FunctionType(TupelType[IntType,IntType], IntType)
  | typeOf Sub = FunctionType(TupelType[IntType,IntType], IntType)
  | typeOf Mul = FunctionType(TupelType[IntType,IntType], IntType)
  | typeOf Div = FunctionType(TupelType[IntType,IntType], IntType)
  | typeOf Mod = FunctionType(TupelType[IntType,IntType], IntType)
  | typeOf Eq  = FunctionType(TupelType[IntType,IntType], BoolType)
  | typeOf Neq = FunctionType(TupelType[IntType,IntType], BoolType)
  | typeOf Lt  = FunctionType(TupelType[IntType,IntType], BoolType)
  | typeOf Gt  = FunctionType(TupelType[IntType,IntType], BoolType)
  | typeOf Ge  = FunctionType(TupelType[IntType,IntType], BoolType)
  | typeOf Le  = FunctionType(TupelType[IntType,IntType], BoolType)
  | typeOf Not = FunctionType(BoolType,                   BoolType)
  | typeOf Neg = FunctionType( IntType,                   IntType)

(* typecheck env e bestimmt den Typ eines Ausdrucks e, in dem alle 
   freien Variablen in der Umgebung env vorhanden sind. 
*)
and typecheck env (Id id)              = lookup env id
  | typecheck env (Const( IntConst _)) = IntType
  | typecheck env (Const(BoolConst _)) = BoolType
  | typecheck env (Funsymb f)          = typeOf f
  | typecheck env (Tupel el)           = TupelType (map (typecheck env) el)
  | typecheck env (Sel(i,e))           = 
      (case typecheck env e of 
           TupelType l =>  if (i > 0) andalso (i <= length l)
                           then List.nth (l,i-1)
                           else raise TypeError "Selection: tuple too short"
         |           _ => raise TypeError "Tuple type expected for selection")
  | typecheck env (Cond(b,t,e))        = 
        if typecheck env b = BoolType 
        then let val tt = typecheck env t
                 val et = typecheck env e
             in if tt = et then tt 
                           else raise TypeError "Type error in conditional"
             end
        else raise TypeError "Boolean type expression expected for condition"
  | typecheck env (App(f,a)) = 
      (case typecheck env f of
           FunctionType(ta,tr) => if typecheck env a = ta
                                  then tr
                                  else raise TypeError "Argument type does not match"
         |                   _ => raise TypeError "Function type expected for application")
  | typecheck env (Abstr(x,t,e)) = FunctionType(t, typecheck(insert(env,(x,t))) e)
  | typecheck env (Fix(x,t,e)) = if t = typecheck (insert(env,(x,t))) e
                                 then t
                                 else raise TypeError "Type mismatch in recursion"


(**************************************
*             Substitution            *
***************************************)
(* substitute e1 x e2 liefert die Substitution von x durch e2 
   im Ausdruck e1. 
*)

 
fun substitute (Id a) x e2 =
      if a = x then e2 else Id a
  |substitute (Const c) _ _ = Const c

  | substitute (Funsymb f) _ _ = Funsymb f

  | substitute (Tupel l) x e2 = Tupel (map (fn a => substitute a x e2) l)

  | substitute (Sel(i,a)) x e2 = Sel(i, substitute a x e2)

  | substitute (Cond(b,t,f)) x e =
      Cond(substitute b x e,
           substitute t x e,
           substitute f x e)

  |substitute (App(f,a)) x e2 = App(substitute f x e2, substitute a x e2) 
  | substitute (Abstr(y,t,e1)) x e2 =
      if y = x then Abstr(y,t,e1)
      else if isElem(free e2, y) then
             let
               val z = makefresh(y, union(free e1, union(free e2, union(bound e1, bound e2))))
               val e1' = substitute e1 y (Id z)
             in
               Abstr(z,t, substitute e1' x e2)
             end
           else
             Abstr(y,t, substitute e1 x e2)

  | substitute (Fix(y,t,e1)) x e2 =
      if y = x then Fix(y,t,e1)
      else if isElem(free e2, y) then
             let
               val z = makefresh(y, union(free e1, union(free e2, union(bound e1, bound e2))))
               val e1' = substitute e1 y (Id z)
             in
               Fix(z,t, substitute e1' x e2)
             end
           else
             Fix(y,t, substitute e1 x e2)


(************************)
(*       P R I N T      *)
(************************)

fun oper2string (Not) = "not"
  | oper2string (Neg) = "~"
  | oper2string (Add) = "+"
  | oper2string (Sub) = "-"
  | oper2string (Mul) = "*"
  | oper2string (Div) = "div"
  | oper2string (Mod) = "mod"
  | oper2string (Eq)  = "="
  | oper2string (Neq) = "<>"
  | oper2string (Lt)  = "<"
  | oper2string (Gt)  = ">"
  | oper2string (Ge)  = ">="
  | oper2string (Le)  = "<="


fun toStringType( IntType)    = "int"
  | toStringType(BoolType)    = "bool"
  | toStringType(TupelType l) = 
        "(" ^ (String.concatWith " * " (map toStringType l)) ^ ")"
  | toStringType(FunctionType(arg,erg)) 
                              =   "(" ^ toStringType(arg) ^ " -> " 
                                ^ toStringType(erg) ^ ")"
  | toStringType( Unknown   ) = "? Type"
  | toStringType(ErrorType s) = "Error Type: " ^ s


fun toString(Id(id)):string          = id
  | toString(Const( IntConst   (n))) = Int.toString(n)
  | toString(Const(BoolConst  true)) = "true"
  | toString(Const(BoolConst false)) = "false"
  | toString(Funsymb(fs))            = "op " ^ oper2string(fs)
  | toString(Tupel el)               = 
        "(" ^ (String.concatWith ", " (map toString el)) ^ ")"
  | toString(Sel(i,e))               = 
        "(#" ^ Int.toString(i) ^ " " ^ toString(e) ^ ")"
  | toString(Cond(b,t,e))            = 
        "(if " ^ toString(b) ^ " then " ^ toString(t) 
                             ^ " else " ^ toString(e) ^ ")"
  | toString(App(Funsymb fs, Tupel [a1,a2])) = 
        "(" ^ toString(a1) ^ " " ^ oper2string(fs) ^ " " ^ toString(a2) ^ ")"
  | toString(App(f,a))               = 
        "(" ^ toString(f) ^ " " ^ toString(a) ^")"
  | toString(Abstr(id,t,e))          = 
        "(fn " ^ id ^ " : " ^ toStringType(t) ^ " => " ^ toString(e) ^ ")"
  | toString(Fix(id,t,e))            = 
        "(fix " ^ id ^ " : " ^ toStringType(t) ^ " => " ^ toString(e) ^ ")"




(***********************)
(*   Test Programmes   *)
(***********************)

(*P : (fix fac : int -> int => fn n : int => if n=0 then 1 else n * (fac(n-1))) (#1(5,4)) *)
val p = App
            (Fix
               ("fac",FunctionType (IntType,IntType),
                Abstr
                  ("n",IntType,
                   Cond
                     (App (Funsymb Eq,Tupel [Id "n",Const (IntConst 0)]),
                      Const (IntConst 1),
                      App
                        (Funsymb Mul,
                         Tupel
                           [Id "n",
                            App
                              (Id "fac",
                               App (Funsymb Sub,Tupel [Id "n",Const (IntConst 1)]))])))),
             Sel (1,Tupel [Const (IntConst 5),Const (IntConst 4)])) ;

(* P1: (fn x:int => x+y)(x+y) *)
val p1 =   App
             (Abstr ("x",IntType,App (Funsymb Add,Tupel [Id "x",Id "y"])),
              App (Funsymb Add,Tupel [Id "x",Id "y"]));

(* P2: y+1 *)
val p2 = App (Funsymb Add,Tupel [Id "x",Id "y"]);

substitute p1 "y" p2; 
