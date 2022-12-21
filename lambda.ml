
(* TYPE DEFINITIONS *)

type ty =
    TyBool
  | TyNat
  | TyString
  | TyUnit
  | TyArr of ty * ty
  | TyRecord of (string * ty) list
  | TyList of ty
  | TyTuple of ty list
  
;;

type 'a context =
  (string * 'a) list  (* Contexto polimórfico 'a es alfa *)
;;

type term =
    TmTrue
  | TmFalse
  | TmIf of term * term * term
  | TmZero
  | TmSucc of term
  | TmPred of term
  | TmIsZero of term
  | TmVar of string
  | TmAbs of string * ty * term
  | TmApp of term * term
  | TmLetIn of string * term * term
  | TmFix of term
  | TmString of string
  | TmUnit
  | TmProj of term * string
    (*Records*)
  | TmRecord of (string * term) list
    (*List*)
  | TmNil of ty
  | TmCons of ty * term * term
  | TmIsNil of ty * term
  | TmHead of ty * term
  | TmTail of ty * term
    (*Tuples*)
  | TmTuple of term list
    (* Concat *)
  | TmConcat of term * term 
;;

type command = 
  Eval of term
  | Bind of string * term
;;

(* CONTEXT MANAGEMENT *)

let emptyctx =
  []
;;

let addbinding ctx x bind =
  (x, bind) :: ctx
;;

let getbinding ctx x =
  List.assoc x ctx
;;

(* GLOBAL DEFINITIONS *)

let emptydef = 
  []
;;

let adddef def x bind =
  (x, bind) :: def
;;

let getdef def x = 
  List.assoc x def
;;

(* TYPE MANAGEMENT (TYPING) *)

let rec string_of_ty ty = match ty with
    TyBool ->
      "Bool"
  | TyNat ->
      "Nat"
  | TyString -> 
      "String"
  | TyUnit ->
      "Unit"
  | TyArr (ty1, ty2) ->
      "(" ^ string_of_ty ty1 ^ ")" ^ " -> " ^ "(" ^ string_of_ty ty2 ^ ")"      
  
  | TyTuple tyr -> 
    let rec print = function
      [] -> ""
      |(ty::[]) -> (string_of_ty ty)
      |(ty::t) -> (string_of_ty ty) ^ ", " ^ print t
    in "{" ^ (print tyr) ^ "}"
      
  | TyRecord tyr ->
      let rec printrecord = function
        [] -> ""
        |((s,ty)::[]) -> s ^ ":" ^ (string_of_ty ty)
        |((s,ty)::t) ->  s ^ ":" ^ (string_of_ty ty) ^ "," ^ printrecord t
      in "{" ^ (printrecord tyr) ^ "}"
      
  | TyList ty -> "List[" ^ string_of_ty ty ^ "]"
;;

exception Type_error of string
;;

let rec subtypeof tm1 tm2 = match (tm1, tm2) with
  | (TyArr(s1, s2), TyArr(t1, t2)) -> ((subtypeof s1 t1) && (subtypeof t2 s2))
  | (TyRecord(l1), TyRecord(l2)) ->
    let check (x, ty) l =
      try 
        subtypeof ty (List.assoc x l)
    with _ -> false
    in let rec contains l1 l2 = match l1 with
      | [] -> true
      | (h::t) -> (&&) (check h l2) (contains t l2)
      in contains l1 l2
  | (tm1, tm2) -> tm1 = tm2
;;

let rec typeof ctx tm = match tm with
    (* T-True *)
    TmTrue ->
      TyBool

    (* T-False *)
  | TmFalse ->
      TyBool
  
    (* T-String *)
  | TmString _ -> 
      TyString

    (* T-Concat *)
  | TmConcat (t1,t2) -> 
    if (typeof ctx t1 = TyString) && (typeof ctx t2 = TyString) then
      TyString
    else 
      raise (Type_error "terms of concatenate are not strings")

    (* T-If *)
  | TmIf (t1, t2, t3) ->
      if typeof ctx t1 = TyBool then
        let tyT2 = typeof ctx t2 in
        if typeof ctx t3 = tyT2 then tyT2
        else raise (Type_error "arms of conditional have different types")
      else
        raise (Type_error "guard of conditional not a boolean")
      
    (* T-Zero *)
  | TmZero ->
      TyNat

    (* T-Succ *)
  | TmSucc t1 ->
      if typeof ctx t1 = TyNat then TyNat
      else raise (Type_error "argument of succ is not a number")

    (* T-Pred *)
  | TmPred t1 ->
      if typeof ctx t1 = TyNat then TyNat
      else raise (Type_error "argument of pred is not a number")

    (* T-Iszero *)
  | TmIsZero t1 ->
      if typeof ctx t1 = TyNat then TyBool
      else raise (Type_error "argument of iszero is not a number")

    (* T-Var *)
  | TmVar x ->
      (try getbinding ctx x with
       _ -> raise (Type_error ("no binding type for variable " ^ x)))

    (* T-Abs *)
  | TmAbs (x, tyT1, t2) ->
      let ctx' = addbinding ctx x tyT1 in
      let tyT2 = typeof ctx' t2 in
      TyArr (tyT1, tyT2)

    (* T-App *)
  | TmApp (t1, t2) ->
      let tyT1 = typeof ctx t1 in
      let tyT2 = typeof ctx t2 in
      (match tyT1 with
           TyArr (tyT11, tyT12) ->
             if subtypeof tyT11 tyT2 then tyT12
             else raise (Type_error "parameter type mismatch")
         | _ -> raise (Type_error "arrow type expected"))

    (* T-Let *)
  | TmLetIn (x, t1, t2) ->
      let tyT1 = typeof ctx t1 in
      let ctx' = addbinding ctx x tyT1 in
      typeof ctx' t2
  
    (* T-Fix *)
  | TmFix t1 ->
      let tyT1 = typeof ctx t1 in
      (match tyT1 with
          TyArr (tyT11, tyT12) ->
            if subtypeof tyT11 tyT12 then tyT12
            else raise (Type_error "result of body not compatible with domain")
        | _ -> raise (Type_error "arrow type unexpected"))

    (* T-Unit *)
  | TmUnit ->
      TyUnit
     
    (* T-Record *)
  |TmRecord tmr -> 
      let rec tipos = function
       [] -> []
       |((s,tm)::t) -> ((s,typeof ctx tm)::(tipos t))
      in TyRecord (tipos tmr)
        
  | TmNil ty -> TyList ty
        
        
    (* T-Cons *)
  | TmCons (ty,h,t) ->
      let tyTh = typeof ctx h in
        let tyTt = typeof ctx t in
           if (subtypeof tyTh ty) && (subtypeof tyTt (TyList(ty))) then 
              TyList(ty) else raise (Type_error "elements of list have 
            different types")
            
      (* T-IsNil *)
  | TmIsNil (ty,t) ->
      if typeof ctx t = TyList(ty) then TyBool
      else raise (Type_error ("argument of is empty is not a " ^ "List[" ^ (string_of_ty ty) ^ "]"))

     (* T-Head *)
  | TmHead (ty,t) ->
      if typeof ctx t = TyList(ty) then ty
      else raise (Type_error ("argument of head is not a " ^ "List[" ^ (string_of_ty ty) ^ "]"))
      
     (* T-Tail *)
  | TmTail (ty,t) ->
      if typeof ctx t = TyList(ty) then TyList(ty)
      else raise (Type_error ("argument of tail is not a " ^ "List[" ^ (string_of_ty ty) ^ "]"))
       
      (* T-Tuple *)
  | TmTuple tml ->
      let rec get_types = function
          [] -> []
          |(tm::t) -> ((typeof ctx tm)::(get_types t))
      in TyTuple (get_types tml)
       
       
      (* T-Proj *)
  | TmProj (t,n) ->
      (match(typeof ctx t, n) with
        |(TyList (ty), _) -> ty
        |(TyRecord (tyr), s) ->
          (try List.assoc s tyr with
          _ -> raise (Type_error ("cannot project "^ s ^ ",this key does not exist in the record"))) 
      
        |(TyTuple (tyr), s) ->
          (try List.nth tyr (int_of_string s - 1) with
          _ -> raise (Type_error ("cannot project "^ s ^ ",this key does not exist in the record"))) 
      
      (*Errors*) 
  |(x,_) -> raise (Type_error ("cannot project type " ^ string_of_ty x)))  
;;


(* TERMS MANAGEMENT (EVALUATION) *)

let rec string_of_term = function
    TmTrue ->
      "true"
  | TmFalse ->
      "false"
  | TmString (s) -> 
    "\"" ^ s ^ "\""
  | TmConcat (t1,t2) ->
      string_of_term t1 ^ " ^ " ^ string_of_term t2
  | TmIf (t1,t2,t3) ->
      "if " ^ "(" ^ string_of_term t1 ^ ")" ^
      " then " ^ "(" ^ string_of_term t2 ^ ")" ^
      " else " ^ "(" ^ string_of_term t3 ^ ")"
  | TmZero ->
      "0"
  | TmSucc t ->
     let rec f n t' = match t' with
          TmZero -> string_of_int n
        | TmSucc s -> f (n+1) s
        | _ -> "succ " ^ "(" ^ string_of_term t ^ ")"
      in f 1 t
  | TmPred t ->
      "pred " ^ "(" ^ string_of_term t ^ ")"
  | TmIsZero t ->
      "iszero " ^ "(" ^ string_of_term t ^ ")"
  | TmVar s ->
      s
  | TmAbs (s, tyS, t) ->
      "(lambda " ^ s ^ ":" ^ string_of_ty tyS ^ ". " ^ string_of_term t ^ ")"
  | TmApp (t1, t2) ->
      "(" ^ string_of_term t1 ^ " " ^ string_of_term t2 ^ ")"
  | TmLetIn (s, t1, t2) ->
      "let " ^ s ^ " = " ^ string_of_term t1 ^ " in " ^ string_of_term t2
  | TmFix t ->
      "(fix " ^ string_of_term t ^ ")"
  | TmUnit ->
      "()"
  | TmProj (t,n) ->
      string_of_term t  ^ "." ^ n  
      
  | TmRecord tmr ->
    let rec printrecord = function
      [] -> ""
      |((s,tm)::[]) -> s ^ "=" ^ (string_of_term tm)
      |((s,tm)::t) ->  s ^ "=" ^ (string_of_term tm) ^ "," ^ printrecord t
     in "{" ^ (printrecord tmr) ^ "}"
     
 
  | TmNil ty -> "nil[" ^string_of_ty ty ^ "]"
  | TmCons (ty,h,t) -> "cons[" ^string_of_ty ty ^ "] " ^ "(" ^ string_of_term h ^ " " ^ (string_of_term t) ^ ")"
  | TmIsNil (ty,t) -> "isnil[" ^string_of_ty ty ^ "] " ^ "(" ^ string_of_term t ^ ")" 
  | TmHead (ty,t) -> "head[" ^string_of_ty ty ^ "] " ^ "(" ^ string_of_term t ^ ")" 
  | TmTail (ty,t) -> "tail[" ^string_of_ty ty ^ "] " ^ "(" ^ string_of_term t ^ ")"
  
  | TmTuple tml ->
      let rec print = function
        [] -> ""
        |(tm::[]) -> (string_of_term tm)
        |(tm::t) -> (string_of_term tm) ^ ", " ^ print t
      in "{" ^ (print tml) ^ "}"  

;;

let rec ldif l1 l2 = match l1 with
    [] -> []
  | h::t -> if List.mem h l2 then ldif t l2 else h::(ldif t l2)
;;

let rec lunion l1 l2 = match l1 with
    [] -> l2
  | h::t -> if List.mem h l2 then lunion t l2 else h::(lunion t l2)
;;

let rec free_vars tm = match tm with (* variables libres *)
    TmTrue ->
      []
  | TmFalse ->
      []
  | TmString _ -> 
      []
  | TmUnit ->
      []
  | TmIf (t1, t2, t3) ->
      lunion (lunion (free_vars t1) (free_vars t2)) (free_vars t3)
  | TmZero ->
      []
  | TmSucc t ->
      free_vars t
  | TmPred t ->
      free_vars t
  | TmIsZero t ->
      free_vars t
  | TmVar s ->
      [s]
  | TmAbs (s, _, t) ->
      ldif (free_vars t) [s]
  | TmApp (t1, t2) ->
      lunion (free_vars t1) (free_vars t2)
  | TmLetIn (s, t1, t2) ->
      lunion (ldif (free_vars t2) [s]) (free_vars t1)
  | TmFix t ->
      free_vars t
  | TmProj (t, n) ->
      free_vars t
      
  | TmRecord tmr ->
     let rec get_free = function
      [] -> []  
      |((_,tm)::t) -> lunion (free_vars tm) (get_free t)
     in get_free tmr
     
  | TmNil ty ->
      []
      
  | TmCons (ty,t1,t2) ->
        lunion (free_vars t1) (free_vars t2)
        
  | TmIsNil (ty,t) ->
      free_vars t
     
  | TmHead (ty,t) ->
      free_vars t
      
  | TmTail (ty,t) ->
      free_vars t
      
  | TmTuple tml->
    let rec get_free = function
      [] -> []
      |(tm::t) -> lunion (free_vars tm) (get_free t)
    in get_free tml  
  
  | TmConcat (t1, t2) -> 
      lunion (free_vars t1) (free_vars t2)
;;




let rec fresh_name x l =
  if not (List.mem x l) then x else fresh_name (x ^ "'") l
;;
    
let rec subst x s tm = match tm with (* sustituir las apariciones de x en tm por s *)
    TmTrue ->
      TmTrue
  | TmFalse ->
      TmFalse
  | TmString _ -> 
      tm
  | TmUnit ->
      tm
  | TmConcat (t1, t2) ->
      TmConcat (subst x s t1, subst x s t2)
  | TmIf (t1, t2, t3) ->
      TmIf (subst x s t1, subst x s t2, subst x s t3)
  | TmZero ->
      TmZero
  | TmSucc t ->
      TmSucc (subst x s t)
  | TmPred t ->
      TmPred (subst x s t)
  | TmIsZero t ->
      TmIsZero (subst x s t)
  | TmVar y ->
      if y = x then s else tm
  | TmAbs (y, tyY, t) -> 
      if y = x then tm
      else let fvs = free_vars s in
           if not (List.mem y fvs)
           then TmAbs (y, tyY, subst x s t)
           else let z = fresh_name y (free_vars t @ fvs) in
                TmAbs (z, tyY, subst x s (subst y (TmVar z) t))  
  | TmApp (t1, t2) ->
      TmApp (subst x s t1, subst x s t2)
  | TmLetIn (y, t1, t2) ->
      if y = x then TmLetIn (y, subst x s t1, t2)
      else let fvs = free_vars s in
           if not (List.mem y fvs)
           then TmLetIn (y, subst x s t1, subst x s t2)
           else let z = fresh_name y (free_vars t2 @ fvs) in
                TmLetIn (z, subst x s t1, subst x s (subst y (TmVar z) t2))
  | TmFix t ->
      TmFix (subst x s t)
      
   
  
  | TmProj (t,n) ->
      TmProj  (subst x s t, n)
      
  | TmRecord tmr ->
     let rec subsrecord = function
       [] -> []
       |((str,tm)::t) -> (str,(subst x s tm))::(subsrecord t)
      in TmRecord (subsrecord tmr)
      
      
  | TmNil ty ->
      tm
      
  | TmCons (ty,t1,t2) ->
        TmCons (ty, (subst x s t1), (subst x s t2))
    
  | TmIsNil (ty,t) ->
       TmIsNil (ty, (subst x s t))
       
  | TmHead (ty,t) ->
       TmHead (ty, (subst x s t))
       
       
  | TmTail (ty,t) ->
       TmTail (ty, (subst x s t))
       
  | TmTuple tml ->
      let rec subs_rcd = function
        [] -> []
        |(tm::t) -> (subst x s tm)::(subs_rcd t)
      in TmTuple (subs_rcd tml)
;;

let rec isnumericval tm = match tm with
    TmZero -> true
  | TmSucc t -> isnumericval t
  | _ -> false
;;



exception NoRuleApplies;;

let rec isval tm = match tm with
    TmTrue  -> true
  | TmFalse -> true
  | TmAbs _ -> true
  | TmString _ -> true
  | TmUnit -> true
  | TmTuple l -> List.for_all(fun t -> isval(t)) l 
  | TmRecord [] -> true 
  | TmRecord l -> List.for_all(fun (s,t) -> isval(t)) l 
  | TmNil _ -> true
  | TmCons(_,h,t) -> (isval h) && (isval t)
  | t when isnumericval t -> true
  | _ -> false
;;
let concat t1 t2 = t1 ^ t2;;



let rec eval1 vctx tm = match tm with
    (* E-IfTrue *)
    TmIf (TmTrue, t2, _) ->
      t2

    (* E-IfFalse *)
  | TmIf (TmFalse, _, t3) ->
      t3

    (* E-If *)
  | TmIf (t1, t2, t3) ->
      let t1' = eval1 vctx t1 in
      TmIf (t1', t2, t3)

    (* E-Succ *)
  | TmSucc t1 ->
      let t1' = eval1 vctx t1 in
      TmSucc t1'
  
     (* E-Concat *) 
  | TmConcat (TmString(s1),TmString(s2)) ->  
      TmString(s1^s2)

    (* E-Concat2 *)
  | TmConcat (TmString(s),t1) -> 
      let t1' = eval1 vctx t1 
      in TmConcat (TmString(s),t1')

    (* E-Concat1 *)
  | TmConcat (t1,t2) -> 
      let t1' = eval1 vctx t1 
      in TmConcat (t1',t2)
      
    (* E-PredZero *)
  | TmPred TmZero ->
      TmZero

    (* E-PredSucc *)
  | TmPred (TmSucc nv1) when isnumericval nv1 ->
      nv1

    (* E-Pred *)
  | TmPred t1 ->
      let t1' = eval1 vctx t1 in
      TmPred t1'

    (* E-IszeroZero *)
  | TmIsZero TmZero ->
      TmTrue

    (* E-IszeroSucc *)
  | TmIsZero (TmSucc nv1) when isnumericval nv1 ->
      TmFalse

    (* E-Iszero *)
  | TmIsZero t1 ->
      let t1' = eval1 vctx t1 in
      TmIsZero t1'

    (* E-AppAbs *)
  | TmApp (TmAbs(x, _, t12), v2) when isval v2 ->
      subst x v2 t12

    (* E-App2: evaluate argument before applying function *)
  | TmApp (v1, t2) when isval v1 ->
      let t2' = eval1 vctx t2 in
      TmApp (v1, t2')

    (* E-App1: evaluate function before argument *)
  | TmApp (t1, t2) ->
      let t1' = eval1 vctx t1 in
      TmApp (t1', t2)

    (* E-LetV *)
  | TmLetIn (x, v1, t2) when isval v1 ->
      subst x v1 t2

    (* E-Let *)
  | TmLetIn(x, t1, t2) ->
      let t1' = eval1 vctx t1 in
      TmLetIn (x, t1', t2) 

    (* E-FixBeta *)
  | TmFix (TmAbs (x, _, t2)) ->
      subst x tm t2 
    
    (* E-Fix *)
  | TmFix t1 ->   (* cuando t1 aún no es una abstracción, evalúo t1 para obtener t1' *)
      let t1' = eval1 vctx t1 in
      TmFix t1'

  | TmVar s ->
      getbinding vctx s
      
    (*E-Cons2*)
  |TmCons(ty,h,t) when isval h -> TmCons(ty,h,(eval1 vctx t))
  
    (*E-Cons1*)
  |TmCons(ty,h,t) -> TmCons(ty,(eval1 vctx h),t)
  
    (*E-IsNilNil*)
  |TmIsNil(ty,TmNil(_)) -> TmTrue
  
    (*E-IsNilCons*)
  |TmIsNil(ty,TmCons(_,_,_)) -> TmFalse
  
    (*E-IsNil*)
  |TmIsNil(ty,t) -> TmIsNil(ty,eval1 vctx t)
  
    (*E-HeadCons*)
  |TmHead(ty,TmCons(_,h,_))-> h
  
    (*E-Head*)
  |TmHead(ty,t) -> TmHead(ty,eval1 vctx t)
  
    (*E-TailCons*)
  |TmTail(ty,TmCons(_,_,t)) -> t
  
    (*E-Tail*)
  |TmTail(ty,t) -> TmTail(ty,eval1 vctx t)
      
  |TmProj (TmRecord l as v , s) when isval(v) -> 
    List.assoc s l 

    (*E-ProjRecord*)
  |TmProj (TmRecord (tmr), n) ->
     List.assoc n tmr 
     
    (*E-Proj*)
  | TmProj (TmTuple l as v , s) when isval(v) -> 
    List.nth l (int_of_string s - 1)

  | TmProj (t,n) ->
    TmProj ((eval1 vctx t), n)
           
  | TmTuple tml ->
    let rec eval_rcd = function
      [] -> raise NoRuleApplies
      |(tm::t) when isval tm -> tm::(eval_rcd t)
      |(tm::t) -> (eval1 vctx tm)::t
    in TmTuple (eval_rcd tml)
     
   (*E-Record*)
  | TmRecord tmr ->
     let rec evalrecord = function
       [] -> raise NoRuleApplies
       |((str,tm)::t) when isval tm -> (str,tm)::(evalrecord t)
       |((str,tm)::t) -> (str,(eval1 vctx tm))::t
      in TmRecord (evalrecord tmr)

  | _ ->
      raise NoRuleApplies
      
;;


let apply_ctx ctx tm = 
  List.fold_left (fun t x -> subst x (getbinding ctx x) t) tm (free_vars tm)
 ;;

let rec eval vctx tm =
  try
    let tm' = eval1 vctx tm in
    eval vctx tm'
  with
    NoRuleApplies -> apply_ctx vctx tm
;;

let execute (vctx, tctx) = function
  Eval tm ->
    let tyTm = typeof tctx tm in
    let tm' = eval vctx tm in
    print_endline("- : " ^ string_of_ty tyTm ^ " = " ^ string_of_term tm');
    (vctx, tctx)
| Bind (s, tm) ->
    let tyTm = typeof tctx tm in
    let tm' = eval vctx tm in
    print_endline(s ^ " : " ^ string_of_ty tyTm ^ " = " ^ string_of_term tm');
    (addbinding vctx s tm', addbinding tctx s tyTm)
;;

