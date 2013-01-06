type flag_tweak = SetFlag of string | ClearFlag of string

type dep_expr = Eq of string
              | Gt of string
              | Ge of string
              | Le of string
              | Lt of string
              | And of (dep_expr * dep_expr)
              | Or of (dep_expr * dep_expr)

type dep_constraint = dep_expr

type dep_main_name = string

(* represent a dependency in a form abc[.def.xyz] *)
type dep_name = { dep_name : dep_main_name
                ; dep_subname : string list
                }

type lib_name = { lib_name : string } 

type dependency = dep_name * (dep_constraint option)
