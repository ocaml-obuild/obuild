(* Bindings module for ctypes.cstubs test *)
(* This module defines the C types and functions we want to bind *)

open Ctypes

(* Type descriptions - defines C types we need using Cstubs_structs.TYPE
   for proper struct layout discovery *)
module Types (T : Cstubs_structs.TYPE) = struct
  open T

  (* size_t type - used by strlen *)
  let size_t = typedef size_t "size_t"

  (* timespec struct - demonstrates struct binding *)
  type timespec
  let timespec : timespec structure typ = structure "timespec"
  let tv_sec = field timespec "tv_sec" long
  let tv_nsec = field timespec "tv_nsec" long
  let () = seal timespec
end

(* Function descriptions - defines C functions we want to call *)
module Functions (F : Ctypes.FOREIGN) = struct
  open F
  (* Bind the C strlen function *)
  let strlen = foreign "strlen" (string @-> returning size_t)
end
