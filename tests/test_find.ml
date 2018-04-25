open Obuild
open Ext

let err = ref 0

let assumeEq testname expected got =
  if expected = got then
    Printf.printf "SUCCESS %s\n" testname
  else 
    (Printf.printf "FAILED %s Expected %s Got %s\n" testname expected got; err := !err + 1)

let archive_to_string (ps, n) =
  let pres = List.map (fun p -> Meta.Predicate.to_string p) ps in
  Printf.sprintf "archive(%s) = [%s]" (String.concat "," pres) n

let archives_to_string l =
  String.concat "\n" (List.map (fun a -> archive_to_string a) l)

let () =
  let meta_unix = "requires = \"\"\n" ^
                  "description = \"Unix system calls\"\n" ^
                  "version = \"[distributed with Ocaml]\"\n" ^
                  "directory = \"^\"\n" ^
                  "browse_interfaces = \" Unit name: Unix Unit name: UnixLabels \"\n" ^
                  "archive(byte) = \"unix.cma\"\n" ^
                  "archive(native) = \"unix.cmxa\"\n" ^
                  "archive(byte,mt_vm) = \"vmthreads/unix.cma\"\n"
  in
  let unix = Meta.parse (Filepath.fp "unix") meta_unix "unix" in
  let unix_answer = Meta.Pkg.get_archive_with_filter (None, unix) (Libname.of_string "unix")
      [Meta.Predicate.Byte; Meta.Predicate.Gprof; Meta.Predicate.Mt] in  
  assumeEq "unix description" "Unix system calls" unix.Meta.Pkg.description;
  assumeEq "unix byte" "archive(byte) = [unix.cma]" (archives_to_string unix_answer);
  
  let meta_netstring = "version = \"4.0.2\"\n" ^
                       "requires = \"str unix netsys \"\n" ^
                       "description = \"Ocamlnet - String processing library\"\n" ^ 
                       "\n" ^
                       "archive(byte) = \n" ^
                       "    \"netstring.cma\"\n" ^
                       "archive(byte,toploop) = \n" ^
                       "    \"netstring.cma netstring_top.cmo\"\n" ^
                       "archive(native) = \n" ^
                       "    \"netstring.cmxa\"\n" ^
                       "archive(native,gprof) = \n" ^
                       "    \"netstring.p.cmxa\"\n" ^
                       "archive(byte,-nonetaccel) +=\n" ^
                       "    \"netaccel.cma netaccel_link.cmo\""
  in
  let netstring = Meta.parse (Filepath.fp "netstring") meta_netstring "netstring" in
  Printf.printf "archives\n%s\n" (archives_to_string netstring.Meta.Pkg.archives);
  Printf.printf "append_archives\n%s\n" (archives_to_string netstring.Meta.Pkg.append_archives);
  assumeEq "netstring description" "Ocamlnet - String processing library" netstring.Meta.Pkg.description;
  let netstring_byte = Meta.Pkg.get_archive_with_filter (None, netstring) (Libname.of_string "netstring")
      [Meta.Predicate.Byte] in
  assumeEq "netstring byte" "archive(byte) = [netstring.cma]\narchive(byte,-nonetaccel) = [netaccel.cma netaccel_link.cmo]" (archives_to_string netstring_byte);
  let netstring_byte_nonetaccel = Meta.Pkg.get_archive_with_filter (None, netstring) (Libname.of_string "netstring")
      [Meta.Predicate.Byte; (Meta.Predicate.Unknown "nonetaccel")] in
  assumeEq "netstring byte nonetaccel" "archive(byte) = [netstring.cma]" (archives_to_string netstring_byte_nonetaccel);
  let meta_num = 
    "# Specification for the \"num\" library:\n\
     requires = \"num.core\"\n\
     requires(toploop) = \"num.core,num-top\"\n\
     version = \"[distributed with Ocaml]\"\n\
     description = \"Arbitrary-precision rational arithmetic\"\n\
     package \"core\" (\n\
    \  directory = \"^\"\n\
    \  version = \"[internal]\"\n\
    \  browse_interfaces = \" Unit name: Arith_flags Unit name: Arith_status Unit name: Big_int Unit name: Int_misc Unit name: Nat Unit name: Num Unit name: Ratio \"\n\
    \  archive(byte) = \"nums.cma\"\n\
    \  archive(native) = \"nums.cmxa\"\n\
    \  plugin(byte) = \"nums.cma\"\n\
    \  plugin(native) = \"nums.cmxs\"\n\
     )\n" in
  let num = Meta.parse (Filepath.fp "num") meta_num "num" in
  let num_answer = Meta.Pkg.get_archive_with_filter (None, num) (Libname.of_string "num.core")
      [Meta.Predicate.Native; Meta.Predicate.Plugin] in  
  assumeEq "num plugin native" "archive(plugin,native) = [nums.cmxs]" (archives_to_string num_answer);
  let meta_threads =  "# Specifications for the \"threads\" library:\n\
  version = \"[distributed with Ocaml]\"\n\
  description = \"Multi-threading\"\n\
  requires(mt,mt_vm) = \"threads.vm\"\n\
  requires(mt,mt_posix) = \"threads.posix\"\n\
  directory = \"^\"\n\
  type_of_threads = \"posix\"\n\
  \n\
  browse_interfaces = \" Unit name: Condition Unit name: Event Unit name: Mutex Unit name: Thread Unit name: ThreadUnix \"\n\
  \n\
  warning(-mt) = \"Linking problems may arise because of the missing -thread or -vmthread switch\"\n\
  warning(-mt_vm,-mt_posix) = \"Linking problems may arise because of the missing -thread or -vmthread switch\"\n\
  \n\
  package \"vm\" (\n\
  \  # --- Bytecode-only threads:\n\
  \  requires = \"unix\"\n\
  \  directory = \"+vmthreads\"\n\
  \  exists_if = \"threads.cma\"\n\
  \  archive(byte,mt,mt_vm) = \"threads.cma\"\n\
  \  version = \"[internal]\"\n\
  )\n\
  \n\
  package \"posix\" (\n\
  \  # --- POSIX-threads:\n\
  \  requires = \"unix\"\n\
  \  directory = \"+threads\"\n\
  \  exists_if = \"threads.cma\"\n\
  \  archive(byte,mt,mt_posix) = \"threads.cma\"\n\
  \  archive(native,mt,mt_posix) = \"threads.cmxa\"\n\
  \  version = \"[internal]\"\n\
                       )\n" in
  let threads = Meta.parse (Filepath.fp "threads") meta_threads "threads" in
  let threads_answer = Meta.Pkg.get_archive_with_filter (None, threads) (Libname.of_string "threads.posix")
      [Meta.Predicate.Native; Meta.Predicate.Mt; Meta.Predicate.Mt_posix] in
  assumeEq "threads native" "archive(native,mt,mt_posix) = [threads.cmxa]" (archives_to_string threads_answer);

  let meta_ctypes = 
    "\
  version = \"0.4\"\n\
  description = \"Combinators for binding to C libraries without writing any C.\"\n\
  requires = \"unix bigarray str bytes\"\n\
  archive(byte) = \"ctypes.cma\"\n\
  archive(byte, plugin) = \"ctypes.cma\"\n\
  archive(native) = \"ctypes.cmxa\"\n\
  archive(native, plugin) = \"ctypes.cmxs\"\n\
  exists_if = \"ctypes.cma\"\n\
  \n\
  package \"top\" (\n\
  \  version = \"0.4\"\n\
  \  description = \"Toplevel printers for C types\"\n\
  \  requires = \"ctypes\"\n\
  \  archive(byte) = \"ctypes-top.cma\"\n\
  \  archive(byte, plugin) = \"ctypes-top.cma\"\n\
  \  archive(native) = \"ctypes-top.cmxa\"\n\
  \  archive(native, plugin) = \"ctypes-top.cmxs\"\n\
  \  exists_if = \"ctypes-top.cma\"\n\
  )\n\
  \n\
  package \"stubs\" (\n\
  \  version = \"0.4\"\n\
  \  description = \"Stub generation from C types\"\n\
  \  requires = \"ctypes\"\n\
  \  archive(byte) = \"cstubs.cma\"\n\
  \  archive(byte, plugin) = \"cstubs.cma\"\n\
  \  archive(native) = \"cstubs.cmxa\"\n\
  \  archive(native, plugin) = \"cstubs.cmxs\"\n\
  \  xen_linkopts = \"-lctypes_stubs_xen\"\n\
  \  exists_if = \"cstubs.cma\"\n\
  )\n\
  \n\
  package \"foreign\" (\n\
  \ version = \"0.4\"\n\
  \ description = \"Dynamic linking of C functions\"\n\
  \ requires(-mt) = \"ctypes.foreign.unthreaded\"\n\
  \ requires(mt) = \"ctypes.foreign.threaded\"\n\
  \n\
  \ package \"base\" (\n\
  \  version = \"0.4\"\n\
  \  description = \"Dynamic linking of C functions (base package)\"\n\
  \  requires = \"ctypes\"\n\
  \  archive(byte) = \"ctypes-foreign-base.cma\"\n\
  \  archive(byte, plugin) = \"ctypes-foreign-base.cma\"\n\
  \  archive(native) = \"ctypes-foreign-base.cmxa\"\n\
  \  archive(native, plugin) = \"ctypes-foreign-base.cmxs\"\n\
  \  exists_if = \"ctypes-foreign-base.cma\"\n\
  \ )\n\
  \n\
  \ package \"threaded\" (\n\
  \  version = \"0.4\"\n\
  \  description = \"Dynamic linking of C functions (for use in threaded programs)\"\n\
  \  requires = \"threads ctypes ctypes.foreign.base\"\n\
  \  archive(byte) = \"ctypes-foreign-threaded.cma\"\n\
  \  archive(byte, plugin) = \"ctypes-foreign-threaded.cma\"\n\
  \  archive(native) = \"ctypes-foreign-threaded.cmxa\"\n\
  \  archive(native, plugin) = \"ctypes-foreign-threaded.cmxs\"\n\
  \  exists_if = \"ctypes-foreign-threaded.cma\"\n\
  \ )\n\
  \n\
  \ package \"unthreaded\" (\n\
  \  version = \"0.4\"\n\
  \  description = \"Dynamic linking of C functions (for use in unthreaded programs)\"\n\
  \  requires = \"ctypes ctypes.foreign.base\"\n\
  \  archive(byte) = \"ctypes-foreign-unthreaded.cma\"\n\
  \  archive(byte, plugin) = \"ctypes-foreign-unthreaded.cma\"\n\
  \  archive(native) = \"ctypes-foreign-unthreaded.cmxa\"\n\
  \  archive(native, plugin) = \"ctypes-foreign-unthreaded.cmxs\"\n\
  \  exists_if = \"ctypes-foreign-unthreaded.cma\"\n\
  \ )\n\
  )\n\
  " in
  let ctypes = Meta.parse (Filepath.fp "ctypes") meta_ctypes "ctypes" in
  Printf.printf "archives\n%s\n" (archives_to_string ctypes.Meta.Pkg.archives);
  Printf.printf "append_archives\n%s\n" (archives_to_string ctypes.Meta.Pkg.append_archives);

  if !err > 0 then 
    exit 1
  else 
    exit 0
      
