open Types
open Ext

let wrap_module x = { modname = x }
let wrap_filepath x = { filepath = x }
let wrap_filename x = { filename = x }

let with_optpath dir filename = wrap_filepath (default "" dir </> filename.filename)
let with_path dir filename = wrap_filepath (dir </> filename.filename)

let cmxa_of_lib lib = wrap_filename (lib ^ ".cmxa")
let cma_of_lib lib = wrap_filename (lib ^ ".cma")

let cmx_of_module modname = wrap_filename (String.uncapitalize modname.modname ^ ".cmx")
let cmo_of_module modname = wrap_filename (String.uncapitalize modname.modname ^ ".cmo")
let cmc_of_module b = if b then cmx_of_module else cmo_of_module
let cmi_of_module modname = wrap_filename (String.uncapitalize modname.modname ^ ".cmi")
let o_of_module modname = wrap_filename (String.uncapitalize modname.modname ^ ".o")

let filename_of_module modname = wrap_filename (String.uncapitalize modname.modname ^ ".ml")
let interface_of_module modname = wrap_filename (String.uncapitalize modname.modname ^ ".mli")

let module_of_filename filename = wrap_module (String.capitalize (Filename.chop_extension filename.filename))

