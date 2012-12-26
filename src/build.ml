open Types

exception CompilationFailed of string
exception DependencyAnalyzeFailed of string

let compileExe generalConf projFile exe =
    ()
