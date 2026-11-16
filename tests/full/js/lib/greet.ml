external punctuation : unit -> string = "greet_punctuation"

let greeting name = "hello " ^ name ^ punctuation ()
