let bytes_of_string = Bytes.of_string
let bytes_to_string = Bytes.to_string
let bytes_make = Bytes.make
let bytes_create = Bytes.create
let bytes_get = Bytes.get
let bytes_set = Bytes.set
let bytes_length = Bytes.length
let bytes_index_from = Bytes.index_from

let buffer_add_subbytes = Buffer.add_subbytes

let string_uncapitalize = String.uncapitalize_ascii
let string_capitalize = String.capitalize_ascii
let string_lowercase = String.lowercase_ascii
let string_uppercase = String.uppercase_ascii
let char_uppercase = Char.uppercase_ascii

include Compat_common
