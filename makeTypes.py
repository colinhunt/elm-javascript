types = [
    'Int',
    'Str',
    'Bool',
    'Date',
    'Void'
]

with open('JavaScript.elm', 'w') as f:
    f.write("""module JavaScript exposing (..)

""")
    for t in types:
        f.write(""" 
{{-| {type}
-}}
type {type}Type
    = {type}Type


type alias {type} =
    {{ str : String, t : {type}Type }}


{type_lc} : String -> {type}
{type_lc} str =
    {{ str = str, t = {type}Type }}

""".format(type=t, type_lc=t.lower()))
