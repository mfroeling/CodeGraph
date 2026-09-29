(* ::Package:: *)

Paclet[
    Name -> "CodeGraph",
    Version -> "0.1.2",
    WolframVersion -> "13.0+",
    Description -> "Static call graph of Wolfram Language paclets: definitions, callers and package dependencies, with interactive views",
    Creator -> "Martijn Froeling <m.froeling@gmail.com>",
    Support -> "https://github.com/mfroeling/CodeGraph",
    Extensions ->
        {
            {"Kernel", Root -> "Kernel", Context -> "CodeGraph`"}
        }
]
