// Copyright (c) Stephen Tetley 2018
// License: BSD 3 Clause

#I @"C:\Users\stephen\.nuget\packages\slformat\1.0.2-alpha-20190205\lib\netstandard2.0"
#r "SLFormat.dll"

#load @"../src/SLDot/Internal/DotPrint.fs"
#load @"../src/SLDot/DotOutput.fs"
open SLFormat.Pretty
open SLDot.DotOutput

let testNesting01 () : unit = 
    let a : Doc = text "aaa"
    let b : Doc = text "bbb"
    let c : Doc = text "ccc"
    let d : Doc = text "ddd"
    let newline = character '\n'
    let doc1 : Doc  = a ^/^ (nest 2 (newline ^/^ b ^/^ newline ^/^ c)) ^/^ newline ^^ d
    printfn "%s" <| render 80 doc1

let testNesting02 () : unit = 
    let a : Doc = text "aaa"
    let b : Doc = text "bbb"
    let c : Doc = text "ccc"
    let d : Doc = text "ddd"
    let doc1 : Doc  = group (        group (nest 4 a) 
                                ^//^ group (nest 4 b) 
                                ^//^ group (nest 4 c)) ^//^ d
    printfn "%s" <| render 80 doc1

let test01 () : unit = 
    let procM = 
        graphvizOutput { 
            do! lineComment "dot -Tjpeg mydot.dot -o mydot.jpg"
            do! digraph "G1" <| 
                graphvizOutput { 
                    do! attrib <| rankdir LR
                    do! attrib <| ranksep 1.5
                    do! nodeAttributes [fontname "Arial"; fontsize 10]
                    do! anonSubgraph 
                        <| graphvizOutput { 
                            do! node "node1" []
                            do! node "node2" []
                            do! node "node3" []
                            do! edge "node1" "node2" []
                            do! edges "node3" ["node2";"node1"] []
                            return ()
                            } 
                }
            }
    ignore <| runGraphvizOutputConsole procM
