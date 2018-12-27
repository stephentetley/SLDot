// Copyright (c) Stephen Tetley 2018
// License: BSD 3 Clause

#load "../src/SLDot/Internal/PrettyPrint.fs"
#load "../src/SLDot/Internal/DotPrint.fs"
#load "../src/SLDot/DotOutput.fs"
open SLDot.DotOutput



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
