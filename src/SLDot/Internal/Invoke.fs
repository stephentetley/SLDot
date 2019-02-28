// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause



namespace SLDot.Internal


module Invoke = 

    open System.IO

    open SLFormat.CommandOptions

    // ************************************************************************
    // Invoking dot

    
    // Currently missing from SLFormat

    /// No space or other punctuation between command and value
    let (&^) (cmd:CmdOpt) (value:string) : CmdOpt = cmd ^^ literal value
    
    /// print space between command and value
    let (&^^) (cmd:CmdOpt) (value:string) : CmdOpt = cmd ^^ character ' ' ^^ literal value


    // // dot -Tjpeg sample1.dot -o sample1.jpg
    /// > latex "<InputFile>.tex"         
    let runDot (shellWorkingDirectory:string) 
               (outputFormat:string) 
               (dotFile:string) 
               (outputFile:string) : unit =
        let args = 
            [ argument "-T" &^ outputFormat
            ; literal dotFile
            ; argument "-o" &^^ outputFile
            ]
        SimpleInvoke.runProcess shellWorkingDirectory "dot" args
