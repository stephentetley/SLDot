// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause



namespace SLDot.Internal


module Invoke = 

    open System.IO

    open SLFormat.CommandOptions

    // ************************************************************************
    // Invoking dot

    // Running a process (e.g dot)
    let private executeProcess (workingDirectory:string) (toolPath:string) (command:string) : Choice<string,int> = 
        try
            let procInfo = new System.Diagnostics.ProcessStartInfo ()
            procInfo.WorkingDirectory <- workingDirectory
            procInfo.FileName <- toolPath
            procInfo.Arguments <- command
            procInfo.CreateNoWindow <- true
            let proc = new System.Diagnostics.Process()
            proc.StartInfo <- procInfo
            proc.Start() |> ignore
            proc.WaitForExit () 
            Choice2Of2 <| proc.ExitCode
        with
        | ex -> Choice1Of2 (sprintf "executeProcess: \n%s" ex.Message)

    let shellRun (workingDirectory:string) (toolPath:string) (command:string)  : unit = 
        try
            match executeProcess workingDirectory toolPath command with
            | Choice1Of2(errMsg) -> failwith errMsg
            | Choice2Of2(code) -> 
                if code <> 0 then
                    failwithf "shellRun fail - error code: %i" code
                else ()
        with
        | ex -> 
            let diagnosis = 
                String.concat "\n" <| 
                    [ ex.Message
                    ; sprintf "Working Directory: %s" workingDirectory 
                    ; sprintf "Command Args: %s" command
                    ]
            failwithf "shellRun exception: \n%s" diagnosis

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
        shellRun shellWorkingDirectory "dot" (arguments args)
