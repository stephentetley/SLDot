// Copyright (c) Stephen Tetley 2018
// License: BSD 3 Clause



namespace SLDot.Internal


module DotPrint = 

    open SLDot.Internal.PrettyPrint

    
    // Single case union
    [<Struct>]
    type WriterDoc = 
        | WriterDoc of Doc
        member internal x.Body with get() = match x with | WriterDoc(doc) -> doc

        /// Internal note - this renders with a huge line width to avoid line breaking
        member x.Render() : string = 
            render 1000 x.Body

        member x.SaveAsDot(lineWidth:int, fileName:string) : unit = 
            writeDoc lineWidth fileName x.Body

        member x.Tell(doc:Doc) : WriterDoc = 
            WriterDoc(x.Body ^@@^ doc)

        member x.Nest(doc:WriterDoc) : WriterDoc = 
            WriterDoc(x.Body ^@@^ nest 4 (braces doc.Body))

        member x.PrefixNest (prefix:Doc) (doc:WriterDoc) : WriterDoc = 
            WriterDoc(x.Body ^@@^ prefix ^+^ nest 4 (braces doc.Body))

    let wempty : WriterDoc =  WriterDoc(empty)

    type Attribute =
        | Unquoted of string * string
        | Quoted of string * string
        member x.Body 
            with get() = 
                match x with 
                | Unquoted(name, attrVal) -> text name ^^ character '=' ^^ text attrVal
                | Quoted(name, attrVal) -> text name ^^ character '=' ^^  dquotes (text attrVal)


    let private attributeList (attrs:Attribute list) : Doc = 
        commaList <| List.map (fun (x:Attribute) -> x.Body) attrs


    let nodeDoc (nodeId:string) (attrs:Attribute list) : Doc =
        match attrs with
        | [] -> text nodeId
        |_ -> text nodeId ^^ attributeList attrs

    let edgeDoc (edgeOp:string) (nodeId1:string) (nodeId2:string) (attrs:Attribute list) : Doc =
        match attrs with
        | [] -> text nodeId1 ^+^ text edgeOp ^+^ text nodeId2
        |_ -> text nodeId1 ^+^ text edgeOp ^+^ text nodeId2 ^+^ attributeList attrs


    let edgesDoc (edgeOp:string) (nodeId1:string) (nodes:string list) (attrs:Attribute list) : Doc =
            let path = punctuateSpaced (text edgeOp) <| List.map text (nodeId1::nodes)
            match attrs with
            | [] -> path
            | _ -> path ^+^ attributeList attrs


    /// cat should be one of "graph", "node","edge"
    let attribStmtDoc (cat:string) (attrs:Attribute list) : Doc =
        text cat ^+^ attributeList attrs

    let lineCommentDoc (comment:string) : Doc =
        let lines = comment.Split [|'\n'|] |> Array.toList
        vcat <| List.map (text << sprintf "%% %s") lines

