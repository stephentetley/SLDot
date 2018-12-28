// Copyright (c) Stephen Tetley 2018
// License: BSD 3 Clause



namespace SLDot.Internal


module DotPrint = 

    open SLDot.Internal.PrettyPrint

    
    // Single case union
    [<Struct>]
    type WDoc = 
        | WDoc of Doc
        member internal x.Body with get() = match x with | WDoc(doc) -> doc

        member x.Render(lineWidth:int) : string = 
            render lineWidth x.Body

        member x.SaveAsDot(lineWidth:int, fileName:string) : unit = 
            writeDoc lineWidth fileName x.Body

        member x.Tell(doc:Doc) : WDoc = 
            WDoc(x.Body ^@@^ doc)

        member x.Nest(doc:WDoc) : WDoc = 
            WDoc(x.Body ^@@^ nest 4 (braces doc.Body))



    type Attribute =
        | Unquoted of string * string
        | Quoted of string * string
        member x.Body 
            with get() = 
                match x with 
                | Unquoted(name, attrVal) -> text name ^^ character '=' ^^ text attrVal
                | Quoted(name, attrVal) -> text name ^^ character '=' ^^  dquotes (text attrVal)

    //let makeBoolAttribute (name:string) (value:bool)  : Doc = 
    //    let value1 = if value then "true" else "false" in attr name value1

    //let color (value:string) : Doc            = attr "color" value

    //let fillcolor (value:string) : Attribute        = Unquoted ("fillcolor", value)
    //let fixedsize (value:bool) : Attribute          = makeBoolAttribute "fixedsize" value
    //let fontname (name:string) : Attribute          = Quoted ("fontname", name)
    //let fontsize (size:int) : Attribute             = Unquoted ("fontsize", size.ToString())
    //let height (h:float) : Attribute                = Unquoted ("height", sprintf "%.2f" h)
    //let label (value:string) : Attribute            = Quoted ("label", value)
    //let regular (value:bool) : Attribute            = makeBoolAttribute "regular" value
    //let shape (value:string) : Attribute            = Unquoted ("shape", value)
    //let sides (count:int) : Attribute               = Unquoted ("sides", count.ToString())
    //let style (values:string list) : Attribute      = Quoted ("style", String.concat "," values)
    //let width (w:float) : Attribute                 = Unquoted ("width", sprintf "%.2f" w)

    //let arrowhead (value:string) : Attribute        = Unquoted ("arrowhead", value)
    //let arrowsize (size:float) : Attribute          = Unquoted ("arrowsize", sprintf "%.2f" size)
    //let arrowtail (value:string) : Attribute        = Unquoted ("arrowtail", value)

