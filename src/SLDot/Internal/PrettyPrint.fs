// Copyright (c) Stephen Tetley 2018
// License: BSD 3 Clause

// Acknowledgment: These are Christian Lindig's pretty printers 
// from the paper "Strictly Pretty" with extra combinators from 
// Daan Leijen's PPrint.
// Lindig's functions 'fits' and 'format' have been CPS converted 
// to work well in F#.


namespace SLDot.Internal


module PrettyPrint = 

    open System.Text
    open System

    type Doc = 
        | DocNil
        | DocCons of Doc * Doc
        | DocChar of char
        | DocText of string
        | DocNest of int * Doc
        | DocBreak of string
        | DocGroup of Doc



    type SDoc = 
        | SNil 
        | SText of string * SDoc
        | SLine of int * SDoc       // newline + spaces

    /// CPS / Tail recursive
    let sdocToString (source:SDoc) : string = 
        let sb = new StringBuilder ()
        let rec work (sdoc:SDoc) (cont : unit -> 'a) = 
            match sdoc with 
            | SNil -> cont ()
            | SText(s,d) -> 
                sb.Append(s) |> ignore
                work d cont
            | SLine(i,d) -> 
                let prefix = String.replicate i " " 
                sb.Append("\n" + prefix) |> ignore
                work d cont                
        work source (fun _ -> ())
        sb.ToString()

    let writeSDoc (path:string) (source:SDoc) : unit = 
        use sw = IO.File.CreateText(path)
        let rec work (sdoc:SDoc) (cont : unit -> 'a) = 
            match sdoc with 
            | SNil -> cont ()
            | SText(s,d) -> 
                sw.Write(s) |> ignore
                work d cont
            | SLine(i,d) -> 
                let indent = String.replicate i " " 
                sw.Write("\n" + indent) |> ignore
                work d cont                
        work source (fun _ -> ())

    type Mode = | Flat | Break

    type private Format1 = int * Mode * Doc

    let fits (width:int) (formats:Format1 list) : bool = 
        let rec work (w:int) (xs:Format1 list) (cont : bool -> bool) = 
            match xs with
            | _ when w < 0                  -> cont false  
            | []                            -> cont true
            | (_,_,DocNil)          :: zs   -> work w zs cont
            | (i,m,DocCons(x,y))    :: zs   -> work w ((i,m,x) :: (i,m,y) :: zs) cont
            | (i,m,DocNest(j,x))    :: zs   -> work w ((i+j,m,x) :: zs) cont
            | (_,_,DocText(s))      :: zs   -> work (w - s.Length) zs cont
            | (_,_,DocChar(c))      :: zs   -> work (w - 1) zs cont
            | (_,Flat,DocBreak(s))  :: zs   -> work (w - s.Length) zs cont
            | (_,Break,DocBreak(_)) :: _    -> cont true    // Impossible
            | (i,_,DocGroup(x))     :: zs   -> work w ((i,Flat,x) :: zs) cont
        work width formats (fun x -> x)


    let private format (width:int) (currentIndent:int) (formats:Format1 list) : SDoc = 
        let rec work (w:int) (k:int) (xs:Format1 list) (cont:SDoc -> SDoc) : SDoc = 
            match xs with
            | [] -> cont SNil
            | (_,_,DocNil)          :: zs -> work w k zs cont
            | (i,m,DocCons(x,y))    :: zs -> work w k ((i,m,x) :: (i,m,y) :: zs) cont
            | (i,m,DocNest(j,x))    :: zs -> work w k ((i+j,m,x) :: zs) cont
            | (_,_,DocText(s))      :: zs -> 
                work w (k + s.Length) zs (fun v1 -> cont (SText(s,v1)))

            | (_,_,DocChar(c))      :: zs -> 
                work w (k + 1) zs (fun v1 -> cont (SText(c.ToString(),v1)))

            | (_,Flat,DocBreak(s))  :: zs -> 
                work w (k + s.Length) zs (fun v1 -> cont (SText(s,v1)))

            | (i,Break,DocBreak(_)) :: zs -> 
                work w i zs (fun v1 -> cont (SLine(i,v1)))

            | (i,_,DocGroup(x))     :: zs -> 
                if fits (w - k) ((i,Flat,x) :: zs) then 
                    work w k ((i,Flat,x) :: zs) cont
                else
                    work w k ((i,Break,x) :: zs) cont
        work width currentIndent formats (fun x -> x)


    let render (lineWidth:int) (doc:Doc) : string = 
        format lineWidth 0 [(0,Flat,doc)] |> sdocToString

    let writeDoc (lineWidth:int) (path:string) (doc:Doc) : unit = 
        format lineWidth 0 [(0,Flat,doc)] |> writeSDoc path

    // ************************************************************************
    // Primitives

    
    let empty : Doc = DocNil

    let character (c:char) : Doc = DocChar c

    let text (s:string) : Doc = DocText s

    let line : Doc = text "\n"

    let spacebreak : Doc = DocBreak " "

    let linebreak : Doc = DocBreak "\n"

    let beside (x:Doc) (y:Doc) = DocCons (x,y)

    let breakWith (s:string) = DocBreak s

    let group (d:Doc) : Doc = DocGroup d

    let nest (i:int) (d:Doc) = DocNest (i, d)


    // ************************************************************************
    // Character printers

    /// Single left parenthesis: '('
    let lparen : Doc = character '('

    /// Single right parenthesis: ')'
    let rparen : Doc = character ')'

    /// Single left angle: '<'
    let langle : Doc = character '<'

    /// Single right angle: '>'
    let rangle : Doc = character '>'

    /// Single left brace: '{'
    let lbrace : Doc = character '{'
    
    /// Single right brace: '}'
    let rbrace : Doc= character '}'
    
    /// Single left square bracket: '['
    let lbracket : Doc = character '['
    
    /// Single right square bracket: ']'
    let rbracket : Doc = character ']'


    /// Single quote: '
    let squote : Doc= character '\''

    ///The document @dquote@ contains a double quote, '\"'.
    let dquote : Doc = character '"'

    /// The document @semi@ contains a semi colon, \";\".
    let semi : Doc = character ';'

    /// The document @colon@ contains a colon, \":\".
    let colon : Doc = character ':'

    /// The document @comma@ contains a comma, \",\".
    let comma : Doc = character ','

    /// The document @space@ contains a single space, \" \".
    let space : Doc = character ' '

    /// The document @dot@ contains a single dot, \".\".
    let dot : Doc = character '.'

    /// The document @backslash@ contains a back slash, \"\\\".
    let backslash : Doc = character '\\'

    /// The document @equals@ contains an equal sign, \"=\".
    let equals : Doc = character '='


    // ************************************************************************
    // Concatenation operators

    // Don't try to define (<>) - it is a reserved operator name in F#

    let (^^) (x:Doc) (y:Doc) = beside x y

    let besideSpace (x:Doc) (y:Doc) : Doc = x ^^ space ^^ y

    let (^+^) (x:Doc) (y:Doc) : Doc = besideSpace x y

    /// Concatenates d1 and d2 horizontally, with spaceBreak.
    let (^/^) (d1:Doc)  (d2:Doc) : Doc = 
        match d1,d2 with
        | DocNil, _ -> d1
        | _, DocNil -> d2
        |_, _       -> d1 ^^ spacebreak ^^ d2

    /// Concatenates d1 and d2 vertically, with optionally breaking space.
    let (^//^) (d1:Doc)  (d2:Doc) : Doc = 
        match d1,d2 with
        | DocNil, _ -> d1
        | _, DocNil -> d2
        |_, _    -> d1 ^^ linebreak ^^ d2


    /// Haskell / PPrint's: <$>
    let (^@^) (x:Doc) (y:Doc) : Doc = x ^^ line ^^ y


    let below (x:Doc) (y:Doc) : Doc = x ^^ linebreak ^^ y

    /// Haskell / PPrint's: <$$>
    let (^@@^) (x:Doc) (y:Doc) : Doc = below x y


    // ************************************************************************
    // List concatenation 

    let foldDocs f (docs:Doc list) : Doc = 
        match docs with
        | [] -> empty
        | (x::xs) -> List.fold f x xs

    let fillSep (docs:Doc list) : Doc = foldDocs (^/^) docs

    let hsep (docs:Doc list) : Doc = foldDocs (^+^) docs

    let vsep (docs:Doc list) : Doc = foldDocs (^@^) docs

    let hcat (docs:Doc list) : Doc = foldDocs (^^) docs

    let vcat (docs:Doc list) : Doc = foldDocs (^@@^) docs

    let cat (docs:Doc list) : Doc = group <| vcat docs

    let punctuate (sep:Doc) (docs:Doc list) : Doc = 
        let rec work acc ds = 
            match ds with
            | [] -> acc
            | (x :: xs) -> work (acc ^^ sep ^^ x) xs
        match docs with
        | [] -> empty
        | (x :: xs) -> work x xs


    let enclose (left:Doc) (right:Doc) (d:Doc) : Doc = left ^^ d ^^ right



    let encloseSep (left:Doc) (right:Doc) (sep:Doc) (docs:Doc list) : Doc = 
        left ^^ punctuate sep docs ^^ right
        


    /// Enclose in single quotes: '..'
    let squotes (d:Doc) : Doc = enclose squote squote d

    /// Enclose in double quotes: ".."
    let dquotes (d:Doc) : Doc = enclose dquote dquote d

    /// Enclose in braces: {..}
    let braces (d:Doc) : Doc = enclose lbrace rbrace d

    /// Enclose in parenthesis: (..)
    let parens (d:Doc) : Doc = enclose lparen rparen d

    /// Enclose in angles: <..>
    let angles (d:Doc) : Doc = enclose langle rangle d

    /// Enclose in brackets: [..]
    let brackets (d:Doc) : Doc = enclose lbracket rbracket d


    let commaList (docs:Doc list) : Doc = encloseSep lbracket rbracket comma docs

    let semiList (docs:Doc list) : Doc = encloseSep lbracket rbracket semi docs
