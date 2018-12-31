// Copyright (c) Stephen Tetley 2018
// License: BSD 3 Clause

// Acknowledgment: An implementation of are Daan Leijen's PPrint.
// The original Haskell library was strictified for Mercury by Ralph Beckett 
// and subsequently ported to Racket by David Herman
// The CPS transformation of layout is new, any mistakes are mine (SPT).


// At some point move to a dependency on SLPretty rather than copy its code...

namespace SLDot.Internal


module SLPretty =
    
    open System
    open System.Text
    
    
    type Doc = 
        private 
            | Nil
            | Cat of Doc * Doc
            | Nest of int * Doc
            | Text of string
            | Line of bool
            | Group of Doc
            | Column of (int -> Doc)
            | Nesting of (int -> Doc)   

    type SimpleDoc = 
        private 
            | SEmpty 
            | SText of string * SimpleDoc          
            | SLine of string * SimpleDoc     




    let extendString (s:string) (spaces:int) = s + String.replicate spaces " "


    let flatten (document:Doc) : Doc = 
        let rec work (doc:Doc) (cont : Doc -> Doc) : Doc = 
            match doc with
            | Cat(x,y) -> 
                work x (fun x1 -> work y (fun y1 -> cont (Cat(x1, y1))))
            | Nest(_,x) -> work x cont
            | Line(true) -> cont Nil
            | Line(false) -> cont (Text(" "))
            | Group(x) -> work x cont
            | Column(f) -> 
                cont (Column(fun i -> work (f i) id))           // Check!
            | Nesting(f) -> 
                cont (Nesting(fun i -> work (f i) id))          // Check!
            | _ -> cont doc
        work document (fun x -> x)


    let private isTooBig (text:string) (col:int) (width:int) : bool = 
        col + text.Length > width


    type Answer =  
        | ErrBacktrack
        | Okay of Doc

    let private layout (width:int) (doc:Doc) : SimpleDoc = 
        let rec best (col:int) (docs: list<string * Doc>) (alternate:bool) sk fk =
            match docs with
            | [] -> sk SEmpty
            | (_, Nil) :: rest ->
                best col rest alternate sk fk
            | (iz, Cat(x,y)) :: rest -> 
                best col ((iz,x) :: (iz,y) :: rest) alternate sk fk
            | (iz, Nest(n,x)) :: rest -> 
                best col ((extendString iz n,x) :: rest) alternate sk fk
            | (iz, Line _) :: rest ->
                best iz.Length rest alternate (fun v1 -> sk (SLine(iz,v1))) fk
                
            | (iz, Group(x)) :: rest ->
                best col ((iz, flatten x) :: rest) true (fun v1 -> sk v1) (fun _ -> 
                best col ((iz, x) :: rest) alternate sk fk)    
            | (iz, Text(t)) :: rest ->
                if (width >= 0) && alternate && isTooBig t col width then
                    fk ()
                else
                    best (col + t.Length) rest alternate (fun v1 -> sk (SText(t,v1))) fk
            | (iz, Column(f)) :: rest ->
                best col ((iz, f col) :: rest) alternate sk fk
            | (iz, Nesting(f)) :: rest ->
                best col ((iz, f iz.Length) :: rest) alternate sk fk
        best 0 [("",doc)] false id (fun () -> SEmpty)

    let prettyPrint (doc:Doc) (width:int) : string = 
        let sb = StringBuilder ()
        let rec work (sdoc:SimpleDoc) (cont:unit -> unit) : unit = 
            match sdoc with
            | SEmpty -> cont ()
            | SText(t,rest) -> 
                ignore <| sb.Append(t)
                work rest cont
            | SLine(x,rest) -> 
                ignore <| sb.Append('\n')
                ignore <| sb.Append(x)
                work rest cont

        work (layout width doc) (fun _ -> ())
        sb.ToString()

    /// prettyPrint with arg order reversed
    let render (width:int) (doc:Doc)  : string = prettyPrint doc width

    let writeDoc  (width:int) (fileName:string) (doc:Doc) : unit = 
        use sw = IO.File.CreateText(fileName)
        let rec work (sdoc:SimpleDoc) (cont:unit -> unit) : unit = 
            match sdoc with
            | SEmpty -> cont ()
            | SText(t,rest) -> 
                ignore <| sw.Write(t)
                work rest cont
            | SLine(x,rest) -> 
                ignore <| sw.Write('\n')
                ignore <| sw.Write(x)
                work rest cont

        work (layout width doc) (fun _ -> ())

    // ************************************************************************
    // Primitive printers   

    let empty : Doc = Nil
    
    let nest (i:int) (d:Doc) : Doc = Nest (i,d)
    
    let text (s:string) : Doc = Text s 

    let column (f:int -> Doc) : Doc = Column(f)

    let nesting (f:int -> Doc) : Doc = Nesting(f)

    let group (d:Doc) : Doc = Group(d)

    let line : Doc = Line false
    
    let linebreak : Doc = Line true

    let character (ch:char) : Doc = 
        match ch with
        | '\n' -> line 
        | _ -> text <| ch.ToString()


    let softline : Doc = Group line

    let softbreak : Doc = Group linebreak

    
    

    

    let beside (x:Doc) (y:Doc) : Doc = Cat(x,y)

    // Don't try to define (<>) - it is a reserved operator name in F#


    // aka beside
    let (^^) (x:Doc) (y:Doc) = beside x y

    let besideSpace (x:Doc) (y:Doc) : Doc = x ^^ character ' ' ^^ y

    let (^+^) (x:Doc) (y:Doc) : Doc = besideSpace x y

    let (^@^) (x:Doc) (y:Doc) : Doc = x ^^ line ^^ y
    let (^@@^) (x:Doc) (y:Doc) : Doc = x ^^ linebreak ^^ y

    let (^/^) (x:Doc) (y:Doc) : Doc = x ^^ softline ^^ y
    let (^//^) (x:Doc) (y:Doc) : Doc = x ^^ softbreak ^^ y

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


    let spaces (i:int) : Doc = text <| String.replicate i " "

    
    let enclose (l:Doc) (r:Doc) (body:Doc)   = l ^^ body ^^ r

    let squotes (x:Doc) : Doc = enclose squote squote x
    let dquotes (x:Doc) : Doc = enclose dquote dquote x
    let braces (x:Doc) : Doc = enclose lbrace rbrace x
    let parens (x:Doc) : Doc = enclose lparen rparen x
    let angles (x:Doc) : Doc = enclose langle rangle x
    let brackets (x:Doc) : Doc = enclose lbracket rbracket x


    // ************************************************************************
    // List concatenation 

    let foldDocs (op:Doc -> Doc -> Doc) (docs:Doc list) : Doc = 
        match docs with
        | [] -> empty
        | (x::xs) -> List.fold op x xs

    let punctuate (sep:Doc) (docs:Doc list) : Doc =
        let rec work acc ds =
            match ds with
            | [] -> acc
            | (x :: xs) -> work (Cat(acc, Cat(sep,x))) xs
        match docs with
        | [] -> empty
        | (x :: xs) -> work x xs

    
    let encloseSep (l:Doc) (r:Doc) (sep:Doc) (ds:Doc list) : Doc = 
        let rec work (acc:Doc) (docs:Doc list) (cont:Doc -> Doc) = 
            match docs with
            | [] -> cont acc
            | [x] -> cont (acc ^^ x)
            | x :: xs -> 
                work (acc ^^ x ^^ sep) xs cont
        work l ds (fun d -> d ^^ r)


    let commaList            = encloseSep lbracket rbracket comma
    let semiList            = encloseSep lbracket rbracket semi
    let tupled          = encloseSep lparen   rparen  comma
    let semiBraces      = encloseSep lbrace   rbrace  semi


    let hcat (docs:Doc list) : Doc = foldDocs beside docs

    let hcatSpace (docs:Doc list) : Doc = punctuate space docs

    let vcat (docs:Doc list) : Doc = punctuate line docs

    let vcatSoft (docs:Doc list) : Doc = punctuate softline docs

    let vcatSoftBreak (docs:Doc list) : Doc = punctuate softbreak docs


    let width d f = 
        column (fun k1 -> d ^^ column (fun k2 -> f (k2 - k1)) )

    let align (d:Doc) = 
        column (fun k -> nesting (fun i -> nest (k - i) d))

    let hang (i:int) (d:Doc) : Doc = align (nest i d)

    let indent (i:int) (d:Doc) : Doc = 
        hang i (spaces i ^^ d)

    let fill f d = 
        width d (fun w -> if w >= f then empty else spaces (f - w))

    let fillBreak f d = 
        width d (fun w -> if w > f then nest f linebreak else spaces (f - w))


