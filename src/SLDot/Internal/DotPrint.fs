// Copyright (c) Stephen Tetley 2018
// License: BSD 3 Clause



namespace SLDot.Internal.DotPrint



[<AutoOpen>]
module DotPrint = 

    open SLDot.Internal.PrettyPrint

    let attr (name:string) (attrVal:string) = 
        text name ^^ character '=' ^^  text attrVal

    let quotedAttr (name:string) (attrVal:string) = 
        text name ^^ character '=' ^^  dquotes (text attrVal)
