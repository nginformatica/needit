/**
 * The MIT License (MIT)
 *
 * Copyright (c) 2015 NG Informática - TOTVS Software Partner
 * Author        Marcelo Camargo <marcelocamargo@linuxmail.org>
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

/**
 * Abstaction for ranges.
 * range ::= @{ <expr> [, <expr> ] .. <expr> } ;
 */
#xtranslate @{ <nStart> .. <nEnd> } => Z_Range( <nStart>, <nEnd> )
#xtranslate @{ <nStart>, <nNext> .. <nEnd> } => ;
	Z_StepRange( <nStart>, <nNext>, <nEnd> )
/**
 * Prelude functions are prefixed by Z_ to preserve the whole system and avoid
 * ambiguity.
 * prelude-func ::= Prelude Function <ident> ;
 */
#xtranslate List Function <cName> => Function Z_<cName>
/**
 * Prelude functions can receive from 1 to 3 arguments and we create a special
 * syntactic abstraction to handle this.
 * prelude-call ::= @<ident> { <expr> [, <expr> [, <expr> ] ] }
 */
#xtranslate @<cName> { } => Z_<cName>()
#xtranslate @<cName> { <arg1> } => Z_<cName>( <arg1> )
#xtranslate @<cName> { <arg1>, <arg2> } => Z_<cName>( <arg1>, <arg2> )
#xtranslate @<cName> { <arg1>, <arg2>, <arg3> } => ;
	Z_<cName>( <arg1>, <arg2>, <arg3> )
/**
 * Block application.
 * block-app ::= @<ident> <expr> ::= <expr>
 */
#xtranslate @<cName> <aList> ::= <bBlock> => Z_<cName>( <bBlock>, <aList> )
/**
 * Of operator - almost the same that ::=, but with reversed operands.
 * of-op ::= @<ident> <expr> Of <expr>
 */
#xtranslate @<cName> <expr> Of <aList> => Z_<cName>( <expr>, <aList> )
/**
 * Inverse function composition with application
 * \f -> \g -> \x -> g f x
 * func-comp ::= <ident> >>= <ident> In <expr>
 */
#xtranslate Do <cF> >>= <cG> In <expr> => <cF>( <cG>( <expr> ) )
/**
 * Array appending abstraction
 * array-append ::= <ident>[] := <expr>
 */
#xtranslate <cVar>\[\] := <expr> => aAdd( <cVar>, <expr> )
/**
 * Use DRY methodology by avoiding code to be repeated
 */
#xtranslate @BUILD ACCUMULATOR <cAcc> => Local <cAcc> := { }, nI
#xtranslate @BUILD FIXED ACCUMULATOR <cAcc>\< <nSize> \> => ;
	Local <cAcc> := Array( <nSize> ), nI

/**
 * Validate functions are prefixed by V_ to preserve the whole system and avoid
 * ambiguity.
 * validate-func ::= Validate Function <ident> ;
 */
#xtranslate Validate Function <cName> => Function V_<cName>
#xtranslate @Validate\<<cData>\> <xExpr> => V_<cData>( <xExpr> )

/**
 * Cast functions are prefixed by X_ to preserve the whole system and avoid
 * ambiguity.
 * cast-func ::= Cast Function <ident> ;
 */
#xtranslate Cast Function <cName> => Function X_<cName>
#xtranslate @\<<cData>\> <xExpr> => X_<cData>( <xExpr> )

/**
 * Syntactic sugar for lambda (or blocks, in this case)
 */
#xtranslate Fun -> <expr> => { || <expr> }
#xtranslate Fun ( <x> ) -> <expr> => { |<x>| <expr> }
#xtranslate Fun ( <x>, <y> ) -> <expr> => { |<x>, <y>| <expr> }
#xtranslate Fun ( <x>, <y>, <z> ) -> <expr> => { |<x>, <y>, <z>| <expr> }

/**
 * Just when not null.
 */
#xtranslate Just <ident>-><prop> Receives <expr> => ;
	If <expr> \<\> Nil ; <ident>-><prop> := <expr> ; EndIf

/**
 * Function application
 */
#xtranslate On <elem> <apply> <dt> => <apply>( <elem>, <dt> )

/**
 * Package definition
 * package ::= Package <ident> Where
 */
#xtranslate Package <(name)> (Version: <v>) Where => ;
   Static Package := <(name)>, _NVERSAO := <v>
/**
 * Assignment operator
 * assign ::= \<-
 */
#xtranslate \<- => :=
/**
 * Temporary bindings
 * let ::= Let <ident> \<- <expr> [, <ident> \<- <expr> ]
 */
#xtranslate Let => Local
/**
 * Extern (private) bindings
 * extern ::= Extern <ident> \<- <expr> [, <ident> \<- <expr> ]
 */
#xtranslate Extern => Private
/**
 * Lambda expressions, they act like functions.
 */
#xtranslate Lambda: <expr> => { || <expr> }
#xtranslate Lambda ( <x> ): <expr> => { |<x>| <expr> }
#xtranslate Lambda ( <x>, <y> ): <expr> => { |<x>, <y>| <expr> }
#xtranslate Lambda ( <x>, <y>, <z> ): <expr> => { |<x>, <y>, <z>| <expr> }
/**
 * Boolean abstraction
 * bool ::= True | False
 */
#xtranslate False => .F.
#xtranslate True => .T.
/**
 * Comparison operators
 */
#xtranslate Like => =
#xtranslate Is => ==
/**
 * Ternary operator
 */
#xtranslate If <expr> Then <t> Else <f> => IIf( <expr>, <t>, <f> )
/**
 * Boolean algebraic abstraction
 */
#xtranslate Or => .Or.
#xtranslate And => .And.
