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

Package Validate(Version: 1) Where

	/**
	 * Filters and validates against SQL Injection
	 * @param String
	 * @return String
	 * @author Marcelo Camargo
	 */
	Validate Function SQL( cEntry )
		Return StrTran( StrTran( StrTran( cEntry, "'" ), '"' ), "--" )

	/**
	 * Validates a brazilian CEP
	 * @param String
	 * @return Bool
	 * @author Marcelo Camargo
	 */
	Validate Function CEP( cCEP )
		Let aCEP   <- @Explode { cCEP }
		Let fValid <- ( Lambda ( Elem, Index ): ;
			If Index Is 6 Then True Else IsDigit( Elem ) )

		If Len( aCEP ) <> 9 Or @ElemIndex { "-", aCEP } <> 6 ;
		   Or !@AndList { @MapIndex { fValid, aCEP } }

			Return False
		EndIf
		Return True

	/**
	 * Validates a brazilian CNPJ.
	 * @param String
	 * @return Bool
	 * @author Marcelo Camargo
	 */
	Validate Function CNPJ( cCNPJ )
		Let aFstCalc    <- { 5, 4, 3, 2, 9, 8, 7, 6, 5, 4, 3, 2 } ;
		  , aSndCalc    <- { 6 , 5, 4, 3, 2, 9, 8, 7, 6, 5, 4, 3, 2 } ;
		  , aCNPJ       <- @Explode { cCNPJ }  ;
		  , aCNPJDigits <- @Take { 12, aCNPJ } ;
		  , aFstZipped  <- { } ;
		  , nSumValues  <- 0   ;
		  , nRem

		aFstZipped <- @ZipWith { ( Lambda ( X, Y ): ;
			X * Y ;
		), aFstCalc, @Map { ( Lambda (Digit): Val( Digit ) ), aCNPJDigits } }

		nSumValues <- @Sum { aFstZipped }
		nRem       <- Int( nSumValues % 11 )

		If Val( aCNPJ[13] ) <> If nRem < 2 Then 0 Else ( 11 - nRem )
			Return False
		EndIf

		aSndZipped <- @ZipWith { ( Lambda ( X, Y ): ;
			X * Y ;
		), aSndCalc, @Take { 13, @Map { ;
		                            ( Lambda ( Digit ): Val( Digit ) ) ;
		                         , aCNPJ } } }

		nSumValues <- @Sum { aSndZipped }
		nRem       <- Int( nSumValues % 11 )

		If Val( aCNPJ[14] ) <> If nRem < 2 Then 0 Else ( 11 - nRem )
			Return False
		EndIf
		Return True

	/**
	 * Validates a brazilian CPF.
	 * @param String
	 * @return Bool
	 * @author Marcelo Camargo
	 */
	Validate Function CPF( cCPF )
		Let aFstCalc     <- @Reverse { @{ 2 .. 11 } } ;
		  , aCPF       <- @Explode { cCPF }         ;
		  , aCPFDigits <- @Take { 10, aCPF }        ;
		  , aFstZipped <- aSndZipped <- { }         ;
		  , nSumValues <- 0                         ;
		  , nRem

		If Len( aFstCalc ) <> Len( aCPFDigits )
			Return False
		EndIf

		aFstZipped <- @ZipWith { ( Lambda ( X, Y ): ;
			X * Y ;
		), @Tail { aFstCalc }, @Map { ( Lambda (Digit): ;
		                                   Val( Digit ) ;
		                              ), @Initial { aCPFDigits } } }

		nSumValues <- @Sum { aFstZipped }
		nRem       <- Int( nSumValues % 11 )

		If Val( aCPF[10] ) <> If nRem < 2 Then 0 Else ( 11 - nRem )
			Return False
		EndIf

		aSndZipped <- @ZipWith { ( Lambda ( X, Y ): ;
			X * Y ;
		), aFstCalc, @Map { ( Lambda (Digit): ;
		                         Val( Digit ) ;
		                    ), aCPFDigits } }

		nSumValues <- @Sum { aSndZipped }
		nRem       <- Int( nSumValues % 11 )

		If Val( aCPF[11] ) <> If nRem < 2 Then 0 Else ( 11 - nRem )
			Return False
		EndIf
		Return True

	/**
	 * Validates an e-mail.
	 * @param String
	 * @return Bool
	 * @author Marcelo Camargo
	 */
	Validate Function Email( cEmail )
		Let aEmail        <- @Explode { cEmail } ;
		  , nIndexOfAt  <- @ElemIndex "@" Of aEmail ;
		  , aEmailName   ;
		  , aEmailDomain ;
		  , aDomainParts ;
		  , aIndicesOfAt

		Let bIsValid <- ;
			Lambda ( Char ): Char $ "abcdefghijklmnopqrstuvwxyz.0123456789_-"


		If nIndexOfAt Is Nil
			Return False
		EndIf

		aEmailName   <- @Slice { 1, nIndexOfAt - 1, aEmail }
		aEmailDomain <- @Slice { nIndexOfAt + 1 , Len( aEmail ), aEmail }
		aDomainParts <- @Split { ".", aEmailDomain }
		aIndicesOfAt <- @ElemIndices "@" Of aEmail

		// @SupressWarnings
		@Id { aEmail }
		@Id { nIndexOfAt }

		If Len( aIndicesOfAt ) <> 1 ;
			Or ( Len( aEmailName ) < 1 Or Len( aEmailDomain ) < 3 ) ;
			Or @ElemIndex { ".", aEmailDomain } Is Nil ;
			Or !@AndList { @Map { bIsValid, aEmailName } } ;
			Or !@AndList { @Map { bIsValid, aEmailDomain } } ;
			Or Len( aDomainParts[ Len( aDomainParts) ] ) < 2

			Return False
		EndIf
		Return True

	/**
	 * Validates an even number.
	 * @param Number
	 * @return Bool
	 * @author Marcelo Camargo
	 */
	Validate Function Even( nNum )
		Return nNum % 2 Is 0

	/**
	 * Validates a single name.
	 * @param String
	 * @return Bool
	 * @author Marcelo Camargo
	 */
	Validate Function Name( cName )
		Let aName <- @Explode { cName } ;
		  , nI
		For nI <- 1 To Len( aName )
			If !IsAlpha( aName[ nI ] ) And aName[ nI ] <> " "
				Return False
			EndIf
		Next nI
		Return True

	/**
	 * Validates a negative number.
	 * @param Number
	 * @return Bool
	 * @author Marcelo Camargo
	 */
	Validate Function Negative( nNum )
		Return nNum < 0

	/**
	 * Validates if a value is numeric.
	 * @param String
	 * @return Bool
	 * @author Marcelo Camargo
	 */
	Validate Function Number( cVal )
		Return IsDigit( cVal )

	/**
	 * Validates an odd number.
	 * @param Number
	 * @return Bool
	 * @author Marcelo Camargo
	 */
	Validate Function Odd( nNum )
		Return nNum % 2 <> 0

	/**
	 * Validates a positive number.
	 * @param Number
	 * @return Bool
	 * @author Marcelo Camargo
	 */
	Validate Function Positive( nNum )
		Return nNum > 0
