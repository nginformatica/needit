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

#include "syntax.ch"
#include "cast.ch"
#include "validate.ch"
#include "list.ch"
#include "lazy.ch"

/**
 * Prelude version
 */
#define PRELUDE_VERSION 1

Package Prelude(Version: 1) Where


	List Function Single( cProcess, xEnv, lExit )
		xEnv  <- If xEnv <> Nil Then xEnv Else GetEnvServer()
		lExit <- If lExit <> Nil Then lExit Else True

		Return StartJob( cProcess, xEnv, lExit )

	List Function Parallel( cProcess1, cProcess2, cProcess3, xEnv, lExit )
		Let aStack <- { }

		xEnv  <- If xEnv <> Nil Then xEnv Else GetEnvServer()
		lExit <- If lExit <> Nil Then lExit Else True

		If cProcess1 <> Nil
			On aStack aAdd StartJob( cProcess1, xEnv, lExit )
		EndIf
		If cProcess2 <> Nil
			On aStack aAdd StartJob( cProcess2, xEnv, lExit )
		EndIf
		If cProcess3 <> Nil
			On aStack aAdd StartJob( cProcess3, xEnv, lExit )
		EndIf

		Return aStack