{ This file is part of the caLibrary (for Delphi 7) package

  Copyright (C) 1999-2017 - Carl Caulkett - carl.caulkett@gmail.com

  MODIFIED LGPL Licence - this is the same licence as that used by the Free Pascal Compiler (FPC)
  A copy of the full licence can be found in the file Licence.md in the same folder as this file.

  This library is free software; you can redistribute it and/or modify it under the terms of the GNU Library General Public
  License as published by the Free Software Foundation; either version 2 of the License, or (at your option) any later version
  with the following modification:

  As a special exception, the copyright holders of this library give you permission to link this library with independent
  modules to produce an executable, regardless of the license terms of these independent modules, and to copy and distribute the
  resulting executable under terms of your choice, provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a module which is not derived from or based on this
  library. If you modify this library, you may extend this exception to your version of the library, but you are not obligated
  to do so. If you do not wish to do so, delete this exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License for more details.

  You should have received a copy of the GNU Library General Public License along with this library; if not, write to the Free
  Software Foundation, Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.
}

//-----------------------------------------------------------------------------
// Optional compiler directives                                                
//-----------------------------------------------------------------------------

{$ASSERTIONS OFF}
{$DEBUGINFO ON}
{$LOCALSYMBOLS ON}
{$STACKFRAMES OFF}
{$DEFINITIONINFO OFF}

{$IFDEF VER130}
  {$DEFINE D5}
  {$DEFINE D5_UP}
{$ENDIF}

{$IFDEF VER140}
  {$DEFINE D5_UP}
  {$DEFINE D6}
  {$DEFINE D6_UP}
{$ENDIF}

{$IFDEF VER150}
  {$DEFINE D5_UP}
  {$DEFINE D6_UP}
  {$DEFINE D7}
  {$DEFINE D7_UP}
{$ENDIF}

{$IFDEF VER180}
  {$DEFINE D5_UP}
  {$DEFINE D6_UP}
  {$DEFINE D7_UP}
  {$DEFINE D2007}    
{$ENDIF}

{$IFDEF D6_UP}
  {$WARN SYMBOL_PLATFORM OFF}
  {$WARN UNIT_PLATFORM OFF}
  {$WARN SYMBOL_DEPRECATED OFF}
{$ENDIF}

// $C - Assertions
// $D - Debug info
// $L - Local information
// $W - Stack frames
// $Y - Symbol declaration

//-----------------------------------------------------------------------------
// Compiler directives that do not need to be changed                          
//-----------------------------------------------------------------------------

{$A+,B-,E-,F-,G+,H+,I+,J+,K-,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,X+,Z1}

// $A - Align
// $B - Full boolean evaluation
// $E - Executable extension
// $F - ### Unknown
// $G - Imported data
// $H - Long strings
// $I - I/O checks
// $J - Writable typed constants
// $K - ### Unknown
// $M - RTTI
// $N - ### Unknown
// $O - Optimization
// $P - Open strings parameters
// $Q - Overflow checking
// $R - Range checking
// $S - ### Unknown
// $T - Type checked pointers
// $U - Pentium safe FDIV
// $V - Var string checking
// $X - Extended syntax
// $Z - Minimum enumeration size


{$MINSTACKSIZE $00004000}

{$MAXSTACKSIZE $00100000}

{$IMAGEBASE $00400000}

{.$DEFINE CHECK_387_CONTROL}

{$DEFINE FIX_387_CONTROL}

{$DEFINE CLEAR_387_STATUS}

{.$DEFINE USE_DELPHI_MATH_POWER}

{.$DEFINE USE_DELPHI_INT_POWER}

{$DEFINE USE_DB}

{.$DEFINE USE_FAST_MATH}

{$DEFINE USE_SAFE_ROUND}






