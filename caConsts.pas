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


unit caConsts;

{$INCLUDE ca.inc}

interface

uses

  // Standard Delphi units 
  Windows,
  Messages,
  Controls,
  Graphics,

  // ca units 
  caTypes;

const

  cNullUniqueID: TcaUniqueID = '--------------------------------';

const

  cLoopUntilBreak         = True;

  cAPIBufferLength        = 254;
  cCRLF                   = #13#10;
  cDefaultTokenDelimiters = #32 + ';:()=.,[]-{}{<>*+/!?"' + #39 + #34 + #9 + #13 + #10 + #0;
  cEvalFunctions          = ' cos exp int sin frac round trunc tan ln log10 sqrt ';
  cInvalidPtr             = 2;
  cKernel                 = 'kernel32.dll';
  cMatrixMagic            = 'caMatrix';
  cSingleQuote            = '''';
  cDoubleQuote            = '"';
  cSingleQuoteEsc = #34#34;
  cDoubleQuoteEsc = #39#39;
  cPoint                  = '.';
  cVisible                = 'Visible';
  cTrue                   = 'True';
  cFalse                  = 'False';
  cText                   = 'Text';
  cID                     = 'ID';
  cOriginalID             = 'OriginalID';
  cNewID                  = 'NewID';
  cFormatIntComma         = '#,##';
  cBlank                  = '';
  cGrid                   = 'Grid';
  cChart                  = 'Chart';
  cWidth                  = 'Width';
  cChecked                = 'Checked';
  cSelected               = 'Selected';
  cName                   = 'Name';
  cPath                   = 'Path';
  cConfig                 = 'Config';
  cColon                  = ':';
  cSemiColon              = ';';
  cTab                    = #9;
  cXML                    = 'xml';
  cXMLExt                 = cPoint + cXML;
  cHash                   = '#';
  cLanguages              = 'Languages';
  cDictionaryFile         = 'Dictionary.txt';
  cUndefined              = '<undefined>';
  cFontDotName            = 'Font.Name';
  cNode                   = 'Node';
  cNodes                  = 'Nodes';
  cData                   = 'Data';
  cComma                  = ',';
  cPipe                   = '|';
  cHeight                 = 'Height';
  cOK                     = 'OK';
  cNoDelphi5              = 'Cannot run under Delphi 5';
  cMyComputer             = 'rfMyComputer';
  cLeft                   = 'Left';
  cTop                    = 'Top';
  cIsMaximized            = 'IsMaximized';

  // Escape codes 
  cESCn                   = #10;
  cESCr                   = #13;
  cESCt                   = #9;

  // General Math constants 

  // cDefaultZeroTolerance   = 0.00001; 
  cDefaultZeroTolerance   = 1E-20;
  cHundred                = 100;
  cThousand               = 1000;
  cMillion                = 1000000;
  cOneOverTen             = 0.1;
  cOneOverHundred         = 0.01;
  cOneOverThousand        = 0.001;
  cOneOverMillion         = 0.000001;
  cHalf                   = 0.5;
  cZero                   = 0;
  cOne                    = 1;

  // Replacements for constants defined in Math.pas 
  // Note that Math.pas defines the MinXXXX constants 
  // as the SMALLEST POSITIVE VALUE that can reliably 
  // stored by a variable of the type in question. These 
  // constants use the more useful meaning of the LARGEST 
  // NEGATIVE value. 

  cMin                    = 'Min';
  cMax                    = 'Max';
  cMinSingle              = -3.4e+38;
  cMaxSingle              = 3.4e+38;
  cMinDouble              = -1.7e+308;
  cMaxDouble              = 1.7e+308;
  cMinExtended            = -1.1e+4932;
  cMaxExtended            = 1.1e+4932;
  cMaxInt                 = MaxInt;
  cMinInt                 = -cMaxInt - 1;

  cGoldenRatio            = 1.618033989;

  // Constants used by IcaFastMath 

  cLn2x                   = 0.693147181;

  cIntSquares: array[0..999] of Integer =
    (0, 1, 4, 9, 16, 25, 36, 49, 64, 81,
     100, 121, 144, 169, 196, 225, 256, 289, 324, 361,
     400, 441, 484, 529, 576, 625, 676, 729, 784, 841,
     900, 961, 1024, 1089, 1156, 1225, 1296, 1369, 1444, 1521,
     1600, 1681, 1764, 1849, 1936, 2025, 2116, 2209, 2304, 2401,
     2500, 2601, 2704, 2809, 2916, 3025, 3136, 3249, 3364, 3481,
     3600, 3721, 3844, 3969, 4096, 4225, 4356, 4489, 4624, 4761,
     4900, 5041, 5184, 5329, 5476, 5625, 5776, 5929, 6084, 6241,
     6400, 6561, 6724, 6889, 7056, 7225, 7396, 7569, 7744, 7921,
     8100, 8281, 8464, 8649, 8836, 9025, 9216, 9409, 9604, 9801,
     10000, 10201, 10404, 10609, 10816, 11025, 11236, 11449, 11664, 11881,
     12100, 12321, 12544, 12769, 12996, 13225, 13456, 13689, 13924, 14161,
     14400, 14641, 14884, 15129, 15376, 15625, 15876, 16129, 16384, 16641,
     16900, 17161, 17424, 17689, 17956, 18225, 18496, 18769, 19044, 19321,
     19600, 19881, 20164, 20449, 20736, 21025, 21316, 21609, 21904, 22201,
     22500, 22801, 23104, 23409, 23716, 24025, 24336, 24649, 24964, 25281,
     25600, 25921, 26244, 26569, 26896, 27225, 27556, 27889, 28224, 28561,
     28900, 29241, 29584, 29929, 30276, 30625, 30976, 31329, 31684, 32041,
     32400, 32761, 33124, 33489, 33856, 34225, 34596, 34969, 35344, 35721,
     36100, 36481, 36864, 37249, 37636, 38025, 38416, 38809, 39204, 39601,
     40000, 40401, 40804, 41209, 41616, 42025, 42436, 42849, 43264, 43681,
     44100, 44521, 44944, 45369, 45796, 46225, 46656, 47089, 47524, 47961,
     48400, 48841, 49284, 49729, 50176, 50625, 51076, 51529, 51984, 52441,
     52900, 53361, 53824, 54289, 54756, 55225, 55696, 56169, 56644, 57121,
     57600, 58081, 58564, 59049, 59536, 60025, 60516, 61009, 61504, 62001,
     62500, 63001, 63504, 64009, 64516, 65025, 65536, 66049, 66564, 67081,
     67600, 68121, 68644, 69169, 69696, 70225, 70756, 71289, 71824, 72361,
     72900, 73441, 73984, 74529, 75076, 75625, 76176, 76729, 77284, 77841,
     78400, 78961, 79524, 80089, 80656, 81225, 81796, 82369, 82944, 83521,
     84100, 84681, 85264, 85849, 86436, 87025, 87616, 88209, 88804, 89401,
     90000, 90601, 91204, 91809, 92416, 93025, 93636, 94249, 94864, 95481,
     96100, 96721, 97344, 97969, 98596, 99225, 99856, 100489, 101124, 101761,
     102400, 103041, 103684, 104329, 104976, 105625, 106276, 106929, 107584, 108241,
     108900, 109561, 110224, 110889, 111556, 112225, 112896, 113569, 114244, 114921,
     115600, 116281, 116964, 117649, 118336, 119025, 119716, 120409, 121104, 121801,
     122500, 123201, 123904, 124609, 125316, 126025, 126736, 127449, 128164, 128881,
     129600, 130321, 131044, 131769, 132496, 133225, 133956, 134689, 135424, 136161,
     136900, 137641, 138384, 139129, 139876, 140625, 141376, 142129, 142884, 143641,
     144400, 145161, 145924, 146689, 147456, 148225, 148996, 149769, 150544, 151321,
     152100, 152881, 153664, 154449, 155236, 156025, 156816, 157609, 158404, 159201,
     160000, 160801, 161604, 162409, 163216, 164025, 164836, 165649, 166464, 167281,
     168100, 168921, 169744, 170569, 171396, 172225, 173056, 173889, 174724, 175561,
     176400, 177241, 178084, 178929, 179776, 180625, 181476, 182329, 183184, 184041,
     184900, 185761, 186624, 187489, 188356, 189225, 190096, 190969, 191844, 192721,
     193600, 194481, 195364, 196249, 197136, 198025, 198916, 199809, 200704, 201601,
     202500, 203401, 204304, 205209, 206116, 207025, 207936, 208849, 209764, 210681,
     211600, 212521, 213444, 214369, 215296, 216225, 217156, 218089, 219024, 219961,
     220900, 221841, 222784, 223729, 224676, 225625, 226576, 227529, 228484, 229441,
     230400, 231361, 232324, 233289, 234256, 235225, 236196, 237169, 238144, 239121,
     240100, 241081, 242064, 243049, 244036, 245025, 246016, 247009, 248004, 249001,
     250000, 251001, 252004, 253009, 254016, 255025, 256036, 257049, 258064, 259081,
     260100, 261121, 262144, 263169, 264196, 265225, 266256, 267289, 268324, 269361,
     270400, 271441, 272484, 273529, 274576, 275625, 276676, 277729, 278784, 279841,
     280900, 281961, 283024, 284089, 285156, 286225, 287296, 288369, 289444, 290521,
     291600, 292681, 293764, 294849, 295936, 297025, 298116, 299209, 300304, 301401,
     302500, 303601, 304704, 305809, 306916, 308025, 309136, 310249, 311364, 312481,
     313600, 314721, 315844, 316969, 318096, 319225, 320356, 321489, 322624, 323761,
     324900, 326041, 327184, 328329, 329476, 330625, 331776, 332929, 334084, 335241,
     336400, 337561, 338724, 339889, 341056, 342225, 343396, 344569, 345744, 346921,
     348100, 349281, 350464, 351649, 352836, 354025, 355216, 356409, 357604, 358801,
     360000, 361201, 362404, 363609, 364816, 366025, 367236, 368449, 369664, 370881,
     372100, 373321, 374544, 375769, 376996, 378225, 379456, 380689, 381924, 383161,
     384400, 385641, 386884, 388129, 389376, 390625, 391876, 393129, 394384, 395641,
     396900, 398161, 399424, 400689, 401956, 403225, 404496, 405769, 407044, 408321,
     409600, 410881, 412164, 413449, 414736, 416025, 417316, 418609, 419904, 421201,
     422500, 423801, 425104, 426409, 427716, 429025, 430336, 431649, 432964, 434281,
     435600, 436921, 438244, 439569, 440896, 442225, 443556, 444889, 446224, 447561,
     448900, 450241, 451584, 452929, 454276, 455625, 456976, 458329, 459684, 461041,
     462400, 463761, 465124, 466489, 467856, 469225, 470596, 471969, 473344, 474721,
     476100, 477481, 478864, 480249, 481636, 483025, 484416, 485809, 487204, 488601,
     490000, 491401, 492804, 494209, 495616, 497025, 498436, 499849, 501264, 502681,
     504100, 505521, 506944, 508369, 509796, 511225, 512656, 514089, 515524, 516961,
     518400, 519841, 521284, 522729, 524176, 525625, 527076, 528529, 529984, 531441,
     532900, 534361, 535824, 537289, 538756, 540225, 541696, 543169, 544644, 546121,
     547600, 549081, 550564, 552049, 553536, 555025, 556516, 558009, 559504, 561001,
     562500, 564001, 565504, 567009, 568516, 570025, 571536, 573049, 574564, 576081,
     577600, 579121, 580644, 582169, 583696, 585225, 586756, 588289, 589824, 591361,
     592900, 594441, 595984, 597529, 599076, 600625, 602176, 603729, 605284, 606841,
     608400, 609961, 611524, 613089, 614656, 616225, 617796, 619369, 620944, 622521,
     624100, 625681, 627264, 628849, 630436, 632025, 633616, 635209, 636804, 638401,
     640000, 641601, 643204, 644809, 646416, 648025, 649636, 651249, 652864, 654481,
     656100, 657721, 659344, 660969, 662596, 664225, 665856, 667489, 669124, 670761,
     672400, 674041, 675684, 677329, 678976, 680625, 682276, 683929, 685584, 687241,
     688900, 690561, 692224, 693889, 695556, 697225, 698896, 700569, 702244, 703921,
     705600, 707281, 708964, 710649, 712336, 714025, 715716, 717409, 719104, 720801,
     722500, 724201, 725904, 727609, 729316, 731025, 732736, 734449, 736164, 737881,
     739600, 741321, 743044, 744769, 746496, 748225, 749956, 751689, 753424, 755161,
     756900, 758641, 760384, 762129, 763876, 765625, 767376, 769129, 770884, 772641,
     774400, 776161, 777924, 779689, 781456, 783225, 784996, 786769, 788544, 790321,
     792100, 793881, 795664, 797449, 799236, 801025, 802816, 804609, 806404, 808201,
     810000, 811801, 813604, 815409, 817216, 819025, 820836, 822649, 824464, 826281,
     828100, 829921, 831744, 833569, 835396, 837225, 839056, 840889, 842724, 844561,
     846400, 848241, 850084, 851929, 853776, 855625, 857476, 859329, 861184, 863041,
     864900, 866761, 868624, 870489, 872356, 874225, 876096, 877969, 879844, 881721,
     883600, 885481, 887364, 889249, 891136, 893025, 894916, 896809, 898704, 900601,
     902500, 904401, 906304, 908209, 910116, 912025, 913936, 915849, 917764, 919681,
     921600, 923521, 925444, 927369, 929296, 931225, 933156, 935089, 937024, 938961,
     940900, 942841, 944784, 946729, 948676, 950625, 952576, 954529, 956484, 958441,
     960400, 962361, 964324, 966289, 968256, 970225, 972196, 974169, 976144, 978121,
     980100, 982081, 984064, 986049, 988036, 990025, 992016, 994009, 996004, 998001);

  // Numbers - main keyboard 
  VK_0 = $30;
  VK_1 = $31;
  VK_2 = $32;
  VK_3 = $33;
  VK_4 = $34;
  VK_5 = $35;
  VK_6 = $36;
  VK_7 = $37;
  VK_8 = $38;
  VK_9 = $39;

  // Numbers - numeric keypad 
  VK_N0 = $60;
  VK_N1 = $61;
  VK_N2 = $62;
  VK_N3 = $63;
  VK_N4 = $64;
  VK_N5 = $65;
  VK_N6 = $66;
  VK_N7 = $67;
  VK_N8 = $68;
  VK_N9 = $69;

  VK_EQUAL = $BB;
  VK_COMMA = $BC;
  VK_PERIOD = $BE;
  VK_SLASH = $BF;
  VK_BACKSLASH = $DC;
  VK_HASH = $DE;

  // Alpha keys - main keyboard 
  VK_A = $41;
  VK_B = $42;
  VK_C = $43;
  VK_D = $44;
  VK_E = $45;
  VK_F = $46;
  VK_G = $47;
  VK_H = $48;
  VK_I = $49;
  VK_J = $4A;
  VK_K = $4B;
  VK_L = $4C;
  VK_M = $4D;
  VK_N = $4E;
  VK_O = $4F;
  VK_P = $50;
  VK_Q = $51;
  VK_R = $52;
  VK_S = $53;
  VK_T = $54;
  VK_U = $55;
  VK_V = $56;
  VK_W = $57;
  VK_X = $58;
  VK_Y = $59;
  VK_Z = $5A;

  VK_NONE = $FF;

  // Component messages 
  CAM_SHOWPOPUP = CB_SHOWDROPDOWN;

  CAM_BASE = WM_USER;

  CAM_BUTTONPRESSED = CAM_BASE + 1;

  CAM_REQUESTHIDE = CAM_BASE + 2;

  CAM_GLYPHLISTCHANGED = CAM_BASE + 3;

  CAM_REQUESTPAINT = CAM_BASE + 4;

  // Other constants 
  CA_NEVER = False;

  cShowWindowValues: array [TcaShowWindow] of Integer =
                     (SW_HIDE, SW_MAXIMIZE, SW_MINIMIZE, SW_RESTORE, SW_SHOW,
                      SW_SHOWDEFAULT, SW_SHOWMAXIMIZED, SW_SHOWMINIMIZED,
                      SW_SHOWMINNOACTIVE, SW_SHOWNA, SW_SHOWNOACTIVATE,
                      SW_SHOWNORMAL);

  // Additional color constants 
  clCream           = TColor($A6CAF0);
  clLedgerGreen     = TColor($D8F0D8);
  clLighRed         = TColor($EA7BDF);
  clLightBlue       = TColor($FEC041);
  clLighterBlue     = TColor($F4DB82);
  clLighterGreen    = TColor($DBEABD);
  clLighterRed      = TColor($F499F7);
  clLighterYellow   = TColor($AEF9F7);
  clLightGreen      = TColor($B5DB8A);
  clLightYellow     = TColor($65E6FC);
  clLtSilver        = TColor($D0D0D0);
  clMidGray         = TColor($A4A0A0);
  clMoneyGreen      = TColor($C0DCC0);
  clVeryLtSilver    = TColor($E0E0E0);

  //-------------------------------------------------------------------------
  // GretagMacbeth color constants                                           
  //                                                                         
  // Name                  $bbggrr        R    G   B       H      S      V   
  //-----------------      -------       ---  --- ---    -----  -----  ----- 
  clgmDarkSkin           = $526081;  //  129   96  82     17.9  0.364  0.506 
  clgmLightSkin          = $90A3CD;  //  205  163 144     18.7  0.298  0.804 
  clgmBlueSky            = $A98770;  //  112  135 169    215.8  0.337  0.663 
  clgmFoliage            = $517967;  //  103  121  81     87.0  0.331  0.475 
  clgmBlueFlower         = $BC8E92;  //  146  142 188    245.2  0.245  0.737 
  clgmBluishGreen        = $B6C678;  //  120  198 182    167.7  0.394  0.776 
  clgmOrange             = $398DDF;  //  223  141  57     30.4  0.744  0.875 
  clgmPurplishBlue       = $B26A5B;  //   91  106 178    229.7  0.489  0.698 
  clgmModerateRed        = $726ACB;  //  203  106 114    355.1  0.478  0.796 
  clgmPurple             = $7A4A6B;  //  107   74 122    281.3  0.393  0.478 
  clgmYellowGreen        = $4EC5AD;  //  173  197  78     72.1  0.604  0.773 
  clgmOrangeYellow       = $3DAFE9;  //  233  175  61     39.8  0.738  0.914 
  clgmBlue               = $A34B41;  //   65   75 163    233.9  0.601  0.639 
  clgmGreen              = $57A058;  //   88  160  87    119.2  0.456  0.627 
  clgmRed                = $4946BA;  //  186   70  73    358.4  0.624  0.729 
  clgmYellow             = $2FD0F0;  //  240  208  47     50.1  0.804  0.941 
  clgmMagenta            = $A267C5;  //  197  103 162    322.3  0.477  0.773 
  clgmCyan               = $B29200;  //    0  146 178    190.8  1.000  0.698 
  clgmWhite              = $F8F8F9;  //  249  248 248      0.0  0.004  0.976 
  clgmNeutral8           = $D2D1D3;  //  211  209 210    330.0  0.009  0.827 
  clgmNeutral65          = $ADACAD;  //  173  172 173    300.0  0.006  0.678 
  clgmNeutral5           = $878788;  //  136  135 135      0.0  0.007  0.533 
  clgmNeutral35          = $636363;  //   99   99  99       -   0.000  0.388 
  clgmBlack              = $414141;  //   65   65  65       -   0.000  0.255 

  //-------------------------------------------------------------------------
  // Netscape color constants                                                
  //                                                                         
  // Name                  $bbggrr        R     G   B       H      S      V  
  //-----------------      -------       ---  ---  ---    -----  -----  -----
  clnsBlack              = $000000;  //    0    0    0     -  0.000 0.000    
  clnsGrey11             = $1C1C1C;  //   28   28   28     -  0.000 0.110    
  clnsGrey21             = $363636;  //   54   54   54     -  0.000 0.212    
  clnsGrey31             = $4F4F4F;  //   79   79   79     -  0.000 0.310    
  clnsDimGrey            = $696969;  //  105  105  105     -  0.000 0.412 * Duplicate
  clnsGrey41             = $696969;  //  105  105  105     -  0.000 0.412    
  clnsGrey51             = $828282;  //  130  130  130     -  0.000 0.510    
  clnsGrey61             = $9C9C9C;  //  156  156  156     -  0.000 0.612    
  clnsDarkGrey           = $A9A9A9;  //  169  169  169     -  0.000 0.663    
  clnsGrey71             = $B5B5B5;  //  181  181  181     -  0.000 0.710    
  clnsGrey               = $BEBEBE;  //  190  190  190     -  0.000 0.745    
  clnsGray81             = $CFCFCF;  //  207  207  207     -  0.000 0.812    
  clnsLightGray          = $D3D3D3;  //  211  211  211     -  0.000 0.827    
  clnsGainsboro          = $DCDCDC;  //  220  220  220     -  0.000 0.863    
  clnsGray91             = $E8E8E8;  //  232  232  232     -  0.000 0.910    
  clnsWhiteSmoke         = $F5F5F5;  //  245  245  245     -  0.000 0.961    
  clnsWhite              = $FFFFFF;  //  255  255  255     -  0.000 1.000    
  clnsSnow4              = $89898B;  //  139  137  137    0.0 0.014 0.545    
  clnsSnow3              = $C9C9CD;  //  205  201  201    0.0 0.020 0.804    
  clnsSnow               = $FAFAFF;  //  255  250  250    0.0 0.020 1.000    
  clnsSnow1              = $FAFAFF;  //  255  250  250    0.0 0.020 1.000 *  
  clnsSnow2              = $E9E9EE;  //  238  233  233    0.0 0.021 0.933    
  clnsRosyBrown          = $8F8FBC;  //  188  143  143    0.0 0.239 0.737    
  clnsRosyBrown1         = $C1C1FF;  //  255  193  193    0.0 0.243 1.000    
  clnsRosyBrown3         = $9B9BCD;  //  205  155  155    0.0 0.244 0.804    
  clnsRosyBrown2         = $B4B4EE;  //  238  180  180    0.0 0.244 0.933    
  clnsRosyBrown4         = $69698B;  //  139  105  105    0.0 0.245 0.545    
  clnsLightCoral         = $8080F0;  //  240  128  128    0.0 0.467 0.941    
  clnsIndianRed          = $5C5CCD;  //  205   92   92    0.0 0.551 0.804    
  clnsIndianRed4         = $3A3A8B;  //  139   58   58    0.0 0.583 0.545    
  clnsIndianRed2         = $6363EE;  //  238   99   99    0.0 0.584 0.933    
  clnsIndianRed1         = $6A6AFF;  //  255  106  106    0.0 0.584 1.000    
  clnsIndianRed3         = $5555CD;  //  205   85   85    0.0 0.585 0.804    
  clnsBrown              = $2A2AA5;  //  165   42   42    0.0 0.745 0.647    
  clnsBrown4             = $23238B;  //  139   35   35    0.0 0.748 0.545    
  clnsBrown1             = $4040FF;  //  255   64   64    0.0 0.749 1.000    
  clnsBrown3             = $3333CD;  //  205   51   51    0.0 0.751 0.804    
  clnsBrown2             = $3B3BEE;  //  238   59   59    0.0 0.752 0.933    
  clnsFirebrick          = $2222B2;  //  178   34   34    0.0 0.809 0.698    
  clnsFirebrick1         = $3030FF;  //  255   48   48    0.0 0.812 1.000    
  clnsFirebrick4         = $1A1A8B;  //  139   26   26    0.0 0.813 0.545    
  clnsFirebrick3         = $2626CD;  //  205   38   38    0.0 0.815 0.804    
  clnsFirebrick2         = $2C2CEE;  //  238   44   44    0.0 0.815 0.933    
  clnsRed4               = $00008B;  //  139    0    0    0.0 1.000 0.545    
  clnsDarkRed            = $00008B;  //  139    0    0    0.0 1.000 0.545 *  
  clnsRed3               = $0000CD;  //  205    0    0    0.0 1.000 0.804    
  clnsRed2               = $0000EE;  //  238    0    0    0.0 1.000 0.933    
  clnsRed                = $0000FF;  //  255    0    0    0.0 1.000 1.000    
  clnsRed1               = $0000FF;  //  255    0    0    0.0 1.000 1.000 *  
  clnsMistyRose3         = $B5B7CD;  //  205  183  181    5.0 0.117 0.804    
  clnsMistyRose          = $E1E4FF;  //  255  228  225    6.0 0.118 1.000    
  clnsMistyRose1         = $E1E4FF;  //  255  228  225    6.0 0.118 1.000 *  
  clnsSalmon             = $7280FA;  //  250  128  114    6.2 0.544 0.980    
  clnsMistyRose2         = $D2D5EE;  //  238  213  210    6.4 0.118 0.933    
  clnsMistyRose4         = $7B7D8B;  //  139  125  123    7.5 0.115 0.545    
  clnsTomato3            = $394FCD;  //  205   79   57    8.9 0.722 0.804    
  clnsTomato             = $4763FF;  //  255   99   71    9.1 0.722 1.000    
  clnsTomato1            = $4763FF;  //  255   99   71    9.1 0.722 1.000    
  clnsTomato2            = $425CEE;  //  238   92   66    9.1 0.723 0.933    
  clnsTomato4            = $26368B;  //  139   54   38    9.5 0.727 0.545    
  clnsCoral3             = $455BCD;  //  205   91   69    9.7 0.663 0.804    
  clnsCoral4             = $2F3E8B;  //  139   62   47    9.8 0.662 0.545    
  clnsCoral1             = $5672FF;  //  255  114   86    9.9 0.663 1.000    
  clnsCoral2             = $506AEE;  //  238  106   80    9.9 0.664 0.933    
  clnsSalmon2            = $6282EE;  //  238  130   98   13.7 0.588 0.933    
  clnsSalmon4            = $394C8B;  //  139   76   57   13.9 0.590 0.545    
  clnsSalmon3            = $5470CD;  //  205  112   84   13.9 0.590 0.804    
  clnsSalmon1            = $698CFF;  //  255  140  105   14.0 0.588 1.000    
  clnsDarkSalmon         = $7A96E9;  //  233  150  122   15.1 0.476 0.914    
  clnsOrangeRed4         = $00258B;  //  139   37    0   16.0 1.000 0.545    
  clnsCoral              = $507FFF;  //  255  127   80   16.1 0.686 1.000    
  clnsOrangeRed3         = $0037CD;  //  205   55    0   16.1 1.000 0.804    
  clnsOrangeRed2         = $0040EE;  //  238   64    0   16.1 1.000 0.933    
  clnsOrangeRed          = $0045FF;  //  255   69    0   16.2 1.000 1.000    
  clnsOrangeRed1         = $0045FF;  //  255   69    0   16.2 1.000 1.000 *  
  clnsLightSalmon2       = $7295EE;  //  238  149  114   16.9 0.521 0.933    
  clnsLightSalmon        = $7AA0FF;  //  255  160  122   17.1 0.522 1.000    
  clnsLightSalmon1       = $7AA0FF;  //  255  160  122   17.1 0.522 1.000 *  
  clnsLightSalmon4       = $42578B;  //  139   87   66   17.3 0.525 0.545    
  clnsLightSalmon3       = $6281CD;  //  205  129   98   17.4 0.522 0.804    
  clnsSienna3            = $3968CD;  //  205  104   57   19.1 0.722 0.804    
  clnsSienna1            = $4782FF;  //  255  130   71   19.2 0.722 1.000    
  clnsSienna2            = $4279EE;  //  238  121   66   19.2 0.723 0.933    
  clnsSienna             = $2D52A0;  //  160   82   45   19.3 0.719 0.627    
  clnsSienna4            = $26478B;  //  139   71   38   19.6 0.727 0.545    
  clnsSeashell           = $EEF5FF;  //  255  245  238   24.7 0.067 1.000    
  clnsSeashell1          = $EEF5FF;  //  255  245  238   24.7 0.067 1.000 *  
  clnsChocolate3         = $1D66CD;  //  205  102   29   24.9 0.859 0.804    
  clnsChocolate1         = $247FFF;  //  255  127   36   24.9 0.859 1.000    
  clnsChocolate2         = $2176EE;  //  238  118   33   24.9 0.861 0.933    
  clnsChocolate          = $1E69D2;  //  210  105   30   25.0 0.857 0.824    
  clnsSaddleBrown        = $13458B;  //  139   69   19   25.0 0.863 0.545    
  clnsChocolate4         = $13458B;  //  139   69   19   25.0 0.863 0.545 *  
  clnsSeashell3          = $BFC5CD;  //  205  197  191   25.7 0.068 0.804    
  clnsSeashell2          = $DEE5EE;  //  238  229  222   26.3 0.067 0.933    
  clnsSeashell4          = $82868B;  //  139  134  130   26.7 0.065 0.545    
  clnsSandyBrown         = $60A4F4;  //  244  164   96   27.6 0.607 0.957    
  clnsPeachPuff2         = $ADCBEE;  //  238  203  173   27.7 0.273 0.933    
  clnsPeachPuff3         = $95AFCD;  //  205  175  149   27.9 0.273 0.804    
  clnsPeachPuff          = $B9DAFF;  //  255  218  185   28.3 0.275 1.000    
  clnsPeachPuff1         = $B9DAFF;  //  255  218  185   28.3 0.275 1.000 *  
  clnsPeachPuff4         = $65778B;  //  139  119  101   28.4 0.273 0.545    
  clnsTan1               = $4FA5FF;  //  255  165   79   29.3 0.690 1.000    
  clnsTan4               = $2B5A8B;  //  139   90   43   29.4 0.691 0.545    
  clnsTan2               = $499AEE;  //  238  154   73   29.5 0.693 0.933    
  clnsPeru               = $3F85CD;  //  205  133   63   29.6 0.693 0.804    
  clnsTan3               = $3F85CD;  //  205  133   63   29.6 0.693 0.804 *  
  clnsDarkOrange2        = $0076EE;  //  238  118    0   29.7 1.000 0.933    
  clnsDarkOrange4        = $00458B;  //  139   69    0   29.8 1.000 0.545    
  clnsDarkOrange3        = $0066CD;  //  205  102    0   29.9 1.000 0.804    
  clnsDarkOrange1        = $007FFF;  //  255  127    0   29.9 1.000 1.000    
  clnsLinen              = $E6F0FA;  //  250  240  230   30.0 0.080 0.980    
  clnsBisque3            = $9EB7CD;  //  205  183  158   31.9 0.229 0.804    
  clnsBisque             = $C4E4FF;  //  255  228  196   32.5 0.231 1.000    
  clnsBisque1            = $C4E4FF;  //  255  228  196   32.5 0.231 1.000 *  
  clnsBisque2            = $B7D5EE;  //  238  213  183   32.7 0.231 0.933    
  clnsDarkOrange         = $008CFF;  //  255  140    0   32.9 1.000 1.000    
  clnsAntiqueWhite3      = $B0C0CD;  //  205  192  176   33.1 0.141 0.804    
  clnsAntiqueWhite1      = $DBEFFF;  //  255  239  219   33.3 0.141 1.000    
  clnsBurlywood4         = $55738B;  //  139  115   85   33.3 0.388 0.545    
  clnsAntiqueWhite2      = $CCDFEE;  //  238  223  204   33.5 0.143 0.933    
  clnsBurlywood2         = $91C5EE;  //  238  197  145   33.5 0.391 0.933    
  clnsBurlywood1         = $9BD3FF;  //  255  211  155   33.6 0.392 1.000    
  clnsBisque4            = $6B7D8B;  //  139  125  107   33.7 0.230 0.545    
  clnsBurlywood3         = $7DAACD;  //  205  170  125   33.8 0.390 0.804    
  clnsBurlywood          = $87B8DE;  //  222  184  135   33.8 0.392 0.871    
  clnsAntiqueWhite       = $D7EBFA;  //  250  235  215   34.3 0.140 0.980    
  clnsTan                = $8CB4D2;  //  210  180  140   34.3 0.333 0.824    
  clnsAntiqueWhite4      = $78838B;  //  139  131  120   34.7 0.137 0.545    
  clnsNavajoWhite2       = $A1CFEE;  //  238  207  161   35.8 0.324 0.933    
  clnsNavajoWhite        = $ADDEFF;  //  255  222  173   35.9 0.322 1.000    
  clnsNavajoWhite1       = $ADDEFF;  //  255  222  173   35.9 0.322 1.000    
  clnsBlanchedAlmond     = $CDEBFF;  //  255  235  205   36.0 0.196 1.000    
  clnsNavajoWhite4       = $5E798B;  //  139  121   94   36.0 0.324 0.545    
  clnsNavajoWhite3       = $8BB3CD;  //  205  179  139   36.4 0.322 0.804    
  clnsPapayaWhip         = $D5EFFF;  //  255  239  213   37.1 0.165 1.000    
  clnsMoccasin           = $B5E4FF;  //  255  228  181   38.1 0.290 1.000    
  clnsOrange4            = $005A8B;  //  139   90    0   38.8 1.000 0.545    
  clnsOrange2            = $009AEE;  //  238  154    0   38.8 1.000 0.933    
  clnsOrange             = $00A5FF;  //  255  165    0   38.8 1.000 1.000 *  
  clnsOrange1            = $00A5FF;  //  255  165    0   38.8 1.000 1.000    
  clnsWheat4             = $667E8B;  //  139  126  102   38.9 0.266 0.545    
  clnsOrange3            = $0085CD;  //  205  133    0   38.9 1.000 0.804    
  clnsOldLace            = $E6F5FD;  //  253  245  230   39.1 0.091 0.992    
  clnsWheat              = $B3DEF5;  //  245  222  179   39.1 0.269 0.961    
  clnsWheat1             = $BAE7FF;  //  255  231  186   39.1 0.271 1.000    
  clnsWheat3             = $96BACD;  //  205  186  150   39.3 0.268 0.804    
  clnsWheat2             = $AED8EE;  //  238  216  174   39.4 0.269 0.933    
  clnsFloralWhite        = $F0FAFF;  //  255  250  240   40.0 0.059 1.000    
  clnsDarkGoldenrod1     = $0FB9FF;  //  255  185   15   42.5 0.941 1.000    
  clnsDarkGoldenrod3     = $0C95CD;  //  205  149   12   42.6 0.941 0.804    
  clnsDarkGoldenrod2     = $0EADEE;  //  238  173   14   42.6 0.941 0.933    
  clnsDarkGoldenrod      = $0B86B8;  //  184  134   11   42.7 0.940 0.722    
  clnsGoldenrod          = $20A5DA;  //  218  165   32   42.9 0.853 0.855    
  clnsGoldenrod1         = $25C1FF;  //  255  193   37   42.9 0.855 1.000    
  clnsGoldenrod4         = $14698B;  //  139  105   20   42.9 0.856 0.545    
  clnsGoldenrod2         = $22B4EE;  //  238  180   34   42.9 0.857 0.933    
  clnsGoldenrod3         = $1D9BCD;  //  205  155   29   43.0 0.859 0.804    
  clnsCornsilk           = $DCF8FF;  //  255  248  220   48.0 0.137 1.000    
  clnsCornsilk1          = $DCF8FF;  //  255  248  220   48.0 0.137 1.000 *  
  clnsCornsilk2          = $CDE8EE;  //  238  232  205   49.1 0.139 0.933    
  clnsCornsilk3          = $B1C8CD;  //  205  200  177   49.3 0.137 0.804    
  clnsLightGoldenrod2    = $82DCEE;  //  238  220  130   50.0 0.454 0.933    
  clnsLightGoldenrod1    = $8BECFF;  //  255  236  139   50.2 0.455 1.000    
  clnsLightGoldenrod3    = $70BECD;  //  205  190  112   50.3 0.454 0.804    
  clnsCornsilk4          = $78888B;  //  139  136  120   50.5 0.137 0.545    
  clnsLightGoldenrod4    = $4C818B;  //  139  129   76   50.5 0.453 0.545    
  clnsGold4              = $00758B;  //  139  117    0   50.5 1.000 0.545    
  clnsLightGoldenrod     = $82DDEE;  //  238  221  130   50.6 0.454 0.933    
  clnsGold3              = $00ADCD;  //  205  173    0   50.6 1.000 0.804    
  clnsGold               = $00D7FF;  //  255  215    0   50.6 1.000 1.000    
  clnsGold1              = $00D7FF;  //  255  215    0   50.6 1.000 1.000 *  
  clnsGold2              = $00C9EE;  //  238  201    0   50.7 1.000 0.933    
  clnsLemonChiffon2      = $BFE9EE;  //  238  233  191   53.6 0.197 0.933    
  clnsLemonChiffon3      = $A5C9CD;  //  205  201  165   54.0 0.195 0.804    
  clnsLemonChiffon       = $CDFAFF;  //  255  250  205   54.0 0.196 1.000    
  clnsLemonChiffon1      = $CDFAFF;  //  255  250  205   54.0 0.196 1.000 *  
  clnsPaleGoldenrod      = $AAE8EE;  //  238  232  170   54.7 0.286 0.933    
  clnsKhaki4             = $4E868B;  //  139  134   78   55.1 0.439 0.545    
  clnsKhaki1             = $8FF6FF;  //  255  246  143   55.2 0.439 1.000    
  clnsKhaki3             = $73C6CD;  //  205  198  115   55.3 0.439 0.804    
  clnsKhaki2             = $85E6EE;  //  238  230  133   55.4 0.441 0.933    
  clnsLemonChiffon4      = $70898B;  //  139  137  112   55.6 0.194 0.545    
  clnsDarkKhaki          = $6BB7BD;  //  189  183  107   55.6 0.434 0.741    
  clnsIvory4             = $838B8B;  //  139  139  131   60.0 0.058 0.545    
  clnsIvory3             = $C1CDCD;  //  205  205  193   60.0 0.059 0.804    
  clnsIvory2             = $E0EEEE;  //  238  238  224   60.0 0.059 0.933    
  clnsIvory              = $F0FFFF;  //  255  255  240   60.0 0.059 1.000    
  clnsIvory1             = $F0FFFF;  //  255  255  240   60.0 0.059 1.000 *  
  clnsBeige              = $DCF5F5;  //  245  245  220   60.0 0.102 0.961    
  clnsLightYellow4       = $7A8B8B;  //  139  139  122   60.0 0.122 0.545    
  clnsLightYellow3       = $B4CDCD;  //  205  205  180   60.0 0.122 0.804    
  clnsLightYellow2       = $D1EEEE;  //  238  238  209   60.0 0.122 0.933    
  clnsLightYellow        = $E0FFFF;  //  255  255  224   60.0 0.122 1.000    
  clnsLightYellow1       = $E0FFFF;  //  255  255  224   60.0 0.122 1.000 *  
  clnsLtGoldenrodYello   = $D2FAFA;  //  250  250  210   60.0 0.160 0.980    
  clnsYellow4            = $008B8B;  //  139  139    0   60.0 1.000 0.545    
  clnsYellow3            = $00CDCD;  //  205  205    0   60.0 1.000 0.804    
  clnsYellow2            = $00EEEE;  //  238  238    0   60.0 1.000 0.933    
  clnsYellow             = $00FFFF;  //  255  255    0   60.0 1.000 1.000    
  clnsYellow1            = $00FFFF;  //  255  255    0   60.0 1.000 1.000 *  
  clnsOliveDrab4         = $228B69;  //  105  139   34   79.4 0.755 0.545    
  clnsOliveDrab          = $238E6B;  //  107  142   35   79.6 0.754 0.557    
  clnsOliveDrab1         = $3EFFC0;  //  192  255   62   79.6 0.757 1.000    
  clnsYellowGreen        = $32CD9A;  //  154  205   50   79.7 0.756 0.804    
  clnsOliveDrab3         = $32CD9A;  //  154  205   50   79.7 0.756 0.804 *  
  clnsOliveDrab2         = $3AEEB3;  //  179  238   58   79.7 0.756 0.933    
  clnsDarkOliveGreen     = $2F6B55;  //   85  107   47   82.0 0.561 0.420    
  clnsDarkOliveGreen1    = $70FFCA;  //  202  255  112   82.2 0.561 1.000    
  clnsDarkOliveGreen4    = $3D8B6E;  //  110  139   61   82.3 0.561 0.545    
  clnsDarkOliveGreen3    = $5ACDA2;  //  162  205   90   82.4 0.561 0.804    
  clnsDarkOliveGreen2    = $68EEBC;  //  188  238  104   82.4 0.563 0.933    
  clnsGreenYellow        = $2FFFAD;  //  173  255   47   83.7 0.816 1.000    
  clnsChartreuse3        = $00CD66;  //  102  205    0   90.1 1.000 0.804    
  clnsChartreuse         = $00FF7F;  //  127  255    0   90.1 1.000 1.000    
  clnsChartreuse1        = $00FF7F;  //  127  255    0   90.1 1.000 1.000 *  
  clnsChartreuse4        = $008B45;  //   69  139    0   90.2 1.000 0.545    
  clnsChartreuse2        = $00EE76;  //  118  238    0   90.3 1.000 0.933    
  clnsLawnGreen          = $00FC7C;  //  124  252    0   90.5 1.000 0.988    
  clnsHoneydew4          = $838B83;  //  131  139  131  120.0 0.058 0.545    
  clnsHoneydew3          = $C1CDC1;  //  193  205  193  120.0 0.059 0.804    
  clnsHoneydew2          = $E0EEE0;  //  224  238  224  120.0 0.059 0.933    
  clnsHoneydew           = $F0FFF0;  //  240  255  240  120.0 0.059 1.000    
  clnsHoneydew1          = $F0FFF0;  //  240  255  240  120.0 0.059 1.000 *  
  clnsDarkSeaGreen       = $8FBC8F;  //  143  188  143  120.0 0.239 0.737    
  clnsDarkSeaGreen1      = $C1FFC1;  //  193  255  193  120.0 0.243 1.000    
  clnsDarkSeaGreen3      = $9BCD9B;  //  155  205  155  120.0 0.244 0.804    
  clnsDarkSeaGreen2      = $B4EEB4;  //  180  238  180  120.0 0.244 0.933    
  clnsDarkSeaGreen4      = $698B69;  //  105  139  105  120.0 0.245 0.545    
  clnsPaleGreen          = $98FB98;  //  152  251  152  120.0 0.394 0.984    
  clnsPaleGreen3         = $7CCD7C;  //  124  205  124  120.0 0.395 0.804    
  clnsPaleGreen2         = $90EE90;  //  144  238  144  120.0 0.395 0.933    
  clnsLightGreen         = $90EE90;  //  144  238  144  120.0 0.395 0.933 *  
  clnsPaleGreen4         = $548B54;  //   84  139   84  120.0 0.396 0.545    
  clnsPaleGreen1         = $9AFF9A;  //  154  255  154  120.0 0.396 1.000    
  clnsForestGreen        = $228B22;  //   34  139   34  120.0 0.755 0.545    
  clnsLimeGreen          = $32CD32;  //   50  205   50  120.0 0.756 0.804    
  clnsDarkGreen          = $006400;  //    0  100    0  120.0 1.000 0.392    
  clnsGreen4             = $008B00;  //    0  139    0  120.0 1.000 0.545    
  clnsGreen3             = $00CD00;  //    0  205    0  120.0 1.000 0.804    
  clnsGreen2             = $00EE00;  //    0  238    0  120.0 1.000 0.933    
  clnsGreen              = $00FF00;  //    0  255    0  120.0 1.000 1.000    
  clnsGreen1             = $00FF00;  //    0  255    0  120.0 1.000 1.000 *  
  clnsSeaGreen1          = $9FFF54;  //   84  255  159  146.3 0.671 1.000    
  clnsSeaGreen2          = $94EE4E;  //   78  238  148  146.3 0.672 0.933    
  clnsSeaGreen           = $578B2E;  //   46  139   87  146.5 0.669 0.545    
  clnsSeaGreen4          = $578B2E;  //   46  139   87  146.5 0.669 0.545 *  
  clnsSeaGreen3          = $80CD43;  //   67  205  128  146.5 0.673 0.804    
  clnsMediumSeaGreen     = $71B33C;  //   60  179  113  146.7 0.665 0.702    
  clnsSpringGreen2       = $76EE00;  //    0  238  118  149.7 1.000 0.933    
  clnsSpringGreen4       = $458B00;  //    0  139   69  149.8 1.000 0.545    
  clnsSpringGreen3       = $66CD00;  //    0  205  102  149.9 1.000 0.804    
  clnsSpringGreen        = $7FFF00;  //    0  255  127  149.9 1.000 1.000    
  clnsSpringGreen1       = $7FFF00;  //    0  255  127  149.9 1.000 1.000 *  
  clnsMintCream          = $FAFFF5;  //  245  255  250  150.0 0.039 1.000    
  clnsMedSpringGreen     = $9AFA00;  //    0  250  154  157.0 1.000 0.980    
  clnsMediumAquamarine   = $AACD66;  //  102  205  170  159.6 0.502 0.804    
  clnsAquamarine3        = $AACD66;  //  102  205  170  159.6 0.502 0.804    
  clnsAquamarine         = $D4FF7F;  //  127  255  212  159.8 0.502 1.000    
  clnsAquamarine1        = $D4FF7F;  //  127  255  212  159.8 0.502 1.000 *  
  clnsAquamarine2        = $C6EE76;  //  118  238  198  160.0 0.504 0.933    
  clnsAquamarine4        = $748B45;  //   69  139  116  160.3 0.504 0.545    
  clnsTurquoise          = $D0E040;  //   64  224  208  174.0 0.714 0.878    
  clnsLightSeaGreen      = $AAB220;  //   32  178  170  176.7 0.820 0.698    
  clnsMediumTurquoise    = $CCD148;  //   72  209  204  177.8 0.656 0.820    
  clnsAzure4             = $8B8B83;  //  131  139  139  180.0 0.058 0.545    
  clnsAzure3             = $CDCDC1;  //  193  205  205  180.0 0.059 0.804    
  clnsAzure2             = $EEEEE0;  //  224  238  238  180.0 0.059 0.933    
  clnsAzure              = $FFFFF0;  //  240  255  255  180.0 0.059 1.000    
  clnsAzure1             = $FFFFF0;  //  240  255  255  180.0 0.059 1.000 *  
  clnsLightCyan4         = $8B8B7A;  //  122  139  139  180.0 0.122 0.545    
  clnsLightCyan3         = $CDCDB4;  //  180  205  205  180.0 0.122 0.804    
  clnsLightCyan2         = $EEEED1;  //  209  238  238  180.0 0.122 0.933    
  clnsLightCyan          = $FFFFE0;  //  224  255  255  180.0 0.122 1.000    
  clnsLightCyan1         = $FFFFE0;  //  224  255  255  180.0 0.122 1.000 *  
  clnsPaleTurquoise      = $EEEEAF;  //  175  238  238  180.0 0.265 0.933    
  clnsPaleTurquoise4     = $8B8B66;  //  102  139  139  180.0 0.266 0.545    
  clnsPaleTurquoise1     = $FFFFBB;  //  187  255  255  180.0 0.267 1.000    
  clnsPaleTurquoise3     = $CDCD96;  //  150  205  205  180.0 0.268 0.804    
  clnsPaleTurquoise2     = $EEEEAE;  //  174  238  238  180.0 0.269 0.933    
  clnsDarkSlateGray      = $4F4F2F;  //   47   79   79  180.0 0.405 0.310    
  clnsDarkSlateGray2     = $EEEE8D;  //  141  238  238  180.0 0.408 0.933    
  clnsDarkSlateGray1     = $FFFF97;  //  151  255  255  180.0 0.408 1.000    
  clnsDarkSlateGray4     = $8B8B52;  //   82  139  139  180.0 0.410 0.545    
  clnsDarkSlateGray3     = $CDCD79;  //  121  205  205  180.0 0.410 0.804    
  clnsCyan4              = $8B8B00;  //    0  139  139  180.0 1.000 0.545    
  clnsDarkCyan           = $8B8B00;  //    0  139  139  180.0 1.000 0.545 *  
  clnsCyan3              = $CDCD00;  //    0  205  205  180.0 1.000 0.804    
  clnsCyan2              = $EEEE00;  //    0  238  238  180.0 1.000 0.933    
  clnsCyan               = $FFFF00;  //    0  255  255  180.0 1.000 1.000    
  clnsCyan1              = $FFFF00;  //    0  255  255  180.0 1.000 1.000 *  
  clnsDarkTurquoise      = $D1CE00;  //    0  206  209  180.9 1.000 0.820    
  clnsCadetBlue          = $A09E5F;  //   95  158  160  181.8 0.406 0.627    
  clnsTurquoise4         = $8B8600;  //    0  134  139  182.2 1.000 0.545    
  clnsTurquoise3         = $CDC500;  //    0  197  205  182.3 1.000 0.804    
  clnsTurquoise2         = $EEE500;  //    0  229  238  182.3 1.000 0.933    
  clnsTurquoise1         = $FFF500;  //    0  245  255  182.4 1.000 1.000    
  clnsCadetBlue4         = $8B8653;  //   83  134  139  185.4 0.403 0.545    
  clnsCadetBlue2         = $EEE58E;  //  142  229  238  185.6 0.403 0.933    
  clnsCadetBlue1         = $FFF598;  //  152  245  255  185.8 0.404 1.000    
  clnsCadetBlue3         = $CDC57A;  //  122  197  205  185.8 0.405 0.804    
  clnsPowderBlue         = $E6E0B0;  //  176  224  230  186.7 0.235 0.902    
  clnsLightBlue4         = $8B8368;  //  104  131  139  193.7 0.252 0.545    
  clnsLightBlue          = $E6D8AD;  //  173  216  230  194.7 0.248 0.902    
  clnsDeepSkyBlue3       = $CD9A00;  //    0  154  205  194.9 1.000 0.804    
  clnsLightBlue1         = $FFEFBF;  //  191  239  255  195.0 0.251 1.000    
  clnsLightBlue2         = $EEDFB2;  //  178  223  238  195.0 0.252 0.933    
  clnsDeepSkyBlue4       = $8B6800;  //    0  104  139  195.1 1.000 0.545    
  clnsDeepSkyBlue2       = $EEB200;  //    0  178  238  195.1 1.000 0.933    
  clnsDeepSkyBlue        = $FFBF00;  //    0  191  255  195.1 1.000 1.000    
  clnsDeepSkyBlue1       = $FFBF00;  //    0  191  255  195.1 1.000 1.000 *  
  clnsLightBlue3         = $CDC09A;  //  154  192  205  195.3 0.249 0.804    
  clnsSkyBlue            = $EBCE87;  //  135  206  235  197.4 0.426 0.922    
  clnsLightSkyBlue3      = $CDB68D;  //  141  182  205  201.6 0.312 0.804    
  clnsLightSkyBlue2      = $EED3A4;  //  164  211  238  201.9 0.311 0.933    
  clnsLightSkyBlue1      = $FFE2B0;  //  176  226  255  202.0 0.310 1.000    
  clnsLightSkyBlue4      = $8B7B60;  //   96  123  139  202.3 0.309 0.545    
  clnsLightSkyBlue       = $FACE87;  //  135  206  250  203.0 0.460 0.980    
  clnsSkyBlue3           = $CDA66C;  //  108  166  205  204.1 0.473 0.804    
  clnsSkyBlue1           = $FFCE87;  //  135  206  255  204.5 0.471 1.000    
  clnsSkyBlue2           = $EEC07E;  //  126  192  238  204.6 0.471 0.933    
  clnsSkyBlue4           = $8B704A;  //   74  112  139  204.9 0.468 0.545    
  clnsSteelBlue2         = $EEAC5C;  //   92  172  238  207.1 0.613 0.933    
  clnsSteelBlue3         = $CD944F;  //   79  148  205  207.1 0.615 0.804    
  clnsSteelBlue          = $B48246;  //   70  130  180  207.3 0.611 0.706    
  clnsSteelBlue1         = $FFB863;  //   99  184  255  207.3 0.612 1.000    
  clnsSteelBlue4         = $8B6436;  //   54  100  139  207.5 0.612 0.545    
  clnsAliceBlue          = $FFF8F0;  //  240  248  255  208.0 0.059 1.000    
  clnsDodgerBlue3        = $CD7418;  //   24  116  205  209.5 0.883 0.804    
  clnsDodgerBlue         = $FF901E;  //   30  144  255  209.6 0.882 1.000    
  clnsDodgerBlue1        = $FF901E;  //   30  144  255  209.6 0.882 1.000 *  
  clnsDodgerBlue2        = $EE861C;  //   28  134  238  209.7 0.882 0.933    
  clnsDodgerBlue4        = $8B4E10;  //   16   78  139  209.8 0.885 0.545    
  clnsSlateGrey          = $908070;  //  112  128  144  210.0 0.222 0.565    
  clnsLightSlateGray     = $998877;  //  119  136  153  210.0 0.222 0.600    
  clnsSlateGray3         = $CDB69F;  //  159  182  205  210.0 0.224 0.804    
  clnsSlateGray1         = $FFE2C6;  //  198  226  255  210.5 0.224 1.000    
  clnsSlateGray2         = $EED3B9;  //  185  211  238  210.6 0.223 0.933    
  clnsSlateGray4         = $8B7B6C;  //  108  123  139  211.0 0.223 0.545    
  clnsLightSteelBlue4    = $8B7B6E;  //  110  123  139  213.1 0.209 0.545    
  clnsLightSteelBlue3    = $CDB5A2;  //  162  181  205  213.5 0.210 0.804    
  clnsLightSteelBlue2    = $EED2BC;  //  188  210  238  213.6 0.210 0.933    
  clnsLightSteelBlue     = $DEC4B0;  //  176  196  222  213.9 0.207 0.871    
  clnsLightSteelBlue1    = $FFE1CA;  //  202  225  255  214.0 0.208 1.000    
  clnsCornflowerBlue     = $ED9564;  //  100  149  237  218.5 0.578 0.929    
  clnsRoyalBlue3         = $CD5F3A;  //   58   95  205  224.9 0.717 0.804    
  clnsRoyalBlue2         = $EE6E43;  //   67  110  238  224.9 0.718 0.933    
  clnsRoyalBlue1         = $FF7648;  //   72  118  255  224.9 0.718 1.000    
  clnsRoyalBlue          = $E16941;  //   65  105  225  225.0 0.711 0.882    
  clnsRoyalBlue4         = $8B4027;  //   39   64  139  225.0 0.719 0.545    
  clnsGhostWhite         = $FFF8F8;  //  248  248  255  240.0 0.027 1.000    
  clnsLavender           = $FAE6E6;  //  230  230  250  240.0 0.080 0.980    
  clnsMidnightBlue       = $701919;  //   25   25  112  240.0 0.777 0.439    
  clnsNavyBlue           = $800000;  //    0    0  128  240.0 1.000 0.502    
  clnsBlue4              = $8B0000;  //    0    0  139  240.0 1.000 0.545    
  clnsDarkBlue           = $8B0000;  //    0    0  139  240.0 1.000 0.545 *  
  clnsMediumBlue         = $CD0000;  //    0    0  205  240.0 1.000 0.804    
  clnsBlue3              = $CD0000;  //    0    0  205  240.0 1.000 0.804 *  
  clnsBlue2              = $EE0000;  //    0    0  238  240.0 1.000 0.933    
  clnsBlue               = $FF0000;  //    0    0  255  240.0 1.000 1.000    
  clnsBlue1              = $FF0000;  //    0    0  255  240.0 1.000 1.000 *  
  clnsSlateBlue          = $CD5A6A;  //  106   90  205  248.3 0.561 0.804    
  clnsSlateBlue1         = $FF6F83;  //  131  111  255  248.3 0.565 1.000    
  clnsSlateBlue3         = $CD5969;  //  105   89  205  248.3 0.566 0.804    
  clnsLightSlateBlue     = $FF7084;  //  132  112  255  248.4 0.561 1.000    
  clnsSlateBlue2         = $EE677A;  //  122  103  238  248.4 0.567 0.933    
  clnsSlateBlue4         = $8B3C47;  //   71   60  139  248.4 0.568 0.545    
  clnsDarkSlateBlue      = $8B3D48;  //   72   61  139  248.5 0.561 0.545    
  clnsMediumSlateBlue    = $EE687B;  //  123  104  238  248.5 0.563 0.933    
  clnsMediumPurple4      = $8B475D;  //   93   71  139  259.4 0.489 0.545    
  clnsMediumPurple2      = $EE799F;  //  159  121  238  259.5 0.492 0.933    
  clnsMediumPurple       = $DB7093;  //  147  112  219  259.6 0.489 0.859    
  clnsMediumPurple3      = $CD6889;  //  137  104  205  259.6 0.493 0.804    
  clnsMediumPurple1      = $FF82AB;  //  171  130  255  259.7 0.490 1.000    
  clnsPurple1            = $FF309B;  //  155   48  255  271.0 0.812 1.000    
  clnsBlueViolet         = $E22B8A;  //  138   43  226  271.1 0.810 0.886    
  clnsPurple2            = $EE2C91;  //  145   44  238  271.2 0.815 0.933    
  clnsPurple4            = $8B1A55;  //   85   26  139  271.3 0.813 0.545    
  clnsPurple3            = $CD267D;  //  125   38  205  271.3 0.815 0.804    
  clnsPurple             = $F020A0;  //  160   32  240  276.9 0.867 0.941    
  clnsDarkOrchid4        = $8B2268;  //  104   34  139  280.0 0.755 0.545    
  clnsDarkOrchid2        = $EE3AB2;  //  178   58  238  280.0 0.756 0.933    
  clnsDarkOrchid         = $CC3299;  //  153   50  204  280.1 0.755 0.800    
  clnsDarkOrchid1        = $FF3EBF;  //  191   62  255  280.1 0.757 1.000    
  clnsDarkOrchid3        = $CD329A;  //  154   50  205  280.3 0.756 0.804    
  clnsDarkViolet         = $D30094;  //  148    0  211  282.1 1.000 0.827    
  clnsMediumOrchid3      = $CD52B4;  //  180   82  205  287.8 0.600 0.804    
  clnsMediumOrchid1      = $FF66E0;  //  224  102  255  287.8 0.600 1.000    
  clnsMediumOrchid2      = $EE5FD1;  //  209   95  238  287.8 0.601 0.933    
  clnsMediumOrchid4      = $8B377A;  //  122   55  139  287.9 0.604 0.545    
  clnsMediumOrchid       = $D355BA;  //  186   85  211  288.1 0.597 0.827    
  clnsThistle4           = $8B7B8B;  //  139  123  139  300.0 0.115 0.545    
  clnsThistle            = $D8BFD8;  //  216  191  216  300.0 0.116 0.847    
  clnsThistle3           = $CDB5CD;  //  205  181  205  300.0 0.117 0.804    
  clnsThistle2           = $EED2EE;  //  238  210  238  300.0 0.118 0.933    
  clnsThistle1           = $FFE1FF;  //  255  225  255  300.0 0.118 1.000    
  clnsPlum4              = $8B668B;  //  139  102  139  300.0 0.266 0.545    
  clnsPlum1              = $FFBBFF;  //  255  187  255  300.0 0.267 1.000    
  clnsPlum3              = $CD96CD;  //  205  150  205  300.0 0.268 0.804    
  clnsPlum2              = $EEAEEE;  //  238  174  238  300.0 0.269 0.933    
  clnsDarkGoldenrod4     = $8B658B;  //  139  101  139  300.0 0.273 0.545    
  clnsPlum               = $DDA0DD;  //  221  160  221  300.0 0.276 0.867    
  clnsViolet             = $EE82EE;  //  238  130  238  300.0 0.454 0.933    
  clnsMagenta4           = $8B008B;  //  139    0  139  300.0 1.000 0.545    
  clnsDarkMagenta        = $8B008B;  //  139    0  139  300.0 1.000 0.545 *  
  clnsMagenta3           = $CD00CD;  //  205    0  205  300.0 1.000 0.804    
  clnsMagenta2           = $EE00EE;  //  238    0  238  300.0 1.000 0.933    
  clnsMagenta            = $FF00FF;  //  255    0  255  300.0 1.000 1.000    
  clnsMagenta1           = $FF00FF;  //  255    0  255  300.0 1.000 1.000 *  
  clnsOrchid4            = $89478B;  //  139   71  137  301.8 0.489 0.545    
  clnsOrchid             = $D670DA;  //  218  112  214  302.3 0.486 0.855    
  clnsOrchid1            = $FA83FF;  //  255  131  250  302.4 0.486 1.000    
  clnsOrchid3            = $C969CD;  //  205  105  201  302.4 0.488 0.804    
  clnsOrchid2            = $E97AEE;  //  238  122  233  302.6 0.487 0.933    
  clnsVioletRed          = $9020D0;  //  208   32  144  321.8 0.846 0.816    
  clnsMaroon4            = $621C8B;  //  139   28   98  322.2 0.799 0.545    
  clnsMediumVioletRed    = $8515C7;  //  199   21  133  322.2 0.894 0.780    
  clnsMaroon3            = $9029CD;  //  205   41  144  322.3 0.800 0.804    
  clnsMaroon2            = $A730EE;  //  238   48  167  322.4 0.798 0.933    
  clnsMaroon1            = $B334FF;  //  255   52  179  322.5 0.796 1.000    
  clnsDeepPink4          = $500A8B;  //  139   10   80  327.4 0.928 0.545    
  clnsDeepPink2          = $8912EE;  //  238   18  137  327.5 0.924 0.933    
  clnsDeepPink3          = $7610CD;  //  205   16  118  327.6 0.922 0.804    
  clnsDeepPink           = $9314FF;  //  255   20  147  327.6 0.922 1.000    
  clnsDeepPink1          = $9314FF;  //  255   20  147  327.6 0.922 1.000 *  
  clnsHotPink            = $B469FF;  //  255  105  180  330.0 0.588 1.000    
  clnsHotPink4           = $623A8B;  //  139   58   98  330.4 0.583 0.545    
  clnsHotPink1           = $B46EFF;  //  255  110  180  331.0 0.569 1.000    
  clnsHotPink2           = $A76AEE;  //  238  106  167  332.3 0.555 0.933    
  clnsVioletRed4         = $52228B;  //  139   34   82  332.6 0.755 0.545    
  clnsVioletRed1         = $963EFF;  //  255   62  150  332.6 0.757 1.000    
  clnsVioletRed2         = $8C3AEE;  //  238   58  140  332.7 0.756 0.933    
  clnsVioletRed3         = $7832CD;  //  205   50  120  332.9 0.756 0.804    
  clnsHotPink3           = $9060CD;  //  205   96  144  333.6 0.532 0.804    
  clnsLavenderBlush4     = $86838B;  //  139  131  134  337.5 0.058 0.545    
  clnsMaroon             = $6030B0;  //  176   48   96  337.5 0.727 0.690    
  clnsLavenderBlush2     = $E5E0EE;  //  238  224  229  338.6 0.059 0.933    
  clnsLavenderBlush3     = $C5C1CD;  //  205  193  197  340.0 0.059 0.804    
  clnsLavenderBlush      = $F5F0FF;  //  255  240  245  340.0 0.059 1.000    
  clnsLavenderBlush1     = $F5F0FF;  //  255  240  245  340.0 0.059 1.000 *  
  clnsPaleVioletRed1     = $AB82FF;  //  255  130  171  340.3 0.490 1.000    
  clnsPaleVioletRed      = $9370DB;  //  219  112  147  340.4 0.489 0.859    
  clnsPaleVioletRed3     = $8968CD;  //  205  104  137  340.4 0.493 0.804    
  clnsPaleVioletRed2     = $9F79EE;  //  238  121  159  340.5 0.492 0.933    
  clnsPaleVioletRed4     = $5D478B;  //  139   71   93  340.6 0.489 0.545    
  clnsPink4              = $6C638B;  //  139   99  108  346.5 0.288 0.545    
  clnsPink2              = $B8A9EE;  //  238  169  184  347.0 0.290 0.933    
  clnsPink1              = $C5B5FF;  //  255  181  197  347.0 0.290 1.000    
  clnsPink3              = $9E91CD;  //  205  145  158  347.0 0.293 0.804    
  clnsPink               = $CBC0FF;  //  255  192  203  349.5 0.247 1.000    
  clnsLightPink          = $C1B6FF;  //  255  182  193  351.0 0.286 1.000    
  clnsLightPink2         = $ADA2EE;  //  238  162  173  351.3 0.319 0.933    
  clnsLightPink3         = $958CCD;  //  205  140  149  351.7 0.317 0.804    
  clnsLightPink4         = $655F8B;  //  139   95  101  351.8 0.317 0.545    
  clnsLightPink1         = $B9AEFF;  //  255  174  185  351.9 0.318 1.000    

function cLn2: Extended;

implementation

function cLn2: Extended;
begin
  Result := Ln(2);
end;

end.


