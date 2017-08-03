-- | Substitutions.

module Substitutions
  ( authorSubst
  , removeFromTitle
  , replace
  , replaceHTMLEscapedText
  , replaceUnicode
  , substTable
  , weirdSubst
  ) where

import Data.Char ( ord )

import Data.Text ( Text )
import qualified Data.Text as T

import Text.Printf ( printf )

------------------------------------------------------------------------------
-- Replacements, pre-processing and post-processing

-- | Remove text from the title.
removeFromTitle ∷ Text → Text
removeFromTitle = replace $ map (\ a → (a, T.empty)) titleErase

-- These substitutions should be done before the Unicode
-- substitutions.
replaceHTMLEscapedText ∷ Text → Text
replaceHTMLEscapedText = replace allSubst
  where

  htmlNameSubst ∷ [(Text, Text)]
  htmlNameSubst = map (\ (a, b, _) → (T.cons '&' $ T.snoc b ';', a)) $
                    filter (\ (_, b,_) → b /= noName && b /= noSupported) substTable

  htmlDecSubst ∷ [(Text, Text)]
  htmlDecSubst = substHelper "&#%d;"

  html4HexSubst ∷ [(Text, Text)]
  html4HexSubst = substHelper "&#x%04X;"

  html5HexSubst ∷ [(Text, Text)]
  html5HexSubst = substHelper "&#x%05X;"

  allSubst ∷ [(Text, Text)]
  allSubst = htmlNameSubst ++ htmlDecSubst ++ html4HexSubst ++ html5HexSubst

  substHelper ∷ String → [(Text, Text)]
  substHelper format = map (\ (a, _, _) → (printfHelper format a, a)) $
                         filter (\ (_, b,_) → b /= noSupported) substTable
    where
    printfHelper ∷ String → Text → Text
    printfHelper pFormat t = T.pack $ printf pFormat (ord $ T.head t)

replaceUnicode ∷ Text → Text
replaceUnicode = replace unicodeSubst
  where
  unicodeSubst ∷ [(Text, Text)]
  unicodeSubst = map (\ (a, _, c) → (a, c)) substTable

replace ∷ [(Text,Text)] → Text → Text
replace xs ys = foldl (flip (uncurry T.replace)) ys xs

unaryOp ∷ Text → Text
unaryOp t = T.snoc t '-'

binaryOp ∷ Text → Text
binaryOp t = T.cons '-' $ T.snoc t '-'

------------------------------------------------------------------------------
-- Author substitutions

-- | Author substitutions.
authorSubst ∷ [(Text, Text)]
authorSubst =
  [ (", ",   ",")
  , (" and", ",")
  ]

------------------------------------------------------------------------------
-- Weird substitutions.

-- These substitutions should be done before the substitutions of the
-- HTML entities and symbols and converting to lower case.

-- | Weird author and title substitutions.
weirdSubst ∷ [(Text, Text)]
weirdSubst =
  [ ("Ã¡",      "a")    -- U+00C3 and U+00A1 (LATIN SMALL LETTER A WITH GRAVE)
  , ("Ã©",      "e")    -- U+00C3 and U+00A9 (LATIN SMALL LETTER E WITH ACUTE)
  , ("Ã\x00AD", "i")    -- U+00C3 and U+00AD (LATIN SMALL LETTER I GRAVE)
  , ("Ã¶",      "o")    -- U+00C3 and U+00B6 (LATIN SMALL LETTER O WITH DIAERESIS)
  , ("Å›",      "s")    -- U+00C5 and U+203A (LATIN CAPITAL LETTER S WITH ACUTE)
  -- TODO (2017-07-17): Missing `;`.
  , ("&#8217",  "")     -- U+2019 RIGHT SINGLE QUOTATION MARK
  ]

------------------------------------------------------------------------------
-- Title pre-processing

titleErase ∷ [Text]
titleErase =
  [ "<Emphasis Type=\"BoldItalic\">"  -- opening
  , "<Emphasis Type=\"Italic\">"      -- opening
  , "</Emphasis>"                     -- closing
  , "<Subscript>"                     -- opening
  , "</Subscript>"                    -- closing
  , "<Superscript>"                   -- opening
  , "</Superscript>"                  -- closing
  , "<TEX>$\\"                        -- opening
  , "$</TEX>"                         -- closing
  , "<TEX>{\\sc "                     -- opening
  , "<TEX>{\\sf "                     -- opening
  , "}</TEX>"                         -- closing
  ]

------------------------------------------------------------------------------
-- Unicode and HTML entities and symbols substitutions

-- HTML escaped text without entity name.
noName ∷ Text
noName = "N/A"

-- Unicode character does not supported by HTML.
noSupported ∷ Text
noSupported = "N/A"

-- | Substitutions table.
substTable ∷ [(Text, Text, Text)]
substTable =
  [ ("\t", noSupported,    "")  -- U+0009 CHARACTER TABULATION
  , ("\n", noSupported,    "")  -- U+000A LINE FEED (LF)
  , ("\f", noSupported,    "")  -- U+000C FORM FEED (FF)
  , ("\r", noSupported,    "")  -- U+000D CARRIAGE RETURN (CR)
  , (" ", "TODO",          "-") -- U+0020 SPACE
  , ("!", noName,          "")  -- U+0021
  , ("\"", "quot",         "")  -- U+0022 QUOTATION MARK
  , ("#", noName,          "")  -- U+0023
  , ("$", noName,          "")  -- U+0024
  , ("%", noName,          "")  -- U+0025
  , ("&", "amp",           "")  -- U+0026
  , ("'", noName,          "")  -- U+0027
  , ("(", noName,          "-")  -- U+0028
  , (")", noName,          "-")  -- U+0029
  , ("*", noName,          "")  -- U+002A
  , ("+", noName,          binaryOp "plus")  -- U+002B
  , (",", noName,          "")  -- U+002C COMMA
  -- We do not substitute U+002D HYPHEN-MINUS.
  , (".", noName,          "")  -- U+002E FULL STOP
  , ("/", noName,          "")  -- U+002F SOLIDUS
  , (":", noName,          "")  -- U+003A
  , (";", noName,          "")  -- U+003B
  , ("<", "lt",            binaryOp "less-than")  -- U+003C
  , ("=", noName,          binaryOp "equals")  -- U+003D
  , (">", "gt",            binaryOp "greater-than")  -- U+003E
  , ("?", noName,          "")  -- U+003F
  , ("@", noName,          "")  -- U+0040
  , ("[", "TODO",          "")  -- U+005B
  , ("\\", "TODO",         "")  -- U+005C REVERSE SOLIDUS
  , ("]", "TODO",          "")  -- U+005D
  , ("^", "TODO",          "")  -- U+005E
  , ("_", "TODO",          "-")  -- U+005F LOW LINE
  , ("`", "TODO",          "")  -- U+0060
  , ("{", "TODO",          "")  -- U+007B
  , ("|", "TODO",          "")  -- U+007C
  , ("}", "TODO",          "")  -- U+007B
  , ("~", "TODO",          "")  -- U+007E
  , ("¡", "iexcl",         "")  -- U+00A1
  , ("¢", "cent",          "cent")  -- U+00A2
  , ("£", "pound",         "pound")  -- U+00A3
  , ("¤", "curren",        "")  -- U+00A4
  , ("¥", "yen",           "yen")  -- U+00A5
  , ("¦", "brvvar",        "")  -- U+00A6 BROKEN BAR
  , ("§", "sect",          "")  -- U+00A7
  , ("¨", "uml",           "")  -- U+00A8
  , ("©", "copy",          "")  -- U+00A9
  , ("ª", "ordf",          "")  -- U+00AA
  , ("«", "laquo",         "")  -- U+00AB
  , ("¬", "not",           "")  -- U+00AC
  -- TODO 2017-07-31
  -- , ("­", "shy",           "-")  -- U+00AD SOFT HYPHEN
  , ("®", "reg",           "")  -- U+00AE
  , ("¯", "macr",          "")  -- U+00AF
  , ("°", "deg",           "degree")  -- U+00B0
  , ("±", "plusmn",        binaryOp "plus-minus")  -- U+00B1
  , ("²", "sup2",          "2")  -- U+00B2
  , ("³", "sup3",          "3")  -- U+00B3
  , ("´", "acute",         "")  -- U+00B4
  , ("µ", "micro",         "micro")  -- U+00B5 MICRO SIGN
  , ("¶", "para",          "")  -- U+00B6 PILCROW SIGN
  , ("·", "middot",        "")  -- U+00B7
  , ("¸", "cedil",         "")  -- U+00B8
  , ("¹", "sup1",          "1")  -- U+00B9
  , ("º", "ordm",          "")  -- U+00BA
  , ("»", "raquo",         "")  -- U+00BB
  , ("¼", "frac14",        "")  -- U+00BC
  , ("½", "frac12",        "")  -- U+00BD
  , ("¾", "frac34",        "")  -- U+00BE
  , ("¿", "iquest",        "")  -- U+00BF
  , ("À", "Agrave",        "A")  -- U+00C0
  , ("Á", "Aacute",        "A")  -- U+00C1
  , ("Â", "Acirc",         "A")  -- U+00C2
  , ("Ã", "Atilde",        "A")  -- U+00C3
  , ("Ä", "Auml",          "A")  -- U+00C4
  , ("Å", "Aring",         "A")  -- U+00C5
  , ("Æ", "AElig",         "AE")  -- U+00C6 LATIN CAPITAL LETTER AE
  , ("Ç", "Ccedil",        "C")  -- U+00C7
  , ("È", "Egrave",        "E")  -- U+00C8
  , ("É", "Eacute",        "E")  -- U+00C9
  , ("Ê", "Ecirc",         "E")  -- U+00CA
  , ("Ë", "Euml",          "E")  -- U+00CB
  , ("Ì", "Igrave",        "I")  -- U+00CC
  , ("Í", "Iacute",        "I")  -- U+00CD
  , ("Î", "Icirc",         "I")  -- U+00CE
  , ("Ï", "Iuml",          "I")  -- U+00CF
  , ("Ð", "ETH",           "ETH")  -- U+00D0
  , ("Ñ", "Ntilde",        "N")  -- U+00D1
  , ("Ò", "Ograve",        "O")  -- U+00D2
  , ("Ó", "Oacute",        "O")  -- U+00D3
  , ("Ô", "Ocirc",         "O")  -- U+00D4
  , ("Õ", "Otilde",        "O")  -- U+00D5
  , ("Ö", "Ouml",          "O")  -- U+00D6
  , ("×", "times",         binaryOp "times")  -- U+00D7
  , ("Ø", "Oslash",        "O")  -- U+00D8 LATIN CAPITAL LETTER O WITH STROKE
  , ("Ù", "Ugrave",        "U")  -- U+00D9
  , ("Ú", "Uacute",        "U")  -- U+00DA
  , ("Û", "Ucirc",         "U")  -- U+00DB
  , ("Ü", "Uuml",          "U")  -- U+00DC
  , ("Ý", "Yacute",        "Y")  -- U+00DD
  , ("Þ", "THORN",         "THORN")  -- U+00DE
  , ("ß", "szlig",         "ss")  -- U+00DF LATIN SMALL LETTER SHARP S
  , ("à", "agrave",        "a")  -- U+00E0
  , ("á", "aacute",        "a")  -- U+00E1
  , ("â", "acirc",         "a")  -- U+00E2
  , ("ã", "atilde",        "a")  -- U+00E3
  , ("ä", "auml",          "a")  -- U+00E4
  , ("å", "aring",         "a")  -- U+00E5
  , ("æ", "aelig",         "ae")  -- U+00E6 LATIN SMALL LETTER AE
  , ("ç", "ccedil",        "c")  -- U+00E7
  , ("è", "egrave",        "e")  -- U+00E8
  , ("é", "eacute",        "e")  -- U+00E9
  , ("ê", "ecirc",         "e")  -- U+00EA
  , ("ë", "euml",          "e")  -- U+00EB
  , ("ì", "igrave",        "i")  -- U+00EC
  , ("í", "iacute",        "i")  -- U+00ED
  , ("î", "icirc",         "i")  -- U+00EE
  , ("ï", "iuml",          "i")  -- U+00EF
  , ("ð", "eth",           "eth") -- U+00F0
  , ("ñ", "ntilde",        "n")  -- U+00F1
  , ("ò", "ograve",        "o")  -- U+00F2
  , ("ó", "oacute",        "o")  -- U+00F3
  , ("ô", "ocirc",         "o")  -- U+00F4
  , ("õ", "otilde",        "o")  -- U+00F5
  , ("ö", "ouml",          "o")  -- U+00F6
  , ("÷", "divide",        "")   -- U+00F7
  , ("ø", "oslash",        "o")  -- U+00F8 LATIN SMALL LETTER O WITH STROKE
  , ("ù", "ugrave",        "u")  -- U+00F9
  , ("ú", "uacute",        "u")  -- U+00FA
  , ("û", "ucirc",         "u")  -- U+00FB
  , ("ü", "uuml",          "u")  -- U+00FC
  , ("ý", "yacute",        "y")  -- U+00FD
  , ("þ", "thorn",         "thorn")  -- U+00FE
  , ("ÿ", "yuml",          "y")  -- U+00FF
  , ("Ā", "Amacr",         "A")  -- U+0100
  , ("ā", "amacr",         "a")  -- U+0101
  , ("Ă", "Abreve",        "A")  -- U+0102
  , ("ă", "abreve",        "a")  -- U+0103
  , ("Ą", "Aogon",         "A")  -- U+0104
  , ("ą", "aogon",         "a")  -- U+0105
  , ("Ć", "Cacute",        "c")  -- U+0106
  , ("ć", "cacute",        "c")  -- U+0107
  , ("Ĉ", "Ccirc",         "C")  -- U+0108
  , ("ĉ", "ccirc",         "c")  -- U+0109
  , ("Ċ", "Cdot",          "C")  -- U+010A
  , ("ċ", "cdot",          "c")  -- U+010B
  , ("Č", "Ccaron",        "C")  -- U+010C
  , ("č", "ccaron",        "c")  -- U+010D
  , ("Ď", "Dcaron",        "D")  -- U+010E
  , ("ď", "dcaron",        "d")  -- U+010F
  , ("Đ", "Dstrok",        "D")  -- U+0110
  , ("đ", "dstrok",        "d")  -- U+0111
  , ("Ē", "Emacr",         "E")  -- U+0112
  , ("ē", "emacr",         "e")  -- U+0113
  -- NB. To add a test case if U+0114 is added.
  , ("ĕ", noName,          "e")  -- U+0115
  , ("Ė", "Edot",          "e")  -- U+0116
  , ("ė", "edot",          "e")  -- U+0117
  , ("Ę", "Eogon",         "e")  -- U+0118
  , ("ę", "eogon",         "e")  -- U+0119
  , ("Ě", "Ecaron",        "E")  -- U+011A
  , ("ě", "ecaron",        "e")  -- U+011B
  , ("Ĝ", "Gcirc",         "G")  -- U+011C
  , ("ĝ", "gcirc",         "g")  -- U+011D
  , ("Ğ", "Gbreve",        "G")  -- U+011E
  , ("ğ", "gbreve",        "g")  -- U+011F
  , ("Ġ", "Gdot",          "G")  -- U+0120
  , ("ġ", "gdot",          "g")  -- U+0121
  , ("Ģ", "Gcedil",        "G")  -- U+0122
  , ("ģ", "gcedil",        "g")  -- U+0123 LATIN SMALL LETTER G WITH CEDILLA
  , ("Ĥ", "Hcirc",         "H")  -- U+0124
  , ("ĥ", "hcirc",         "h")  -- U+0125
  , ("Ħ", "Hstrok",        "H")  -- U+0126
  , ("ħ", "Hstrok",        "h")  -- U+0127
  -- NB. To add a test case if U+0128 or U+0128 are added.
  , ("Ī", "Imacr",         "I")  -- U+012A
  , ("ī", "imacr",         "I")  -- U+012B
  -- NB. To add a test case if U+012C isadded.
  , ("ĭ", noName,          "i")  -- U+012
  , ("Į", "Iogon",         "I")  -- U+012E
  , ("į", "iogon",         "I")  -- U+012F
  , ("İ", "Idot",          "I")  -- U+0130
  , ("ı", "inodot",        "i")  -- U+0131
  , ("Ĳ", "IJlog",         "IJ")  -- U+0132 LATIN CAPITAL LIGATURE IJ
  , ("ĳ", "ijlig",         "ij")  -- U+0133 LATIN SMALL LIGATURE IJ
  , ("Ĵ", "Jcirc",         "J")  -- U+0134
  , ("ĵ", "jcirc",         "j")  -- U+0135
  , ("Ķ", "Kcedil",        "K")  -- U+0136
  , ("ķ", "kcedil",        "k")  -- U+0137
  , ("ĸ", "kgreen",        "k")  -- U+0138 LATIN SMALL LETTER KRA
  -- NB. To add a test case if U+0139 or U+013A are added.
  , ("Ļ", "Lcedil",        "L")  -- U+013B
  , ("ļ", "lcedil",        "l")  -- U+013C
  -- NB. To add a test case if U+013D, U+013E, U+013F or U+0140 are added.
  , ("Ł", "Lstrok",        "L")  -- U+0141
  , ("ł", "lstrok",        "l")  -- U+0142
  , ("Ń", "Nacute",        "N")  -- U+0143
  , ("ń", "nacute",        "n")  -- U+0144
  , ("Ņ", "Ncedil",        "n")  -- U+0145
  , ("ņ", "ncedil",        "n")  -- U+0146
  , ("Ň", "Ncaron",        "N")  -- U+0147
  , ("ň", "ncaron",        "n")  -- U+0148
  -- NB. To add a test case if U+0149 is added.
  , ("Ŋ", "ENG",           "ENG")  -- U+014A
  , ("ŋ", "eng",           "eng")  -- U+014B
  , ("Ō", "Omacr",         "O")  -- U+014C
  , ("ō", "omacr",         "o")  -- U+014D
  , ("Ő", "Odblac",        "O")  -- U+0150
  , ("ő", "odblac",        "o")  -- U+0151
  , ("Œ", "OElig",         "OE")  -- U+0152 LATIN CAPITAL LIGATURE OE
  , ("œ", "oelig",         "oe")  -- U+0153 LATIN SMALL LIGATURE OE
  -- NB. To add a test case if U+0154 or U+0155 are added.
  , ("Ŗ", "Rcedil",        "R")  -- U+0156
  , ("ŗ", "rcedil",        "r")  -- U+0157
  , ("Ř", "Rcaron",        "R")  -- U+0158
  , ("ř", "rcaron",        "r")  -- U+0159
  , ("Ś", "Sacute",        "S")  -- U+015A
  , ("ś", "sacute",        "s")  -- U+015B
  , ("Ŝ", "Scirc",         "S")  -- U+015C
  , ("ŝ", "scirc",         "s")  -- U+015D
  , ("Ş", "Scedil",        "S")  -- U+015E
  , ("ş", "scedil",        "s")  -- U+015F
  , ("Š", "Scaron",        "S")  -- U+0160
  , ("š", "scaron",        "s")  -- U+0161
  -- NB. To add a test case if U+0162 or U+0164 are added.
  , ("Ť", "Tcaron",        "T")  -- U+0164
  , ("ť", "tcaron",        "t")  -- U+0165
  , ("Ŧ", "Tstrok",        "T")  -- U+0166
  , ("ŧ", "tstrok",        "t")  -- U+0167
  -- NB. To add a test case if U+0168 or U+0169 are added.
  , ("Ū", "Umacr",         "U")  -- U+016A
  , ("ū", "Umacr",         "u")  -- U+016B
  , ("Ŭ", "Ubreve",        "U")  -- U+016C
  , ("ŭ", "ubreve",        "u")  -- U+016D
  , ("Ů", "Uring",         "U")  -- U+016E
  , ("ů", "uring",         "u")  -- U+016F
  , ("Ű", "Udblac",        "U")  -- U+0170
  , ("ű", "udblac",        "u")  -- U+0171
  , ("Ų", "Uogon",         "U")  -- U+0172
  , ("ų", "uogon",         "u")  -- U+0173
  , ("Ŵ", "wcirc",         "W")  -- U+0174
  , ("ŵ", "wcirc",         "w")  -- U+0175
  , ("Ŷ", "ycirc",         "Y")  -- U+0176
  , ("ŷ", "ycirc",         "y")  -- U+0177
  , ("Ÿ", "Yuml",          "Y")  -- U+0178
  , ("Ź", "Zacute",        "Z")  -- U+0179
  , ("ź", "zacute",        "z")  -- U+017A
  , ("Ż", "Zdot",          "Z")  -- U+017B
  , ("ż", "zdot",          "z")  -- U+017C
  , ("Ž", "Zcaron",        "z")  -- U+017D
  , ("ž", "zcaron",        "z")  -- U+017E
  -- NB. To add a test case if U+017F is added.
  , ("Ə", noName,          "A")  -- U+018F LATIN CAPITAL LETTER SCHWA
  , ("Ơ", noName,          "O")  -- U+01A0 LATIN CAPITAL LETTER O WITH HORN
  , ("ơ", noName,          "o")  -- U+01A1 LATIN SMALL LETTER O WITH HORN
  , ("Ư", noName,          "U")  -- U+01AF LATIN CAPITAL LETTER U WITH HORN
  , ("ư", noName,          "u")  -- U+01B0 LATIN SMALL LETTER U WITH HORN
  , ("Ǟ", noName,          "A")  -- U+01DE LATIN CAPITAL LETTER A WITH DIAERESIS AND MACRON
  , ("ǟ", noName,          "a")  -- U+01DF LATIN SMALL LETTER A WITH DIAERESIS AND MACRON
  , ("Ș", noName,          "S")  -- U+0218 LATIN CAPITAL LETTER S WITH COMMA BELOW
  , ("ș", noName,          "s")  -- U+0219 LATIN SMALL LETTER S WITH COMMA BELOW
  , ("Ț", noName,          "T")  -- U+021A LATIN CAPITAL LETTER T WITH COMMA BELOW
  , ("ț", noName,          "t")  -- U+021B LATIN SMALL LETTER T WITH COMMA BELOW
  , ("Ȫ", noName,          "O")  -- U+022A LATIN CAPITAL LETTER O WITH DIAERESIS AND MACRON
  , ("ȫ", noName,          "o")  -- U+022B LATIN SMALL LETTER O WITH DIAERESIS AND MACRON
  , ("Ȭ", noName,          "O")  -- U+022C LATIN CAPITAL LETTER O WITH TILDE AND MACRON
  , ("ȭ", noName,          "o")  -- U+022D LATIN SMALL LETTER O WITH TILDE AND MACRON
  , ("Ȯ", noName,          "O")  -- U+022E LATIN CAPITAL LETTER O WITH DOT ABOVE
  , ("ȯ", noName,          "o")  -- U+022F LATIN SMALL LETTER O WITH DOT ABOVE
  , ("Ȱ", noName,          "O")  -- U+0230 LATIN CAPITAL LETTER O WITH DOT ABOVE AND MACRON
  , ("ȱ", noName,          "o")  -- U+0231 LATIN SMALL LETTER O WITH DOT ABOVE AND MACRON
  , ("Ȳ", noName,          "Y")  -- U+0232 LATIN CAPITAL LETTER Y WITH MACRON
  , ("ȳ", noName,          "y")  -- U+0233 LATIN SMALL LETTER Y WITH MACRON
  , ("ə", noSupported,     "a")  -- U+0253 LATIN SMALL LETTER SCHWA
  , ("Α", "Alpha",         "Alpha")    -- U+0391
  , ("Β", "Beta",          "Beta")     -- U+0392
  , ("Γ", "Gamma",         "Gamma")    -- U+0393
  , ("Δ", "Delta",         "Delta")    -- U+0394
  , ("Ε", "Epsilon",       "Epsilon")  -- U+0395
  , ("Ζ", "Zeta",          "Zeta")     -- U+0396
  , ("Η", "Eta",           "Eta")      -- U+0397
  , ("Θ", "Theta",         "Theta")    -- U+0398
  , ("Ι", "Iota",          "Iota")     -- U+0399
  , ("Κ", "Kappa",         "Kappa")    -- U+039A
  , ("Λ", "Lambda",        "Lambda")   -- U+039B
  , ("Μ", "Mu",            "Mu")       -- U+039C
  , ("Ν", "Nu",            "Nu")       -- U+039D
  , ("Ξ", "Xi",            "Xi")       -- U+039E
  , ("Ο", "Omicron",       "Omicron")  -- U+039F
  , ("Π", "Pi",            "Pi")       -- U+03A0
  , ("Ρ", "Rho",           "Rho")      -- U+03A1
  -- The code U+03A2 is reserved.
  , ("Σ", "Sigma",         "Sigma")    -- U+03A3
  , ("Τ", "Tau",           "Tau")      -- U+03A4
  , ("Υ", "Upsilon",       "Upsilon")  -- U+03A5
  , ("Φ", "Phi",           "Phi")      -- U+03A6
  , ("χ", "Chi",           "Chi")      -- U+03A7
  , ("Ψ", "Psi",           "Psi")      -- U+03A8
  , ("Ω", "Omega",         "Omega")    -- U+03A9
  , ("α", "alpha",         "alpha")    -- U+03B1
  , ("β", "beta",          "beta")     -- U+03B2
  , ("γ", "gamma",         "gamma")    -- U+03B3
  , ("δ", "delta",         "delta")    -- U+03B4
  , ("ε", "epsilon",       "epsilon")  -- U+03B5
  , ("ζ", "zeta",          "zeta")     -- U+03B6
  , ("η", "eta",           "eta")      -- U+03B7
  , ("θ", "theta",         "theta")    -- U+03B8
  , ("ι", "iota",          "iota")     -- U+03B9
  , ("κ", "kappa",         "kappa")    -- U+03BA
  , ("λ", "lambda",        "lambda")   -- U+03BB
  , ("μ", "mu",            "mu")       -- U+03BC
  , ("ν", "nu",            "nu")       -- U+03BD
  , ("ξ", "xi",            "xi")       -- U+03BE
  , ("ο", "omicron",       "omicron")  -- U+03BF
  , ("π", "pi",            "pi")       -- U+03C0
  , ("ρ", "rho",           "rho")      -- U+03C1
  , ("ς", "sigmaf",        "sigma")    -- U+03C2 GREEK SMALL LETTER FINAL SIGMA
  , ("σ", "sigma",         "sigma")    -- U+03C3
  , ("τ", "tau",           "tau")      -- U+03C4
  , ("υ", "upsilon",       "upsilon")  -- U+03C5
  , ("φ", "phi",           "phi")      -- U+03C6
  , ("χ", "chi",           "chi")      -- U+03C7
  , ("ψ", "psi",           "psi")      -- U+03C8
  , ("ω", "omega",         "omega")    -- U+03C9
  , ("ϑ", "thetasym",      "theta")    -- U+03D1 GREEK THETA SYMBOL
  , ("ϒ", "upsih",         "Upsilon")  -- U+03D2 GREEK UPSILON WITH HOOK SYMBOL
  , ("ϕ", "straightphi",   "phi")      -- U+03D5 GREEK PHI SYMBOL
  , ("ϖ", "piv",           "pi")       -- U+03D6 GREEK PI SYMBOL
  , ("Ḑ", noSupported,     "D")  -- U+1E10 LATIN CAPITAL LETTER D WITH CEDILLA
  , ("ḑ", noSupported,     "d")  -- U+1E11 LATIN SMALL LETTER D WITH CEDILLA
  , ("Ẁ", noSupported,     "W")  -- U+1E80 LATIN CAPITAL LETTER W WITH GRAVE
  , ("ẁ", noSupported,     "w")  -- U+1E81 LATIN SMALL LETTER W WITH GRAVE
  , ("Ẃ", noSupported,     "W")  -- U+1E82 LATIN CAPITAL LETTER W WITH ACUTE
  , ("ẃ", noSupported,     "w")  -- U+1E83 LATIN SMALL LETTER W WITH ACUTE
  , ("ẞ", noSupported,     "SS")  -- U+1E9E LATIN CAPITAL LETTER SHARP S
  , (" ", "thinsp",        "-")  -- U+2009 THIN SPACE
  , ("‐", noName,          "-")  -- U+2010 HYPHEN
  , ("–", "ndash",         "-")  -- U+2013 EN DASH
  , ("—", "mdash",         "-")  -- U+2014 EM DASH
  , ("‘", "lsquo",         "")  -- U+2018 LEFT SINGLE QUOTATION MARK
  , ("’", "rsquo",         "")  -- U+2019 RIGHT SINGLE QUOTATION MARK
  , ("‚", "sbquo",         "")  -- U+201A SINGLE LOW-9 QUOTATION MARK
  , ("‛", noName,          "")  -- U+201B SINGLE HIGH-REVERSED-9 QUOTATION MARK
  , ("“", "ldquo",         "")  -- U+201C LEFT DOUBLE QUOTATION MARK
  , ("”", "rdquo",         "")  -- U+201D RIGHT DOUBLE QUOTATION MARK
  , ("„", "bdquo",         "")  -- U+201E DOUBLE LOW-9 QUOTATION MARK
  , ("‟", noName,          "")  -- U+201F DOUBLE HIGH-REVERSED-9 QUOTATION MARK
  , ("†", "dagger",        "")  -- U+2020
  , ("‡", "Dagger",        "")  -- U+2021
  , ("•", "bull",          "")  -- U+2022
  , ("…", "hellip",        "")  -- U+2026 HORIZONTAL ELLIPSIS
  , ("⁰", noSupported,     "0")  -- U+2070
  , ("ⁱ", noSupported,     "i")  -- U+2071
  -- The codes U+2072 and U+2073 are reserved.
  , ("⁴", noSupported,     "4")  -- U+2074
  , ("⁵", noSupported,     "5")  -- U+2075
  , ("⁶", noSupported,     "6")  -- U+2076
  , ("⁷", noSupported,     "7")  -- U+2077
  , ("⁸", noSupported,     "8")  -- U+2078
  , ("⁹", noSupported,     "9")  -- U+2079
  , ("₀", noSupported,     "0")  -- U+2080
  , ("₁", noSupported,     "1")  -- U+2081
  , ("₂", noSupported,     "2")  -- U+2082
  , ("₃", noSupported,     "3")  -- U+2083
  , ("₄", noSupported,     "4")  -- U+2074
  , ("₅", noSupported,     "5")  -- U+2075
  , ("₆", noSupported,     "6")  -- U+2076
  , ("₇", noSupported,     "7")  -- U+2077
  , ("₈", noSupported,     "8")  -- U+2078
  , ("₉", noSupported,     "9")  -- U+2079
  , ("ℂ", noName,          "C")  -- U+2102 DOUBLE-STRUCK CAPITAL C
  , ("ℊ", noName,          "g")  -- U+210A SCRIPT SMALL G
  , ("ℋ", noName,          "H")  -- U+210B SCRIPT CAPITAL H
  , ("ℍ", noName,          "H")  -- U+210D DOUBLE-STRUCK CAPITAL H
  , ("ℐ", noName,          "I")  -- U+2110 SCRIPT CAPITAL I
  , ("ℒ", noName,          "L")  -- U+2112 SCRIPT CAPITAL L
  , ("ℓ", noName,          "l")  -- U+2113 SCRIPT SMALL L
  , ("ℕ", noName,          "N")  -- U+2115 DOUBLE-STRUCK CAPITAL N
  , ("℘", "weierp",        "P")  -- U+2118 SCRIPT CAPITAL P
  , ("ℙ", noName,          "P")  -- U+2119 DOUBLE-STRUCK CAPITAL P
  , ("ℚ", noName,          "Q")  -- U+211A DOUBLE-STRUCK CAPITAL Q
  , ("ℛ", noName,          "R")  -- U+211B SCRIPT CAPITAL R
  , ("ℝ", noName,          "R")  -- U+211D DOUBLE-STRUCK CAPITAL R
  , ("™", "trade",         "")  -- U+2122
  , ("ℤ", noName,          "Z")  -- U+2124 DOUBLE-STRUCK CAPITAL Z
  , ("ℬ", noName,          "B")  -- U+212C SCRIPT CAPITAL B
  , ("ℯ", noName,          "e")  -- U+212F SCRIPT SMALL E
  , ("ℰ", noName,          "E")  -- U+2130 SCRIPT CAPITAL E
  , ("ℱ", noName,          "F")  -- U+2131 SCRIPT CAPITAL F
  , ("ℳ", noName,          "M")  -- U+2133 SCRIPT CAPITAL M
  , ("ℴ", noName,          "o")  -- U+2134 SCRIPT SMALL O
  , ("ℵ", "alefsym",       "aleph")  -- U+2135
  , ("ℶ", noName,          "beth")  -- U+2136
  , ("←", "larr",          binaryOp "arrow")  -- U+2190 LEFTWARDS ARROW
  , ("↑", "uarr",          binaryOp "arrow")  -- U+2191 UPWARDS ARROW
  , ("→", "rarr",          binaryOp "arrow")  -- U+2192 RIGHTWARDS ARROW
  , ("↓", "ldrr",          binaryOp "arrow")  -- U+2193 DOWNWARDS ARROW
  , ("↔", "harr",          binaryOp "arrow")  -- U+2194 LEFT RIGHT ARROW
  , ("∀", "forall",        unaryOp "for-all")  -- U+2200
  , ("∃", "exist",         unaryOp "exists")  -- U+2203
  , ("−", "minus",         "minus")  -- U+2212
  , ("∓", noName,          binaryOp "minus-or-plus")  -- U+2213
  , ("√", "radic",         unaryOp "sqrt")  -- U+221A
  , ("⊂", "sub",           binaryOp "subset-of")  -- U+2282
  , ("⊃", "sup",           binaryOp "superset-of-")  -- U+2283
  , ("⊄", "nsub",          "")  -- U+2284
  , ("⊅", noName,          "")  -- U+2285
  , ("⊆", "sube",          binaryOp "subset-of-or-equal-to")  -- U+2286
  , ("⊇", "supe",          binaryOp "superset-of-or-equal-to")  -- U+2287
  , ("⊈", noName,          "")  -- U+2288
  , ("⊉", noName,          "")  -- U+2289
  , ("⊊", noName,          "")  -- U+228A
  , ("⊋", noName,          "")  -- U+228B
  , ("⊕", "oplus",         binaryOp "circled-plus")  -- U+2295
  , ("⊗", "otimes",        binaryOp "circled-times")  -- U+2297
  , ("⋉", noName,          binaryOp "left-semidirect-product")  -- U+22C9) LEFT NORMAL FACTOR SEMIDIRECT PRODUCT
  , ("⋊", noName,          binaryOp "right-semidirect-product")  -- U+22CA RIGHT NORMAL FACTOR SEMIDIRECT PRODUCT
  , ("⌝", noSupported,     "")  -- U+231D TOP RIGHT CORNER
  , ("�", noSupported,    "")  -- U+FFFD REPLACEMENT CHARACTER
  ]
