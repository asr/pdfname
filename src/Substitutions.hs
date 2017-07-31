-- | Substitutions.

module Substitutions
  ( authorSubst
  , removeFromTitle
  , replace
  , replaceHTMLEscapedText
  , unicodeSubst
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
replaceHTMLEscapedText = replace $ htmlNameSubst ++ numericSubst
  where
  htmlDecSubst ∷ [(Text, Text)]
  htmlDecSubst = substHelper "&#%d;"

  html4HexSubst ∷ [(Text, Text)]
  html4HexSubst = substHelper "&#x%04X;"

  html5HexSubst ∷ [(Text, Text)]
  html5HexSubst = substHelper "&#x%05X;"

  numericSubst ∷ [(Text, Text)]
  numericSubst = htmlDecSubst ++ html4HexSubst ++ html5HexSubst

  substHelper ∷ String → [(Text, Text)]
  substHelper format = map (\ (_, b) → (printfHelper format b, b)) htmlNameSubst
    where
    printfHelper ∷ String → Text → Text
    printfHelper pFormat t = T.pack $ printf pFormat (ord $ T.head t)

replace ∷ [(Text,Text)] → Text → Text
replace xs ys = foldl (flip (uncurry T.replace)) ys xs

unaryOp ∷ Text → Text
unaryOp t = T.snoc t '-'

binaryOp ∷ Text → Text
binaryOp t = T.cons '-' $ T.snoc t '-'

------------------------------------------------------------------------------
-- HTML entities and symbols substitutions

noName ∷ Text
noName = "N/A"

-- | Substitutions of HTML-escaped text (entities and symbols).

-- We use @noName@ when the HTML symbol has not an entity name.
htmlNameSubst ∷ [(Text, Text)]
htmlNameSubst =
  [ (noName,      "!")   -- U+0021
  , ("&quot;",    "\"")  -- U+0022
  , (noName,      "#")   -- U+0023
  , (noName,      "$")   -- U+0024
  , (noName,      "%")   -- U+0025
  , ("&amp;",     "&")   -- U+0026
  , (noName,      "'")   -- U+0027
  , (noName,      "(")   -- U+0028
  , (noName,      ")")   -- U+0029
  , (noName,      "*")   -- U+002A
  , (noName,      "+")   -- U+002B
  , (noName,      ",")   -- U+002C
  -- We do not substitute U+002D.
  , (noName,      ".")   -- U+002E
  , (noName,      "/")   -- U+002F
  , (noName,      ":")   -- U+003A
  , (noName,      ";")   -- U+003B
  , ("&lt;",      "<")   -- U+003C
  , (noName,      "=")   -- U+003D
  , ("&gt;",      ">")   -- U+003E
  , (noName,      "?")   -- U+003F
  , (noName,      "@")   -- U+0040
  , ("&plusmn;",  "±")   -- U+00B1
  , ("&sup2;",    "²")   -- U+00B2
  , ("&sup3;",    "³")   -- U+00B3
  , ("&sup1;",    "¹")   -- U+00B9
  , ("&Agrave;",  "À")   -- U+00C0
  , ("&Aacute;",  "Á")   -- U+00C1
  , ("&Acirc;",   "Â")   -- U+00C2
  , ("&Atilde;",  "Ã")   -- U+00C3
  , ("&Auml;",    "Ä")   -- U+00C4
  , ("&Aring;",   "Å")   -- U+00C5
  , ("&AElig;",   "Æ")   -- U+00C6
  , ("&Ccedil;",  "Ç")   -- U+00C7
  , ("&Egrave;",  "È")   -- U+00C8
  , ("&Eacute;",  "É")   -- U+00C9
  , ("&Ecirc;",   "Ê")   -- U+00CA
  , ("&Euml;",    "Ë")   -- U+00CB
  , ("&Igrave;",  "Ì")   -- U+00CC
  , ("&Iacute;",  "Í")   -- U+00CD
  , ("&Icirc;",   "Î")   -- U+00CE
  , ("&Iuml;",    "Ï")   -- U+00CF
  , ("&ETH;",     "Ð")   -- U+00D0
  , ("&Ntilde;",  "Ñ")   -- U+00D1
  , ("&Ograve;",  "Ò")   -- U+00D2
  , ("&Oacute;",  "Ó")   -- U+00D3
  , ("&Ocirc;",   "Ô")   -- U+00D4
  , ("&Otilde;",  "Õ")   -- U+00D5
  , ("&Ouml;",    "Ö")   -- U+00D6
  , ("&times;",   "×")   -- U+00D7
  , ("&Oslash;",  "Ø")   -- U+00D8
  , ("&Ugrave;",  "Ù")   -- U+00D9
  , ("&Uacute;",  "Ú")   -- U+00DA
  , ("&Ucirc;",   "Û")   -- U+00DB
  , ("&Uuml;",    "Ü")   -- U+00DC
  , ("&Yacute;",  "Ý")   -- U+00DD
  , ("&THORN;",   "Þ")   -- U+00DE
  , ("&szlig;",   "ß")   -- U+00DF
  , ("&agrave;",  "à")   -- U+00E0
  , ("&aacute;",  "á")   -- U+00E1
  , ("&acirc;",   "â")   -- U+00E2
  , ("&atilde;",  "ã")   -- U+00E3
  , ("&auml;",    "ä")   -- U+00E4
  , ("&aring;",   "å")   -- U+00E5
  , ("&aelig;",   "æ")   -- U+00E6
  , ("&ccedil;",  "ç")   -- U+00E7
  , ("&egrave;",  "è")   -- U+00E8
  , ("&eacute;",  "é")   -- U+00E9
  , ("&ecirc;",   "ê")   -- U+00EA
  , ("&euml;",    "ë")   -- U+00EB
  , ("&igrave;",  "ì")   -- U+00EC
  , ("&iacute;",  "í")   -- U+00ED
  , ("&icirc;",   "î")   -- U+00EE
  , ("&iuml;",    "ï")   -- U+00EF
  , ("&eth;",     "ð")   -- U+00F0
  , ("&ntilde;",  "ñ")   -- U+00F1
  , ("&ograve;",  "ò")   -- U+00F2
  , ("&oacute;",  "ó")   -- U+00F3
  , ("&ocirc;",   "ô")   -- U+00F4
  , ("&otilde;",  "õ")   -- U+00F5
  , ("&ouml;",    "ö")   -- U+00F6
  , ("&divide;",  "÷")   -- U+00F7
  , ("&oslash;",  "ø")   -- U+00F8
  , ("&ugrave;",  "ù")   -- U+00F9
  , ("&uacute;",  "ú")   -- U+00FA
  , ("&ucirc;",   "û")   -- U+00FB
  , ("&uuml;",    "ü")   -- U+00FC
  , ("&yacute;",  "ý")   -- U+00FD
  , ("&thorn;",   "þ")   -- U+00FE
  , ("&yuml;",    "ÿ")   -- U+00FF
  , (noName,      "ĕ")   -- U+0115
  , ("&Imacr;",   "Ī")   -- U+012A
  , ("&imacr;",   "ī")   -- U+012B
  , (noName,      "ĭ")   -- U+012D
  , ("&Sacute;",  "Ś")   -- U+015A
  , ("&sacute;",  "ś")   -- U+015B
  , ("&Scedil;",  "Ş")   -- U+015E
  , ("&scedil;",  "ş")   -- U+015F
  , ("&Scaron;",  "Š")   -- U+0160
  , ("&scaron;",  "š")   -- U+0161
  , ("&ubreve;",  "ŭ")   -- U+016D
  , ("&Alpha;",   "Α")   -- U+0391
  , ("&Beta;",    "Β")   -- U+0392
  , ("&Gamma;",   "Γ")   -- U+0393
  , ("&Delta;",   "Δ")   -- U+0394
  , ("&Epsilon;", "Ε")   -- U+0395
  , ("&Zeta;",    "Ζ")   -- U+0396
  , ("&Eta;",     "Η")   -- U+0397
  , ("&Theta;",   "θ")   -- U+0398
  , ("&Iota;",    "Ι")   -- U+0399
  , ("&Kappa;",   "Κ")   -- U+03AA
  , ("&Lambda;",  "Λ")   -- U+03AB
  , ("&Mu;",      "Μ")   -- U+03AC
  , ("&Nu;",      "Ν")   -- U+03AD
  , ("&Xi;",      "Ξ")   -- U+03AE
  , ("&Omicron;", "Ο")   -- U+03AF
  , ("&Pi;",      "Π")   -- U+03A0
  , ("&Rho;",     "Ρ")   -- U+03A1
  -- The code U+03A2 is reserved.
  , ("&Sigma;",   "Σ")   -- U+03A3
  , ("&Tau;",     "Τ")   -- U+03A4
  , ("&Upsilon;", "Υ")   -- U+03A5
  , ("&Phi;",     "Φ")   -- U+03A6
  , ("&Chi;",     "Χ")   -- U+03A7
  , ("&Psi;",     "Ψ")   -- U+03A8
  , ("&Omega;",   "Ω")   -- U+03A9
  , ("&alpha;",   "α")   -- U+03B1
  , ("&beta;",    "β")   -- U+03B2
  , ("&gamma;",   "γ")   -- U+03B3
  , ("&delta;",   "δ")   -- U+03B4
  , ("&epsilon;", "ε")   -- U+03B5
  , ("&zeta;",    "ζ")   -- U+03B6
  , ("&eta;",     "η")   -- U+03B7
  , ("&theta;",   "θ")   -- U+03B8
  , ("&iota;",    "ι")   -- U+03B9
  , ("&kappa;",   "κ")   -- U+03BA
  , ("&lambda;",  "λ")   -- U+03BB
  , ("&mu;",      "μ")   -- U+03BC
  , ("&nu;",      "ν")   -- U+03BD
  , ("&xi;",      "ξ")   -- U+03BE
  , ("&omicron;", "ο")   -- U+03BF
  , ("&pi;",      "π")   -- U+03C0
  , ("&rho;",     "ρ")   -- U+03C1
  , ("&sigmaf;",  "ς")   -- U+03C2
  , ("&sigma;",   "σ")   -- U+03C3
  , ("&tau;",     "τ")   -- U+03C4
  , ("&upsilon;", "υ")   -- U+03C5
  , ("&phi;",     "φ")   -- U+03C6
  , ("&chi;",     "χ")   -- U+03C7
  , ("&psi;",     "ψ")   -- U+03C8
  , ("&omega;",   "ω")   -- U+03C9
  , (noName,      "‐")   -- U+2010
  , ("&ndash;",   "–")   -- U+2013
  , ("&mdash;",   "—")   -- U+2014
  , ("&lsquo;",   "‘")   -- U+2018
  , ("&rsquo;",   "’")   -- U+2019
  , ("&sbquo;",   "‚")   -- U+201A
  , (noName,      "‛")   -- U+201B
  , ("&ldquo;",   "“")   -- U+201C
  , ("&rdquo;",   "”")   -- U+201D
  , ("&bdquo;",   "„")   -- U+201E
  , (noName,      "‟")   -- U+201F
  , ("&dagger;",  "†")   -- U+2020
  , ("&Dagger;",  "‡")   -- U+2021
  , ("&bull;",    "•")   -- U+2022
  , ("&hellip;",  "…")   -- U+2026
  , (noName,      "ℂ")   -- U+2102
  , (noName,      "ℕ")   -- U+2115
  , (noName,      "ℚ")   -- U+211A
  , (noName,      "ℝ")   -- U+211D
  , ("&trade;",   "™")   -- U+2122
  , (noName,      "ℤ")   -- U+2124
  , ("&alefsym;", "ℵ")   -- U+2135
  , ("&forall;",  "∀")   -- U+2200
  , ("&exist;",   "∃")   -- U+2203
  , (noName,      "∓")   -- U+2213
  , ("&sub;",     "⊂")   -- U+2282
  , ("&sup;",     "⊃")   -- U+2283
  , ("&nsub;",    "⊄")   -- U+2284
  , (noName,      "⊅")   -- U+2285
  , ("&sube;",    "⊆")   -- U+2286
  , ("&supe;",    "⊇")   -- U+2287
  , (noName,      "⊈")   -- U+2288
  , (noName,      "⊉")   -- U+2289
  , (noName,      "⊊")   -- U+228A
  , (noName,      "⊋")   -- U+228B
  , (noName,      "⌝")   -- U+231D
  ]

------------------------------------------------------------------------------
-- Unicode substitutions

-- | Unicode substitutions.
unicodeSubst ∷ [(Text,Text)]
unicodeSubst =
  [ ("\t", "")  -- U+0009 CHARACTER TABULATION
  , ("\n", "")  -- U+000A LINE FEED (LF)
  , ("\f", "")  -- U+000C FORM FEED (FF)
  , ("\r", "")  -- U+000D CARRIAGE RETURN (CR)
  , (" ",  "-") -- U+0020 SPACE
  , ("!",  "")  -- U+0021 EXCLAMATION MARK
  , ("\"", "")  -- U+0022 QUOTATION MARK
  , ("#",  "")  -- U+0023 NUMBER SIGN
  , ("$",  "")  -- U+0024 DOLLAR SIGN
  , ("%",  "")  -- U+0025 PERCENT SIGN
  , ("&",  "")  -- U+0026 AMPERSAND
  , ("'",  "")  -- U+0027 APOSTROPHE
  , ("(",  "")  -- U+0028 LEFT PARENTHESIS
  , (")",  "")  -- U+0029 RIGHT PARENTHESIS
  , ("*",  "")  -- U+002A ASTERISK
  , ("+",  binaryOp "plus")  -- U+002B PLUS SIGN
  , (",",  "")  -- U+002C COMMA
  -- We do not substitute U+002D HYPHEN-MINUS.
  , (".",  "")  -- U+002E FULL STOP
  , ("/",  "")  -- U+002F SOLIDUS
  , (":",  "")  -- U+003A COLON
  , (";",  "")  -- U+003B SEMICOLON
  , ("<",  binaryOp "less-than")  -- U+003C LESS-THAN SIGN
  , ("=",  binaryOp "equals")  -- U+003D EQUALS SIGN
  , (">",  binaryOp "greater-than")  -- U+003E GREATER-THAN SIGN
  , ("?",  "")  -- U+003F QUESTION MARK
  , ("@",  "")  -- U+0040 COMMERCIAL AT
  , ("[",  "")  -- U+005B LEFT SQUARE BRACKET
  , ("\\", "")  -- U+005C REVERSE SOLIDUS
  , ("]",  "")  -- U+005D RIGHT SQUARE BRACKET
  , ("^",  "")  -- U+005E CIRCUMFLEX ACCENT
  , ("_",  "-")  -- U+005F LOW LINE
  , ("`",  "")  -- U+0060 GRAVE ACCENT
  , ("{",  "")  -- U+007B LEFT CURLY BRACKET
  , ("|",  "")  -- U+007C VERTICAL LINE
  , ("}",  "")  -- U+007B RIGHT CURLY BRACKET
  , ("~",  "")  -- U+007E TILDE
  , ("¡",  "")  -- U+00A1 INVERTED EXCLAMATION MARK
  , ("¬",  "")  -- U+00AC NOT SIGN
  , ("±",  binaryOp "plus-minus")  -- U+00B1 PLUS-MINUS SIGN
  , ("²",  "2")  -- U+00B2 SUPERSCRIPT TWO
  , ("³",  "3")  -- U+00B3 SUPERSCRIPT THREE
  , ("¹",  "1")  -- U+00B9 SUPERSCRIPT ONE
  , ("À",  "A")  -- U+00C0 LATIN CAPITAL LETTER A WITH GRAVE
  , ("Á",  "A")  -- U+00C1 LATIN CAPITAL LETTER A WITH ACUTE
  , ("Â",  "A")  -- U+00C2 LATIN CAPITAL LETTER A WITH CIRCUMFLEX
  , ("Ã",  "A")  -- U+00C3 LATIN CAPITAL LETTER A WITH TILDE
  , ("Ä",  "A")  -- U+00C4 LATIN CAPITAL LETTER A WITH DIAERESIS
  , ("Å",  "A")  -- U+00C5 LATIN CAPITAL LETTER A WITH RING ABOVE
  , ("Æ",  "AE")  -- U+00C6 LATIN CAPITAL LETTER AE
  , ("Ç",  "C")  -- U+00C7 LATIN CAPITAL LETTER C WITH CEDILLA
  , ("È",  "E")  -- U+00C8 LATIN CAPITAL LETTER E WITH GRAVE
  , ("É",  "E")  -- U+00C9 LATIN CAPITAL LETTER E WITH ACUTE
  , ("Ê",  "E")  -- U+00CA LATIN CAPITAL LETTER E WITH CIRCUMFLEX
  , ("Ë",  "E")  -- U+00CB LATIN CAPITAL LETTER E WITH DIAERESIS
  , ("Ì",  "I")  -- U+00CC LATIN CAPITAL LETTER I WITH GRAVE
  , ("Í",  "I")  -- U+00CD LATIN CAPITAL LETTER I WITH ACUTE
  , ("Î",  "I")  -- U+00CE LATIN CAPITAL LETTER I WITH CIRCUMFLEX
  , ("Ï",  "I")  -- U+00CF LATIN CAPITAL LETTER I WITH DIAERESIS
  , ("Ð",  "ETH")  -- U+00D0 LATIN CAPITAL LETTER ETH
  , ("Ñ",  "N")  -- U+00D1 LATIN CAPITAL LETTER N WITH TILDE
  , ("Ò",  "O")  -- U+00D2 LATIN CAPITAL LETTER O WITH GRAVE
  , ("Ó",  "O")  -- U+00D3 LATIN CAPITAL LETTER O WITH ACUTE
  , ("Ô",  "O")  -- U+00D4 LATIN CAPITAL LETTER O WITH CIRCUMFLEX
  , ("Õ",  "O")  -- U+00D5 LATIN CAPITAL LETTER O WITH TILDE
  , ("Ö",  "O")  -- U+00D6 LATIN CAPITAL LETTER O WITH DIAERESIS
  , ("×",  binaryOp "times")  -- U+00D7 MULTIPLICATION SIGN
  , ("Ø",  "O")  -- U+00D8 LATIN CAPITAL LETTER O WITH STROKE
  , ("Ù",  "U")  -- U+00D9 LATIN CAPITAL LETTER U WITH GRAVE
  , ("Ú",  "U")  -- U+00DA LATIN CAPITAL LETTER U WITH ACUTE
  , ("Û",  "U")  -- U+00DB LATIN CAPITAL LETTER U WITH CIRCUMFLEX
  , ("Ü",  "U")  -- U+00DC LATIN CAPITAL LETTER U WITH DIAERESIS
  , ("Ý",  "Y")  -- U+00DD LATIN CAPITAL LETTER Y WITH ACUTE
  , ("Þ",  "THORN")  -- U+00DE LATIN CAPITAL LETTER THORN
  , ("ß",  "ss")  -- U+00DF LATIN SMALL LETTER SHARP S
  , ("à",  "a")  -- U+00E0 LATIN SMALL LETTER A WITH GRAVE
  , ("á",  "a")  -- U+00E1 LATIN SMALL LETTER A WITH ACUTE
  , ("â",  "a")  -- U+00E2 LATIN SMALL LETTER A CIRCUMFLEX
  , ("ã",  "a")  -- U+00E3 LATIN SMALL LETTER A WITH TILDE
  , ("ä",  "a")  -- U+00E4 LATIN SMALL LETTER A WITH DIAERESIS
  , ("å",  "a")  -- U+00E5 LATIN SMALL LETTER A WITH RING ABOVE
  , ("æ",  "ae")  -- U+00E6 LATIN SMALL LETTER AE
  , ("ç",  "c")  -- U+00E7 LATIN SMALL LETTER C WITH CEDILLA
  , ("è",  "e")  -- U+00E8 LATIN SMALL LETTER E WITH GRAVE
  , ("é",  "e")  -- U+00E9 LATIN SMALL LETTER E WITH ACUTE
  , ("ê",  "e")  -- U+00EA LATIN SMALL LETTER E WITH CIRCUMFLEX
  , ("ë",  "e")  -- U+00EB LATIN SMALL LETTER E WITH DIAERESIS
  , ("ì",  "i")  -- U+00EC LATIN SMALL LETTER I WITH GRAVE
  , ("í",  "i")  -- U+00ED LATIN SMALL LETTER I WITH ACUTE
  , ("î",  "i")  -- U+00EE LATIN SMALL LETTER I WITH CIRCUMFLEX
  , ("ï",  "i")  -- U+00EF LATIN SMALL LETTER I WITH DIAERESIS
  , ("ð",  "eth") -- U+00F0 LATIN SMALL LETTER ETH
  , ("ñ",  "n")  -- U+00F1 LATIN SMALL LETTER N WITH TILDE
  , ("ò",  "o")  -- U+00F2 LATIN SMALL LETTER O WITH GRAVE
  , ("ó",  "o")  -- U+00F3 LATIN SMALL LETTER O WITH ACUTE
  , ("ô",  "o")  -- U+00F4 LATIN SMALL LETTER O WITH CIRCUMFLEX
  , ("õ",  "o")  -- U+00F5 LATIN SMALL LETTER O WITH TILDE
  , ("ö",  "o")  -- U+00F6 LATIN SMALL LETTER O WITH DIAERESIS
  , ("÷",  "")   -- U+00F7 DIVISION SIGN
  , ("ø",  "o")  -- U+00F8 LATIN SMALL LETTER O WITH STROKE
  , ("ù",  "u")  -- U+00F9 LATIN SMALL LETTER U WITH GRAVE
  , ("ú",  "u")  -- U+00FA LATIN SMALL LETTER U WITH ACUTE
  , ("û",  "u")  -- U+00FB LATIN SMALL LETTER U WITH CIRCUMFLEX
  , ("ü",  "u")  -- U+00FC LATIN SMALL LETTER U WITH DIAERESIS
  , ("ý",  "y")  -- U+00FD LATIN SMALL LETTER Y WITH ACUTE
  , ("þ",  "thorn")  -- U+00FE LATIN SMALL LETTER THORN
  , ("ÿ",  "y")  -- U+00FF LATIN SMALL LETTER Y WITH DIAERESIS
  , ("Ā",  "A")  -- U+0100 LATIN CAPITAL LETTER A WITH MACRON
  , ("ā",  "a")  -- U+0101 LATIN SMALL LETTER A WITH MACRON
  , ("Ă",  "A")  -- U+0102 LATIN CAPITAL LETTER A WITH BREVE
  , ("ă",  "a")  -- U+0103 LATIN SMALL LETTER A WITH
  , ("Ą",  "A")  -- U+0104 LATIN CAPITAL LETTER A WITH OGONEK
  , ("ą",  "a")  -- U+0105 LATIN SMALL LETTER A WITH OGONEK
  , ("Ć",  "c")  -- U+0106 LATIN CAPITAL LETTER C WITH ACUTE
  , ("ć",  "c")  -- U+0107 LATIN SMALL LETTER C WITH ACUTE
  , ("Č",  "C")  -- U+010C LATIN CAPITAL LETTER C WITH CARON
  , ("č",  "c")  -- U+010D LATIN SMALL LETTER C WITH CARON
  , ("Ď",  "D")  -- U+010E LATIN CAPITAL LETTER D WITH CARON
  , ("ď",  "d")  -- U+010F LATIN SMALL LETTER D WITH CARON
  , ("Ē",  "E")  -- U+0112 LATIN CAPITAL LETTER E WITH MACRON
  , ("ē",  "e")  -- U+0113 LATIN SMALL LETTER E WITH MACRON
  , ("ĕ",  "e")  -- U+0115 LATIN SMALL LETTER E WITH BREVE
  , ("Ę",  "e")  -- U+0118 LATIN CAPITAL LETTER E WITH OGONEK
  , ("ę",  "e")  -- U+0119 LATIN SMALL LETTER E WITH OGONEK
  , ("Ě",  "E")  -- U+011A LATIN CAPITAL LETTER E WITH CARON
  , ("ě",  "e")  -- U+011B LATIN SMALL LETTER E WITH CARON
  , ("Ğ",  "G")  -- U+011E LATIN CAPITAL LETTER G WITH BREVE
  , ("ğ",  "g")  -- U+011F LATIN SMALL LETTER G WITH BREVE
  , ("Ģ",  "G")  -- U+0122 LATIN CAPITAL LETTER G WITH CEDILLA
  , ("ģ",  "g")  -- U+0123 LATIN SMALL LETTER G WITH CEDILLA
  , ("Ī",  "I")  -- U+012A LATIN CAPITAL LETTER I WITH MACRON
  , ("ī",  "I")  -- U+012B LATIN SMALL LETTER I WITH MACRON
  , ("ĭ",  "i")  -- U+012D LATIN SMALL LETTER I WITH BREVE
  , ("İ",  "I")  -- U+0130 LATIN CAPITAL LETTER I WITH DOT ABOVE
  , ("Ķ",  "K")  -- U+0136 LATIN CAPITAL LETTER K WITH CEDILLA
  , ("ķ",  "k")  -- U+0137 LATIN SMALL LETTER K WITH CEDILLA
  , ("Ļ",  "L")  -- U+013B LATIN CAPITAL LETTER L WITH CEDILLA
  , ("ļ",  "l")  -- U+013C LATIN SMALL LETTER L WITH CEDILLA
  , ("Ł",  "L")  -- U+0141 LATIN CAPITAL LETTER L WITH STROKE
  , ("ł",  "l")  -- U+0142 LATIN SMALL LETTER L WITH STROKE
  , ("Ń",  "N")  -- U+0143 LATIN CAPITAL LETTER N WITH ACUTE
  , ("ń",  "n")  -- U+0144 LATIN SMALL LETTER N WITH ACUTE
  , ("Ņ",  "n")  -- U+0145 LATIN CAPITAL LETTER N WITH CEDILLA
  , ("ņ",  "n")  -- U+0146 LATIN SMALL LETTER N WITH CEDILLA
  , ("Ň",  "N")  -- U+0147 LATIN CAPITAL LETTER N WITH CARON
  , ("ň",  "n")  -- U+0148 LATIN SMALL LETTER N WITH CARON
  , ("Ő",  "O")  -- U+0150 LATIN CAPITAL LETTER O WITH DOUBLE ACUTE
  , ("ő",  "o")  -- U+0151 LATIN SMALL LETTER O WITH DOUBLE ACUTE
  , ("Œ",  "OE")  -- U+0152 LATIN CAPITAL LIGATURE OE
  , ("œ",  "oe")  -- U+0153 LATIN SMALL LIGATURE OE
  , ("Ř",  "R")  -- U+0158 LATIN CAPITAL LETTER R WITH CARON
  , ("ř",  "r")  -- U+0159 LATIN SMALL LETTER R WITH CARON
  , ("Ś",  "S")  -- U+015A LATIN CAPITAL LETTER S WITH ACUTE
  , ("ś",  "s")  -- U+015B LATIN SMALL LETTER S WITH ACUTE
  , ("Ş",  "S")  -- U+015E LATIN CAPITAL LETTER S WITH CEDILLA
  , ("ş",  "s")  -- U+015F LATIN SMALL LETTER S WITH CEDILLA
  , ("Š",  "S")  -- U+0160 LATIN CAPITAL LETTER S WITH CARON
  , ("š",  "s")  -- U+0161 LATIN SMALL LETTER S WITH CARON
  , ("Ť",  "T")  -- U+0164 LATIN CAPITAL LETTER T WITH CARON
  , ("ť",  "t")  -- U+0165 LATIN SMALL LETTER T WITH CARON
  , ("Ū",  "U")  -- U+016A LATIN CAPITAL LETTER U WITH MACRON
  , ("ŭ",  "u")  -- U+016D LATIN SMALL LETTER U WITH BREVE
  , ("ū",  "u")  -- U+016B LATIN SMALL LETTER U WITH MACRON
  , ("Ů",  "U")  -- U+016E LATIN CAPITAL LETTER U WITH RING ABOVE
  , ("ů",  "u")  -- U+016F LATIN SMALL LETTER U WITH RING ABOVE
  , ("Ű",  "U")  -- U+0170 LATIN CAPITAL LETTER U WITH DOUBLE ACUTE
  , ("ű",  "u")  -- U+0171 LATIN SMALL LETTER U WITH DOUBLE ACUTE
  , ("Ÿ",  "Y")  -- U+0178 LATIN CAPITAL LETTER Y WITH DIAERESIS
  , ("Ź",  "Z")  -- U+0179 LATIN CAPITAL LETTER Z WITH ACUTE
  , ("ź",  "z")  -- U+017A LATIN SMALL LETTER Z WITH ACUTE
  , ("Ż",  "Z")  -- U+017B LATIN CAPITAL LETTER Z WITH DOT ABOVE
  , ("ż",  "z")  -- U+017C LATIN SMALL LETTER Z WITH DOT ABOVE
  , ("Ž",  "z")  -- U+017D LATIN CAPITAL LETTER Z WITH CARON
  , ("ž",  "z")  -- U+017E LATIN SMALL LETTER Z WITH CARON
  , ("Ə",  "A")  -- U+018F LATIN CAPITAL LETTER SCHWA
  , ("Ș",  "S")  -- U+0218 LATIN CAPITAL LETTER S WITH COMMA BELOW
  , ("ș",  "s")  -- U+0219 LATIN SMALL LETTER S WITH COMMA BELOW
  , ("Ț",  "T")  -- U+021A LATIN CAPITAL LETTER T WITH COMMA BELOW
  , ("ț",  "t")  -- U+021B LATIN SMALL LETTER T WITH COMMA BELOW
  , ("ə",  "a")  -- U+0259 LATIN SMALL LETTER SCHWA
  , ("Α",  "Alpha")    -- U+0391 GREEK CAPITAL LETTER ALPHA
  , ("Β",  "Beta")     -- U+0392 GREEK CAPITAL LETTER BETA
  , ("Γ",  "Gamma")    -- U+0393 GREEK CAPITAL LETTER GAMMA
  , ("Δ",  "Delta")    -- U+0394 GREEK CAPITAL LETTER DELTA
  , ("Ε",  "Epsilon")  -- U+0395 GREEK CAPITAL LETTER EPSILON
  , ("Ζ",  "Zeta")     -- U+0396 GREEK CAPITAL LETTER ZETA
  , ("Η",  "Eta")      -- U+0397 GREEK CAPITAL LETTER ETA
  , ("Θ",  "Theta")    -- U+0398 GREEK CAPITAL LETTER THETA
  , ("Ι",  "Iota")     -- U+0399 GREEK CAPITAL LETTER IOTA
  , ("Κ",  "Kappa")    -- U+039A GREEK CAPITAL LETTER KAPPA
  , ("Λ",  "Lambda")   -- U+039B GREEK CAPITAL LETTER LAMDA
  , ("Μ",  "Mu")       -- U+039C GREEK CAPITAL LETTER MU
  , ("Ν",  "Nu")       -- U+039D GREEK CAPITAL LETTER NU
  , ("Ξ",  "Xi")       -- U+039E GREEK CAPITAL LETTER ZI
  , ("Ο",  "Omicron")  -- U+039F GREEK CAPITAL LETTER OMICRON
  , ("Π",  "Pi")       -- U+03A0 GREEK CAPITAL LETTER PI
  , ("Ρ",  "Rho")      -- U+03A1 GREEK CAPITAL LETTER RHO
  -- The code U+03A2 is reserved.
  , ("Σ",  "Sigma")    -- U+03A3 GREEK CAPITAL LETTER SIGMA
  , ("Τ",  "Tau")      -- U+03A4 GREEK CAPITAL LETTER TAU
  , ("Υ",  "Upsilon")  -- U+03A5 GREEK CAPITAL LETTER UPSILON
  , ("Φ",  "Phi")      -- U+03A6 GREEK CAPITAL LETTER PHI
  , ("χ",  "Chi")      -- U+03A7 GREEK CAPITAL LETTER CHI
  , ("Ψ",  "Psi")      -- U+03A8 GREEK CAPITAL LETTER PSI
  , ("Ω",  "Omega")    -- U+03A9 GREEK CAPITAL LETTER OMEGA
  , ("α",  "alpha")    -- U+03B1 GREEK SMALL LETTER ALPHA
  , ("β",  "beta")     -- U+03B2 GREEK SMALL LETTER BETA
  , ("γ",  "gamma")    -- U+03B3 GREEK SMALL LETTER GAMMA
  , ("δ",  "delta")    -- U+03B4 GREEK SMALL LETTER DELTA
  , ("ε",  "epsilon")  -- U+03B5 GREEK SMALL LETTER EPSILON
  , ("ζ",  "zeta")     -- U+03B6 GREEK SMALL LETTER ZETA
  , ("η",  "eta")      -- U+03B7 GREEK SMALL LETTER ETA
  , ("θ",  "theta")    -- U+03B8 GREEK SMALL LETTER THETA
  , ("ι",  "iota")     -- U+03B9 GREEK SMALL LETTER IOTA
  , ("κ",  "kappa")    -- U+03BA GREEK SMALL LETTER KAPPA
  , ("λ",  "lambda")   -- U+03BB GREEK SMALL LETTER LAMDA
  , ("μ",  "mu")       -- U+03BC GREEK SMALL LETTER MU
  , ("ν",  "nu")       -- U+03BD GREEK SMALL LETTER NU
  , ("ξ",  "xi")       -- U+03BE GREEK SMALL LETTER ZI
  , ("ο",  "omicron")  -- U+03BF GREEK SMALL LETTER OMICRON
  , ("π",  "pi")       -- U+03C0 GREEK SMALL LETTER PI
  , ("ρ",  "rho")      -- U+03C1 GREEK SMALL LETTER RHO
  , ("ς",  "sigma")    -- U+03C2 GREEK SMALL LETTER FINAL SIGMA
  , ("σ",  "sigma")    -- U+03C3 GREEK SMALL LETTER SIGMA
  , ("τ",  "tau")      -- U+03C4 GREEK SMALL LETTER TAU
  , ("υ",  "upsilon")  -- U+03C5 GREEK SMALL LETTER UPSILON
  , ("φ",  "phi")      -- U+03C6 GREEK SMALL LETTER PHI
  , ("χ",  "chi")      -- U+03C7 GREEK SMALL LETTER CHI
  , ("ψ",  "psi")      -- U+03C8 GREEK SMALL LETTER PSI
  , ("ω",  "omega")    -- U+03C9 GREEK SMALL LETTER OMEGA
  , ("ẞ",  "SS")  -- U+1E9E LATIN CAPITAL LETTER SHARP S
  , (" ",  "-")  -- U+2009 THIN SPACE
  , ("‐",  "-")  -- U+2010 HYPHEN
  , ("–",  "-")  -- U+2013 EN DASH
  , ("—",  "-")  -- U+2014 EM DASH
  , ("‘",  "")  -- U+2018 LEFT SINGLE QUOTATION MARK
  , ("’",  "")  -- U+2019 RIGHT SINGLE QUOTATION MARK
  , ("‚",  "")  -- U+201A SINGLE LOW-9 QUOTATION MARK
  , ("‛",  "")  -- U+201B SINGLE HIGH-REVERSED-9 QUOTATION MARK
  , ("“",  "")  -- U+201C LEFT DOUBLE QUOTATION MARK
  , ("”",  "")  -- U+201D RIGHT DOUBLE QUOTATION MARK
  , ("„",  "")  -- U+201E DOUBLE LOW-9 QUOTATION MARK
  , ("‟",  "")  -- U+201F DOUBLE HIGH-REVERSED-9 QUOTATION MARK
  , ("†",  "")  -- U+2020 DAGGER
  , ("‡",  "")  -- U+2021 DOUBLE DAGGER
  , ("•",  "")  -- U+2022 BULLET
  , ("…",  "")  -- U+2026 HORIZONTAL ELLIPSIS
  , ("⁰",  "0")  -- U+2070 SUPERSCRIPT ZERO
  , ("ⁱ",  "i")  -- U+2071 SUPERSCRIPT LATIN SMALL LETTER I
  -- The codes U+2072 and U+2073 are reserved.
  , ("⁴",  "4")  -- U+2074 SUPERSCRIPT FOUR
  , ("⁵",  "5")  -- U+2075 SUPERSCRIPT FIVE
  , ("⁶",  "6")  -- U+2076 SUPERSCRIPT SIX
  , ("⁷",  "7")  -- U+2077 SUPERSCRIPT SEVEN
  , ("⁸",  "8")  -- U+2078 SUPERSCRIPT EIGHT
  , ("⁹",  "9")  -- U+2079 SUPERSCRIPT NINE
  , ("₀",  "0")  -- U+2080 SUBSCRIPT ZERO
  , ("₁",  "1")  -- U+2081 SUBSCRIPT ONE
  , ("₂",  "2")  -- U+2082 SUBSCRIPT TWO
  , ("₃",  "3")  -- U+2083 SUBSCRIPT THREE
  , ("₄",  "4")  -- U+2074 SUBSCRIPT FOUR
  , ("₅",  "5")  -- U+2075 SUBSCRIPT FIVE
  , ("₆",  "6")  -- U+2076 SUBSCRIPT SIX
  , ("₇",  "7")  -- U+2077 SUBSCRIPT SEVEN
  , ("₈",  "8")  -- U+2078 SUBSCRIPT EIGHT
  , ("₉",  "9")  -- U+2079 SUBSCRIPT NINE
  , ("ℂ",  "C")  -- U+2102 DOUBLE-STRUCK CAPITAL C
  , ("ℕ",  "N")  -- U+2115 DOUBLE-STRUCK CAPITAL N
  , ("ℚ",  "Q")  -- U+211A DOUBLE-STRUCK CAPITAL Q
  , ("ℝ",  "R")  -- U+211D DOUBLE-STRUCK CAPITAL R
  , ("™",  "")  -- U+2122 TRADE MARK SIGN
  , ("ℤ",  "Z")  -- U+2124 DOUBLE-STRUCK CAPITAL Z
  , ("ℵ",  "aleph")  -- U+2135 ALEF SYMBOL
  , ("ℶ",  "beth")  -- U+2136 BET SYMBOL
  , ("∀",  unaryOp "for-all")  -- U+2200 FOR ALL
  , ("∃",  unaryOp "exists")  -- U+2203 THERE EXISTS
  , ("∓",  binaryOp "minus-or-plus")  -- U+2213 MINUS-OR-PLUS SIGN
  , ("⊂",  "")  -- U+2282 SUBSET OF
  , ("⊃",  "")  -- U+2283 SUPERSET OF
  , ("⊄",  "")  -- U+2284 NOT A SUBSET OF
  , ("⊅",  "")  -- U+2285 NOT A SUPERSET OF
  , ("⊆",  "")  -- U+2286 SUBSET OF OR EQUAL TO
  , ("⊇",  "")  -- U+2287 SUPERSET OF OR EQUAL TO
  , ("⊈",  "")  -- U+2288 NEITHER A SUBSET OF NOR EQUAL TO
  , ("⊉",  "")  -- U+2289 NEITHER A SUPERSET OF NOR EQUAL TO
  , ("⊊",  "")  -- U+228A SUBSET OF WITH NOT EQUAL TO
  , ("⊋",  "")  -- U+228B SUPERSET OF WITH NOT EQUAL TO
  , ("⌝",  "")  -- U+231D TOP RIGHT CORNER
  , ("�", "")  -- U+FFFD REPLACEMENT CHARACTER
  ]

------------------------------------------------------------------------------
-- Author and title substitutions

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
  -- We erase `Ã¶` because it follows an `o` in the examples we know.
  , ("Ã¶",      "")     -- U+00C3 and U+00B6 (LATIN SMALL LETTER O WITH DIAERESIS)
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
