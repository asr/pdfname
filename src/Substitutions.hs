-- | Substitutions.

module Substitutions
  ( authorSubst
  , titleSubst
  , replace
  , replaceHTMLEntities
  , replaceHTMLSymbols
  , unicodeSubst
  , weirdSubst
  ) where

import Data.Char ( ord )

import Data.Text ( Text )
import qualified Data.Text as T

import Text.Printf ( printf )

------------------------------------------------------------------------------
-- Replacements

-- These substitutions should be done before the Unicode
-- substitutions.
replaceHTMLEntities ∷ Text → Text
replaceHTMLEntities =
  replace (entityNameSubst ++ entityDecSubst ++ entityHexSubst)
  where
   entityDecSubst ∷ [(Text, Text)]
   entityDecSubst = map (\ (_, b) → (format "&#%d;" b, b)) entityNameSubst

   entityHexSubst ∷ [(Text, Text)]
   entityHexSubst = map (\ (_, b) → (format "&#x%04X;" b, b)) entityNameSubst

   format ∷ String → Text → Text
   format f u = T.pack $ printf f (ord $ T.head u)

-- These substitutions should be done before the Unicode
-- substitutions.
replaceHTMLSymbols ∷ Text → Text
replaceHTMLSymbols = replace decSubst . replace hexSubst
  where
   decSubst ∷ [(Text, Text)]
   decSubst = map (\ (a, _, c) → (a, c)) htmlSymbolSubst

   hexSubst ∷ [(Text, Text)]
   hexSubst = map (\ (_, b, c) → (b, c)) htmlSymbolSubst

replace ∷ [(Text,Text)] → Text → Text
replace xs ys = foldl (flip (uncurry T.replace)) ys xs

------------------------------------------------------------------------------
-- HTML entities and symbols substitutions

-- Used by example for the Journal of Functional Programmning.

-- | Substitutions of HTML entities.
entityNameSubst ∷ [(Text, Text)]
entityNameSubst =
  [ ("&Acirc;",   "Â")  -- U+00C2
  , ("&Auml;",    "Ä")  -- U+00C4
  , ("&Ccedil;",  "Ç")  -- U+00C7
  , ("&Euml;",    "Ë")  -- U+00CB
  , ("&Iuml;",    "Ï")  -- U+00CF
  , ("&Ouml;",    "Ö")  -- U+00D6
  , ("&Oslash;",  "Ø")  -- U+00D8
  , ("&Uuml;",    "Ü")  -- U+00DC
  , ("&aacute;",  "á")  -- U+00E1
  , ("&acirc;",   "â")  -- U+00E2
  , ("&auml;",    "ä")  -- U+00E4
  , ("&ccedil;",  "ç")  -- U+00E7
  , ("&eacute;",  "é")  -- U+00E9
  , ("&euml;",    "ë")  -- U+00EB
  , ("&iacute;",  "í")  -- U+00ED
  , ("&iuml;",    "ï")  -- U+00EF
  , ("&oacute;",  "ó")  -- U+00F3
  , ("&ouml;",    "ö")  -- U+00F6
  , ("&oslash;",  "ø")  -- U+00F8
  , ("&uacute;",  "ú")  -- U+00FA
  , ("&uuml;",    "ü")  -- U+252
  , ("&Imacr;",   "Ī")  -- U+012A
  , ("&imacr;",   "ī")  -- U+012B
  , ("&Sacute;",  "Ś")  -- U+015A
  , ("&sacute;",  "ś")  -- U+015B
  , ("&Scedil;",  "Ş")  -- U+015E
  , ("&scedil;",  "ş")  -- U+015F
  , ("&Scaron;",  "Š")  -- U+0160
  , ("&scaron;",  "š")  -- U+0161
  , ("&alpha;",   "α")  -- U+03B1
  , ("&beta;",    "β")  -- U+03B2
  , ("&gamma;",   "γ")  -- U+03B3
  , ("&delta;",   "δ")  -- U+03B4
  , ("&epsilon;", "ε")  -- U+03B5
  , ("&zeta;",    "ζ")  -- U+03B6
  , ("&eta;",     "η")  -- U+03B7
  , ("&theta;",   "θ")  -- U+03B8
  , ("&iota;",    "ι")  -- U+03B9
  , ("&kappa;",   "κ")  -- U+03BA
  , ("&lambda;",  "λ")  -- U+03BB
  , ("&mu;",      "μ")  -- U+03BC
  , ("&nu;",      "ν")  -- U+03BD
  , ("&xi;",      "ξ")  -- U+03BE
  , ("&omicron;", "ο")  -- U+03BF
  , ("&pi;",      "π")  -- U+03C0
  , ("&rho;",     "ρ")  -- U+03C1
  , ("&sigmaf;",  "ς")  -- U+03C2
  , ("&sigma;",   "σ")  -- U+03C3
  , ("&tau;",     "τ")  -- U+03C4
  , ("&upsilon;", "υ")  -- U+03C5
  , ("&phi;",     "φ")  -- U+03C6
  , ("&chi;",     "χ")  -- U+03C7
  , ("&psi;",     "ψ")  -- U+03C8
  , ("&omega;",   "ω")  -- U+03C9
  , ("&ndash;",   "–")  -- U+2013
  , ("&mdash;",   "—")  -- U+2014
  , ("&lsquo;",   "‘")  -- U+2018
  , ("&rsquo;",   "’")  -- U+2019
  , ("&sbquo;",   "‚")  -- U+201A
  , ("&ldquo;",   "“")  -- U+201C
  , ("&rdquo;",   "”")  -- U+201D
  , ("&bdquo;",   "„")  -- U+201E
  , ("&dagger;",  "†")  -- U+2020
  , ("&Dagger;",  "‡")  -- U+2021
  , ("&bull;",    "•")  -- U+2022
  , ("&hellip;",  "…")  -- U+2026
  , ("&sup;",     "⊃")  -- U+x2283
  ]

-- | Substitutions of HTML symbols.
htmlSymbolSubst ∷ [(Text, Text, Text)]
htmlSymbolSubst =
  [ ("&#8208;", "&#x2010;", "‐")
  , ("&#8219;", "&#x201B;", "‛")
  , ("&#8223;", "&#x201F;", "‟")
  , ("&#8989;", "&#x231D;", "⌝")
  ]

------------------------------------------------------------------------------
-- Unicode substitutions

-- | Unicode substitutions.
unicodeSubst ∷ [(Text,Text)]
unicodeSubst =
  [ ("\t",          "")         -- U+0009 CHARACTER TABULATION
  , ("\n",          "")         -- U+000A LINE FEED (LF)
  , ("\f",          "")         -- U+000C FORM FEED (FF)
  , ("\r",          "")         -- U+000D CARRIAGE RETURN (CR)
  , (" ",           "-")        -- U+0020 SPACE
  , ("!",           "")         -- U+0021 EXCLAMATION MARK
  , ("\"",          "")         -- U+0022 QUOTATION MARK
  , ("#",           "")         -- U+0023 NUMBER SIGN
  , ("$",           "")         -- U+0024 DOLLAR SIGN
  , ("%",           "")         -- U+0025 PERCENT SIGN
  , ("&",           "")         -- U+0026 AMPERSAND
  , ("'",           "")         -- U+0027 APOSTROPHE
  , ("(",           "")         -- U+0028 LEFT PARENTHESIS
  , (")",           "")         -- U+0029 RIGHT PARENTHESIS
  , ("*",           "")         -- U+002A ASTERISK
  , ("+",           "")         -- U+002B PLUS SIGN
  , (",",           "")         -- U+002C COMMA
  , (".",           "")         -- U+002D FULL STOP
  , ("/",           "")         -- U+002F SOLIDUS
  , (":",           "")         -- U+003A COLON
  , (";",           "")         -- U+003B SEMICOLON
  , ("<",           "")         -- U+003C LESS-THAN SIGN
  , ("=",           "")         -- U+003D EQUALS SIGN
  , (">",           "")         -- U+003E GREATER-THAN SIGN
  , ("?",           "")         -- U+003F QUESTION MARK
  , ("@",           "")         -- U+0040 COMMERCIAL AT
  , ("[",           "")         -- U+005B LEFT SQUARE BRACKET
  , ("\\",          "")         -- U+005C REVERSE SOLIDUS
  , ("]",           "")         -- U+005D RIGHT SQUARE BRACKET
  , ("^",           "")         -- U+005E CIRCUMFLEX ACCENT
  , ("_",           "-")        -- U+005F LOW LINE
  , ("`",           "")         -- U+0060 GRAVE ACCENT
  , ("{",           "")         -- U+007B LEFT CURLY BRACKET
  , ("|",           "")         -- U+007C VERTICAL LINE
  , ("}",           "")         -- U+007B RIGHT CURLY BRACKET
  , ("~",           "")         -- U+007E TILDE
  , ("¡",           "")         -- U+00A1 INVERTED EXCLAMATION MARK
  , ("¬",           "")         -- U+00AC NOT SIGN
  , ("²",           "2")        -- U+00B2 SUPERSCRIPT TWO
  , ("³",           "3")        -- U+00B3 SUPERSCRIPT THREE
  , ("¹",           "1")        -- U+00B9 SUPERSCRIPT ONE
  , ("À",           "A")        -- U+00C0 LATIN CAPITAL LETTER A WITH GRAVE
  , ("Á",           "A")        -- U+00C1 LATIN CAPITAL LETTER A WITH ACUTE
  , ("Â",           "A")        -- U+00C2 LATIN CAPITAL LETTER A WITH CIRCUMFLEX
  , ("Ã",           "A")        -- U+00C3 LATIN CAPITAL LETTER A WITH TILDE
  , ("Ä",           "A")        -- U+00C4 LATIN CAPITAL LETTER A WITH DIAERESIS
  , ("Å",           "A")        -- U+00C5 LATIN CAPITAL LETTER A WITH RING ABOVE
  , ("Æ",           "AE")       -- U+00C6 LATIN CAPITAL LETTER AE
  , ("Ç",           "C")        -- U+00C7 LATIN CAPITAL LETTER C WITH CEDILLA
  , ("È",           "E")        -- U+00C8 LATIN CAPITAL LETTER E WITH GRAVE
  , ("É",           "E")        -- U+00C9 LATIN CAPITAL LETTER E WITH ACUTE
  , ("Ê",           "E")        -- U+00CA LATIN CAPITAL LETTER E WITH CIRCUMFLEX
  , ("Ë",           "E")        -- U+00CB LATIN CAPITAL LETTER E WITH DIAERESIS
  , ("Í",           "I")        -- U+00CD LATIN CAPITAL LETTER I WITH ACUTE
  , ("Î",           "I")        -- U+00CE LATIN CAPITAL LETTER I WITH CIRCUMFLEX
  , ("Ï",           "I")        -- U+00CF LATIN CAPITAL LETTER I WITH DIAERESIS
  , ("Ñ",           "N")        -- U+00D1 LATIN CAPITAL LETTER N WITH TILDE
  , ("Ó",           "O")        -- U+00D3 LATIN CAPITAL LETTER O WITH ACUTE
  , ("Ô",           "O")        -- U+00D4 LATIN CAPITAL LETTER O WITH CIRCUMFLEX
  , ("Õ",           "O")        -- U+00D5 LATIN CAPITAL LETTER O WITH TILDE
  , ("Ö",           "O")        -- U+00D6 LATIN CAPITAL LETTER O WITH DIAERESIS
  , ("×",           "")         -- U+00D7 MULTIPLICATION SIGN
  , ("Ø",           "O")        -- U+00D8 LATIN CAPITAL LETTER O WITH STROKE
  , ("Ù",           "U")        -- U+00D9 LATIN CAPITAL LETTER U WITH GRAVE
  , ("Ú",           "U")        -- U+00DA LATIN CAPITAL LETTER U WITH ACUTE
  , ("Û",           "U")        -- U+00DB LATIN CAPITAL LETTER U WITH CIRCUMFLEX
  , ("Ü",           "U")        -- U+00DC LATIN CAPITAL LETTER U WITH DIAERESIS
  , ("Ý",           "Y")        -- U+00DD LATIN CAPITAL LETTER Y WITH ACUTE
  , ("ß",           "ss")       -- U+00DF LATIN SMALL LETTER SHARP S
  , ("à",           "a")        -- U+00E0 LATIN SMALL LETTER A WITH GRAVE
  , ("á",           "a")        -- U+00E1 LATIN SMALL LETTER A WITH ACUTE
  , ("â",           "a")        -- U+00E2 LATIN SMALL LETTER A CIRCUMFLEX
  , ("ã",           "a")        -- U+00E3 LATIN SMALL LETTER A WITH TILDE
  , ("ä",           "a")        -- U+00E4 LATIN SMALL LETTER A WITH DIAERESIS
  , ("å",           "a")        -- U+00E5 LATIN SMALL LETTER A WITH RING ABOVE
  , ("æ",           "ae")       -- U+00E6 LATIN SMALL LETTER AE
  , ("ç",           "c")        -- U+00E7 LATIN SMALL LETTER C WITH CEDILLA
  , ("è",           "e")        -- U+00E8 LATIN SMALL LETTER E WITH GRAVE
  , ("é",           "e")        -- U+00E9 LATIN SMALL LETTER E WITH ACUTE
  , ("ê",           "e")        -- U+00EA LATIN SMALL LETTER E WITH CIRCUMFLEX
  , ("ë",           "e")        -- U+00EB LATIN SMALL LETTER E WITH DIAERESIS
  , ("í",           "i")        -- U+00ED LATIN SMALL LETTER I WITH ACUTE
  , ("î",           "i")        -- U+00EE LATIN SMALL LETTER I WITH CIRCUMFLEX
  , ("ï",           "i")        -- U+00EF LATIN SMALL LETTER I WITH DIAERESIS
  , ("ñ",           "n")        -- U+00F1 LATIN SMALL LETTER N WITH TILDE
  , ("ò",           "o")        -- U+00F2 LATIN SMALL LETTER O WITH GRAVE
  , ("ó",           "o")        -- U+00F3 LATIN SMALL LETTER O WITH ACUTE
  , ("ô",           "o")        -- U+00F4 LATIN SMALL LETTER O WITH CIRCUMFLEX
  , ("õ",           "o")        -- U+00F5 LATIN SMALL LETTER O WITH TILDE
  , ("ö",           "o")        -- U+00F6 LATIN SMALL LETTER O WITH DIAERESIS
  , ("ø",           "o")        -- U+00F8 LATIN SMALL LETTER O WITH STROKE
  , ("ù",           "u")        -- U+00F9 LATIN SMALL LETTER U WITH GRAVE
  , ("ú",           "u")        -- U+00FA LATIN SMALL LETTER U WITH ACUTE
  , ("û",           "u")        -- U+00FB LATIN SMALL LETTER U WITH CIRCUMFLEX
  , ("ü",           "u")        -- U+00FC LATIN SMALL LETTER U WITH DIAERESIS
  , ("ý",           "y")        -- U+00FD LATIN SMALL LETTER Y WITH ACUTE
  , ("þ",           "t")        -- U+00FE LATIN SMALL LETTER THORN
  , ("ÿ",           "y")        -- U+00FF LATIN SMALL LETTER Y WITH DIAERESIS
  , ("Ā",           "A")        -- U+0100 LATIN CAPITAL LETTER A WITH MACRON
  , ("ā",           "a")        -- U+0101 LATIN SMALL LETTER A WITH MACRON
  , ("Ă",           "A")        -- U+0102 LATIN CAPITAL LETTER A WITH BREVE
  , ("ă",           "a")        -- U+0103 LATIN SMALL LETTER A WITH
  , ("Ą",           "A")        -- U+0104 LATIN CAPITAL LETTER A WITH OGONEK
  , ("ą",           "a")        -- U+0105 LATIN SMALL LETTER A WITH OGONEK
  , ("Ć",           "c")        -- U+0106 LATIN CAPITAL LETTER C WITH ACUTE
  , ("ć",           "c")        -- U+0107 LATIN SMALL LETTER C WITH ACUTE
  , ("Č",           "C")        -- U+010C LATIN CAPITAL LETTER C WITH CARON
  , ("č",           "c")        -- U+010D LATIN SMALL LETTER C WITH CARON
  , ("Ď",           "D")        -- U+010E LATIN CAPITAL LETTER D WITH CARON
  , ("ď",           "d")        -- U+010F LATIN SMALL LETTER D WITH CARON
  , ("Ē",           "E")        -- U+0112 LATIN CAPITAL LETTER E WITH MACRON
  , ("ē",           "e")        -- U+0113 LATIN SMALL LETTER E WITH MACRON
  , ("Ę",           "e")        -- U+0118 LATIN CAPITAL LETTER E WITH OGONEK
  , ("ę",           "e")        -- U+0119 LATIN SMALL LETTER E WITH OGONEK
  , ("Ě",           "E")        -- U+011A LATIN CAPITAL LETTER E WITH CARON
  , ("ě",           "e")        -- U+011B LATIN SMALL LETTER E WITH CARON
  , ("Ğ",           "G")        -- U+011E LATIN CAPITAL LETTER G WITH BREVE
  , ("ğ",           "g")        -- U+011F LATIN SMALL LETTER G WITH BREVE
  , ("Ģ",           "G")        -- U+0122 LATIN CAPITAL LETTER G WITH CEDILLA
  , ("ģ",           "g")        -- U+0123 LATIN SMALL LETTER G WITH CEDILLA
  , ("Ī",           "I")        -- U+012A LATIN CAPITAL LETTER I WITH MACRON
  , ("ī",           "I")        -- U+012B LATIN SMALL LETTER I WITH MACRON
  , ("İ",           "I")        -- U+0130 LATIN CAPITAL LETTER I WITH DOT ABOVE
  , ("Ķ",           "K")        -- U+0136 LATIN CAPITAL LETTER K WITH CEDILLA
  , ("ķ",           "k")        -- U+0137 LATIN SMALL LETTER K WITH CEDILLA
  , ("Ļ",           "L")        -- U+013B LATIN CAPITAL LETTER L WITH CEDILLA
  , ("ļ",           "l")        -- U+013C LATIN SMALL LETTER L WITH CEDILLA
  , ("Ł",           "L")        -- U+0141 LATIN CAPITAL LETTER L WITH STROKE
  , ("ł",           "l")        -- U+0142 LATIN SMALL LETTER L WITH STROKE
  , ("Ń",           "N")        -- U+0143 LATIN CAPITAL LETTER N WITH ACUTE
  , ("ń",           "n")        -- U+0144 LATIN SMALL LETTER N WITH ACUTE
  , ("Ņ",           "n")        -- U+0145 LATIN CAPITAL LETTER N WITH CEDILLA
  , ("ņ",           "n")        -- U+0146 LATIN SMALL LETTER N WITH CEDILLA
  , ("Ň",           "N")        -- U+0147 LATIN CAPITAL LETTER N WITH CARON
  , ("ň",           "n")        -- U+0148 LATIN SMALL LETTER N WITH CARON
  , ("Ő",           "O")        -- U+0150 LATIN CAPITAL LETTER O WITH DOUBLE ACUTE
  , ("ő",           "o")        -- U+0151 LATIN SMALL LETTER O WITH DOUBLE ACUTE
  , ("Œ",           "OE")       -- U+0152 LATIN CAPITAL LIGATURE OE
  , ("œ",           "oe")       -- U+0153 LATIN SMALL LIGATURE OE
  , ("Ř",           "R")        -- U+0158 LATIN CAPITAL LETTER R WITH CARON
  , ("ř",           "r")        -- U+0159 LATIN SMALL LETTER R WITH CARON
  , ("Ś",           "S")        -- U+015A LATIN CAPITAL LETTER S WITH ACUTE
  , ("ś",           "s")        -- U+015B LATIN SMALL LETTER S WITH ACUTE
  , ("Ş",           "S")        -- U+015E LATIN CAPITAL LETTER S WITH CEDILLA
  , ("ş",           "s")        -- U+015F LATIN SMALL LETTER S WITH CEDILLA
  , ("Š",           "S")        -- U+0160 LATIN CAPITAL LETTER S WITH CARON
  , ("š",           "s")        -- U+0161 LATIN SMALL LETTER S WITH CARON
  , ("Ť",           "T")        -- U+0164 LATIN CAPITAL LETTER T WITH CARON
  , ("ť",           "t")        -- U+0165 LATIN SMALL LETTER T WITH CARON
  , ("Ū",           "U")        -- U+016A LATIN CAPITAL LETTER U WITH MACRON
  , ("ū",           "u")        -- U+016B LATIN SMALL LETTER U WITH MACRON
  , ("Ů",           "U")        -- U+016E LATIN CAPITAL LETTER U WITH RING ABOVE
  , ("ů",           "u")        -- U+016F LATIN SMALL LETTER U WITH RING ABOVE
  , ("Ű",           "U")        -- U+0170 LATIN CAPITAL LETTER U WITH DOUBLE ACUTE
  , ("ű",           "u")        -- U+0171 LATIN SMALL LETTER U WITH DOUBLE ACUTE
  , ("Ÿ",           "Y")        -- U+0178 LATIN CAPITAL LETTER Y WITH DIAERESIS
  , ("Ź",           "Z")        -- U+0179 LATIN CAPITAL LETTER Z WITH ACUTE
  , ("ź",           "z")        -- U+017A LATIN SMALL LETTER Z WITH ACUTE
  , ("Ż",           "Z")        -- U+017B LATIN CAPITAL LETTER Z WITH DOT ABOVE
  , ("ż",           "z")        -- U+017C LATIN SMALL LETTER Z WITH DOT ABOVE
  , ("Ž",           "z")        -- U+017D LATIN CAPITAL LETTER Z WITH CARON
  , ("ž",           "z")        -- U+017E LATIN SMALL LETTER Z WITH CARON
  , ("Ə",           "A")        -- U+018F LATIN CAPITAL LETTER SCHWA
  , ("Ș",           "S")        -- U+0218 LATIN CAPITAL LETTER S WITH COMMA BELOW
  , ("ș",           "s")        -- U+0219 LATIN SMALL LETTER S WITH COMMA BELOW
  , ("Ț",           "T")        -- U+021A LATIN CAPITAL LETTER T WITH COMMA BELOW
  , ("ț",           "t")        -- U+021B LATIN SMALL LETTER T WITH COMMA BELOW
  , ("ə",           "a")        -- U+0259 LATIN SMALL LETTER SCHWA
  , ("Ω",           "Omega")    -- U+03A9 GREEK CAPITAL LETTER OMEGA
  , ("α",           "alpha")    -- U+03B1 GREEK SMALL LETTER ALPHA
  , ("β",           "beta")     -- U+03B2 GREEK SMALL LETTER BETA
  , ("γ",           "gamma")    -- U+03B3 GREEK SMALL LETTER GAMMA
  , ("δ",           "delta")    -- U+03B4 GREEK SMALL LETTER DELTA
  , ("ε",           "epsilon")  -- U+03B5 GREEK SMALL LETTER EPSILON
  , ("ζ",           "zeta")     -- U+03B6 GREEK SMALL LETTER ZETA
  , ("η",           "eta")      -- U+03B7 GREEK SMALL LETTER ETA
  , ("θ",           "theta")    -- U+03B8 GREEK SMALL LETTER THETA
  , ("ι",           "iota")     -- U+03B9 GREEK SMALL LETTER IOTA
  , ("κ",           "kappa")    -- U+03BA GREEK SMALL LETTER KAPPA
  , ("λ",           "lambda")   -- U+03BB GREEK SMALL LETTER LAMDA
  , ("μ",           "mu")       -- U+03BC GREEK SMALL LETTER MU
  , ("ν",           "nu")       -- U+03BD GREEK SMALL LETTER NU
  , ("ξ",           "xi")       -- U+03BE GREEK SMALL LETTER ZI
  , ("ο",           "omicron")  -- U+03BF GREEK SMALL LETTER OMICRON
  , ("π",           "pi")       -- U+03C0 GREEK SMALL LETTER PI
  , ("ρ",           "rho")      -- U+03C1 GREEK SMALL LETTER RHO
  , ("ς",           "sigma")    -- U+03C2 GREEK SMALL LETTER FINAL SIGMA
  , ("σ",           "sigma")    -- U+03C3 GREEK SMALL LETTER SIGMA
  , ("τ",           "tau")      -- U+03C4 GREEK SMALL LETTER TAU
  , ("υ",           "upsilon")  -- U+03C5 GREEK SMALL LETTER UPSILON
  , ("φ",           "phi")      -- U+03C6 GREEK SMALL LETTER PHI
  , ("χ",           "chi")      -- U+03C7 GREEK SMALL LETTER CHI
  , ("ψ",           "psi")      -- U+03C8 GREEK SMALL LETTER PSI
  , ("ω",           "omega")    -- U+03C9 GREEK SMALL LETTER OMEGA
  , ("ẞ",           "SS")       -- U+1E9E LATIN CAPITAL LETTER SHARP S
  , ("‐",           "-")        -- U+2210 HYPHEN
  , ("–",           "-")        -- U+2013 EN DASH
  , ("—",           "-")        -- U+2014 EM DASH
  , ("‘",           "")         -- U+2018 LEFT SINGLE QUOTATION MARK
  , ("’",           "")         -- U+2019 RIGHT SINGLE QUOTATION MARK
  , ("‚",           "")         -- U+201A SINGLE LOW-9 QUOTATION MARK
  , ("‛",           "")         -- U+201B SINGLE HIGH-REVERSED-9 QUOTATION MARK
  , ("“",           "")         -- U+201C LEFT DOUBLE QUOTATION MARK
  , ("”",           "")         -- U+201D RIGHT DOUBLE QUOTATION MARK
  , ("„",           "")         -- U+201E DOUBLE LOW-9 QUOTATION MARK
  , ("‟",           "")         -- U+201F DOUBLE HIGH-REVERSED-9 QUOTATION MARK
  , ("†",           "")         -- U+2020 DAGGER
  , ("‡",           "")         -- U+2021 DOUBLE DAGGER
  , ("•",           "")         -- U+2022 BULLET
  , ("…",           "")         -- U+2026 HORIZONTAL ELLIPSIS
  , ("™",           "")         -- U+2122 TRADE MARK SIGN
  , ("ℵ",           "aleph")    -- U+2135 ALEF SYMBOL
  , ("ℶ",           "beth")     -- U+2136 BET SYMBOL
  , ("⊃",           "")         -- U+2283 SUPERSET OF
  , ("⌝",           "")         -- U+231D TOP RIGHT CORNER
  , ("�",          "")         -- U+FFFD REPLACEMENT CHARACTER
  ]

------------------------------------------------------------------------------
-- Author and title substitutions

-- | Author substitutions.
authorSubst ∷ [(Text, Text)]
authorSubst =
  [ (", ",   ",")
  , (" and", ",")
  ]

-- | Title substitutions.

-- These substitutions should be done before the substitutions of the
-- HTML entities and converting to lower case.
titleSubst ∷ [(Text,Text)]
titleSubst =
  [ ("<Emphasis Type=\"BoldItalic\">P </Emphasis>",       "P")
  , ("<Emphasis Type=\"Italic\">0 </Emphasis>",           "0")
  , ("<Emphasis Type=\"Italic\">C</Emphasis>",            "C")
  , ("<Emphasis Type=\"Italic\">CC</Emphasis>",           "CC")
  , ("<Emphasis Type=\"Italic\">I </Emphasis>",           "I")
  , ("<Emphasis Type=\"Italic\">J</Emphasis>",            "J")
  , ("<Emphasis Type=\"Italic\">Modus ponens</Emphasis>", "Modus ponens")
  , ("<Emphasis Type=\"Italic\">P </Emphasis>",           "P")
  , ("<Emphasis Type=\"Italic\">really </Emphasis>",      "really")
  , ("<Emphasis Type=\"Italic\">S-P</Emphasis>",          "S-P")
  , ("<Subscript>3</Subscript>",                          "3")
  -- The whitespace around `+` is not the standard one.
  -- TODO (2017-07-04): Added test case.
  , ("<Superscript> + </Superscript>",                    "plus")
  , ("<Subscript>&#x03C9;</Subscript>",                   "omega")
  , ("<TEX>$\\alpha$</TEX>",                              "alpha")
  , ("<TEX>$\\beta$</TEX>",                               "beta")
  , ("<TEX>$\\gamma$</TEX>",                              "gamma")
  , ("<TEX>$\\epsilon$</TEX>",                            "epsilon")
  , ("<TEX>$\\eta$</TEX>",                                "eta")
  , ("<TEX>$\\lambda$</TEX>",                             "lambda")
  , ("<TEX>$\\pi$</TEX>",                                 "pi")
  , ("<TEX>$\\omega$</TEX>",                              "omega")
  , ("<TEX>{\\sc Coq}</TEX>",                             "Coq")
  , ("<TEX>{\\sf Haskell}</TEX>:",                        "Haskell")
  , ("<TEX>{\\sc QuickSpec}</TEX>:",                      "QuickSpec")
  , ("<TEX>{\\sc QuodLibet}</TEX>!",                      "QuodLibet")
  , ("<TEX>{\\sc Vampire}</TEX>",                         "Vampire")
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
  -- TODO (2017-07-20): Five hex numbers
  , ("&#x02010;", "-")  -- U+2010 HYPHEN
  -- TODO (2017-07-17): Missing `;`.
  , ("&#8217",  "")     -- U+2019 RIGHT SINGLE QUOTATION MARK
  ]
