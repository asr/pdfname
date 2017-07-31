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
  htmlNameSubst = map (\ (a, b, _) → (T.cons '&' $ T.snoc b ';', a)) substTable

  htmlDecSubst ∷ [(Text, Text)]
  htmlDecSubst = substHelper "&#%d;"

  html4HexSubst ∷ [(Text, Text)]
  html4HexSubst = substHelper "&#x%04X;"

  html5HexSubst ∷ [(Text, Text)]
  html5HexSubst = substHelper "&#x%05X;"

  allSubst ∷ [(Text, Text)]
  allSubst = htmlNameSubst ++ htmlDecSubst ++ html4HexSubst ++ html5HexSubst

  substHelper ∷ String → [(Text, Text)]
  substHelper format = map (\ (a, _, _) → (printfHelper format a, a)) substTable
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
  , ("!", noName,          "")  -- U+0021 EXCLAMATION MARK
  , ("\"", "quot",         "")  -- U+0022 QUOTATION MARK
  , ("#", noName,          "")  -- U+0023 NUMBER SIGN
  , ("$", noName,          "")  -- U+0024 DOLLAR SIGN
  , ("%", noName,          "")  -- U+0025 PERCENT SIGN
  , ("&", "amp",           "")  -- U+0026 AMPERSAND
  , ("'", noName,          "")  -- U+0027 APOSTROPHE
  , ("(", noName,          "-")  -- U+0028 LEFT PARENTHESIS
  , (")", noName,          "-")  -- U+0029 RIGHT PARENTHESIS
  , ("*", noName,          "")  -- U+002A ASTERISK
  , ("+", noName,          binaryOp "plus")  -- U+002B PLUS SIGN
  , (",", noName,          "")  -- U+002C COMMA
  -- We do not substitute U+002D HYPHEN-MINUS.
  , (".", noName,          "")  -- U+002E FULL STOP
  , ("/", noName,          "")  -- U+002F SOLIDUS
  , (":", noName,          "")  -- U+003A COLON
  , (";", noName,          "")  -- U+003B SEMICOLON
  , ("<", "lt",            binaryOp "less-than")  -- U+003C LESS-THAN SIGN
  , ("=", noName,          binaryOp "equals")  -- U+003D EQUALS SIGN
  , (">", "gt",            binaryOp "greater-than")  -- U+003E GREATER-THAN SIGN
  , ("?", noName,          "")  -- U+003F QUESTION MARK
  , ("@", noName,          "")  -- U+0040 COMMERCIAL AT
  , ("[", "TODO",          "")  -- U+005B LEFT SQUARE BRACKET
  , ("\\", "TODO",         "")  -- U+005C REVERSE SOLIDUS
  , ("]", "TODO",          "")  -- U+005D RIGHT SQUARE BRACKET
  , ("^", "TODO",          "")  -- U+005E CIRCUMFLEX ACCENT
  , ("_", "TODO",          "-")  -- U+005F LOW LINE
  , ("`", "TODO",          "")  -- U+0060 GRAVE ACCENT
  , ("{", "TODO",          "")  -- U+007B LEFT CURLY BRACKET
  , ("|", "TODO",          "")  -- U+007C VERTICAL LINE
  , ("}", "TODO",          "")  -- U+007B RIGHT CURLY BRACKET
  , ("~", "TODO",          "")  -- U+007E TILDE
  , ("¡", "iexcl",         "")  -- U+00A1 INVERTED EXCLAMATION MARK
  , ("¢", "cent",          "cent")  -- U+00A2 CENT SIGN
  , ("£", "pound",         "pound")  -- U+00A3 POUND SIGN
  , ("¤", "curren",        "")  -- U+00A4 CURRENCY SIGN
  , ("¥", "yen",           "yen")  -- U+00A5 YEN SIGN
  , ("¦", "brvvar",        "")  -- U+00A6 BROKEN BAR
  , ("§", "sect",          "")  -- U+00A7 SECTION SIGN
  , ("¨", "uml",           "")  -- U+00A8 DIAERESIS
  , ("©", "copy",          "")  -- U+00A9 COPYRIGHT SIGN
  , ("ª", "ordf",          "")  -- U+00AA FEMININE ORDINAL INDICATOR
  , ("«", "laquo",         "")  -- U+00AB LEFT-POINTING DOUBLE ANGLE QUOTATION MARK
  , ("¬", "not",           "")  -- U+00AC NOT SIGN
  -- TODO 2017-07-31
  -- , ("­", "shy",           "-")  -- U+00AD SOFT HYPHEN
  , ("®", "reg",           "")  -- U+00AE REGISTERED SIGN
  , ("¯", "macr",          "")  -- U+00AF MACRON
  , ("°", "deg",           "degree")  -- U+00B0 DEGREE SIGN
  , ("±", "plusmn",        binaryOp "plus-minus")  -- U+00B1 PLUS-MINUS SIGN
  , ("²", "sup2",          "2")  -- U+00B2 SUPERSCRIPT TWO
  , ("³", "sup3",          "3")  -- U+00B3 SUPERSCRIPT THREE
  , ("´", "acute",         "")  -- U+00B4 ACUTE ACCENT
  , ("µ", "micro",         "micro")  -- U+00B5 MICRO SIGN
  , ("¶", "para",          "")  -- U+00B6 PILCROW SIGN
  , ("·", "middot",        "")  -- U+00B7 MIDDLE DOT
  , ("¸", "cedil",         "")  -- U+00B8 CEDILLA
  , ("¹", "sup1",          "1")  -- U+00B9 SUPERSCRIPT ONE
  , ("º", "ordm",          "")  -- U+00BA MASCULINE ORDINAL INDICATOR
  , ("»", "raquo",         "")  -- U+00BB RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK
  , ("¼", "frac14",        "")  -- U+00BC VULGAR FRACTION ONE QUARTER
  , ("½", "frac12",        "")  -- U+00BD VULGAR FRACTION ONE HALF
  , ("¾", "frac34",        "")  -- U+00BE VULGAR FRACTION THREE QUARTERS
  , ("¿", "iquest",        "")  -- U+00BF INVERTED QUESTION MARK
  , ("À", "Agrave",        "A")  -- U+00C0 LATIN CAPITAL LETTER A WITH GRAVE
  , ("Á", "Aacute",        "A")  -- U+00C1 LATIN CAPITAL LETTER A WITH ACUTE
  , ("Â", "Acirc",         "A")  -- U+00C2 LATIN CAPITAL LETTER A WITH CIRCUMFLEX
  , ("Ã", "Atilde",        "A")  -- U+00C3 LATIN CAPITAL LETTER A WITH TILDE
  , ("Ä", "Auml",          "A")  -- U+00C4 LATIN CAPITAL LETTER A WITH DIAERESIS
  , ("Å", "Aring",         "A")  -- U+00C5 LATIN CAPITAL LETTER A WITH RING ABOVE
  , ("Æ", "AElig",         "AE")  -- U+00C6 LATIN CAPITAL LETTER AE
  , ("Ç", "Ccedil",        "C")  -- U+00C7 LATIN CAPITAL LETTER C WITH CEDILLA
  , ("È", "Egrave",        "E")  -- U+00C8 LATIN CAPITAL LETTER E WITH GRAVE
  , ("É", "Eacute",        "E")  -- U+00C9 LATIN CAPITAL LETTER E WITH ACUTE
  , ("Ê", "Ecirc",         "E")  -- U+00CA LATIN CAPITAL LETTER E WITH CIRCUMFLEX
  , ("Ë", "Euml",          "E")  -- U+00CB LATIN CAPITAL LETTER E WITH DIAERESIS
  , ("Ì", "Igrave",        "I")  -- U+00CC LATIN CAPITAL LETTER I WITH GRAVE
  , ("Í", "Iacute",        "I")  -- U+00CD LATIN CAPITAL LETTER I WITH ACUTE
  , ("Î", "Icirc",         "I")  -- U+00CE LATIN CAPITAL LETTER I WITH CIRCUMFLEX
  , ("Ï", "Iuml",          "I")  -- U+00CF LATIN CAPITAL LETTER I WITH DIAERESIS
  , ("Ð", "ETH",           "ETH")  -- U+00D0 LATIN CAPITAL LETTER ETH
  , ("Ñ", "Ntilde",        "N")  -- U+00D1 LATIN CAPITAL LETTER N WITH TILDE
  , ("Ò", "Ograve",        "O")  -- U+00D2 LATIN CAPITAL LETTER O WITH GRAVE
  , ("Ó", "Oacute",        "O")  -- U+00D3 LATIN CAPITAL LETTER O WITH ACUTE
  , ("Ô", "Ocirc",         "O")  -- U+00D4 LATIN CAPITAL LETTER O WITH CIRCUMFLEX
  , ("Õ", "Otilde",        "O")  -- U+00D5 LATIN CAPITAL LETTER O WITH TILDE
  , ("Ö", "Ouml",          "O")  -- U+00D6 LATIN CAPITAL LETTER O WITH DIAERESIS
  , ("×", "times",         binaryOp "times")  -- U+00D7 MULTIPLICATION SIGN
  , ("Ø", "Oslash",        "O")  -- U+00D8 LATIN CAPITAL LETTER O WITH STROKE
  , ("Ù", "Ugrave",        "U")  -- U+00D9 LATIN CAPITAL LETTER U WITH GRAVE
  , ("Ú", "Uacute",        "U")  -- U+00DA LATIN CAPITAL LETTER U WITH ACUTE
  , ("Û", "Ucirc",         "U")  -- U+00DB LATIN CAPITAL LETTER U WITH CIRCUMFLEX
  , ("Ü", "Uuml",          "U")  -- U+00DC LATIN CAPITAL LETTER U WITH DIAERESIS
  , ("Ý", "Yacute",        "Y")  -- U+00DD LATIN CAPITAL LETTER Y WITH ACUTE
  , ("Þ", "THORN",         "THORN")  -- U+00DE LATIN CAPITAL LETTER THORN
  , ("ß", "szlig",         "ss")  -- U+00DF LATIN SMALL LETTER SHARP S
  , ("à", "agrave",        "a")  -- U+00E0 LATIN SMALL LETTER A WITH GRAVE
  , ("á", "aacute",        "a")  -- U+00E1 LATIN SMALL LETTER A WITH ACUTE
  , ("â", "acirc",         "a")  -- U+00E2 LATIN SMALL LETTER A CIRCUMFLEX
  , ("ã", "atilde",        "a")  -- U+00E3 LATIN SMALL LETTER A WITH TILDE
  , ("ä", "auml",          "a")  -- U+00E4 LATIN SMALL LETTER A WITH DIAERESIS
  , ("å", "aring",         "a")  -- U+00E5 LATIN SMALL LETTER A WITH RING ABOVE
  , ("æ", "aelig",         "ae")  -- U+00E6 LATIN SMALL LETTER AE
  , ("ç", "ccedil",        "c")  -- U+00E7 LATIN SMALL LETTER C WITH CEDILLA
  , ("è", "egrave",        "e")  -- U+00E8 LATIN SMALL LETTER E WITH GRAVE
  , ("é", "eacute",        "e")  -- U+00E9 LATIN SMALL LETTER E WITH ACUTE
  , ("ê", "ecirc",         "e")  -- U+00EA LATIN SMALL LETTER E WITH CIRCUMFLEX
  , ("ë", "euml",          "e")  -- U+00EB LATIN SMALL LETTER E WITH DIAERESIS
  , ("ì", "igrave",        "i")  -- U+00EC LATIN SMALL LETTER I WITH GRAVE
  , ("í", "iacute",        "i")  -- U+00ED LATIN SMALL LETTER I WITH ACUTE
  , ("î", "icirc",         "i")  -- U+00EE LATIN SMALL LETTER I WITH CIRCUMFLEX
  , ("ï", "iuml",          "i")  -- U+00EF LATIN SMALL LETTER I WITH DIAERESIS
  , ("ð", "eth",           "eth") -- U+00F0 LATIN SMALL LETTER ETH
  , ("ñ", "ntilde",        "n")  -- U+00F1 LATIN SMALL LETTER N WITH TILDE
  , ("ò", "ograve",        "o")  -- U+00F2 LATIN SMALL LETTER O WITH GRAVE
  , ("ó", "oacute",        "o")  -- U+00F3 LATIN SMALL LETTER O WITH ACUTE
  , ("ô", "ocirc",         "o")  -- U+00F4 LATIN SMALL LETTER O WITH CIRCUMFLEX
  , ("õ", "otilde",        "o")  -- U+00F5 LATIN SMALL LETTER O WITH TILDE
  , ("ö", "ouml",          "o")  -- U+00F6 LATIN SMALL LETTER O WITH DIAERESIS
  , ("÷", "divide",        "")   -- U+00F7 DIVISION SIGN
  , ("ø", "oslash",        "o")  -- U+00F8 LATIN SMALL LETTER O WITH STROKE
  , ("ù", "ugrave",        "u")  -- U+00F9 LATIN SMALL LETTER U WITH GRAVE
  , ("ú", "uacute",        "u")  -- U+00FA LATIN SMALL LETTER U WITH ACUTE
  , ("û", "ucirc",         "u")  -- U+00FB LATIN SMALL LETTER U WITH CIRCUMFLEX
  , ("ü", "uuml",          "u")  -- U+00FC LATIN SMALL LETTER U WITH DIAERESIS
  , ("ý", "yacute",        "y")  -- U+00FD LATIN SMALL LETTER Y WITH ACUTE
  , ("þ", "thorn",         "thorn")  -- U+00FE LATIN SMALL LETTER THORN
  , ("ÿ", "yuml",          "y")  -- U+00FF LATIN SMALL LETTER Y WITH DIAERESIS
  , ("Ā", "Amacr",         "A")  -- U+0100 LATIN CAPITAL LETTER A WITH MACRON
  , ("ā", "amacr",         "a")  -- U+0101 LATIN SMALL LETTER A WITH MACRON
  , ("Ă", "Abreve",        "A")  -- U+0102 LATIN CAPITAL LETTER A WITH BREVE
  , ("ă", "$abreve",        "a")  -- U+0103 LATIN SMALL LETTER A WITH BREVE
  , ("Ą", "Aogon",         "A")  -- U+0104 LATIN CAPITAL LETTER A WITH OGONEK
  , ("ą", "aogon",         "a")  -- U+0105 LATIN SMALL LETTER A WITH OGONEK
  , ("Ć", "Cacute",        "c")  -- U+0106 LATIN CAPITAL LETTER C WITH ACUTE
  , ("ć", "cacute",        "c")  -- U+0107 LATIN SMALL LETTER C WITH ACUTE
  , ("Č", "Ccaron",        "C")  -- U+010C LATIN CAPITAL LETTER C WITH CARON
  , ("č", "ccaron",        "c")  -- U+010D LATIN SMALL LETTER C WITH CARON
  , ("Ď", "Dcaron",        "D")  -- U+010E LATIN CAPITAL LETTER D WITH CARON
  , ("ď", "dcaron",        "d")  -- U+010F LATIN SMALL LETTER D WITH CARON
  , ("Ē", "Emacr",         "E")  -- U+0112 LATIN CAPITAL LETTER E WITH MACRON
  , ("ē", "emacr",         "e")  -- U+0113 LATIN SMALL LETTER E WITH MACRON
  , ("ĕ", noName,          "e")  -- U+0115 LATIN SMALL LETTER E WITH BREVE
  , ("Ę", "Eogon",         "e")  -- U+0118 LATIN CAPITAL LETTER E WITH OGONEK
  , ("ę", "eogon",         "e")  -- U+0119 LATIN SMALL LETTER E WITH OGONEK
  , ("Ě", "Ecaron",        "E")  -- U+011A LATIN CAPITAL LETTER E WITH CARON
  , ("ě", "ecaron",        "e")  -- U+011B LATIN SMALL LETTER E WITH CARON
  , ("Ğ", "Gbreve",        "G")  -- U+011E LATIN CAPITAL LETTER G WITH BREVE
  , ("ğ", "gbreve",        "g")  -- U+011F LATIN SMALL LETTER G WITH BREVE
  , ("Ģ", "Gcedil",        "G")  -- U+0122 LATIN CAPITAL LETTER G WITH CEDILLA
  , ("ģ", "gcedil",        "g")  -- U+0123 LATIN SMALL LETTER G WITH CEDILLA
  , ("Ī", "Imacr",         "I")  -- U+012A LATIN CAPITAL LETTER I WITH MACRON
  , ("ī", "imacr",         "I")  -- U+012B LATIN SMALL LETTER I WITH MACRON
  , ("ĭ", noName,          "i")  -- U+012D LATIN SMALL LETTER I WITH BREVE
  , ("İ", "Idot",          "I")  -- U+0130 LATIN CAPITAL LETTER I WITH DOT ABOVE
  , ("Ķ", "Kcedil",        "K")  -- U+0136 LATIN CAPITAL LETTER K WITH CEDILLA
  , ("ķ", "kcedil",        "k")  -- U+0137 LATIN SMALL LETTER K WITH CEDILLA
  , ("Ļ", "Lcedil",        "L")  -- U+013B LATIN CAPITAL LETTER L WITH CEDILLA
  , ("ļ", "lcedil",        "l")  -- U+013C LATIN SMALL LETTER L WITH CEDILLA
  , ("Ł", "Lstrok",        "L")  -- U+0141 LATIN CAPITAL LETTER L WITH STROKE
  , ("ł", "lstrok",        "l")  -- U+0142 LATIN SMALL LETTER L WITH STROKE
  , ("Ń", "Nacute",        "N")  -- U+0143 LATIN CAPITAL LETTER N WITH ACUTE
  , ("ń", "nacute",        "n")  -- U+0144 LATIN SMALL LETTER N WITH ACUTE
  , ("Ņ", "Ncedil",        "n")  -- U+0145 LATIN CAPITAL LETTER N WITH CEDILLA
  , ("ņ", "ncedil",        "n")  -- U+0146 LATIN SMALL LETTER N WITH CEDILLA
  , ("Ň", "Ncaron",        "N")  -- U+0147 LATIN CAPITAL LETTER N WITH CARON
  , ("ň", "ncaron",        "n")  -- U+0148 LATIN SMALL LETTER N WITH CARON
  , ("Ő", "Odblac",        "O")  -- U+0150 LATIN CAPITAL LETTER O WITH DOUBLE ACUTE
  , ("ő", "odblac",        "o")  -- U+0151 LATIN SMALL LETTER O WITH DOUBLE ACUTE
  , ("Œ", "OElig",         "OE")  -- U+0152 LATIN CAPITAL LIGATURE OE
  , ("œ", "oelig",         "oe")  -- U+0153 LATIN SMALL LIGATURE OE
  , ("Ř", "Rcaron",        "R")  -- U+0158 LATIN CAPITAL LETTER R WITH CARON
  , ("ř", "rcaron",        "r")  -- U+0159 LATIN SMALL LETTER R WITH CARON
  , ("Ś", "Sacute",        "S")  -- U+015A LATIN CAPITAL LETTER S WITH ACUTE
  , ("ś", "sacute",        "s")  -- U+015B LATIN SMALL LETTER S WITH ACUTE
  , ("Ş", "Scedil",        "S")  -- U+015E LATIN CAPITAL LETTER S WITH CEDILLA
  , ("ş", "scedil",        "s")  -- U+015F LATIN SMALL LETTER S WITH CEDILLA
  , ("Š", "Scaron",        "S")  -- U+0160 LATIN CAPITAL LETTER S WITH CARON
  , ("š", "scaron",        "s")  -- U+0161 LATIN SMALL LETTER S WITH CARON
  , ("Ť", "Tcaron",        "T")  -- U+0164 LATIN CAPITAL LETTER T WITH CARON
  , ("ť", "tcaron",        "t")  -- U+0165 LATIN SMALL LETTER T WITH CARON
  , ("Ū", "Umacr",         "U")  -- U+016A LATIN CAPITAL LETTER U WITH MACRON
  , ("ū", "Umacr",         "u")  -- U+016B LATIN SMALL LETTER U WITH MACRON
  , ("ŭ", "ubreve",        "u")  -- U+016D LATIN SMALL LETTER U WITH BREVE
  , ("Ů", "Uring",         "U")  -- U+016E LATIN CAPITAL LETTER U WITH RING ABOVE
  , ("ů", "uring",         "u")  -- U+016F LATIN SMALL LETTER U WITH RING ABOVE
  , ("Ű", "Udblac",       "U")  -- U+0170 LATIN CAPITAL LETTER U WITH DOUBLE ACUTE
  , ("ű", "udblac",       "u")  -- U+0171 LATIN SMALL LETTER U WITH DOUBLE ACUTE
  , ("Ÿ", "Yuml",          "Y")  -- U+0178 LATIN CAPITAL LETTER Y WITH DIAERESIS
  , ("Ź", "Zacute",        "Z")  -- U+0179 LATIN CAPITAL LETTER Z WITH ACUTE
  , ("ź", "zacute",        "z")  -- U+017A LATIN SMALL LETTER Z WITH ACUTE
  , ("Ż", "Zdot",          "Z")  -- U+017B LATIN CAPITAL LETTER Z WITH DOT ABOVE
  , ("ż", "zdot",          "z")  -- U+017C LATIN SMALL LETTER Z WITH DOT ABOVE
  , ("Ž", "Zcaron",        "z")  -- U+017D LATIN CAPITAL LETTER Z WITH CARON
  , ("ž", "<caron",        "z")  -- U+017E LATIN SMALL LETTER Z WITH CARON
  , ("Ə", noName,          "A")  -- U+018F LATIN CAPITAL LETTER SCHWA
  , ("Ș", noName,          "S")  -- U+0218 LATIN CAPITAL LETTER S WITH COMMA BELOW
  , ("ș", noName,          "s")  -- U+0219 LATIN SMALL LETTER S WITH COMMA BELOW
  , ("Ț", noName,          "T")  -- U+021A LATIN CAPITAL LETTER T WITH COMMA BELOW
  , ("ț", noName,          "t")  -- U+021B LATIN SMALL LETTER T WITH COMMA BELOW
  , ("ə", noSupported,     "a")  -- U+0259 LATIN SMALL LETTER SCHWA
  , ("Α", "Alpha",         "Alpha")    -- U+0391 GREEK CAPITAL LETTER ALPHA
  , ("Β", "Beta",          "Beta")     -- U+0392 GREEK CAPITAL LETTER BETA
  , ("Γ", "Gamma",         "Gamma")    -- U+0393 GREEK CAPITAL LETTER GAMMA
  , ("Δ", "Delta",         "Delta")    -- U+0394 GREEK CAPITAL LETTER DELTA
  , ("Ε", "Epsilon",       "Epsilon")  -- U+0395 GREEK CAPITAL LETTER EPSILON
  , ("Ζ", "Zeta",          "Zeta")     -- U+0396 GREEK CAPITAL LETTER ZETA
  , ("Η", "Eta",           "Eta")      -- U+0397 GREEK CAPITAL LETTER ETA
  , ("Θ", "Theta",         "Theta")    -- U+0398 GREEK CAPITAL LETTER THETA
  , ("Ι", "Iota",          "Iota")     -- U+0399 GREEK CAPITAL LETTER IOTA
  , ("Κ", "Kappa",         "Kappa")    -- U+039A GREEK CAPITAL LETTER KAPPA
  , ("Λ", "Lambda",        "Lambda")   -- U+039B GREEK CAPITAL LETTER LAMDA
  , ("Μ", "Mu",            "Mu")       -- U+039C GREEK CAPITAL LETTER MU
  , ("Ν", "Nu",            "Nu")       -- U+039D GREEK CAPITAL LETTER NU
  , ("Ξ", "Xi",            "Xi")       -- U+039E GREEK CAPITAL LETTER ZI
  , ("Ο", "Omicron",       "Omicron")  -- U+039F GREEK CAPITAL LETTER OMICRON
  , ("Π", "Pi",            "Pi")       -- U+03A0 GREEK CAPITAL LETTER PI
  , ("Ρ", "Rho",           "Rho")      -- U+03A1 GREEK CAPITAL LETTER RHO
  -- The code U+03A2 is reserved.
  , ("Σ", "Sigma",         "Sigma")    -- U+03A3 GREEK CAPITAL LETTER SIGMA
  , ("Τ", "Tau",           "Tau")      -- U+03A4 GREEK CAPITAL LETTER TAU
  , ("Υ", "Upsilon",       "Upsilon")  -- U+03A5 GREEK CAPITAL LETTER UPSILON
  , ("Φ", "Phi",           "Phi")      -- U+03A6 GREEK CAPITAL LETTER PHI
  , ("χ", "Chi",           "Chi")      -- U+03A7 GREEK CAPITAL LETTER CHI
  , ("Ψ", "Psi",           "Psi")      -- U+03A8 GREEK CAPITAL LETTER PSI
  , ("Ω", "Omega",         "Omega")    -- U+03A9 GREEK CAPITAL LETTER OMEGA
  , ("α", "alpha",         "alpha")    -- U+03B1 GREEK SMALL LETTER ALPHA
  , ("β", "beta",          "beta")     -- U+03B2 GREEK SMALL LETTER BETA
  , ("γ", "gamma",         "gamma")    -- U+03B3 GREEK SMALL LETTER GAMMA
  , ("δ", "delta",         "delta")    -- U+03B4 GREEK SMALL LETTER DELTA
  , ("ε", "epsilon",       "epsilon")  -- U+03B5 GREEK SMALL LETTER EPSILON
  , ("ζ", "zeta",          "zeta")     -- U+03B6 GREEK SMALL LETTER ZETA
  , ("η", "eta",           "eta")      -- U+03B7 GREEK SMALL LETTER ETA
  , ("θ", "theta",         "theta")    -- U+03B8 GREEK SMALL LETTER THETA
  , ("ι", "iota",          "iota")     -- U+03B9 GREEK SMALL LETTER IOTA
  , ("κ", "kappa",         "kappa")    -- U+03BA GREEK SMALL LETTER KAPPA
  , ("λ", "lambda",        "lambda")   -- U+03BB GREEK SMALL LETTER LAMDA
  , ("μ", "mu",            "mu")       -- U+03BC GREEK SMALL LETTER MU
  , ("ν", "nu",            "nu")       -- U+03BD GREEK SMALL LETTER NU
  , ("ξ", "xi",            "xi")       -- U+03BE GREEK SMALL LETTER ZI
  , ("ο", "omicron",       "omicron")  -- U+03BF GREEK SMALL LETTER OMICRON
  , ("π", "pi",            "pi")       -- U+03C0 GREEK SMALL LETTER PI
  , ("ρ", "rho",           "rho")      -- U+03C1 GREEK SMALL LETTER RHO
  , ("ς", "sigmaf",        "sigma")    -- U+03C2 GREEK SMALL LETTER FINAL SIGMA
  , ("σ", "sigma",         "sigma")    -- U+03C3 GREEK SMALL LETTER SIGMA
  , ("τ", "tau",           "tau")      -- U+03C4 GREEK SMALL LETTER TAU
  , ("υ", "upsilon",       "upsilon")  -- U+03C5 GREEK SMALL LETTER UPSILON
  , ("φ", "phi",           "phi")      -- U+03C6 GREEK SMALL LETTER PHI
  , ("χ", "chi",           "chi")      -- U+03C7 GREEK SMALL LETTER CHI
  , ("ψ", "psi",           "psi")      -- U+03C8 GREEK SMALL LETTER PSI
  , ("ω", "omega",         "omega")    -- U+03C9 GREEK SMALL LETTER OMEGA
  , ("ϑ", "thetasym",      "theta")    -- U+03D1 GREEK THETA SYMBOL
  , ("ϒ", "upsih",         "Upsilon")  -- U+03D2 GREEK UPSILON WITH HOOK SYMBOL
  , ("ϕ", "straightphi",   "phi")      -- U+03D5 GREEK PHI SYMBOL
  , ("ϖ", "piv",           "pi")       -- U+03D6 GREEK PI SYMBOL
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
  , ("†", "dagger",        "")  -- U+2020 DAGGER
  , ("‡", "Dagger",        "")  -- U+2021 DOUBLE DAGGER
  , ("•", "bull",          "")  -- U+2022 BULLET
  , ("…", "hellip",        "")  -- U+2026 HORIZONTAL ELLIPSIS
  , ("⁰", noSupported,     "0")  -- U+2070 SUPERSCRIPT ZERO
  , ("ⁱ", noSupported,     "i")  -- U+2071 SUPERSCRIPT LATIN SMALL LETTER I
  -- The codes U+2072 and U+2073 are reserved.
  , ("⁴", noSupported,     "4")  -- U+2074 SUPERSCRIPT FOUR
  , ("⁵", noSupported,     "5")  -- U+2075 SUPERSCRIPT FIVE
  , ("⁶", noSupported,     "6")  -- U+2076 SUPERSCRIPT SIX
  , ("⁷", noSupported,     "7")  -- U+2077 SUPERSCRIPT SEVEN
  , ("⁸", noSupported,     "8")  -- U+2078 SUPERSCRIPT EIGHT
  , ("⁹", noSupported,     "9")  -- U+2079 SUPERSCRIPT NINE
  , ("₀", noSupported,     "0")  -- U+2080 SUBSCRIPT ZERO
  , ("₁", noSupported,     "1")  -- U+2081 SUBSCRIPT ONE
  , ("₂", noSupported,     "2")  -- U+2082 SUBSCRIPT TWO
  , ("₃", noSupported,     "3")  -- U+2083 SUBSCRIPT THREE
  , ("₄", noSupported,     "4")  -- U+2074 SUBSCRIPT FOUR
  , ("₅", noSupported,     "5")  -- U+2075 SUBSCRIPT FIVE
  , ("₆", noSupported,     "6")  -- U+2076 SUBSCRIPT SIX
  , ("₇", noSupported,     "7")  -- U+2077 SUBSCRIPT SEVEN
  , ("₈", noSupported,     "8")  -- U+2078 SUBSCRIPT EIGHT
  , ("₉", noSupported,     "9")  -- U+2079 SUBSCRIPT NINE
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
  , ("™", "trade",         "")  -- U+2122 TRADE MARK SIGN
  , ("ℤ", noName,          "Z")  -- U+2124 DOUBLE-STRUCK CAPITAL Z
  , ("ℬ", noName,          "B")  -- U+212C SCRIPT CAPITAL B
  , ("ℯ", noName,          "e")  -- U+212F SCRIPT SMALL E
  , ("ℰ", noName,          "E")  -- U+2130 SCRIPT CAPITAL E
  , ("ℱ", noName,          "F")  -- U+2131 SCRIPT CAPITAL F
  , ("ℳ", noName,          "M")  -- U+2133 SCRIPT CAPITAL M
  , ("ℴ", noName,          "o")  -- U+2134 SCRIPT SMALL O
  , ("ℵ", "alefsym",       "aleph")  -- U+2135 ALEF SYMBOL
  , ("ℶ", noName,          "beth")  -- U+2136 BET SYMBOL
  , ("←", "larr",          binaryOp "arrow")  -- U+2190 LEFTWARDS ARROW
  , ("↑", "uarr",          binaryOp "arrow")  -- U+2191 UPWARDS ARROW
  , ("→", "rarr",          binaryOp "arrow")  -- U+2192 RIGHTWARDS ARROW
  , ("↓", "ldrr",          binaryOp "arrow")  -- U+2193 DOWNWARDS ARROW
  , ("↔", "harr",          binaryOp "arrow")  -- U+2194 LEFT RIGHT ARROW
  , ("∀", "forall",        unaryOp "for-all")  -- U+2200 FOR ALL
  , ("∃", "exist",         unaryOp "exists")  -- U+2203 THERE EXISTS
  , ("−", "minus",         "minus")  -- U+2212 MINUS SIGN
  , ("∓", noName,          binaryOp "minus-or-plus")  -- U+2213 MINUS-OR-PLUS SIGN
  , ("√", "radic",         unaryOp "sqrt")  -- U+221A SQUARE ROOT
  , ("⊂", "sub",           binaryOp "subset-of")  -- U+2282 SUBSET OF
  , ("⊃", "sup",           binaryOp "superset-of-")  -- U+2283 SUPERSET OF
  , ("⊄", "nsub",          "")  -- U+2284 NOT A SUBSET OF
  , ("⊅", noName,          "")  -- U+2285 NOT A SUPERSET OF
  , ("⊆", "sube",          binaryOp "subset-of-or-equal-to")  -- U+2286 SUBSET OF OR EQUAL TO
  , ("⊇", "supe",          binaryOp "superset-of-or-equal-to")  -- U+2287 SUPERSET OF OR EQUAL TO
  , ("⊈", noName,          "")  -- U+2288 NEITHER A SUBSET OF NOR EQUAL TO
  , ("⊉", noName,          "")  -- U+2289 NEITHER A SUPERSET OF NOR EQUAL TO
  , ("⊊", noName,          "")  -- U+228A SUBSET OF WITH NOT EQUAL TO
  , ("⊋", noName,          "")  -- U+228B SUPERSET OF WITH NOT EQUAL TO
  , ("⊕", "oplus",         binaryOp "circled-plus")  -- U+2295 CIRCLED PLUS
  , ("⊗", "otimes",        binaryOp "circled-times")  -- U+2297 CIRCLED TIMES
  , ("⋉", noName,          binaryOp "left-semidirect-product")  -- U+22C9) LEFT NORMAL FACTOR SEMIDIRECT PRODUCT
  , ("⋊", noName,          binaryOp "right-semidirect-product")  -- U+22CA RIGHT NORMAL FACTOR SEMIDIRECT PRODUCT
  , ("⌝", noSupported,     "")  -- U+231D TOP RIGHT CORNER
  , ("�", noSupported,    "")  -- U+FFFD REPLACEMENT CHARACTER
  ]
