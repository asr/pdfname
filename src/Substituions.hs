-- | Substituions.

module Substituions
  ( authorSubst
  , chSubst
  , commonSubst
  , titleSubst
  ) where

import Data.Text ( Text )

------------------------------------------------------------------------------
-- Characters substituions

-- If a new entry is added here, please also add it to the
-- characters-decimal substituions.

-- | Characters in non-numeric notation.
chNNSubst ∷ [(Text,Text)]
chNNSubst =
  [ ("&ouml;", "o")  -- LATIN SMALL LETTER O WITH DIAERESIS   (ö)
  , ("&Auml;", "A")  -- LATIN CAPITAL LETTER A WITH DIAERESIS (Ä)
  , ("&Uuml;", "U")  -- LATIN CAPITAL LETTER U WITH DIAERESIS (Ü)
  ]

-- If a new entry is added here, please also add it to the
-- characters-hexadecimal substituions.

-- | Characters substituions in decimal notation.
chDecSubst ∷ [(Text,Text)]
chDecSubst =
  [ ("&#225;",  "a")              -- LATIN SMALL LETTER A WITH ACUTE
  , ("&#233;",  "e")              -- LATIN SMALL LETTER E WITH ACUTE
  , ("&#237;",  "i")              -- LATIN SMALL LETTER I WITH ACUTE
  , ("&#243;",  "o")              -- LATIN SMALL LETTER O WITH ACUTE
  , ("&#246;",  "o")              -- LATIN SMALL LETTER O WITH DIAERESIS (ö)
  , ("&#250;",  "u")              -- LATIN SMALL LETTER U WITH ACUTE
  , ("&#352",   "S")              -- LATIN CAPITAL LETTER S WITH CARON
  , ("&#353",   "s")              -- LATIN SMALL LETTER S WITH CARON
  , ("&#955;",  "lambda")         -- GREEK SMALL LETTER LAMDA
  , ("&#8216;", "")               -- LEFT SINGLE QUOTATION MARK
  , ("&#8217",  "")               -- RIGHT SINGLE QUOTATION MARK
  ]

-- | Characters substituions in hexadecimal notation.
chHexSubst ∷ [(Text,Text)]
chHexSubst =
  [ ("&#x00C4;",  "A")              -- LATIN CAPITAL LETTER A WITH DIAERESIS (Ä)
  , ("&#x00DC;",  "U")              -- LATIN CAPITAL LETTER U WITH DIAERESIS (Ü)
  , ("&#x00E1;",  "a")              -- LATIN SMALL LETTER A WITH ACUTE
  , ("&#x00E9;",  "e")              -- LATIN SMALL LETTER E WITH ACUTE
  , ("&#x00ED;",  "i")              -- LATIN SMALL LETTER I WITH ACUTE
  , ("&#x00F3;",  "o")              -- LATIN SMALL LETTER O WITH ACUTE
  , ("&#x00FA;",  "u")              -- LATIN SMALL LETTER U WITH ACUTE
  , ("&#x00F6;",  "o")              -- LATIN SMALL LETTER O WITH DIAERESIS (ö)
  , ("&#x00FC;",  "u")              -- LATIN SMALL LETTER U WITH DIAERESIS (ü)
  , ("&#x0160",   "S")              -- LATIN CAPITAL LETTER S WITH CARON
  , ("&#x0161",   "s")              -- LATIN SMALL LETTER S WITH CARON
  , ("&#x012A;",  "I")              -- LATIN CAPITAL LETTER I WITH MACRON
  , ("&#x012B;",  "I")              -- LATIN SMALL LETTER I WITH MACRON
  , ("&#x015A;",  "S")              -- LATIN CAPITAL LETTER S WITH ACUTE
  , ("&#x015B;",  "s")              -- LATIN SMALL LETTER S WITH ACUTE
  , ("&#x015E;",  "s")              -- LATIN CAPITAL LETTER S WITH CEDILLA
  , ("&#x015F;",  "s")              -- LATIN SMALL LETTER S WITH CEDILLA
  , ("&#x012A;",  "I")              -- LATIN CAPITAL LETTER I WITH MACRON
  , ("&#x012B;",  "i")              -- LATIN SMALL LETTER I WITH MACRON
  , ("&#x03B1;",  "alpha")          -- GREEK SMALL LETTER ALPHA
  , ("&#x03B2;",  "beta")           -- GREEK SMALL LETTER BETA
  , ("&#x03B3;",  "gamma")          -- GREEK SMALL LETTER GAMMA
  , ("&#x03B4;",  "delta")          -- GREEK SMALL LETTER DELTA
  , ("&#x03B5;",  "epsilon")        -- GREEK SMALL LETTER EPSILON
  , ("&#x03B6;",  "zeta")           -- GREEK SMALL LETTER ZETA
  , ("&#x03B7;",  "eta")            -- GREEK SMALL LETTER ETA
  , ("&#x03B8;",  "theta")          -- GREEK SMALL LETTER THETA
  , ("&#x03B9;",  "iota")           -- GREEK SMALL LETTER IOTA
  , ("&#x03BA;",  "kappa")          -- GREEK SMALL LETTER KAPPA
  , ("&#x03BB;",  "lambda")         -- GREEK SMALL LETTER LAMDA
  , ("&#x03BC;",  "mu")             -- GREEK SMALL LETTER MU
  , ("&#x03BD;",  "nu")             -- GREEK SMALL LETTER NU
  , ("&#x03BE;",  "xi")             -- GREEK SMALL LETTER XI
  , ("&#x03BF;",  "omicron")        -- GREEK SMALL LETTER OMICRON
  , ("&#x03C0;",  "pi")             -- GREEK SMALL LETTER PI
  , ("&#x03C1;",  "rho")            -- GREEK SMALL LETTER RHO
  , ("&#x03C2;",  "sigma")          -- GREEK SMALL LETTER FINAL SIGMA
  , ("&#x03C3;",  "sigma")          -- GREEK SMALL LETTER SIGMA
  , ("&#x03C4;",  "tau")            -- GREEK SMALL LETTER TAU
  , ("&#x03C5;",  "upsilon")        -- GREEK SMALL LETTER UPSILON
  , ("&#x03C6;",  "phi")            -- GREEK SMALL LETTER PHI
  , ("&#x03C7;",  "chi")            -- GREEK SMALL LETTER CHI
  , ("&#x03C8;",  "psi")            -- GREEK SMALL LETTER PSI
  , ("&#x03C9;",  "omega")          -- GREEK SMALL LETTER OMEGA
  , ("&#x2010;",  "-")              -- HYPHEN
  , ("&#x2013;",  "-")              -- EN DASH
  , ("&#x2014;",  ".")              -- EM DAS
  , ("&#x2018;",  "")               -- LEFT SINGLE QUOTATION MARK
  , ("&#x2019;",  "")               -- RIGHT SINGLE QUOTATION MARK
  , ("&#x201A;",  "")               -- SINGLE LOW-9 QUOTATION MAR
  , ("&#x201B;",  "")               -- SINGLE HIGH-REVERSED-9 QUOTATION MARK
  , ("&#x201C;",  "")               -- LEFT DOUBLE QUOTATION MARK
  , ("&#x201D;",  "")               -- RIGHT DOUBLE QUOTATION MARK
  , ("&#x201E;",  "")               -- DOUBLE LOW-9 QUOTATION MARK
  , ("&#x201F;",  "")               -- DOUBLE HIGH-REVERSED-9 QUOTATION MARK
  , ("&#x2020;",  "dagger")         -- DAGGER
  , ("&#x2021;",  "dagger-dagger")  -- DOUBLE DAGGER
  , ("&#x2022;",  "")               -- BULLET
  , ("&#x2026;",  "")               -- HORIZONTAL ELLIPSIS
  , ("&#x2283;",  "")               -- SUPERSET OF
  , ("&#x231D;",  "")               -- TOP RIGHT CORNER
  , ("&#x02010;", "-")              -- HYPHEN
  ]

-- | Characters substituions in Unicode notation.
chUnicodeSubst ∷ [(Text,Text)]
chUnicodeSubst =
  [ ("\r",          "")         -- U+000D CARRIAGE RETURN (CR)
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
  , (":",           ".")        -- U+003A COLON
  , (";",           ".")        -- U+003B SEMICOLON
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
  , ("Ú",           "U")        -- U+00DA LATIN CAPITAL LETTER U WITH ACUTE
  , ("Û",           "U")        -- U+00DB LATIN CAPITAL LETTER U WITH CIRCUMFLEX
  , ("Ü",           "U")        -- U+00DC LATIN CAPITAL LETTER U WITH DIAERESIS
  , ("Ö",           "O")        -- U+00D6 LATIN CAPITAL LETTER O WITH DIAERESIS
  , ("×",           "")         -- U+00D7 MULTIPLICATION SIGN
  , ("Ù",           "U")        -- U+00D9 LATIN CAPITAL LETTER U WITH GRAVE
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
  , ("ö",           "o")        -- U+00F6 LATIN SMALL LETTER O WITH DIAERESIS
  , ("ø",           "o")        -- U+00F8 LATIN SMALL LETTER O WITH STROKE
  , ("ù",           "u")        -- U+00F9 LATIN SMALL LETTER U WITH GRAVE
  , ("ú",           "u")        -- U+00FA LATIN SMALL LETTER U WITH ACUTE
  , ("û",           "u")        -- U+00FB LATIN SMALL LETTER U WITH CIRCUMFLEX
  , ("ü",           "u")        -- U+00FC LATIN SMALL LETTER U WITH DIAERESIS
  , ("þ",           "t")        -- U+00FE LATIN SMALL LETTER THORN
  , ("ÿ",           "y")        -- U+00FF LATIN SMALL LETTER Y WITH DIAERESIS
  , ("Ă",           "A")        -- U+0102 LATIN CAPITAL LETTER A WITH BREVE
  , ("ă",           "a")        -- U+0103 LATIN SMALL LETTER A WITH BREVE
  , ("ć",           "c")        -- U+0107 LATIN SMALL LETTER C WITH ACUTE
  , ("č",           "c")        -- U+010D LATIN SMALL LETTER C WITH CARON
  , ("Ł",           "L")        -- U+0141 LATIN CAPITAL LETTER L WITH STROKE
  , ("ņ",           "n")        -- U+0146 LATIN SMALL LETTER N WITH CEDILLA
  , ("Œ",           "OE")       -- U+0152 LATIN CAPITAL LIGATURE OE
  , ("œ",           "oe")       -- U+0153 LATIN SMALL LIGATURE OE
  , ("ř",           "r")        -- U+0159 LATIN SMALL LETTER R WITH CARON
  , ("š",           "s")        -- U+0161 LATIN SMALL LETTER S WITH CARON
  , ("ū",           "u")        -- U+016B LATIN SMALL LETTER U WITH MACRON
  , ("Ÿ",           "Y")        -- U+0178 LATIN CAPITAL LETTER Y WITH DIAERESIS
  , ("Ș",           "S")        -- U+0218 LATIN CAPITAL LETTER S WITH COMMA BELOW
  , ("ș",           "s")        -- U+0219 LATIN SMALL LETTER S WITH COMMA BELOW
  , ("Ț",           "T")        -- U+021A LATIN CAPITAL LETTER T WITH COMMA BELOW
  , ("ț",           "t")        -- U+021B LATIN SMALL LETTER T WITH COMMA BELOW
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
  , ("σ",           "sigma")    -- U+03C3 GREEK SMALL LETTER SIGMA
  , ("ς",           "sigma")    -- U+03C2 GREEK SMALL LETTER FINAL SIGMA
  , ("τ",           "tau")      -- U+03C4 GREEK SMALL LETTER TAU
  , ("υ",           "upsilon")  -- U+03C5 GREEK SMALL LETTER UPSILON
  , ("φ",           "phi")      -- U+03C6 GREEK SMALL LETTER PHI
  , ("χ",           "chi")      -- U+03C7 GREEK SMALL LETTER CHI
  , ("ψ",           "psi")      -- U+03C8 GREEK SMALL LETTER PSI
  , ("ω",           "omega")    -- U+03C9 GREEK SMALL LETTER OMEGA
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
  , ("‡",           "")         -- U+2021 DOUBLE DAGGER
  , ("™",           "")         -- U+2122 TRADE MARK SIGN
  , ("�",          "")         -- U+FFFD REPLACEMENT CHARACTER
  ]

-- | All the characters substituions.
-- NB that the substituions are not commutative.
chSubst ∷ [(Text, Text)]
chSubst =  chHexSubst ++ chNNSubst ++ chDecSubst ++ chUnicodeSubst

------------------------------------------------------------------------------
-- Author and title substituions

commonSubst ∷ [(Text, Text)]
commonSubst =
  [ ("Ã¡",      "a")  -- U+00C3 and U+00A1 (LATIN SMALL LETTER A WITH GRAVE)
  , ("Ã©",      "e")  -- U+00C3 and U+00A9 (LATIN SMALL LETTER E WITH ACUTE)
  , ("Ã\x00AD", "i")  -- U+00C3 and U+00AD (LATIN SMALL LETTER I GRAVE)
  -- We erase `Ã¶` because it follows an `o` in the examples we know.
  , ("Ã¶",      "")   -- U+00C3 and U+00B6 (LATIN SMALL LETTER O WITH DIAERESIS)
  , ("Å›",      "s")  -- U+00C5 and U+203A (LATIN CAPITAL LETTER S WITH ACUTE)
  ]

------------------------------------------------------------------------------
-- Author substituions

authorSubst ∷ [(Text, Text)]
authorSubst =
  [ (", ",   ",")
  , (" and", ",")

  ]

------------------------------------------------------------------------------
-- Title substituions

-- These substituions should be done before converting to lower case.
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
