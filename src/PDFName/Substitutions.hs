{-# LANGUAGE OverloadedStrings #-}

-- | Substitutions.

module PDFName.Substitutions
  (  replace
  , replaceUnicode
  , unicodeSubstTable
  ) where

import Data.Text ( Text )
import qualified Data.Text as T

------------------------------------------------------------------------------
replace :: [(Text,Text)] -> Text -> Text
replace xs ys = foldl (flip (uncurry T.replace)) ys xs

------------------------------------------------------------------------------
-- Unicode substitutions

replaceUnicode :: Text -> Text
replaceUnicode = replace unicodeSubstTable

unaryOp :: Text -> Text
unaryOp t = T.snoc t '-'

binaryOp :: Text -> Text
binaryOp t = T.cons '-' $ T.snoc t '-'

-- | Unicode substitutions table.
unicodeSubstTable :: [(Text, Text)]
unicodeSubstTable =
  -- TODO (2017-11-14). Added U+0000 NULL. Test case: Jan von Plato
  -- (2001). Natural deduction with general elimination rules.
  [ ("\t", "")  -- U+0009 CHARACTER TABULATION
  , ("\n", "")  -- U+000A LINE FEED (LF)
  , ("\f", "")  -- U+000C FORM FEED (FF)
  , ("\r", "")  -- U+000D CARRIAGE RETURN (CR)
  , (" ",  "-")  -- U+0020 SPACE
  , ("!", "")  -- U+0021
  , ("\"","")  -- U+0022 QUOTATION MARK
  , ("#", "")  -- U+0023
  , ("$", "")  -- U+0024
  , ("%", "")  -- U+0025
  , ("&", "")  -- U+0026
  , ("'", "")  -- U+0027
  , ("(", "-")  -- U+0028
  , (")", "-")  -- U+0029
  , ("*", "")  -- U+002A
  , ("+", binaryOp "plus")  -- U+002B
  , (",", "")  -- U+002C COMMA
  -- We do not substitute U+002D HYPHEN-MINUS.
  , (".", "")  -- U+002E FULL STOP
  , ("/", "")  -- U+002F SOLIDUS
  , (":", "")  -- U+003A
  , (";", "")  -- U+003B
  , ("<", binaryOp "less-than")  -- U+003C
  , ("=", binaryOp "equals")  -- U+003D
  , (">", binaryOp "greater-than")  -- U+003E
  , ("?", "")  -- U+003F
  , ("@", "")  -- U+0040
  , ("[", "")  -- U+005B
  , ("\\","")  -- U+005C REVERSE SOLIDUS
  , ("]", "")  -- U+005D
  , ("^", "")  -- U+005E
  , ("_", "-")  -- U+005F LOW LINE
  , ("`", "")  -- U+0060
  , ("{", "")  -- U+007B
  , ("|", "")  -- U+007C
  , ("}", "")  -- U+007B
  , ("~", "")  -- U+007E
  , ("¡", "")  -- U+00A1
  , ("¢", "cent")  -- U+00A2
  , ("£", "pound")  -- U+00A3
  , ("¤", "")  -- U+00A4
  , ("¥", "yen")  -- U+00A5
  , ("¦", "")  -- U+00A6 BROKEN BAR
  , ("§", "")  -- U+00A7
  , ("¨", "")  -- U+00A8
  , ("©", "")  -- U+00A9
  , ("ª", "")  -- U+00AA
  , ("«", "")  -- U+00AB
  , ("¬", "")  -- U+00AC

  -- GHC lexer rejects the literal "­" (U+00AD SOFT HYPHEN) character in
  -- source file. I replaced it with the hexadecimal escape "\xAD".
  , ("\xAD", "-")  -- U+00AD SOFT HYPHEN

  , ("®", "")  -- U+00AE
  , ("¯", "")  -- U+00AF
  , ("°", "degree")  -- U+00B0
  , ("±",  binaryOp "plus-minus")  -- U+00B1
  , ("²", "2")  -- U+00B2
  , ("³", "3")  -- U+00B3
  , ("´", "")  -- U+00B4
  , ("µ", "micro")  -- U+00B5 MICRO SIGN
  , ("¶", "")  -- U+00B6 PILCROW SIGN
  , ("·", "")  -- U+00B7
  , ("¸", "")  -- U+00B8
  , ("¹", "1")  -- U+00B9
  , ("º", "")  -- U+00BA
  , ("»", "")  -- U+00BB
  , ("¼", "")  -- U+00BC
  , ("½", "")  -- U+00BD
  , ("¾", "")  -- U+00BE
  , ("¿", "")  -- U+00BF
  , ("À", "A")  -- U+00C0
  , ("Á", "A")  -- U+00C1
  , ("Â", "A")  -- U+00C2
  , ("Ã", "A")  -- U+00C3
  , ("Ä", "A")  -- U+00C4
  , ("Å", "A")  -- U+00C5
  , ("Æ", "AE")  -- U+00C6 LATIN CAPITAL LETTER AE
  , ("Ç", "C")  -- U+00C7
  , ("È", "E")  -- U+00C8
  , ("É", "E")  -- U+00C9
  , ("Ê", "E")  -- U+00CA
  , ("Ë", "E")  -- U+00CB
  , ("Ì", "I")  -- U+00CC
  , ("Í", "I")  -- U+00CD
  , ("Î", "I")  -- U+00CE
  , ("Ï", "I")  -- U+00CF
  , ("Ð", "ETH")  -- U+00D0
  , ("Ñ", "N")  -- U+00D1
  , ("Ò", "O")  -- U+00D2
  , ("Ó", "O")  -- U+00D3
  , ("Ô", "O")  -- U+00D4
  , ("Õ", "O")  -- U+00D5
  , ("Ö", "O")  -- U+00D6
  , ("×", binaryOp "times")  -- U+00D7
  , ("Ø", "O")  -- U+00D8 LATIN CAPITAL LETTER O WITH STROKE
  , ("Ù", "U")  -- U+00D9
  , ("Ú", "U")  -- U+00DA
  , ("Û", "U")  -- U+00DB
  , ("Ü", "U")  -- U+00DC
  , ("Ý", "Y")  -- U+00DD
  , ("Þ", "THORN")  -- U+00DE
  , ("ß", "ss")  -- U+00DF LATIN SMALL LETTER SHARP S
  , ("à", "a")  -- U+00E0
  , ("á", "a")  -- U+00E1
  , ("â", "a")  -- U+00E2
  , ("ã", "a")  -- U+00E3
  , ("ä", "a")  -- U+00E4
  , ("å", "a")  -- U+00E5
  , ("æ", "ae")  -- U+00E6 LATIN SMALL LETTER AE
  , ("ç", "c")  -- U+00E7
  , ("è", "e")  -- U+00E8
  , ("é", "e")  -- U+00E9
  , ("ê", "e")  -- U+00EA
  , ("ë", "e")  -- U+00EB
  , ("ì", "i")  -- U+00EC
  , ("í", "i")  -- U+00ED
  , ("î", "i")  -- U+00EE
  , ("ï", "i")  -- U+00EF
  , ("ð", "eth") -- U+00F0
  , ("ñ", "n")  -- U+00F1
  , ("ò", "o")  -- U+00F2
  , ("ó", "o")  -- U+00F3
  , ("ô", "o")  -- U+00F4
  , ("õ", "o")  -- U+00F5
  , ("ö", "o")  -- U+00F6
  , ("÷", "")-- U+00F7
  , ("ø", "o")  -- U+00F8 LATIN SMALL LETTER O WITH STROKE
  , ("ù", "u")  -- U+00F9
  , ("ú", "u")  -- U+00FA
  , ("û", "u")  -- U+00FB
  , ("ü", "u")  -- U+00FC
  , ("ý", "y")  -- U+00FD
  , ("þ", "thorn")  -- U+00FE
  , ("ÿ", "y")  -- U+00FF
  , ("Ā", "A")  -- U+0100
  , ("ā", "a")  -- U+0101
  , ("Ă", "A")  -- U+0102
  , ("ă", "a")  -- U+0103
  , ("Ą", "A")  -- U+0104
  , ("ą", "a")  -- U+0105
  , ("Ć", "c")  -- U+0106
  , ("ć", "c")  -- U+0107
  , ("Ĉ", "C")  -- U+0108
  , ("ĉ", "c")  -- U+0109
  , ("Ċ", "C")  -- U+010A
  , ("ċ", "c")  -- U+010B
  , ("Č", "C")  -- U+010C
  , ("č", "c")  -- U+010D
  , ("Ď", "D")  -- U+010E
  , ("ď", "d")  -- U+010F
  , ("Đ", "D")  -- U+0110
  , ("đ", "d")  -- U+0111
  , ("Ē", "E")  -- U+0112
  , ("ē", "e")  -- U+0113
  -- NB. To add a test case if U+0114 is added.
  , ("ĕ", "e")  -- U+0115
  , ("Ė", "e")  -- U+0116
  , ("ė", "e")  -- U+0117
  , ("Ę", "e")  -- U+0118
  , ("ę", "e")  -- U+0119
  , ("Ě", "E")  -- U+011A
  , ("ě", "e")  -- U+011B
  , ("Ĝ", "G")  -- U+011C
  , ("ĝ", "g")  -- U+011D
  , ("Ğ", "G")  -- U+011E
  , ("ğ", "g")  -- U+011F
  , ("Ġ", "G")  -- U+0120
  , ("ġ", "g")  -- U+0121
  , ("Ģ", "G")  -- U+0122
  , ("ģ", "g")  -- U+0123 LATIN SMALL LETTER G WITH CEDILLA
  , ("Ĥ", "H")  -- U+0124
  , ("ĥ", "h")  -- U+0125
  , ("Ħ", "H")  -- U+0126
  , ("ħ", "h")  -- U+0127
  -- NB. To add a test case if U+0128 or U+0128 are added.
  , ("Ī", "I")  -- U+012A
  , ("ī", "I")  -- U+012B
  -- NB. To add a test case if U+012C isadded.
  , ("ĭ", "i")  -- U+012
  , ("Į", "I")  -- U+012E
  , ("į", "I")  -- U+012F
  , ("İ", "I")  -- U+0130
  , ("ı", "i")  -- U+0131
  , ("Ĳ", "IJ")  -- U+0132 LATIN CAPITAL LIGATURE IJ
  , ("ĳ", "ij")  -- U+0133 LATIN SMALL LIGATURE IJ
  , ("Ĵ", "J")  -- U+0134
  , ("ĵ", "j")  -- U+0135
  , ("Ķ", "K")  -- U+0136
  , ("ķ", "k")  -- U+0137
  , ("ĸ", "k")  -- U+0138 LATIN SMALL LETTER KRA
  -- NB. To add a test case if U+0139 or U+013A are added.
  , ("Ļ", "L")  -- U+013B
  , ("ļ", "l")  -- U+013C
  -- NB. To add a test case if U+013D, U+013E, U+013F or U+0140 are added.
  , ("Ł", "L")  -- U+0141
  , ("ł", "l")  -- U+0142
  , ("Ń", "N")  -- U+0143
  , ("ń", "n")  -- U+0144
  , ("Ņ", "n")  -- U+0145
  , ("ņ", "n")  -- U+0146
  , ("Ň", "N")  -- U+0147
  , ("ň", "n")  -- U+0148
  -- NB. To add a test case if U+0149 is added.
  , ("Ŋ", "ENG")  -- U+014A
  , ("ŋ", "eng")  -- U+014B
  , ("Ō", "O")  -- U+014C
  , ("ō", "o")  -- U+014D
  , ("Ő", "O")  -- U+0150
  , ("ő", "o")  -- U+0151
  , ("Œ", "OE")  -- U+0152 LATIN CAPITAL LIGATURE OE
  , ("œ", "oe")  -- U+0153 LATIN SMALL LIGATURE OE
  -- NB. To add a test case if U+0154 or U+0155 are added.
  , ("Ŗ", "R")  -- U+0156
  , ("ŗ", "r")  -- U+0157
  , ("Ř", "R")  -- U+0158
  , ("ř", "r")  -- U+0159
  , ("Ś", "S")  -- U+015A
  , ("ś", "s")  -- U+015B
  , ("Ŝ", "S")  -- U+015C
  , ("ŝ", "s")  -- U+015D
  , ("Ş", "S")  -- U+015E
  , ("ş", "s")  -- U+015F
  , ("Š", "S")  -- U+0160
  , ("š", "s")  -- U+0161
  -- NB. To add a test case if U+0162 or U+0164 are added.
  , ("Ť", "T")  -- U+0164
  , ("ť", "t")  -- U+0165
  , ("Ŧ", "T")  -- U+0166
  , ("ŧ", "t")  -- U+0167
  -- NB. To add a test case if U+0168 or U+0169 are added.
  , ("Ū", "U")  -- U+016A
  , ("ū", "u")  -- U+016B
  , ("Ŭ", "U")  -- U+016C
  , ("ŭ", "u")  -- U+016D
  , ("Ů", "U")  -- U+016E
  , ("ů", "u")  -- U+016F
  , ("Ű", "U")  -- U+0170
  , ("ű", "u")  -- U+0171
  , ("Ų", "U")  -- U+0172
  , ("ų", "u")  -- U+0173
  , ("Ŵ", "W")  -- U+0174
  , ("ŵ", "w")  -- U+0175
  , ("Ŷ", "Y")  -- U+0176
  , ("ŷ", "y")  -- U+0177
  , ("Ÿ", "Y")  -- U+0178
  , ("Ź", "Z")  -- U+0179
  , ("ź", "z")  -- U+017A
  , ("Ż", "Z")  -- U+017B
  , ("ż", "z")  -- U+017C
  , ("Ž", "z")  -- U+017D
  , ("ž", "z")  -- U+017E
  -- NB. To add a test case if U+017F is added.
  , ("Ə", "A")  -- U+018F LATIN CAPITAL LETTER SCHWA
  , ("Ơ", "O")  -- U+01A0 LATIN CAPITAL LETTER O WITH HORN
  , ("ơ", "o")  -- U+01A1 LATIN SMALL LETTER O WITH HORN
  , ("Ư", "U")  -- U+01AF LATIN CAPITAL LETTER U WITH HORN
  , ("ư", "u")  -- U+01B0 LATIN SMALL LETTER U WITH HORN
  , ("Ǟ", "A")  -- U+01DE LATIN CAPITAL LETTER A WITH DIAERESIS AND MACRON
  , ("ǟ", "a")  -- U+01DF LATIN SMALL LETTER A WITH DIAERESIS AND MACRON
  , ("Ș", "S")  -- U+0218 LATIN CAPITAL LETTER S WITH COMMA BELOW
  , ("ș", "s")  -- U+0219 LATIN SMALL LETTER S WITH COMMA BELOW
  , ("Ț", "T")  -- U+021A LATIN CAPITAL LETTER T WITH COMMA BELOW
  , ("ț", "t")  -- U+021B LATIN SMALL LETTER T WITH COMMA BELOW
  , ("Ȫ", "O")  -- U+022A LATIN CAPITAL LETTER O WITH DIAERESIS AND MACRON
  , ("ȫ", "o")  -- U+022B LATIN SMALL LETTER O WITH DIAERESIS AND MACRON
  , ("Ȭ", "O")  -- U+022C LATIN CAPITAL LETTER O WITH TILDE AND MACRON
  , ("ȭ", "o")  -- U+022D LATIN SMALL LETTER O WITH TILDE AND MACRON
  , ("Ȯ", "O")  -- U+022E LATIN CAPITAL LETTER O WITH DOT ABOVE
  , ("ȯ", "o")  -- U+022F LATIN SMALL LETTER O WITH DOT ABOVE
  , ("Ȱ", "O")  -- U+0230 LATIN CAPITAL LETTER O WITH DOT ABOVE AND MACRON
  , ("ȱ", "o")  -- U+0231 LATIN SMALL LETTER O WITH DOT ABOVE AND MACRON
  , ("Ȳ", "Y")  -- U+0232 LATIN CAPITAL LETTER Y WITH MACRON
  , ("ȳ", "y")  -- U+0233 LATIN SMALL LETTER Y WITH MACRON
  , ("ə", "a")  -- U+0253 LATIN SMALL LETTER SCHWA

  , ("ˆ", "")  -- U+02C6
  , ("˜", "")  -- U+02DC

  , ("Α", "Alpha")    -- U+0391
  , ("Β", "Beta")     -- U+0392
  , ("Γ", "Gamma")    -- U+0393
  , ("Δ", "Delta")    -- U+0394
  , ("Ε", "Epsilon")  -- U+0395
  , ("Ζ", "Zeta")     -- U+0396
  , ("Η", "Eta")      -- U+0397
  , ("Θ", "Theta")    -- U+0398
  , ("Ι", "Iota")     -- U+0399
  , ("Κ", "Kappa")    -- U+039A
  , ("Λ", "Lambda")   -- U+039B
  , ("Μ", "Mu")       -- U+039C
  , ("Ν", "Nu")       -- U+039D
  , ("Ξ", "Xi")       -- U+039E
  , ("Ο", "Omicron")  -- U+039F
  , ("Π", "Pi")       -- U+03A0
  , ("Ρ", "Rho")      -- U+03A1
  -- The code U+03A2 is reserved.
  , ("Σ", "Sigma")    -- U+03A3
  , ("Τ", "Tau")      -- U+03A4
  , ("Υ", "Upsilon")  -- U+03A5
  , ("Φ", "Phi")      -- U+03A6
  , ("χ", "Chi")      -- U+03A7
  , ("Ψ", "Psi")      -- U+03A8
  , ("Ω", "Omega")    -- U+03A9
  , ("α", "alpha")    -- U+03B1
  , ("β", "beta")     -- U+03B2
  , ("γ", "gamma")    -- U+03B3
  , ("δ", "delta")    -- U+03B4
  , ("ε", "epsilon")  -- U+03B5
  , ("ζ", "zeta")     -- U+03B6
  , ("η", "eta")      -- U+03B7
  , ("θ", "theta")    -- U+03B8
  , ("ι", "iota")     -- U+03B9
  , ("κ", "kappa")    -- U+03BA
  , ("λ", "lambda")   -- U+03BB
  , ("μ", "mu")       -- U+03BC
  , ("ν", "nu")       -- U+03BD
  , ("ξ", "xi")       -- U+03BE
  , ("ο", "omicron")  -- U+03BF
  , ("π", "pi")       -- U+03C0
  , ("ρ", "rho")      -- U+03C1
  , ("ς", "sigma")    -- U+03C2 GREEK SMALL LETTER FINAL SIGMA
  , ("σ", "sigma")    -- U+03C3
  , ("τ", "tau")      -- U+03C4
  , ("υ", "upsilon")  -- U+03C5
  , ("φ", "phi")      -- U+03C6
  , ("χ", "chi")      -- U+03C7
  , ("ψ", "psi")      -- U+03C8
  , ("ω", "omega")    -- U+03C9
  , ("ϑ", "theta")    -- U+03D1 GREEK THETA SYMBOL
  , ("ϒ", "Upsilon")  -- U+03D2 GREEK UPSILON WITH HOOK SYMBOL
  , ("ϕ", "phi")      -- U+03D5 GREEK PHI SYMBOL
  , ("ϖ", "pi")       -- U+03D6 GREEK PI SYMBOL

  , ("Ḑ", "D")  -- U+1E10 LATIN CAPITAL LETTER D WITH CEDILLA
  , ("ḑ", "d")  -- U+1E11 LATIN SMALL LETTER D WITH CEDILLA
  , ("Ẁ", "W")  -- U+1E80 LATIN CAPITAL LETTER W WITH GRAVE
  , ("ẁ", "w")  -- U+1E81 LATIN SMALL LETTER W WITH GRAVE
  , ("Ẃ", "W")  -- U+1E82 LATIN CAPITAL LETTER W WITH ACUTE
  , ("ẃ", "w")  -- U+1E83 LATIN SMALL LETTER W WITH ACUTE
  , ("ẞ", "SS")  -- U+1E9E LATIN CAPITAL LETTER SHARP S
  , (" ", "-")  -- U+2009 THIN SPACE
  , ("‐", "-")  -- U+2010 HYPHEN
  , ("–", "-")  -- U+2013 EN DASH
  , ("—", "-")  -- U+2014 EM DASH
  , ("‘", "")  -- U+2018 LEFT SINGLE QUOTATION MARK
  , ("’", "")  -- U+2019 RIGHT SINGLE QUOTATION MARK
  , ("‚", "")  -- U+201A SINGLE LOW-9 QUOTATION MARK
  , ("‛", "")  -- U+201B SINGLE HIGH-REVERSED-9 QUOTATION MARK
  , ("“", "")  -- U+201C LEFT DOUBLE QUOTATION MARK
  , ("”", "")  -- U+201D RIGHT DOUBLE QUOTATION MARK
  , ("„", "")  -- U+201E DOUBLE LOW-9 QUOTATION MARK
  , ("‟", "")  -- U+201F DOUBLE HIGH-REVERSED-9 QUOTATION MARK
  , ("†", "")  -- U+2020
  , ("‡", "")  -- U+2021
  , ("•", "")  -- U+2022
  , ("…", "")  -- U+2026 HORIZONTAL ELLIPSIS
  , ("›", "")  -- U+203A SINGLE RIGHT-POINTING ANGLE QUOTATION MARK
  , ("⁰", "0")  -- U+2070
  , ("ⁱ", "i")  -- U+2071
  -- The codes U+2072 and U+2073 are reserved.
  , ("⁴", "4")  -- U+2074
  , ("⁵", "5")  -- U+2075
  , ("⁶", "6")  -- U+2076
  , ("⁷", "7")  -- U+2077
  , ("⁸", "8")  -- U+2078
  , ("⁹", "9")  -- U+2079
  , ("₀", "0")  -- U+2080
  , ("₁", "1")  -- U+2081
  , ("₂", "2")  -- U+2082
  , ("₃", "3")  -- U+2083
  , ("₄", "4")  -- U+2074
  , ("₅", "5")  -- U+2075
  , ("₆", "6")  -- U+2076
  , ("₇", "7")  -- U+2077
  , ("₈", "8")  -- U+2078
  , ("₉", "9")  -- U+2079
  , ("ℂ", "C")  -- U+2102 DOUBLE-STRUCK CAPITAL C
  , ("ℊ", "g")  -- U+210A SCRIPT SMALL G
  , ("ℋ", "H")  -- U+210B SCRIPT CAPITAL H
  , ("ℍ", "H")  -- U+210D DOUBLE-STRUCK CAPITAL H
  , ("ℐ", "I")  -- U+2110 SCRIPT CAPITAL I
  , ("ℒ", "L")  -- U+2112 SCRIPT CAPITAL L
  , ("ℓ", "l")  -- U+2113 SCRIPT SMALL L
  , ("ℕ", "N")  -- U+2115 DOUBLE-STRUCK CAPITAL N
  , ("℘", "P")  -- U+2118 SCRIPT CAPITAL P
  , ("ℙ", "P")  -- U+2119 DOUBLE-STRUCK CAPITAL P
  , ("ℚ", "Q")  -- U+211A DOUBLE-STRUCK CAPITAL Q
  , ("ℛ", "R")  -- U+211B SCRIPT CAPITAL R
  , ("ℝ", "R")  -- U+211D DOUBLE-STRUCK CAPITAL R
  , ("™", "")  -- U+2122
  , ("ℤ", "Z")  -- U+2124 DOUBLE-STRUCK CAPITAL Z
  , ("ℬ", "B")  -- U+212C SCRIPT CAPITAL B
  , ("ℯ", "e")  -- U+212F SCRIPT SMALL E
  , ("ℰ", "E")  -- U+2130 SCRIPT CAPITAL E
  , ("ℱ", "F")  -- U+2131 SCRIPT CAPITAL F
  , ("ℳ", "M")  -- U+2133 SCRIPT CAPITAL M
  , ("ℴ", "o")  -- U+2134 SCRIPT SMALL O
  , ("ℵ", "aleph")  -- U+2135
  , ("ℶ", "beth")  -- U+2136
  , ("←", "")  -- U+2190 LEFTWARDS ARROW
  , ("↑", "")  -- U+2191 UPWARDS ARROW
  , ("→", "")  -- U+2192 RIGHTWARDS ARROW
  , ("↓", "")  -- U+2193 DOWNWARDS ARROW
  , ("↔", "")  -- U+2194 LEFT RIGHT ARROW
  , ("∀", unaryOp "for-all")  -- U+2200
  , ("∂", "")  -- U+2202
  , ("∃", unaryOp "exists")  -- U+2203
  , ("∅", "empty-set")  -- U+2205
  , ("∇", "nabla")  -- U+2207
  , ("∈", "belong")  -- U+2208
  , ("∉", "not-belong")  -- U+2209
  , ("−", "minus")  -- U+2212
  , ("∓", binaryOp "minus-or-plus")  -- U+2213
  , ("∗", "")  -- U+2217
  , ("√", unaryOp "sqrt")  -- U+221A
  , ("⊂", "")  -- U+2282
  , ("⊃", "")  -- U+2283
  , ("⊄", "")  -- U+2284
  , ("⊅", "")  -- U+2285
  , ("⊆", "")  -- U+2286
  , ("⊇", "")  -- U+2287
  , ("⊈", "")  -- U+2288
  , ("⊉", "")  -- U+2289
  , ("⊊", "")  -- U+228A
  , ("⊋", "")  -- U+228B
  , ("⊕", binaryOp "circled-plus")  -- U+2295
  , ("⊗", binaryOp "circled-times")  -- U+2297
  , ("⋉", "")  -- U+22C9) LEFT NORMAL FACTOR SEMIDIRECT PRODUCT
  , ("⋊", "")  -- U+22CA RIGHT NORMAL FACTOR SEMIDIRECT PRODUCT
  , ("⌝", "")  -- U+231D TOP RIGHT CORNER
  , ("�", "")  -- U+FFFD REPLACEMENT CHARACTER
  ]
