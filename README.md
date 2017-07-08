pdfname [![Hackage version](https://img.shields.io/hackage/v/pdfname.svg?label=Hackage)](http://hackage.haskell.org/package/pdfname)
=======

Description
-----------

Name a PDF file using information (author, year of creation and title)
from the `pdfinfo` program.


Prerequisites
--------------

* Glasgow Haskell Compiler ([GHC](https://www.haskell.org/ghc/))

* [cabal-install](http://www.haskell.org/cabal/)

* [pdfinfo](http://linuxcommand.org/man_pages/pdfinfo1.html) Unix
  program


Installation
------------

The program can installed with the following commands:

```bash
$ cabal update
$ cabal install pdfname
```

Usage
-----

Just run `pdfname` on your PDF file.


How is the FileName Chosen?
---------------------------

Given the author, year of creation and title information extracted
from the `pdfinfo` program (fields `Author`, `CreationDate` and
`Title`, respectively) the name of the PDF file will be

```
author-year.title.pdf
```

where `author` and `title` are the strings obtained after making
certain substitutions (e.g. remove whitespace, translate non-ASCCI
characters and remove TeX/LaTeX specific information) to the
information shown by the `pdfinfo` program.

Example. Let's suppose that running `pdfinfo` on the file `foo.pdf`
shows the following (fictional and incomplete) information:

```bash
$ pdfinfo foo.pdf
Title:          Introducction to the <TEX>$\lambda$</TEX>-Calculus
Author:         Per Martin-Löf
CreationDate:   Fri Apr  9 07:14:01 2010
```

Now, running `pdfname` on that file will create the new file

```
/tmp/martin-lof-2010.introduction-to-the-lambda-calculus.pdf
```
