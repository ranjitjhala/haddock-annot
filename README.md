Modified version of Haddock/Cabal to generate HTML documentation with
mouse-over types. See:

+ http://goto.ucsd.edu/~rjhala/Annot/

or in particular,

+ http://goto.ucsd.edu/~rjhala/Annot/Cabal/src/Distribution-Simple-Haddock.html#hscolour
+ http://goto.ucsd.edu/~rjhala/Annot/haddock/src/Haddock-Types.html#Interface

for examples.

Status
------

Ideally, this would be patched into the regular haddock distribution but
for an unfortunate dependency on the `syb` package. See: 

    http://bb10.com/haskell-cabal-devel/2011-06/msg00004.html

Have not found the time to remove the dependency on `syb` -- should be
quite easy, so it would be great if someone could do that!


Build
-----

Build and install cabal and haddock (from these sources) 
in the usual way (the current hscolour supports annotations directly.)
    
      
Usage: Cabal Packages
---------------------

To use generate annotated html source (with mouseover types)

    cabal haddock --hyperlink-source --annot --hscolour-css=path/to/css

To generate the HTML for a package in one shot 

    cabal haddock --hyperlink-source --annot

but you have to copy hscolour.css over to the destination html directory.

To use a particular annot.txt to generate files

    cabal hscolour --annot=dist/doc/html/hscolour/annot.txt  --css=hscolour.css

To use a specific `haddock` binary 

    cabal haddock -v3 --hyperlink-source --haddock-options="--annot=annot.txt" --with-haddock=/home/rjhala/.cabal/bin/haddock 

Usage: Single Files
-------------------

To work with a single `.hs` file and generate random.txt (with the annots)

    haddock Misc.hs --annot=random.txt

You can then generate the html by

    hscolour -acss=random.txt Misc.hs > goo.html
   


TODO
----

* wierd bug with --executables (off by one path -- see cabal-install case)

* remove the dependency on `syb` to facilitate merging into mainline  `haddock`


To generate `annot.txt` and render separately with `hscolour`

    cabal haddock  --hyperlink-source --haddock-options="--annot=annot.txt"


    cabal hscolour --hscolour-options="-acss=annot.txt" --hscolour-css=/home/jhala/research/haskellery/hscolour-1.18/hscolour.css 


