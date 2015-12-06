In order to compile the sources you need to install several packages:

cabal install MonadRandom
cabal install csv-conduit
cabal install vector
cabal install HUnit
cabal install test-framework
cabal install test-framework-hunit

Compiling the sources:
ghc Main.hs
ghc FCMTests.hs

To run the unit-tests:
FCMTests.exe

Command line arguments to Main.exe:
    "-s" - to pass a separator; optional; usage: -s , 
    "-sh" - to skip a header; optional; usage: -sh
    "-sfc" - to skip a first column; optional; usage: -sfc 
    "-slc" - to skip a last column; optional; usage: -slc
    "-outFile" - to put the results in a file; optional; usage: -outFile results.txt
    "-c" - to specify clusters' count; optional; usage: -c 5
    "-m" : to specify exponentional weight; optional; usage: -m 3
    "-eps" - to specify the calculations' precision; optional; usage: -eps = 0.0000001
    "-d" - to specify a metric to calculate vectors' distance; optional; usage: -d Hamming
                                                                         usage: -d Euclid
    "-initStep" - to specify how to start the FCM algorithm; optional; usage: -initStep RandomPartitioinMatrix
                                                                       usage: -initStep RandomClusterCenters
                                                                       
Please note, that the last argument has to be a path to the file with input data, it's a required argument.
If you don't pass an optional optional argument, a default one will be used.

Usage examples:
Main.exe data/butterfly.txt  
Main.exe -s , -d Hamming -initStep RandomClusterCenters -outFile results.txt data/butterfly.txt