# Calculate square root of integer in several languages.

## Caveats

    This project is documented only in french. Comments in code are in french.

## Language C
 
    Original version: nracine.c
        To compile: gcc -O2 -Wall -W -o nracine nracine.c
        Usage: ./nracine -r2 -p100
 
    Version using gmp: racine.c
        To compile: gcc -Wall -W -O2 racine.c -lm -lgmp -o racine
        Usage: ./racine 2 100 20
         2 is the number for we compute the square root.
         100 is the number of decimals
         20 is the granularity i.e. the number of computed decimals
         for each iteration.

## Scheme

    Version for guile: racine.scm
    Usage: guile racine.scm 2 100 20
    
## Common lisp
    
    Version for sbcl: lracine.lisp
    To compile: sbcl --script make-lracine.lisp
    Usage: ./lracine 2 100 20

## Haskell
 
    Version using list: lsqrt.hs
    To compile: ghc -O2 lsqrt.hs
    Usage: ./lsqrt 2 100 20

    Version using explicit recursion: nsqrt.hs
    To compile: ghc -O2 nsqrt.hs
    Usage: ./nsqrt 2 100 20
 
    Common function between lsqrt.hs and nsqrt.hs: SqrtUtils.hs
 
    Compute the integer part of a square root of an integer: SqrtFloorInt.hs
    To compile: ghc -O2 SqrtFloorInt.hs
    Usage: ./SqrtFloorInt 15
 
    An experience to compute the cube root: CubeRoot.hs 

## Python
    
    Usage: python racine.py 2 100 20
    
    heron.py compute the square root of integer using the newton algorithm
    with the decimal package. So it's slow.
    Usage: python heron.py 2
    Choosing the number of compute decimals: python heron.py -p100 2
    Choosing the number of iterations: python heron.py -i10 2
    Choosing both: python heron.py -i5 -p20 2

## Ruby
    
    Usage: ruby racine.rb 2 100 20

