test-suite asum
   ghc-options:         -O2
                        -threaded
                        -eventlog
                        -rtsopts
   type:                exitcode-stdio-1.0
   default-language:    Haskell2010
   main-is:             tests/testASUM.hs
   build-depends:       base,
                        vector,
                        containers,
                        hblas,
                        time >= 1.6

test-suite idamax
   ghc-options:         -O2
                        -threaded
                        -eventlog
                        -rtsopts
   type:                exitcode-stdio-1.0
   default-language:    Haskell2010
   main-is:             tests/testIDAMAX.hs
   build-depends:       base,
                        vector,
                        containers,
                        hblas,
                        time >= 1.6

test-suite dot_10_14
   ghc-options:         -O2
                        -threaded
                        -eventlog
                        -rtsopts
   type:                exitcode-stdio-1.0
   default-language:    Haskell2010
   main-is:             tests/testDOT_10_14.hs
   build-depends:       base,
                        vector,
                        containers,
                        hblas,
                        time >= 1.6

test-suite dot_15_18
   ghc-options:         -O2
                        -threaded
                        -eventlog
                        -rtsopts
   type:                exitcode-stdio-1.0
   default-language:    Haskell2010
   main-is:             tests/testDOT_15_18.hs
   build-depends:       base,
                        vector,
                        containers,
                        hblas,
                        time >= 1.6

test-suite dot_19_21
   ghc-options:         -O2
                        -threaded
                        -eventlog
                        -rtsopts
   type:                exitcode-stdio-1.0
   default-language:    Haskell2010
   main-is:             tests/testDOT_19_21.hs
   build-depends:       base,
                        vector,
                        containers,
                        hblas,
                        time >= 1.6

test-suite dot_22_24
   ghc-options:         -O2
                        -threaded
                        -eventlog
                        -rtsopts
   type:                exitcode-stdio-1.0
   default-language:    Haskell2010
   main-is:             tests/testDOT_22_24.hs
   build-depends:       base,
                        vector,
                        containers,
                        hblas,
                        time >= 1.6


test-suite nrm2_10_14
   ghc-options:         -O2
                        -threaded
                        -eventlog
                        -rtsopts
   type:                exitcode-stdio-1.0
   default-language:    Haskell2010
   main-is:             tests/testNRM2_10_14.hs
   build-depends:       base,
                        vector,
                        containers,
                        hblas,
                        time >= 1.6

test-suite nrm2_15_18
   ghc-options:         -O2
                        -threaded
                        -eventlog
                        -rtsopts
   type:                exitcode-stdio-1.0
   default-language:    Haskell2010
   main-is:             tests/testNRM2_15_18.hs
   build-depends:       base,
                        vector,
                        containers,
                        hblas,
                        time >= 1.6

test-suite nrm2_19_21
   ghc-options:         -O2
                        -threaded
                        -eventlog
                        -rtsopts
   type:                exitcode-stdio-1.0
   default-language:    Haskell2010
   main-is:             tests/testNRM2_19_21.hs
   build-depends:       base,
                        vector,
                        containers,
                        hblas,
                        time >= 1.6

test-suite nrm2_22_24
   ghc-options:         -O2
                        -threaded
                        -eventlog
                        -rtsopts
   type:                exitcode-stdio-1.0
   default-language:    Haskell2010
   main-is:             tests/testNRM2_22_24.hs
   build-depends:       base,
                        vector,
                        containers,
                        hblas,
                        time >= 1.6


test-suite cg_01_09
   ghc-options:         -O2
                        -threaded
                        -eventlog
                        -rtsopts
   type:                exitcode-stdio-1.0
   default-language:    Haskell2010
   main-is:             examples/testCGM_01_09.hs
   build-depends:       base,
                        vector,
                        hblas,
                        time >= 1.6

test-suite cg_10
   ghc-options:         -O2
                        -threaded
                        -eventlog
                        -rtsopts
   type:                exitcode-stdio-1.0
   default-language:    Haskell2010
   main-is:             examples/testCGM_10.hs
   build-depends:       base,
                        vector,
                        hblas,
                        time >= 1.6


test-suite cg_11
   ghc-options:         -O2
                        -threaded
                        -eventlog
                        -rtsopts
   type:                exitcode-stdio-1.0
   default-language:    Haskell2010
   main-is:             examples/testCGM_11.hs
   build-depends:       base,
                        vector,
                        hblas,
                        time >= 1.6

test-suite cg_12
   ghc-options:         -O2
                        -threaded
                        -eventlog
                        -rtsopts
   type:                exitcode-stdio-1.0
   default-language:    Haskell2010
   main-is:             examples/testCGM_12.hs
   build-depends:       base,
                        vector,
                        hblas,
                        time >= 1.6


test-suite tfqmr_01_04
   ghc-options:         -O2
                        -threaded
                        -eventlog
                        -rtsopts
   type:                exitcode-stdio-1.0
   default-language:    Haskell2010
   main-is:             examples/testTFQMR_01_04.hs
   build-depends:       base,
                        vector,
                        hblas,
                        time >= 1.6

test-suite tfqmr_05_07
   ghc-options:         -O2
                        -threaded
                        -eventlog
                        -rtsopts
   type:                exitcode-stdio-1.0
   default-language:    Haskell2010
   main-is:             examples/testTFQMR_05_07.hs
   build-depends:       base,
                        vector,
                        hblas,
                        time >= 1.6

test-suite tfqmr_08_09
   ghc-options:         -O2
                        -threaded
                        -eventlog
                        -rtsopts
   type:                exitcode-stdio-1.0
   default-language:    Haskell2010
   main-is:             examples/testTFQMR_08_09.hs
   build-depends:       base,
                        vector,
                        hblas,
                        time >= 1.6


test-suite tfqmr_10
   ghc-options:         -O2
                        -threaded
                        -eventlog
                        -rtsopts
   type:                exitcode-stdio-1.0
   default-language:    Haskell2010
   main-is:             examples/testTFQMR_10.hs
   build-depends:       base,
                        vector,
                        hblas,
                        time >= 1.6

test-suite tfqmr_11
   ghc-options:         -O2
                        -threaded
                        -eventlog
                        -rtsopts
   type:                exitcode-stdio-1.0
   default-language:    Haskell2010
   main-is:             examples/testTFQMR_11.hs
   build-depends:       base,
                        vector,
                        hblas,
                        time >= 1.6

test-suite tfqmr_12
   ghc-options:         -O2
                        -threaded
                        -eventlog
                        -rtsopts
   type:                exitcode-stdio-1.0
   default-language:    Haskell2010
   main-is:             examples/testTFQMR_12.hs
   build-depends:       base,
                        vector,
                        hblas,
                        time >= 1.6

test-suite gemv
   ghc-options:         -O2
                        -threaded
                        -eventlog
                        -rtsopts
   type:                exitcode-stdio-1.0
   default-language:    Haskell2010
   main-is:             tests/testGEMV.hs
   build-depends:       base,
                        vector,
                        containers,
                        hblas,
                        time >= 1.6

test-suite trmv
   ghc-options:         -O2
                        -threaded
                        -eventlog
                        -rtsopts
   type:                exitcode-stdio-1.0
   default-language:    Haskell2010
   main-is:             tests/testTRMV.hs
   build-depends:       base,
                        vector,
                        containers,
                        hblas,
                        time >= 1.6

test-suite trsv
   ghc-options:         -O2
                        -threaded
                        -eventlog
                        -rtsopts
   type:                exitcode-stdio-1.0
   default-language:    Haskell2010
   main-is:             tests/testTRSV.hs
   build-depends:       base,
                        vector,
                        containers,
                        hblas,
                        time >= 1.6


test-suite symv
   ghc-options:         -O2
                        -threaded
                        -eventlog
                        -rtsopts
   type:                exitcode-stdio-1.0
   default-language:    Haskell2010
   main-is:             tests/testSYMV.hs
   build-depends:       base,
                        vector,
                        containers,
                        hblas,
                        time >= 1.6