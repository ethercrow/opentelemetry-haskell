{-# LANGUAGE CPP #-}

#if __GLASGOW_HASKELL__ >= 808
{-# OPTIONS_GHC -F -pgmF tasty-discover #-}
#else
main :: IO ()
main = do
  putStrLn "Tests are not available below ghc 8.8"
  putStrLn "Use ghc 8.8 or above to run the tests"
#endif
