  $ alias run="$TESTDIR/../.stack-work/dist/x86_64-osx/Cabal-3.2.1.0/build/hoogle-cli-exe/hoogle-cli-exe"
  $ run :dd completeWord +haskeline
  
  \x1b[96m\x1b[1mcompleteWord\x1b[0;96m\x1b[0m   (esc)
    :: \x1b[96mMonad\x1b[0m m  (esc)
    => \x1b[96mMaybe\x1b[0m \x1b[96mChar\x1b[0m (esc)
      An optional escape character
      
    -> [\x1b[96mChar\x1b[0m] (esc)
      Characters which count as whitespace
      
    -> (\x1b[96mString\x1b[0m -> m [\x1b[96mCompletion\x1b[0m]) (esc)
      Function to produce a list of possible completions
      
    -> \x1b[96mCompletionFunc\x1b[0m m  (esc)
  
  
  
    A custom \x1b[90m\x1b[96mCompletionFunc\x1b[0;90m\x1b[0m which completes the word immediately to the left of  (esc)
    the cursor.
    
    A word begins either at the start of the line or after an unescaped whitespace
     character.
    
