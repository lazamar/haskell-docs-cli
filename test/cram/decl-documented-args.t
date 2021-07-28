  $ alias run="$TESTDIR/../../.stack-work/dist/x86_64-osx/Cabal-3.2.1.0/build/hoogle-cli-exe/hoogle-cli-exe --data-dir $TESTDIR/cram-data --unlimited-cache"

A function that has documentation for its arguments
  $ run :dd completeWord +haskeline
  
  completeWord  
    :: Monad m 
    => Maybe Char
      An optional escape character
      
    -> [Char]
      Characters which count as whitespace
      
    -> (String -> m [Completion])
      Function to produce a list of possible completions
      
    -> CompletionFunc m 
  
  
  
    A custom CompletionFunc which completes the word immediately to the left of 
    the cursor.
    
    A word begins either at the start of the line or after an unescaped whitespace
     character.
    









