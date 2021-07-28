  $ alias run="$TESTDIR/../.stack-work/dist/x86_64-osx/Cabal-3.2.1.0/build/hoogle-cli-exe/hoogle-cli-exe --data-dir $TESTDIR/cram-data"

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
    
Argument documentation being rendered in single lines
  $ run :mi System.Console.Haskeline
  ================================================================================
    System.Console.Haskeline
  ================================================================================
  1  data InputT m a 
  2  runInputT :: (MonadIO m, MonadMask m) => Settings m -> InputT m a -> m a 
  3  haveTerminalUI :: Monad m => InputT m Bool 
  4  mapInputT :: (forall b. m b -> m b) -> InputT m a -> InputT m a 
  5  data Behavior 
  6  runInputTBehavior :: (MonadIO m, MonadMask m) => Behavior -> Settings m ->
      InputT m a -> m a 
  7  defaultBehavior :: Behavior 
  8  useFileHandle :: Handle -> Behavior 
  9  useFile :: FilePath -> Behavior 
  10 preferTerm :: Behavior 
  11 getInputLine :: (MonadIO m, MonadMask m) => String -> InputT m 
     (Maybe String) 
  12 getInputLineWithInitial :: (MonadIO m, MonadMask m) => String -> (String,
      String) -> InputT m (Maybe String) 
  13 getInputChar :: (MonadIO m, MonadMask m) => String -> InputT m (Maybe Char) 
  14 getPassword :: (MonadIO m, MonadMask m) => Maybe Char -> String -> InputT m 
     (Maybe String) 
  15 waitForAnyKey :: (MonadIO m, MonadMask m) => String -> InputT m Bool 
  16 outputStr :: MonadIO m => String -> InputT m () 
  17 outputStrLn :: MonadIO m => String -> InputT m () 
  18 getExternalPrint :: MonadIO m => InputT m (String -> IO ()) 
  19 data Settings m 
  20 defaultSettings :: MonadIO m => Settings m 
  21 setComplete :: CompletionFunc m -> Settings m -> Settings m 
  22 data Prefs 
  23 readPrefs :: FilePath -> IO Prefs 
  24 defaultPrefs :: Prefs 
  25 runInputTWithPrefs :: (MonadIO m, MonadMask m) => Prefs -> Settings m ->
      InputT m a -> m a 
  26 runInputTBehaviorWithPrefs :: (MonadIO m, MonadMask m) => Behavior -> Prefs 
     -> Settings m -> InputT m a -> m a 
  27 withRunInBase :: Monad m => ((forall a. InputT m a -> m a) -> m b) -> InputT 
     m b 
  28 getHistory :: MonadIO m => InputT m History 
  29 putHistory :: MonadIO m => History -> InputT m () 
  30 modifyHistory :: MonadIO m => (History -> History) -> InputT m () 
  31 withInterrupt :: (MonadIO m, MonadMask m) => InputT m a -> InputT m a 
  32 data Interrupt 
  33 handleInterrupt :: MonadMask m => m a -> m a -> m a 
  34 module System.Console.Haskeline.Completion

A class with code examples, different methods, and different headings
  $ run :dd Bifunctor
  class Bifunctor p where 
  
    A bifunctor is a type constructor that takes two type arguments and is a 
    functor in both arguments. That is, unlike with Functor, a type constructor 
    such as Either does not need to be partially applied for a Bifunctor instance,
     and the methods in this class permit mapping functions over the Left value or
     the Right value, or both at the same time.
    
    Formally, the class Bifunctor represents a bifunctor from Hask -> Hask.
    
    Intuitively it is a bifunctor where both the first and second arguments are 
    covariant.
    
    You can define a Bifunctor by either defining bimap or by defining both first 
    and second.
    
    If you supply bimap, you should ensure that:
    
      bimap id id \xe2\x89\xa1 id (esc)
      
    If you supply first and second, ensure:
    
      first id \xe2\x89\xa1 id (esc)
      second id \xe2\x89\xa1 id (esc)
      
      
    If you supply both, you should also ensure:
    
      bimap f g \xe2\x89\xa1 first f . second g (esc)
      
    These ensure by parametricity:
    
      bimap  (f . g) (h . i) \xe2\x89\xa1 bimap f h . bimap g i (esc)
      first  (f . g) \xe2\x89\xa1 first  f . first  g (esc)
      second (f . g) \xe2\x89\xa1 second f . second g (esc)
      
      
    Since: base-4.8.0.0
    
  
  Minimal complete definition
  
  bimap | first, second
  
  
    Methods
    
    bimap :: (a -> b) -> (c -> d) -> p a c -> p b d  
    
      Map over both arguments at the same time.
      
        bimap f g \xe2\x89\xa1 first f . second g (esc)
        
      #### Examples
      
      Expand
      
        >>> bimap toUpper (+1) ('j', 3)
        ('J',4)
        
        
        >>> bimap toUpper (+1) (Left 'j')
        Left 'J'
        
        
        >>> bimap toUpper (+1) (Right 3)
        Right 4
        
        
    first :: (a -> b) -> p a c -> p b c  
    
      Map covariantly over the first argument.
      
        first f \xe2\x89\xa1 bimap f id (esc)
        
      #### Examples
      
      Expand
      
        >>> first toUpper ('j', 3)
        ('J',3)
        
        
        >>> first toUpper (Left 'j')
        Left 'J'
        
        
    second :: (b -> c) -> p a b -> p a c  
    
      Map covariantly over the second argument.
      
        second \xe2\x89\xa1 bimap id (esc)
        
      #### Examples
      
      Expand
      
        >>> second (+1) ('j', 3)
        ('j',4)
        
        
        >>> second (+1) (Right 3)
        Right 4
        
        
  
    #### Instances
    
    Instances details
    -  Bifunctor Either 
        Since: base-4.8.0.0
        
    -  Bifunctor (,) 
        Since: base-4.8.0.0
        
    -  Bifunctor Arg 
        Since: base-4.9.0.0
        
    -  Bifunctor ((,,) x1) 
        Since: base-4.8.0.0
        
    -  Bifunctor (Const :: Type -> Type -> Type) 
        Since: base-4.8.0.0
        
    -  Bifunctor (K1 i :: Type -> Type -> Type) 
        Since: base-4.9.0.0
        
    -  Bifunctor ((,,,) x1 x2) 
        Since: base-4.8.0.0
        
    -  Bifunctor ((,,,,) x1 x2 x3) 
        Since: base-4.8.0.0
        
    -  Bifunctor ((,,,,,) x1 x2 x3 x4) 
        Since: base-4.8.0.0
        
    -  Bifunctor ((,,,,,,) x1 x2 x3 x4 x5) 
        Since: base-4.8.0.0
        
    
A full package documentation
  $ run :pd directory
  ================================================================================
    directory: Platform-agnostic library for filesystem operations
  ================================================================================
  https://hackage.haskell.org/package/directory
  Description
     
    This library provides a basic set of operations for manipulating files and 
    directories in a portable way.
      [Skip to Readme] 
  Readme
    
    # directory
     
      
     
    Documentation can be found on Hackage. Changes between versions are recorded 
    in the change log.
     
    ## Building from Git repository
     
    When building this package directly from the Git repository, one must run
     autoreconf -fi to generate the configure script needed by cabal configure. 
    This requires Autoconf to be installed.
     
      autoreconf -fi
      cabal install
      
       
    There is no need to run the configure script manually however, as cabal 
    configure does that automatically.
     
  Properties
    Versions [RSS]  [faq]
      1.0.0.0, 1.0.0.3, 1.0.1.0, 1.0.1.1, 1.0.1.2, 1.1.0.0, 1.1.0.1, 1.1.0.2,
       1.2.0.0, 1.2.0.1, 1.2.1.0, 1.2.2.0, 1.2.2.1, 1.2.3.0, 1.2.3.1, 1.2.4.0,
       1.2.5.0, 1.2.5.1, 1.2.6.1, 1.2.6.2, 1.2.6.3, 1.2.7.0, 1.2.7.1, 1.3.0.0,
       1.3.0.1, 1.3.0.2, 1.3.1.0, 1.3.1.1, 1.3.1.2, 1.3.1.3, 1.3.1.4, 1.3.1.5,
       1.3.2.0, 1.3.2.1, 1.3.2.2, 1.3.3.0, 1.3.3.1, 1.3.3.2, 1.3.4.0, 1.3.5.0,
       1.3.6.0, 1.3.6.1, 1.3.6.2 (info)
    Change\xc2\xa0log (esc)
      changelog.md
    Dependencies
      base (>=4.5 && <4.17), filepath (>=1.3 && <1.5), time (>=1.4 && <1.12),
       unix (>=2.5.1 && <2.9), Win32 (>=2.2.2 && <2.13) [details]
    License
      BSD-3-Clause
    Author
      
    Maintainer
      libraries@haskell.org
    Category
      System
    Bug\xc2\xa0tracker (esc)
        https://github.com/haskell/directory/issues  
    Source\xc2\xa0repo (esc)
      head: git clone https://github.com/haskell/directory
    Uploaded
      by Rufflewind at 2021-05-24T08:12:17Z
    Distributions
      Arch:1.3.6.0, Fedora:1.3.6.0
    Downloads
      116277 total (1048 in the last 30 days)
     Rating
      (no votes yet) [estimated by Bayesian average]
    Status
      
      Docs available [build log]
      Last success reported on 2021-05-24 [all 1 reports]
