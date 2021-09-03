Argument documentation being rendered in single lines
  $ $TESTDIR/run.sh :mi System.Console.Haskeline
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

