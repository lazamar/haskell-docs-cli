Accepts data from standard input
  $ echo getInputLine | $TESTDIR/run.sh
  ---- haskell-docs-cli ----------------------------------------------------------
  Say :help for help and :quit to exit
  --------------------------------------------------------------------------------
  [>] 3  getInputLineWithInitial .+ (re)
     String) -> InputT m (Maybe String)
     haskeline System.Console.Haskeline
  2  getInputLine :: MonadIO m => Text -> m (InputResult Text)
     linenoise Linenoise, linenoise Linenoise.Unlift
  1  getInputLine :: (MonadIO m, MonadMask m) => String -> InputT m (Maybe String)
     haskeline System.Console.Haskeline
  search: getInputLine
  [>]  \(no-eol\) (re)
