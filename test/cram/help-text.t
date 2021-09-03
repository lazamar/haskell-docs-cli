Test command line help text
  $ $TESTDIR/run.sh --help
  haskell-docs-cli
  
    Search Hoogle and view Hackage documentation from the command line.
    Search modules, packages, types and functions by name or by approximate type signature.
  
  Usage: hdc [CMD] [--data-dir PATH] [--cache unlimited|off] [--hoogle URL] 
             [--hackage URL]
  
  Available options:
    -h,--help                Show this help text
    --data-dir PATH          Specify the directory for application data such as
                             requests cache to be stored.
    --cache unlimited|off    Set a custom cache eviction policy
    --hoogle URL             Address of Hoogle instance to be used
    --hackage URL            Address of Hackage instance to be used
  
  More info at <https://github.com/lazamar/haskell-docs-cli>
  






Test help text inside application
  $ $TESTDIR/run.sh :help
  Commands:
    :documentation <selector>    
    :interface <selector>        
    :src <selector>              View the source code of a function or type
                                 Set the editor with the 'EDITOR' environment variable.
    :declaration <selector>      View the Hackage documentation for a function or type
    :ddocumentation <selector>   Alias of :declaration
    :module <selector>           View documentation for a module matching a selector
    :mdocumentation <selector>   Alias of :module
    :minterface <selector>       View a module's interface
    :package <selector>          View documentation for a package matching a selector
    :pdocumentation <selector>   Alias of :package
    :pinterface <selector>       View a package's interface
    :help                        View this help text
    :quit                        Exit the program
  
  Selectors:
    <int>    select an option by index
    /<str>   select an option by prefix
    <str>    search for an option
  
  Examples:
    takeWhile             View Hoogle search results for 'takeWhile'
    :package containers   View package documentation for the 'containers' package
    :module Data.List     View module documentation for the 'Data.List' module
    :src insertWith       View the source for the first Hoogle result for 'insertWith'
    :package 2            View package documentation for the item with index 2 in the
                          current context
    :module /tak          View module documentation for the first item with prefix
                          'tak' in the current context
  
  More info at <https://github.com/lazamar/haskell-docs-cli>
  




