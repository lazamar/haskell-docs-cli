A class with code examples, different methods, and different headings
  $ $TESTDIR/run.sh :dd Bifunctor
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
        
    





























































































































































































