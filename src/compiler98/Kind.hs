module Kind(Kind(..)) where

data Kind = Var
          | Arg
          | Method
          | TVar
          | Con
          | TCon
          | TSyn
          | TClass
          | TC
          | Modid
          | MethodInstance
          | MethodDefault
	  | Field

ordKind :: Kind -> Int
ordKind  Var    =  1
ordKind  Arg    =  1
ordKind  Method =  1
ordKind  TVar   =  2
ordKind  Con    =  3
ordKind  TCon   =  4
ordKind  TSyn   =  4
ordKind  TClass =  4
ordKind  TC     =  4
ordKind  Modid  =  5
ordKind  MethodInstance =  6
ordKind  MethodDefault  = 7
ordKind  Field =   8

instance Eq Kind where
         a      == b      = ordKind a == ordKind b

instance Ord Kind where
         a  <= b = ordKind a <= ordKind b
         a  <  b = ordKind a <  ordKind b
         compare a  b = compare (ordKind a) (ordKind b)


instance Show Kind where
  showsPrec d Var    = ("Identifier"++)
  showsPrec d Arg    = ("Argument"++)
  showsPrec d Method = ("Method"++)
  showsPrec d TVar   = ("Typevar"++)
  showsPrec d Con    = ("Constructor"++)
  showsPrec d TC     = ("Type constructor/class"++)
  showsPrec d TCon   = ("Type constructor"++)
  showsPrec d TSyn   = ("Type synonym"++)
  showsPrec d TClass = ("Type class"++)
  showsPrec d Modid  = ("Module identifier"++)
  showsPrec d MethodDefault  = ("Default method"++)
  showsPrec d MethodInstance = ("Instance method"++)
  showsPrec d Field = ("Field"++)

