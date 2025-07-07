 -- Do not load all the prelude definitions automatically
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TupleSections  #-}

module Main where

-- required only some of the functions from the standard library
import Prelude(Num, Integer, IO, (.), ($), fst, snd, (+), (*), print, (&&), (||), Bool(True, False))

-- Void is an empty type that has no members and can not be constructed
data Void

-- Either represents the alternative of either a left or right kind of value
data Either l r = Left l | Right r

-- maybe is a type containg either a value of some type or nothing
data Maybe t = None | Some t

-- Semigroup is a type that provides a function two combine two 
-- values to a third
class Semigroup sem where
    combine :: sem -> sem -> sem

-- A Monoid is a semigroup that additionally has
-- an identity element
-- it is required that forall x: (combine identity x) === (combine x identity) === x
-- for the Semigroup and Monoid definitions to make sense/behave correctly
-- but this requirement can not be expressed inside Haskell via the type system but must
-- be assumed/tested/assured/required/asserted/forced by other means
-- Note: all the type classes below have similar requirements regarding the behaviors
-- of there respective functions but they will be not further mentioned.
-- Read the documentation for the actual official type class definitions for a better understanding
class Semigroup mon => Monoid mon where
    identity  :: mon

-- a (covariant) functor is a provider of values
-- that allows to transform each value via a given function
class Functor fun where
    map :: (a -> b) -> fun a -> fun b

-- a contravarient (functor) is a consumer of values
-- that allows to preprocess each value via a given function
class Contravariant cof where
    contramap :: (b -> a) -> cof a -> cof b

-- a (covariant) bi-functor is a provider of two kinds of values
-- that allows for both kind of values to transform each value 
-- with a given function
class Bifunctor pro where
    bimap :: (a -> b) -> (c -> d) -> pro a c -> pro b d

-- a pro functor is a provider of values of one kind
-- and a consumer of values of another kind
-- that allows both to transform the consumed value via
-- a given function and the produced values via another function
class Profunctor pro where
    dimap :: (a -> b) -> (c -> d) -> pro b c -> pro a d


-- a foldable is a structure containing values of some kind
-- and providing a way to combine a structure into a single
-- value of some kind, given a binary function 
class Foldable fo where
    foldr :: (a -> b -> b) -> b -> fo a -> b

-- A functor is pointed if an instance can be
-- constructed from a single value
class Functor fun => Pointed fun where
    inject :: a -> fun a -- Coalgebra fun a

-- functor is copointed if a single value can be
-- extracted from the inner structure
class Functor fun => Copointed fun where
    extract :: fun a -> a -- Algebra fun a

-- a pointed functor is an applicative if
-- if provided functions can be applied to provided values
-- resulting in a provider of results
class Pointed app => Applicative app where
    apply :: app (a -> b) -> app a -> app b

-- an applicative is an alternative if two providers
-- of values (of the same type) can be combined into a single
-- provider
-- and if there is a neutral element of this combining operation
class Applicative alt => Alternative alt where
    neutral :: alt a
    either :: alt a -> alt a -> alt a

-- a contra variant functor (consumer of values) is divisible if 
-- if a consumer for values of type b and a consumer of values of type c
-- can be combined into a consumer of type a, given a function that splits
-- values of type a into tuples (b,c)
-- and given that a consumer for any value type can be constructed from nothing
class Contravariant f => Divisible f where
    conquer :: f a
    divide  :: (a -> (b, c)) -> f b -> f c -> f a

-- contra variant functor (consumer of values) is decidable 
-- if it is divisible and also allows two consumers for values
-- of type b and c to be combined into a consumer of type a
-- given a function that turns values of type a into either
-- values of type b or values of type c
class Divisible f => Decidable f where
    lose   :: (a -> Void) -> f a
    choose :: (a -> Either b c) -> f b -> f c -> f a


-- a monad is an applicative covariant functor
-- that allows to turn a provider of values of type a
-- into a provider of values of type b via a function that turns
-- each original value of type b into its own provider of type b
class Applicative mon => Monad mon where
    bind :: mon a -> (a -> mon b) -> mon b

class Copointed com => Comonad com where
    extend :: (com a -> b) -> com a -> com b

-- a covariant functor is traversable if it is foldable
-- and allows to collapse a structure containg applicatives
-- into a single applicative containing the structure
class (Functor trav, Foldable trav) => Traversable trav where
    sequence :: Applicative app =>  trav (app a) -> app (trav a)


-- a Semigroupoid is a type with two paramters that allows
-- to compose (type a b) and (type b c) into (type a c)
class Semigroupoid sem where
    compose :: sem b c -> sem a b -> sem a c

-- a category is a Semigroupoid that has an identity element (type a a)
class Semigroupoid cat => Category cat where
    id :: cat a a

-- an arrow is a category that can be constructed from a function and
-- that allows turning a type working on values a and b into working
-- on tuples (a,c) and (b,c)
class Category arr => Arrow arr where
    arr :: (a -> b) -> arr a b
    first :: arr a b -> arr (a,c) (b,c)

-- functions are semigroupoids because they can be composed
instance Semigroupoid (->) where
    compose = (.)

-- functions are functors because they can be seen as value
-- providers of their return values (that can be transformed)
instance Functor ((->) a) where
    map = (.)

-- Op is a helper to flip the type arguments
-- of the function type, to make the return type the first type parameter 
-- and theargument type the last type parameter
newtype Op b a = Op {runOp :: a -> b}

-- a predicate a function with return type Bool
type Predicate x = Op Bool

-- Functions a contra variant functors (value consumers) in respect to 
-- their argument type.
-- they consume their argument and the argument can be transformed by pre-composing
-- another function
instance Contravariant (Op a) where
    contramap a (Op b) = Op (compose b a)

-- These types below are simple wrapper types of a single
-- value.
-- Their purpose is to allow implementing instances of typeclasses
-- with specific semantics

-- Identity is a simple wrapper that delegates everything to its inner type
newtype Identity a = Identity { getIdentity :: a }

-- Const is a simple wrapper thats dicards transformations of its inner type
newtype Const a = Const { getConst :: a }

-- Sum is a wrapper around numeric types exposing ADDITION as general "combining operation"
newtype Num a => Sum a = Sum { runSum :: a }

-- Product is a wrapper around numeric types exposing MULTIPLICATION as general "combining operation"
newtype Num a => Product a = Product { runProduct :: a }

-- Conj is a wrapper around Boolean types that exposes LOGICAL AND as general "combining operation"
newtype Conj = Conj { runConj :: Bool }

-- Disj is a wrapper around Boolean types that exposes LOGICAL OR as general "combining operation"
newtype Disj = Disj { runDisj :: Bool }

-- Fix is a recursive type
-- it can be used to turn a functor into and infinite nested typed
newtype Fix f = Fix {unfix :: f (Fix f)}

-- definine the "combine" function in respect to summation as sum of two numbers
instance Num a => Semigroup (Sum a) where
    combine(Sum a) (Sum b) = Sum $ a + b

-- define the identity in respect to summation as 0
instance Num a => Monoid (Sum a) where
    identity = Sum 0

-- definine the "combine" function in respect to multiplication as product of two numbers
instance Num a => Semigroup (Product a) where
    combine(Product a) (Product b) = Product $ a * b

-- define the identity in respect to multiplication as 1
instance Num a => Monoid (Product a) where
    identity = Product 1

-- define the "combine" funnction in respect to conjugation as logical AND
instance Semigroup Conj where
    combine(Conj a) (Conj b) = Conj $ a && b

-- define the identity in respect to conjugation as logical TRUE
instance  Monoid Conj where
    identity = Conj True


-- define the "combine" funnction in respect to conjugation as logical OR
instance Semigroup Disj where
    combine(Disj a) (Disj b) = Disj $ a || b

-- define the identity in respect to conjugation as logical FALSe
instance  Monoid Disj where
    identity = Disj False

-- implement functor map over Identity as delegation to the wrapped value
instance Functor Identity where
    map f (Identity x) = Identity $ f x

-- ListF is a Pattern Functor /Base Functor for the List data type
-- instead of by a recursive data type it just consists of the various cases
-- and is generic/polymorphic over the field that usally holds the recursive part
-- this allows it to be used for both the recursive construction of a propert List
-- but also as a container type for storing other values as summaries of the tail of a list
-- for example ConstF can hold the head value of a list and a value that has already been calculated (reduced)
-- from the original tail of the list (instead of the actual tail)
data ListF a r = NilF | ConsF a r

-- Together with the Fix type a recursive definition of List can be constructed from the non-recursive ListF type
-- by injecting the Fix into its second polymorpic type
type ListFix a = (Fix (ListF a))

-- Instead of defining List as 
-- data List a = Nil | Cons a (List a)
-- it is defined as newtype of the fixpointed ListF
newtype List t = List (ListFix t)
-- alternative:
-- newtype List t = List (Fix (ListF t))


-- ListF is a functor over its RECURSIVE part, OTHER THAN (Functor List)
-- this allows the map function to replace the recursive part of a list
instance Functor (ListF a) where
    map _ NilF = NilF
    map f (ConsF v r) = ConsF v $ f r

-- this is the proper (Functor List) instance that applies the transformation to the head part of the list
-- recursively througout the complete tail
instance Functor List where
    map f (List l) = List $ cata alg l
        where
            alg NilF = Fix NilF
            alg (ConsF x r) = Fix (ConsF (f x) r)

-- cata a recursion scheme that applies an algebra to the a fixpoint of a pattern functor
-- in order to reduce a recursive structor into a single value
-- the pattern functor encodes the various (base) cases of the recursion and acts as container
-- of the results from deeper recursionc calles
cata :: Functor f => (f a -> a) -> Fix f -> a
cata alg = go
  where
    go (Fix fx) = alg (map go fx)

-- ana is a recursion scheme that applies an coalgebra to a seed value to build up a fixpoint of a
-- pattern functor recuresively.
-- the coalgebra takes a seed value and returns on of the cases of the pattern functor containing a seed value for the next iteration
-- depending on the returned case the recursion continues building a recursive nested of the pattern functor
ana :: Functor f => (a -> f a) -> a -> Fix f
ana coalg = go
  where
    go a = Fix $ map go (coalg a)

-- hylo is a combination of ana and cata
-- it takes one coalgebra that builds a recursive structure from a seed values
-- and one algebra that reduces the recursive structure into a single value
-- the "building up" and "reducing down" are interleaved in a way that part of the structure is already
-- reduced into a value while part of it is still built up 
hylo :: Functor f => (f b -> b) -> (a -> f a) -> a -> b
hylo alg coalg = go
  where
    go = compose (compose alg $ map go) coalg

-- NonEmptyListF is a pattern functor similar to list, 
--- except that the Nil case also contains a value
data NonEmptyListF a r = NonEmptyNilF a | NonEmptyConsF a r

-- again the fixpoint of the NonEmptyF pattern functor
type NonEmptyListFix a = (Fix (NonEmptyListF a))

-- newtype wrapper for NonEmptyListFix
newtype NonEmptyList t = NonEmptyList (NonEmptyListFix t)

-- the NonEmptyListF pattern functor implementation over the recursive field
instance Functor (NonEmptyListF a) where
    map _ (NonEmptyNilF v) = NonEmptyNilF v
    map f (NonEmptyConsF v r) = NonEmptyConsF v $ f r

-- this is the proper (Functor List) instance that applies the transformation to the head part of the list
-- recursively througout the complete tail
instance Functor NonEmptyList where
    map f (NonEmptyList l) = NonEmptyList $ cata alg l
        where
            alg (NonEmptyNilF x) = Fix $ NonEmptyNilF (f x)
            alg (NonEmptyConsF x r) = Fix (NonEmptyConsF (f x) r)


-- FreeF is a pattern functor for a FreeMonat
-- compare:
-- data NonEmptyListF a r = NonEmptyNilF a | NonEmptyConsF a r
data FreeF fun leaf rec
    = PureF leaf -- consists of just an value of type leaf
    | FreeF (fun rec) -- or of a functor with inner type rec, usally being the recursive part, so FreeF itself again

-- new type wrapper for the fixpoint of FreeF
newtype Free fun leaf = Free { unFree :: Fix (FreeF fun leaf) }

-- FreeF being a pattern functor the functor implementation applies the function
-- to the "recursive" part
-- but the recursive part being a functor itself, the function must be mapped over it
instance Functor fun => Functor (FreeF fun leaf) where
    map _ (PureF x) = PureF x -- non recursive case is not effected
    map f (FreeF v) = FreeF (map f v) -- map over functor inside recursive case

-- this is the proper functor implementation that applies the mapping to the non-recursive part
-- it uses the cata recursion scheme to fold/reduce the recursive FreeF structun into a new FreeF structure
instance Functor fun => Functor (Free fun) where
    map f (Free l) = Free $ cata alg l
        where
            -- if a PureF leaf is visted, apply f to the leaf value inside
            alg (PureF leaf) = Fix (PureF (f leaf))
            -- if a FreeF inner node is seen, it can be assumed, that the mapping has already been applied
            -- inside the contained functor by cata, because it works "from the leafs to the root"
            -- in the way that being a recursion scheme we do not have to handle the recursive case manually
            alg (FreeF fun) = Fix (FreeF fun)

-- The Free monad is a Pointed functor since it can be constructed from any given value simply by wrapping it into the PureF
-- case
instance Functor f => Pointed (Free f) where
    inject x = Free $ Fix $ PureF x

-- Free is also an applicative the same way that a List is an applicative
instance Functor f => Applicative (Free f) where
    apply (Free ff) x = cata alg ff
      where
        -- if a single function is contained in the leaf of the left Free we can map it over right right value
        alg (PureF f)  = map f x
        -- we have a functor containing functions in the left applicative
        -- we can assumed that they have already been applied to the right values
        -- by the catamorphism. We just need to unwrap their results
        alg (FreeF fun) = Free (Fix (FreeF (map unFree fun)))


-- monadic bind can not be implemented as catamorpihsm
instance Functor fun => Monad (Free fun) where
    bind :: Functor fun => Free fun a -> (a -> Free fun b) -> Free fun b
    bind (Free t) f = Free (go t)
      where
        -- if the monad contains just a plain leaf value we can extract it, and apply the function, then unwrap one layer of Free
        go (Fix (PureF leaf))  = unFree $ f leaf
        -- if the monad contains a functor we can recursively map the function over the functor
        go (Fix (FreeF fr)) = Fix (FreeF (map go fr))

-- liftF turns a plain functor into a free Monat
liftF :: Functor f => f a -> Free f a
liftF fa = Free $ Fix (FreeF (map (Fix . PureF) fa))


-- a strong profunctor is a profunctor that can be made 
-- to work only on the first or the second element of a tuple
class Profunctor pro => Strong pro where
  first'  :: pro a b -> pro (a, c) (b, c)
  second' :: pro a b -> pro (c, a) (c, b)

-- a choice profunctor is one that can be made to work only on the
-- left or the right case of an Either
class Profunctor pro => Choice pro where
  left'  :: pro a b -> pro (Either a c) (Either b c)
  right' :: pro a b -> pro (Either c a) (Either c b)

-- a Traversing profunctor is one that is both strong and choice and
-- prives a wander function, that moves an applicative context from the
-- contravariant position of the profunctor to the covariant position
class (Strong pro, Choice pro) => Traversing pro where
  wander :: (forall app. Applicative app => (a -> app b) -> s -> app t) -> pro a b -> pro s t

-- an Iso is a partially applied dimap that works for any kind of profunctor
type Iso source target readFocus writeFocus = forall p. Profunctor p => p readFocus writeFocus -> p source target
iso :: (source -> readFocus) -> (writeFocus -> target) -> Iso source target readFocus writeFocus

-- iso takes two functions and turns them into a mapping between any two profunctors
iso = dimap

-- an Lens is a partially applied dimap that works for any Strong profunctor
type Lens source target readFocus writeFocus = forall p. Strong p => p readFocus writeFocus -> p source target

-- lens takes a getter and a setter function and turns them into a mapping between any strong profuctors
lens :: (source -> readFocus) -> (source -> writeFocus -> target) -> Lens source target readFocus writeFocus
lens getter setter = dimap (\s -> (getter s, s)) (uncurry setter . swap) . first'
    where 
        swap (x, y) = (y, x)
        uncurry f (x,y) = f x y 

-- prism is a mapping between any choice profunctor
type Prism source target readFocus writeFocus = forall p. Choice p => p readFocus writeFocus -> p source target

-- iso takes two functions and turns them into a mapping between any two profunctors
-- the one function is a getter the either succeeds or fails
-- the other is a setter that assumes/"forces" the success case of the getter 
prism ::  (source -> Either target readFocus) -> (writeFocus -> target) -> Prism source target readFocus writeFocus
prism tryGetter setter = dimap tryGetter (applyEither ident setter) . right'
    where
        applyEither :: (a -> c) -> (b -> c) -> Either a b -> c
        applyEither f _ (Left l) = f l
        applyEither _ f (Right r) = f r
        
        ident :: p -> p
        ident x = x

-- prism is a mapping between any traversing profunctor
type Traversal source target readFocus writeFocus = forall p. Traversing p => p readFocus writeFocus -> p source target

-- traversal takes a function that lifts a function from readFocus to applicative of writeFocus to a function 
-- from source to applicative of target
-- and turns this into a traversable profunctor
traversal :: (forall app. Applicative app => (readFocus -> app writeFocus) -> source -> app target)
          -> Traversal source target readFocus writeFocus
traversal = wander

-- A Setter is also a mapping between any two profunctors
-- with the law that:
-- over l id == id
-- and
-- over l (f . g) == over l f . over l g
type Setter source target readFocus writeFocus = forall p. Profunctor p => p source target -> p readFocus writeFocus

-- the default implementation of profunctor for a simple function is to
-- precompose the contravariant mapping and postcompose the covariant mapping
instance Profunctor (->) where
  dimap f h g = h . g . f

-- by default functions are also Strong profunctors because they can be lifted to work only on the
-- first or second value of a tuples
instance Strong (->) where
  first' f (a,b) = (f a,b)
  second' f (a,b) = (a, f b)


-- by default functions are also Choice profunctors because they can be lifted to be applied
-- to only one side of Either
instance Choice (->) where
  left' f (Left l) = Left $ f l
  left' _ (Right r) = Right r

  right' f (Right r) = Right $ f r
  right' _ (Left l) = Left l



-- Forget is a typwrapper to provide an alternative profunctor 
-- implementation for normal functions. 
-- Especially one hat ignores the contravariant mapping
newtype Forget r a b = Forget { runForget :: a -> r }

-- The Forget implemetation of Profunctor for a simple function is to 
-- just ignore the contravariant mapping
instance Profunctor (Forget r) where
  dimap f _ (Forget g) = Forget (g . f)

-- Forget is still a strong profunctor because it can be lefted to work on tuples
-- (but is not a Choice profunctor because it can not be lifted to work on Eithers)
instance Strong (Forget r) where
  first' (Forget f) = Forget (f . fst)
  second' (Forget f) = Forget (f . snd)


-- Star is a newtype to implement the Profunctor for 
-- functions whose result type is a functor
newtype Star fun a b = Star { runStar :: a -> fun b }

-- the profunctor implementation for dimap still precomposes the contravariant function 
-- and postcomposes the covariant function but since the return value of the 
-- actual function is already in the context of a funtor, the postcomposed covariant function
-- needs to be lifted into the functor  
instance Functor fun => Profunctor (Star fun) where
  dimap f g (Star h) = Star $ map g . h . f


-- a Star is also a Strong function since it can be lifted to work on tuples
instance Functor f => Strong (Star f) where
  first' (Star f) = Star (\(a, c) -> map (,c) (f a))
  second' :: Functor f => Star f a b -> Star f (c, a) (c, b)
  second' (Star f) = Star (\(a, c) -> map (a,) (f c))


-- A CoStar is the dual of a start, a function from a functor to a value
-- for wich Contravariant is implemented.
-- In contrast to Star, for CoStar the contravariant function needs to be lifted into the functor
-- and the covariant function can simply be postcomposed
newtype CoStar fun a b = CoStar { runCostar :: fun a -> b }
instance Functor fun => Profunctor (CoStar fun) where
  dimap f g (CoStar h) = CoStar $ g . h . map f


-- A CoStar is only Strong of the base functor in question is a Comonad
-- why? Because otherwise the tuples can not be extracted
instance Comonad f => Strong (CoStar f) where
  first' (CoStar h) = CoStar $ \fac ->
    let fproj = map fst fac   -- f a
        c     = snd (extract fac)
    in (h fproj, c)
  second' (CoStar h) = CoStar $ \fac ->
    let fproj = map snd fac   -- f a
        c     = fst (extract fac)
    in (c, h fproj)

-- view takes a lense (ie. any mapping from any Profunctor of one type to that of another type)
-- and applies it to the Forget Profunctor.
-- The Forget Profunctor skips the contravariant mapping ENTIRELY
-- resulting in an "early return" of the result value at the "end of the covariant chain"
-- so to speak
view :: Lens source target readFocus writeFocus -> source -> readFocus
view l = runForget (l (Forget ident))
    where ident x = x

-- over takes a lense (ie. any mapping from any Profunctor of one type to that of another type)
-- and a function mapping the lenses readFocus into its writeFocus
-- it wraps the given transformation function into the Identity Functor and then into the Start
-- Profunctor to chose the correct Profunctor instance and then applies the lense
-- to transform this profunctor yielding a function from source to target type
over :: Lens source target readFocus writeFocus -> (readFocus -> writeFocus) -> source -> target
-- over lns f s = getIdentity (runStar (lns (Star (Identity . f))) s)
-- alternativ
-- over f = f
-- over f x = f x
over = ($)

-- set is a specialization of over that takes a constant value to put into the writeFocus
-- of the lense, instead of a transformation function that maps the readFocus to the writeFocus 
set :: Lens source target readFocus writeFocus -> writeFocus -> source -> target
set l b = over l (const b)
    where const x _y = x



-- a Natural transformation is a mapping from one kind of functor to another kind of functor
-- that works nomatter the type inside the functor
type Natural f g = forall a. f a -> g a

-- an example for a natural Transformation is the conversion from maybe to a list of
-- either none or one element
-- note that the reverse is not possible becaus not all lists can be converted into
-- a Maybe of the same inner type (without discarding values of the list)
maybeToList :: Natural Maybe List
maybeToList None = List $ Fix NilF
maybeToList (Some x) = List $ Fix $ ConsF x $ Fix NilF

-- another example of a natural transforamtion is the 
-- conversion from either to maybe
-- note that the reverse is not possible because from a Maybe.None
-- we can no construct a Either.Left
eitherToMaybe :: Natural (Either a) Maybe
eitherToMaybe (Right x) = Some x
eitherToMaybe _ = None

leftSide :: Lens (a,b) (c,b) a c
leftSide = lens fst (\(_, b) c -> (c, b))

-- Entry point
main :: IO ()
main = do
    let (x,y) = over leftSide (+ 1) (40 :: Integer, 50 :: Integer)
    print x
    print y
