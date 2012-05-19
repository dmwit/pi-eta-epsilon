-- {-# OPTIONS_GHC -fglasgow-exts #-} -- 6.12.3
{-# OPTIONS_GHC -XGADTs -XTypeOperators #-} -- 7.0.1

-- Copyright (c) 2011, William J. Bowman, Roshan P. James, and Amr
-- Sabry. The code is released under the MIT license.
--
-- This code tested with GHC version 6.12.3, and version 7.0.1

module Pi where

-----------------------------------------------------------------------
-- Isomorphisms 

data Zero 

data a :<=> b where 
-- Congruence
  Id    :: a :<=> a
  Sym   :: (a :<=> b) -> (b :<=> a) 
  (:.:) :: (a :<=> b) -> (b :<=> c) -> (a :<=> c)
  (:*:) :: (a :<=> b) -> (c :<=> d) -> ((a,c) :<=> (b,d))
  (:+:) :: (a :<=> b) -> (c :<=> d) -> (Either a c :<=> Either b d)
-- (+) is associative, commutative, and has a unit
  PlusZeroL   ::         Either Zero a :<=> a
  PlusZeroR   :: a :<=>  Either Zero a

  CommutePlus :: Either a b :<=> Either b a

  AssocPlusL  :: Either      a        (Either b c)     :<=> Either (Either a b)      c 
  AssocPlusR  :: Either (Either a b)        c          :<=> Either      a       (Either b c) 
-- (*) is associative, commutative, and has a unit
  TimesOneL    :: ((), a) :<=> a
  TimesOneR    ::              a :<=> ((), a)
  CommuteTimes :: (a,b) :<=> (b,a) 
  AssocTimesL  :: (  a   , (b,c)) :<=> ((a,b) ,   c  )
  AssocTimesR  :: ((a,b) ,   c  ) :<=> (  a   , (b,c))
-- (*) distributes over (+) 
  TimesZeroL  :: (Zero, a) :<=> Zero
  TimesZeroR  ::                Zero :<=> (Zero, a)

  Distribute  ::                           (Either b c, a) :<=> Either (b, a) (c, a)
  Factor      :: Either (b, a) (c, a) :<=> (Either b c, a)

-- Encoding of booleans
  FoldB   :: Either () () :<=> Bool
  UnfoldB ::                   Bool :<=> Either () ()
-- Encoding of natural numbers using the isorecursive type (mu x.1+x)
  FoldN   :: Either () Int :<=> Int
  UnfoldN ::                    Int :<=> Either () Int
-- Encoding of lists of natural numbers using the isorecursive type
-- (mu x.1+nat*x)
  FoldLN :: Either () (Int, [Int]) :<=> [Int]
  UnfoldLN ::                           [Int] :<=> Either () (Int, [Int])
-- Encoding of lists of natural numbers using the isorecursive type
-- (mu x.1+nat*x)
  FoldL :: Either () (a, [a]) :<=> [a]
  UnfoldL ::                       [a] :<=> Either () (a, [a])
-- Trace operators for looping/recursion
  TracePlus :: (Either a b1 :<=> Either a b2) -> (b1 :<=> b2)

-- Adjoint

adjoint :: (a :<=> b) -> (b :<=> a)
adjoint Id = Id
adjoint (Sym f) = f 
adjoint (f :.: g) = adjoint g :.: adjoint f
adjoint (f :*: g) = adjoint f :*: adjoint g
adjoint (f :+: g) = adjoint f :+: adjoint g
adjoint PlusZeroL = PlusZeroR
adjoint PlusZeroR = PlusZeroL
adjoint AssocPlusL = AssocPlusR
adjoint AssocPlusR = AssocPlusL
adjoint TimesOneL = TimesOneR
adjoint TimesOneR = TimesOneL
adjoint CommutePlus = CommutePlus
adjoint CommuteTimes = CommuteTimes
adjoint AssocTimesL = AssocTimesR
adjoint AssocTimesR = AssocTimesL
adjoint TimesZeroL = TimesZeroR
adjoint TimesZeroR = TimesZeroL
adjoint Distribute = Factor
adjoint Factor = Distribute
adjoint FoldB = UnfoldB
adjoint UnfoldB = FoldB
adjoint FoldN = UnfoldN
adjoint UnfoldN = FoldN
adjoint FoldLN = UnfoldLN
adjoint UnfoldLN = FoldLN
adjoint FoldL = UnfoldL
adjoint UnfoldL = FoldL
adjoint (TracePlus c) = TracePlus (adjoint c)

-- Semantics
(@@) :: (a :<=> b) -> a -> b
Id             @@ a                     = a
(Sym f)        @@ b                     = (adjoint f) @@ b
(f :.: g)      @@ a                     = g @@ (f @@ a)
(f :*: g)      @@ (a,b)                 = (f @@ a, g @@ b) 
(f :+: g)      @@ (Left a)              = Left (f @@ a) 
(f :+: g)      @@ (Right b)             = Right (g @@ b) 

PlusZeroL      @@ (Right a)             = a
PlusZeroR      @@ a                     = Right a
CommutePlus    @@ (Left a)              = Right a
CommutePlus    @@ (Right b)             = Left b 
AssocPlusL     @@ (Left a)              = Left (Left a) 
AssocPlusL     @@ (Right (Left b))      = Left (Right b) 
AssocPlusL     @@ (Right (Right c))     = Right c
AssocPlusR     @@ (Left (Left a))       = Left a
AssocPlusR     @@ (Left (Right b))      = Right (Left b)
AssocPlusR     @@ (Right c)             = Right (Right c)
TimesOneL      @@ ((), a)               = a
TimesOneR      @@ a                     = ( ( )  ,   a  )
CommuteTimes   @@ (a,b)                 = (  b   ,   a  ) 
AssocTimesL    @@ (a,(b,c))             = ((a,b) ,   c  ) 
AssocTimesR    @@ ((a,b),c)             = (  a   , (b,c))
Distribute     @@ (Left b, a)           = Left (b, a) 
Distribute     @@ (Right c, a)          = Right (c, a) 
Factor         @@ (Left (b, a))         = (Left b, a) 
Factor         @@ (Right (c, a))        = (Right c, a) 
FoldB          @@ (Left ())             = True
FoldB          @@ (Right ())            = False
UnfoldB        @@ True                  = Left ()
UnfoldB        @@ False                 = Right ()
FoldN          @@ (Left ())             = 0
FoldN          @@ (Right n)             = 1 + n
UnfoldN        @@ 0                     = Left ()
UnfoldN        @@ n                     = Right (n-1) 
FoldLN         @@ Left ()               = []
FoldLN         @@ Right (h, t)          = h : t
UnfoldLN       @@ []                    = Left ()
UnfoldLN       @@ (h : t)               = Right (h, t)
FoldL          @@ Left ()               = []
FoldL          @@ Right (h, t)          = h : t
UnfoldL        @@ []                    = Left ()
UnfoldL        @@ (h : t)               = Right (h, t)
(TracePlus c)  @@ v                     = loop c (c @@ (Right v))
    where
                                          loop c (Left v)   = loop c (c @@ (Left v))
                                          loop c (Right v)  = v

versor :: a :<=> b -> b :<=> b -> a :<=> a
versor f g = f :.: g :.: (adjoint f)

------------------------------------------------------------------------
-- Combinational circuits 

-- Not works by first converting the Haskell boolean via a Unfold to
-- either Left () or Right (), where True is Left (), False is Right ().
-- not can simply use CommutePlus to perform a not, then Fold up the
-- bool.
-- REPL Session:

-- |
-- >>> inot @@ True
-- False

-- |
-- >>> inot @@ False
-- True
inot :: Bool :<=> Bool
inot = UnfoldB :.: CommutePlus :.: FoldB

-- Cond takes two isomorphisms from some type a to some type b, and 
-- creates an isomorphism between a pair of (Bool, a) which will apply
-- the first isomorphism if the Bool is True, and the second if the Bool
-- is False.
cond :: (a :<=> b) -> (a :<=> b) -> ((Bool, a) :<=> (Bool, b))
cond f g = (UnfoldB :*: Id) 
           :.: Distribute 
           :.: ((Id :*: f) :+: (Id :*: g))
           :.: Factor 
           :.: (FoldB :*: Id) 

-- controlled takes an isomorphism between a type a to type a, and 
-- creates an isomorphism (using cond) that will apply the isomorphism to
-- the second value of the pair, if the first value if True, and apply
-- Id otherwise.
controlled :: (a :<=> a) -> ((Bool, a) :<=> (Bool, a))
controlled f = cond f Id

-- cnot is Controlled Not, as found in reversible computing papers such
-- as Reversible Computing by Toffoli. It takes a pair of bools, and
-- applies not to the second Bool if the first is True, and otherwise
-- leaves the value unchanged.

-- |
-- >>> cnot @@ (True, True)
-- (True,False)

-- |
-- >>> cnot @@ (False, True)
-- (False,True)

-- |
-- >>> cnot @@ (True, False)
-- (True,True)
cnot :: (Bool, Bool) :<=> (Bool, Bool)
cnot = controlled inot

-- Toffoli is the universal nand/and gate presented in Reversible
-- Computing by Toffoli.  It is equivalent to controlled controlled
-- not. It takes 3 bools, if the first is True, if applies controlled not
-- to the second 2 bools.
-- REPL Session:

-- |
-- >>> toffoli @@ ((True, True), False)
-- ((True,True),True)


-- |
-- >>> toffoli @@ ((True, False), False)
-- ((True,False),False)

-- |
-- >>> toffoli @@ ((True, True), True)
-- ((True,True),False)

-- |
-- >>> toffoli @@ ((False, True), False)
-- ((False,True),False)

toffoli :: ((Bool,Bool),Bool) :<=> ((Bool,Bool),Bool)
toffoli = AssocTimesR :.: controlled cnot :.: AssocTimesL

-- The Fredkin gate is a well known universal gate.
-- If the first bool is true, it swaps the second two, otherwise it
-- leaves the values unchanged.
fredkin :: (Bool,(Bool,Bool)) :<=> (Bool,(Bool,Bool))
fredkin = controlled CommuteTimes

-- The Peres gate is a universal gate: it takes three inputs a, b, and c, 
-- and produces a, a xor b, (a and b) xor c
peres :: ((Bool,Bool),Bool) :<=> ((Bool,Bool),Bool)
peres = toffoli :.: (cnot :*: Id) 

-- fullAdder can be interpreted as an irreversible 2 bit adder with
-- carry, by fixing the first input to be False and interpreting the
-- inputs and outputs as follows:
--
-- Input: (Constant, ((Number1, Number2), CarryIn)))
-- Output (Garbage1, (Garbage2, (Sum, Carry_Out)))
--
-- All values should be booleans, where False is 0 and True is 1.
-- Constant must be initialized to 0.
fullAdder :: (Bool, ((Bool, Bool), Bool)) :<=> (Bool,(Bool,(Bool,Bool)))
fullAdder = CommuteTimes :.: (CommuteTimes :*: Id) :.: 
            AssocTimesR :.: CommuteTimes :.: (peres :*: Id) :.:
            AssocTimesR :.: (Id :*: CommuteTimes) :.: 
            AssocTimesR :.: (Id :*: AssocTimesL) :.:
            (Id :*: peres) :.: (Id :*: AssocTimesR)

--------------------------------------------------------------
-- Some handy swaps etc.

sw :: (a, (b, c)) :<=> (b, (a, c))
sw = AssocTimesL :.: (CommuteTimes :*: Id) :.: AssocTimesR

sw2 :: ((a, b), (c, d)) :<=> ((a, c), (b, d))
sw2 = AssocTimesR :.: (Id :*: sw) :.: AssocTimesL

-- We can introduce unit freely. hide_unit makes something 
-- that needs a unit temporary value, and creates a new isomorphism that 
-- performs the same function, but automatically introduces and removes
-- a unit temp value.
hide_unit :: (c, ()) :<=> (c, ()) -> c :<=> c
hide_unit c = TimesOneR :.: CommuteTimes :.: c :.: CommuteTimes :.: TimesOneL

------------------------------------------------------------------------
-- Simple primitives on inductive types

-- addSub1 can be thought of as the function add1 mod (the sum of both
-- inputs), by ignoring the second input and output. 
-- 
-- By ignoring the first input and first output, the function can be
-- thought of as sub1 (mod the sum of both inputs)
-- 
-- REPL Session:

-- |
-- >>> addSub1 @@ (10, 1)
-- (11,0)

-- |
-- >>> addSub1 @@ (1, 10)
-- (2,9)

-- |
-- >>> addSub1 @@ (10, 0)
-- (0,10)
addSub1 :: (Int, Int) :<=> (Int, Int)
addSub1 = CommuteTimes 
            :.: (UnfoldN :*: Id) 
            :.: Distribute 
            :.: (Id :+: CommuteTimes)
            :.: Factor 
            :.: (FoldN :*: Id) 

--------------------------------------------------------------------------
-- Counter

-- 
--------------------------------------------------------------------------
counter :: (Int, Int) :<=> (Int, Int)
counter = TracePlus counterLoop

--counterLoop = Factor :.: (FoldN :*: Id) :.: CommuteTimes :.: (UnfoldN :*: Id) :.: Distribute :.: 
--                (Id :+: CommuteTimes)

firstS  x = (x :+: Id)
secondS x = (Id :+: x)

first x = (x :*: Id)

counterLoop = (versor Factor $ versor (first FoldN) CommuteTimes) :.: secondS CommuteTimes

------------------------------------------------------------------------------------
--Adder
------------------------------------------------------------------------------------

type StartState  = (Int, (Int, Int))
type StuckState  = (Int, (Int, Int))
type AdderInput  = (Either StartState StuckState)
type AdderOutput = (Int, (Int, Int)) 

adder :: AdderInput :<=> AdderOutput
adder = TracePlus adderLoop

type LoopInput = (Int, (Int, Int))

--I need to move the eithers around. 
adderLoop :: Either LoopInput AdderInput :<=> Either LoopInput AdderOutput
--adderLoop =  AssocPlusL :.: firstS (Factor :.: (first FoldN) :.: undefined) :.: undefined
adderLoop = undefined
--firstS (Factor :.: undefined) :.: undefined -- (firstS ) :.: undefined

tester = AssocPlusL :.: firstS (Factor :.: (first FoldN) :.: sw n )
-- :.: Factor :.: first FoldN 
--                :.: undefined

--------------------------------------------------------------------------
-- Iterating a list of nats.

-- iter_ls_nat takes an isomorphism which represents a single step of a
-- loop over a list, and creates an isomorphism which loops over a list,
-- threading through any other arbitrary values.
--
-- To do this, we introduce a unit, which is used to build up a new
-- list, as we traverse the input list.  This is necessary, as information
-- preservation is needed to maintain reversibility.
-- After introducing the unit, we trace a body that does some
-- rearranging and deconstruction of the list.
-- 
-- Step should be an isomorphism that takes a pair, whose first element
-- is a pair of the head and tail of the list at a given step, and whose
-- second element is a pair of the list that is being built to preserve
-- information, and the threaded values of its choosing.
--
-- The resulting isomorphism will take a list and some threaded values,
-- and iterate over the list, performing step each time a tail operation
-- is performed (i.e. the list is 'decremented')
iter_ls_nat :: ((Int, [Int]), ([Int], a)) :<=> ((Int, [Int]), ([Int], a)) -> 
               ([Int], a) :<=> ([Int], a)
iter_ls_nat step = TimesOneR 
                   :.: (TracePlus body)
                   :.: TimesOneL
    where
      -- body :: Either ((Int, [Int]), ([Int], a)) ((), ([Int], a)) :<=> 
      --         Either ((Int, [Int]), ([Int], a)) ((), ([Int], a))
      body = Factor 
             :.: ((CommutePlus :.: FoldLN) :*: Id)
             :.: sw
             :.: ((UnfoldLN :.: CommutePlus) :*: Id)
             :.: Distribute
             :.: ((step :.: sw2) :+: Id)

-- Isomorphisms over lists

-- To reverse a list, we introduce a unit value, as iterating requires
-- (or allows, depending on perspective) that we give some values to
-- thread through the loop, and performs Id at each step.  The resulting
-- list is inherently reversed, due to how the list has to be built up
-- as we iterate it.
--
-- REPL Session:

-- |
-- >>> ireverse @@ [1..5]
-- [5,4,3,2,1]

-- |
-- >>> ireverse @@ [5,4..1]
-- [1,2,3,4,5]
ireverse :: [Int] :<=> [Int]
ireverse = hide_unit (iter_ls_nat Id)

-- shuffle performs a shuffle on the list; it reverses the tail of the
-- list at each step of iteration, before recurring on it.

-- |
-- >>> shuffle @@ [1..5]
-- [1,5,2,4,3]
shuffle :: [Int] :<=> [Int]
shuffle = hide_unit (iter_ls_nat rev') :.: ireverse
    where rev' = (Id :*: ireverse) :*: Id
    
------------------------------------------------------------------------
-- Iterating on a nat.
-- 
-- Given an isomorphism between a type a, generates a isomorphism 
-- between a pair of an Int and type a, which will apply the given
-- isomorphism at each step, as it iterates over the int.  At each step,
-- the given isomorphism has access to only the values of a, which are
-- threaded through the loop.
iter_nat ::  (a :<=> a) -> 
             (Int, a) :<=> (Int, a)
iter_nat step = TimesOneR 
                :.: (TracePlus body)
                :.: TimesOneL
    where
      -- body :: Either (Int, (Int, a)) ((), (Int, a))  :<=> 
      --         Either (Int, (Int, a)) ((), (Int, a))
      body = Factor 
             :.: ((CommutePlus :.: FoldN) :*: Id)
             :.: sw
             :.: ((UnfoldN :.: CommutePlus) :*: Id)
             :.: Distribute
             :.: (((Id :*: (Id :*: step)) :.: sw) :+: Id)

iter_nat_i ::  ((Int, a) :<=> (Int, a)) -> 
             (Int, a) :<=> (Int, a)
iter_nat_i step = TimesOneR 
                :.: (TracePlus body)
                :.: TimesOneL
    where
      -- body :: Either (Int, (Int, a)) ((), (Int, a))  :<=> 
      --         Either (Int, (Int, a)) ((), (Int, a))
      body = Factor 
             :.: ((CommutePlus :.: FoldN) :*: Id)
             :.: sw
             :.: ((UnfoldN :.: CommutePlus) :*: Id)
             :.: Distribute
             :.: (((Id :*: (step)) :.: sw) :+: Id)
-- evenOdd can be thought of as the irreversible function even, by
-- fixing the second input to True, and ignoring the first output. It
-- can also represent the irreversible function odd by fixing the second
-- output to False, and again ignoring the first output.
--

-- REPL Session:

-- |
-- >>> evenOdd @@ (0, True)
-- (0,True)

-- |
-- >>> evenOdd @@ (1, True)
-- (1,False)

-- |
-- >>> evenOdd @@ (0, False)
-- (0,False)

-- |
-- >>> evenOdd @@ (1, False)
-- (1,True)

-- |
-- >>> evenOdd @@ (4, False)
-- (4,False)

-- |
-- >>> evenOdd @@ (5, False)
-- (5,True)

-- |
-- >>> evenOdd @@ (4, True)
-- (4,True)
evenOdd :: (Int, Bool) :<=> (Int, Bool)
evenOdd = iter_nat inot

-- addSubN can be thought of the irreversible function add by providing
-- h_large for the second input, a first number as the first input, and
-- the second number in the third input, and ignoring the last 2 outputs
--
-- addSubN can be thought of as the irreversible function subtract by
-- providing h_large in the first input, and treating the last two
-- inputs as the inputs to subtract.  By ignoring the first and third
-- outputs, you have the result.
--
-- Note that both addition and subtraction is performed mod the sum of
-- both the first 2 inputs plus 1.  So, if you try to perform 
-- subtraction with h_large = 10, and arguments 0 and 1, the result 
-- will wrap around to 10.
--
-- Sample REPL session:

-- |
-- >>> addSubN @@ ((10, 10), 7)
-- ((17,3),7)

-- |
-- >>> addSubN @@ ((10, 1000000), 7)
-- ((17,999993),7)

-- |
-- >>> addSubN @@ ((10, 1000000), 200)
-- ((210,999800),200)

-- |
-- >>> addSubN @@ ((10, 0), 0)
-- ((10,0),0)

-- |
-- >>> addSubN @@ ((10, 0), 1)
-- ((0,10),1)

-- |
-- >>> addSubN @@ ((10, 0), 2)
-- ((1,9),2)
addSubN :: ((Int, Int), Int) :<=> ((Int, Int), Int)
addSubN = CommuteTimes :.: (iter_nat addSub1) :.: CommuteTimes

-- Mult can be thought of as the irreversible function multiply by
-- fixing the first two arguments to 0 and h_large, respectively, and
-- using the last two inputs as the arguments to multiply.
-- (again, mod the sum of the first two arguments + 1)
--
-- The result is obtained by ignoring the last 3 inputs:
-- 
--
-- mult(((accumulator, heap), n1), n2)
-- Sample REPL Session:

-- |
-- >>> mult @@ (((0,10000), 2), 3)
-- (((6,9994),2),3)

-- |
-- >>> mult @@ (((0,10000), 7), 11)
-- (((77,9923),7),11)

-- |
-- >>> mult @@ (((0,10000), 7), 0)
-- (((0,10000),7),0)

-- |
-- >>> mult @@ (((0,10000), 0), 11)
-- (((0,10000),0),11)
mult :: (((Int, Int), Int), Int) :<=> (((Int, Int), Int), Int)
mult = CommuteTimes :.: (iter_nat addSubN) :.: CommuteTimes

-- Factorial. Shuffle the accumulator and the 2nd input around.  Assumes the same
-- input as mult.  Used for fact.
fshuf :: (((Int, Int), Int), Int) :<=> (((Int, Int), Int), Int)
fshuf = AssocTimesR 
        :.: AssocTimesR 
        :.: (Id :*: (AssocTimesL :.: CommuteTimes)) 
        :.: AssocTimesL 
        :.: CommuteTimes 
        :.: (CommuteTimes :*: Id) 
        :.: AssocTimesL

-- (((((Acc, Heap), ?), Input), []), [0, 0..]) ->
-- (((((0, Heap), Acc), Input), [?]), [0..])
-- Collect garbage does a massive amount of shuffling, and pushes some
-- garbage onto a garbage list, and pulls a fresh 0 off the 0 list for
-- the new accumulator.
-- It was written in a very systematic way, and as a result is much more
-- verbose than necessary, and rather inefficient.
collect_garbage :: (((((Int, Int), Int), Int), [Int]), [Int]) :<=> 
                   (((((Int, Int), Int), Int), [Int]), [Int])
collect_garbage = (AssocTimesR :*: Id)
                  :.: ((Id :*: CommuteTimes) :*: Id)
                  :.: (AssocTimesR :*: Id)
                  :.: ((Id :*: AssocTimesL) :*: Id)
                  :.: (Id :*: UnfoldLN)
                  :.: CommuteTimes :.: Distribute
                  :.: (CommuteTimes :+: CommuteTimes)
                  :.: (AssocTimesR :+: Id)
                  :.: ((Id :*: (AssocTimesR :*: Id)) :+: (AssocTimesR))
                  :.: ((Id :*: ((Id :*: CommuteTimes) :*: Id)) :+:
                          (Id :*: AssocTimesL))
                  :.: ((Id :*: (AssocTimesL :*: Id)) :+: 
                          (Id :*: ((CommuteTimes :*: Id) :*: Id)))
                  :.: (Id :+: (Id :*: (CommuteTimes :*: Id)))
                  :.: (Id :+: (Id :*: (AssocTimesL :*: Id)))
                  :.: (Id :+: (Id :*: AssocTimesR))
                  :.: (Id :+: (Id :*: (Id :*: CommuteTimes)))
                  :.: (Id :+: (Id :*: AssocTimesL))
                  :.: (AssocTimesL :+: AssocTimesL)
                  :.: (CommuteTimes :+: CommuteTimes)
                  :.: Factor
                  :.: CommuteTimes
                  :.: (AssocTimesR)
                  :.: (Id :*: (Id :*: FoldLN))
                  :.: AssocTimesL
                  :.: (AssocTimesL :*: Id)
                  :.: ((AssocTimesL :*: Id) :*: Id)
                  :.: (AssocTimesR)
                  :.: (Id :*: CommuteTimes)
                  :.: AssocTimesL
                  :.: ((fshuf :*: Id) :*: Id)
-- Reversible fact!
-- Sample Input: ((([], [0,0,0,0,0,0,0]), (((0, 10000), 1), 5)), 5)
-- Sample Output: ((([120,60,20,5,1],[0,0,0,0,0,0]),(((0,9680),120),0)),5)
--
-- Requires a garbage list to store junk in, a (sufficiently large) list
-- of zeros, a (sufficiently large) heap number, and 1.  Requires the
-- input twice.  This restriction could be lifted by initializing one of
-- them to 0 and counting up during the recursion instead of down, but
-- this way it much easier.
--
-- It should also be possible to eliminate one input by using a
-- different sort of iteration over nats, that gives us access to
-- intermediate values.
-- There are many optimizations possible in this code.
fact :: ((([Int], [Int]), (((Int, Int), Int), Int)), Int) :<=>
        ((([Int], [Int]), (((Int, Int), Int), Int)), Int)
fact =  CommuteTimes :.: iter_nat ((Id :*: mult)
                :.: (CommuteTimes :.: AssocTimesL :.: collect_garbage)
                :.: AssocTimesR
                :.: CommuteTimes -- Garbage collected, and accumulator 
                                 -- reinitialized
                :.: (Id :*: (AssocTimesR -- Sub1 from the recursive step
                :.: (Id :*: addSub1)
                :.: AssocTimesL 
                :.: (AssocTimesR :*: Id)
                :.: ((Id :*: addSub1) :*: Id)
                :.: (AssocTimesL :*: Id)
                ))) :.: CommuteTimes

------------------------------------------------------------------------
-- Infinite loops

-- Isomorphic inc function
-- (n, True) --> (n+1, True)
-- (n, False) --> (n-1, False)
-- (0, False) --> (0, True)
-- 
-- the adjoint of this function is also an isomorphism. 
iso_inc :: (Int, Bool) :<=> (Int, Bool)
iso_inc = CommuteTimes 
          :.: (UnfoldB :*: Id)
          :.: Distribute
          :.: (TimesOneL :+: TimesOneL)
          :.: (Id :+: UnfoldN)
          :.: AssocPlusL
          :.: ((CommutePlus :.: FoldN) :+: Id)
          :.: (TimesOneR :+: TimesOneR)
          :.: Factor
          :.: (FoldB :*: Id)
          :.: CommuteTimes

-- inc function
-- n --> n+1
-- 
-- the adjoint of this function is undefined for 0. 
inc :: Int :<=> Int
inc = TracePlus body
    where
      body = ((UnfoldN :.: CommutePlus)  :+: Id)
             :.: AssocPlusR
             :.: (Id :+: FoldN)

dec :: Int :<=> Int
dec = adjoint inc

-- A total function that will turn a () into False
introFalse :: () :<=> Bool
introFalse = TracePlus body
           where 
             body = AssocPlusR
                    :.: (UnfoldN :.: CommutePlus :+: FoldB)

introTrue :: () :<=> Bool
introTrue = introFalse :.: inot

-- A partial function that will delete a False.  Only defined on input
-- False
deleteFalse :: Bool :<=> ()
deleteFalse = adjoint introFalse

-- A partial function that will delete True.
deleteTrue :: Bool :<=> ()
deleteTrue = adjoint introTrue

-- A total function which will introduce a 0
introZero :: () :<=> Int
introZero = TracePlus body
     where 
       -- Either Int () :<=>
       -- Either Int Int
       body = CommutePlus -- () + Int
              :.: FoldN -- Int
              :.: TimesOneR -- ((), Int)
              :.: (introFalse :*: Id) -- (Bool, Int)
              :.: (UnfoldB :*: Id) -- (() + (), Int)
              :.: Distribute -- ((), Int + (), Int)
              :.: (TimesOneL :+: TimesOneL)

-- A partial function which will delete a zero
deleteZero :: Int :<=> ()
deleteZero = adjoint introZero

-- Convenient ways to introduce zeros.
introZeroL :: a :<=> (Int, a)
introZeroL = TimesOneR :.: (introZero :*: Id)

deleteZeroL :: (Int, a) :<=> a
deleteZeroL = adjoint introZeroL

-- Some more interesting functions that do unexpected things
--
-- intToBool is a partial function, undefined for all n > 1.
-- It transforms 1 to False and 0 to True
intToBool :: Int :<=> Bool
intToBool = UnfoldN :.: (Id :+: deleteZero) :.: FoldB

-- A total function which converts True to 0 and False to 1
boolToInt :: Bool :<=> Int
boolToInt = adjoint intToBool

-- A partial function defined only on zero.  It's inverse is also
-- defined only on zero.
zero :: Int :<=> Int
zero = TimesOneR 
       :.: CommuteTimes 
       :.: iter_nat_i body 
       :.: CommuteTimes
       :.: TimesOneL
     where
       body = (Id :*: introFalse)
              :.: iso_inc
              :.: (Id :*: UnfoldB)
              :.: (Id :*: (introZero :+: Id)) 
              :.: (Id :*: CommutePlus)
              :.: (Id :*: (FoldN :.: deleteZero))

add :: (Int, Int) :<=> (Int, Int)
add = iter_nat inc

mult' = iter_nat add

-- Some list operations
cons :: (Int, [Int]) :<=> [Int]
cons = TracePlus body
     where
       body = AssocPlusR 
              :.: ((UnfoldN :.: CommutePlus) :+: FoldLN)

car :: [Int] :<=> (Int, [Int])
car = adjoint cons

nil :: () :<=> [Int]
nil = TracePlus body
    where
       body = CommutePlus -- () + (Int, [Int])
              :.: FoldLN -- [Int]
              :.: TimesOneR -- ((), [Int])
              :.: (introFalse :*: Id) -- (Bool, [Int])
              :.: (UnfoldB :*: Id) -- (() + (), [Int])
              :.: Distribute -- ((), [Int] + (), [Int])
              :.: ((introZero :*: Id) :+: TimesOneL)

-- Convenient way to introduce nil
introNilR :: a :<=> (a, [Int])
introNilR = TimesOneR :.: (nil :*: Id) :.: CommuteTimes

deleteNilR :: (a, [Int]) :<=> a
deleteNilR = adjoint introNilR

-- Duplicate an integer
duplicate :: Int :<=> (Int, Int)
duplicate = introZeroL :.: CommuteTimes :.: add

-- A much better implementation of fact' !
fact' :: Int :<=> (Int, [Int])
fact' = heap
        :.: arrangeIn
        :.: (iter_nat_i (arrangeOut :.: body :.: arrangeIn)) 
        :.: arrangeOut
        :.: garbage
      where
        -- Int :<=> ((Int, (Int, 0)), [Int])
        -- Introduces some extra terms to work with.
        heap = introNilR 
             :.: ((duplicate 
                 :.: introZeroL
                 :.: CommuteTimes 
                 :.: AssocTimesR) :*: Id) 
        -- ((Int, (Int, 0)), [Int]) :<=> (Int, (Int, (0, [Int])))
        -- Arranges the terms in the way iter_nat_i expects
        arrangeIn = (AssocTimesL :*: Id)
                    :.: AssocTimesR 
                    :.: AssocTimesR
        -- (Int, (Int, (Int, [Int]))) :<=> ((Int, (Int, 0)), [Int])
        -- Arranges the terms in a more friendly way to work with
        arrangeOut = adjoint arrangeIn
        -- ((int, (Int, Int)), [Int]) :<=> ((Int, (Int, Int)), [Int])
        -- The main loop, which performs incremental multiplication,
        -- and cons intermediate values onto the garbage list
        body = ((inc :*: Id) :*: Id) 
               :.: (mult' :*: Id)
               :.: ((Id :*: CommuteTimes) :*: Id)
               :.: (AssocTimesL :*: Id)
               :.: AssocTimesR
               :.: (Id :*: (cons :.: introZeroL)) 
               :.: AssocTimesL
               :.: (AssocTimesR :*: Id)
               :.: ((dec :*: Id) :*: Id)
        -- ((Int, (Int, 0)), [Int]) :<=> (Int, (Int, [Int]))
        -- Delete the leftover zero.
        deleteZero = ((Id :*: (CommuteTimes :.: deleteZeroL)) :*: Id)
                     :.: AssocTimesR
        -- (Int, (Int, [Int])) :<=> (Int, [Int])
        -- After deleting the zero, our answer isn't in the nicest
        -- place, and we still have 1 intermediate value left, and the
        -- original input. Let's put the answer in a nicer place, and
        -- put those unneeded values in the garbage list.
        garbage = deleteZero
                  :.: (Id :*: (Id :*: car)) -- (Int, (Int, (Int, [Int])))
                  :.: (Id :*: (Id :*: CommuteTimes))
                  :.: AssocTimesL
                  :.: sw2 -- ((Int, [Int]), (Int, Int))
                  :.: (cons :*: Id) -- ([Int], (Int, Int))
                        :.: AssocTimesL -- (([Int], Int), Int)
                        :.: ((CommuteTimes :.: cons) :*: Id)
                        :.: CommuteTimes

-- Some interesting divergent functions (partial bijections)
--
omega0 :: (Int, Bool) :<=> (Int, Bool)
omega0 = TimesOneR
         :.: (TracePlus body)
         :.: TimesOneL
    where
      -- body :: Either (Int, (Int, a)) ((), (Int, a))  :<=>
      --         Either (Int, (Int, a)) ((), (Int, a))
      body = Factor -- (Int + (), (Int, a)) 
             :.: ((CommutePlus :.: FoldN) :*: Id) -- (Int (0 or i+1), (Int, a))
             :.: sw -- (Int, (Int (0 or i+1), a))
             :.: ((UnfoldN :.: CommutePlus) :*: Id) -- (Int + (), (Int, a))
             :.: Distribute  -- (Int, (Int, a)) + ((), (Int, a))
             :.: ((sw :.: (Id :*: iso_inc)) :+: Id) 

-- A couple of functions based on how we compose the above. 
--
-- This is a partial function. It is the identity on the defined
-- inputs.  i.e. (c v) |-->* v if it terminates.
omega0_partial_id = omega0 :.: (adjoint omega0)

-- This is just the identity. 
omega0_id = (adjoint omega0) :.: omega0

-- Another infinite loop, but this time on a finite type.  This is
-- defined only on input False. Input True causes it to diverge. Since
-- this is on a finite type, we can ask our usual information
-- theoretic questions about this: Does this function preserve
-- information?
omega1 :: Bool :<=> Bool
omega1 = TimesOneR 
         :.: (TracePlus body)
         :.: TimesOneL
    where
      -- body :: Either (Int, a) ((), a)  :<=> 
      --         Either (Int, a) ((), a)
      body = Factor 
             :.: ((CommutePlus :.: FoldN) :*: Id)
             :.: iso_inc
             :.: ((UnfoldN :.: CommutePlus) :*: Id)
             :.: Distribute


-- Undefined on all inputs. Does this constitute some form of
-- "deleting a bit"? Entropy of bool is 1. 
omega1_bool :: Bool :<=> Bool
omega1_bool = omega1 :.: omega1

-- Another infinite loop.  Unit however is supposed to have no
-- information, so what does non-termination mean in this case?
-- Entropy of unit is 0.
omega1_unit :: () :<=> ()
omega1_unit = TracePlus (FoldB :.: omega1 :.: UnfoldB)

------------------------------------------------------------------------

{-
class CreateConst a where 
    createConst :: () :<=> a

instance CreateConst () where 
    createConst = Id 

instance (CreateConst a, CreateConst b) => CreateConst (a, b) where 
    createConst =  TimesOneR :.:  (createConst :*: createConst)

instance (CreateConst a, CreateConst b) => CreateConst (Either a b) where 
    createConst =  introTrue :.: UnfoldB :.:  (createConst :+: createConst)

instance CreateConst Int where 
    createConst =  introZero

instance CreateConst a => CreateConst [a] where 
    createConst = TracePlus body 
        where 
          body :: Either (a, [a]) () :<=> Either (a, [a]) [a]
          body = CommutePlus
                 :.: FoldL 
                 :.: withUnit (introFalse :.: UnfoldB)
                 :.: Distribute 
                 :.: ((createConst :*: Id) :+: TimesOneL)

withUnit :: () :<=> b -> a :<=> (b, a)
withUnit c = TimesOneR :.: (c :*: Id)

-}
