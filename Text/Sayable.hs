{- |
Module: Text.Sayable

This module provides a set of data structures, classes, and operators that facilitate the construction of a Prettyprinter Doc object.

= Motivation

Standard prettyprinting is a monotonic conversion that does not allow for
customization for different uses or environments.  For example, when debugging,
full and explicit information about a structure should be generated, but for
checkpoint logging, a simple overview is usually more appropriate.

This library provides for an additional type parameter that can be used to
control the conversion to a suitably verbose Prettyprinter Doc representation.

This is also highly useful in conjunction with logging to generate successively
more verbose information as the logging verbosity increases.

= Usage

Typical usage is to create a sayable message using the operators defined here and
then extract Prettyprinter Doc from the saying and convert it to a printable
format (here, simply using @show@ for the default Prettyprinter rendering).

@
import qualified Prettyprinter as PP

foo :: Members '[ Logging SayMessage, Config ] r -> a -> b -> Eff r [b]
foo arg1 arg2 =
   do putStrLn $ show $ saying $ sayable @info "Entering foo with" &- arg1 &- "and" &- arg2
      rslt <- something arg1 arg2
      case rslt of
        Right vals ->
          do putStrLn $ show $ saying $ sayable @"verbose"
                 $ "Foo successfully returning" &% length vals &- "results:" &- vals
             return vals
        Left err ->
          do putStrLn $ show $ saying $ sayable @"error"
                 $ "Foo error (" &- arg1 &- PP.comma &- arg2 &- ") is" &- err
             throwError err
@

[Note: if viewing via Haddock HTML, the '@' in front of @"info"@,
@"verbose"@, and @"error"@ on the putStrLn lines above may not be
visible.]

There are three messages printed: one on entry and one on either the success or
failure paths.  Each message may have different levels of information reported for the various arguments.

== The 'saytag' type parameter

Each sayable message uses a 'TypeApplication' to specify a "saytag" which should
be used for controlling the rendering of that message (e.g. "info", "verbose", "error", etc.).

As a developer, it is encouraged to use whatever saytag makes sense relative to the current context and type of information being processed.

== Individual Arguments

The arguments passed to the sayable should be instances of the 'Sayable' class.
There are a number of standard instances of 'Sayable', but an instance can be
declared for any object that might be output.  The 'Sayable' class has two class
parameters: the second is object to be converted, and the first is the "saytag".
This allows different Sayable instances for an object to be used in different
saytag scenarios.  For example:

@
import Network.URL

instance Sayable "verbose" URL where
  sayable url =
    let newline = PP.line :: PP.Doc SayableAnn
        prettyShow x = PP.viaShow x :: PP.Doc SayableAnn
    in "URL {"
        &- "url_type=" &- prettyShow (url_type url) &- newline
        &- "url_path=" &- url_path url &- newline
        &- "url_params=" &* url_params url
        &- "}"
instance Sayable saytag URL where
  sayable = Sayable . PP.viaShow . exportURL
@

The above would cause a url emitted via a "verbose" saytag to be
expanded into a report on each individual field, whereas all other
saytags would simply output the 'exportURL' representation of the 'URL'.

>>> let host = Host (HTTP True) "github.com" Nothing
>>> url' = URL (Absolute host) "by/one"
>>> saying $ sayable @"verbose" url'
URL { url_type= Absolute (Host {protocol = HTTP True, host= "github.com", port= Nothing})
 url_path= by/one
 url_params= }
>>> saying @"info" $ sayable url'
https://github.com:442/by/one

There are some tricky elements to the above however; see "Unfortunate
Details" below.

Note that there are several pre-declared Sayable instances for common
datatypes for convenience.

== Operators

In the logging lines above, there are three operators used, each of
which starts with the '&' character:

  ['&-'] This is the standard operator that takes two Sayable
         arguments and converts them to their Sayable form, then
         combining them (with an intervening space).  This is the
         standard argument to use for building the output message from
         distict parts.

  ['&+'] This is a variation of the standard '&-' operator that has no
         intervening space between the two arguments that are
         converted to a Sayable form.

  ['&%'] This is a variation of the standard '&-' operator that only
         requires the second argument to be an instances of
         Prettyprinter.Pretty instead of an instance of 'Sayable',
         which can be convenient and avoids the need to define large
         numbers of 'Sayable' instances.

  ['&*'] This is a helper operator whose second argument is a
         'Foldable' series of 'Sayable' elements.  This will fold over
         the series, adding the 'Sayable' instance value for each
         element separated by commas.

  ['&+*'] This is similar to the '&*' helper, but it uses the first
          argument as the separator between the elements of the
          'Foldable' second argument (instead of the ", " default used
          by the '&*' helper).

  ['&?'] This is a helper operator whose second argument is a 'Maybe
         a' (where 'a' is a 'Showable').  This will emit the
         'Showable' of 'a' if the argument is a 'Just' value, or
         nothing (an empty Text Showable) if the argument is a
         'Nothing' value.

  ['&!'] This is a helper operator to apply a Prettyprinter
         transformation function (the first argument) to a 'Sayable'
         message (the second argument).

  ['&!*'] This helper operator is a combination of the '&!' operator
          and the '&*' operator: it applies the first argument (a
          @[PrettyPrinter.Doc ann] -> PrettyPrinter.Doc ann@ function)
          to the foldable collection represented by the second
          argument.

  ['&!+*'] This helper operator is a combination of the '&!' operator and the
           '&+*' operator (and is a trinary rather than a binary operator): it
           applies the first argument (a @[PrettyPrinter.Doc ann] ->
           PrettyPrinter.Doc ann@ function) to the foldable collection
           represented by the third argument, using the second argument to
           specify the separators between the elements.

== Convenience/other

  * This module also provides an instance to convert a Sayable back
    into a Prettyprinter.Pretty.

  * This module provides a helper function: 't'' which can be useful
    when 'OverloadedStrings' is active to designate its argument as
    being a Text string.

    If the following:

@
saying @"error" $ "This is an error:" &- err
@

    results in an error @Could not deduce (Data.String.IsString m0)
    arising from the literal '"This is an error:"'@ then this helper
    can fix that:

@
__ @"error" $ t'"This is an error:" &- err
@

  * This module provides a helper function: 'd'' which can be useful
    when creating a PP.Doc SayableAnn for inclusion into a 'Sayable'
    by fixing the 'ann' of 'PP.Doc ann' to 'SayableAnn'.

    Fixes the error:
@
    • Overlapping instances for Sayable saytag (PP.Doc ann1)
        arising from a use of ‘&-’
      Matching instances:
        instance [overlappable] Sayable tag (PP.Doc ann)
          -- Defined in ‘Taphos.Say’
        instance Sayable tag (PP.Doc SayableAnn)
          -- Defined in ‘Taphos.Say’
      (The choice depends on the instantiation of ‘saytag, ann1’
       To pick the first instance above, use IncoherentInstances
       when compiling the other instance declarations)
@

    This is similar to the '&%' operator except it takes a single
    argument rather than the two arguments passed to the operator.

  * The pattern of converting a saying into a String (e.g. for passing to
    putStrLn) is fairly common, so the simplistic operations of that is provided
    by the 'sez' function.


== Generating final output

  The 'sayable' method of the 'Sayable' class generates instances of
  the 'Saying' data object.  The 'saying' function can be used to
  extract the 'Prettyprinter.Doc' from the 'Saying' object.  This Doc
  can then be converted to a 'Lumberjack.LogMessage' or to a plain
  Text format for display.

== Unfortunate Details

  The use of the Sayable class to translate individual objects is
  fairly straightforward, but the management of the phantom 'saytag'
  type parameter is a bit tricky.  As described above (with the
  Network.URL example), it's possible to provide different output
  generation by providing specialized instances for specific saytags.
  The determination of which instance GHC will use has some
  idiosyncrasies that make lead to unexpected instance selection when
  used transitively (viz.
  https://ghc.gitlab.haskell.org/ghc/doc/users_guide/extsinstances.html).

  For instance:

@
import Network.URL ( URL )
newtype Foo = Foo URL
data Bar a  = Bar String a

-- [previous instances for Sayable URL here...]

instance Sayable "loud" Foo where sayable (Foo url) = t'"{!" &- url &- t'"!}"
instance Sayable saytag Foo where sayable (Foo url) = sayable url

instance (Sayable saytag a) => Sayable saytag (Bar a) where
  sayable (Bar b a) = b &- t'"is" &- a

let host = Host (HTTP True) "github.com" Nothing
let url' = URL (Absolute host) "by/one"
let foo = Foo url'
let bar = Bar "bar" foo
@

  will generate:

>>> putStrLn $ sez @"info" $ t'"INFO:" &- bar &- "via" &- foo
INFO: bar is "https://github.com/by/one" via "https://github.com/by/one"
>>> putStrLn $ sez @"loud" $ t'"LOUD:" &- bar &- "via" &- foo
LOUD: bar is {! "https://github.com/by/one" !} via {! "https://github.com/by/one" !}

  which is expected.  However, if the calls to 'sez' are moved
  to a separate file from the instance declarations, the compilation
  error will be:

  @Overlapping instances for Sayable "loud" Foo arising from a use of &-@

  for the last (loud) line.  To resolve this, use OVERLAPPING and/or
  OVERLAPPABLE specifications on the instance declarations.  Usually
  it's sufficient (and easiest) to add the OVERLAPPABLE to the generic
  instance:

> instance Sayable "loud" Foo where sayable (Foo s) = t'"{!" &- s &- t'"!}"
> instance {-# OVERLAPPABLE #-} Sayable saytag Foo where sayable (Foo s) = sayable s

  [Note: if you are viewing the above via Haddock HTML, the second
  line has the @instance@ keyword, followed by an open comment
  directive (open curly brace, dash, hash) , @OVERLAPPABLE@ and a
  closing comment directive (hash, dash, close curly brace), followed
  by the @Sayable@ keyword, but that doesn't render under HTML Haddock
  (circa 2022).]

  There's another twist to this story though.  To observe this new
  twist, add a 'Baz' datastructure and its generic 'Sayable' instance:

  > data Baz = Baz Foo
  > instance Sayable saytag Baz where sayable (Baz a) = t'"BAZ :=" &- foo

  Now the following calls and corresponding output can be observed:

>>> putStrLn $ sez @"info" $ t'"INFO:" &- bar &- t'"and" &- baz
INFO: bar is "https://github.com/by/one" and BAZ := "https://github.com/by/one"
>>> putStrLn $ sez @"loud" $ t'"LOUD:" &- bar &- t'"and" &- baz
LOUD: bar is {! "https://github.com/by/one" !} and BAZ := "https://github.com/by/one"

  Notice how the @foo@ value in @bar@ changes when the '@"loud"' saytag is
  used, but the same @foo@ value in @baz@ does not change!

  The difference here is in the mechanism GHC uses to select instances
  (as described on the referenced link above).  In short, for @bar@, the
  generic 'Sayable' instance has a constraint for the inner element,
  which causes GHC to wait until the final use case to determine what
  the specific type parameters are; it sees the @"loud"@ @saytag@ value
  and selects the @"loud"@ 'Foo' 'Sayable' instance as the most specific.
  However, the @baz@ 'Sayable' instance does not have a constraint, so GHC
  takes the conservative approach and uses the most general instance,
  which means that it transitively selects the generic 'Foo' 'Sayable'
  instance instead of the @"loud"@ instance.

  There are two ways to fix this:

  1. Provide explicit @"loud"@ 'Sayable' instance for 'Baz'.  This is
     problematic, because this must be done for *each* saytag for
     which there is a variation and it must be done for *each* upper
     level 'Sayable' instance.

  2. Provide 'Sayable' constraints for each sub-element.  This generates
     larger type signatures, but is preferrable to solution 1 because
     it makes no assumptions about current or future saytags and
     variations.

     This 'Sayable' constraint was already present on the 'Bar'
     'Sayable' instance because of the parameterized type for 'Bar';
     the 'Baz' type has no type parameter, but a constraint can still
     be added for each interior type:

> instance Sayable saytag Foo => Sayable saytag Baz where
>   sayable (Baz a) = t'"BAZ :=" &- foo

  Using either of the above solutions, the new output is fully
  specialized as desired:

>>> putStrLn $ sez @"info" $ t'"INFO:" &- bar &- t'"and" &- baz
INFO: bar is "https://github.com/by/one" and BAZ := "https://github.com/by/one"
>>> putStrLn $ sez @"loud" $ t'"LOUD:" &- bar &- t'"and" &- baz
LOUD: bar is {! "https://github.com/by/one" !} and BAZ := {! "https://github.com/by/one" !}


  The good news here is that the complexity is all handled at the
  Sayable instance definition and the client usage calls are all
  unaffected, regardless of which solution is chosen.

-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Text.Sayable
  (
    Sayable(sayable)
  , Saying(Saying, saying)
  , SayMessage(SayMessage, sayMsg)
  , t'
  , d'
  , (&-)
  , (&+)
  , (&%)
  , (&*)
  , (&+*)
  , (&?)
  , (&!)
  , (&!*)
  , (&!+*)
  , SayableAnn(SayableAnn)
  , sez
  )
where

import qualified Control.Monad.Catch as X
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Int as I
import           Data.Text ( Text, pack )
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TEL
import qualified Data.Word as W
import           GHC.Exts ( Proxy#, proxy# )
import           GHC.OverloadedLabels
import           GHC.TypeLits ( Symbol, KnownSymbol, symbolVal' )
import           Numeric.Natural ( Natural )
import           Prettyprinter ( (<+>) )
import qualified Prettyprinter as PP


-- | The main class of things that can be passed to 'say'.  Arguments
-- provided to 'sayable' or 'sez' will be converted to the sayable form by
-- automatically applying the appropriate instance of this class.  The
-- default implementation is:
--
-- > sayable = Saying . Prettyprinter.pretty

class Sayable (tag :: Symbol) v where
  sayable :: v -> Saying tag

  default sayable :: PP.Pretty v => v -> Saying tag
  sayable = Saying . PP.pretty


-- | The result of applying the sayable method of the Sayable class is
-- the Saying object.  This object is internal to the Say module and
-- is mostly used for subsequently combining with additional Saying
-- objects to produce the final Saying object that is converted to a
-- SayMessage for actual logging.  A Sayable supports a Semigroup
-- combinator to allow composition of messages.

newtype Saying (tag :: Symbol) = Saying { saying :: PP.Doc SayableAnn }

instance Semigroup (Saying tag) where
  Saying sm1 <> Saying sm2 = Saying $ sm1 <+> sm2

-- | This is the principle data type that carries 'say' messages to the output
-- conversion point.  This is generated internally by the 'saying' functions and
-- is usually not directly needed by client code.

newtype SayMessage = SayMessage { sayMsg :: PP.Doc SayableAnn }

-- | Inputs that are 'Sayable', i.e. that can be converted to a Saying

instance {-# OVERLAPPING #-} (tagA ~ tagB) => Sayable tagA (Saying tagB) where sayable = id
instance Sayable tag Text
instance Sayable tag String
instance Sayable tag Char
instance Sayable tag Bool
instance Sayable tag Int
instance Sayable tag Integer
instance Sayable tag I.Int32
instance Sayable tag I.Int64
instance Sayable tag W.Word8
instance Sayable tag W.Word16
instance Sayable tag W.Word32
instance Sayable tag W.Word64
instance Sayable tag Natural
instance Sayable tag TL.Text
instance Sayable tag BS.ByteString where sayable = sayable . TE.decodeUtf8
instance Sayable tag BSL.ByteString where sayable = sayable . TEL.decodeUtf8
instance Sayable tag X.SomeException where sayable = sayable . X.displayException
instance Sayable tag (PP.Doc SayableAnn) where sayable = Saying
instance {-# OVERLAPPABLE #-} Sayable tag (PP.Doc ann) where sayable = Saying . PP.unAnnotate

-- | A Saying can be converted back into a PP.Pretty instance
-- representation.  Just saying... :-)
--
-- This can be convenient to apply Prettyprinter formatting elements.
-- For example:
--
-- > instance Sayable saytag Foo where
-- >   sayable foo = sayable @saytag $ PP.group $ PP.pretty
-- >                 $ field1 foo &- sayable @saytag PP.line &- field2 foo
--
-- This uses Prettyprinter's 'group' and 'line' formatters to show the
-- two fields on the same line if they will fit, otherwise stacked on
-- top of each other.  Note that the second portion needs an explicit
-- 'TypeApplication' (applied here to the 'PP.line') because the
-- 'PP.group' and 'PP.pretty' functions do not propagate that outer
-- 'saytag' to the inner portion.

instance PP.Pretty (Saying tag) where pretty = PP.unAnnotate . saying


-- | A helper operator allowing two Sayable items to be composed into
-- a Saying.  This is the most common operator used to construct
-- composite Sayable messages.  The two Sayable items are separated by
-- a space.
(&-) :: forall saytag m n . (Sayable saytag m, Sayable saytag n)
     => m -> n -> Saying saytag
m &- n = sayable m <> sayable n
infixl 1 &-

-- | A helper operator allowing two Sayable items to be composed into
-- a Saying by placing the two Sayable items immediately adjacent with
-- no intervening spaces.  This is the high-density version of the
-- more common '&-' operator.
(&+) :: forall saytag m n . (Sayable saytag m, Sayable saytag n)
     => m -> n -> Saying saytag
m &+ n = Saying $ (saying $ sayable @saytag m) <> (saying $ sayable @saytag n)
infixl 1 &+

-- | A helper operator allowing a Sayable item to be composed with a
-- Pretty item into a Saying.  This is infrequently used and primarily
-- allows the composition of a data object which has a "Prettyprinter"
-- instance but no 'Sayable' instance.
(&%) :: (Sayable tag m, PP.Pretty n) => m -> n -> Saying tag
m &% n = sayable m <> sayable (PP.pretty n :: PP.Doc SayableAnn)
infixl 1 &%

-- | A helper operator to /apply/ a "Prettyprinter" (@Doc ann -> Doc
-- ann@) function (the first argument) to the Sayable in the second
-- argument.  This is different from the '&%' operator in that the
-- former uses 'Prettyprinter.hsep' to join two independent
-- 'Prettyprinter.Doc' 'Saying' values, whereas this operator applies
-- a transformation (e.g. @Prettyprinter.annotate AnnValue@ or
-- @Prettyprinter.align . Prettyprinter.group@) to the
-- 'Prettyprinter.Doc' in the second 'Saying' argument.
(&!) :: forall tag m . Sayable tag m
     => (PP.Doc SayableAnn -> PP.Doc SayableAnn) -> m -> Saying tag
pf &! m = Saying $ pf $ saying $ sayable @tag m
infixl 2 &!

-- | A helper operator allowing a Sayable item to be composed with a
-- Foldable series of Sayable items.  This can be used when the second
-- argument is a List, Sequence, Set, etc. to add all elements of the
-- set (comma-separated).
--
-- Note: this instance makes it easy to output lists, Sequence,
-- NonEmpty.List, etc., but it can have undesireable effects for data
-- tructures whose Foldable (Functor) is irregular... for example,
-- folding over a tuple only returns the 'snd' value of a tuple.
-- Consider wrapping tuples in a newtype with an explicit Sayable to
-- avoid this.
(&*) :: forall tag m e t
        . (Sayable tag m, Sayable tag e, Foldable t) => m -> t e -> Saying tag
m &* l = let addElem e (s, Saying p) =
               (", ", Saying $ saying (sayable @tag e) <> s <> p)
         in sayable m <> (snd $ foldr addElem ("", Saying PP.emptyDoc) l)
infixl 1 &*


-- | A helper operator that generates a sayable from a list of sayable
-- items, separated by the first sayable
(&+*) :: forall tag m e t
         . (Sayable tag m, Sayable tag e, Foldable t) => m -> t e -> Saying tag
m &+* l = let addElem e (s, Saying p) = (Just m,
                                         case s of
                                           Nothing -> sayable @tag e &+ p
                                           Just s' -> sayable @tag e &+ s' &+ p
                                        )
          in snd $ foldr addElem (Nothing, Saying PP.emptyDoc) l
infixl 2 &+*


-- | A helper operator that applies the first argument which converts
-- an array of 'Prettyprinter.Doc ann' elements to a single
-- 'PrettyPrinter.Doc ann' element to the second argument, which is a
-- Foldable collection of 'Sayable' items.  This is essentially a
-- combination of the '&!' and '&*' operators.
(&!*) :: forall tag m t
         . (Sayable tag m, Foldable t)
      => ([PP.Doc SayableAnn] -> PP.Doc SayableAnn) -> t m -> Saying tag
pf &!* l = let addElem e (s, p) = (", ", saying (sayable @tag e) <> s : p)
           in Saying $ pf $ snd $ foldr addElem ("", []) l
infixl 2 &!*


-- | A helper operator that applies the first argument which converts
-- an array of 'Prettyprinter.Doc ann' elements to a single
-- 'PrettyPrinter.Doc ann' element to the second argument, which is a
-- Foldable collection of 'Sayable' items.  This is essentially a
-- combination of the '&!' and '&+*' operators.
--
-- Unlike the other operators defined in this package, this is a trinary operator
-- rather than a binary operator.  Because function application (whitespace) is
-- the highest precedence, the last argument will typically need a preceeding $
-- to prevent applying the second argument to the third argument before applying
-- this operator.
--
-- > import qualified Prettyprinter as PP
-- >
-- > putStrLn $ sez @"info" $ PP.fillSep &!+* t'" and " $ ["one", "two", "three"]
-- one and two and three
--
(&!+*) :: forall tag m t b . (Sayable tag b, Sayable tag m, Foldable t)
       => ([PP.Doc SayableAnn] -> PP.Doc SayableAnn) -> b -> t m -> Saying tag
pf &!+* b = let addElem e (s, p) =
                  (Just b, (case s of
                               Nothing -> saying (sayable @tag e)
                               Just x -> saying (sayable @tag e &+ x)
                           ) : p)
            in Saying . pf . snd . foldr addElem (Nothing, [])
infixl 2 &!+*


-- | A helper operator allowing a Sayable item to be wrapped in a
-- 'Maybe'.  This adds the 'Sayable' of the first argument to the
-- 'Sayable' of the second argument in the 'Just' case, or just emits
-- the 'Sayable' of the first argument if the second argument is
-- 'Nothing'.
(&?) :: forall tag m e
        . (Sayable tag m, Sayable tag e) => m -> Maybe e -> Saying tag
m &? Nothing = sayable m
m &? (Just a) = sayable m <> sayable a
infixl 1 &?

-- | A helper function to use when 'OverloadedStrings' is active to
-- identify the following quoted literal as a "Data.Text" object.
t' :: Text -> Text
t' = id
{-# INLINE t' #-}


-- | A helper function to use when creating a PP.Doc SayableAnn data
-- object (i.e. fixing the 'ann' of 'Doc ann' to 'SayableAnn')
d' :: PP.Pretty n => n -> PP.Doc SayableAnn
d' = PP.pretty


----------------------------------------------------------------------

-- | This is the default annotation type for the Saying module.  The
-- Prettyprinter reannotate operation can be used to change this annotation into
-- any other annotation type the client desires.
--
-- The SayableAnn is an instance of IsLabel, so if OverloadedLabels is enabled,
-- this can easily be specified:
--
-- @
-- import qualified Prettyprinter as PP
-- import Text.Sayable
--
-- putStrLn $ sez @"info" $ PP.annotate #myann $ "Hello" &- "world!"
-- @
--
-- Note however that labels cannot start with a capital letter.

data SayableAnn = SayableAnn Text

instance KnownSymbol ann => IsLabel (ann :: Symbol) SayableAnn where
  fromLabel = SayableAnn $ pack $ symbolVal' (proxy# :: Proxy# ann)


----------------------------------------------------------------------

-- | This is a convenience function that can be used for simple conversions of a
-- Sayable to a String.

sez :: forall saytag a . Sayable saytag a => a -> String
sez = show . saying . sayable @saytag
