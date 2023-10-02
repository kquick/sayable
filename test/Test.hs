{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import           Data.Text ( Text )
import qualified Prettyprinter as PP
import           Text.Sayable

import           Test.Hspec
import           Test.Tasty
import           Test.Tasty.Hspec
import           Test.Tasty.Runners.AntXML


main :: IO ()
main = tests >>= defaultMainWithIngredients (antXMLRunner : defaultIngredients)

tests :: IO TestTree
tests = testGroup "Sayable" <$> sequence
  [
    testSpec "Operators" $
    describe "operator results shown in haddocks" $ do
      it "renders &-" $
        (sez @"info" $ t'"hello" &- t'"world") `shouldBe` "hello world"
      it "renders &+" $
        (sez @"info" $ t'"hello" &+ t'"world") `shouldBe` "helloworld"
      it "renders &%" $
        (sez @"info" $ t'"hello" &% (t'"world", t'"!")) `shouldBe` "hello (world, !)"
      it "renders &!" $
        (sez @"info" $ PP.group &! t'"hi") `shouldBe` "hi"
      it "renders &*" $
        (sez @"info" $ t'"three:" &* [1, 2, 3::Int]) `shouldBe` "three: 1, 2, 3"
      it "renders &+*" $
        (sez @"info" $ t'"three:" &- '(' &+* [1, 2, 3::Int] &+ ')')
        `shouldBe` "three: (1, 2, 3)"
      it "renders &:*" $
        (sez @"info" $ t'"three:" &- t'".." &:* [1, 2, 3::Int])
        `shouldBe` "three: 1..2..3"
      it "renders &!*" $
        (sez @"info" $ t'"three:" &- PP.align . PP.vsep &!* [1, 2, 3::Int])
        `shouldBe` "three: 1, \n       2, \n       3"
      it "renders &!$*" $
        (sez @"info" $ t'"three:" &- PP.align &!$* [1, 2, 3::Int])
         `shouldBe` "three: 1, 2, 3"
      it "renders &!:*" $
        (sez @"info" $ t'"three:" &- (PP.align . PP.vsep &!:* (t'" or")) [1, 2, 3::Int])
        `shouldBe` "three: 1 or\n       2 or\n       3"
      it "renders &?" $
        (sez @"info" $ t'"It's" &? Just (t'"something") &- t'"or" &? (Nothing :: Maybe Text))
        `shouldBe` "It's something or"
      it "renders &!?" $
        (sez @"info" $ PP.group &!? Just (t'"hi")) `shouldBe` "hi"
      it "renders &<" $
        (sez @"info" $ t'"Hello" &< t'"world") `shouldBe` "Hello\nworld"
      it "renders &<*" $
        (sez @"info" $ t'"three:" &<* [1, 2, 3::Int]) `shouldBe` "three:\n1, 2, 3"
      it "renders &<? Just" $
        (sez @"info" $ t'"First" &<? Just (t'"something"))
        `shouldBe` "First\nsomething"
      it "renders &<? Nothing" $
        (sez @"info" $ t'"Then" &<? (Nothing :: Maybe Text)) `shouldBe` "Then"
      it "renders &+?" $
        (sez @"info" $ t'"It's" &+? (Nothing :: Maybe Text) &- t'"ok" &+? Just ("time" :: Text))
        `shouldBe` "It's oktime"
  ]
