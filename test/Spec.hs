{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec

import SyntaxChecker

main :: IO ()
main = hspec $ do
  describe "SyntaxChecker.checkSyntax" $ do
    it "plain string" $ do
      checkSyntax "cellar door" `shouldBe` Right "cellar door"

    it "plain string with quotes" $ do
      checkSyntax "\"cellar\" 'door'" `shouldBe` Right "\"cellar\" 'door'"
      checkSyntax "\"cellar door" `shouldBe` Left '\"'
      checkSyntax "\"cellar' \"door'" `shouldBe` Left '\''
      checkSyntax "\"ce'llar do'or\"" `shouldBe` Right "\"ce'llar do'or\""

    it "unmatched (" $ do
      checkSyntax "cellar (door" `shouldBe` Left '('
      checkSyntax "(cellar (door)" `shouldBe` Left '('
      checkSyntax "(cellar door)(" `shouldBe` Left '('

    it "unmatched ( with quotes" $ do
      checkSyntax "'cellar' (door" `shouldBe` Left '('
      checkSyntax "cellar '(door'" `shouldBe` Right "cellar '(door'"
      checkSyntax "'cellar \"(door\"'" `shouldBe` Right "'cellar \"(door\"'"
      checkSyntax "\"(\"cellar (door)" `shouldBe` Right "\"(\"cellar (door)"
      checkSyntax "(cellar do'or)('" `shouldBe` Left '('
      checkSyntax "'(cellar do'or)(" `shouldBe` Left '('

    it "unmatched )" $ do
      checkSyntax "cellar )door" `shouldBe` Left ')'
      checkSyntax "(cellar )door)" `shouldBe` Left ')'
      checkSyntax "(cellar door))" `shouldBe` Left ')'

    it "unmatched ) with quotes" $ do
      checkSyntax "'cellar' )door" `shouldBe` Left ')'
      checkSyntax "'cellar )d'oor" `shouldBe` Right "'cellar )d'oor"

    it "matching ()" $ do
      checkSyntax "cellar (door)" `shouldBe` Right "cellar (door)"
      checkSyntax "(cellar (door))" `shouldBe` Right "(cellar (door))"
      checkSyntax "(cel(lar) (do)or)" `shouldBe` Right "(cel(lar) (do)or)"

    it "mixed brackets" $ do
      checkSyntax "ce{l(l[ar] )d(o)o}r" `shouldBe` Right "ce{l(l[ar] )d(o)o}r"

    it "terminal string escape slash" $ do
      checkSyntax "ce{l(l[ar])} '\\" `shouldBe` Left '\\'
      checkSyntax "ce{l(l[ar])} \\" `shouldBe` Right "ce{l(l[ar])} \\"
