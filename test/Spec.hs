{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec ( hspec, describe, it, shouldBe )
import Day01 (solve)

main :: IO ()
main = hspec $ do
  describe "Day 1" $ do
    let p1 = 
          "1abc2\n\
          \pqr3stu8vwx\n\
          \a1b2c3d4e5f\n\
          \treb7uchet"
        
        p2 =
          "two1nine\n\
          \eightwothree\n\
          \abcone2threexyz\n\
          \xtwone3four\n\
          \4nineeightseven2\n\
          \zoneight234\n\
          \7pqrstsixteen"
        
        cases =
          [ ("Part 1", p1, 142, fst)
          , ("Part 2", p2, 281, snd) ]
        
        test (name, txt, expected, itm) = 
          it (name ++ " matches the sample") $ do
            itm (Day01.solve txt) `shouldBe` expected
        
        in mapM_ test cases


