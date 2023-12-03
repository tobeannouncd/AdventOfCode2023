{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec ( hspec, describe, it, shouldBe )
import Day01 (solve)
import Day02 (solve)
import Day03 (solve)

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

  describe "Day 2" $ do
    let example =
          "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green\n\
          \Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue\n\
          \Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red\n\
          \Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red\n\
          \Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"

        (ans1, ans2) = Day02.solve example

    it "Part 1 matches the sample" $ do
      ans1 `shouldBe` 8
    it "Part 2 matches the sample" $ do
      ans2 `shouldBe` 2286

  describe "Day 3" $ do
    let example =
          "467..114..\n\
          \...*......\n\
          \..35..633.\n\
          \......#...\n\
          \617*......\n\
          \.....+.58.\n\
          \..592.....\n\
          \......755.\n\
          \...$.*....\n\
          \.664.598.."

    it "Part 1" $ do
      fst (Day03.solve example) `shouldBe` 4361
      