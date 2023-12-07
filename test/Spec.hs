{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec ( hspec, describe, it, shouldBe )
import Day01 (solve)
import Day02 (solve)
import Day03 (solve)
import Day04 (solve)
import Day05 (solve)
import Day06 (solve)

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

        (ans1, ans2) = Day03.solve example

    it "Part 1 matches the sample" $ do
      ans1 `shouldBe` 4361
    it "Part 2 matches the sample" $ do
      ans2 `shouldBe` 467835
  
  describe "Day 4" $ do
    let example = 
          "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53\n\
          \Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19\n\
          \Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1\n\
          \Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83\n\
          \Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36\n\
          \Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"

        (ans1,ans2) = Day04.solve example

    it "Part 1 matches the sample" $ do
      ans1 `shouldBe` 13
    it "Part 2 matches the sample" $ do
      ans2 `shouldBe` 30

  describe "Day 5" $ do
    let example = 
          "seeds: 79 14 55 13\n\
          \\n\
          \seed-to-soil map:\n\
          \50 98 2\n\
          \52 50 48\n\
          \\n\
          \soil-to-fertilizer map:\n\
          \0 15 37\n\
          \37 52 2\n\
          \39 0 15\n\
          \\n\
          \fertilizer-to-water map:\n\
          \49 53 8\n\
          \0 11 42\n\
          \42 0 7\n\
          \57 7 4\n\
          \\n\
          \water-to-light map:\n\
          \88 18 7\n\
          \18 25 70\n\
          \\n\
          \light-to-temperature map:\n\
          \45 77 23\n\
          \81 45 19\n\
          \68 64 13\n\
          \\n\
          \temperature-to-humidity map:\n\
          \0 69 1\n\
          \1 0 69\n\
          \\n\
          \humidity-to-location map:\n\
          \60 56 37\n\
          \56 93 4"
        
        (ans1, ans2) = Day05.solve example
    
    it "Part 1 matches the sample" $ do
      ans1 `shouldBe` 35
    it "Part 2 matches the sample" $ do
      ans2 `shouldBe` 46
  
  describe "Day 6" $ do
    let example =
          "Time:      7  15   30\n\
          \Distance:  9  40  200"
        (ans1, ans2) = Day06.solve example
    
    it "Part 1 matches the sample" $ do
      ans1 `shouldBe` 288
    it "Part 2 matches the sample" $ do
      ans2 `shouldBe` 71503
