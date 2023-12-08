{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec ( hspec, describe, it, shouldBe, Spec )
import Day01 (solve)
import Day02 (solve)
import Day03 (solve)
import Day04 (solve)
import Day05 (solve)
import Day06 (solve)
import Day07 (solve)
import Day08 (solve)

testDay :: (Show a1, Show a2, Eq a1, Eq a2)
        => (p -> (a1, a2)) -> a1 -> a2 -> p -> Spec
testDay solver exp1 exp2 example = do
  let (ans1, ans2) = solver example
  it "Part 1 matches the example" $ do
    ans1 `shouldBe` exp1
  it "Part 2 matches the example" $ do
    ans2 `shouldBe` exp2

testDay2 :: (Show a1, Show a2, Eq a1, Eq a2)
        => (p -> (a1, a2)) -> a1 -> a2 -> p -> p -> Spec
testDay2 solver exp1 exp2 example1 example2 = do
  let ans1 = fst $ solver example1
      ans2 = snd $ solver example2
  it "Part 1 matches the example" $ do
    ans1 `shouldBe` exp1
  it "Part 2 matches the example" $ do
    ans2 `shouldBe` exp2

main :: IO ()
main = hspec $ do
  describe "Day 1" $ do
    testDay2 Day01.solve 142 281
      "1abc2\n\
      \pqr3stu8vwx\n\
      \a1b2c3d4e5f\n\
      \treb7uchet"
        "two1nine\n\
        \eightwothree\n\
        \abcone2threexyz\n\
        \xtwone3four\n\
        \4nineeightseven2\n\
        \zoneight234\n\
        \7pqrstsixteen"


  describe "Day 2" $ do
    testDay Day02.solve 8 2286
      "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green\n\
      \Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue\n\
      \Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red\n\
      \Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red\n\
      \Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"

  describe "Day 3" $ do
    testDay Day03.solve 4361 467835
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

  describe "Day 4" $ do
    testDay Day04.solve 13 30
      "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53\n\
      \Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19\n\
      \Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1\n\
      \Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83\n\
      \Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36\n\
      \Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"

  describe "Day 5" $ do
    testDay Day05.solve 35 46
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


  describe "Day 6" $ do
    testDay Day06.solve 288 71503
      "Time:      7  15   30\n\
      \Distance:  9  40  200"

  describe "Day 7" $ do
    testDay Day07.solve 6440 5905
      "32T3K 765\n\
      \T55J5 684\n\
      \KK677 28\n\
      \KTJJT 220\n\
      \QQQJA 483"

  describe "Day 8" $ do
    testDay2 Day08.solve 2 6
      "RL\n\
      \\n\
      \AAA = (BBB, CCC)\n\
      \BBB = (DDD, EEE)\n\
      \CCC = (ZZZ, GGG)\n\
      \DDD = (DDD, DDD)\n\
      \EEE = (EEE, EEE)\n\
      \GGG = (GGG, GGG)\n\
      \ZZZ = (ZZZ, ZZZ)"
        "LR\n\
        \\n\
        \11A = (11B, XXX)\n\
        \11B = (XXX, 11Z)\n\
        \11Z = (11B, XXX)\n\
        \22A = (22B, XXX)\n\
        \22B = (22C, 22C)\n\
        \22C = (22Z, 22Z)\n\
        \22Z = (22B, 22B)\n\
        \XXX = (XXX, XXX)"