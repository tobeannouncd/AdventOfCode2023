for i in {10..25}
do
    printf "{-# LANGUAGE OverloadedStrings #-}\nmodule Day%02d (solve) where\n\nimport qualified Data.Text as T\n\nsolve :: T.Text -> ((),())\nsolve = undefined" $i > "Day$i.hs"
done
