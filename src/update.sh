for i in {2..25}
do
    printf "{-# LANGUAGE OverloadedStrings #-}\nmodule Day%d (solve) where\n\nimport qualified Data.Text as T\n\nsolve :: T.Text -> IO ()\nsolve = undefined" $i > "Day$i.hs"
done
