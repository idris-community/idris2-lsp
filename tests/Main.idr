module Main

import Test.Golden

%default covering

allTests : TestPool
allTests = MkTestPool []
  [ "messages001"
  ]

main : IO ()
main = runner
  [ testPaths "lsp" allTests
  ] where
    testPaths : String -> TestPool -> TestPool
    testPaths dir = record { testCases $= map ((dir ++ "/") ++) }
