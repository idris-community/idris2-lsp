module Main

import Test.Golden

%default covering

allTests : TestPool
allTests = MkTestPool "Messages" [] Nothing
  [ "messages001"
  ]

main : IO ()
main = runner
  [ testPaths "lsp" allTests
  ] where
    testPaths : String -> TestPool -> TestPool
    testPaths dir = { testCases $= map ((dir ++ "/") ++) }
