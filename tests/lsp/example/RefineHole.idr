
import Data.Maybe
import Data.List

data NoParam
  = NPCons1
  | NPCons2 Nat

noParam : NoParam
noParam = ?test1

-- SCENARIO
-- WHEN: Code action on ?test1
-- THEN: Code action result contains:
--       - 'Fill Hole ~ NPCons1'
--       - 'Fill Hole ~ (NDCons2 ?test1_0)'

data WithParam a
  = WPCons1
  | WPCons2 a Nat a

withParam : WithParam Nat
withParam = ?test2

-- SCENARIO
-- WHEN: Code action on ?test2
-- THEN: Code action result contains:
--       - 'Fill Hole ~ WPCons1'
--       - 'Fill Hole ~ (WPCons2 ?test2_2 ?test2_1 ?test2_0)'

similarNames : Bool
similarNames = ?isJ (Just 0)

-- SCENARIO
-- WHEN: Code action on ?isJ
-- THEN: Code action result contains:
--       - 'Fill Hole ~ isJust'
-- NOTE: This is failing now, as we need to improve the polymorphic type unification

record Record a where
  constructor MkRecord
  field1 : a
  field2 : String

someRecord : Record Int
someRecord = ?test3

-- SCENARIO
-- WHEN: Code action on ?test3
-- THEN: Code action result contains:
--       - 'Fill Hole ~ (MkRecord ?field1_1 ?field2_0)'

applied : NoParam
applied = ?test4 (the Nat 0)

-- SCENARIO
-- WHEN: Code action on ?test4
-- THEN: Code action result contains:
--       - 'Fill Hole ~ NPCons2'

not : Bool -> Bool
not True = False
not False = True

-- SCENARIO
-- WHEN: Code action on ?no
-- THEN: Code action result contains:
--       - 'Fill Hole ~ not'

someBool : Bool
someBool = ?no False

namespace Private

  export
  data PrivateData
    = PrivateConstructor

  export
  privateData : PrivateData
  privateData = PrivateConstructor

-- SCENARIO
-- WHEN: Code action on ?pd2
-- THEN: Code action result contains:
--       - 'Fill Hole ~ PrivateDataConstructor'

  export
  privateData2 : PrivateData
  privateData2 = ?pd2

-- SCENARIO
-- WHEN: Code action on ?pd1
-- THEN: Code action result contains:
--       - 'Fill Hole ~ privateData'
--  AND: Code action does not contain:
--       - 'Fill Hole ~ PrivateDataConstructor'

somePrivate : PrivateData
somePrivate = ?pd1

data SomeNamedADT : Type -> Type where
  NamedField1 : (name1 : String) -> (name2 : a) -> SomeNamedADT a
  NamedField2 : (name3 : Int) -> SomeNamedADT Int
  NamedField3 : (name3 : Int) -> String -> SomeNamedADT a

-- SCENARIO
-- WHEN: Code action on ?sna
-- THEN: Code action result contains:
--       - 'Fill Hole ~ (NameField1 ?name1_1 ?name2_0)'
--       - 'Fill Hole ~ (NameField2 ?name3_0)'
--       - 'Fill Hole ~ (NameField1 ?name3_1 ?sna_0)'

someNamedADT : SomeNamedADT Int
someNamedADT = ?sna
