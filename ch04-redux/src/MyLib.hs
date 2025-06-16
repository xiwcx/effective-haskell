{-# LANGUAGE RecordWildCards #-}

module MyLib where

import Control.Exception (MaskingState)

-- value constructor
-- left side assignment is type, right side is normal function at the value level
data CustomerInfo = CustomerInfo String String Int Int
  deriving (Show)

customerGeorge :: CustomerInfo
customerGeorge =
  CustomerInfo "Georgie" "Bird" 10 100

-- using the data type to pattern match the first argument
showCustomer :: CustomerInfo -> String
showCustomer (CustomerInfo first l count balance) =
  let fullName = first <> " " <> l
      name = "name: " <> fullName
      count' = "count: " <> show count
      balance' = "balance: " <> show balance
   in name <> " " <> count' <> " " <> balance'

applyDiscount :: CustomerInfo -> CustomerInfo
applyDiscount
  customer =
    case customer of
      (CustomerInfo "Georgie" "Bird" count balance) ->
        CustomerInfo "Georgie" "Bird" count (balance `div` 4)
      (CustomerInfo "Porter" "Pupper" count balance) ->
        CustomerInfo "Porter" "Pupper" count (balance `div` 2)
      otherCustomer -> otherCustomer

firstName :: CustomerInfo -> String
firstName (CustomerInfo name _ _ _) = name

lastName :: CustomerInfo -> String
lastName (CustomerInfo _ l _ _) = l

count :: CustomerInfo -> Int
count (CustomerInfo _ _ count _) = count

balance :: CustomerInfo -> Int
balance (CustomerInfo _ _ _ balance) = balance

updateFirstName :: CustomerInfo -> String -> CustomerInfo
updateFirstName (CustomerInfo _ lastName count balance) firstName =
  CustomerInfo firstName lastName count balance

data CustomerInfoRec = CustomerInfoRec
  { custFirstName :: String,
    custLastName :: String,
    custWidgetCount :: Int,
    custBalance :: Int
  }

customerFactory :: String -> String -> CustomerInfoRec
customerFactory fname lname =
  CustomerInfoRec
    { custBalance = 0,
      custWidgetCount = 5,
      custFirstName = fname,
      custLastName = lname
    }

totalWidgetCount :: [CustomerInfoRec] -> Int
totalWidgetCount =
  sum . map custWidgetCount

emptyCart :: CustomerInfoRec -> CustomerInfoRec
emptyCart customer =
  customer {custWidgetCount = 0, custBalance = 0}

showCustomerRec :: CustomerInfoRec -> String
showCustomerRec CustomerInfoRec {..} =
  unwords [custFirstName, custLastName, show custWidgetCount, show custBalance]

customerJorge :: CustomerInfoRec
customerJorge =
  let custFirstName = "Jorge"
      custLastName = "Bird"
      custWidgetCount = 10
      custBalance = 100
   in CustomerInfoRec {..}

customerRecFactory :: String -> String -> CustomerInfoRec
customerRecFactory custFirstName custLastName =
  let custWidgetCount = 10
      custBalance = 100
   in CustomerInfoRec {..}

data MyBool = MyTrue | MyFalse

data Direction = North | South | East | West

-- sum-of-product type example
data PreferredContactMethod
  = Email String
  | TextMessage String
  | Mail String String String Int

emailContact :: PreferredContactMethod
emailContact = Email "me@example.com"

textContact :: PreferredContactMethod
textContact = TextMessage "+1 555 555 5555"

mailContact :: PreferredContactMethod
mailContact = Mail "road" "unit" "city, state" 5555555

confirmContact :: PreferredContactMethod -> String
confirmContact contact =
  case contact of
    -- Email emailAddress ->
    --   "OK, I'll email you at " <> emailAddress
    -- TextMessage phoneNumber ->
    --   "OK, I'll text you at " <> phoneNumber
    -- Mail road _ cityState _ ->
    --   unwords ["OK, I'll mial this to you at", road, cityState]
    Mail {} -> "OK, letter!"

data StringOrNumber = Str String | N Int

stringsAndNumbers :: [StringOrNumber]
stringsAndNumbers =
  [ Str "foo",
    N 2,
    Str "bar",
    N 4
  ]

data CustInfo = CustInfo
  { custName :: String,
    custBal :: Int
  }

data EmpInfo = EmpInfo
  { empName :: String,
    empManagerName :: String,
    empSalary :: Int
  }

data Person = Customer CustInfo | Employee EmpInfo

g :: Person
g =
  Customer $
    CustInfo
      { custName = "geo",
        custBal = 100
      }

porter :: Person
porter =
  Employee $
    EmpInfo
      { empName = "porter",
        empManagerName = "remi",
        empSalary = 10
      }

getPersonName :: Person -> String
getPersonName person =
  case person of
    Employee employee -> empName employee
    Customer customer -> custName customer

data MyMaybe a = MyNothing | MyJust a

getPersonManager :: Person -> MyMaybe String
getPersonManager person =
  case person of
    Employee employee -> MyJust (empName employee)
    _ -> MyNothing

getPersonSalary :: Person -> MyMaybe Int
getPersonSalary person =
  case person of
    Employee employee -> MyJust (empSalary employee)
    _ -> MyNothing

maybeToList :: Maybe a -> [a]
maybeToList (Just val) = [val]
maybeToList Nothing = []

data Weither a b = WLeft a | WRight b

weitherToMaybe :: Weither b a -> MyMaybe a
weitherToMaybe e =
  case e of
    WLeft _ -> MyNothing
    WRight val -> MyJust val

handleMissingRight :: Either String (Maybe a) -> Either String a
handleMissingRight e =
  case e of
    Left err -> Left err
    Right (Just val) -> Right val
    Right Nothing -> Left "missing value"

-- most data structures in Haskell are recursive or inductively defined
-- inductive -- adj., characterized by the inference of general laws from particular instances
-- a peano number is either zero or a successor to a peano number
data Peano = Z | S Peano
  deriving (Show)

toPeano :: Int -> Peano
toPeano 0 = Z
toPeano n = S (toPeano $ n - 1)

fromPeano :: Peano -> Int
fromPeano Z = 0
fromPeano (S p) = succ (fromPeano p)

eqPeano :: Peano -> Peano -> Bool
eqPeano p p' =
  case (p, p') of
    -- both Zs, resolve True
    (Z, Z) -> True
    -- Both Ss, keep trying
    (S n, S n') -> eqPeano n n'
    -- implicity we have found an S and a Z, False
    _ -> False

addPeano :: Peano -> Peano -> Peano
addPeano Z b = b
addPeano (S a) b = addPeano a (S b)

data List a = Empty | Cons a (List a)

listFoldr :: (a -> b -> b) -> b -> List a -> b
listFoldr _ b Empty = b
listFoldr f b (Cons x xs) = f x $ listFoldr f b xs

toList :: [a] -> List a
toList = foldr Cons Empty

fromList :: List a -> [a]
fromList = listFoldr (:) []

listFoldl :: (b -> a -> b) -> b -> List a -> b
listFoldl _ b Empty = b
listFoldl f b (Cons x xs) = listFoldl f (f b x) xs

listHead :: List a -> Maybe a
listHead Empty = Nothing
listHead (Cons x _) = Just x

listTail :: List a -> List a
listTail Empty = Empty
listTail (Cons _ xs) = xs

listReverse :: List a -> List a
listReverse =
  listFoldl (\acc x -> Cons x acc) Empty

listMap :: (a -> b) -> List a -> List b
listMap _ Empty = Empty
listMap f (Cons x xs) =
  Cons (f x) (listMap f xs)
