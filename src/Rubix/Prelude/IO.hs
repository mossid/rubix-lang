module Rubix.Prelude.IO where

--import Rubix.Code (Code(..))

import GHC.Generics

import Data.ByteString

-- data Request = LaunchMissile Int City | NewEntry
-- data Error = NoMissile Int | NotEnoughVote 
-- doSomething :: Request -> IO Error ()
--

type Key = ByteString
type Value = ByteString
type Error = ByteString

data Context = Context { sender :: Address
                       }

type Address = ByteString

data Coin = Coin { denom  :: String
                 , amount :: Int
                 }

data Permission = MintPerm   String -- Denom
                | SpendPerm  String -- Denom
                | ReadPerm   Key 
                | WritePerm  Key 
                | GivePerm   [Permission]
                | CreatePerm
                | DeletePerm
                | Master

type HandlerName = String
type Handler req res = (req -> IO res)

data InternalError = SerializationError
                   | PermissionError HandlerName Permission

class Error a where
    fromInternal :: InternalError -> a

data Encoder = Encoder (Maybe Int) ByteString
             | Fail

class Serial' f where
    encode' :: f p -> Encoder
    decode' :: ByteString -> f p

instance Serial' V1 where
    encode' _ = Encoder { constr = Just 0
                        , result = empty
                        }

instance Serial' U1 where
    encode' U1 = Encoder { constr = Just 0
                         , result = empty
                         }

instance (Serial' f, Serial' g) => Serial' (f :+: g) where
    encode' (L1 x) = case encode' x of Encoder (Just c) r -> Encoder Nothing (fromIntegral c `cons` r)
                                       _                  -> Fail
    encode' (R1 x) = case encode' x of Encoder (Just c) r -> Encoder (Just (c+1)) r
                                       _                  -> Fail
                                

instance (Serial' f, Serial' g) => Serial' (f :*: g) where
    encode' (x :*: y) = case (encode' x, encode' y) of
        (Encoder _ r1, Encoder _ r2) -> r1 `append` r2

instance (Serial c) => Serial' (K1 i c) where
    encode' (K1 x) = undefined

instance (Serial' f) => Serial (M1 i t f) where
    encode' (M1 x) = undefined

data IO a where
    -- Generic key-value storage
    Get     :: Key -> IO Value
    Set     :: Key -> Value -> IO ()
    -- With prefix
    Prefix  :: Key -> IO a -> IO a
    -- Contract call context
    Context :: IO Context
    -- IBC module
    -- IBC ::
    -- Coin module
    Mint    :: Address -> Coin -> IO ()
    Send    :: Address -> Address -> Coin > IO ()
    -- Handler management
    Create  :: Handler req res -> IO () 
    Update  :: Handler req res -> IO ()
    Delete  :: Handler req res -> IO ()
    -- Permission management
    Permit  :: [Permission] -> Handler req res -> IO ()
    Forbid  :: [Permission] -> Handler req res -> IO ()
    -- Calling 
    Call    :: Address -> Handler req res -> req -> IO res
    -- Suicide
    Suicide :: IO ()
    -- Monadic
    Bind    :: IO a -> (a -> IO b) -> IO b
    Return  :: a -> IO a
    Throw   :: Error -> IO ()
    Catch   :: IO a -> (Error -> IO a) -> IO Error a

get :: (Serial k, Serial v) => k -> IO v
get k = decode (Get (encode k))

set :: (Serial k, Serial v) => k -> v -> IO ()
set k v = Set (encode k) (encode v)

prefix :: (Serial k) => k -> IO res -> IO res
prefix k = Prefix (encode k)

context = Context
mint = Mint
send = Send
create = Create
update = Update
delete = Delete
permit = Permit
forbit = Forbid

deploy :: Handler req res -> IO ()
deploy handler = do
    create handler
    permit handler [Master]    

instance Monad (IO a) where
    return = Return
    (>>=)  = Bind

instance Functor (IO a) where
    fmap f x = Bind x $ Return . f











