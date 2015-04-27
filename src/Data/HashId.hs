module Data.HashId
       (
       -- * Core types
         HashId
       , HashEncoder
       , HashOptions
       , Salt

       -- * Core functions
       , defaultOptions
       , mkEncoder
       , encode
       , decode
       , parse
       , toText
       , salt
       ) where

import           Data.HashId.Internal
