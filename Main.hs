{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wall #-}
-- | 

module Main where

import Control.Monad.ST
import Data.IORef
import Data.Word
-- import Data.TLS.PThread

-- | A monotinically increasing value visible on any thread.
--   This is a deterministic logical clock.
myClock :: IO Word64
myClock = undefined


data Pedigree = Pedigree { bits :: !Integer
                         , len  :: !Int }
-- We start at the lowest bit and set higher and higher bits without bound.
-- Here we need to store the length because the leading bits may be zero.


-- | Pedigree in a par monad is simply a series of "left/right"
-- choices.
myPedigree :: IO Pedigree
myPedigree = undefined

-- IO-based logical clocks.
--------------------------------------------------------------------------------

-- For this we need to initialize GLOBAL state.

-- | When we uniquely hold the minimum logical clock value, execute an
-- action that is potentially racey or nondeterministic.
--
-- This version is "unsafe" because it does not limit what IO actions
-- can be accomplished with the lock.
unsafeWithLock :: IO a -> IO a
unsafeWithLock = undefined

-- Indexed logical clock values.
--------------------------------------------------------------------------------
                 
-- | An indexed variant of logical clock.
newtype LogicalClock s = LogicalClock (IORef Word64)
    -- ^ TODO: should probably store this with the per-thread
    -- scheduler state.

-- | Here we are protected by the minimum logical clock, which is
-- deterministic, and thus our resulting computation can stay in ST.
withLock :: LogicalClock s -> ST s a -> ST s a
withLock = undefined


-- Gensym API
--------------------------------------------------------------------------------

data Par s a 

fork :: Par s () -> Par s ()
fork = undefined

-- For now this simple keeps track of "where we are at".
newtype AllocatorState = AllocatorState (IORef Word64)

-- | The size of blocks doled out.
blocksize :: Word64
blocksize = 2 ^ (27::Word64) -- About 134M


-- | Return a deterministic, unique value.  The value returned is
-- guaranteed not to caollide with other calls to gensym, past or
-- future, this thread or another thread.
gensym :: Par s Word64
gensym = undefined
   -- First, check if we have a thread-local counter.
   -- If not ask the global allocator for a block.
   -- If so, check if we have exhausted the current block.
   -- If everything looks good, bump the counter and return the value.


-- | Retrieve a reserved block of numbers starting with the value returned.
getBlock :: IO Word64
getBlock =
  -- Increment global counter
  unsafeWithLock undefined
            

main :: IO ()
main = do putStrLn "hi"
          return ()
