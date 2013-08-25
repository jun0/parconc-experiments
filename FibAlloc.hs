import Control.Parallel.Strategies
import System.Environment
import Control.Monad.Par
import Data.List
-- Various combinations of fib and mib, with or without parallelism.  The
-- options with ppss uses the rpar-rpar/rseq-rseq parallelized functions.  For
-- example, -ff runs two copies of fib sequentially while -ffppss runs two
-- copies of fib in parallel with the rpar-rpar/rseq-rseq pattern.

-- Using GHC 7.6.3 on an Ubuntu GNU/Linux 13.04 with Intel Core i5-2557M CPU @
-- 1.70GHz × 4.  Compiled with
--
--   ghc --make -O2 FibAlloc.hs -o seq
--   ghc --make -O2 -threaded FibAlloc.hs -o par
--
-- The running times (wall-clock time) are:
--
--   seq -ff 40			 2.894 s
--   par -ffppss		 1.643 s
--
--   seq -fm 40			 2.739 s
--   par -fmppss		 3.065 s
--
--   seq -mf 40			 2.708 s
--   par -mfppss		 3.154 s
--
--   seq -mm 40			10.706 s
--   par -mmppss		 6.247 s
--
-- -mmppss runs almost twice as fast as -mm
-- -ffppss runs almost twice as fast as -ff
-- -mfppss runs slower than -mf
-- -fmppss runs slower than -fm
--
-- So it seems something goes
main = do
  args <- getArgs
  case args of
    ["-ff", size] -> print (fibfib $ read size)
    ["-mm", size] -> print (mibmib $ read size)
    ["-fm", size] -> print (fibmib $ read size)
    ["-mf", size] -> print (mibfib $ read size)
    ["-ffppss", size] -> print (fibfib_rpar_rpar_rseq_rseq $ read size)
    ["-mmppss", size] -> print (mibmib_rpar_rpar_rseq_rseq $ read size)
    ["-fmppss", size] -> print (fibmib_rpar_rpar_rseq_rseq $ read size)
    ["-mfppss", size] -> print (mibfib_rpar_rpar_rseq_rseq $ read size)

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

-- Like fib, but does gratuitous memory allocation.  GHC 7.6.3 cannot seem to
-- optimize away the list.
mib :: Int -> Int
mib 0 = 0
mib 1 = 1
mib n = foldl' (+) 0 [mib (n-1), mib (n-2)]

-- mib is slower, so running fib n and mib n head-to-head provides for bad
-- workload balancing.  Instead we run fib n and mib (n-mib_handicap).  The
-- suitable size of this handicap depends on your system.
mib_handicap = 3 :: Int


fibfib_rpar_rpar_rseq_rseq :: Int -> (Int,Int)
fibfib_rpar_rpar_rseq_rseq n = runEval $ do
  x <- rpar $ fib n
  y <- rpar $ fib n
  rseq x
  rseq y
  return $ (x,y)

mibmib_rpar_rpar_rseq_rseq :: Int -> (Int,Int)
mibmib_rpar_rpar_rseq_rseq n = runEval $ do
  x <- rpar (mib n)
  y <- rpar (mib n)
  rseq x
  rseq y
  return $ (x,y)

fibmib_rpar_rpar_rseq_rseq :: Int -> (Int,Int)
fibmib_rpar_rpar_rseq_rseq n = runEval $ do
  x <- rpar (fib n)
  y <- rpar (mib (n - mib_handicap))
  rseq x
  rseq y
  return $ (x,y)

mibfib_rpar_rpar_rseq_rseq :: Int -> (Int,Int)
mibfib_rpar_rpar_rseq_rseq n = runEval $ do
  x <- rpar (mib (n - mib_handicap))
  y <- rpar (fib n)
  rseq x
  rseq y
  return $ (x,y)

fibfib :: Int -> (Int,Int)
fibfib n = (fib n, fib n)

mibmib :: Int -> (Int,Int)
mibmib n = (mib n, mib n)

fibmib :: Int -> (Int,Int)
fibmib n = (fib n, mib (n - mib_handicap))

mibfib :: Int -> (Int,Int)
mibfib n = (mib (n - mib_handicap), fib n)

