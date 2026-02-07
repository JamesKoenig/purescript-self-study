module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Performance.Minibench (bench)
import Control.Monad.ST (for
                        ,run
                        )
import Control.Monad.ST.Ref (modify
                            ,new
                            ,read
                            )

fibTailRec :: Int -> Int
fibTailRec n = fibh n 0 1
  where fibh :: Int -> Int -> Int -> Int
        fibh 0   acc _    = acc
        fibh rem acc next = fibh (rem-1) next (next+acc)

fibonacciST :: Int -> Int
fibonacciST n = run do
  let init = { acc: 0, next: 1 }
  ref <- new init

  for 0 n \_ ->
    modify (\{acc, next} -> {acc: next,next: next+acc}) ref

  { acc } <- read ref
  pure acc

main :: Effect Unit
main = do
  log "testing fibTailRec 1000"
  bench \_ -> fibTailRec  1000
  log "testing fibonacciST 1000"
  bench \_ -> fibonacciST 1000
  log "done"
