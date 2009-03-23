module Main where
import Galo

main = show3Dvec (\t -> (t, t, 0)) [-1.0,-0.99 .. 1.0]
