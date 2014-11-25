module Mappings where

import Data.Complex
import Data.List

import ComplexShapes

diskAutomorphism :: Complex Double -> Angle -> Complex Double -> Complex Double
diskAutomorphism zerod angle z = (mkPolar 1 angle)*(z - zerod)/(1 - (conjugate zerod)*z)

adaptiveInterval :: Tolerance
                 -> (Complex Double -> Complex Double)
	 	 -> Complex Double -> Complex Double
		 -> Complex Double -> Complex Double -> Polygon
adaptiveInterval tol f a b fa fb = if norm (fab - fafb) < tol
				   then [fa, fab, fb]
		 		   else (adaptiveInterval tol f a mid fa fab)
				     ++ (adaptiveInterval tol f mid b fab fb)
  where
    mid = (a+b)/(2.0 :+ 0.0)
    fab = f mid
    fafb = (fa + fb)/(2.0 :+ 0.0)

adaptiveMapping :: Tolerance
                -> (Complex Double -> Complex Double)
		-> Polygon -> Polygon
adaptiveMapping tol f poly = let mapInterval (a,b,fa,fb) = adaptiveInterval tol f a b fa fb
				 ypol = (last poly):(init poly)
				 listData = zip4 poly ypol (map f poly) (map f ypol)
		             in concatMap mapInterval listData

applyAdaptiveSqrt :: Spacing -> Complex Double -> Complex Double -> Complex Double -> [Complex Double]
applyAdaptiveSqrt step a b sqrta = if norm (sqrta - branchedSqrt) < step
				   then [sqrta, branchedSqrt]
				   else (applyAdaptiveSqrt step a mid sqrta)
				     ++ (applyAdaptiveSqrt step mid b branchedSqrt)
  where
    mid = (a + b)/(2.0 :+ 0.0)
    unBranchedSqrt = sqrt b
    distplus = norm (unBranchedSqrt - sqrta)
    distminus = norm (unBranchedSqrt + sqrta)
    branchedSqrt = if distplus < distminus then unBranchedSqrt else (-unBranchedSqrt)

adaptiveSqrt :: Spacing -> Complex Double -> Polygon -> Polygon
adaptiveSqrt step sqrtinit (p:q:ps) = ss ++ (adaptiveSqrt step (last ss) (q:ps))
  where 
    ss = applyAdaptiveSqrt step p q sqrtinit
adaptiveSqrt _ sqrtinit (p:[]) = if dist then [unbsqrt] else [(-unbsqrt)]
  where
    unbsqrt = sqrt p
    dist = norm (unbsqrt - sqrtinit) > norm (unbsqrt + sqrtinit)
adaptiveSqrt _ _ [] = []
