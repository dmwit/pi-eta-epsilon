{-
    BNF Converter: Abstract syntax
    Copyright (C) 2004  Author:  Aarne Ranta

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}

module Language.LBNF.Utils where

import Control.Monad (unless)

infixr 5 +++
infixr 5 ++++
infixr 5 +++++
infixr 2 |||
infixr 5 ...
infixr 3 ***


-- printing operations

a +++ b   = a ++ " "    ++ b
a ++++ b  = a ++ "\n"   ++ b
a +++++ b = a ++ "\n\n" ++ b

prParenth s = if s == "" then "" else "(" ++ s ++ ")"


-- parser combinators a` la Wadler and Hutton

type Parser a b = [a] -> [(b,[a])]

(...) :: Parser a b -> Parser a c -> Parser a (b,c)
(p ... q) s = [((x,y),r) | (x,t) <- p s, (y,r) <- q t]

(|||) :: Parser a b -> Parser a b -> Parser a b
(p ||| q) s = p s ++ q s

lit :: (Eq a) => a -> Parser a a
lit x (c:cs) = [(x,cs) | x == c]
lit _ _ = []

(***) :: Parser a b -> (b -> c) -> Parser a c
(p *** f) s = [(f x,r) | (x,r) <- p s] 

succeed :: b -> Parser a b
succeed v s = [(v,s)]

fails :: Parser a b
fails s = []

-- to get parse results

parseResults :: Parser a b -> [a] -> [b]
parseResults p s = [x | (x,r) <- p s, null r]


-- * List utilities

-- | Replace all occurences of a value by another value
replace :: Eq a => 
	   a -- ^ Value to replace 
	-> a -- ^ Value to replace it with
	-> [a] -> [a]
replace x y xs = [ if z == x then y else z | z <- xs]

-- | Split a list on the first occurence of a value.
--   Does not include the value that was split on in either
--   of the returned lists.
split :: Eq a => a -> [a] -> ([a],[a])
split x xs = let (ys, zs) = break (==x) xs
		 in (ys, drop 1 zs)

-- | Split a list on every occurence of a value.
--   If the value does not occur in the list,
--   the result is the singleton list containing the input list.
--   Thus the returned list is never the empty list.
splitAll :: Eq a => a -> [a] -> [[a]]
splitAll _ [] = [[]]
splitAll x xs = let (ys, zs) = break (==x) xs
		    in ys : case zs of
				    [] -> []
				    _:zs' -> splitAll x zs'


pathSep :: Char
pathSep = '/'

-- | Like the prelude function 'inits' but for path names.
--   For example:
-- > pathInits "foo/bar" = ["foo","foo/bar"]
-- > pathInits "foo/bar/baz.hs" = ["foo","foo/bar","foo/bar/baz.hs"]
pathInits :: String -> [String]
pathInits "" = []
pathInits xs = let (ys,zs) = split pathSep xs
		   in ys : map ((ys ++ [pathSep]) ++) (pathInits zs)

-- | Like basename(1), remove all leading directories from a path name.
basename :: String -> String
basename = last . splitAll pathSep