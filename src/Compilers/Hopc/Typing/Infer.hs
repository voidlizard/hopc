module Compilers.Hopc.Typing.Infer where

import Compilers.Hopc.Compile
import Compilers.Hopc.Error
import Compilers.Hopc.Typing.Types

import Data.Maybe
import Data.Either
import Control.Monad

import Control.Monad.Error

import Debug.Trace

data InferError a = Occurs a a | NotMatch a a deriving (Show)

{-

let sub x y rs = List.map (fun (a,b) -> ((T.substitute x y a), (T.substitute x y b)) ) rs

let unify constr =
    let not_occurs a b f = if not (T.occurs (T.id_of a) b) then f () else raise Occurs
    in let rec uni = function
      | [] -> []
      | (a, b)::rest when a = b -> uni rest
      | (a, b)::rest when (T.isvar a) ->  not_occurs a b (fun () -> uni (sub (T.id_of a) b rest) @ [(a, b)] )
      | (a, b)::rest when (not (T.isvar a)) && T.isvar b -> uni ((b, a)::rest)
      | ((x,y) as z)::rest -> uni (T.merge_constraints z @ rest )
       in uni constr |> uni

-}

inferM :: (Eq a, TType a) => [(a, a)] -> CompileM [(a, a)]
inferM constr = withError $ infer constr
    where withError (Right x) = return x
          withError (Left x)  = throwError TypingError -- FIXME: more error handling


infer :: (Eq a, TType a) => [(a, a)] -> Either (InferError a) [(a, a)]
infer = uni . unify 

unify :: (Eq a, TType a) => [(a, a)] -> Either (InferError a) [(a, a)]
unify x = uni $ Right x

uni :: (Eq a, TType a) => Either (InferError a) [(a, a)] -> Either (InferError a) [(a, a)]

uni (Right []) = Right []
uni (Right ((a,b):rs)) | a == b = uni $ Right rs
                       | isVar a = if occurs (typeid a) b
                                     then Left (Occurs a b)
                                     else result (a, b) (uni $ Right $ map (sub (typeid a) b) rs)
                       | (not.isVar) a && isVar b = uni $ Right ((b,a):rs)
                       | otherwise = case merge a b of
                                        Nothing  -> Left $ NotMatch a b
                                        (Just l) -> uni $ Right $ l ++ rs

uni (Left x) = Left x

sub ta b (x, y) = (subst ta b x, subst ta b y)

result (a, b) (Left x)  = Left x
result (a, b) (Right x) = Right $ x ++ [(a, b)]

