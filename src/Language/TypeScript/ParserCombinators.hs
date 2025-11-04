{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
module Language.TypeScript.ParserCombinators where

import Language.TypeScript.AST
import Lib
import Data.Maybe (fromMaybe, mapMaybe)
import Data.List (find)
import qualified Data.Text as T
import Data.Maybe
import Debug.Trace
import Data.Either
import Control.Monad.State
import Control.Monad
import Control.Applicative

instance MonadPlus (Either TsNode) where
    mzero = Left $ TsNode "zero" Nothing Nothing (Loc 0 0)
    mplus (Right x) _ = Right x
    mplus (Left _) y = y

instance Alternative (Either TsNode) where
    empty = mzero
    (<|>) = mplus

type Parser a = StateT [TsNode] (Either TsNode) a

par p = evalStateT p

skip :: Parser ()
skip = StateT $ \nodes -> case nodes of
        [] -> Left (error "no more nodes")
        (_:res) -> Right ((), res)

skipToken :: String -> Parser ()
skipToken s = void $ pToken s

eat = skipToken

skipKind :: String -> Parser ()
skipKind s = void $ pKindNode s

pToken :: String -> Parser String
pToken s = StateT $ \nodes -> case nodes of
    [] -> Left (error "no more nodes")
    (n:res) -> if content n == Just s
                    then Right (s, res)
                    else Left n

eatContent :: Parser String
eatContent = StateT $ \nodes -> case nodes of
    [] -> Left (error "no more nodes")
    (n:res) -> case content n of
                    Just s -> Right (s, res)
                    Nothing -> Left n

pKindContent :: String -> Parser String
pKindContent s = do
    n <- pKindNode s
    case content n of
        Just c -> return c
        Nothing -> lift $ Left (error $ "Expected content for kind: " ++ s ++ ", but got: " ++ show n)

pKindNode :: String -> Parser TsNode
pKindNode s = StateT $ \nodes -> case nodes of
    [] -> Left (error "no more nodes")
    (n:res) -> if kind n == s
                    then Right (n, res)
                    else Left (error $ "Expected kind: " ++ s ++ ", but got: " ++ show n)

pChildren :: Parser [TsNode]
pChildren = StateT $ \nodes -> case nodes of
    [] -> Left (error "no more nodes")
    (n:res) -> case children n of
                    Just ch -> Right (ch, res)
                    Nothing -> error $ "Expected children, but got: " ++ show n

pKindChildren :: String -> Parser [TsNode]
pKindChildren s = do
    n <- pKindNode s
    case children n of
        Just ch -> return ch
        Nothing -> error $ "Expected children for kind: " ++ s ++ ", but got: " ++ show n

pushKindChildren :: String -> Parser ()
pushKindChildren s = do
    ch <- pKindChildren s
    push ch

peek :: Parser TsNode
peek = StateT $ \nodes -> case nodes of
    [] -> Left (error "no more nodes")
    (n:_) -> Right (n, nodes)

peekKind :: String -> Parser Bool
peekKind s = do
    n <- peek
    return $ kind n == s

push :: [TsNode] -> Parser ()
push ns = StateT $ \nodes -> Right ((), ns ++ nodes)

pop :: Parser TsNode
pop = StateT $ \nodes -> case nodes of
    [] -> Left (error "no more nodes")
    (n:res) -> Right (n, res)

