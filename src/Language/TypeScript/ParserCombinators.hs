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
import GHC.Stack

instance MonadPlus (Either TsNode) where
    mzero = Left $ TsNode "zero" Nothing Nothing (Loc 0 0)
    mplus (Right x) _ = Right x
    mplus (Left _) y = y

instance Alternative (Either TsNode) where
    empty = mzero
    (<|>) = mplus

type Parser a = StateT [TsNode] (Either TsNode) a

par p = evalStateT p

skip :: HasCallStack => Parser ()
skip = StateT $ \nodes -> case nodes of
        [] -> Left (error "no more nodes")
        (_:res) -> Right ((), res)

skipToken :: HasCallStack => String -> Parser ()
skipToken s = void $ pToken s

eat :: HasCallStack => String -> Parser ()
eat = skipToken

skipKind :: HasCallStack => String -> Parser ()
skipKind s = void $ pKindNode s

pToken :: HasCallStack => String -> Parser String
pToken s = StateT $ \nodes -> case nodes of
    [] -> Left (error "no more nodes")
    (n:res) -> if content n == Just s
                    then Right (s, res)
                    else Left n

eatContent :: HasCallStack => Parser String
eatContent = StateT $ \nodes -> case nodes of
    [] -> Left (error "no more nodes")
    (n:res) -> case content n of
                    Just s -> Right (s, res)
                    Nothing -> Left n

pKindContent :: HasCallStack => String -> Parser String
pKindContent s = do
    n <- pKindNode s
    case content n of
        Just c -> return c
        Nothing -> lift $ Left (error $ "Expected content for kind: " ++ s ++ ", but got: " ++ show n)

pKindNode :: HasCallStack =>  String -> Parser TsNode
pKindNode s = StateT $ \nodes -> case nodes of
    [] -> Left (error "no more nodes")
    (n:res) -> if kind n == s
                    then Right (n, res)
                    else Left (error $ "Expected kind: " ++ s ++ ", but got: " ++ show n)

pChildren :: HasCallStack => Parser [TsNode]
pChildren = StateT $ \nodes -> case nodes of
    [] -> Left (error "no more nodes")
    (n:res) -> case children n of
                    Just ch -> Right (ch, res)
                    Nothing -> error $ "Expected children, but got: " ++ show n

pKindChildren :: HasCallStack => String -> Parser [TsNode]
pKindChildren s = do
    n <- pKindNode s
    case children n of
        Just ch -> return ch
        Nothing -> if s == "SyntaxList"
                        then return []  -- SyntaxList may have no children
                        else error $ "Expected children for kind: " ++ s ++ ", but got: " ++ show n

pushKindChildren :: HasCallStack => String -> Parser ()
pushKindChildren s = do
    ch <- pKindChildren s
    push ch

peek :: HasCallStack => Parser TsNode
peek = StateT $ \nodes -> case nodes of
    [] -> Left (error "no more nodes")
    (n:_) -> Right (n, nodes)

peekKind :: HasCallStack => String -> Parser Bool
peekKind s = do
    n <- peek
    return $ kind n == s

push :: HasCallStack => [TsNode] -> Parser ()
push ns = StateT $ \nodes -> Right ((), ns ++ nodes)

pushNode :: HasCallStack => TsNode -> Parser ()
pushNode n = push [n]

pop :: HasCallStack => Parser TsNode
pop = StateT $ \nodes -> case nodes of
    [] -> Left (error "no more nodes")
    (n:res) -> Right (n, res)

