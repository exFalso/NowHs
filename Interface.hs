{-# LANGUAGE TemplateHaskell #-}

module Interface where

import Function
import FunctionCall
import Schema
import SchemaTH
import SchemaTypes
import Message
import NowHs

import Data.Aeson
import Data.Maybe (mapMaybe)

import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Applicative
import Control.Monad (replicateM)
import Control.Monad.Error
import Control.Monad.State as S
import Control.Arrow

import Language.Haskell.TH
import Language.Haskell.TH.Lift as L

data Interface m
    = Interface { interfaceInternal :: FunctionCall -> m Value
                , interfaceExternal :: ([Function], Set.Set AnySchema) }

-- $(interface names) :: MonadNowHs m => Interface m
genInterface :: [Name] -> Q Exp
genInterface namesR = do
    -- uniqify
    let names = Set.toList . Set.fromList $ namesR
    funs <- mapM genFun names
    -- mkName . show NEEDED OTHERWISE WONT BE ABLE TO CALL FROM js!!!
    funPairs <- ListE <$> zipWithM (\n f -> [| (mkName . show $ n,
                                                $(return f)) |]) names funs
    internal <- [| \(FunctionCall nam vals) ->
                  let mp = Map.fromList $(return funPairs) in do
                      case Map.lookup nam mp of
                          Nothing -> liftNowHs $ throwError (NoSuchFunction nam)
                          Just f -> f vals
                 |]
    external <- [| runState (sequence $(ListE <$> mapM funSchemaNames names)) Set.empty |]
    [| Interface { interfaceInternal = $(return internal)
                 , interfaceExternal = $(return external) } |]

-- $(funSchemaNames nam) :: State (Set.Set AnySchema) Function
funSchemaNames :: Name -> Q Exp
funSchemaNames nam = do
    typ <- funType nam
    (args, ret) <- getArgsRet typ
    ((argSchFields, retSchField), argSchNameSet) <- flip runStateT Set.empty $ do
        argFs <- mapM schField args
        retF <- schField ret
        return (argFs, retF)
    let anySchemas = mapM (schemaNam . ConT) $ Set.toList argSchNameSet
    [| do
          let argFs = $(return $ ListE argSchFields)
              retF = $(return retSchField)
          modify (\s -> foldl (flip Set.insert) s $(ListE <$> anySchemas))
          return $ Function { functionName     = nam
                            , functionNameRaw  = nameBase nam
                            , functionArgTypes = argFs
                            , functionRetType  = retF }
     |]

-- $(schField typ) :: SchemaField
schField :: Type -> StateT (Set.Set Name) Q Exp
schField = flip evalStateT [] . deriveSchemaField

anySchemaName :: AnySchema -> Name
anySchemaName (MkAnySchema sch) = schemaName sch

getArgsRet :: Type -> Q ([Type], Type)
getArgsRet (AppT (AppT ArrowT typ) rest) = first (typ :) <$> getArgsRet rest
getArgsRet (AppT _ ret) = return ([], ret) -- _ is the monad
getArgsRet _ = fail "Function has to be monadic with (MonadNowHs m)"

funType :: Name -> Q Type
funType name = do
    info <- reify name
    case info of
        (VarI _ ty _ _) -> return ty
        _ -> fail "Function name expected"

-- $(genFun fun) :: MonadNowHs m => [Value] -> m Value
genFun :: Name -> Q Exp
genFun nam = do
    typ <- funType nam
    (pat, names) <- listPat $ arity typ
    vals <- newName "vals"
    incor <- [| IncorrectNumArgs |]
    (binds, names) <- unzip <$> mapM fJSON names
    let vars = map VarE names
    resNam <- newName "res"
    returnEx <- [| return (toJSON $(return $ VarE resNam)) |]

    return . LamE [VarP vals] $ CaseE (VarE vals)
        [ Match pat (NormalB $ DoE
                     (binds ++ [ BindS (VarP resNam) $
                                 foldl AppE (VarE nam) vars 
                               , NoBindS returnEx
                                                ])) []
        , Match WildP (NormalB $ AppE (VarE 'liftNowHs) $ AppE (VarE 'throwError) incor) []
        ]

fJSON :: Name -> Q (Stmt, Name)
fJSON nam = do
    n <- newName "n"
    ex <- [| case $(return $ AppE (VarE 'fromJSON) (VarE nam)) of
                Error str -> liftNowHs . throwError $ JSONParseError str
                Success v -> return v
           |]
    return $ (BindS (VarP n) ex, n)

listPat :: Int -> Q (Pat, [Name])
listPat num = do
    names <- replicateM num (newName "x")
    return (ListP $ map VarP names, names)

arity :: Type -> Int
arity (AppT (AppT ArrowT _) rest) = succ $ arity rest
arity _ = 0