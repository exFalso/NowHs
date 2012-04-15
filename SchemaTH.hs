{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell, FlexibleInstances, MonadComprehensions #-}

module SchemaTH where

import Schema
import SchemaTypes

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.State as S
import Control.Applicative

deriveSchema :: Name -> Q [Dec]
deriveSchema n = return <$> deriveSchemaOne n

deriveSchemaOne :: Name -> Q Dec
deriveSchemaOne t = do
    info <- reify t
    case info of
        TyConI (DataD _ n tyVars [c] _) -> deriveSchema' n tyVars c
        TyConI (NewtypeD _ n tyVars c _) -> deriveSchema' n tyVars c
        TyConI DataD{}                  -> fail "Disjunctions not supported yet"
        _                               -> fail "deriveSchema expects a type name"
  where
    deriveSchema' n tyVars c = do
        (schFs, cx) <- flip evalStateT Set.empty $ runStateT (deriveSchemaC c) []
        sch <- [| Schema n $(return schFs) |]
        let typ = AppT (ConT ''HasSchema) (applT n tyVars)
        return $ InstanceD cx typ [FunD 'schema [Clause [] (NormalB sch) []]]


applT :: Name -> [TyVarBndr] -> Type
applT nam = foldl AppT (ConT nam) . map tyVarToType

tyVarToType :: TyVarBndr -> Type
tyVarToType (PlainTV n) = VarT n
tyVarToType (KindedTV n k) = SigT (VarT n) k

deriveSchemaC :: Con -> SchemaM Exp
deriveSchemaC (RecC _ typs) = do
    ListE <$> mapM deriveSchemaT typs
deriveSchemaC _ = fail "Use record syntax"

deriveSchemaT :: (Name, Strict, Type) -> SchemaM Exp
deriveSchemaT (Name n _, _, t) = do
    let fieldName = occString n
    field <- deriveSchemaField t
    S.lift . S.lift $ [| (fieldName, $(return field)) |]

deriveSchemaField :: Type -> SchemaM Exp
deriveSchemaField t = case t of
    VarT v              -> do
        polyTyp <- deriveSchemaPolyTyp v
        S.lift . S.lift $ [| SchemaField Required $(return polyTyp) |]
    ConT c              ->
        S.lift $ do
          schTyp <- nameToSchTyp c
          S.lift $ [| SchemaField Required schTyp |]
    AppT ListT ty       -> do
        schTyp <- deriveSchemaTyp ty
        S.lift . S.lift $ [| SchemaField Repeated $(return schTyp) |]
    AppT (ConT c) ty
        | Set.member c setRepTyp -> do
            schTyp <- deriveSchemaTyp ty
            S.lift . S.lift $ [| SchemaField Repeated $(return schTyp) |]
        | c == ''Maybe -> do
            schTyp <- deriveSchemaTyp ty
            S.lift . S.lift $ [| SchemaField Optional $(return schTyp) |]
    _                   -> fail "Cannot derive schema for this type"

deriveSchemaPolyTyp :: Name -> SchemaM Exp
deriveSchemaPolyTyp v = do
    let prd ty = ClassP ''HasSchema [ty]
    modify (prd (VarT v) :) -- add type var to cxt
        -- (schemaName (schema :: Schema type))
    S.lift . S.lift $ [| SchemaName (schemaName (schema :: Schema $(return $ VarT v))) |]

-- can only derive name for typevars and ground constructors
deriveSchemaTyp :: Type -> SchemaM Exp
deriveSchemaTyp t = case t of
    VarT v -> deriveSchemaPolyTyp v
    ConT c -> S.lift $ do
        schTyp <- nameToSchTyp c
        S.lift [| schTyp |]
    _ -> fail "Type too compound for schemas"

nameToSchTyp :: Name -> StateT (Set.Set Name) Q SchemaType
nameToSchTyp nam = case Map.lookup nam mapNamePrimTyp of
    Nothing -> do
        modify (Set.insert nam)
        return (SchemaName nam)
    Just p -> return (Prim p)

-- SchemaM holds the context and the names of the schemas used
type SchemaM a = StateT Cxt (StateT (Set.Set Name) Q) a

-- gives the expression for a type's schema
-- $(schemaNam t) :: AnySchema
schemaNam :: Type -> Q Exp
schemaNam t = [| MkAnySchema (schema :: Schema $(return t)) |]
