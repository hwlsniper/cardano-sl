{-# LANGUAGE TypeOperators #-}

-- | Logic for global processing of transactions.
-- Global transaction is a transaction which has already been added to the blockchain.

module Pos.Txp.Logic.Global
       ( txpGlobalSettings

       -- * Helpers
       , ApplyBlocksSettings (..)
       -- , applyBlocksWith
       -- , blundToAuxNUndo
       -- , genericGlobalToilStateToBatch
       -- , runToilAction
       ) where

import           Universum

import           Data.Default (Default)
import qualified Data.HashMap.Strict as HM
import qualified Data.List.NonEmpty as NE
import           Formatting (build, sformat, (%))

import           Pos.Core.Block.Union (ComponentBlock (..))
import           Pos.Core.Class (epochIndexL)
import           Pos.Core.Configuration (HasConfiguration)
import           Pos.Core.Txp (TxAux, TxUndo, TxpUndo)
import           Pos.DB (MonadDBRead, SomeBatchOp (..))
import           Pos.DB.Class (gsAdoptedBVData)
import qualified Pos.DB.GState.Stakes as DB
import           Pos.Exception (assertionFailed)
import           Pos.Txp.Base (flattenTxPayload)
import qualified Pos.Txp.DB as DB
import           Pos.Txp.Settings.Global (TxpBlock, TxpBlund, TxpGlobalApplyMode,
                                          TxpGlobalRollbackMode, TxpGlobalSettings (..),
                                          TxpGlobalVerifyMode)
import           Pos.Txp.Toil (GlobalToilEnv (..), GlobalToilM, GlobalToilState (..),
                               StakesView (..), ToilVerFailure, UtxoM, applyToil,
                               defGlobalToilState, rollbackToil, runGlobalToilM, verifyToil)
import           Pos.Util.AssertMode (inAssertMode)
import           Pos.Util.Chrono (NE, NewestFirst (..), OldestFirst (..))
import qualified Pos.Util.Modifier as MM



-- | Settings used for global transactions data processing used by a
-- simple full node.
txpGlobalSettings :: TxpGlobalSettings
txpGlobalSettings =
    TxpGlobalSettings
    { tgsVerifyBlocks = verifyBlocks
    , tgsApplyBlocks = undefined -- applyBlocksWith applyBlocksSettings
    , tgsRollbackBlocks = rollbackBlocks
    }

verifyBlocks ::
       forall m. TxpGlobalVerifyMode m
    => Bool
    -> OldestFirst NE TxpBlock
    -> m $ Either ToilVerFailure $ OldestFirst NE TxpUndo
verifyBlocks verifyAllIsKnown newChain = undefined
  --   runExceptT $ do
  --       bvd <- gsAdoptedBVData
  --       let verifyPure :: TxpBlock -> UtxoM (Either ToilVerFailure TxpUndo)
  --           verifyPure =
  --               runExceptT .
  --               verifyToil bvd epoch verifyAllIsKnown . convertPayload
  --           verifyDo :: TxpBlock -> m (Either ToilVerFailure TxpUndo)
  --           verifyDo = evalUtxoM mempty DB.getTxOut . verifyPure
  --       mapM (ExceptT . verifyDo) newChain
  -- where
  --   epoch = NE.last (getOldestFirst newChain) ^. epochIndexL
  --   convertPayload :: TxpBlock -> [TxAux]
  --   convertPayload (ComponentBlockMain _ payload) = flattenTxPayload payload
  --   convertPayload (ComponentBlockGenesis _)      = []

data ApplyBlocksSettings extra m = ApplyBlocksSettings
    { absApplySingle     :: TxpBlund -> m ()
    , absExtraOperations :: extra -> SomeBatchOp
    }

-- applyBlocksSettings
--     :: GlobalApplyToilMode m
--     => ApplyBlocksSettings () m
-- applyBlocksSettings =
--     ApplyBlocksSettings
--     { absApplySingle = applyToil . blundToAuxNUndo
--     , absExtraOperations = const mempty
--     }

-- applyBlocksWith
--     :: (TxpGlobalApplyMode ctx m, Default extra)
--     => ApplyBlocksSettings extra (ToilT extra (DBToil m))
--     -> OldestFirst NE TxpBlund
--     -> m SomeBatchOp
-- applyBlocksWith ApplyBlocksSettings {..} blunds = do
--     let blocks = map fst blunds
--     inAssertMode $ do
--         verdict <- verifyBlocks False blocks
--         whenLeft verdict $
--             assertionFailed .
--             sformat ("we are trying to apply txp blocks which we fail to verify: "%build)
--     genericToilModifierToBatch absExtraOperations . snd <$>
--         runToilAction (mapM absApplySingle blunds)

rollbackBlocks ::
       forall m. TxpGlobalRollbackMode m
    => NewestFirst NE TxpBlund
    -> m SomeBatchOp
rollbackBlocks blunds = undefined
  -- convert <$> do
  --   totalStake <- DB.getRealTotalStake
  --   let env = GlobalToilEnv totalStake
  --       rollbackPure :: TxpBlund -> GlobalToilM ()
  --       rollbackPure = rollbackToil . blundToAuxNUndo
  --       rollbackDo :: GlobalToilState -> TxpBlund -> m GlobalToilState
  --       rollbackDo gts =
  --           fmap snd .
  --           runGlobalToilM env gts DB.getTxOut DB.getRealStake .
  --           rollbackPure
  --   foldM rollbackDo defGlobalToilState blunds
  -- where
  --   convert :: GlobalToilState -> SomeBatchOp
  --   convert = undefined

-- toilModifierToBatch . snd <$>
-- runToilAction (mapM (rollbackToil . blundToAuxNUndo) blunds)

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

-- | Convert 'GenericToilModifier' to batch of database operations.
genericGlobalToilStateToBatch :: HasConfiguration
                           => (e -> SomeBatchOp)
                           -> GlobalToilState
                           -> SomeBatchOp
genericGlobalToilStateToBatch = undefined
-- genericToilModifierToBatch convertExtra modifier =
--     SomeBatchOp (extraOp : [SomeBatchOp utxoOps, SomeBatchOp stakesOps])
--   where
--     ToilModifier
--         { _tmUtxo = um
--         , _tmStakes = (StakesView (HM.toList -> stakes) total)
--         , _tmExtra = extra
--         } = modifier
--     utxoOps =
--         map DB.DelTxIn (MM.deletions um) ++
--         map (uncurry DB.AddTxOut) (MM.insertions um)
--     stakesOpsAlmost = map (uncurry DB.PutFtsStake) stakes
--     stakesOps =
--         case total of
--             Nothing -> stakesOpsAlmost
--             Just x  -> DB.PutTotalStake x : stakesOpsAlmost
--     extraOp = convertExtra extra

-- | Convert simple 'GlobalToilState' to batch of database operations.
toilModifierToBatch :: HasConfiguration => GlobalToilState -> SomeBatchOp
toilModifierToBatch = genericGlobalToilStateToBatch (const mempty)

-- -- | Run action which requires toil interfaces.
-- runToilAction
--     :: (MonadDBRead m, Default e)
--     => ToilT e (DBToil m) a -> m (a, GenericToilModifier e)
-- runToilAction action = runDBToil . runToilTGlobal $ action

-- Zip block's TxAuxes and corresponding TxUndos.
blundToAuxNUndo :: TxpBlund -> [(TxAux, TxUndo)]
blundToAuxNUndo (ComponentBlockGenesis _ , _)        = []
blundToAuxNUndo (ComponentBlockMain _ payload, undo) = zip (flattenTxPayload payload) undo
