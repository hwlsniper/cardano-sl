-- | Impure functions which are used by both local and global txp.

module Pos.Txp.Logic.Common
       ( buildUtxoLookup
       ) where

import           Universum

import           Control.Lens (at)
import           Data.Default (Default (def))
import qualified Data.HashMap.Strict as HM
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M (fromList)
import           Formatting (build, sformat, (%))
import           System.Wlog (NamedPureLogger, WithLogger, logDebug, logError, logWarning)

import           Pos.Core (BlockVersionData, EpochIndex, HeaderHash, siEpoch)
import           Pos.Core.Txp (Tx (..), TxAux (..), TxId, TxUndo)
import           Pos.Crypto (WithHash (..))
import           Pos.DB.Class (MonadDBRead, MonadGState (..))
import qualified Pos.DB.GState.Common as GS
import           Pos.Reporting (reportError)
import           Pos.Slotting (MonadSlots (..))
import           Pos.StateLock (Priority (..), StateLock, StateLockMetrics, withStateLock)
import qualified Pos.Txp.DB as DB
import           Pos.Txp.MemState (GenericTxpLocalData (..), GenericTxpLocalDataPure, MempoolExt,
                                   MonadTxpMem, TxpLocalWorkMode, askTxpMem, getLocalTxsMap,
                                   getUtxoModifier, modifyTxpLocalData, setTxpLocalData)
import           Pos.Txp.Toil (LocalToilM, LocalToilState (..), ToilVerFailure (..), Utxo,
                               UtxoLookup, UtxoModifier, mpLocalTxs, normalizeToil, processTx,
                               utxoToLookup)
import           Pos.Txp.Topsort (topsortTxs)
import qualified Pos.Util.Modifier as MM

-- | Build base 'UtxoLookup' for given transactions considering given
-- 'UtxoModifier' (can be 'mempty'). Necessary data is taken from the DB.
buildUtxoLookup ::
       forall m ctx. (MonadIO m, MonadDBRead m)
    => UtxoModifier
    -> [TxAux]
    -> m UtxoLookup
buildUtxoLookup utxoModifier txs = do
    utxo <- concatMapM buildForOne txs
    return (utxoToLookup utxo)
  where
    buildForOne :: TxAux -> m Utxo
    buildForOne txAux = do
        let UnsafeTx {..} = taTx txAux
        let utxoLookupM txIn = MM.lookupM DB.getTxOut txIn utxoModifier
        resolvedOuts <- mapM utxoLookupM _txInputs
        return $
            M.fromList $
            catMaybes $
            toList $ NE.zipWith (liftM2 (,) . Just) _txInputs resolvedOuts
