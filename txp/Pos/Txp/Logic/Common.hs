-- | Impure functions which are used by both local and global txp.

module Pos.Txp.Logic.Common
       ( buildUtxo
       ) where

import           Universum

import qualified Data.Map as M (fromList)

import           Pos.Core.Txp (Tx (..), TxAux (..), TxIn, TxOutAux)
import           Pos.DB.Class (MonadDBRead)
import qualified Pos.Txp.DB as DB
import           Pos.Txp.Toil (Utxo, UtxoModifier)
import qualified Pos.Util.Modifier as MM

-- | Build base 'Utxo' for given transactions considering given
-- 'UtxoModifier' (can be 'mempty'). Necessary data is taken from the DB.
buildUtxo ::
       forall m. (MonadDBRead m)
    => UtxoModifier
    -> [TxAux]
    -> m Utxo
buildUtxo utxoModifier txs = concatMapM buildForOne txs
  where
    buildForOne :: TxAux -> m Utxo
    buildForOne txAux = do
        let UnsafeTx {..} = taTx txAux
        let utxoLookupM :: TxIn -> m (Maybe (TxIn, TxOutAux))
            utxoLookupM txIn =
                fmap (txIn, ) <$> MM.lookupM DB.getTxOut txIn utxoModifier
        resolvedPairs <- mapM utxoLookupM _txInputs
        return $ M.fromList $ catMaybes $ toList resolvedPairs
