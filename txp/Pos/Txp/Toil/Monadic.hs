{-# LANGUAGE DeriveFunctor #-}

-- | Some monads used in Toil and primitive actions.

module Pos.Txp.Toil.Monadic
       (
         -- * Monadic Utxo
         UtxoM
       , runUtxoM
       , evalUtxoM
       , execUtxoM
       , utxoGet
       , utxoPut
       , utxoDel

         -- * Monadic local Toil
       , LocalToilState (..)
       , ltsMemPool
       , ltsUtxoModifier
       , ltsUndos
       , LocalToilM
       , hasTx
       , memPoolSize
       , putTxWithUndo

         -- * Monadic global Toil
       , GlobalToilState (..)
       , defGlobalToilState
       , GlobalToilEnv (..)
       , GlobalToilM
       , runGlobalToilM
       , getStake
       , getTotalStake
       , setStake
       , setTotalStake

         -- * Conversions
       , utxoMToLocalToilM
       , utxoMToGlobalToilM
       ) where

import           Universum

import           Control.Lens (at, makeLenses, zoom, (%=), (+=), (.=))
import           Control.Monad.Free (Free (..), foldFree, hoistFree)
import           Control.Monad.Reader (mapReaderT)
import           Control.Monad.State.Strict (mapStateT)
import           Data.Default (def)
import           Fmt ((+|), (|+))
import           System.Wlog (NamedPureLogger, WithLogger, launchNamedPureLog)

import           Pos.Core.Common (Coin, StakeholderId)
import           Pos.Core.Txp (TxAux, TxId, TxIn, TxOutAux, TxUndo)
import           Pos.Txp.Toil.Types (MemPool, StakesView, UndoMap, UtxoLookup, UtxoModifier,
                                     mpLocalTxs, mpSize, svStakes, svTotal)
import qualified Pos.Util.Modifier as MM

----------------------------------------------------------------------------
-- Monadic actions with Utxo.
----------------------------------------------------------------------------

-- | Utility monad which allows to lookup values in UTXO and modify it.
type UtxoM = ReaderT UtxoLookup (State UtxoModifier)

-- | Run 'UtxoM' action using 'UtxoLookup' and 'UtxoModifier'.
runUtxoM :: UtxoModifier -> UtxoLookup -> UtxoM a -> (a, UtxoModifier)
runUtxoM modifier getter = usingState modifier . usingReaderT getter

-- | Version of 'runUtxoM' which discards final state.
evalUtxoM :: UtxoModifier -> UtxoLookup -> UtxoM a -> a
evalUtxoM = fst ... runUtxoM

-- | Version of 'runUtxoM' which discards action's result.
execUtxoM :: UtxoModifier -> UtxoLookup -> UtxoM a -> UtxoModifier
execUtxoM = snd ... runUtxoM

-- | Look up an entry in 'Utxo' considering 'UtxoModifier' stored
-- inside 'State'.
utxoGet :: TxIn -> UtxoM (Maybe TxOutAux)
utxoGet txIn = do
    utxoLookup <- ask
    MM.lookup utxoLookup txIn <$> use identity

-- | Add an unspent output to UTXO. If it's already there, throw an 'error'.
utxoPut :: TxIn -> TxOutAux -> UtxoM ()
utxoPut id txOut = utxoGet id >>= \case
    Nothing -> identity %= MM.insert id txOut
    Just _  ->
        -- TODO [CSL-2173]: Comment
        error ("utxoPut: "+|id|+" is already in utxo")

-- | Delete an unspent input from UTXO. If it's not there, throw an 'error'.
utxoDel :: TxIn -> UtxoM ()
utxoDel id = utxoGet id >>= \case
    Just _  -> identity %= MM.delete id
    Nothing ->
        -- TODO [CSL-2173]: Comment
        error ("utxoDel: "+|id|+" is not in the utxo")

----------------------------------------------------------------------------
-- Monad used for local Toil and some actions.
----------------------------------------------------------------------------

data LocalToilState = LocalToilState
    { _ltsMemPool      :: !MemPool
    , _ltsUtxoModifier :: !UtxoModifier
    , _ltsUndos        :: !UndoMap
    }

makeLenses ''LocalToilState

type LocalToilM = ReaderT UtxoLookup (State LocalToilState)

-- | Check whether Tx with given identifier is stored in the pool.
hasTx :: TxId -> LocalToilM Bool
hasTx id = isJust <$> use (ltsMemPool . mpLocalTxs . at id)

-- | Put a transaction with corresponding 'TxUndo' into MemPool.
-- Transaction must not be in MemPool (but it's checked anyway).
putTxWithUndo :: TxId -> TxAux -> TxUndo -> LocalToilM ()
putTxWithUndo id tx undo =
    unlessM (hasTx id) $ do
        ltsMemPool . mpLocalTxs . at id .= Just tx
        ltsMemPool . mpSize += 1
        ltsUndos . at id .= Just undo

-- | Return the number of transactions contained in the pool.
memPoolSize :: LocalToilM Int
memPoolSize = use $ ltsMemPool . mpSize

----------------------------------------------------------------------------
-- Monad used for global Toil and some actions.
----------------------------------------------------------------------------

-- | Type which parameterizes free monad with access to Stakes.
data StakesLookupF a =
    StakesLookupF StakeholderId
                  (Maybe Coin -> a)
    deriving (Functor)

data GlobalToilState = GlobalToilState
    { _gtsUtxoModifier :: !UtxoModifier
    , _gtsStakesView   :: !StakesView
    }

-- | Default 'GlobalToilState'.
defGlobalToilState :: GlobalToilState
defGlobalToilState =
    GlobalToilState {_gtsUtxoModifier = mempty, _gtsStakesView = def}

makeLenses ''GlobalToilState

data GlobalToilEnv = GlobalToilEnv
    { _gteUtxo       :: !UtxoLookup
    , _gteTotalStake :: !Coin
    }

makeLenses ''GlobalToilEnv

type GlobalToilM
     = NamedPureLogger (ReaderT GlobalToilEnv (StateT GlobalToilState (Free StakesLookupF)))

-- | Run 'GlobalToilM' action in some monad capable of getting 'TxOutAux'
-- for 'TxIn', getting stakeholders' stakes and logging.
runGlobalToilM ::
       forall m a. (WithLogger m)
    => GlobalToilEnv
    -> GlobalToilState
    -> (TxIn -> m (Maybe TxOutAux))
    -> (StakeholderId -> m (Maybe Coin))
    -> GlobalToilM a
    -> m (a, GlobalToilState)
runGlobalToilM env gts utxoGetter stakeGetter = undefined -- launchNamedPureLog hoist'
  -- where
  --   foldFree' :: forall x. Free GlobalLookupF x -> m x
  --   foldFree' =
  --       foldFree $ \case
  --           GlobalLookupUtxo (UtxoLookupF txIn f) -> f <$> utxoGetter txIn
  --           GlobalLookupStakes sId f -> f <$> stakeGetter sId
  --   hoist' ::
  --          forall f. Functor f
  --       => ReaderT GlobalToilEnv (StateT GlobalToilState (Free GlobalLookupF)) (f a)
  --       -> m (f (a, GlobalToilState))
  --   hoist' = shuffle . foldFree' . usingStateT gts . usingReaderT env
  --   shuffle ::
  --          forall f. Functor f
  --       => m (f a, GlobalToilState)
  --       -> m (f (a, GlobalToilState))
  --   shuffle action = do
  --       (fa, st) <- action
  --       return $ (, st) <$> fa

getStake :: StakeholderId -> GlobalToilM (Maybe Coin)
getStake id =
    (<|>) <$> lift (use (gtsStakesView . svStakes . at id)) <*> baseLookup id
  where
    baseLookup :: StakeholderId -> GlobalToilM (Maybe Coin)
    baseLookup i = lift $ lift $ lift $ Free $ StakesLookupF i pure

getTotalStake :: GlobalToilM Coin
getTotalStake =
    lift $ maybe (view gteTotalStake) pure =<< use (gtsStakesView . svTotal)

setStake :: StakeholderId -> Coin -> GlobalToilM ()
setStake id c = lift $ gtsStakesView . svStakes . at id .= Just c

setTotalStake :: Coin -> GlobalToilM ()
setTotalStake c = lift $ gtsStakesView . svTotal .= Just c

----------------------------------------------------------------------------
-- Conversions
----------------------------------------------------------------------------

-- | Lift 'UtxoM' action to 'LocalToilM'.
utxoMToLocalToilM :: UtxoM a -> LocalToilM a
utxoMToLocalToilM = mapReaderT f
  where
    f :: forall a. State UtxoModifier a -> State LocalToilState a
    f = zoom ltsUtxoModifier

-- | Lift 'UtxoM' action to 'GlobalToilM'.
utxoMToGlobalToilM :: UtxoM a -> GlobalToilM a
utxoMToGlobalToilM = undefined
  --   lift . lift . zoom gtsUtxoModifier . mapStateT hoistFree'
  -- where
  --   hoistFree' :: forall a. Free UtxoLookupF a -> Free GlobalLookupF a
  --   hoistFree' = hoistFree GlobalLookupUtxo






----------------------------------------------------------------------------
-- Obsolete
----------------------------------------------------------------------------

-- runToilTGlobal
--     :: (Default ext, Functor m)
--     => ToilT ext m a -> m (a, GenericToilModifier ext)
-- runToilTGlobal txpt = Ether.runStateT' txpt def

-- -- | Run ToilT using empty stakes modifier. Should be used for local
-- -- transaction processing.
-- runToilTLocal
--     :: (Functor m)
--     => UtxoModifier
--     -> MemPool
--     -> UndoMap
--     -> ToilT () m a
--     -> m (a, ToilModifier)
-- runToilTLocal um mp undo txpt =
--     Ether.runStateT' txpt (def {_tmUtxo = um, _tmMemPool = mp, _tmUndos = undo})

-- evalToilTEmpty
--     :: Monad m
--     => ToilT () m a
--     -> m a
-- evalToilTEmpty txpt = Ether.evalStateT txpt def

-- -- | Execute ToilT using empty stakes modifier. Should be used for
-- -- local transaction processing.
-- execToilTLocal
--     :: (Functor m)
--     => UtxoModifier
--     -> MemPool
--     -> UndoMap
--     -> ToilT () m a
--     -> m ToilModifier
-- execToilTLocal um mp undo = fmap snd . runToilTLocal um mp undo

-- -- | Like 'runToilTLocal', but takes extra data as argument.
-- runToilTLocalExtra
--     :: (Functor m)
--     => UtxoModifier
--     -> MemPool
--     -> UndoMap
--     -> extra
--     -> ToilT extra m a
--     -> m (a, GenericToilModifier extra)
-- runToilTLocalExtra um mp undo e =
--     flip Ether.runStateT' $
--         ToilModifier
--         { _tmUtxo = um
--         , _tmStakes = def
--         , _tmMemPool = mp
--         , _tmUndos = undo
--         , _tmExtra = e
--         }
