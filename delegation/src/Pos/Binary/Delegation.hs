-- | Delegation types serialization.

module Pos.Binary.Delegation
       (
       ) where

import           Universum

import qualified Data.Set as Set

import           Pos.Binary.Class (Bi (..), encodeListLen, enforceSize)
import           Pos.Binary.Core ()
import           Pos.Binary.Crypto ()
import           Pos.Communication.Types.Relay (DataMsg (..))
import           Pos.Core (ProxySKHeavy)
import           Pos.Core.Configuration (HasConfiguration)
import           Pos.Crypto (PublicKey, pskIssuerPk)
import           Pos.Delegation.Types (DlgUndo (..))

instance HasConfiguration => Bi DlgUndo where
    encode DlgUndo{..} =
        encodeListLen 2 <>
        encode duPsks <>
        encode duPrevEpochPosted
    decode = do
        enforceSize "DlgUndo" 2

        (duPsks :: [ProxySKHeavy]) <- decode
        let duPsksSet :: Set PublicKey
            duPsksSet = Set.fromList $ map pskIssuerPk duPsks
        when (length duPsks /= Set.size duPsksSet) $
              fail "DlgUndo.duPsks is not a set: it has duplicates in issuers"

        duPrevEpochPosted <- decode

        pure DlgUndo{..}

instance HasConfiguration => Bi (DataMsg ProxySKHeavy) where
    encode = encode . dmContents
    decode = DataMsg <$> decode
