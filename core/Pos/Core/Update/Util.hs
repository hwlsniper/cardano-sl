-- | Utility functions related to core update system types.

module Pos.Core.Update.Util
       (
       -- * Smart constructors
         mkUpdateProposalWSign
       , mkVoteId
       , mkUpdateProof

       -- * Formatters
       , softforkRuleF

       -- * System tag helpers
       , archHelper
       , osHelper
       ) where

import           Universum

import qualified Data.HashMap.Strict as HM
import           Distribution.System (Arch (..), OS (..))
import           Distribution.Text (display)
import           Formatting (Format, build)
import           Instances.TH.Lift ()

import           Pos.Binary.Class (Bi)
import           Pos.Binary.Crypto ()
import           Pos.Core.Configuration (HasConfiguration)
import           Pos.Core.Update.Types (BlockVersion, BlockVersionModifier (..), SoftforkRule (..),
                                        SoftwareVersion, SystemTag, UpAttributes, UpdateData,
                                        UpdatePayload (..), UpdateProof, UpdateProposal (..),
                                        UpdateProposalToSign (..), UpdateVote (..), VoteId)
import           Pos.Crypto (HasCryptoConfiguration, SafeSigner,
                             SignTag (SignUSProposal, SignUSVote), checkSig, hash, safeSign,
                             safeToPublic)
import           Pos.Util.Verification (PVerifiable (..), pverFail, pverField)

----------------------------------------------------------------------------
-- Verification
----------------------------------------------------------------------------

instance PVerifiable SoftforkRule where
    pverify SoftforkRule{..} = do
        pverify srInitThd
        pverify srMinThd
        pverify srThdDecrement

instance PVerifiable BlockVersionModifier where
    pverify BlockVersionModifier{..} = do
        whenJust bvmMpcThd $ pverField "mpcThd" . pverify
        whenJust bvmHeavyDelThd $ pverField "heavyDlgThd" . pverify
        whenJust bvmUpdateVoteThd $  pverField "updateVoteThd" . pverify
        whenJust bvmUpdateProposalThd $ pverField "updateProposalThd" . pverify
        whenJust bvmSoftforkRule $ pverField "softforkRule" . pverify

instance HasCryptoConfiguration => PVerifiable UpdateVote where
    pverifyOne it = do
        let sigValid = checkSig SignUSVote
                                (uvKey it)
                                (uvProposalId it, uvDecision it)
                                (uvSignature it)
        unless sigValid $ pverFail "UpdateVote: invalid signature"

instance (HasCryptoConfiguration, Bi UpdateProposalToSign) => PVerifiable UpdateProposal where
    pverifyOne UnsafeUpdateProposal{..} = do
        let toSign = UpdateProposalToSign
                         upBlockVersion
                         upBlockVersionMod
                         upSoftwareVersion
                         upData
                         upAttributes
        unless (checkSig SignUSProposal upFrom toSign upSignature)
               (pverFail "UpdateProposal: invalid signature")
    pverify UnsafeUpdateProposal{..} = do
        pverField "blockVersionMod" $ pverify upBlockVersionMod
        pverField "upSoftwareVersion" $ pverify upSoftwareVersion
        forM_ (HM.keys upData) $ pverField "upData" . pverify

instance (HasCryptoConfiguration, Bi UpdateProposalToSign) => PVerifiable UpdatePayload where
    pverify it = do
        whenJust (upProposal it) $ pverField "upProposal" . pverify
        forM_ (upVotes it) $ pverField "upVotes.elem" . pverify

----------------------------------------------------------------------------
-- Utilities/creation
----------------------------------------------------------------------------

-- | 'SoftforkRule' formatter which restricts type.
softforkRuleF :: Format r (SoftforkRule -> r)
softforkRuleF = build

mkUpdateProposalWSign
    :: (HasConfiguration, Bi UpdateProposalToSign)
    => BlockVersion
    -> BlockVersionModifier
    -> SoftwareVersion
    -> HM.HashMap SystemTag UpdateData
    -> UpAttributes
    -> SafeSigner
    -> UpdateProposal
mkUpdateProposalWSign upBlockVersion upBlockVersionMod upSoftwareVersion upData upAttributes ss =
    UnsafeUpdateProposal {..}
  where
    toSign =
        UpdateProposalToSign
            upBlockVersion
            upBlockVersionMod
            upSoftwareVersion
            upData
            upAttributes
    upFrom = safeToPublic ss
    upSignature = safeSign SignUSProposal ss toSign

mkVoteId :: UpdateVote -> VoteId
mkVoteId vote = (uvProposalId vote, uvKey vote, uvDecision vote)

mkUpdateProof
    :: Bi UpdatePayload
    => UpdatePayload -> UpdateProof
mkUpdateProof = hash

-- | Helper to turn an @OS@ into a @String@ compatible with the @systemTag@ previously
-- used in 'configuration.yaml'.
osHelper :: OS -> String
osHelper sys = case sys of
    Windows -> "win"
    OSX     -> "macos"
    Linux   -> "linux"
    _       -> display sys

-- | Helper to turn an @Arch@ into a @String@ compatible with the @systemTag@ previously
-- used in 'configuration.yaml'.
archHelper :: Arch -> String
archHelper archt = case archt of
    I386   -> "32"
    X86_64 -> "64"
    _      -> display archt
