module JobHandler where

import qualified Agent
import Core
import RIO
import qualified Data.Aeson as Aeson

data Job = Job
  { pipeline :: Pipeline,
    state :: JobState,
    info :: CommitInfo
  }
  deriving (Eq, Show)

data JobState
  = JobQueued
  | JobAssigned
  | JobScheduled Build
  deriving (Eq, Show)

data CommitInfo = CommitInfo
  { sha :: Text,
    branch :: Text,
    message :: Text,
    author :: Text,
    repo :: Text
  }
  deriving (Eq, Show, Generic, Aeson.ToJSON)

data Service = Service
  { queueJob :: CommitInfo -> Pipeline -> IO BuildNumber,
    dispatchCmd :: IO (Maybe Agent.Cmd),
    processMsg :: Agent.Msg -> IO (),
    findJob :: BuildNumber -> IO (Maybe Job),
    fetchLogs :: BuildNumber -> StepName -> IO (Maybe ByteString),
    latestJobs :: IO [(BuildNumber, Job)]
  }
