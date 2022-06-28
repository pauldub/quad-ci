module Core where

import qualified Codec.Serialise as Serialise
import qualified Data.Aeson as Aeson
import qualified Data.Time.Clock.POSIX as Time
import qualified Docker
import RIO
import qualified RIO.List as List
import qualified RIO.Map as Map
import qualified RIO.NonEmpty as NonEmpty
import qualified RIO.Text as Text

data Pipeline = Pipeline
  {steps :: NonEmpty Step}
  deriving (Eq, Show, Generic, Aeson.FromJSON, Serialise.Serialise)

data Step = Step
  { name :: StepName,
    commands :: NonEmpty Text,
    image :: Docker.Image
  }
  deriving (Eq, Show, Generic, Aeson.FromJSON, Serialise.Serialise)

data Build = Build
  { pipeline :: Pipeline,
    state :: BuildState,
    completedSteps :: Map StepName StepResult,
    volume :: Docker.Volume
  }
  deriving (Eq, Show, Generic, Serialise.Serialise)

data BuildState
  = BuildReady
  | BuildRunning BuildRunningState
  | BuildFinished BuildResult
  deriving (Eq, Show, Generic, Serialise.Serialise)

data BuildRunningState = BuildRunningState
  { step :: StepName,
    containerId :: Docker.ContainerId
  }
  deriving (Eq, Show, Generic, Serialise.Serialise)

data BuildResult
  = BuildSucceeded
  | BuildFailed
  | BuildUnexpectedState Text
  deriving (Eq, Show, Generic, Serialise.Serialise)

newtype BuildNumber = BuildNumber Int
  deriving (Eq, Show, Ord, Generic, Serialise.Serialise)

buildNumberToInt :: BuildNumber -> Int
buildNumberToInt (BuildNumber n) = n

displayBuildNumber :: BuildNumber -> String
displayBuildNumber number = "#" <> show (buildNumberToInt number)

newtype StepName = StepName Text
  deriving (Eq, Show, Ord, Generic, Aeson.FromJSON, Serialise.Serialise)

stepNameToText :: StepName -> Text
stepNameToText (StepName step) = step

data StepResult
  = StepFailed Docker.ContainerExitCode
  | StepSucceeded
  deriving (Eq, Show, Generic, Serialise.Serialise)

progress :: Docker.Service -> Build -> IO Build
progress docker build =
  case build.state of
    BuildReady -> case buildHasNextStep build of
      Left result ->
        pure build{state = BuildFinished result}
      Right step -> do
        let options =
              Docker.CreateContainerOptions
                { image = step.image,
                  script = Text.unlines $ ["set -ex"] <> NonEmpty.toList step.commands,
                  volume = build.volume
                }

        docker.pullImage step.image
        container <- docker.createContainer options
        docker.startContainer container

        let s =
              BuildRunningState
                { step = step.name,
                  containerId = container
                }
        pure build{state = BuildRunning s}
    BuildRunning state -> do
      status <- docker.containerStatus state.containerId

      case status of
        Docker.ContainerRunning -> pure build
        Docker.ContainerExited exitCode -> do
          let result = exitCodeToStepResult exitCode
          pure
            build
              { state = BuildReady,
                completedSteps = Map.insert state.step result build.completedSteps
              }
        Docker.ContainerOther other -> do
          let s = BuildUnexpectedState other
          pure build{state = BuildFinished s}
    BuildFinished _ -> pure build

exitCodeToStepResult :: Docker.ContainerExitCode -> StepResult
exitCodeToStepResult code =
  if Docker.exitCodeToInt code == 0
    then StepSucceeded
    else StepFailed code

buildHasNextStep :: Build -> Either BuildResult Step
buildHasNextStep build =
  if allSucceeded
    then case nextStep of
      Just step -> Right step
      Nothing -> Left BuildSucceeded
    else Left BuildFailed
  where
    allSucceeded = List.all ((==) StepSucceeded) build.completedSteps
    nextStep = List.find f build.pipeline.steps
    f step = not $ Map.member step.name build.completedSteps

type LogCollection = Map StepName CollectionStatus

data CollectionStatus
  = CollectionReady
  | CollectingLogs Docker.ContainerId Time.POSIXTime
  | CollectionFinished
  deriving (Eq, Show)

data Log = Log
  { output :: ByteString,
    step :: StepName
  }
  deriving (Eq, Show, Generic, Serialise.Serialise)

collectLogs ::
  Docker.Service ->
  LogCollection ->
  Build ->
  IO (LogCollection, [Log])
collectLogs docker collection build = do
  now <- Time.getPOSIXTime
  logs <- runCollection docker now collection
  let newCollection = updateCollection build.state now collection
  pure (newCollection, logs)

initLogCollection :: Pipeline -> LogCollection
initLogCollection pipeline =
  Map.fromList $ NonEmpty.toList steps
  where
    steps = pipeline.steps <&> \step -> (step.name, CollectionReady)

updateCollection ::
  BuildState ->
  Time.POSIXTime ->
  LogCollection ->
  LogCollection
updateCollection state lastCollection collection =
  Map.mapWithKey f collection
  where
    update step since nextState =
      case state of
        BuildRunning state ->
          if state.step == step
            then CollectingLogs state.containerId since
            else nextState
        _ -> nextState
    f step = \case
      CollectionReady ->
        update step 0 CollectionReady
      CollectingLogs _ _ ->
        update step lastCollection CollectionFinished
      CollectionFinished -> CollectionFinished

runCollection ::
  Docker.Service ->
  Time.POSIXTime ->
  LogCollection ->
  IO [Log]
runCollection docker collectUntil collection = do
  logs <- Map.traverseWithKey f collection
  pure $ concat (Map.elems logs)
  where
    f step = \case
      CollectionReady -> pure []
      CollectionFinished -> pure []
      CollectingLogs container since -> do
        let options =
              Docker.FetchLogsOptions
                { container = container,
                  since = since,
                  until = collectUntil
                }
        output <- docker.fetchLogs options
        pure [Log {step = step, output = output}]
