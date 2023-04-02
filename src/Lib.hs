{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DeriveGeneric #-}

module Lib where

import qualified Data.Aeson as A
import Data.Aeson
import GHC.Generics (Generic)
import qualified Data.Map as M
import qualified Data.HashMap as HM
import Data.Time
import Data.Scientific
import qualified Data.Text as T
import GHC.Float (divideDouble)

data RateCardEntry = RateCardEntry
  { title :: String,
    value :: String,
    label :: String 
  } deriving (Generic, Show, FromJSON, ToJSON)

data ConstantTypes = NUMBER | TIME_OF_DAY | BOOL deriving (Generic, Show, FromJSON, ToJSON)

data CalculatedValues = NUMBER_ Double | TIME_OF_DAY_ TimeOfDay | BOOL_ Bool deriving (Show, Eq, Ord)

data FareConstants = FareConstants
  { constType :: ConstantTypes        
  , _value :: A.Value 
  } deriving (Generic, Show)

instance FromJSON FareConstants where 
  parseJSON = withObject "FareConstants" $ \obj -> do
   FareConstants
      <$> obj .: "type"
      <*> obj .: "value"

instance ToJSON FareConstants where 
  toJSON fc = 
    A.Object $
      "type" .= constType fc 
        <> "value" .= _value fc

data FareParameters = DISTANCE | CURRENT_TIME deriving (Generic, Show, FromJSON, ToJSON)
  
data FareOperators = IF | DIFF | MIN | MAX | MULTIPLY | DIVIDE | IF_TIME_BETWEEN | LESS_THAN | GREATER_THAN | ADD deriving (Show) 

instance FromJSON FareOperators where 
  parseJSON = withText "FareOperators" $ \s -> do
    case T.unpack s of
      "#if"            -> pure IF  
      "#diff"          -> pure DIFF
      "#max"           -> pure MAX 
      "#min"           -> pure MIN 
      "#multiply"      -> pure MULTIPLY 
      "#div"           -> pure DIVIDE
      "#ifTimeBetween" -> pure IF_TIME_BETWEEN 
      "#lessThan"      -> pure LESS_THAN 
      "#greaterThan"   -> pure GREATER_THAN 
      "#add"           -> pure ADD 
      _                -> fail "Unable to parse FareOperators"
 
instance ToJSON FareOperators where 
  toJSON a = String . T.pack $  case a of
    IF              -> "#if"
    DIFF            -> "#diff"
    MAX             -> "#max"
    MIN             -> "#min"
    MULTIPLY        -> "#multiply"
    DIVIDE          -> "#div"
    IF_TIME_BETWEEN -> "#ifTimeBetween"
    LESS_THAN       -> "#lessThan"
    GREATER_THAN    -> "#greaterThan"
    ADD             -> "#add"


data FareVariables = FareVariables
  { _type :: ConstantTypes
  , polyfill :: Maybe FareParameters 
  , operand :: [String]
  , operator :: FareOperators
  } deriving (Generic, Show)

instance ToJSON FareVariables where 
  toJSON fv = 
    A.Object $
      "type" .= _type fv 
        <> "polyfill" .= polyfill fv
        <> "operand" .= operand fv
        <> "operator" .= operator fv


instance FromJSON FareVariables where 
 parseJSON = withObject "FareVariables" $ \obj -> do
   FareVariables
      <$> obj .: "type"
      <*> obj .:? "polyfill"
      <*> obj .: "operand"
      <*> obj .: "operator"

type Constants = M.Map String FareConstants
type ConstantsHM = HM.Map String FareConstants
type Variables = M.Map String FareVariables
type VariablesHM = HM.Map String FareVariables

data FareConfig = FareConfig 
  { constants :: Constants 
  , variables :: Variables
  , finalFare :: String 
  , rateCardInfo :: [RateCardEntry]
  } deriving (Generic, Show, FromJSON, ToJSON)

calculate :: String -> ConstantsHM -> VariablesHM -> Int -> UTCTime -> CalculatedValues
calculate finalParam _constants _variables distance now = do 
  go (HM.lookup finalParam _variables) 
    where 
      getValueOrCalculate varKeyStr = 
        case HM.lookup varKeyStr _constants of 
          Just val -> do 
            let setVal = _value val
            case constType val of 
              NUMBER -> 
                NUMBER_ $ case setVal of 
                  A.Number num -> toRealFloat num
                  _ -> error $ "expected Num type got: " <> show setVal
              TIME_OF_DAY -> 
                TIME_OF_DAY_ $ case setVal of 
                  A.String timeOfDay -> read $ T.unpack timeOfDay
                  _ -> error $ "expected TimeOfDay type got: " <> show setVal
              BOOL -> 
                BOOL_ $ case setVal of
                  A.Bool b -> b 
                  _ -> False
          Nothing -> 
            case varKeyStr of 
              "{#DISTANCE#}" -> NUMBER_ $ fromIntegral distance
              "{#CURRENT_TIME#}" -> TIME_OF_DAY_ $ localTimeOfDay $ utcToLocalTime timeZoneIST now
              _ -> calculate varKeyStr _constants _variables distance now

      maxNum :: Ord a => [a] -> a
      maxNum = foldr1 (\x y ->if x >= y then x else y)

      minNum :: Ord a => [a] -> a
      minNum = foldr1 (\x y ->(if x <= y then x else y))

      go :: Maybe FareVariables -> CalculatedValues
      go Nothing = error $ "Value: " <> finalParam <> " not in variables"
      go (Just fareVariables) = do
        let operandValues = map getValueOrCalculate $ operand fareVariables
        case operator fareVariables of
          IF           -> case operandValues of 
                            [boolVar, trueVar, falseVar] -> 
                              if boolVar == BOOL_ True 
                                then trueVar
                                else falseVar
                            _  -> error $ "Got wrong parameters for #if: " <> show operandValues
          DIFF         -> NUMBER_ $ case toIntList operandValues of 
                            [a, b] -> a - b
                            _ -> error $ "Got wrong parameters for #diff: " <> show operandValues
          MIN          -> NUMBER_ $ case toIntList operandValues of 
                                   [] -> error "empty array passed for #max operator"
                                   nonEmptyList -> minNum nonEmptyList
          MAX          -> NUMBER_ $ case toIntList operandValues of 
                                   [] -> error "empty array passed for #max operator"
                                   nonEmptyList -> maxNum nonEmptyList
          MULTIPLY     -> NUMBER_ . product $ toIntList operandValues
          DIVIDE       -> NUMBER_ $ case toIntList operandValues of 
                                      [a, b] -> divideDouble a b
                                      _ -> error $ "wrong number of arguments for #div: " <> show operandValues
          ADD          -> NUMBER_ . sum $ toIntList operandValues
          GREATER_THAN -> BOOL_ $ case operandValues of 
                                  [a, b] -> a > b
                                  _ -> error $ "wrong number of arguments for #lessThan: " <> show operandValues
          LESS_THAN    -> BOOL_ $ case operandValues of 
                                    [a, b] -> a < b
                                    _ -> error $ "wrong number of arguments for #lessThan: " <> show operandValues
          IF_TIME_BETWEEN   -> BOOL_ $ case operandValues of 
                                    [TIME_OF_DAY_ startTime, TIME_OF_DAY_ time, TIME_OF_DAY_ endTime] -> isTimeWithinBounds startTime endTime time
                                    _ -> error $ "Got wrong parameters for #ifTimeBetween: " <> show operandValues

timeZoneIST :: TimeZone
timeZoneIST = minutesToTimeZone 330 -- TODO: Should be configurable. Hardcoded to IST +0530

toIntList :: [CalculatedValues] -> [Double]
toIntList cvs = do 
  map 
    (\cv -> 
      case cv of 
        NUMBER_ val -> val 
        _ -> error $ "expected Number type got: " <> show cv
    ) 
    cvs


isTimeWithinBounds :: TimeOfDay -> TimeOfDay -> TimeOfDay -> Bool
isTimeWithinBounds startTime endTime time =
  if startTime >= endTime
    then do
      let midnightBeforeTimeleap = TimeOfDay 23 59 60
      (startTime < time && time < midnightBeforeTimeleap) || (midnight <= time && time < endTime)
    else startTime < time && time < endTime

calculateFare :: FareConfig -> Int -> String -> IO Integer
calculateFare fareConfig distance key = do 
  now <- getCurrentTime
  let res = calculate key (HM.fromList . M.toList $ constants fareConfig) (HM.fromList . M.toList $ variables fareConfig) distance now
  case res of 
    NUMBER_ fare -> pure . roundToIntegral $ (realToFrac fare)
    a -> do 
      print a
      error "Supplied variable is not of fare type."
      pure 0

roundToPowerOfTen :: RealFrac a => Int -> a -> a
roundToPowerOfTen tenExp a = do
  let f = 10 ^^ negate tenExp
      lifted = a * f
      eps = 1e-8
      roundVia func = fromIntegral (func lifted) / f
  if abs (abs (lifted - fromIntegral (floor lifted)) - 0.5) < eps
    then if a >= 0 then roundVia ceiling else roundVia floor
    else roundVia round

roundToUnits :: (RealFrac a) => a -> a
roundToUnits = roundToPowerOfTen 0

roundToIntegral :: (RealFrac a, Integral b) => a -> b
roundToIntegral = round . roundToUnits

