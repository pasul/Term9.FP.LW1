module FCMSettings where

data VectorsDistance = Hamming | Euclid deriving (Show, Read, Eq)
data InitialFCMStep = RandomPartitioinMatrix | RandomClusterCenters deriving (Show, Read, Eq)

data Settings = Settings
    { separator :: Char 
    , skipHeader :: Bool
    , skipFirstCol :: Bool
    , skipLastCol :: Bool
    , outFile :: Maybe FilePath
    , clustersCount :: Int
    , m :: Double
    , eps :: Double
    , vectorsDistance :: VectorsDistance
    , initialFCMStep :: InitialFCMStep
    }
  
defaultSettings :: Settings
defaultSettings = Settings
    { separator = ','
    , skipHeader = True
    , skipFirstCol = False
    , skipLastCol = False  
    , outFile = Nothing
    , clustersCount = 2
    , m = 2
    , eps = 0.0001
    , vectorsDistance = Euclid
    , initialFCMStep = RandomPartitioinMatrix   
    }

parseSettings :: [String] -> Settings -> (Settings, [String])
parseSettings [] parameters = (parameters, [])
parseSettings ("-s" : separator' : t) parameters = parseSettings t $ parameters { separator = head separator' }
parseSettings ("-sh" : t) parameters = parseSettings t $ parameters { skipHeader = True }
parseSettings ("-sfc" : t) parameters = parseSettings t $ parameters { skipFirstCol = True }
parseSettings ("-slc" : t) parameters = parseSettings t $ parameters { skipLastCol = True }
parseSettings ("-outFile" : outFile' : t) parameters = parseSettings t $ parameters { outFile = Just outFile' }
parseSettings ("-c" : clustersCount' : t) parameters = parseSettings t $ parameters { clustersCount = read clustersCount' }
parseSettings ("-m" : m' : t) parameters = parseSettings t $ parameters { m = read m' }
parseSettings ("-eps" : eps' : t) parameters = parseSettings t $ parameters { eps = read eps' }
parseSettings ("-d" : vectorsDistance' : t) parameters = parseSettings t $ parameters { vectorsDistance = read vectorsDistance' }
parseSettings ("-initStep" : initialFCMStep' : t) parameters = parseSettings t $ parameters { initialFCMStep = read initialFCMStep' }
parseSettings args parameters = (parameters, args) 
