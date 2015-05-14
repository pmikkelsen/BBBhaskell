module GPIO where

import System.IO
import Data.Maybe (fromJust)

data PinMode = Output | Input deriving (Show, Eq) 

data LogicLevel = Low | High deriving (Enum, Show, Eq)

data PinHeader = P8 | P9 deriving (Show, Eq)

data PinNumber =  Pin1 | Pin2 | Pin3 | Pin4 | Pin5 | Pin6 | Pin7 | Pin8 | Pin9
		| Pin10 | Pin11 | Pin12 | Pin13 | Pin14 | Pin15 | Pin16 | Pin17
		| Pin18 | Pin19 | Pin20 | Pin21 | Pin22 | Pin23 | Pin24 | Pin25
		| Pin26 | Pin27 | Pin28 | Pin29 | Pin30 | Pin31 | Pin32 | Pin33
		| Pin34 | Pin35 | Pin36 | Pin37 | Pin38 | Pin39 | Pin40 | Pin41
		| Pin42 | Pin43 | Pin44 | Pin45 | Pin46 deriving (Enum, Show, Eq)		

type Pin = (PinHeader, PinNumber)
		
data LED = LED0 | LED1 | LED2 | LED3 deriving (Enum, Show, Eq)

analogPins :: [Pin]
analogPins = [(P9, Pin33)
             ,(P9, Pin35)
             ,(P9, Pin36)
             ,(P9, Pin37)
             ,(P9, Pin38)
             ,(P9, Pin39)
             ,(P9, Pin40)]

gpioPins :: [(Pin, Int)]
gpioPins = [((P8, Pin3), 38)
	   ,((P8, Pin4), 39)
	   ,((P8, Pin5), 34)
	   ,((P8, Pin6), 35)
	   ,((P8, Pin7), 66)
	   ,((P8, Pin8), 67)
	   ,((P8, Pin9), 69)
	   ,((P8, Pin10), 68)
	   ,((P8, Pin11), 45)
	   ,((P8, Pin12), 44)
	   ,((P8, Pin13), 23)
	   ,((P8, Pin14), 26)
	   ,((P8, Pin15), 47)
	   ,((P8, Pin16), 46)
	   ,((P8, Pin17), 27)
	   ,((P8, Pin18), 65)
	   ,((P8, Pin19), 22)
	   ,((P8, Pin20), 63)
	   ,((P8, Pin21), 62)
	   ,((P8, Pin22), 37)
	   ,((P8, Pin23), 36)
	   ,((P8, Pin24), 33)
	   ,((P8, Pin25), 32)
	   ,((P8, Pin26), 61)
	   ,((P8, Pin27), 86)
	   ,((P8, Pin28), 88)
	   ,((P8, Pin29), 87)
	   ,((P8, Pin30), 89)
	   ,((P8, Pin31), 10)
	   ,((P8, Pin32), 11)
	   ,((P8, Pin33), 9)
	   ,((P8, Pin34), 81)
	   ,((P8, Pin35), 8)
	   ,((P8, Pin36), 80)
	   ,((P8, Pin37), 78)
	   ,((P8, Pin38), 79)
	   ,((P8, Pin39), 76)
	   ,((P8, Pin40), 77)
	   ,((P8, Pin41), 74)
	   ,((P8, Pin42), 75)
	   ,((P8, Pin43), 72)
	   ,((P8, Pin44), 73)
	   ,((P8, Pin45), 70)
	   ,((P8, Pin46), 71)
	   ,((P9, Pin11), 30)
	   ,((P9, Pin12), 60)
	   ,((P9, Pin13), 31)
	   ,((P9, Pin14), 40)
	   ,((P9, Pin15), 48)
	   ,((P9, Pin16), 51)
	   ,((P9, Pin17), 4)
	   ,((P9, Pin18), 5)
	   ,((P9, Pin21), 3)
	   ,((P9, Pin22), 2)
	   ,((P9, Pin23), 49)
	   ,((P9, Pin24), 15)
	   ,((P9, Pin25), 117)
	   ,((P9, Pin26), 14)
	   ,((P9, Pin27), 125)
	   ,((P9, Pin28), 123)
	   ,((P9, Pin29), 121)
	   ,((P9, Pin30), 122)
	   ,((P9, Pin31), 120)
	   ,((P9, Pin41), 20)
	   ,((P9, Pin42), 7)
	   ]

baseGPIOPath :: FilePath
baseGPIOPath = "./hue/"

baseLEDPath :: FilePath
baseLEDPath = "/sys/class/leds/beaglebone:green:usr"

getPinPath :: Pin -> Maybe FilePath
getPinPath pin = 
	if lookup pin gpioPins == Nothing
		then Nothing
		else Just $ baseGPIOPath ++ "gpio" ++ pinGPIO pin ++ "/"
		where pinGPIO pin = show $ fromJust (lookup pin gpioPins)

getLEDpath :: LED -> FilePath
getLEDpath led = baseLEDPath ++ (show (fromEnum led)) ++ "/brightness"

makePin :: PinHeader -> PinNumber -> IO Pin
makePin ph pn = return (ph, pn)

exportPin :: Pin -> IO ()
exportPin pin = 
	if lookup pin gpioPins == Nothing
		then return ()
		else writeFile (baseGPIOPath ++ "export") (show $ fromJust $ lookup pin gpioPins)

unexportPin :: Pin -> IO ()
unexportPin pin =
	if lookup pin gpioPins == Nothing
		then return ()
		else writeFile (baseGPIOPath ++ "unexport") (show $ fromJust $ lookup pin gpioPins)

setGPIOMode :: Pin -> PinMode -> IO ()
setGPIOMode = undefined

readGPIO :: Pin -> IO (Maybe LogicLevel)
readGPIO = undefined

readAnalogPin :: Pin -> IO (Maybe Int)
readAnalogPin pin = do
	if pin `elem` analogPins 
		then do
			--fileHandle <- openFile
			--hClose fileHandle
			return (Just 42)
		else return Nothing

setPin :: Pin -> LogicLevel -> IO ()
setPin pin level =
	if lookup pin gpioPins == Nothing
		then putStrLn $ show pin ++ " not a valid GPIO pin."
		else do
			fileHandle <- openFile (fromJust (getPinPath pin) ++ "/value") WriteMode
			hPutStr fileHandle (show (fromEnum level)) 
			hFlush fileHandle
			hClose fileHandle

toogleLED :: LED -> LogicLevel -> IO ()
toogleLED led level = do
	fileHandle <- openFile (getLEDpath led) WriteMode
	hPutStr fileHandle (show (fromEnum level))
	hFlush fileHandle
	hClose fileHandle
