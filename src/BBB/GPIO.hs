module BBB.GPIO 
    ( LogicLevel(..)
    , PinNumber(..)
    , Pin
    , ExportedPin
    , PinHeader(..)
    , LED(..)
    , gpioPin
    , toogleLED
    , exportPin
    , unexportPin
    , setPin 
    ) where

import System.IO
import Data.Maybe (fromJust, isNothing)

data PinMode = Output | Input deriving (Show, Eq) 

data LogicLevel = Low | High deriving (Enum, Show, Eq)

data PinHeader = P8 | P9 deriving (Show, Eq)

data PinNumber =  Pin1 | Pin2 | Pin3 | Pin4 | Pin5 | Pin6 | Pin7 | Pin8 | Pin9
		| Pin10 | Pin11 | Pin12 | Pin13 | Pin14 | Pin15 | Pin16 | Pin17
		| Pin18 | Pin19 | Pin20 | Pin21 | Pin22 | Pin23 | Pin24 | Pin25
		| Pin26 | Pin27 | Pin28 | Pin29 | Pin30 | Pin31 | Pin32 | Pin33
		| Pin34 | Pin35 | Pin36 | Pin37 | Pin38 | Pin39 | Pin40 | Pin41
		| Pin42 | Pin43 | Pin44 | Pin45 | Pin46 deriving (Enum, Show, Eq)		

data Pin = Pin PinHeader PinNumber | InvalidPin PinHeader PinNumber deriving (Show, Eq)
data ExportedPin = ExportedPin Pin Handle | InvalidExportedPin
		
data LED = LED0 | LED1 | LED2 | LED3 deriving (Enum, Show, Eq)

analogPins :: [Pin]
analogPins = [Pin P9 Pin33
             ,Pin P9 Pin35
             ,Pin P9 Pin36
             ,Pin P9 Pin37
             ,Pin P9 Pin38
             ,Pin P9 Pin39
             ,Pin P9 Pin40]

gpioPins :: [(Pin, Int)]
gpioPins = [(Pin P8 Pin3, 38)
	   ,(Pin P8 Pin4, 39)
	   ,(Pin P8 Pin5, 34)
	   ,(Pin P8 Pin6, 35)
	   ,(Pin P8 Pin7, 66)
	   ,(Pin P8 Pin8, 67)
	   ,(Pin P8 Pin9, 69)
	   ,(Pin P8 Pin10, 68)
	   ,(Pin P8 Pin11, 45)
	   ,(Pin P8 Pin12, 44)
	   ,(Pin P8 Pin13, 23)
	   ,(Pin P8 Pin14, 26)
	   ,(Pin P8 Pin15, 47)
	   ,(Pin P8 Pin16, 46)
	   ,(Pin P8 Pin17, 27)
	   ,(Pin P8 Pin18, 65)
	   ,(Pin P8 Pin19, 22)
	   ,(Pin P8 Pin20, 63)
	   ,(Pin P8 Pin21, 62)
	   ,(Pin P8 Pin22, 37)
	   ,(Pin P8 Pin23, 36)
	   ,(Pin P8 Pin24, 33)
	   ,(Pin P8 Pin25, 32)
	   ,(Pin P8 Pin26, 61)
	   ,(Pin P8 Pin27, 86)
	   ,(Pin P8 Pin28, 88)
	   ,(Pin P8 Pin29, 87)
	   ,(Pin P8 Pin30, 89)
	   ,(Pin P8 Pin31, 10)
	   ,(Pin P8 Pin32, 11)
	   ,(Pin P8 Pin33, 9)
	   ,(Pin P8 Pin34, 81)
	   ,(Pin P8 Pin35, 8)
	   ,(Pin P8 Pin36, 80)
	   ,(Pin P8 Pin37, 78)
	   ,(Pin P8 Pin38, 79)
	   ,(Pin P8 Pin39, 76)
	   ,(Pin P8 Pin40, 77)
	   ,(Pin P8 Pin41, 74)
	   ,(Pin P8 Pin42, 75)
	   ,(Pin P8 Pin43, 72)
	   ,(Pin P8 Pin44, 73)
	   ,(Pin P8 Pin45, 70)
	   ,(Pin P8 Pin46, 71)
	   ,(Pin P9 Pin11, 30)
	   ,(Pin P9 Pin12, 60)
	   ,(Pin P9 Pin13, 31)
	   ,(Pin P9 Pin14, 40)
	   ,(Pin P9 Pin15, 48)
	   ,(Pin P9 Pin16, 51)
	   ,(Pin P9 Pin17, 4)
	   ,(Pin P9 Pin18, 5)
	   ,(Pin P9 Pin21, 3)
	   ,(Pin P9 Pin22, 2)
	   ,(Pin P9 Pin23, 49)
	   ,(Pin P9 Pin24, 15)
	   ,(Pin P9 Pin25, 117)
	   ,(Pin P9 Pin26, 14)
	   ,(Pin P9 Pin27, 125)
	   ,(Pin P9 Pin28, 123)
	   ,(Pin P9 Pin29, 121)
	   ,(Pin P9 Pin30, 122)
	   ,(Pin P9 Pin31, 120)
	   ,(Pin P9 Pin41, 20)
	   ,(Pin P9 Pin42, 7)
	   ]

baseGPIOPath :: FilePath
baseGPIOPath = "/sys/class/gpio/"

baseLEDPath :: FilePath
baseLEDPath = "/sys/class/leds/beaglebone:green:usr"

getPinPath :: Pin -> Maybe FilePath
getPinPath pin = 
    if isNothing $ lookup pin gpioPins
	then Nothing
	else Just $ baseGPIOPath ++ "gpio" ++ pinGPIO pin 
	where pinGPIO pin = show $ fromJust (lookup pin gpioPins)

getLEDpath :: LED -> FilePath
getLEDpath led = baseLEDPath ++ show (fromEnum led) ++ "/brightness"

gpioPin :: PinHeader -> PinNumber -> Pin
gpioPin ph pn = 
    if isNothing $ lookup pin gpioPins
        then InvalidPin ph pn
        else pin
    where pin = Pin ph pn

exportPin :: Pin -> IO ExportedPin
exportPin (InvalidPin ph pn) = do
    putStrLn $ show (ph,pn) ++ " is an invalid GPIO pin, cant export. No actions will be performed on it."
    return InvalidExportedPin
exportPin pin = do
    writeFile (baseGPIOPath ++ "export") (show $ fromJust $ lookup pin gpioPins)
    writeFile ((fromJust (getPinPath pin)) ++ "/direction") "out"
    handle <- openFile (fromJust (getPinPath pin) ++ "/value") WriteMode
    hSetBuffering handle NoBuffering
    putStrLn $ show pin ++ " is now exported."
    return $ ExportedPin pin handle

unexportPin :: ExportedPin -> IO ()
unexportPin (ExportedPin pin handle) = do
    hClose handle
    writeFile (baseGPIOPath ++ "unexport") (show $ fromJust $ lookup pin gpioPins)
    putStrLn $ show pin ++ " is now unexported."
unexportPin _ = return ()

setPin :: ExportedPin -> LogicLevel -> IO ()
setPin (ExportedPin _ handle) High = hPutChar handle '1'
setPin (ExportedPin _ handle) Low = hPutChar handle '0'
setPin _ _ = return ()

toogleLED :: LED -> LogicLevel -> IO ()
toogleLED led level = writeFile (getLEDpath led) (show (fromEnum level))
