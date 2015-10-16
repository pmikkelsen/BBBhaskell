module BBB.IC74HC595 where

import BBB.GPIO

data IC74HC595 = IC74HC595  { latchPin  :: ExportedPin
                            , dataPin   :: ExportedPin
                            , clockPin  :: ExportedPin
                            , outputEnablePin :: ExportedPin}

icPins :: IC74HC595 -> [ExportedPin]
icPins ic = map ($ ic) [latchPin, dataPin, clockPin, outputEnablePin] 

clock :: IC74HC595 -> LogicLevel -> IO ()
clock ic level = do
    setPin (dataPin ic)     level
    setPin (clockPin ic)    High
    setPin (dataPin ic)     Low
    setPin (clockPin ic)    Low
    
flush :: IC74HC595 -> IO ()
flush ic = do
    setPin (latchPin ic) High
    setPin (latchPin ic) Low

flushData :: IC74HC595 -> [LogicLevel] -> IO ()
flushData ic levels = do
    mapM_ (clock ic) levels
    flush ic

flushAll :: IC74HC595 -> LogicLevel -> IO ()
flushAll ic level = flushData ic $ replicate 8 level

setupIC74HC595 :: Pin -> Pin -> Pin -> Pin -> IO IC74HC595 
setupIC74HC595 latchP dataP clockP oeP = do
    latchP' <- exportPin latchP
    dataP' <- exportPin dataP
    clockP' <- exportPin clockP
    oeP' <- exportPin oeP
    mapM_ (`setPin` Low) [latchP', dataP', clockP', oeP']
    return $ IC74HC595 latchP' dataP' clockP' oeP' 

cleanupIC74HC595 :: IC74HC595 -> IO ()
cleanupIC74HC595 ic = do
    flushAll ic Low
    mapM_ unexportPin $ icPins ic
