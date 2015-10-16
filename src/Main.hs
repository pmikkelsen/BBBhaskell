import BBB.GPIO
import BBB.IC74HC595
import Control.Concurrent
import Control.Monad

main :: IO ()
main = do
	testIc <- setupIC74HC595 (gpioPin P9 Pin21) (gpioPin P9 Pin15) (gpioPin P9 Pin11) (gpioPin P9 Pin26)
	replicateM_ (60*30*3) $ flushAll testIc High
	flushAll testIc Low
	cleanupIC74HC595 testIc
