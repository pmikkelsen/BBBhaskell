import GPIO
import Control.Monad

main :: IO ()
main = do
    pin1 <- exportPin $ gpioPin P8 Pin5
    replicateM_ 10000000 (setPin pin1 Low)
    unexportPin pin1
