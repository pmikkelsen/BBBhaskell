import GPIO

main :: IO ()
main = do
	pin1 <- makePin P8 Pin5		
	print $ getPinPath pin1
	exportPin pin1
	pin2 <- makePin P9 Pin34
	print $ getPinPath pin2
	exportPin pin2
	setPin pin2 High
	pin3 <- makePin P8 Pin10
	print $ getPinPath pin3
	exportPin pin3
	setPin pin3 Low
	unexportPin pin3
	return ()
