data HTML = HTML [Char] deriving Show

returnToBrowser :: HTML -> IO ()
returnToBrowser (HTML f) = putStrLn f

-- generally, we wouldn't export this internal
-- binding. We could also only export the safeEncode
-- below, and avoid expsing this internal function.
-- could also define it locally to safeEncode proper
safeEncode' :: [Char] -> [Char]
safeEncode' ('<' : xs) = "&lt;" ++ safeEncode' xs
safeEncode' ('>' : xs) = "&gt;" ++ safeEncode' xs
safeEncode' ('&' : xs) = "&amp;" ++ safeEncode' xs
safeEncode' (x : xs) = [x] ++ safeEncode' xs 
safeEncode' [] = []

-- not *actually* safe.
-- void where prohibited...
safeEncode :: [Char] -> HTML
safeEncode x = HTML $ safeEncode' x
