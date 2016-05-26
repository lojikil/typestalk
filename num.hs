data MyNum = FlNum Float | INum Int deriving Show

numsum :: MyNum -> MyNum -> MyNum
numsum (FlNum f) (FlNum h) = FlNum $ h + f
numsum (FlNum j) (INum i0) = FlNum $ j + (fromIntegral i0)
numsum (INum i1) (FlNum f3) = FlNum $ (fromIntegral i1) + f3
numsum (INum i2) (INum i3) = INum $ i2 + i3
