-- Ayden Best
-- CPSC 354-01


-- We need to implement a combination of real and imaginary numbers
-- Here we have a complex number consisting of 2 floats (real, imaginary)
newtype Complex = Complex (Float,Float) deriving (Show,Eq)

instance (Num) Complex where
    -- simple conversion from int to imaginary
    fromInteger n = Complex (fromIntegral n,0.0)

    -- add complex numbers: add reals and imaginaries
    Complex (x,y) + Complex (z,t) = Complex (x+z, y+t)

    -- subtract complex numbers: just like add but not really
    Complex (x,y) - Complex (z,t) = Complex (x-z, y-t)

    -- multiply complex numbers
    -- ex: (1+2i)*(2+3i) = (1*2+(2i*3i)) + (3i*1 + 2*2i)
    Complex (x,y) * Complex (z,t) = Complex (z*x - y*t, y*z + x*t)

    -- divide complex numbers
    -- ex: only using it for: (1+i) / (2+0i) => (1/2 + i/2) typeclass Num does not have a div operator?
    --Complex (x,y) / Complex (z,t) = Complex (x/z , y/z)

    -- equation for absolute value of complex numbers
    abs (Complex (x,y)) = Complex (sqrt (x*x + y*y),0.0)


complexdiv :: Complex -> Float -> Complex
complexdiv (Complex(x,y)) r = Complex(x/r, y/r) 

    -- signum (Complex (x,y))  = Complex (signum x , 0.0)

-- Constants based on community recommendations

    -- speed multiplier /100%
zoomSpeed = 100

    -- initial view-range, recommended for mandelbrot tests
planeStartBottom = Complex (-2.0,-1.0)
planeStartTop = Complex (1.0,1.0)

    -- offset from the center while zooming
    -- keeps the view on the edge of the set, found this by trial and error
startOffset = Complex (-1.713,-0.000)

    -- main function
    -- starts loop and redoes it
main = do
    putStrLn $ loopNextFrame 0
    where
        loopNextFrame n = mandel (screen n) ++ "\x1b[H\x1b[25A" ++ loopNextFrame (n+1)
        screen n = zoom planeStartBottom planeStartTop startOffset (zoomSpeed*(0.0105)*n) 

-- Initial frame bottom point -> Initial frame top point ->
-- Centering offset -> zoom multiplier*render loop number
-- takes the current corner points of the frame and adjusts them to be smaller based on the zoomSpeed and offset
zoom :: Complex -> Complex -> Complex -> Float -> (Complex,Complex)
zoom bot top offset zoom = (zoomAmount bot, zoomAmount top)
    where -- shifting the viewport, based on a function from typeclasses.com
        zoomAmount initialPoint = (complexdiv (offset*(Complex (zoom, 0.0)) + initialPoint ) (zoom + 1))


-- Render function
-- Takes a complex number, maps darker chars to pixels that diverged the least
mandel :: (Complex,Complex) -> String
mandel (bottomleft,topright) = concat $ map renderToText $ renderValues bottomleft topright
    where
        renderToText (i,nextRow) = " .'`^\",:;Il!i><~+_-?][}{1)(|\\/tfjrxnuvczXYUJCLQ0OZmwqpdbkhao*#MW&8%B@$" !! (div (i*70) 32):rst nextRow
        rst True = "\n"
        rst False = ""


    -- apply the progressive function 
    -- on a complex number value, keep squaring it and it may or may not diverge. Non-divergence will fall out of the shading sweetspot
    -- after 32 squares, we will assume it has diverged enough already and we will not print that pixel
progress :: Complex -> Complex -> Int -> Int
progress c z 0 = 0
progress c (Complex (x,y)) n = if (abs x > 2) then n else progress c (((Complex (x,y))*(Complex (x,y)))+c) (n-1) 

-- Somewhat unknown function?
-- Initially mapped values across a 2D plane and converted them to different values based on a scale
-- The scale we use is 32. For whatever reason, this number works out with the contrast for our shading chars
renderValues (Complex (a,b)) (Complex (x,y)) = map (\z -> (progress (Complex z) (Complex(0,0)) 32, (fst z > right - hstep/2 ))) [(x,y) | y <- [bottom,(bottom + vstep)..top], x<-[left,(left + hstep)..right]]
    where
        top = y
        bottom = b
        left = a
        right = x
        vstep=(top-bottom)/40 -- dividing the range of the 
        hstep=(right-left)/80




