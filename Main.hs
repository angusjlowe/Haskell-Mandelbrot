import Graphics.Gloss
import Graphics.Gloss.Raster.Field

dodger = makeColorI 30 144 255 255
navy = makeColorI 0 0 128 255
midnight = makeColorI 25 25 112 255
cobalt = makeColorI 61 89 171 255
royal1 = makeColorI 65 105 225 255
royal2 = makeColorI 72 118 255 255
royal3 = makeColorI 58 95 205 255
royal4 = makeColorI 39 64 139 255
cfblue = makeColorI 100 149 237 255
steel = makeColorI 176 196 222 255
steelblue1 = makeColorI 99 184 255 255
steelblue2 = makeColorI 79 148 205 255
skyblue1 = makeColorI 135 206 255 255
skyblue2 = makeColorI 108 166 205 255
deepsky1 = makeColorI 0 178 238 255
deepsky2 = makeColorI 0 191 255 255
peacock = makeColorI 51 161 201 255
cadet = makeColorI 152 245 255 255
turquiose = makeColorI 0 229 238 255
mangblue = makeColorI 3 168 158 255



next :: Point -> Point -> Point
next (u,v) (x,y) = (x*x - y*y + u, 2*x*y + v)

mandelbrotFunction :: Point -> [Point]
mandelbrotFunction (u,v) = (iterate (next (u,v)) (0,0))

fairlyClose :: Point -> Bool
fairlyClose (x,y) = x*x + y*y < 500

colors :: [Color]
colors = [dodger, navy, midnight, cobalt, royal1, royal2, royal3, royal4, cfblue, steel, turquiose, steelblue1, steelblue2, skyblue1, skyblue2, deepsky1, deepsky2, peacock, cadet, turquiose, mangblue, black]

chooseColor :: [Color] -> [Point] -> Color
chooseColor palette = (palette !!) . length . take (n-1) . takeWhile fairlyClose
                    where n = length palette

mandelbrot :: Point -> Color
mandelbrot (u,v) = (chooseColor colors . (mandelbrotFunction)) (u-1,v)

mandelbrot2 :: Point -> Color
mandelbrot2 (u,v) = (chooseColor colors . (mandelbrotFunction)) (u+1,v)

main :: IO ()
main = display (InWindow "Mandelbrot" (1000,1000) (20,20)) white (Pictures [makePicture 300 300 1 1 (mandelbrot), translate 500 0 (makePicture 500 500 1 1 (mandelbrot2))])
