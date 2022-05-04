import Text.Printf

type Point     = (Float,Float)
type Rect      = (Point,Float,Float)
type Circle    = (Point,Float)

--Geração de números aleatórios
meuBSD :: Int -> [Int]
meuBSD n = tail (iterate (\x -> (1103515245 * x + 12345) `mod` 2^31) n)


main :: IO()
main = do
  writeFile "figs.svg" $ svgstrs
  where svgstrs = svgBegin (fromIntegral w) (fromIntegral h) ++ svgfigs ++ svgEnd
        svgfigs = svgElements svgRect rects (map svgStyle palette)
        rects = genRectsInRandom w h nrects
        palette = rgbPalette nrects
        nrects = 10
        (w,h) = (1000,1000) -- width,height da imagem SVG

-------------------
-- Coisas do SVG --
-------------------

-- String inicial do SVG
svgBegin :: Float -> Float -> String
svgBegin w h = printf "<svg width='%.2f' height='%.2f' xmlns='http://www.w3.org/2000/svg'>\n" w h 

-- String final do SVG
svgEnd :: String
svgEnd = "</svg>"


-- Gera string com atributos de estilo para uma dada cor
-- Atributo mix-blend-mode permite misturar cores
svgStyle :: (Int,Int,Int) -> String
svgStyle (r,g,b) = printf "fill:rgb(%d,%d,%d); mix-blend-mode: screen;" r g b

-- Gera strings SVG para uma dada lista de figuras e seus atributos de estilo
-- Recebe uma função geradora de strings SVG, uma lista de círculos/retângulos e strings de estilo
svgElements :: (a -> String -> String) -> [a] -> [String] -> String
svgElements func elements styles = concat $ zipWith func elements styles

-- Gera string representando retângulo SVG 
-- dadas coordenadas e dimensões do retângulo e uma string com atributos de estilo
svgRect :: Rect -> String -> String 
svgRect ((x,y),w,h) style = 
  printf "<rect x='%.3f' y='%.3f' width='%.2f' height='%.2f' style='%s' />\n" x y w h style


-- Geração de Retãngulos

genRectsInRandom :: Int -> Int -> Int -> [(Rect)]
genRectsInRandom _ _ 0 = []
genRectsInRandom w h n = ((xcoord, ycoord), width, height) : genRectsInRandom w h (n-1)
  where nmbrs = take 4 (meuBSD (n+(fromIntegral (w*h))))
        xcoord = fromIntegral $ nmbrs!!0 `mod` w
        ycoord = fromIntegral $ nmbrs!!1 `mod` h
        width = fromIntegral $ (nmbrs!!2 `mod` 50)+30
        height = fromIntegral $ (nmbrs!!3 `mod` 50)+30

-- Paleta com n valores retirados de uma lista com sequências de R, G e B 
rgbPalette :: Int -> [(Int,Int,Int)]
rgbPalette n = take n $ cycle [(255,0,0),(0,255,0),(0,0,255)]