import SimpleODE
import ODEExample
import Data.List (intercalate)
import Text.Printf

-- gnuplot
-- plot "ode1.txt" using 1:2 with linespoints
-- replot "ode12txt" using 1:2 with linespoints
main :: IO ()
main = do
    let ret1 = solve example_4_1
    let ret2 = solve example_4_1_euler
    put "ode1.txt" ret1
    put "ode2.txt" ret2
    return ()




put :: String -> [(Float, Float)] -> IO ()
put filename xs = do
    putStrLn text
    putStrLn ""
    writeFile filename text

    where
        text = intercalate "\n" $ map (\a -> printf "%.5f" (fst a) ++ " " ++ printf "%.5f" (snd a)) xs

