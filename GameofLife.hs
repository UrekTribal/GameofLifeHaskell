-- file: GameofLife.hs

data Estado  = Vivo | Morto | Zumbi
               deriving (Show, Eq)

type Posicao = (Integer, Integer)
type Grid   = [(Posicao, Estado)]

-- Funções para simplificar código
-- essas duas funções xContagem são aplicadas a uma lista de estados
zumbiContagem = length. filter (Zumbi ==)
vivoContagem  = length. filter (Vivo ==)
incrementa (x,y) n = if y < (n-1)
                     then (x,y+1)
                     else if x < (n-1)
                          then (x+1, 0)
                          else (n,n)   

morta = ((-1,-1), Morto)

vizinhos :: Posicao -> Integer -> [Posicao]
vizinhos (x,y) n =  if x == 0 
                  then if y == 0
                       then [(x+1, y), (x, y+1), (x+1, y+1)]
                       else if y == (n-1)
                            then [(x, y-1), (x+1, y-1), (x+1, y)]
                       else
                            [(x,   y-1),           (x,   y+1),
                             (x+1, y-1), (x+1, y), (x+1, y+1)]
                  else if x == (n-1)
                       then if y == 0
                            then [(x-1, y), (x-1, y+1), (x, y+1)]
                            else if y == (n-1)
                                 then [(x-1, y-1), (x-1, y), (x, y-1)]
                            else  
                                 [(x-1, y-1), (x-1, y), (x-1, y+1),
                                  (x,   y-1),           (x, y+1)]
                  else
                      [(x-1, y-1), (x, y-1), (x+1, y-1),
                       (x-1,   y),           (x+1,   y),
                       (x-1, y+1), (x, y+1), (x+1, y+1)]
                       

posicaoEstado :: Posicao -> Grid -> (Posicao, Estado)
posicaoEstado p (x:xs) = if p == fst x
                         then x
                         else posicaoEstado p xs


pegaEstado :: [Posicao] -> Grid -> Grid
pegaEstado (x:xs) grid = if x `elem` (map fst grid)
                          then let (x', y') = posicaoEstado x grid
                          in (x', y') : pegaEstado xs grid
                          else pegaEstado xs grid 
pegaEstado  _     grid = []


-- proximoEstado :: Posicao -> Grid -> Integer -> (Posicao, Estado)
-- proximoEstado x grid n = let viz = vizinhos x n
--                               estadoviz  = pegaEstado viz grid
--                               zumbiTotal = zumbiContagem (map snd estadoviz)
--                               vivoTotal  = vivoContagem  (map snd estadoviz)
--                           in if x `elem` (map fst grid)
--                              then let (pos, est) = posicaoEstado x grid
--                                   in if est == Vivo 
--                                      then if zumbiTotal == 0 && vivoTotal < 2
--                                           then morta
--                                           else if zumbiTotal == 0 && vivoTotal > 3
--                                                then morta
--                                           else if zumbiTotal == 0
--                                                then (pos, est)
--                                               else (pos, Zumbi)
--                                           else if vivoTotal == 0
--                                                then morta
--                                                else (pos, est)
--                              else if vivoTotal == 3
--                                   then (x, Vivo)
--                                   else morta

checaEstado :: Estado -> Int -> Int -> Estado
-- checaEstado Estado Vivos Zumbis
checaEstado Vivo  2 0 = Vivo
checaEstado Vivo  3 0 = Vivo
checaEstado Vivo  _ 0 = Morto
checaEstado Vivo  _ _ = Zumbi
checaEstado Zumbi 0 _ = Morto 
checaEstado Zumbi _ _ = Zumbi
checaEstado Morto 3 _ = Vivo
checaEstado _     _ _ = Morto

proximoEstado :: Posicao -> Grid -> Integer -> (Posicao, Estado)
proximoEstado x grid n = if  x `elem` (map fst grid)
                          then let (pos, estado) = posicaoEstado x grid
                                   novoestado  = checaEstado estado vivos zumbis 
                                    in if novoestado == Morto
                                       then morta
                                       else (pos, novoestado)
                          else let novoestado = checaEstado Morto vivos zumbis
                                   in if novoestado == Morto
                                       then morta
                                       else (x, novoestado)
    where viz       = vizinhos x n
          estadoviz = pegaEstado viz grid
          vivos     = vivoContagem  (map snd estadoviz) 
          zumbis    = zumbiContagem (map snd estadoviz)
          
proximoEstadoGrid :: Grid -> Integer -> Grid
proximoEstadoGrid grid tamanho = proximoEstadoGrid' (0,-1) []
    where proximoEstadoGrid' (x,y) novagrid = let (x',y') = incrementa (x,y) tamanho
                                              in if (x',y') == (tamanho,tamanho)
                                                  then novagrid
                                                   else let p = proximoEstado (x',y') grid tamanho
                                                        in if p == morta 
                                                           then proximoEstadoGrid' (x',y') novagrid
                                                           else proximoEstadoGrid' (x',y')  (p:novagrid)  
                                            


printarEstado :: Posicao -> Grid -> String
printarEstado x grid = if x `elem` (map fst grid)
                 then let (pos, estado) = posicaoEstado x grid
                      in if estado == Zumbi
                         then "-|"
                         else "o|"
                 else " |"


printarGrid :: Grid -> Integer -> String
printarGrid grid tamanho = printarGrid' (0,-1) tamanho
    where printarGrid' (x,y) tamanho = let (x',y') = incrementa (x,y) tamanho
                                            in if (x',y') == (tamanho,tamanho)
                                               then ""
                                               else let posicaoString = printarEstado (x',y') grid
                                                    in if y' == (tamanho - 1)
                                                       then posicaoString ++ "\n" ++ printarGrid' (x',y') tamanho
                                                       else posicaoString ++ printarGrid' (x',y') tamanho
                                                       


gameOfLife :: Grid -> Integer -> Integer -> IO ()
gameOfLife grid tamanho maximo = loopOfLife grid 0
    where loopOfLife grid n = if n == maximo
                         then putStrLn (printarGrid grid tamanho)
                         else let grid' = proximoEstadoGrid grid tamanho
                              in if grid' == grid
                                 then putStrLn ("O sistema se estabilizou no " ++ show n ++ " passo!\n")
                                 else loopOfLife grid' (n+1)
                                 

-- teste simples
teste' = [((1,2),Vivo),((1,4),Vivo),((2,1),Vivo),((2,2),Vivo),((2,4),Zumbi),((4,4),Zumbi)]

-- exemplos no caso onde não temos zumbis
block'   = [((1,1),Vivo),((1,2),Vivo),((2,1),Vivo),((2,2),Vivo)]
blinker' = [((1,2),Vivo),((2,2),Vivo),((3,2),Vivo)]
pulsar'  = [((2,4),Vivo),((2,5),Vivo),((2,6),Vivo),((2,10),Vivo),((2,11),Vivo),((2,12),Vivo),
           ((4,2),Vivo),((4,7),Vivo),((4,9),Vivo),((4,14),Vivo),
           ((5,2),Vivo),((5,7),Vivo),((5,9),Vivo),((5,14),Vivo),
           ((6,2),Vivo),((6,7),Vivo),((6,9),Vivo),((6,14),Vivo),
           ((7,4),Vivo),((7,5),Vivo),((7,6),Vivo),((7,10),Vivo),((7,11),Vivo),((7,12),Vivo),
           ((9,4),Vivo),((9,5),Vivo),((9,6),Vivo),((9,10),Vivo),((9,11),Vivo),((9,12),Vivo),
           ((10,2),Vivo),((10,7),Vivo),((10,9),Vivo),((10,14),Vivo),
           ((11,2),Vivo),((11,7),Vivo),((11,9),Vivo),((11,14),Vivo),
           ((12,2),Vivo),((12,7),Vivo),((12,9),Vivo),((12,14),Vivo),
           ((14,4),Vivo),((14,5),Vivo),((14,6),Vivo),((14,10),Vivo),((14,11),Vivo),((14,12),Vivo)]
pentadecathlon' = [((4,10),Vivo),((6,10),Vivo),((7,9),Vivo),((7,11),Vivo),
                  ((8,10),Vivo),((9,10),Vivo),((10,10),Vivo),((11,10),Vivo),
                  ((12,9),Vivo),((12,11),Vivo),((13,10),Vivo),((14,10),Vivo)]
glider' = [((1,1),Vivo),((2,2),Vivo),((2,3),Vivo),((3,1),Vivo),((3,2),Vivo)]


-- exemplos com zumbis
terradosmortos' = [((0,0),Zumbi),((1,1),Vivo),((2,2),Vivo),((3,3),Vivo),((4,4),Vivo),((5,5),Vivo),((6,6),Vivo),
                        ((7,7),Vivo),((8,8),Vivo),((9,9),Vivo),((10,10),Zumbi),((0,10),Zumbi),((1,9),Vivo),((2,8),Vivo),
                        ((3,7), Vivo),((4,6),Vivo),((6,4),Vivo),((7,3),Vivo),((8,2),Vivo),((9,1),Vivo),((10,0),Zumbi)]
diadosmortos' = [((0,0),Zumbi),((1,1),Vivo),((2,2),Vivo),((2,3),Vivo),((3,1),Vivo),((3,2),Vivo)]


teste          = gameOfLife teste' 5
block          = gameOfLife block' 4
blinker        = gameOfLife blinker' 5
pulsar         = gameOfLife pulsar' 17
pentadecathlon = gameOfLife pentadecathlon' 18
glider         = gameOfLife glider' 10
terradosmortos = gameOfLife terradosmortos' 11
diadosmortos   = gameOfLife diadosmortos' 6