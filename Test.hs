--
-- Projecto CP 2015/16
--
-- O projecto consiste em desenvolver testes para o módulo Graph.hs
-- (para grafos orientados e não pesados).
-- Mais concretamente, o projecto consiste em 3 tarefas que são descritas abaixo.
-- O prazo para entrega é o dia 3 de Abril. Cada grupo deve enviar apenas
-- o módulo de testes (este módulo) por email para calculodeprogramas@gmail.com
-- O nome do ficheiro deve identificar os números dos 2 alunos do grupo (numero1_numero2.hs).
-- Certifiquem-se que o módulo de testes desenvolvido compila correctamente antes
-- de submeter. O módulo Graph.hs não deve ser alterado.
-- Os 2 alunos do grupo devem também indentificar-se nos comentários abaixo.
--
-- Aluno 1
-- Número: A74817
-- Nome: Marcelo António Caridade Miranda
-- Curso: MIEI
--
-- Aluno 2
-- Número: A73860
-- Nome: Guilherme Vasconcelos da Silva Guerreiro
-- Curso: MIEI
--


module Main where

import Graph
import Test.HUnit hiding (path)
import Test.QuickCheck
import Data.Set as Set

import Data.List as List
import Control.Monad.State

--
-- Teste unitário
--

t_list = concat  [t_swap, [t_empty], t_transpose, [t_union], t_isEmpty, t_isValid,
                  t_isDAG, t_isForest, t_isSubgraphOf, t_adj, t_isPathOf, t_path,
                  t_reachable, [t_bft], t_topo]

-- Testar swap
t_swap = [t_swap1, t_swap2]

t_swap1 = swap Edge {source = 140, target = 39 } ~?= Edge { source = 39, target = 140 }
t_swap2 = swap Edge {source = 3, target = 3 } ~?= Edge { source = 3, target = 3 }

-- Testar empty
t_empty = Set.null (nodes Graph.empty) && Set.null (edges Graph.empty) ~?= True

-- Testar isEmpty
t_isEmpty = [t_isEmpty1, t_isEmpty2]

emptyGraph :: Graph Int
emptyGraph = Graph Set.empty Set.empty

g2 :: Graph Int
g2 = Graph (fromList [1,2]) Set.empty

t_isEmpty1 = isEmpty emptyGraph ~?= True
t_isEmpty2 = isEmpty g2 ~?= False

-- Testar isValid, isDAG, isForest
t_isValid = [t_isValid1, t_isValid2, t_isValid4]
t_isDAG = [t_isDAG1, t_isDAG2]
t_isForest = [t_isForest1, t_isForest2]

g3 :: Graph Int
g3 = Graph (fromList [1]) (fromList [Edge 1 3])

g4 :: Graph Int
g4 = Graph (fromList [1]) (fromList [Edge 3 1])

g5 :: Graph Int
g5 = Graph (fromList [1,2]) (fromList [Edge 1 2, Edge 2 1])

g6 :: Graph Int
g6 = Graph (fromList [1,2,3]) (fromList [Edge 1 2, Edge 2 3, Edge 3 1])

g7 :: Graph Int
g7 = Graph (fromList [1,2,3]) (fromList [Edge 1 2, Edge 1 3])

g8 :: Graph Int
g8 = Graph (fromList [1,2,3]) (fromList [Edge 1 3, Edge 2 3])

t_isValid1 = isValid g3 ~?= False
t_isValid2 = isValid g4 ~?= False
--t_isValid3 = isValid g5 ~?= False
t_isValid4 = isValid g6 ~?= True

t_isDAG1 = isDAG g6 ~?= False
t_isDAG2 = isDAG g7 ~?= True

t_isForest1 = isForest g7 ~?= False
t_isForest2 = isForest g8 ~?= True

-- Testar isSubgraphOf
t_isSubgraphOf = [t_isSubgraphOf1, t_isSubgraphOf2, t_isSubgraphOf3, t_isSubgraphOf4]

g9 :: Graph Int
g9 = Graph (fromList [1,2,3,4]) (fromList [Edge 1 3])

g10 :: Graph Int
g10 = Graph (fromList[1,2,3]) (fromList[Edge 1 3, Edge 1 2])

t_isSubgraphOf1 = isSubgraphOf g9 g8 ~?= False
t_isSubgraphOf2 = isSubgraphOf g10 g8 ~?= False
t_isSubgraphOf3 = isSubgraphOf g8 g8 ~?= True
t_isSubgraphOf4 = isSubgraphOf emptyGraph g8 ~?= True

-- Testar adj
g11 :: Graph Int
g11 = Graph (fromList [1]) Set.empty

g12 :: Graph Int
g12 = Graph (fromList [1,2,3]) (fromList [Edge 1 2, Edge 1 3, Edge 2 3])

t_adj = [t_adj1, t_adj2]
t_adj1 = adj g11 1 ~?= fromList []
t_adj2 = adj g12 1 ~?= fromList [Edge 1 2, Edge 1 3]

-- Testar transpose
t_transpose = [t_transpose1, t_transpose2]

g13 :: Graph Int
g13 = Graph (fromList [1,2,3]) (fromList [Edge 1 2, Edge 2 3, Edge 3 1])

t_transpose1 = Graph.transpose g13 ~?= Graph (nodes g13) (fromList [Edge 1 3, Edge 2 1, Edge 3 2])
t_transpose2 = Graph.transpose emptyGraph ~?= emptyGraph

-- Testar union
g14 :: Graph Int
g14 = Graph (fromList [1,2]) (fromList [Edge 2 1])

g15 :: Graph Int
g15 = Graph (fromList [2,3]) (fromList [Edge 3 2])

t_union = Graph.union g14 g15 ~?= Graph (fromList [1,2,3]) (fromList [Edge 2 1, Edge 3 2])

-- Testar bft
t_bft =  bft (Graph (fromList [3,4,1,5,7,1,0]) (fromList [Edge 4 3, Edge 0 1, Edge 5 7, Edge 1 3, Edge 5 4])) (singleton 0) ~?= Graph (fromList [0,1,3]) (fromList [Edge 1 0, Edge 3 1])

-- Testar reachable
g17 :: Graph Int
g17 = Graph { nodes = fromList [1,2,3], edges = fromList [Edge 1 3, Edge 1 2, Edge 3 2] }

g18 :: Graph Int
g18 = Graph { nodes = fromList [1], edges = Set.empty }

t_reachable = [t_reachable1, t_reachable2]
t_reachable1 = reachable g17 1 ~?= fromList [1, 2, 3]
t_reachable2 = reachable g18 3 ~?= fromList [3]

-- Testar isPathOf
g19 :: Graph Int
g19 = Graph { nodes = fromList [1,2,3], edges = fromList [Edge 1 3, Edge 1 2, Edge 2 3] }

g20 :: Graph Int
g20 = Graph { nodes = fromList [1,2,3], edges = fromList [Edge 1 2, Edge 2 3] }

t_isPathOf = [t_isPathOf1, t_isPathOf2, t_isPathOf3]
t_isPathOf1 = isPathOf [ Edge { source = 1, target = 2 }, Edge { source = 2, target = 3 } ] g19 ~?= True
t_isPathOf2 = isPathOf [ Edge { source = 1, target = 2}, Edge { source = 1, target = 3} ] g20 ~?= False
t_isPathOf3 = isPathOf [] g20 ~?= True

-- Testar path
g21 :: Graph Int
g21 = Graph { nodes = fromList [1,2,3,4], edges = fromList [Edge 1 2, Edge 2 3] }

t_path = [t_path1, t_path2]
t_path1 = path g21 1 3 ~?= Just [Edge 1 2, Edge 2 3]
t_path2 = path g21 2 4 ~?= Nothing

-- Testar topo
g22 :: Graph Int
g22 = Graph { nodes = fromList [1,2,3,4], edges = fromList [Edge 1 2, Edge 1 3, Edge 2 4] }

g23 :: Graph Int
g23 = Graph (fromList []) (fromList [])

t_topo = [t_topo1, t_topo2]
t_topo1 = topo g22 ~?= [fromList [1], fromList [2,3], fromList [4]]
t_topo2 = topo g23 ~?= []

-- Tarefa 1
--
-- Defina testes unitários para todas as funções do módulo Graph,
-- tentando obter o máximo de cobertura de expressões, condições, etc.
--
           
main = runTestTT $ TestList t_list

--
-- Teste aleatório
--

--
-- Tarefa 2
--
-- A instância de Arbitrary para grafos definida abaixo gera grafos
-- com muito poucas arestas, como se pode constatar testando a
-- propriedade prop_valid.
-- Defina uma instância de Arbitrary menos enviesada.
-- Este problema ainda é mais grave nos geradores dag e forest que
-- têm como objectivo gerar, respectivamente, grafos que satisfazem
-- os predicados isDag e isForest. Estes geradores serão necessários
-- para testar propriedades sobre estas classes de grafos.
-- Melhore a implementação destes geradores por forma a serem menos enviesados.
--

-- Instância de Arbitrary para arestas
instance Arbitrary v => Arbitrary (Edge v) where
    arbitrary = do s <- arbitrary
                   t <- arbitrary
                   return $ Edge {source = s, target = t}


instance (Ord v, Arbitrary v) => Arbitrary (Graph v) where
    arbitrary = do ns <- arbitrary
                   case ns of
                      [] -> return (Graph Set.empty Set.empty)
                      _  -> do es <- listOf (createEdge ns)
                               return $ Graph (fromList ns) (fromList es)

prop_valid :: Graph Int -> Property
prop_valid g = collect (length (edges g)) $ isValid g

-- Gerador de DAGs
dag :: (Ord v, Arbitrary v) => Gen (DAG v)
dag = do ns <- arbitrary
         case ns of
            [] -> return $ Graph Set.empty Set.empty
            _  -> do num <- choose (0, 90)
                     genDAG (Graph (fromList ns) Set.empty) num

      where genSafeEdge :: Ord v => Graph v -> Int -> Gen (Maybe (Edge v))
            genSafeEdge _ 9 = return Nothing
            genSafeEdge g n = do let ns = toList $ nodes g
                                 edge <- createEdge ns
                                 let g' = Graph (fromList ns) (Set.insert edge (edges g))
                                 if source edge `notMember` reachable g (target edge) then return (Just edge)
                                 else genSafeEdge g (n+1)
 
            genDAG :: Ord v => Graph v -> Int -> Gen (Graph v)
            genDAG g 0 = return g
            genDAG g n = do g' <- genDAG g (n-1)
                            edge <- genSafeEdge g' 0
                            case edge of
                                Nothing -> return g
                                (Just edge) -> return $ Graph (nodes g') (Set.insert edge (edges g'))
                            
                            
prop_dag :: Property
prop_dag = forAll (dag :: Gen (DAG Int)) $ \g -> collect (length (edges g)) $ isDAG g

-- Gerador de florestas
forest :: (Ord v, Arbitrary v) => Gen (Forest v)
forest = do ns <- arbitrary
            case ns of
               [] -> return $ Graph Set.empty Set.empty
               _  -> do num <- choose (0, 100)
                        genForest (Graph (fromList ns) Set.empty) num

      where genSafeEdge :: Ord v => Graph v -> Int -> Gen (Maybe (Edge v))
            genSafeEdge _ 9 = return Nothing
            genSafeEdge g n = do let ns = toList $ nodes g
                                 edge <- createEdge ns
                                 let g' = Graph (fromList ns) (Set.insert edge (edges g))
                                 if isForest g' then return (Just edge)
                                 else genSafeEdge g (n+1)
 
            genForest :: Ord v => Graph v -> Int -> Gen (Graph v)
            genForest g 0 = return g
            genForest g n = do g' <- genForest g (n-1)
                               edge <- genSafeEdge g' 0
                               case edge of
                                   Nothing -> return g
                                   (Just edge) -> return $ Graph (nodes g') (Set.insert edge (edges g'))
                            
prop_forest :: Property
prop_forest = forAll (forest :: Gen (Forest Int)) $ \g -> collect (length (edges g)) $ isForest g

--
-- Tarefa 3
--
-- Defina propriedades QuickCheck para testar todas as funções
-- do módulo Graph.
--

-- quickCheck swap
prop_swap :: Edge Int -> Property
prop_swap e = source e == target (swap e) .&&. target e == source (swap e)

-- quickCheck empty
prop_empty :: Property
prop_empty = Set.null (nodes Graph.empty) .&&. Set.null (edges Graph.empty)

-- quickCheck isEmpty
prop_isEmpty :: Graph Int -> Property
prop_isEmpty g = case Set.null (nodes g) && Set.null (edges g) of
                     True  -> isEmpty g === True
                     False -> isEmpty g === False

--quickCheck isValid
prop_isValid :: Ord v => Graph v -> Property
prop_isValid g = Set.isSubsetOf (Set.map source (edges g)) (nodes g) .&&. Set.isSubsetOf (Set.map target (edges g)) (nodes g) 

--quickCheck isDAG
prop_isDAG :: Ord v => Graph v -> Property
prop_isDAG g = let prop1 = forAll (dag) adjx
               in prop1

       where adjx :: Graph Int -> Property
             adjx g = forAll (elements $ elems $ nodes g) (\n -> adjxAux n g)

             adjxAux :: Int -> Graph Int -> Bool
             adjxAux n g = let adjs = Prelude.map target (toList $ adj g n)
                               r = reachable g n
                           in Set.notMember n r

--quickCheck isForest

--quickCheck isSubgraphOf 

-- Exemplo de uma propriedade QuickCheck para testar a função adj          
prop_adj :: (Show v, Ord v) => Graph v -> Property
prop_adj g = forAll (elements $ elems $ nodes g) $ \v -> adj g v `isSubsetOf` edges g

-- quickcheck transpose
prop_transpose :: Graph Int -> Property
prop_transpose g = nodes (Graph.transpose g) == nodes g .&&. Set.map swap (edges (Graph.transpose g)) == edges g

-- quickCheck union
prop_union :: Graph Int -> Graph Int -> Property
prop_union g1 g2 = let g = Graph.union g1 g2
                       nodesG1 = size (Set.filter (\ v -> member v (nodes g)) (nodes g1)) == size (nodes g1)
                       nodesG2 = size (Set.filter (\ v -> member v (nodes g)) (nodes g2)) == size (nodes g2)
                       edgesG1 = size (Set.filter (\ e -> member e (edges g)) (edges g1)) == size (edges g1)
                       edgesG2 = size (Set.filter (\ e -> member e (edges g)) (edges g2)) == size (edges g2)
                       noExtraNodes = Set.null $ (nodes g1 `Set.union` nodes g2) Set.\\ (nodes g)
                       noExtraEdges = Set.null $ (edges g1 `Set.union` edges g2) Set.\\ (edges g)
                   in conjoin [nodesG1, nodesG2, edgesG1, edgesG2, noExtraNodes, noExtraEdges]

-- quickCheck bft
prop_bft :: Graph Int -> Property
prop_bft g = property $ aux g
    where aux :: Graph Int -> Gen Bool
          aux g | size (nodes g) == 0 = return True
                | otherwise =  do i <- elements $ elems $ nodes g
                                  let res = bft g (singleton i)
                                  case size (edges res) > 0 of
                                     False -> return True
                                     True  -> do edg <- elements $ elems $ edges res
                                                 let prop1 = isForest res
                                                 let prop2 = (edges res) `isSubsetOf` (edges $ Graph.transpose g)
                                                 let prop3 = checkAdj g (singleton i) == nodes res
                                                 let prop4 = (swap edg) `member` (minimumClassified $ Set.filter (\ x -> source (fst x) == target edg) (classifyEdges g i))
                                                 return (prop1 && prop2 && prop3 && prop4)


-- quickCheck reachable
prop_reachable :: Graph Int -> Property
prop_reachable g = case Set.null (nodes g) of 
                    True  -> property True
                    False -> forAll (elements $ elems $ nodes g) (\x -> checkAdj g (singleton x) == reachable g x)

-- quickCheck isPathOf
prop_isPathOf :: Graph Int -> Property
prop_isPathOf g = property $ aux g

   where aux :: Graph Int -> Gen Bool
         aux g | Set.null $ edges g = return $ isPathOf [] g == True
               | otherwise = do path <- listOf $ createEdge $ toList (nodes g)
                                return $ isPathOf path g == (all (\ x -> x `member` edges g) path
                                                             && 
                                                             and (zipWith (\ x y -> target x == source y) path (tail path)))

-- quickCheck path
prop_path :: Graph Int -> Int -> Int -> Property
prop_path g x y | x == y = property $ path g x y == Just []
                | size (nodes g) <= 1 = property $ path g x y == Nothing
                | otherwise = property $ genPath g
                                 

                where genPath :: Graph Int -> Gen Bool
                      genPath g = do x <- elements $ elems $ nodes g
                                     y <- elements $ elems $ (Set.delete x $ nodes g)
                                     let p = path g x y
                                     return $ checkPath g x y p

                      checkPath :: Graph Int -> Int -> Int -> Maybe (Graph.Path Int) -> Bool
                      checkPath g start end path = let forest = bft g (singleton start)
                                                       prop1 = end `member` Set.map (target . swap) (edges forest)
                                                       in case path of
                                                            Nothing -> prop1 == False
                                                            (Just p) -> prop1 && all (\ x -> swap x `member` (edges forest)) p

-- quickCheck topo
prop_topo :: Property
prop_topo = let prop1 = forAll (dag) checkAdjs
                prop2 = forAll (dag) sameNodes 
                prop3 = forAll (dag) sameSize
             in prop1 .&&. prop2 .&&. prop3

      where checkAdjs :: Graph Int -> Property
            checkAdjs g = case isEmpty g of
                           True -> topo g === []
                           False -> case Set.null (edges g) of
                                     True -> topo g === [nodes g]
                                     False -> forAll (elements $ elems $ nodes g) (\n -> checkAdjAux n g)

            checkAdjAux :: Int -> Graph Int -> Bool
            checkAdjAux node g = let nodeInd = topo g `index` node
                                     adjs = Prelude.map target (toList $ adj g node)
                                     adjInd = Prelude.map (\n -> (topo g) `index` n) adjs
                                  in if adjInd == [] then True
                                     else nodeInd < (minimum $ adjInd)

            sameNodes :: DAG Int -> Bool
            sameNodes g = nodes g == unions (topo g)

            sameSize :: DAG Int -> Bool
            sameSize g = size (nodes g) == sum (Prelude.map size (topo g))

------ Auxiliar ------------------------------------------------------------

concatSet :: Ord a => Set (Set a) -> Set a
concatSet s = Set.foldr Set.union Set.empty s

checkAdj :: Graph Int -> Set Int -> Set Int
checkAdj g s = let adjs = Set.map target $ concatSet $ Set.map (adj g) s
                   s' = Set.union s adjs
                in if s == s' then s else checkAdj g s'

index :: Ord a => [Set a] -> a -> Maybe Int
index [] _ = Nothing
index (h:t) x | x `member` h = return 0
              | otherwise = do r <- index t x
                               return (1 + r)

createEdge :: [v] -> Gen (Edge v)
createEdge nodes = do source <- elements nodes
                      target <- elements nodes
                      return (Edge source target)

classifyEdges :: Graph Int -> Int -> Set ((Edge Int, Int))
classifyEdges g ns = fst $ execState aux (Set.empty, ([ns], 0))
            where aux = do (edges, (queue, num)) <- get
                           if Prelude.null queue
                           then return ()
                           else do let c = head queue
                                   let new = Set.filter (\e -> e `notMember` (Set.map fst edges)) (adj g c)
                                   let es = Set.map (\ edg -> (edg, num)) new
                                   put (edges `Set.union` es, (tail queue ++ elems (Set.map (\e -> target $ fst e) es), num+1))
                                   aux

minimumClassified :: Set (Edge Int, Int) -> Set (Edge Int)
minimumClassified list = let min = minimum $ Set.map snd list
                          in Set.map fst (Set.filter (\x -> snd x == min) list)
