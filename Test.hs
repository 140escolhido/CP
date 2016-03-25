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
-- Número:
-- Nome:
-- Curso:
--


module Main where

import Graph
import Test.HUnit hiding (path)
import Test.QuickCheck
import Data.Set as Set

import Data.List

--
-- Teste unitário
--

t_list = concat  [t_swap, [t_empty], t_transpose, [t_union], t_isEmpty, t_isValid,
                  t_isDAG, t_isForest, t_isSubgraphOf, t_adj]

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
t_isValid = [t_isValid1, t_isValid2, t_isValid3, t_isValid4]
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
t_isValid3 = isValid g5 ~?= False
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
                     [] -> return $ Graph { nodes = Set.empty, edges = Set.empty} 
                     _  -> do es <- listOf (aux ns) 
                              return $ Graph {nodes = fromList ns, edges = fromList es}

                     where aux ns = do src <- elements ns
                                       trg <- elements ns
                                       return $ Edge {source = src, target = trg }
 
prop_valid :: Graph Int -> Property
prop_valid g = collect (length (edges g)) $ isValid g

-- Gerador de DAGs
dag :: (Ord v, Arbitrary v) => Gen (DAG v)
dag = do ns <- arbitrary
         case ns of
           [] -> return $ Graph { nodes = Set.empty, edges = Set.empty }
           _  -> do n <- choose (0, 100)
                    e <- aux n (fromList ns)
                    return $ Graph {nodes = fromList ns, edges = fromList e }

                    where selectEdge :: v -> Set v -> Gen (Edge v)
                          selectEdge src nodes = do trg <- elements $ toList nodes
                                                    return $ Edge {source = src, target = trg}

                          aux :: (Ord v) => Int -> Set v -> Gen [Edge v]
                          aux 0 ns = return []
                          aux n ns = do t <- aux (n-1) ns 
                                        p <- choose (0, length ns - 1)
                                        let g = Graph ns (fromList t)
                                            l = Set.map (\ node -> (node, reachable (Graph.transpose g) node)) ns
                                            (node, r) = elemAt p l
                                            diff = ns Set.\\ r
                                         in case Set.null diff of
                                               True  -> return t
                                               False -> do h <- selectEdge node diff
                                                           return (h:t)

                                          

prop_dag :: Property
prop_dag = forAll (dag :: Gen (DAG Int)) $ \g -> collect (length (edges g)) $ isDAG g

-- Gerador de florestas
forest :: (Ord v, Arbitrary v) => Gen (Forest v)
forest = arbitrary `suchThat` isForest

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
prop_empty = let g = Graph.empty
              in Set.null (nodes g) .&&. Set.null (edges g)

-- Exemplo de uma propriedade QuickCheck para testar a função adj          
prop_adj :: Graph Int -> Property
prop_adj g = forAll (elements $ elems $ nodes g) $ \v -> adj g v `isSubsetOf` edges g

-- quickcheck transpose
prop_transpose :: Graph Int -> Property
prop_transpose g = let g' = Graph.transpose g
                    in nodes g == nodes g' .&&. Set.map (swap) (edges g') == edges g

-- quickCheck union
prop_union :: Graph Int -> Graph Int -> Property
prop_union g1 g2 = let g = Graph.union g1 g2
                       nodes_g1 = size (Set.filter (\ v -> member v (nodes g)) (nodes g1)) == size (nodes g1)
                       nodes_g2 = size (Set.filter (\ v -> member v (nodes g)) (nodes g2)) == size (nodes g2)
                       edges_g1 = size (Set.filter (\ e -> member e (edges g)) (edges g1)) == size (edges g1)
                       edges_g2 = size (Set.filter (\ e -> member e (edges g)) (edges g2)) == size (edges g2)
                       extra_nd = Set.null $ (nodes g1 `Set.union` nodes g2) Set.\\ (nodes g)
                       extra_ed = Set.null $ (edges g1 `Set.union` edges g2) Set.\\ (edges g)
                   in conjoin [nodes_g1, nodes_g2, edges_g1, edges_g2, extra_nd, extra_ed]


prop_topo :: Property
prop_topo = let g = (dag :: Gen (DAG Int))
                prop1 = forAll g checkAdj
                prop2 = sameNodes g
                prop3 = sameSize g
             in prop1 .&&. prop2 .&&. prop3

      where index :: Ord a => [Set a] -> a -> Maybe Int
            index [] _ = Nothing
            index (h:t) x | x `member` h = return 0
                          | otherwise = do r <- index t x
                                           return (1 + r)

            checkAdj :: Graph Int -> Property
            checkAdj g = case isEmpty g of
                           True -> topo g === []
                           False -> case Set.null (edges g) of
                                     True -> topo g === [nodes g]
                                     False -> forAll (elements $ elems $ nodes g) (\ n -> checkAdjAux n g)

            checkAdjAux :: Int -> Graph Int -> Bool
            checkAdjAux node g = let node_ind = topo g `index` node
                                     adjs = Prelude.map target (toList $ adj g node)
                                     adj_ind = Prelude.map (\n -> (topo g) `index` n) adjs
                                  in if adj_ind == [] then True
                                     else node_ind < (minimum $ adj_ind)

            sameNodes :: Gen (DAG Int) -> Gen Bool
            sameNodes g = do g' <- g
                             let n = unions $ topo g'
                             return (n == nodes g')

            sameSize :: Gen (DAG Int) -> Gen Bool
            sameSize g = do g' <- g
                            let s = sum $ Prelude.map size (topo g')
                            return (s == size (nodes g'))
