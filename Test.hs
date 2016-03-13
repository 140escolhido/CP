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

--
-- Teste unitário
--

t_list = concat  [[t_swap], [t_empty], [t_transpose], [t_union], t_isEmpty, t_isValid,
                  t_isDAG, t_isForest, t_isSubgraphOf, t_adj]

-- Testar swap
t_swap :: Test
t_swap = swap Edge {source = 1, target = 2 } ~?= Edge {source = 2, target = 1}

-- Testar empty
t_empty :: Test
t_empty = let g = Graph.empty
              in Set.null (nodes g) && Set.null (edges g) ~?= True

-- Testar isEmpty
t_isEmpty = [t_isEmpty1, t_isEmpty2]

g1 :: Graph Int
g1 = Graph { nodes = fromList [1,2], edges = fromList [Edge 1 2] }

g2 :: Graph Int
g2 = Graph { nodes = fromList[], edges = fromList [] }

t_isEmpty1 = isEmpty g1 ~?= False
t_isEmpty2 = isEmpty g2 ~?= True

-- Testar isValid, isDAG, isForest
t_isValid = [t_isValid1, t_isValid2, t_isValid3]
t_isDAG = [t_isDAG1, t_isDAG2]
t_isForest = [t_isForest1, t_isForest2]

g3 :: Graph Int
g3 = Graph { nodes = fromList [1], edges = fromList [Edge 1 3] }

g4 :: Graph Int
g4 = Graph { nodes = fromList [1], edges = fromList [Edge 3 1] }

g5 :: Graph Int
g5 = Graph { nodes = fromList [1,2,3], edges = fromList [Edge 1 2, Edge 2 3, Edge 3 1] }

g6 :: Graph Int
g6 = Graph { nodes = fromList [1,2,3], edges = fromList [Edge 1 2, Edge 1 3] }

g7 :: Graph Int
g7 = Graph { nodes = fromList [1,2,3], edges = fromList [Edge 1 3, Edge 2 3] }

t_isValid1 = isValid g3 ~?= False
t_isValid2 = isValid g4 ~?= False
t_isValid3 = isValid g5 ~?= True

t_isDAG1 = isDAG g5 ~?= False
t_isDAG2 = isDAG g6 ~?= True

t_isForest1 = isForest g6 ~?= False
t_isForest2 = isForest g7 ~?= True

-- Testar isSubgraphOf
t_isSubgraphOf = [t_isSubgraphOf1, t_isSubgraphOf2, t_isSubgraphOf3, t_isSubgraphOf4]

g8 :: Graph Int
g8 = Graph { nodes = fromList [1,2,3,4], edges = fromList [Edge 1 3] }

g9 :: Graph Int
g9 = Graph { nodes = fromList[1,2,3], edges = fromList[Edge 1 3, Edge 1 2] }

t_isSubgraphOf1 = isSubgraphOf g8 g7 ~?= False
t_isSubgraphOf2 = isSubgraphOf g9 g7 ~?= False
t_isSubgraphOf3 = isSubgraphOf g7 g7 ~?= True
t_isSubgraphOf4 = isSubgraphOf (Graph { nodes = Set.empty, edges = Set.empty }) g7 ~?= True

-- Testar adj
g10 :: Graph Int
g10 = Graph { nodes = fromList [1], edges = Set.empty }

g11 :: Graph Int
g11 = Graph { nodes = fromList [1,2,3], edges = fromList [Edge 1 2, Edge 1 3, Edge 2 3] }

t_adj = [t_adj1, t_adj2]
t_adj1 = adj g10 1 ~?= fromList []
t_adj2 = adj g11 1 ~?= fromList [Edge 1 2, Edge 1 3]

-- Testar transpose
g12 :: Graph Int
g12 = Graph { nodes = fromList [1,2,3], edges = fromList [Edge 1 2, Edge 2 3, Edge 3 1] }

t_transpose = transpose g12 ~?= Graph { nodes = nodes g12, edges = fromList [Edge 1 3, Edge 2 1, Edge 3 2] }

-- Testar union
g13 :: Graph Int
g13 = Graph { nodes = fromList [1,2], edges = fromList [Edge 2 1] }

g14 :: Graph Int
g14 = Graph { nodes = fromList [2,3], edges = fromList [Edge 3 2] }

t_union = Graph.union g13 g14 ~?= Graph { nodes = fromList [1,2,3], edges = fromList [Edge 2 1, Edge 3 2] }

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
dag = arbitrary `suchThat` isDAG

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

-- Exemplo de uma propriedade QuickCheck para testar a função adj          
prop_adj :: Graph Int -> Property
prop_adj g = forAll (elements $ elems $ nodes g) $ \v -> adj g v `isSubsetOf` edges g
