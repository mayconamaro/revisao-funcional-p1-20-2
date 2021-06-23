---
author: Programação Funcional
title: Aula de Revisão 
date: Maycon Amaro
---

Introdução
===

Realizaremos juntos alguns exercícios envolvendo listas,
tipos de dados algébricos e funções de ordem superior.
Durante a aula e também ao final, eventuais dúvidas
podem ser levantadas para esclarecimento.
Utilizaremos o VSCode com a extensão para apenas destacar
a sintaxe de Haskell e o Stack em linha de comando para 
compilação e teste manual. A diretiva para mostrar avisos
estará ativa.

\begin{code}
{-# OPTIONS_GHC -Wall #-}

module Revisao where
\end{code}

Exercícios
===

Primeiro vamos implementar um *flattening* de listas.
Dada uma lista de listas de um determinado tipo, vamos 
concatenar as sub-listas para formar uma lista de uma 
dimensão só. Faremos de várias formas, mas primeiro 
tentaremos com recursão.

\begin{code}
exemploFlat :: [[Int]]
exemploFlat = [[11, 4, 8, 15, 22], [1, 3, 5], [7, 2, 9, 13]]

flatRec :: [[a]] -> [a]
flatRec []       = []
flatRec (xs:xss) = xs ++ flatRec xss 
\end{code}

Agora vamos usar funções de ordem superior.

\begin{code}
flat :: [[a]] -> [a]
flat xss = foldr step [] xss
    where
        step xs acc = foldr step' acc xs
        step' x acc' = x : acc'
\end{code}

E ainda em estilo *point-free* =D

\begin{code}
flat' :: [[a]] -> [a]
flat' = foldr (flip (foldr (:))) []
\end{code}

Vamos implementar agora uma função que executa
uma operação sobre todos os elementos das sublistas
de uma lista de listas, usando funções de ordem superior.

\begin{code}
map2 :: (a -> b) -> [[a]] -> [[b]]
map2 f xss = map f' xss
    where 
        -- map :: ([a] -> [b]) -> [[a]] -> [[b]]

        -- f' :: [a] -> [b]
        f' xs = map f xs

map2' :: (a -> b) -> [[a]] -> [[b]]
map2' = map . map 
\end{code}

E se pra cada lista eu quiser uma função diferente?

\begin{code}
exemploFuncoes :: [(Int -> Int)]
exemploFuncoes = [(+ 1), (+ 2), (* 2)]

mapApply :: [(a -> b)] -> [[a]] -> [[b]]
mapApply fs xss = map f' ys
    where 
        ys = zip fs xss -- [(a->b, [a])]
        f' (f, xs) = map f xs -- f' :: (a->b, [a]) -> b
\end{code}

Pra finalizar, considere a seguinte definição 
sobre números inteiros.

\begin{code}
data NatSemZero = One 
                | Suc NatSemZero
                deriving (Eq, Ord, Show)

data Inteiro = Neg NatSemZero 
             | Zero 
             | Pos NatSemZero 
             deriving (Eq, Show)
\end{code}

Vamos definir a seguinte função que converte um número 
inteiro nessa representação. Depois faremos o caminho inverso.

\begin{code}
intToInteiro :: Int -> Inteiro
intToInteiro n
    | n > 0 = Pos (convert' n)
    | n < 0 = Neg (convert' (abs n))
    | otherwise = Zero
    where
        convert' 1 = One -- :: Int -> NatSemZero  
        convert' x = Suc (convert' (x-1))

convert :: NatSemZero -> Int
convert One     = 1
convert (Suc x) = 1 + convert x

inteiroToInt :: Inteiro -> Int
inteiroToInt (Neg x) = negate (convert x)
inteiroToInt Zero    = 0
inteiroToInt (Pos x) = convert x
\end{code}