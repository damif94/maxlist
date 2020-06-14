{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}


data Nat = Zero | Succ Nat deriving Show


data Natty (n :: Nat) where
  Zy :: Natty 'Zero
  Sy :: Natty n -> Natty ('Succ n)

data Listty (l :: [Nat]) :: * where
  Ny :: Listty '[]
  Cy :: Natty n -> Listty l -> Listty (n:l)


type family Max (n:: Nat) (m::Nat) where 
  Max n Zero = n 
  Max Zero n = n
  Max (Succ m) (Succ n) = Succ (Max m n)

type family MaxL (l :: [Nat]) where
  MaxL '[] = Zero
  MaxL (n:ns) = Max n (MaxL ns)


data MaxValueN (n :: Nat) (m :: Nat)  where
  LeftM ::  (Max n m ~ n) =>  Natty n -> MaxValueN n m
  RightM ::  (Max n m ~ m) =>  Natty m -> MaxValueN n m

  
data MaxValueL (l :: [Nat])  where
  Val ::  (MaxL l ~ n) =>  Natty n -> MaxValueL l


maxn :: Natty m -> Natty n -> MaxValueN m n
maxn Zy n = RightM n
maxn n Zy = LeftM n
maxn (Sy m) (Sy n) = case maxn m n of
                       LeftM _ -> LeftM (Sy m)
                       RightM _ -> RightM (Sy n)


maxl :: Listty l  -> MaxValueL l
maxl Ny = Val Zy
maxl (Cy n ns) = case (maxl ns) of
                   Val k -> case (maxn n k) of
                              RightM _ -> Val k
                              LeftM _ -> Val n

demote :: Natty m -> Nat
demote Zy = Zero
demote (Sy m) = Succ (demote m)


instance Show (MaxValueL l) where
  show m = case m of
           Val x -> show (demote x)


ex = maxl (Cy (Sy Zy) (Cy (Sy Zy) (Cy (Sy Zy) ((Cy (Sy (Sy (Sy  Zy)))) (Cy (Sy Zy) Ny)))))
