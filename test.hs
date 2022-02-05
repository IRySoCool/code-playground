{-# LANGUAGE DeriveFunctor #-}
data HalfEdge a = HalfEdge { label :: a , companion :: HalfEdge a } deriving Functor