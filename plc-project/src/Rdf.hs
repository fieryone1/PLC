module Rdf where

import Data.RDF (RDF, TList, uniqTriplesOf, subjectOf, predicateOf, objectOf)
import Data.RDF.Types (Node(..), LValue(..), RdfParser(..), Triple, ParseFailure)
import Text.RDF.RDF4H.TurtleParser (TurtleParser(..))
import Data.Text (unpack)



data RDFNode = Str String | Num Int | URI String deriving (Eq,Show)
type RDFTriple = (RDFNode, RDFNode, RDFNode)

type Graph = [RDFTriple]

loadG :: String -> IO Graph
loadG name = do
  result <- parseFile (TurtleParser Nothing Nothing) (name ++ ".ttl")
  case (result :: Either ParseFailure (RDF TList)) of
    Left err  -> error (show err)
    Right rdf -> return (map toTriple (uniqTriplesOf rdf))

toTriple :: Triple -> (RDFNode, RDFNode, RDFNode)
toTriple t = (toNode (subjectOf t), toNode (predicateOf t), toNode (objectOf t))


toNode :: Node -> RDFNode
toNode (UNode uri)          = URI (unpack uri)
toNode (LNode (PlainL s))   = Str (unpack s)
toNode (LNode (TypedL s _)) = case reads (unpack s) of
                                   [(n, "")] -> Num n
                                   _         -> Str (unpack s)
toNode _                    = error "Unsupported node type"