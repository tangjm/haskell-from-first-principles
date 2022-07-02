module Madlibs where

import Data.Monoid

type Verb = String
type Adjective = String
type Adverb = String
type Noun = String
type Exclamation = String

madlibbin' :: Exclamation 
           -> Adverb 
           -> Noun 
           -> Adjective 
           -> String

madlibbin' e adv noun adj =
  e <> "! he said " <>
  adv <> " as he jumped into his car " <>
  noun <> " and drove off with his " <>
  adj <> " wife."

madlibbinBetter' :: Exclamation 
                 -> Adverb 
                 -> Noun 
                 -> Adjective 
                 -> String

madlibbinBetter' d adv noun adj = 
  mconcat [
    d, "! he said ",
    adv, " as he jumped into his car ",
    noun, " and drove off with his ",
    adj, " wife."
  ] 
