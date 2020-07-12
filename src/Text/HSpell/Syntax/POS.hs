module Text.HSpell.Syntax.POS where

import Data.Text (Text)

data PartOfSpeech
  = Noum                    -- ^ @(n)@: refers to a person place or thing.
  | NoumPlural (Maybe Text) -- ^ @(n pl)@: refers to a group of persons or things.
  | Adjective               -- ^ @(a)@: describe noums or pronoums.
  | Adverb                  -- ^ @(adv)@: gives information about a verb, adjective.
                            --   another adverb, or a sentence.
  | Pronoum                 -- ^ @(pron)@: used to replace a noum.
  | Preposition             -- ^ @(prep)@: used before a noum, a noum phrase, or a
                            --   pronoum, connecting ti to another word.
  | Plural (Maybe Text)     -- ^ @(pl)@: plural of some word.
  | Conjunction             -- ^ @(conj)@: used to link two parts of a sentence.
  | Article                 -- ^ @(art)@: defines a noum
  | Verb [VerbForm] (Maybe Text)
  deriving (Eq , Show)

data VerbForm
  = PastParticiple    -- | @(p p)@: eat, ate, /eaten/.
  | PastSimple        -- | @(imp)@: eat, /ate/, eaten.
  | PresentParticiple -- | @(p pr)@: eat, /eating/. 
  | Transitive        -- | @(v t)@: a verb that has an object.
  | Intransitive      -- | @(v i)@: a verb that has no object.
  deriving (Eq , Show)
