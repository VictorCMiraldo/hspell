
module Text.HSpell.POS where
{-

data PartOfSpeech
  = Noum            -- ^ @(n.)@: refers to a person place or thing.
  | NoumPlural      -- ^ @(n. pl.)@: refers to a group of persons or things.
  | Adjective       -- ^ @(a.)@: describe noums or pronoums.
  | Adverb          -- ^ @(adv.)@: gives information about a verb, adjective.
                    --   another adverb, or a sentence.
  | Pronoum         -- ^ @(pron.)@: used to replace a noum.
  | Preposition     -- ^ @(prep.)@: used before a noum, a noum phrase, or a
                    --   pronoum, connecting ti to another word.
  | Plural Text     -- ^ @(pl.)@: plural of some word.
  | Conjunction     -- ^ @(conj.)@: used to link two parts of a sentence.
  | Verb [VerbForm]

data VerbForm
  = PastParticiple    -- | @(p. p.)@: eat, ate, /eaten/.
  | PastSimple        -- | @(imp.)@: eat, /ate/, eaten.
  | PresentParticiple -- | @(p. pr.)@: eat, /eating/. 
  | Transitive        -- | @(v. t.)@: a verb that has an object.
  | Intrantitive      -- | @(v. i.)@: a verb that has no object.

-}
