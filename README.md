# `hspell`: A Haskell Spellchecker

## Why put another spellchecker out there?

  The spellchecking situation for bare text files is not great. Sure,
we can use a number of individual tools to check the spelling of words 
and these tend to work great for simple or widely used formats such as 
plain text or `(La)TeX`, but these tools offer no grammatical advice, 
nor catch repeated words for example. You can do this (to a certain extent)
with other tools, such as `diction` and `style`, but that's adding another 
tool to the workflow and hopping they play well with whatever file format you're
checking.

  Besides the difficulty of doing spelling and grammatical checks on text files
the general interfaces also frustrate me. It is often the case that while spellchecking
a large document I will missclick on the wrong suggestion. Consequently, if the tool has no
way of _undoing_ the previous choise, I must write it down in a piece of paper 
to remember to go back and fix it manually.

  Finally, I have found no spellchecking library in Haskell and found myself with
plenty of free time in the past month and decided to build something! :)

## Ok, but what's the status so far?

  At this point, this repo hosts a proof-of-concept instead of a fully usable tool.
I hope to get to the point of having a fully usable tool in the future and contributions
are very welcome.

## Great, I want to give it a try!

Clone the repo, then run ```stack build && stack exec hspell -- test.in```


