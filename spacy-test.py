import spacy, sys

# Load English tokenizer, tagger, parser and NER
nlp = spacy.load("en_core_web_sm")

def show_token(tok):
    if tok.pos_ == 'PUNCT':
        return '\\' + tok.text
    else:
        return tok.text

def analyze(line):
  doc = nlp(line)
  for sent in doc.sents:
      toks = ['{0}:{1}:{2}'.format(show_token(token), token.pos_, token.tag_) for token in sent]
      [print(t, end=';') for t in toks]
      print()

if __name__ == '__main__':
    for line in sys.stdin:
        analyze(line.rstrip())
        
