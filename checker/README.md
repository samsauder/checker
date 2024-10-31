# LL(1) Parser for English Syntax
## Synopsis
This is an LL(1) parser module for English that checks if a list of part-of-speech tags is a sentence. The parser accepts words in the language of a context free grammar designed to model English syntax. 

The terminal symbols of this grammar are not concrete words, but abstract POS-tags describing syntactic roles. Phrases can be decomposed into combinations of tags and/or phrases according to a set of production rules. 

---
### Terminals (tags):
*d* - determiners

*n* - nouns
    
*v* - verbs
    
*p* - prepositions 
    
*aj* - adjectives 

*av* - adverbs

---
### Nonterminals (phrases): 
**S** - Sentence

**Np** - Noun Phrase 

**Pp** - Prepositional Phrase

**Vp** - Verb Phrase 

**Np^** - Np helper

**X*** and **Pp*** - Vp helpers

---
### Rules:
**S** -> **Np** **Vp**

**Np** -> d **Np^** | n

**Np^** -> aj n | n

**X*** -> **Np** **Pp*** | av | e

**Pp** -> p **Np**

**Pp*** -> **Pp** | e

**Vp** -> v **X***

---

## Setup
1. clone the repository: https://github.com/samsauder/checker
2. navigate to the *checker* directory
3. enter the racket REPL 



## Usage
Suppose you want to test the grammatically correct sentence: "the person moves this onto that" against the parser. The POS-tag list of this sentence can be analyzed as: *d n v n p n*. To check for syntactic validity, simply give the **S** procedure a list of tags:

```racket
(S '(d n v n p n))
```

The **S** function returns the list of tags if it is a valid sentence, otherwise it prints a syntax error.
