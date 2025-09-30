# proj1
Group Information
Course: Extended Statistical Programming (Proj1 – Group 33)
Group Members:
Shuo Li (s2795688)
Zhe Zhu (s2841606)
Antrea Filippou (s2766374)
Contributions:
Shuo Li: xx% (xx)
Zhe Zhu: xx% (xx)
Antrea Filippou: xx% (xx)

1. Introduction
This project implements a text generation model using Markov Chains on Shakespeare’s Complete Works. The program processes the original text, constructs transition probabilities between tokens, and simulates new sentences that resemble Shakespeare’s style.
The main objectives are:
Pre-process Shakespeare’s text to remove irrelevant symbols and standardize tokens.
Build a vocabulary of the most frequent words.
Implement a Markov Chain–based model to predict the next word.
Generate sentences using different Markov orders (m = 3, 4, 5) and compare the outputs.

2. Files in Submission
proj1.R – main R script containing all steps (pre-processing, model building, simulation).
shakespeare.txt – input dataset containing Shakespeare’s works.
README.md – documentation and usage instructions.

3. Implementation Steps
Step 4 – Pre-processing
Remove stage directions inside square brackets [ ... ].
Remove fully uppercase words (character names, headings) and Arabic numerals.
Remove underscores _ and hyphens -.
Separate punctuation marks (e.g., . , , , !) into independent tokens.
Convert all tokens to lowercase.
Step 5 – Vocabulary
Build a vocabulary from unique tokens.
Count frequencies and keep the top 1000 most common words for modeling.
Step 6 – Token Sequences
Map each token to its vocabulary index.
Build an (n – mlag) × (mlag + 1) matrix M of token sequences, where mlag is the maximum Markov order.
Step 7 – Next Word Function
Implement next.word() which samples the next token given the current context using weighted probabilities.
Step 8 – Random Start Word
Randomly select a starting word from the vocabulary (excluding punctuation).
Step 9 – Sentence Simulation
Generate sentences iteratively using the Markov model.
Enforce constraints:
Each clause must contain at least 5 words before punctuation.
Sentences are capped at 30 words.
If no full stop is reached, the last punctuation is replaced with ".".
Step 10 – Running Models
Run simulations with Markov orders m = 3, 4, 5.
Compare outputs to observe differences in fluency and coherence.
