# Shakespeare Text Generation using Markov Chains  

## 📌 Group Information  
- **Course**: Extended Statistical Programming (Proj1 – Group 33)  
- **Group Members**:  
  - Shuo Li (s2795688)  
  - Zhe Zhu (s2841606)  
  - Antrea Filippou (s2766374)  

- **Contributions**:  
  - Shuo Li: single word token selection at random, function to simulate a sentence (33%)
  - Zhe Zhu: text pre-processing, create common words vector (34%)
  - Antrea Filippou: make the matrices of common word token sequences, next.word function (33%)

---

## 📖 Introduction  
This project implements a text generation model using **Markov Chains** on *Shakespeare’s Complete Works*.  
The main objectives are:  

1. Pre-process Shakespeare’s text to remove irrelevant symbols and standardize tokens.  
2. Build a vocabulary of the most frequent words.  
3. Implement a Markov Chain–based model to predict the next word.  
4. Generate sentences using different Markov orders (*m = 3, 4, 5*) and compare the outputs.  

---

## 📂 Files in Submission  
- `proj1.R` – main R script (pre-processing, model building, simulation).  
- `shakespeare.txt` – input dataset.  
- `README.md` – documentation.  

---

## ⚙️ Implementation Steps  

### Step 4 – Pre-processing  
- Remove stage directions `[ ... ]`.  
- Remove fully uppercase words (character names, headings) and Arabic numerals.  
- Remove `_` and `-`.  
- Separate punctuation marks into independent tokens.  
- Convert all tokens to lowercase.  

### Step 5 – Vocabulary  
- Build a vocabulary of unique words.  
- Keep the **top 1000 most frequent words**.  

### Step 6 – Token Sequences  
- Map tokens to vocabulary indices.  
- Build an `(n – mlag) × (mlag + 1)` matrix `M`.  

### Step 7 – Next Word Function  
- Implement `next.word()` to sample the next token based on context.  

### Step 8 – Random Start Word  
- Select a random starting word (no punctuation).  

### Step 9 – Sentence Simulation  
- Generate sentences with constraints:  
  - Each clause ≥ 5 words before punctuation.  
  - Sentence length ≤ 30 words.  
  - Ensure sentences end with `"."`.  

### Step 10 – Running Models  
- Run simulations with Markov orders `m = 3, 4, 5`.  
- Compare outputs for fluency and coherence.  

---

   git clone https://github.com/yourusername/proj1-markov.git
   cd proj1-markov
