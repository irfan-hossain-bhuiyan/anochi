
Got it 👍


ArgList ::= /* empty */
          | Expr
          | Expr "," ArgList
You want a **CFG (Context-Free Grammar)** that can generate or parse **mathematical expressions**.

Here’s a classic CFG for arithmetic expressions with addition, subtraction, multiplication, division, parentheses, and numbers:

---

### CFG for Arithmetic Expressions

**Non-terminals**:

* $E$ → Expression
* $T$ → Term
* $F$ → Factor
* $N$ → Number
* $D$ → Digit

**Terminals**: `+`, `-`, `*`, `/`, `(`, `)`, `0–9`

---

**Production rules:**

1. $E \to E + T \mid E - T \mid T$
2. $T \to T * F \mid T / F \mid F$
3. $F \to (E) \mid N$
4. $N \to N D \mid D$
5. $D \to 0 \mid 1 \mid 2 \mid 3 \mid 4 \mid 5 \mid 6 \mid 7 \mid 8 \mid 9$

---

### Example derivation

Expression:
$(3 + 4) * 5$

* $E \Rightarrow T$
* $T \Rightarrow T * F$
* $T \Rightarrow F$
* $F \Rightarrow (E)$
* $E \Rightarrow E + T$
* $E \Rightarrow T + T$
* $T \Rightarrow F$ → $F \Rightarrow N$ → $N \Rightarrow D \Rightarrow 3$
* $T \Rightarrow F$ → $F \Rightarrow N$ → $N \Rightarrow D \Rightarrow 4$
* $F \Rightarrow N \Rightarrow D \Rightarrow 5$

So final: $(3+4)*5$ ✅

---

👉 Do you want me to extend this grammar to also handle **exponentiation (`^`) and unary minus (like -3)**, or just keep it simple with +, -, \*, /?
