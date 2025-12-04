# Anochi Language Syntax Reference

This document describes the syntax of the Anochi programming language.

---

### Comments

Comments are not yet documented in the parser. (TODO: verify comment syntax)

---

### Literals

#### Integers
````anochi
42
-123
0
````

#### Floats
````anochi
3.14
-2.5
````

#### Strings
````anochi
"hello"
"world"
````

#### Booleans
````anochi
true
false
````

#### Identifiers
Identifiers follow C-style naming rules:
- Must start with a letter or underscore
- Can contain letters, digits, and underscores

````anochi
myVar
_private
count123
````

---

### Variables

#### Variable Declaration (`let`)
````anochi
let x = 42;
let name = "Alice";
let flag = true;
````

#### Typed Variable Declaration
````anochi
let x: i64 = 42;
let point: {x=i64, y=i64} = {x=10, y=20};
````

#### Mutable Assignment
Reassign an existing variable (without `let`):
````anochi
x = 100;
````

---

### Operators

#### Arithmetic Operators
| Operator | Description    |
|----------|----------------|
| `+`      | Addition       |
| `-`      | Subtraction    |
| `*`      | Multiplication |
| `/`      | Division       |

#### Comparison Operators
| Operator | Description           |
|----------|-----------------------|
| `==`     | Equal to              |
| `!=`     | Not equal to          |
| `<`      | Less than             |
| `<=`     | Less than or equal    |
| `>`      | Greater than          |
| `>=`     | Greater than or equal |

#### Logical Operators
| Operator | Description |
|----------|-------------|
| `and`    | Logical AND |
| `or`     | Logical OR  |
| `not`    | Logical NOT |

#### Unary Operators
| Operator | Description       |
|----------|-------------------|
| `-`      | Numeric negation  |
| `not`    | Logical negation  |

---

### Operator Precedence (lowest to highest)

1. Type Union (`|`)
2. Logical Or (`or`)
3. Logical And (`and`)
4. Equality (`==`, `!=`)
5. Comparison (`<`, `<=`, `>`, `>=`)
6. Additive (`+`, `-`)
7. Multiplicative (`*`, `/`)
8. Unary (`-`, `not`)
9. Member Access (`.`), Function Call (`!`)
10. Primary (literals, grouping, structs, functions)

---

### Structs (Product Types)

#### Struct Literal
````anochi
{x = 10, y = 20}
{}
{name = "Alice", age = 30}
````

#### Struct Type Definition
````anochi
{x = i64, y = i64}
````

#### Member Access
````anochi
point.x
point.y
{x = 10, y = 20}.x
````

---

### Sum Types (Type Unions)

Use `|` to create union types:
````anochi
i64 | bool
{x = i64} | {y = bool}
````

---

### Functions

#### Function Definition
````anochi
fn input -> output { statement }
````

- `fn` - keyword to define a function
- `input` - input parameter type/value (usually a struct)
- `->` - arrow separating input from output type (optional)
- `output` - return type (optional)
- `{ statement }` - function body

#### Examples

Simple function:
````anochi
let identity = fn x { x };
````

Function with typed input and output:
````anochi
let add = fn {a = i64, b = i64} -> i64 { a + b };
````

Function returning a struct field:
````anochi
let getX = fn {x = i64} -> i64 { x.x };
````

#### Function Call
Use `!` to call a function:
````anochi
function_name!argument
````

Examples:
````anochi
let double = fn x { x + x };
let result = double!5;

let add = fn {a = i64, b = i64} { a + b };
let sum = add!{a = 3, b = 4};
````

---

### Control Flow

#### If Statement
````anochi
if condition { statements }
````

Example:
````anochi
if (x > 10) { x = 10; }
if true { let y = 42; }
````

#### If-Else Statement
````anochi
if condition { statements } else { statements }
````

Example:
````anochi
if (x > 0) { y = 1; } else { y = -1; }
````

---

### Loops

#### Loop (Infinite Loop)
````anochi
loop { statements }
````

#### Break
Exit a loop:
````anochi
loop {
    x = x + 1;
    if (x > 10) { break; }
}
````

#### Continue
Skip to next iteration:
````anochi
loop {
    x = x + 1;
    if (x == 5) { continue; }
    y = y + x;
}
````

---

### Blocks

Group statements with braces:
````anochi
{
    let x = 10;
    let y = 20;
}
````

Blocks create a new scope:
````anochi
let x = 10;
{
    let x = 20;  // shadows outer x
    y = x;       // y = 20
}
// x is still 10 here
````

---

### Grouping

Use parentheses to control precedence:
````anochi
(a + b) * c
(x == 10)
````

---

### Debug Statement

Print expressions for debugging:
````anochi
debug(expr1, expr2, expr3);
````

Example:
````anochi
debug(x, y, x + y);
````

---

### Keywords

| Keyword    | Description                    |
|------------|--------------------------------|
| `let`      | Variable declaration           |
| `fn`       | Function definition            |
| `if`       | Conditional statement          |
| `else`     | Alternative branch             |
| `loop`     | Infinite loop                  |
| `break`    | Exit loop                      |
| `continue` | Skip to next loop iteration    |
| `true`     | Boolean true literal           |
| `false`    | Boolean false literal          |
| `and`      | Logical AND                    |
| `or`       | Logical OR                     |
| `not`      | Logical NOT                    |
| `debug`    | Debug print statement          |
| `for`      | (Reserved, not yet implemented)|

---

### Symbols

| Symbol | Usage                          |
|--------|--------------------------------|
| `=`    | Assignment                     |
| `==`   | Equality comparison            |
| `!=`   | Not equal comparison           |
| `<`    | Less than                      |
| `<=`   | Less than or equal             |
| `>`    | Greater than                   |
| `>=`   | Greater than or equal          |
| `+`    | Addition                       |
| `-`    | Subtraction / Negation         |
| `*`    | Multiplication                 |
| `/`    | Division                       |
| `!`    | Function call                  |
| `.`    | Member access                  |
| `:`    | Type annotation                |
| `;`    | Statement terminator           |
| `,`    | Separator (in structs)         |
| `|`    | Type union                     |
| `->`   | Function return type           |
| `{}`   | Block / Struct                 |
| `()`   | Grouping                       |

---

### Complete Example

````anochi
// Define a point type
let Point = {x = i64, y = i64};

// Create a point
let p: {x = i64, y = i64} = {x = 10, y = 20};

// Function to add two points
let addPoints = fn {a = {x = i64, y = i64}, b = {x = i64, y = i64}} -> {x = i64, y = i64} {
    {x = a.x + b.x, y = a.y + b.y}
};

// Loop example
let sum = 0;
let i = 0;
loop {
    i = i + 1;
    if (i > 10) { break; }
    sum = sum + i;
}

debug(sum);
````
