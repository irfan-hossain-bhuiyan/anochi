# Anochi Programming Language - Development Tasks

## Project Roadmap
This document outlines the step-by-step implementation plan for the Anochi programming language, organized by development phases.

## Phase 1: Tokenization (Current Focus)

### Task 1.1: Core Token Types
- [ ] Design and implement the main `Token` enum in Rust
- [ ] Include token variants for:
  - Identifiers (following C identifier format)
  - Integer literals
  - Float literals  
  - String literals (with multiline support)
  - Error tokens for parsing failures
- [ ] Each token type should store its associated value where applicable

### Task 1.2: Identifier Tokenization
- [ ] Implement identifier parsing following C conventions:
  - Must start with letter or underscore
  - Can contain letters, digits, and underscores
  - Case-sensitive
- [ ] Validate identifier format during tokenization

### Task 1.3: Numeric Literal Tokenization
- [ ] Integer literal parsing (decimal, hexadecimal, binary, octal)
- [ ] Float literal parsing (standard and scientific notation)
- [ ] Handle numeric parsing errors gracefully

### Task 1.4: String Literal Tokenization
- [ ] Basic string parsing with escape sequences
- [ ] Multiline string support
- [ ] String interpolation (decide format later)

### Task 1.5: Error Handling
- [ ] Implement Error token type for invalid input
- [ ] Provide meaningful error messages with position information
- [ ] Graceful recovery from tokenization errors

### Task 1.6: Future Considerations
- [ ] Keyword tokens (to be defined in later phases)
- [ ] Operator tokens (to be defined in later phases)
- [ ] Comment handling (to be defined in later phases)

## Future Phases (To Be Detailed Later)
- Phase 2: Parsing and AST Construction
- Phase 3: Semantic Analysis
- Phase 4: Code Generation
- Phase 5: Standard Library
- Phase 6: Tooling and CLI
