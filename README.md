# JavaScript-Style Calculator

A powerful command-line calculator built in Haskell that supports JavaScript-style variable declarations and arithmetic expressions with proper operator precedence.

![License](https://img.shields.io/badge/license-BSD3-blue.svg)
![Haskell](https://img.shields.io/badge/language-Haskell-purple.svg)
![Stack](https://img.shields.io/badge/build-Stack-orange.svg)

## 🚀 Features

- **JavaScript Syntax**: Familiar `let` and `const` declarations
- **Decimal Arithmetic**: Proper floating-point calculations (35/2 = 17.5)
- **Variable Storage**: Variables persist throughout the session
- **Operator Precedence**: Follows mathematical rules (*, / before +, -)
- **Error Handling**: Division by zero and undefined variable protection
- **Interactive REPL**: Real-time expression evaluation
- **Comments Support**: `//` line comments and `/* */` block comments

## 📋 Supported Syntax

### Variable Declarations
```javascript
let x = 10;           // Variable declaration
const pi = 3.14159;   // Constant declaration  
z = x + y;            // Regular assignment
```

### Mathematical Operations
```javascript
addition = 5 + 3;           // Addition: 8
subtraction = 10 - 4;       // Subtraction: 6
multiplication = 6 * 7;     // Multiplication: 42
division = 35 / 2;          // Division: 17.5
precedence = 5 + 3 * 2;     // Precedence: 11 (not 16)
parentheses = (5 + 3) * 2;  // Parentheses: 16
```

## 🛠️ Installation

### Prerequisites
- [Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/) (Haskell build tool)
- Git

### Build from Source
```bash
# Clone the repository
git clone https://github.com/yourusername/js-calculator.git
cd js-calculator

# Build with Stack
stack build

# Run the calculator
stack exec js-add
```

### Quick Start
```bash
# Interactive mode
stack exec js-add

# Single expression
echo "let result = 35 / 2" | stack exec js-add
```

## 🎯 Usage Examples

### Interactive Session
```
$ stack exec js-add
JavaScript-Style Calculator!
Supports: +, -, *, /, variables, decimals, and JavaScript syntax

calc> let x = 10;
Let declaration: x = 10.0
Result: x = 10.0

calc> let y = 20;
Let declaration: y = 20.0
Result: y = 20.0

calc> result = (x + y) / 2;
Assignment: result = ((10.0 + 20.0) / 2.0)
Result: result = 15.0

calc> const pi = 3.14159;
Const declaration: pi = 3.14159
Result: pi = 3.14159

calc> area = pi * 5 * 5;
Assignment: area = ((3.14159 * 5.0) * 5.0)
Result: area = 78.53975

calc> quit
Goodbye!
```

### Standalone Executable
A pre-built executable with batch launcher is available in the `Calculator-Standalone/` folder:
- **Double-click** `Start Calculator.bat` for guided startup
- **Or run** `JavaScript-Calculator.exe` directly

## 🏗️ Architecture

### Parser (Megaparsec)
- **Lexical Analysis**: Tokenizes JavaScript-style syntax
- **Syntax Analysis**: Builds Abstract Syntax Tree (AST)
- **Operator Precedence**: Handles mathematical precedence correctly

### Evaluator
- **Environment**: Maps variable names to values
- **Type Safety**: Uses `Maybe` for error handling
- **Functional Style**: Clean applicative operations

### REPL
- **Interactive Loop**: Read-Eval-Print-Loop
- **Error Recovery**: Continues after parse/evaluation errors
- **EOF Handling**: Graceful exit on input end

## 🔧 Technical Details

### Built With
- **Language**: Haskell (GHC 9.10.3)
- **Parser**: Megaparsec library
- **Build Tool**: Stack
- **Dependencies**: 
  - `megaparsec` - Parser combinators
  - `text` - Efficient text processing
  - `containers` - Data structures
  - `parser-combinators` - Expression parsing

### Project Structure
```
js-calculator/
├── src/
│   └── Main.hs              # Main calculator implementation
├── Calculator-Standalone/    # Pre-built executables
├── js-add.cabal            # Package configuration
├── stack.yaml              # Stack configuration
├── README.md               # This file
└── LICENSE                 # BSD3 License
```

## 🧪 Development

### Running Tests
```bash
# Build and test
stack build --test

# Run specific tests
stack test
```

### Code Quality
- **Zero Warnings**: Clean compilation
- **Haddock Documentation**: Comprehensive API docs
- **Type Safety**: Leverages Haskell's type system
- **Functional Style**: Immutable data structures

## 🤝 Contributing

1. **Fork** the repository
2. **Create** a feature branch (`git checkout -b feature/amazing-feature`)
3. **Commit** your changes (`git commit -m 'Add amazing feature'`)
4. **Push** to the branch (`git push origin feature/amazing-feature`)
5. **Open** a Pull Request

### Development Setup
```bash
# Clone your fork
git clone https://github.com/yourusername/js-calculator.git

# Install dependencies
stack build --dependencies-only

# Run in development mode
stack ghci
```

## 📝 License

This project is licensed under the **BSD3 License** - see the [LICENSE](LICENSE) file for details.

## 🙏 Acknowledgments

- **Megaparsec** library for excellent parser combinators
- **Stack** for reliable Haskell builds
- **Haskell Community** for the amazing ecosystem

## 🐛 Known Issues

- Variables are session-only (not persistent between runs)
- No support for functions or complex data types
- Limited to basic arithmetic operations

## 🚧 Future Enhancements

- [ ] Mathematical functions (sin, cos, sqrt, etc.)
- [ ] Variable persistence to file
- [ ] Multi-line expressions
- [ ] Function definitions
- [ ] More JavaScript syntax (if/else, loops)
- [ ] Web interface

---

**Made with ❤️ and Haskell** | **Perfect for learning parser techniques!**