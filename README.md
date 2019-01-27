# µFactor

A scripting language inspired by the [**Forth**](http://www.forth.org/) and [**Factor**](http://factorcode.org/) programming languages.
It is not a general-purpose language but has a reduced feature set so that it would be suitable to run on a microcontroller (e.g. an Arduino) like Forth - but with proper control structures.

# MicroFactor [![Build Status](https://travis-ci.com/ob-fun-ws18/microFactor.svg?branch=master)](https://travis-ci.com/ob-fun-ws18/microFactor)

A Haskell implementation of µFactor, providing a command-line REPL for trying it out on the Desktop.

## Usage
### How to run

Clone the repository and do
```
stack build && stack exec microfactor
```
### How to include
```yaml
# package.yaml
dependencies:
- microfactor >= 0.1
```
```hs
import MicroFactor
import Text.Parsec (runParser, ParseError)

result :: Either ParseError (InterpreterResult ResolvedRef ())
result = do
    code <- runParser expressionParser () "" "(\"Hello World!\") execute"
    bytecode <- resolve [] code
    interpret newThread bytecode
```

Read the [module documentation](https://ob-fun-ws18.github.io/microFactor/index.html) for more!

## About

The REPL is built using [ANSI escape codes](http://www.lihaoyi.com/post/BuildyourownCommandLinewithANSIescapecodes.html).
More elaborate GUI libraries, such as Brick [[1]](https://github.com/jtdaugherty/brick/blob/master/docs/samtay-tutorial.md)[[2]](https://samtay.github.io/articles/brick.html), are available only for Unix.
