# !!EXPERIMENTAL!!

# rudralang

A dynamic general-purpose high-level functional-programming language with familiar syntax that compiles to native binaries.

## Code Examples

Check out the [sample programs](samples/).

## Goals

- **Ergonomic** familiar syntax, destructuring everywhere
- **Extensible** top-level functions are polymorphic by default
- **Immutable data structures** by default
- **Concurrency-friendly** mutability using Clojure-like [`atom`s](https://clojuredocs.org/clojure.core/atom)
- **Full numeric tower** example: no integer overflows, `pow(-1, 0.5)` is `0+i`
- **Recursion-friendly** many algorithms are simpler when defined recursively - they should be written as such

## Notes on this implementation

This repo contains a compiler written in Clojure.

It compiles Rudra source to Chez Scheme, and then to a native binary via [chez-exe](https://github.com/gwatt/chez-exe).

## Installation

### Install Leiningen

[Leiningen](https://leiningen.org)

### Clone this repo

```
$ git clone git@github.com:divs1210/rudralang.git
```

### Install Chez Scheme

#### Mac

```
$ brew install chezscheme
```

#### Others

There are compiled binaries available for all major platforms, otherwise build it yourself from [source](https://github.com/cisco/ChezScheme).

### Install chez-exe

It's really easy to build from [source](https://github.com/gwatt/chez-exe).

## Usage

### Go to the `rudralang` repo

```
$ cd path/to/rudralang
```

### Configure

Update `config.edn` to point to the correct `chez-exe` path.

### Compile a sample program

```
$ lein -compile samples/fact.rudra
```

### Run it

```
$ target/fact 5
```

### REPL

In the rudralang directory, run:

```
$ ./repl
```

## License

Copyright © 2021 Divyansh Prakash

This program and the accompanying materials are made available under the
terms of the Eclipse Public License 2.0 which is available at
http://www.eclipse.org/legal/epl-2.0.

This Source Code may also be made available under the following Secondary
Licenses when the conditions for such availability set forth in the Eclipse
Public License, v. 2.0 are satisfied: GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or (at your
option) any later version, with the GNU Classpath Exception which is available
at https://www.gnu.org/software/classpath/license.html.
