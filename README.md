# WIP

# rudralang

A general purpose functional programming language with familiar syntax.

Inspired from Clojure, Elixir.

Compiles to [Chez Scheme](https://github.com/cisco/ChezScheme) and to native via [chez-exe](https://github.com/gwatt/chez-exe).

Example factorial program:

```
ns!(fact,
  {} =>

  defn!(fact, {}, [n] =>
    cond(
      equal?(n, 0) => 1,
      :else        => mul(n, fact(sub(n, 1)))
    )
  )

  defn!(main!, {}, args =>
    println!(fact(5))
  )
)
```

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
