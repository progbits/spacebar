# Spacebar

A [C](https://en.wikipedia.org/wiki/The_C_Programming_Language) to
[Whitespace](https://en.wikipedia.org/wiki/Whitespace_(programming_language))
transpiler.

## Getting Started

Spacebar is written in [OCaml](https://ocaml.org/), instructions on how to install
OCaml for your operating system can be found in the [offical
documentation](https://ocaml.org/docs/install.html).

Spacebar uses the [Dune](https://github.com/ocaml/dune) build
system. Instructions on installing Dune can be found in the [project
documentation](https://github.com/ocaml/dune#installation).

Once you have installed OCaml and Dune, the project can be built by running

```shell
dune build
```

Once built, the `spacebar.exe` build artifact can be found in the
`_build/default` directory.

## Usage

Spacebar reads C code from stdin and outputs transpiled Whitespace code to
stdout. For example, to transpile one of the examples in the functional test
suite to a file called `out.ws`:

```shell
cat test/basic_main.c | ./_build/default/spacebar.exe > out.ws
```

## Testing

A [Bats](https://github.com/bats-core/bats-core) based functional test suite is
provided in the [`test`](https://github.com/progbits/spacebar/tree/main/test)
directory. By default the test suite expects an existing Whitespace interpreter
on the path with the name `wspace`. The Spacebar binary used for testing is
sourced from the default build directory (`_build/default/`).

To run the test suite:

```shell
./test/functional.sh
```
