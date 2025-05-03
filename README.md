# Fly Programming Language

This repository holds the source code for the Fly compiler

## Building

### Environment Setup

This project is built using [`opam`](https://opam.ocaml.org/).
If you don't have opam, we recommend installing it through homebrew (Mac).

Mac (homebrew):

```bash
brew install opam
```

Also, the project requires `llvm@14` on your machine.
You don't have to set it up as a default or anything.
As long as it's installed, you're good.

Mac (homebrew):

```bash
❯ brew install llvm@14
...
❯ ls /opt/homebrew/Cellar/llvm@14
14.0.6
```

Also, you'll have to use `ocaml v4` to be able to use the `llvm@14` bindings.
If you're already using version 4, you should be good. If you're using v5, follow these steps:

```bash
# create a new "switch", basically a virtual environment,
# called "ocaml4.14.4" using the v4.14.2 ocaml compiler
❯ opam switch create ocaml4.14.2 ocaml-base-compiler.4.14.2

# ensure we've successfully moved switches
❯ opam switch list
#  switch       compiler                                            description
   default      ocaml-base-compiler.5.3.0,ocaml-options-vanilla.1   ocaml >= 4.05.0
→  ocaml4.14.2  ocaml-base-compiler.4.14.2,ocaml-options-vanilla.1  ocaml-base-compiler = 4.14.2

# do this in every shell that existed prior to the opam switch
# to update your opam environment (or just create a new shell)
❯ eval $(opam env)
```

In some cases, this make still fail.
If so, delete opam from your machine and reinstall. Then re-run the commands above.
This should be fine, as you likely have no important stuff in your opam files:

Mac (homebrew):

```bash
❯ brew uninstall opam --force
❯ rm -rf ~/.opam
❯ brew install opam
```

### Project setup

To install all dependencies:

```bash
❯ opam install . --deps-only -y
```

This project is built using [`dune`](https://dune.readthedocs.io/en/stable/index.html) v3.17.
You should have installed dune as a project dependency in the opam install script above.

To build the compiler:

```bash
dune build
```

This will create the compiler executable in `./_build/default/src/` as `fly.exe`

You can also run the compiler using dune:

```bash
./_build/default/src/fly.exe
# or
dune exec fly
```

To pass program arguments to the executable through dune:

```bash
dune exec fly -- [<flags>] <filename>
```

## Testing

To run all tests on the compiler:

```bash
dune runtest #runs all test files
```

To run all tests in a specific file:

```bash
dune exec test/scanner/test_and.exe # Tests if the scanner correctly parses the AND operator
```
