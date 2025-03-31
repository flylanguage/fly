# Fly Programming Language

This repository holds the source code for the Fly compiler


## Building

The Fly compiler executable is built using [`dune`](https://dune.readthedocs.io/en/stable/index.html) v3.17.
Download dune with [`opam`](https://opam.ocaml.org/) (below).
If you don't have opam, get opam.

```bash
opam install dune

dune --version # should be >= 3.17
```


To compile the compiler:

```bash
dune build
```

This will create the compiler executable in `./_build/default/src/` as `fly.exe`

You can also run the compiler using dune:

```bash
dune exec src/fly.exe
```

