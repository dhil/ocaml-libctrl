# Libctrl: a playground for programming with control

[![Libctrl build, install, and run testsuite](https://github.com/dhil/ocaml-libctrl/actions/workflows/ci.yml/badge.svg)](https://github.com/dhil/ocaml-libctrl/actions/workflows/ci.yml)

Libctrl (pronounced /lɪb/ /kənˈtrəʊl/) is programming library which
codifies a wide range of control phenomena on top of OCaml's native
effect handlers.

Currently, this library offers implementations of the following
control operators:

* McCarthy's `amb`.
* Scheme's `call/cc` (aka. Reynolds' `escape`).
* Filinski's monadic reflection for any monad.
* Kiselyov's interface for programming with typed multi-prompt continuations.
* Danvy and Filinski's `shift` and `reset` (and some of their variations).

<hr />

This work is largely based on Appendix A of [my PhD
thesis](https://dhil.net/research/papers/thesis.pdf).
