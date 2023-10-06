# Libctrl version 1.0.0-alpha.2 (latest)

This prerelease includes some bug fixes:

* Plugged a memory leak in the implementation of `callcc`.
* Fixed a bug in the implementation of `callcc` which would occur
  whenever the capture continuation would escape the scope of its
  binding occurrence without being invoked at least once.
* Renamed the `C` operator `resume` to `throw`.
* Slightly generalised the type of `C`'s `throw` operator.

# Libctrl version 1.0.0-alpha.1

This is an initial prerelease. This version includes the following
operators:

* McCarthy's `amb`.
* Scheme's `call/cc` (aka. Reynolds' `escape`).
* Filinski's monadic reflection for any monad.
* Kiselyov's interface for programming with typed multi-prompt continuations.
* Danvy and Filinski's `shift` and `reset` (and some of their variations).
* Felleisen's `control` and `prompt`.
* Felleisen'c `C` operator.
