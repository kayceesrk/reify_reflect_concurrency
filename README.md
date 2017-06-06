# (Monadic) Reflections on Concurrency

Monadic reflection on concurrency monad.

## Install

    $ opam remote add multicore https://github.com/ocamllabs/multicore-opam.git
    $ opam switch 4.02.2+multicore
    $ make

## Configurations

- [chameneos](https://github.com/kayceesrk/reify_reflect_concurrency/blob/master/chameneos.ml):
  chameneos-redux benchmark in direct-style using algebraic effect handlers. The
  scheduler and MVar implementations are in direct-style.
- [chameneos_monad](https://github.com/kayceesrk/reify_reflect_concurrency/blob/master/chameneos_monad.ml):
  chameneos-redux benchmar in monadic style. The scheduler and MVar
  implementations are monadic. Uses concurrency monad.
- [chameneos_rr](https://github.com/kayceesrk/reify_reflect_concurrency/blob/master/chameneos_rr.ml):
  chameneos-redux benchmark in direct-style using reify-reflect on monadic
  scheduler and MVar. Illustrates that one can recover direct-style without
  changing monadic libraries whole-sale.
- [chameneos_shallow](https://github.com/kayceesrk/reify_reflect_concurrency/blob/master/chameneos_shallow.ml):
  Shallow monadic implementation over direct-style scheduler and MVar. The idea
  is to illustrate that you can get the beneficial type-level marker that you
  get with monadic libraries with a simple shallow embedding. With monadic
  markers, you get automatic mutual exclusion -- context switchest are only
  possible at binds.
