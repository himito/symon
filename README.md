# SyMoN

Symbolic model checker for the non deterministic temporal concurrent constraint
programming *ntcc* [1]. The underlying theory was published in:

    Arias, J., Guzmán, M., & Olarte, C. (2015). A Symbolic Model for Timed Concurrent
    Constraint Programming. Electronic Notes in Theoretical Computer Science, 312, 161–177.
    https://doi.org/10.1016/j.entcs.2015.04.010.


## Installation

### Dependencies

We recommend to install the following dependencies using the package manager [opam](https://opam.ocaml.org).

```Bash
  brew install opam
```

 * [Ocaml](http://ocaml.org)
 * Ocamllex
 * [Menhir](http://gallium.inria.fr/~fpottier/menhir/)
 * [OCamlgraph](http://ocamlgraph.lri.fr)

### Build

The compilation is as usual:`make`.

## References

[1] Nielsen, M., Palamidessi, C., & Valencia, F. D. (2002). Temporal
Concurrent Constraint Programming: Denotation, Logic and Applications. Nord. J.
Comput., 9(1), 145–188.

## Note

Folder `utils` contains the necessary files for syntax highlighting of ntcc language.
