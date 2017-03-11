# SyMoN

Symbolic model checker for the non deterministic temporal concurrent constraint
programming *ntcc* [1]. The underlying theory was published in [2].


## Installation

### Dependencies

  * [Ocaml](http://ocaml.org)
  * Ocamllex
  * Ocamlyacc

To install dependencies, we advise to use `opam`.

  * For OSX: `brew install opam`

### Compilation

The compilation is as usual:`make`


## References

[1]: Nielsen, M., Palamidessi, C., & Valencia, F. D. (2002). Temporal
Concurrent Constraint Programming: Denotation, Logic and Applications. Nord. J.
Comput., 9(1), 145–188.

[2]: Arias, J., Guzmán, M., & Olarte, C. (2015). A Symbolic Model for Timed
Concurrent Constraint Programming. Electronic Notes in Theoretical Computer
Science, 312, 161–177. https://doi.org/10.1016/j.entcs.2015.04.010.
