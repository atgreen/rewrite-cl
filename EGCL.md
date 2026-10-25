# EGCL support

Source parsing preserves comments and whitespace through node serialization. Reader conditional examples referring to SBCL are syntax fixtures, not platform dependencies.

With dependencies discoverable by ASDF, run:

```sh
egcl --no-init --load tests/egcl-smoke.lisp
```

The smoke test selects this checkout before inherited ASDF configuration.
