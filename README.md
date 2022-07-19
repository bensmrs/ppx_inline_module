# `ppx_inline_module`

`ppx_inline_module` is a PPX rewriter to write one-line dotted expressions on packed modules.

## Usage

```ocaml
[%imod m.f] ()
```

behaves like:

```ocaml
let module M = (val m) in
M.f ()
```