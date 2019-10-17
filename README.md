# Little Scheme

A little scheme implementation

### Installation

To install the interpreter you can:

```
git clone https://github.com/dannypsnl/little-scheme.git
cd little-scheme
runhaskell Setup.hs configure --ghc
runhaskell Setup.hs build
runhaskell Setup.hs install
```

To complete remove all stuffs from Little Scheme you can:

```
little-scheme cleanup
```

### Using

`little-scheme` supports two ways to evalute scheme program right now.

- `little-scheme examples/hello.scm` would evalute `examples/hello.scm`.
- `little-scheme` would create a REPL.

### Contributing

1. Install [direnv](https://github.com/direnv/direnv)
2. (If Zsh) Edit your `~/.zshrc` and add following code at the end
   ```zsh
   eval "$(direnv hook zsh)"
   ```
3. (Not Zsh) https://github.com/direnv/direnv/blob/master/docs/hook.md
4. `cabal build` and others commands are working now.

##### Reference

- [R5RS](https://schemers.org/Documents/Standards/R5RS/)
- [Writing-the-Cyclone-Scheme-Compiler](https://justinethier.github.io/cyclone/docs/Writing-the-Cyclone-Scheme-Compiler)
