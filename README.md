# Little Scheme

A little scheme implementation

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
2. (Not Zsh) https://github.com/direnv/direnv/blob/master/docs/hook.md
3. `cabal build` and others commands are working now.

##### Reference

- [R5RS](https://schemers.org/Documents/Standards/R5RS/)
