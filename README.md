# Little Scheme

A little scheme implementation

### Development

1. Install [direnv](https://github.com/direnv/direnv)
2. (If Zsh) Edit your `~/.zshrc` and add following code at the end
    ```
    eval "$(direnv hook zsh)"
    ```
2. (Not Zsh) https://github.com/direnv/direnv/blob/master/docs/hook.md
3. `cabal build` and others commands are working now.

### Using

`little-scheme` supports two ways to evalute scheme program right now.

With one argument such as `little-scheme "(+ 1 2 3)"` would directly evalute expression and return value.

Without argument such as `little-scheme` would create a REPL environment.
