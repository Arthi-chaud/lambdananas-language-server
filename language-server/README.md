# Language Server

This folder hosts the actual Language Server for lambdananas.

## A few notes on the architecture

- While we could have used lambdananas as a library, we decided not to. This way, the user to only has to update lambdananas, and not the LSP in case of new rules.
- The project architecture was inspired by Curry's [Language Server](https://github.com/fwcd/curry-language-server/tree/main), as it is simple enough to fit our needs.

## Related sources:

- The [Language Server Protocol Specification](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/)
- Haskell's [`lsp` library](https://github.com/haskell/haskell-language-server) library
- Curry's [Language Server](https://github.com/fwcd/curry-language-server/tree/main)
