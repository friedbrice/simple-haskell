# SimpleHaskell

A low-feature Haskell package for Sublime Text

## Description and Goals

This package aims to support Haskell development on Sublime Text without relying on specific build tools or project structure.

Right now, this package focuses on fixing issues with syntax highlighting and providing some rudimentary completions.

Future directions might involve adding a simple build system based on [ghcid](https://github.com/ndmitchell/ghcid) and slightly-more intelligent code completion.

## Getting Started

Once my Package Control PR goes through, you can install this as you would install any other Sublime Text 3 package.

Until then, clone the repository, and from the project root, run one of the following commands.

For MacOS:

```
cp ./* ~/Library/Application\ Support/SublimeText3/Packages/User/
```

For Linux:

```
cp ./* ~/.config/sublime-text-3/Packages/User/
```

It should also work in Windows, but I don't know the location of the User package or how to copy files.

Open a Haskell source file (for example, the provided _syntax-test.hs_) and try it out.

## Copyright and Contributing

Syntax highlighting is based on the default _Haskell.sublime-syntax_ file included with Sublime Text.

Multi-line string support by [SQbQxeKd3JHD8](https://github.com/SublimeHaskell/SublimeHaskell/pull/422).

Copyright (c) 2018 Daniel Brice, except as otherwise noted.

Permission granted to anyone who posesses the software to use, distribute, and modify.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.

**Pull requests welcomed!** While I don't have time to work on feature requests, I will happily consider any pull requests and either merge them or give feedback.
