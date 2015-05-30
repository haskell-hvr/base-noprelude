## `base-noprelude` - `base` package sans `Prelude`

This package is, as the title gives away, a `base`-replacement that allows to define and/or use custom `Prelude`s without having to use the `-XNoImplicitePrelude` language extension. I.e. instead of

```haskell
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import CustomPrelude
import OtherStuff
```

by having a `build-depends` on `base-noprelude` *instead* of `base` and having a `Prelude` module in scope (can be provided by the current package or a 3rd package from `build-depends`), one can just write

```haskell
module Main where

import OtherStuff
```
