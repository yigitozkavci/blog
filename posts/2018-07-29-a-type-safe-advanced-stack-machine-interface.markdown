---
title: A type-safe advanced stack machine interface
author: Yiğit Özkavcı
---

# Introduction
While developing [Cenary](https://github.com/cenary-lang/cenary/), a programming language for writing smart contracts, there were many places requiring an API for imperative stack machine procedures.

While generating code for procedural operations (say, a loop or branching), we need to somehow tame its imperative nature and bring safety; we are able to do it since we are equipped with Haskell.

In this post, we are going to achieve such behavior by introducing useful tools such as **indexed monoids (or categories)**, **RebindableSyntax**, **TypeApplications** and many others.

(Note: you are encouraged to copy-paste the code in the section "Implementation" and modify things as we go through this post, don't worry, it compiles just fine).

# Problem
Let's look at an example:

```c
PUSH32 0x3f
PUSH32 0x22
MSTORE8 // Store the value 0x3f at memory address 0x22
```

Imagine writing a code generator for this, it would look like:
```haskell
save :: Int -> Int -> Procedure
save addr val = do
  push32 val
  push32 addr
  mstore
```

This may look fine at first place, but both `val` and `addr` indicate that even though they are both `Int`s, actually they are quite different in meaning. We mean the address of an integer while saying `addr`, and a value integer for `val`.

Let's look at another example:
```haskell
broken :: Int -> Int -> Procedure
broken val1 val2 = do
  push32 val1
  push32 val2
  pop
  pop
  pop
```

Even just by looking at it, you saw the problem: we are making compiler allow us to pop from an empty stack. We can catch error(s) by using applicatives like Maybe or Either, but we want this kind of code not to compile at all.

# Motivation
Eventually, in this post, we are aiming to achieve an API that would allow us a usage as such:

```haskell
main :: IO ()
main =
  generate $ do
    push @IntAddress 1 -- Push an address to stack
    push @'IntVal 3    -- Push an integer to stack
    push @'IntVal 5    -- Push an integer to stack
    push @IntAddress 1 -- Push an address to stack
    mstore             -- Pop 2 items from stack
    pop                -- Pop 1 item from stack
    load               -- Pop 1 item from stack and push 1 item
    pop                -- Pop 1 item from stack
    where
      (>>) = (Cat.>>>)
```
Some assurances going on behind-the-scenes:

* We only allow `mstore` in generation of the code if top two items on the stack is in correct format (`s[0]` is an address, and `s[1]` is a value).

* We are, obviously not allowing `pop`ping from an empty stack.

* We only allow `load` instruction if `s[0]` (ie. the top element in stack) is an address, and we are putting its value to the stack. So a `IntAddress 1` is replaced by `IntVal 1` after load instruction is executed.

These constraints are just the tip of the iceberg. Once we have the API, we can introduce many, possibly more sophisticated checks.

# Implementation
## Tools Required
We begin with preparing the tools we are going to need. 

First things first, we are going to need these extensions:
```haskell
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE InstanceSigs     #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}

import qualified Control.Category as Cat
import           Prelude          (IO, Int, Show, fromInteger, id, print,
                                   return, ($), (.))

```

A couple of EVM instructions. Even though it's not needed to follow this post, if you are interested, there are **many** of them documented in the [yellowpaper](http://yellowpaper.io/)
```haskell
data EvmInstruction =
    PUSH Int
  | POP
  | LOAD
  | MSTORE8
  deriving Show
```

And the tricky part. The type `Instr` represents an instruction in our API.
```haskell
{-
   Meant to be used as a kind, so a type like 'Address ('Address 'IntVal)
   means the address of the address of an integer
-}
data ValueTy = IntVal
            | Address ValueTy
            | CharVal

{-
   `xs` is the stack before, and `ys` is after.
   And in value level, it's just a modifier for stack full of `EvmInstruction`s.
-}
newtype Instr (xs :: [ValueTy]) (ys :: [ValueTy]) = Instr
  { _unInstr :: [EvmInstruction] -> [EvmInstruction]
  }

{-
  And this is the essence to our API. We are propagating our
  stack state at type level by binding instructions, given
  that they tell us what kind of modification they do.
-}
instance Cat.Category Instr where

  (.) :: Instr b c -> Instr a b -> Instr a c
  Instr f1 . Instr f2 = Instr (f2 . f1)

  id :: Instr a a
  id = Instr id
```

## Adding Instructions
And finally, we are ready to describe our instructions. We basically encode instructions in two ways: one in type, and one in value. 
```haskell
addInstr :: EvmInstruction -> Instr a b
addInstr i = Instr $ (i:)

push :: forall elemTy xs. Int -> Instr xs (elemTy ': xs)
push v = addInstr (PUSH v)

pop :: Instr (elemTy ': xs) xs
pop = addInstr POP

load :: Instr ('Address elemTy ': xs) (elemTy ': xs)
load = addInstr LOAD

mstore :: Instr ('Address addr ': valueTy ': xs) xs
mstore = addInstr MSTORE8
```

## Execution
And finally, we are ready to write our code generator:

```haskell
generate :: Instr '[] xs -> IO ()
generate = print . ($ []) . _unInstr

type IntAddress = 'Address 'IntVal

main :: IO ()
main =
  generate $ do
    push @IntAddress 0x16
    push @'IntVal 3
    push @'IntVal 5
    push @IntAddress 0x24
    mstore
    pop
    load
    pop
    where
      (>>) = (Cat.>>>)
```
Notice that we are telling push method what type to use for given integer when it's pushed into the stack, we are making use of `TypeApplications` there.

# Analysis
The following signature for `load` captures all the behavior of a `load` instruction:
```haskell
load :: Instr ('Address elemTy ': xs) (elemTy ': xs)
```
Signature of the `load` instruction has something special as well, thanks to parametric polymorphism: whichever type you have in the address at the top of the stack, you have the value of that address. So if you have an `Address (Address Int)`, you get back an `Address Int` and this is powerful.

Now let's look at the `mstore` generator:
```haskell
mstore :: Instr ('Address addr ': valueTy ': xs) xs
```
It's pattern matching on the state of the stack, so that it both ensures that there are at least 2 items, and type of the items are suitable for mstore instruction.

## Composition
In this manner, we are able to compose small pieces of procedures of which behaviors are indicated with their signatures. A real world example:

SHA3 instruction takes two values: an address and a span indicated by an integer value. So with values [address, 0x40], it takes the SHA3 hash of the value in memory area (address, address + 0x40). But we often have values, not addresses in stack, so we need an intermediate function that does the work of allocating that value along with taking SHA3 hash of it. First, we need a couple more functions for Evm API:
```haskell
-- | Duplicates top value of stack
data EvmInstruction = ...
                    | DUP1
                    | DUP2
                    | SWAP1
                    | SHA3

data ValueTy = ...
             | SHA3Hash

-- | Duplicates top value of stack
dup1 :: Instr (val ': xs) (val ': val ': xs)
dup1 = addInstr DUP1

-- | Duplicates second from top value of stack
dup2 :: Instr (x ': y ': xs) (y ': x ': y ': xs)
dup2 = addInstr DUP2

-- | Swaps the top 2 values in stack
swap1 :: Instr (x ': y ': xs) (y ': x ': xs)
swap1 = addInstr SWAP1

-- | Takes tha SHA3 hash of the value in address (s[0]...(s[0] + s[1]))
sha3 :: Instr (IntAddress ': 'IntVal ': xs) ('SHA3Hash ': xs)
sha3 = addInstr SHA3
```

And then, we write our utility functions using these above:
```haskell
-- | Given a value in stack, takes SHA3 hash of it.
sha3value :: Instr ('IntVal ': xs) ('SHA3Hash ': xs)
sha3value = do
  let allocatedAddr = 0x1afe
  push @IntAddress allocatedAddr
  swap1
  dup2
  mstore
  push @'IntVal 0x20
  swap1
  sha3
  where
    (>>) = (Cat.>>>)

```
# Conclusion
Initially when I first started writing this API and procedures, I felt urge to write comprehensive tests to be sure that those long procedures are doing what they should be doing. Even one line missing can be a cause of bugs which are hard to detect.

It's important to write tests to keep runtime behavior of programs in check, but in these kind of problems, creating proven small pieces and composing them together should be the way to go since you have very small space for errors (instruction primitives like `dup1`, `swap1`, `sha3` are the error-prone components in this program's case).
