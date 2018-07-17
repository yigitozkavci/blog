---
title: Jumping through instructions with RecursiveDo
author: Yiğit Özkavcı
---

When we read a program written in any programming language, we implicitly assume that the state flows in the direction we read the code, at least inside functions. I had my mind blown when I first saw the `RevState` and `Tardis` monads; they break our assumption on the direction of the state flow. Since they are already interesting enough for one to dig deep into, in this post I won't focus on them, instead, I will talk about an interesting real-world problem I encountered, and a solution that involves depending on a future value.

First, I will describe the problem of jumping through instructions, and then I will develop several solutions for it.
The reader is assumed to have some basic familiarity with Haskell, at least up to a basic understanding of monads.

Imagine a simple virtual machine managed by a stack and memory. Let `u` denote the stack, and `u[i]` denote the ith element from the top in the stack.
Consider having 6 kinds of instructions:
```bash
# Push the given element into the stack so that u[0] = val
# and all other values are pushed one level downwards
PUSH val

# Add u[0] and u[1] then pop both of them and push the
# result into the stack
ADD

# Test and jump: jump to destination u[0] if (u[1] != 0)
# and pop two elements from the stack
JUMPI

# Apply not unary operator to the value u[0]
# So if (u[0] == 1) before, now (u[0] == 0) and vice versa
NOT

# Store the value u[0] into the address u[1]
# and pop two elements from the stack
STORE

# Marks a valid jump destination. All jumps
# should be made onto a JUMPDEST instruction
JUMPDEST
```
So if we were to convert an if statement to instructions;
```c
if(predicate) {
  a = 3 + 5;
}
```
We would end up with the program below:
```bash
1 PUSH predicate # Push if's condition expression
2 NOT # We will jump outside of if's body if the condition is NOT true
3 PUSH 10
4 JUMPI
5 PUSH 0xf4 # Arbitrary address for variable "a"
6 PUSH 5
7 PUSH 3
8 ADD
9 STORE
10 JUMPDEST # If statement's body is completed
```
If we represent this computation in Haskell, it would look something like the following:
```haskell
module Main where

import           Control.Monad.Trans.State
import           Data.Monoid ((<>))

data Instruction =
    NOT
  | PUSH Int
  | ADD
  | JUMPI
  | STORE
  | JUMPDEST
  deriving (Show, Eq)

type InstructionSet = [Instruction]

data InstrState = InstrState
  { _instrSet :: InstructionSet
  , _pc       :: Int
  } deriving Show

op :: Instruction -> State InstrState ()
op instr =
  modify $ \(InstrState set pc) -> InstrState (set <> [instr]) (pc + 1)

ifBlock :: State InstrState ()
ifBlock = do
  op (PUSH 0xf4) -- An arbitrary address for variable a
  op (PUSH 5)
  op (PUSH 3)
  op ADD
  op STORE

compute :: Int -> State InstrState ()
compute predicate = do
  op (PUSH predicate)
  op NOT
  op (PUSH 10)
  op JUMPI
  ifBlock
  op JUMPDEST

main :: IO ()
main = print $ _instrSet $ execState (compute 1) (InstrState [] 0)
-- Output:
-- [PUSH 1,NOT,PUSH 10,JUMPI,PUSH 244,PUSH 5,PUSH 3,ADD,STORE,JUMPDEST]
```
You may already have noticed that at instruction number 3, we've pushed 10; it's nice to directly jump to your destination, but how did we know we had to jump to PC 10?
The problem with this solution is that we need to know that we need to push 10 into stack before we know the total cost of theif statement's body.

Even if in this case if statement's body cost does not change, in the real world scenario it does according to the state of the variables & lexical scope of the statement.
One pattern to observe is that in some places, we implicitly depend on some future computation, which is `ifBlock` in this case. We need to somehow be able to say "give me the destination, but I don't know its value yet, it will be computed later so wait for it".

We could achieve this behavior within pure contexts thanks to let bindings in Haskell being lazy and recursive, but here, we are inside a monadic context under State monad. The correspondent of this lazy behavior for effectful computations is rec enabled by the `RecursiveDo` extension (you can develop an intuition for it [here](https://ocharles.org.uk/blog/posts/2014-12-09-recursive-do.html)).
So we modify our compute function to calculate jump destination before it arrives at that location:
```haskell
{-# LANGUAGE RecursiveDo #-}
...
jumpdest :: State InstrState Int
jumpdest = do
  op JUMPDEST
  _pc <$> get

compute :: Int -> State InstrState ()
compute predicate = do
  op (PUSH predicate)
  op NOT
  rec op (PUSH dest)
      op JUMPI
      ifBlock
      dest <- jumpdest
  pure ()
...
```
Thanks to lazy evaluation, `RecursiveDo` extension lets us declare that we want to use some value, but not necessarily compute it right now; most importantly, does it while preserving the effect order. This is important because even in our small use-case, we need the proper ordering of instructions.

There is a drawback here, though. If we were to care for optimization here, we would want our values to be strictly evaluated at the time we construct the instructions. You cannot simply strictly evaluate the destination value, so if we do the following change to `PUSH` constructor in order to force it to evaluate the Int value it takes right at construction time:

```haskell
data Instruction =
...
 | PUSH !Int
... 
```

... nothing changes! Why? Let's look at the definition of op function. It takes the instruction as an argument, but it's a thunk at that time. Haskell does not evaluate the `PUSH !Int`, so it cannot evaluate `!Int` part at all! In order to observe what happens in the case of strict evaluation, we need the strictness operator also on instr parameter of `op` function. Remember that `BangPatterns` extension is required for this task:
```haskell
{-# LANGUAGE BangPatterns #-}
...
op :: Instruction -> State InstrState ()
op !instr =
 modify $ \(InstrState set pc) -> InstrState (set <> [instr]) (pc + 1)
...
-- Output:
-- <<infinite loop>>
```
So we have our infinite loop. This concludes the post.

## Notes
* Huge thanks Enis Bayramoğlu for reviewing the whole post
