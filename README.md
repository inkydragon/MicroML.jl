# MicroML.jl

[![Dev](https://img.shields.io/badge/docs-dev-blue.svg)](https://inkydragon.github.io/MicroML.jl/dev/)
[![Build Status](https://github.com/inkydragon/MicroML.jl/actions/workflows/CI.yml/badge.svg?branch=main)](https://github.com/inkydragon/MicroML.jl/actions/workflows/CI.yml?query=branch%3Amain)
[![codecov](https://codecov.io/gh/inkydragon/MicroML.jl/graph/badge.svg?token=R9CQ3IE4FP)](https://codecov.io/gh/inkydragon/MicroML.jl)

A tree-walking interpreter for a simple ML-like language.

Rewrite [hellerve/microml](https://github.com/hellerve/microml) in julia.

> Note that hellerve' implement can compile MicroML' code to C and do compiled execution. I haven't finished this part yet, branch `genc` contain some progress.

Most test copy from:
- [2018/type-inference · eliben/code-for-blog](https://github.com/eliben/code-for-blog/tree/master/2018/type-inference)
- [2018/unif · eliben/code-for-blog](https://github.com/eliben/code-for-blog/tree/master/2018/unif)

You may want to read Eli Bendersky’s blog post to get some idea about:
- [Type inference - Eli Bendersky's website](https://eli.thegreenplace.net/2018/type-inference/)
- [Unification - Eli Bendersky's website](https://eli.thegreenplace.net/2018/unification/)

## Usage

You can open a REPL by typing `julia mml.jl` in the `src` folder of this repository.

```ml
MicroML.jl/src$ julia mml.jl
μML> 

μML> x y z = y + z
x = (lambda y, z -> (y + z)) :: (Int -> Int -> Int)
μML> main = lambda -> print(x(1, 2))
main = (lambda  -> print(x(1, 2))) :: (-> Int)
μML>:i
3
μML>main = lambda -> print(1+1)
[Warning] Redefining main!
main = (lambda  -> print((1 + 1))) :: (-> Int)
μML>:i
2
μML>main = lambda -> print(0)
[Warning] Redefining main!
main = (lambda  -> print(0)) :: (-> Int)
μML>:i
0
μML>

μML> foo f g x = if f(x) then g(x) else 20
foo = (lambda f, g, x -> (if f(x) then g(x) else 20)) :: ((a -> Bool) -> (a -> Int) -> a -> Int)
μML> foo f x = if x then lambda t -> f(t) else lambda j -> f(x)
[Warning] Redefining foo!
foo = (lambda f, x -> (if x then (lambda t -> f(t)) else (lambda j -> f(x)))) :: ((Bool -> a) -> Bool -> (Bool -> a))
μML>:q
Moriturus te saluto!
```

Read some test code in [`runtests.jl`](./test/runtests.jl) to get some idea about how to use functions in this package.
