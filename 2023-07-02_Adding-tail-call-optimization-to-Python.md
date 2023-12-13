---
title: "Adding tail call optimization to Python"
date: 2023-07-02T18:21:00+02:00
draft: false
---

[Tail call optimization](https://stackoverflow.com/questions/310974/what-is-tail-call-optimization) is a great feature. It's obviously easier to solve recursive problems recursively rather than iteratively.
It's a shame it's not available in Python (or many other languages), so let's add it.

## TL;DR

The following function will result in `RecursionError` when its argument >= 1000

```python
def my_fn(target_iters, iteration=0):
  if iteration >= target_iters:
    return iteration

  return my_fn(target_iters, iteration+1)
```

```python
>>> my_fn(1000)
...
RecursionError: maximum recursion depth exceeded in comparison
```


With some extra code, we can add _tail call optimization_

```python
def my_fn2(my_fn2, target_iters, iteration=0):
  if iteration >= target_iters:
    return iteration

  return my_fn2(target_iters, iteration+1)

optimized_fn = tailcall_optimize(my_fn2)
```

```python
>>> optimized_fn(10000)
10000
```

Arguably a very contrived example, I admit.
But not too bad - the only thing that changed is the signature of the function, everything else is the same.

## How it works

Pretty straightforward actually! The key is - as you may already expect - in the `my_fn2` passed in as a function argument.

We're making use of the fact that only the leaf returns _any data_ - that means that all other functions leading to the leaf don't need to maintain any state

Let's look at some pseudocode

```
args = init_args
while true:
  return_value = my_fn(lambda new_args: update_args(new_args), *args)
  if return_value is not None:
    return return_value
```

Here it also becomes obvious why only the leaf can return any data. If other nodes would return data while recursing we’d require a strategy to merge them back together in order to emulate the behavior we’d experience with a regular recursive function.

The Python code I used is the following

```python
def tailcall_optimize(fn):
  def recursion_wrapper(*args, **kwargs):
    is_called = True
    result = None
    val = (args, kwargs)

    def fn_wrapper(*args, **kwargs):
      nonlocal val, is_called

      val = (args, kwargs)
      is_called = True


    while is_called:
      is_called = False
      result = fn(fn_wrapper, *val[0], **val[1])
    return result

  return recursion_wrapper
```


Instead of relying on the fact that the non-leaf node returns `None`, I'm tracking if the fn_wrapper passed in is being called or not - but either works!

This certainly requires a few more checks to bring it into production, but conceptually this is all we need.
This could also be easily ported to other languages.


This was all good fun, but let's address the elephant is the room: How to make this work with a broader set of recursive functions.

Naturally I looked into that as well...
