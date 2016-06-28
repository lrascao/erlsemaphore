# erlsemaphore

[erlsemaphore](https://github.com/lrascao/erlsemaphore) is a SystemV IPC semaphore NIF interface.

## Introduction

Allows for resource signalling between the Erlang VM and an external process (eg. Cnode/Cport).

## Build

    $ git clone https://github.com/lrascao/erlsemaphore.git
    $ cd erlsemaphore
    $ make

## Getting Started!

Start an Erlang console with `erlsemaphore` running:

    $ make shell

```erlang
% create a new semaphore named test
> erlsemaphore:new(test).
{ok,test}
```

Let's try waiting on a resource:

```erlang
> erlsemaphore:wait(test, 1, [{no_wait, true}]).
{error, would_block}
> erlsemaphore:signal(test, 1).
ok
> erlsemaphore:wait(test, 1, [{no_wait, true}]).
ok
> erlsemaphore:signal(test, 2).
ok
> erlsemaphore:wait(test, 1, [{no_wait, true}]).
ok
> erlsemaphore:wait(test, 1, [{no_wait, true}]).
ok
> erlsemaphore:wait(test, 1, [{no_wait, true}]).
{error, would_block}
```

## Benchmarks

    $ make bench

## Copyright and License

Copyright (c) 2016 Luis Miguel Mourato Rascão

**erlsemaphore** source code is licensed under the [Apache License 2.0](LICENSE.md).
