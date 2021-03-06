# Hive

A collection of distributed algorithms. See subdirectories..


## [chord](chord)

Distributed hash table.


## [memcached](memcached)

An implementation of the memcached protocol built mainly to excercise
the chord algorithm. It is slower than THE memcached by about a factor
of 10. And it doesn't fully implement the whole protocol. Currently,
it implements just the binary protocol enough to make `memslap` think
it's talking to a real memcached server.


## more to come...

## HOWTO

Chord And Memcached are both Erlang 'application's. Chord can be used
stand alone from within Erlang. To provide an external interface to
Chord, I decided to implement a Memcached server. Memcached depends on
Chord. And only one instance of Memcached should be started in a
machine because the sole purpose of Memcached is to bind to port 2222
and on any machine you can only have one application bind to a
port. You can start multiple instances of Chord even within the same
physical machine if you run multiple Erlang nodes.

It's not completely polished so there are some manual steps to get it
up and going:

1. Start 4-5 terminal sessions.
2. On each terminal run `erl -sname nX@localhost` where X is a number
   that is distinct on each terminal (**Important**: see
   [Node Names](./chord#node-names)).
3. After starting erlang on all the terminals, at the `erl>` prompt,
   type the following set of commands:

```
    erl> net_adm:world().
    erl> application:ensure_all_started(chord).
    erl> chord_server:join().
```

4. On *one* of the erlang nodes, type:

```
    erl> application:ensure_all_started(memcached).
```

You should now have memcached up and running. The memcached server
uses chord to store and retrieve values from the chord ring. Chord, in
turn, uses ETS for storage. The code is designed to be able to swap it
out for DETS or filesystem storage or what-have-you.

See [chord](chord) to play with the chord ring.

See [memcached](memcached) to play with the minimal memcached
implementation.

##### Important

there is a .hosts.erlang file that says to look for other nodes only
on localhost. If you want to try it out on different physical nodes,
be sure to edit the .hosts.erlang file or otherwise use the
appropriate `net_adm:ping/1` commands instead of `net_adm:world/0`. In
the future, I'll probably integrate some method to auto-discover nodes
running chord.


[chord]: ./chord "Chord"
[memcached]: ./memcached "Memcached"
