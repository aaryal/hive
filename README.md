# Hive

A collection of distributed algorithms. See subdirectories..


## [chord](chord)

Distributed hash table.


## [memcached](memcached)

An implementation of the memcached protocol built mainly to excercise
the chord algorithm. It is slower than THE memcached by about a factor
of 10. And it doesn't fully implement the whole protocol. Currently,
it implements just the binary protocol enough to make memslap think
it's talking to a real memcached server.


## more to come...

## HOWTO

Chord And Memcached are both Erlang 'application'. Chord can be used
stand alone from within Erlang. To provide an external interface to
Chord, I decided to implement a Memcached server. Memcached depends on
Chord. And only one instance of Memcached should be started in a
machine (need to add checks to stop multiple instances being started
within the same machine). You can start multiple instances of Chord
even within the same physical machine.

It's not completely polished so there are some manual steps to get it
up and going:

1. Start 4-5 terminal sessions.
2. On each terminal run `erl -sname nX@localhost` where X is a number
   that is distinct on each terminal (see **Node Names** at the end of
   this document).
3. After starting erlang on all the terminals, at the `erl>` prompt,
   type the following set of commands:

    erl> net_adm:world().
    erl> application:ensure_all_started(chord).
    erl> chord_server:join().

4. On *one* of the erlang nodes, type:

    erl> application:ensure_all_started(memcached).

You should now have memcached up and running. The memcached server
uses chord to store and retrieve values from the chord ring. Chord, in
turn, uses ETS for storage. The code is designed to be able to swap it
out for DETS or filesystem storage or what-have-you.

**Important**: there is a .hosts.erlang file that says to look for other
 nodes only on localhost. If you want to try it out on different
 physical nodes, be sure to edit the .hosts.erlang file or otherwise
 use the appropriate `net_adm:ping/1` commands instead of
 `net_adm:world/0`. In the future, I'll probably integrate some method
 to auto-discover nodes running chord.

**Node Names**: It's best right now to use n1, n2, n3, n4, n5 and n6 as your node names.
Here's what they map to in the chord id:

    n1: 2
    n2: 5
    n3: 4
    n4: 7
    n5: 1
    n6: 6

At some point, i'll need to detect collision and use a random string
to create a random id. For testing though, i need predictibility which
is why it is the way it is.


[chord]: ./chord "Chord"
[memcached]: ./memcached "Memcached"
