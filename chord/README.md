# Chord

This is an implementation of the chord algorithm. The major goals are:

1. Stay as close to the paper as possible. You should be able to read
   the psudocode in the paper and map that to the source
   code. Exceptions are, of course, where there are `while/for` loops
   in the paper, those are recursions and some `if/then/else` are
   turned into Erlang's function clauses.
2. Provide a basis (storage) for further distributed applications so
   that the storage for those applications is automatically
   'sharded'. Data loss is ok at this level. Much like the memcached
   model - except that the addition and removal of nodes is possible
   and is dynamic.
3. For data persistence guarentees, use a `Raft` implementation on top
   of `chord` much like a RAID system using a clusters of `chord`
   nodes.

## Howto

1. Start 4-5 terminal sessions.
2. On each terminal run `erl -sname nX@localhost` where X is a number
   that is distinct on each terminal (**Important**: see [Node Names](#node-names)).
3. After starting erlang on all the terminals, at the `erl>` prompt, type the following set of commands:

````
    erl> net_adm:world().
    erl> application:ensure_all_started(chord).
    erl> chord_server:join().
````

Then, on one node do:

````
    erl> chord_server:put("ABC", "def").
````

And on another node, do:

````
    erl> chord_server:put("XyZ", "xxx").
````

And then, on yet another node do:

````
    erl> chord_server:get("ABC").
    erl> chord_server:get("XyZ").
````

### Nodes leaving and joining

1. follow the steps above on getting a chord ring setup on 3-4 nodes.
2. On each node run `erl> chord_server:status().`.
3. On one of the nodes, quit erlang using `q()` at the erlang prompt.
4. On the remaining nodes, run `erl> chord_server:status().` to see
   the finger table and the predecessor being readjusted.
5. run `chord_server:get/1` commands on the remaining nodes to see if
   the chord ring continues to function. it's not an error to lose
   data that was on the node that you shutdown on step #3.

Conversly, to add a new node,
1. On the node that you quit erlang on, rerun the `erl ..` command
2. run

````
    erl> net_adm:world().
    erl> application:ensure_all_started(chord).
    erl> chord_server:status()
````

3. Note the finger table and the predecessor: itself. Run the
    `chord_server:status()` on other nodes and see that no one is
    referencing this node.
4. Then on the node from #1 run:
    erl> chord_server:join().
5. Repeat #3 and see that it's now part of the ring.

##### Important

there is a .hosts.erlang file that says to look for
other nodes only on localhost. If you want to try it out on different
physical nodes, be sure to edit the .hosts.erlang file or otherwise
use the appropriate `net_adm:ping/1` commands instead of
`net_adm:world/0`. In the future, I'll probably integrate some method
to auto-discover nodes running chord.

##### Node Names

It's best right now to use n1, n2, n3, n4, n5 and n6 as your node names.
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
