# Memcached

Minimal implementation of the memcached server. Only support the
binary protocol and only supports `get`, `getk`, `set`, and
`add`. Will accept `flush` but will not actually empty out the
storage.

The primary purpose of this is to expose the chord application through
the memcached protocol so that I can use the same tool (`memslap`) to
compare chord against memcached.


## Preliminary findings
Ran the official memcached using `memcached -d`. And ran this Erlang memcached using instructions from [Here](../Readme.md#howto).

### Speed test
Using
````
    memslap --servers=127.0.0.1:11211 --concurrency=100 --execute-number=100 --initial-load=10 --flush --binary
````

and

````
    memslap --servers=127.0.0.1:2222 --concurrency=100 --execute-number=100 --initial-load=10 --flush --binary
````

Findings:

1. This memcached is about 10x slower than the official memcached when
   running chord on about 5 nodes.
2. It is about 2x slower when running chord on a single node.
3. Running chord on 2,3 and 4 nodes didn't change the speed noticibly
   from when it was run on 5. It seems the `find_successor` takes
   about the same time (which was expected by the `log n` design using
   the finger table). In the special case where it's run on 1 node, it
   is faster.

### Load test

Using

````
    memslap --servers=127.0.0.1:11211 --concurrency=1000 --execute-number=1000 --initial-load=10 --flush --binary
````

and

````
    memslap --servers=127.0.0.1:2222 --concurrency=1000 --execute-number=1000 --initial-load=10 --flush --binary
````

Findings:

1. Erlang version still 10x slow.
2. Official version causes multiple connection timeout errors. Cannot
   use all the CPU available.
3. Erlang version has no errors. Uses all the CPUs (cores) available.
4. I suspect that if there were more cores available, the Erlang
   version would scale up and may reduce the speed gap.


## Node failures

If you shut down a chord node (not the one running memcached
application) in the middle of the test, you'll get a bunch of errors
from memslap but it will recover. This is because I haven't figured
out a nice way to make the client wait while chord re-jiggers its
finger table. If that can be made atomic, it would be really
atomic. But we might be walking into the CAP territory. And the paper
doesn't really specify anything in this regard. There are quite a few
things that are 'hand waved' in the paper, it seems.
