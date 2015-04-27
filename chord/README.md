= Chord =

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
