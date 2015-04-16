
all: chord.d memcached.d

chord.d:
	$(MAKE) -C chord

memcached.d:
	$(MAKE) -C memcached

.PHONEY: all chord.d memcached.d
