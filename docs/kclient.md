# kclient unit

An event-based network client class which runs inside a thread.  It supports some basic network protocols for sending basic primitive data types over a network connection.  Extremely lightweight.

Supports 4 different client mode types:

  * **kcmApi** uses the type exposed in the `kapi` unit for data transfer.
  * **kcmString** sends basic Pascal ansistrings over the pipe.
  * **kcmLine** basic line-based network protocol.
  * **kcmBlock** Transfer a blocks in a specific size over the pipe.

I believe one of the original ideas for this unit was to replace the current network layer in my *Memory Card Server*.
