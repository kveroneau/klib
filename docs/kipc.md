# kipc unit

A very simple *Inter-Process Call* Server/Client implementation over a standard UNIX domain socket.

Rather than using Pipes as the FPC includes IPC Server/Client use, this implementation uses UNIX domain sockets for the purpose of transferring data between programs.  It is also extremely easy to use and very lightweight.
