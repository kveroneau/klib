# klogger unit

An easy to use wrapper unit around `TEventLog` which can allow a global logging unit for any application.  I originally wrote it was `logger.pas` in program, then realized I should make it reuse-able, and the version inside klib was then named `klogger` as to not clash with my existing program's unit.

Super easy to use:

`SetupLog('/var/log/mylog.log');`

To send an info to the log, simply use:

`LogInfo('Log message to log!');`

Easy as that!
