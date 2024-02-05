# ksignals unit

A better and easier to use version of `unixsignals` unit I have here.

Add the unit to your uses, then set one of the event callbacks based on your application:

`OnSignal:=@SignalEventHandler;`

`procedure TMyObject.SignalEventHandler;`

The signal event will trigger if the application is signaled to terminate via an external source, either a simple **SIGTERM** or if the end-user hits *Ctrl-C* at the console while the program is running.
