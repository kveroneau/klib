# cmdsys unit

A unit to wrap a commit function call for token parsing.  For example, take this string:

`cmdline:='echo "Hello World"';`

To parse this with ease using `cmdsys`, do as follows:

```var
  cmd, param: string;
begin
  cmd:=getToken(cmdline);
  // cmdline now = 'Hello World'
  // cmd now = 'echo'
  // getToken can be called again to get additional param tokens:
  param:=getToken(cmdline);

```

The most easiest unit to use in this library.
