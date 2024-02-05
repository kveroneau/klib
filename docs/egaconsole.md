# egaconsole unit

Ported over from some Python code I wrote back in 2014 to emulate a standard IBM-PC text framebuffer.  My original version for pygame was misnamed `pygame-vgaconsole`, but then I later found out that this emulation is actually for something the EGA driver does, not the VGA one.  During the port to ObjectPascal, I updated the name to reflect that new knowledge.

### Usage

Install into your Lazarus IDE and rebuild to install.  Once installed, there will be a new component in the `klib` component tab called `EGAConsole` which can be easily added to any GUI form.  It should also be fully cross-platform with MacOS and Windows.  I have currently only tested it on Linux.

Once added to a form, you can use the instance through your IDE as such:

`EGAConsole1.Write('Write to framebuffer.');`

It is far from feature complete yet, but expect new features in the future, such as event hooks.
