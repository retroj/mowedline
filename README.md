
Mowedline
=========

Mowedline is a status bar program for X, written in [Chicken
Scheme](http://www.call-cc.org/) with XLib, Xft, and DBus.  It was
inspired by [dzen2](https://github.com/robm/dzen), and like
that program, aims at an unobtrusive, minimalist look.  It is based on a
client/daemon design, where the daemon maintains one or more windows, and
the client sends commands to the daemon over DBus to update the contents
of those windows.  A mowedline window is divided into widgets.  Each
widget has a unique name, by which the client can refer to it to update
its contents.


Obtaining Mowedline
-------------------

The source code can be obtained from
[my git repository](https://github.com/retroj/mowedline/).  Please
report any issues or feature requests you might have in the
[issue tracker](https://github.com/retroj/mowedline/issues).


Installing Mowedline
--------------------

To install Mowedline you should use `chicken-install -s mowedline`.
The `-s` switch tells chicken-install to use sudo to get root
privileges.

For more detailed instructions and instructions to build from source
see the [installation](https://github.com/retroj/mowedline/wiki/Installation)
page.


Project Status
--------------

Refer to Mowedline's
[CHANGELOG](https://github.com/retroj/mowedline/blob/master/CHANGELOG.md).


License
-------

Mowedline is licensed under the terms of GPL3.
