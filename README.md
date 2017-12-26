
Mowedline
=========

Mowedline is a statbar program for X: a docked window, usually at the top
or bottom of the screen, which displays whatever information you want to
put in it.  It aims to be lightweight, aesthetically minimalistic but
elegant, not specific to any particular desktop environment, and easy to
connect to other programs.

Information can be sent to Mowedline in a variety of ways:

 - command line
 - UNIX pipe
 - DBus method call
 - DBus signals
 - X events

A Mowedline window contains any number of widgets.  Some of the supported
widgets are:

 - text, populated by any of the methods above, which may be colorized and
   buttonized
 - clock
 - active window title
 - active window icon
 - network-manager status
 - laptop power information
 - selection/clipboard owner tracker

Mowedline is programmed in [CHICKEN Scheme](htttps://call-cc.org/), an
easy-to-learn multiparadigm programming language.  Not only that,
Mowedline is also configured in CHICKEN Scheme, which means that a full
featured programming language is available in the file you use to
configure Mowedline, which you can use to customize and extend Mowedline
to suit your own needs.

In order to be minimalistic with respect to both resources and aesthetics,
Mowedline does not use a GUI toolkit (like GTK or Qt).  Instead, it uses
Xlib directly to create and manage its windows, the Xft font library to
draw antialiased text, and Imlib2 for images.  This gives you precise
control over the exact geometry and the look of your status bar.


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
