
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

__June 19, 2016:__ Version 2.0.0 --- mowedline now uses keywords
instead of symbols to specify properties for widgets.  See
[configuration](https://github.com/retroj/mowedline/wiki/Configuration)
to find out how to update your config.

__June 12 2016:__ Version 1.0.0 --- Add support for transparent window
background.  Add active-window-title widget, to show the title of the
currently focused window.  Consider a flex option of 0 to mean _not_
to flex.  Look at the XDG\_CONFIG_HOME environment variable to find
the configuration file, instead of always only looking at
`~/.config/mowedline`.

__August 3, 2015:__ Version 0.2.8 --- simplified configuration syntax;
window margins; basic window transparency (non-compositing); mowedline can
now be used as a library.

__May 31, 2015:__ Version 0.2.7 --- font support in markup; Emacs package,
mowedline.el, for updating mowedline from Emacs.

__February 9, 2013:__ Version 0.2pre2 --- chicken-install mowedline.

__January 29, 2013:__ Version 0.2pre1 --- mowedline has been split
into two programs, mowedline and mowedline-client, to address the
problem of simultaneous multiple server starting,
[discussed here](http://retroj.net/blog/2013/01/28/mowedline-three-bugs).

__August 30, 2011:__ Xft, unicode, color, and more, all supported in
mowedline 0.2pre.

__August 26, 2011:__ Progress on writing Xft bindings for Chicken Scheme.
They're not polished enough to release yet, but this will allow me to
start experimenting with using Xft in mowedline.

__March 24, 2011:__ _Mowedline 0.1_ The program now has the minimum
feature set to be considered useful, though much work remains to be done.


License
-------

Mowedline is licensed under the terms of GPL3.
