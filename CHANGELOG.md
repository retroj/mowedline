
# Change Log

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/)
and this project adheres to [Semantic Versioning](http://semver.org/).


## [Unreleased]


## [3.3.0] - 2017-12-17
### Added

- move, resize, and update struts when xinerama configuration changes.
- active-window-icon: new widget

### Fixed

- window: `_NET_WM_STRUT_PARTIAL` correct x maximum
- window: set `WM_CLIENT_MACHINE` correctly


## [3.2.1] - 2017-12-03
### Fixed

- update for changes in gochan egg


## [3.2.0] - 2017-06-06
### Added

- xinerama support


## [3.1.1] - 2017-05-17
### Fixed

- corrected value of `_NET_WM_WINDOW_TYPE` property


## [3.1.0] - 2017-01-24
### Added

- <widget> init: field
- widget-update-at-interval
- mowedline.el: mowedline-clear

### Changed

- Switch back to dbus egg (work from dbus2 has been merged).


## [3.0.0] - 2016-11-14
### Added

- Support org.freedesktop.DBus.Introspectable.

### Changed

- DBus service, interface, and path are now net.retroj.mowedline,
  net.retroj.mowedline, and /net/retroj/mowedline.
- Use dbus2 egg instead of dbus egg.


## [2.0.0] - 2016-06-19
### Changed

- All object properties are now keywords instead of symbols
- Terser error output on command line parse error


## [1.0.1] - 2016-06-16
### Changed

- Use gochan egg instead of mailbox egg.
- Remove a couple of egg dependencies.

### Fixed

- Drawing bug related to zero-width widgets (issue #17)


## [1.0.0] - 2016-06-11
### Added

- `-active-window-title` command line widget.
- Set `NET_WM_STRUT` X property for old window managers.
- CHICKEN 4.10.0 compatibility.
- `XDG_CONFIG_HOME` environment variable support.

### Changed

- Flex <= 0 means no flex.

### Removed

- Flex may no longer be `#f`.


## [0.2.9-dev] - 2015-08-18
### Added

- window-background 'transparent
- widget:active-window-title

### Fixed

- Proper handling of X ClientMessage.


## [0.2.8] - 2015-08-03
## [0.2.7] - 2015-05-31
## [0.2.6] - 2013-03-02
## [0.2.5] - 2013-02-19
## [0.2.4] - 2013-02-18
## [0.2pre3] - 2013-02-14
## [0.2pre2] - 2013-02-09
## [0.2pre1] - 2013-01-29
## [0.1] - 2011-04-08
