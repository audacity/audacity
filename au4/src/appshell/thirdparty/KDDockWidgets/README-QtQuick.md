Supported Qt versions and toolchains
=====================================

KDDockWidgets for QtQuick requires a C++17 capable compiler and either
Qt >= 5.15.0 or Qt >= 6.2.


TROUBLESHOOTING
===============

- QtGraphicalEffects is not supported, as it's buggy when moving between different QWindows.
  See for example QTBUG-94943, KDDockWidgets issue #213. Also search the Qt bug tracker
  for "QQuickItem: Cannot use same item on different windows at the same time"

- Very rarely, in some Nvidia/X11 setups, floating/docking has noticeable lag (like 1 second)
  This could be solved by going to Nvidia's settings and making sure all monitors have
  the same refresh rate and disabling "Allow Flipping". It's not known why this solves it. Might also
  be a bug in Qt.
