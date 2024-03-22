Wayland support is done and has been tested on KDE (Kwin) and weston.

Limitations
============

Wayland works very differently than traditional desktops and imposes us some,
limitations. Here's a list of different behaviours which KDDockWidgets will have
when running on Wayland:


- A title bar can either be used for drag&dock or for moving the window around.

- For this reason, floating windows now have two title bars.
  The native one, drawn by the server and the client one, drawn by KDDockWidgets.
  The native one allows you to drag the window around but not drop/dock.
  The client title bar allows you to perform a drag and drop/dock but not move the window around.

- You can detach a window by:
  - Clicking the title-bar's float button
  - Double-clicking the title bar of a docked widget
  - Double-clicking a tab
  - If no title bar is shown, double-clicking the empty space of the tab bar will detach
  the entire group of tabbed dock widgets

- Layout save/restore won't restore the position of floating windows, as wayland
  doesn't allow us to set geometry.

- Kwin specific:
  - The pixmap that's shown during a drag can't be bigger than 250x250. Might be a bug.



All in all it's pretty decent and usable. Any further improvements should be done at the server or
protocol level now.
