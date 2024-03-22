WebAssembly
===========

KDDockWidgets works with WebAssembly with the following known limitations:

- Classic drop indicators are not supported, only the segmented ones. This is because
WASM doesn't support windows with translucency.

- Might be slow on Linux, depending on your browser, while dragging or resizing windows.
Please file a bug with Qt, as it's out of scope for KDDW to fix.

Demo:
=====

A demo is available at https://demos.kdab.com/wasm/kddockwidgets/dockwidgets.html

Build tips for KDDW:
====================

- Visit https://doc.qt.io/qt-5/wasm.html if you haven't yet
- Open a terminal suitable for WASM development (with the correct Qt and toolchain in PATH, etc)
- KDDockWidgets can be built with `cmake -DCMAKE_TOOLCHAIN_FILE=/usr/local/emsdk-1.39.8/upstream/emscripten/cmake/Modules/Platform/Emscripten.cmake -DCMAKE_FIND_ROOT_PATH=~/Qt/5.15.1/wasm_32/ -DKDDockWidgets_EXAMPLES=OFF -DCMAKE_BUILD_TYPE=Release`
  (Adapt the paths to your own situation)

Builds tips for your own app:
=============================
- Link to KDDW (libkddockwidgets.a, or similar)
- As the build is static, don't forget to initialize KDDW's resources:
```
#ifdef QT_STATIC
    Q_INIT_RESOURCE(kddockwidgets_resources);
#endif
```
