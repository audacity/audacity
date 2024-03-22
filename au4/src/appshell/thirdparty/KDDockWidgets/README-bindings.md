These are the instructions for building the Python bindings for KDDockWidgets.

Make sure you have PySide2, shiboken2 and shiboken2-generator installed.
As this time, you cannot get shiboken2-generator because the wheels are not on PyPi.
To use the wheels do this:

```
% pip3 install \
    --index-url=http://download.qt.io/official_releases/QtForPython/ \
    --trusted-host download.qt.io \
    shiboken2 pyside2 shiboken2_generator
```

For more info visit https://doc.qt.io/qtforpython/shiboken2/gettingstarted.html

afterwards run 'pip3 list | grep PySide2'
Note the version *must* match the same Qt you intend to use when building KDDockWidgets.

Not supported:
 - Debug builds
 - static builds
 - python2 bindings
 - only some 32-bit platforms are supported.  see https://wiki.qt.io/Qt_for_Python

Tell CMake to build the bindings by passing the `-DKDDockWidgets_PYTHON_BINDINGS=True' option,
followed by the make command.

The bindings will be installed to the passed `-DCMAKE_INSTALL_PREFIX`, which
might require setting the `PYTHONPATH` env variable to point to that path when
running applications.  Alternatively, configure the bindings install location
by passing `-DKDDockWidgets_PYTHON_BINDINGS_INSTALL_PREFIX=/usr/lib/python3.8/site-packages`
to CMake (adjust to the python path on your system).

To run the KDDW python example

```
$ export PYTHONPATH=/kddw/install/path # Only if needed
$ cd python/examples/
$ rcc -g python -o rc_assets.py ../../examples/dockwidgets/resources_example.qrc
$ python3 main.py
```

Build Issues

* If you see errors like "Unable to locate Clang's built-in include directory"
  then first make sure you have llvm installed.  If you still have problems try
  setting the environment variable `LLVM_INSTALL_DIR` to point to your llvm installation.

  Examples:

  ```
  export LLVM_INSTALL_DIR=/usr/local/opt/llvm-11
  set "LLVM_INSTALL_DIR=C:\Program Files\LLVM" #Windows
  ```

* When building the examples you may encounter errors loading shared libraries from shiboken2_generator.

  Try:

  ```
  export LD_LIBRARY_PATH=/usr/local/lib/python/dist-packages/PySide2/Qt/lib #linux
  export DYLD_LIBRARY_PATH=/usr/local/lib/python/dist-packages/PySide2/Qt/lib #Mac
  #adjust to wherever your PySide is installed
  ```

* On Windows the `libclang.dll` that ship with QtForPython is not compatible with MSVC2019.
  To fix this, copy the `libclang.dll` that comes with llvm into shiboken2, like so:

  ```
  cd C:\Python37\Lib\site-packages\shiboken2_generator
  copy libclang.dll libclang.dll.save
  copy "C:\Program Files\llvm\bin\libclang.dll" libclang.dll
  #Python3 installation in C:\Python37 and llvm in c:\Program Files\llvm. adjust as needed
  ```
