# profiler

Simple, embedded profiler with very small overhead  
Requires C++17 and higher.
  
Features:
* Embedded profiler (can run anywhere and anytime)
* Function duration measure
* Steps duration measure
* Very small overhead
* Enabled / disabled on compile time and run time
* Thread safe (without use mutex)
* Custom data printer

[Example](example/main.cpp)

Used in at least two private commercial projects and one [open source](https://github.com/musescore/MuseScore).

## Integration 

### Add source 
To use Profiler within your software project include the Profiler source into your project

Source:
* profiler.h/cpp - profiler and macros
* funcinfo.h - macros for parsing signatures

or see and include `profiler.cmake` in the cmake project (see [example/CMakeLists.txt](example/CMakeLists.txt))

### Add aliases 
Recommended add own aliases to use profiler
See example:
* [profiler.h](example/profiler.h)

## ChangeLog

### v1.2
* Fixed thread data race 

### v1.1
* Improved parsing of function signatures
* Fixed get threads data

### v1.0
* Ported from [https://github.com/igorkorsukov/qzebradev](https://github.com/igorkorsukov/qzebradev)
