# modularity

A convenient and efficient implementation of dependency injection, consisting of IoC-container and macros for ease of use. It also contains a minimal template for organizing modules.  
Requires C++17 and higher.

[Example](example/main.cpp)

Used in at least two private commercial projects and one [open source](https://github.com/musescore/MuseScore).

## Integration 

### Add source 
To use `modularity` within your software project include the `modularity` source into your project
See and include `modularity/modularity.cmake` in the cmake project (see [example/CMakeLists.txt](example/CMakeLists.txt))

### Add aliases 
Recommended add own aliases to use `modularity`, see [example/modularity](example/modularity)
   
## ChangeLog

### v1.2 
* Added Inject class (replacing a macro)

### v1.1
* Added internal interfaces support 

### v1.0
* Ported from Qt implementation
* Added INJECT macros
