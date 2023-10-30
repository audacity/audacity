// -*- mode: c++ -*-
// Indirectly include Allegro header so that we can disable warnings about unused parameters
// when compiling Audacity itself.

#ifdef _MSC_VER
// If this is compiled with MSVC (Visual Studio)

#pragma warning( push )
#pragma warning( disable : 4100)
#include "allegro.h"
#pragma warning( pop )

#elif defined(__linux__)

#include <cstring> // Allegro include fails if this header isn't included due to no memcpy
#include "allegro.h"

#else //_MSC_VER

#include "allegro.h"

#endif //_MSC_VER
