// -*- mode: c++ -*-
// Indirectly include Allegro header so that we can disable warnings about unused parameters
// when compiling Audacity itself.

#ifdef _MSC_VER
// If this is compiled with MSVC (Visual Studio)

#pragma warning( push )
#pragma warning( disable : 4100)
#include "../portsmf/allegro.h"
#pragma warning( pop )

#else //_MSC_VER

#include "../portsmf/allegro.h"

#endif //_MSC_VER
