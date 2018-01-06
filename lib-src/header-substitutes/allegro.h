// -*- mode: c++ -*-
// Indirectly include Allegro header so that we can disable warnings about unused parameters
// when compiling Audacity itself.

#pragma warning( push )
#pragma warning( disable : 4100)
#include "../portsmf/allegro.h"
#pragma warning( pop ) 
