// -*- mode: c++ -*-
// Indirectly include SBSMS header so that we can disable warnings about unused parameters
// when compiling Audacity itself.

#pragma warning( push )
#pragma warning( disable : 4100)
#include "../sbsms/include/sbsms.h"
#pragma warning( pop ) 
