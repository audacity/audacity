#include "ModuleConstants.h"

DEFINE_VERSION_CHECK
extern "C" DLL_API int ModuleDispatch(ModuleDispatchTypes type)
{
   return 1;
}
