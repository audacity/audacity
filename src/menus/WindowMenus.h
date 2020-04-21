#include "../Audacity.h"

#include "../commands/CommandManager.h"

#ifdef __WXMAC__

/// Namespace for functions for window management (mac only?)
namespace WindowActions {

   // Range of assigned menu IDs
   static const wxWindowID ID_BASE = 30000;
   static const wxWindowID ID_LAST = 30999;

   // Exported helper functions
   void Refresh();
};

#else

// Not WXMAC.

#endif
