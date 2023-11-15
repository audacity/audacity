#include "MusicInformationRetrievalPrefs.h"

ChoiceSetting UseMirResultToConfigureProject {
   wxT("/GUI/UseMirResultToConfigureProject"),
   { ByColumns,
     { {}, {}, {} },
     { wxString("Yes"), wxString("No"), wxString("Ask") } },
   2, // "Ask"
};
