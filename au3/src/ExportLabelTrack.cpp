#include "LabelTrack.h"
#include "ShuttleGui.h"
#include "prefs/ImportExportPrefs.h"

namespace {
void AddControls(ShuttleGui& S)
{
    S.StartStatic(XO("Exported Label Style:"));
    {
#if defined(__WXMAC__)
        // Bug 2692: Place button group in panel so tabbing will work and,
        // on the Mac, VoiceOver will announce as radio buttons.
        S.StartPanel();
#endif
        {
            S.StartRadioButtonGroup(LabelStyleSetting);
            {
                S.TieRadioButton();
                S.TieRadioButton();
            }
            S.EndRadioButtonGroup();
        }
#if defined(__WXMAC__)
        S.EndPanel();
#endif
    }
    S.EndStatic();
}

ImportExportPrefs::RegisteredControls reg { wxT("LabelStyle"), AddControls };
} // namespace
