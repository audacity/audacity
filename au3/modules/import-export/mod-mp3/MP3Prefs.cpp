/**********************************************************************

  Audacity: A Digital Audio Editor

  @file MP3Prefs.cpp
  @brief adds controls for MP3 import/export to Library preferences

  Paul Licameli split from LibraryPrefs.cpp

**********************************************************************/

#include "ExportMP3.h"
#include "Internat.h"
#include "ShuttleGui.h"
#include "LibraryPrefs.h"
#include "ReadOnlyText.h"

namespace {
void AddControls(ShuttleGui& S)
{
    S.StartStatic(XO("LAME MP3 Export Library"));
    {
        S.StartTwoColumn();
        {
            auto MP3Version = S
                              .Position(wxALIGN_CENTRE_VERTICAL)
                              .AddReadOnlyText(XO("MP3 Library Version:"), "");
            MP3Version->SetValue(GetMP3Version(S.GetParent(), false));
        }
        S.EndTwoColumn();
    }
    S.EndStatic();
}

LibraryPrefs::RegisteredControls reg{ wxT("MP3"), AddControls };
}
