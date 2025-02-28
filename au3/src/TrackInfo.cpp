/**********************************************************************

Audacity: A Digital Audio Editor

TrackInfo.cpp

Paul Licameli split from TrackPanel.cpp


**********************************************************************/

#include "TrackInfo.h"

#include <wx/app.h>
#include <wx/dc.h>
#include <wx/font.h>
#include <wx/window.h>

#include "AColor.h"
#include "AllThemeResources.h"
#include "PlayableTrack.h"
#include "Prefs.h"
#include "Project.h"
#include "SyncLock.h"
#include "Theme.h"
#include "TrackPanelDrawingContext.h"
#include "UIHandle.h"
#include "ViewInfo.h"
#include "tracks/ui/ChannelView.h"

// Subscribe to preference changes to update static variables
struct Settings : PrefsListener {
    wxFont gFont;

    bool mInitialized{ false };

    void UpdatePrefs() override
    {
#if defined __WXMAC__
        int fontSize = 12;
#else
        int fontSize = 10;
#endif
        gFont.Create(fontSize, wxFONTFAMILY_SWISS, wxFONTSTYLE_NORMAL, wxFONTWEIGHT_NORMAL);

        mInitialized = true;
    }
};

static Settings& settings()
{
    static Settings theSettings;
    if (!theSettings.mInitialized) {
        theSettings.UpdatePrefs();
    }
    return theSettings;
}

// return y value and height
std::pair< int, int >
TrackInfo::CalcItemY(const TCPLines& lines, unsigned iItem)
{
    int y = 0;
    auto pLines = lines.begin();
    while (pLines != lines.end()
           && 0 == (pLines->items & iItem)) {
        y += pLines->height + pLines->extraSpace;
        ++pLines;
    }
    int height = 0;
    if (pLines != lines.end()) {
        height = pLines->height;
    }
    return { y, height };
}

/// \todo Probably should move to 'Utils.cpp'.
void TrackInfo::SetTrackInfoFont(wxDC* dc)
{
    dc->SetFont(settings().gFont);
}
