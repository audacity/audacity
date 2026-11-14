/**********************************************************************

Audacity: A Digital Audio Editor

CommonTrackControls.h

Paul Licameli split from TrackControls.h

**********************************************************************/

#ifndef __AUDACITY_COMMON_TRACK_CONTROLS__
#define __AUDACITY_COMMON_TRACK_CONTROLS__

#include "TrackControls.h" // to inherit

class CloseButtonHandle;
class MenuButtonHandle;
class PopupMenuTable;
class MinimizeButtonHandle;
class TrackSelectHandle;

namespace TrackInfo {
struct TCPLine;
}
using TCPLines = std::vector< TrackInfo::TCPLine >;

class AUDACITY_DLL_API CommonTrackControls /* not final */ : public TrackControls
{
public:
    using TrackControls::TrackControls;

    // This is passed to the InitUserData() methods of the PopupMenuTable
    // objects returned by GetMenuExtension:
    struct InitMenuData
    {
    public:
        AudacityProject& project;
        Track& track;
        wxWindow* pParent;
        unsigned result;
    };

    const TCPLines& GetTCPLines() const override;

protected:
    // An override is supplied for derived classes to call through but it is
    // still marked pure virtual
    virtual std::vector<UIHandlePtr> HitTest(const TrackPanelMouseState& state, const AudacityProject*) override = 0;

    unsigned DoContextMenu(const wxRect& rect, wxWindow* pParent, const wxPoint* pPosition, AudacityProject* pProject) override;
    virtual PopupMenuTable* GetMenuExtension(Track* pTrack) = 0;

    // TrackPanelDrawable implementation
    void Draw(
        TrackPanelDrawingContext& context, const wxRect& rect, unsigned iPass) override;

    wxRect DrawingArea(
        TrackPanelDrawingContext&, const wxRect& rect, const wxRect& panelRect, unsigned iPass) override;

    std::weak_ptr<CloseButtonHandle> mCloseHandle;
    std::weak_ptr<MenuButtonHandle> mMenuHandle;
    std::weak_ptr<MinimizeButtonHandle> mMinimizeHandle;
    std::weak_ptr<TrackSelectHandle> mSelectHandle;
};

#endif
