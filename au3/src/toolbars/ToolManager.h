/**********************************************************************

  Audacity: A Digital Audio Editor

  ToolManager.h

  Dominic Mazzoni
  Shane T. Mueller
  Leland Lucius

**********************************************************************/

#ifndef __AUDACITY_TOOLMANAGER__
#define __AUDACITY_TOOLMANAGER__

#include <functional>

#include <wx/defs.h>
#include <wx/eventfilter.h> // to inherit
#include <wx/frame.h> // to inherit
#include <wx/timer.h> // member variable

#include "ClientData.h"
#include "GlobalVariable.h"
#include "Observer.h"
#include "ToolDock.h"

#include "CommandFunctors.h"

class wxCommandEvent;
class wxFrame;
class wxMouseEvent;
class wxPaintEvent;
class wxPoint;
class wxRect;
class wxRegion;
class wxSize;
class wxTimer;
class wxTimerEvent;
class wxWindow;

class AudacityProject;
class ProjectWindow;
class ToolFrame;

////////////////////////////////////////////////////////////
/// class ToolManager
////////////////////////////////////////////////////////////

class AUDACITY_DLL_API ToolManager final : public wxEvtHandler, public wxEventFilter, public ClientData::Base
{
public:
    // a hook function to break dependency of ToolManager on ProjectWindow
    struct AUDACITY_DLL_API TopPanelHook : GlobalHook<TopPanelHook,
                                                      wxWindow* (wxWindow&)
                                                      > {};

    static ToolManager& Get(AudacityProject& project);
    static const ToolManager& Get(const AudacityProject& project);

    ToolManager(AudacityProject* parent);
    ToolManager(const ToolManager&) = delete;
    ToolManager& operator=(const ToolManager&) = delete;
    ~ToolManager();

    void CreateWindows();

    void LayoutToolBars();

    bool IsDocked(Identifier type) const;

    bool IsVisible(Identifier type) const;

    void ShowHide(Identifier type);

    void Expose(Identifier type, bool show);

    ToolBar* GetToolBar(const Identifier& type) const;

    ToolDock* GetTopDock();
    const ToolDock* GetTopDock() const;
    ToolDock* GetBotDock();
    const ToolDock* GetBotDock() const;

    void Reset();
    static void OnResetToolBars(const CommandContext& context);

    void Destroy();
    void RegenerateTooltips();

    int FilterEvent(wxEvent& event) override;

    bool RestoreFocus();

    //! Visit bars, lexicographically by their textual ids
    template< typename F >
    void ForEach(F&& fun)
    {
        std::for_each(std::begin(mBars), std::end(mBars), [&fun](auto& pair){
            fun(pair.second.get());
        });
    }

    size_t CountBars() const
    {
        return mBars.size();
    }

    static void ModifyToolbarMenus(AudacityProject& project);
    // Calls ModifyToolbarMenus() on all projects
    static void ModifyAllProjectToolbarMenus();

private:

    ToolBar* Float(ToolBar* t, wxPoint& pos);

    void OnMenuUpdate(struct MenuUpdateMessage);
    void OnTimer(wxTimerEvent& event);
    void OnMouse(wxMouseEvent& event);
    void OnCaptureLost(wxMouseCaptureLostEvent& event);
    void UndockBar(wxPoint mp);
    void OnGrabber(GrabberEvent& event);
    void HandleEscapeKey();
    void DoneDragging();

    void OnIndicatorCreate(wxWindowCreateEvent& event);
    void OnIndicatorPaint(wxPaintEvent& event);

    void ReadConfig();
    void WriteConfig();
    void Updated();

    Observer::Subscription mMenuManagerSubscription;
    AudacityProject* mParent;
    wxWindowRef mLastFocus{};

    ToolFrame* mDragWindow;
    ToolDock* mDragDock;
    ToolBar* mDragBar {};
    wxPoint mDragOffset;
    ToolBarConfiguration::Position mDragBefore {};

    wxPoint mLastPos;
    wxRect mBarPos;

    using FramePtr = Destroy_ptr<wxFrame>;
    FramePtr mIndicator;
    std::unique_ptr<wxRegion> mLeft, mDown;
    wxRegion* mCurrent;

    wxTimer mTimer;
    bool mLastState;

#if defined(__WXMAC__)
    bool mTransition;
#endif

    ToolDock* mTopDock{};
    ToolDock* mBotDock{};

    //! map not unordered_map, for the promise made by ForEach
    std::map<Identifier, ToolBar::Holder> mBars;

    wxPoint mPrevPosition {};
    ToolDock* mPrevDock {};
    ToolBarConfiguration::Position mPrevSlot
    { ToolBarConfiguration::UnspecifiedPosition };
    ToolBarConfiguration mPrevConfiguration;
    bool mDidDrag{};
    bool mClicked{};

public:

    DECLARE_CLASS(ToolManager)
    DECLARE_EVENT_TABLE()
};

////////////////////////////////////////////////////////////
/// class ToolFrame
////////////////////////////////////////////////////////////

class ToolFrame final : public wxFrame
{
public:

    ToolFrame(AudacityProject* parent, ToolManager* manager, ToolBar* bar, wxPoint pos);

    ~ToolFrame();

    ToolBar* GetBar() { return mBar; }
    void ClearBar() { mBar = nullptr; }
    void LockInMinSize(ToolBar* pBar);

    //
    // Transition a toolbar from float to dragging
    //
    void OnGrabber(GrabberEvent& event);

    //
    // Handle toolbar updates
    //
    void OnToolBarUpdate(wxCommandEvent& event);

    //
    // Handle frame paint events
    //
    void OnPaint(wxPaintEvent & WXUNUSED(event));

    void OnMotion(wxMouseEvent& event);

    void OnCaptureLost(wxMouseCaptureLostEvent & WXUNUSED(event));

    //
    // Do not allow the window to close through keyboard accelerators
    // (like ALT+F4 on Windows)
    //
    void OnClose(wxCloseEvent& event);

    void OnKeyDown(wxKeyEvent& event);

    void Resize(const wxSize& size);

private:

    AudacityProject* const mParent;
    ToolManager* mManager;
    ToolBar* mBar;
    wxSize mMinSize;
    wxSize mOrigSize;

public:

    DECLARE_CLASS(ToolFrame)
    DECLARE_EVENT_TABLE()
};

#include "MenuRegistry.h"

// Construct a static instance of this class to add a menu item that shows and
// hides a toolbar
struct AUDACITY_DLL_API AttachedToolBarMenuItem : CommandHandlerObject {
    AttachedToolBarMenuItem(
        Identifier id, const CommandID& name, const TranslatableString& label_in, const Registry::OrderingHint& hint = {},
        // IDs of other toolbars not to be shown simultaneously with this one:
        std::vector< Identifier > excludeIds = {});

    void OnShowToolBar(const CommandContext& context);

    const Identifier mId;
    const MenuRegistry::AttachedItem mAttachedItem;
    const std::vector< Identifier > mExcludeIds;
};

#endif
