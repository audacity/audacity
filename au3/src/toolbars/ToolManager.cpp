/**********************************************************************

  Audacity: A Digital Audio Editor

  ToolManager.cpp

  Dominic Mazzoni
  Shane T. Mueller
  Leland Lucius

  See ToolManager.h for details.

*******************************************************************//**

\file ToolManager.cpp

  Implements ToolManager

*//*******************************************************************//**

\class ToolManager
\brief Manages the ToolDocks and handles the dragging, floating, and
  docking of ToolBars.

*//**********************************************************************/

#include "ToolManager.h"

#include "CommandContext.h"

// For compilers that support precompilation, includes "wx/wx.h".
#include <wx/wxprec.h>

#ifndef WX_PRECOMP
#include <wx/dcclient.h>
#include <wx/defs.h>
#include <wx/frame.h>
#include <wx/gdicmn.h>
#include <wx/region.h>
#include <wx/settings.h>
#include <wx/sizer.h>
#include <wx/sysopt.h>
#include <wx/utils.h>
#include <wx/window.h>
#endif  /*  */

#include <wx/minifram.h>
#include <wx/popupwin.h>

#include "AColor.h"
#include "AllThemeResources.h"
#include "CommandManager.h"
#include "ImageManipulation.h"
#include "Prefs.h"
#include "Project.h"
#include "ProjectWindows.h"
#include "SyncLock.h"
#include "widgets/AButton.h"
#include "widgets/ASlider.h"
#include "widgets/MeterPanelBase.h"
#include "widgets/Grabber.h"

////////////////////////////////////////////////////////////
/// Methods for ToolFrame
////////////////////////////////////////////////////////////
#define sizerW 11

//
// Constructor
//
ToolFrame::ToolFrame
    (AudacityProject* parent, ToolManager* manager, ToolBar* bar, wxPoint pos)
    : wxFrame(FindProjectFrame(parent),
              bar->GetId(),
              wxEmptyString,
              pos,
              wxDefaultSize,
              wxNO_BORDER
              | wxFRAME_NO_TASKBAR |
#if !defined(__WXMAC__) // bug1358
              wxFRAME_TOOL_WINDOW |
#endif
              wxFRAME_FLOAT_ON_PARENT)
    , mParent{parent}
{
    int width = bar->GetSize().x;
    int border = 1;

    // Save parameters
    mManager = manager;
    mBar = bar;

    // Transfer the bar to the ferry
    bar->Reparent(this);

    // Bug 2120 (comment 6 residual): No need to set a minimum size
    // if the toolbar is not resizable. On GTK, setting a minimum
    // size will prevent the frame from shrinking if the toolbar gets
    // reconfigured and needs to resize smaller.
    if (bar->IsResizable()) {
        SetMinSize(bar->GetDockedSize());
    }

    {
        // We use a sizer to maintain proper spacing
        auto s = std::make_unique<wxBoxSizer>(wxHORIZONTAL);

        // Add the bar to the sizer
        s->Add(bar, 1, wxEXPAND | wxALL, border);

        // Add space for the resize grabber
        if (bar->IsResizable()) {
            s->Add(sizerW, 1);
            width += sizerW;
        }

        SetSize(width + 2 * ToolBarFloatMargin,
                bar->GetDockedSize().y + 2 * ToolBarFloatMargin);

        // Attach the sizer and resize the window to fit
        SetSizer(s.release());
    }

    Layout();

    // Inform toolbar of change
    bar->SetDocked(NULL, true);

    // Make sure resizable floaters don't get any smaller than initial size
    if (bar->IsResizable()) {
        // Calc the minimum size of the frame
        mMinSize = bar->GetMinSize() + (GetSize() - bar->GetSize());
    }
}

ToolFrame::~ToolFrame()
{
    if (HasCapture()) {
        ReleaseMouse();
    }
}

void ToolFrame::OnGrabber(GrabberEvent& event)
{
    // Pass it on to the manager since it isn't in the handling hierarchy
    mManager->ProcessEvent(event);
}

// The current size determines the min size for resizing...
// the 'lock in' is at that aspect ratio.
void ToolFrame::LockInMinSize(ToolBar* pBar)
{
    mBar = pBar;

    wxSize sz = mBar->GetSize();
    SetClientSize(sz);
    int yDesiredMin = 26;
    int y = sz.GetHeight();
    if (y > yDesiredMin) {
        sz.SetWidth((sz.GetWidth() * yDesiredMin) / y);
        sz.SetHeight(yDesiredMin);
    }
    mMinSize = sz - wxSize(10, 0);
}

void ToolFrame::OnToolBarUpdate(wxCommandEvent& event)
{
    // Resize floater window to exactly contain toolbar
    // use actual size rather than minimum size.
    if (mBar) {
        mBar->GetParent()->SetClientSize(mBar->GetSize());// ->GetMinSize() );
    }
    // Allow it to propagate to our parent
    event.Skip();
}

void ToolFrame::OnPaint(wxPaintEvent& WXUNUSED(event))
{
    wxPaintDC dc(this);
    wxSize sz = GetSize();
    wxRect r;

    dc.SetPen(theTheme.Colour(clrTrackPanelText));
    dc.SetBackground(wxBrush(theTheme.Colour(clrMedium)));
    dc.Clear();
    dc.SetBrush(*wxTRANSPARENT_BRUSH);
    dc.DrawRectangle(0, 0, sz.GetWidth(), sz.GetHeight());

    if (mBar && mBar->IsResizable()) {
        r.x = sz.x - sizerW - 2,
        r.y = sz.y - sizerW - 2;
        r.width = sizerW + 2;
        r.height = sizerW + 2;

        AColor::Line(dc, r.GetLeft(), r.GetBottom(), r.GetRight(), r.GetTop());
        AColor::Line(dc, r.GetLeft() + 3, r.GetBottom(), r.GetRight(), r.GetTop() + 3);
        AColor::Line(dc, r.GetLeft() + 6, r.GetBottom(), r.GetRight(), r.GetTop() + 6);
        AColor::Line(dc, r.GetLeft() + 9, r.GetBottom(), r.GetRight(), r.GetTop() + 9);
    }
}

void ToolFrame::OnMotion(wxMouseEvent& event)
{
    // Don't do anything if we're docked or not resizeable
    if (!mBar || mBar->IsDocked() || !mBar->IsResizable()) {
        return;
    }

    // Retrieve the mouse position
    wxPoint pos = ClientToScreen(event.GetPosition());
    if (HasCapture() && event.Dragging()) {
        wxRect rect = GetRect();

        rect.SetBottomRight(pos);

        // Keep it within max size, if specified
        wxSize maxsz = mBar->GetMaxSize();
        if (maxsz != wxDefaultSize) {
            if (maxsz.x != wxDefaultCoord && rect.width > maxsz.x) {
                rect.width = maxsz.x;
            }
            if (maxsz.y != wxDefaultCoord && rect.height > maxsz.y) {
                rect.height = maxsz.y;
            }
        }

        if (rect.width < mMinSize.x) {
            rect.width = mMinSize.x;
        }

        if (rect.height < mMinSize.y) {
            rect.height = mMinSize.y;
        }

        Resize(rect.GetSize());
    } else if (HasCapture() && event.LeftUp()) {
        ReleaseMouse();
    } else if (!HasCapture()) {
        wxRect rect = GetRect();
        wxRect r;

        r.x = rect.GetRight() - sizerW - 2,
        r.y = rect.GetBottom() - sizerW - 2;
        r.width = sizerW + 2;
        r.height = sizerW + 2;

        // Is left click within resize grabber?
        if (r.Contains(pos) && !event.Leaving()) {
            mOrigSize = GetSize();

            SetCursor(wxCURSOR_SIZENWSE);
            if (event.LeftDown()) {
                CaptureMouse();
            }
        } else {
            SetCursor(wxCURSOR_ARROW);
        }
    }
}

void ToolFrame::OnCaptureLost(wxMouseCaptureLostEvent& WXUNUSED(event))
{
    if (HasCapture()) {
        ReleaseMouse();
    }
}

void ToolFrame::OnClose(wxCloseEvent& event)
{
    event.Veto();
}

void ToolFrame::OnKeyDown(wxKeyEvent& event)
{
    event.Skip();
    if (HasCapture() && event.GetKeyCode() == WXK_ESCAPE) {
        Resize(mOrigSize);
        ReleaseMouse();
    }
}

void ToolFrame::Resize(const wxSize& size)
{
    SetMinSize(size);
    SetSize(size);
    Layout();
    Refresh(false);
}

IMPLEMENT_CLASS(ToolFrame, wxFrame);

BEGIN_EVENT_TABLE(ToolFrame, wxFrame)
EVT_GRABBER(wxID_ANY, ToolFrame::OnGrabber)
EVT_PAINT(ToolFrame::OnPaint)
EVT_MOUSE_EVENTS(ToolFrame::OnMotion)
EVT_MOUSE_CAPTURE_LOST(ToolFrame::OnCaptureLost)
EVT_CLOSE(ToolFrame::OnClose)
EVT_COMMAND(wxID_ANY, EVT_TOOLBAR_UPDATED, ToolFrame::OnToolBarUpdate)
EVT_KEY_DOWN(ToolFrame::OnKeyDown)
END_EVENT_TABLE()

IMPLEMENT_CLASS(ToolManager, wxEvtHandler);

////////////////////////////////////////////////////////////
/// Methods for ToolManager
////////////////////////////////////////////////////////////

BEGIN_EVENT_TABLE(ToolManager, wxEvtHandler)
EVT_GRABBER(wxID_ANY, ToolManager::OnGrabber)
EVT_TIMER(wxID_ANY, ToolManager::OnTimer)
END_EVENT_TABLE()

static const AudacityProject::AttachedObjects::RegisteredFactory key{
    []( AudacityProject& parent ){
        return std::make_shared< ToolManager >(&parent);
    }
};

ToolManager& ToolManager::Get(AudacityProject& project)
{
    return project.AttachedObjects::Get< ToolManager >(key);
}

const ToolManager& ToolManager::Get(const AudacityProject& project)
{
    return Get(const_cast< AudacityProject& >(project));
}

//
// Constructor
//
ToolManager::ToolManager(AudacityProject* parent)
    : wxEvtHandler()
{
    wxPoint pt[ 3 ];

#if defined(__WXMAC__)
    // Save original transition
    mTransition = wxSystemOptions::GetOptionInt(wxMAC_WINDOW_PLAIN_TRANSITION);
#endif

    // Initialize everything
    mParent = parent;
    mLastPos.x = mBarPos.x = -1;
    mLastPos.y = mBarPos.y = -1;
    mDragWindow = NULL;
    mDragDock = NULL;
    mDragBar = NULL;

    // Create the down arrow
    pt[ 0 ].x = 0;
    pt[ 0 ].y = 0;
    pt[ 1 ].x = 9;
    pt[ 1 ].y = 9;
    pt[ 2 ].x = 18;
    pt[ 2 ].y = 0;

    // Create the shaped region
    mDown = std::make_unique<wxRegion>(3, &pt[0]);

    // Create the down arrow
    pt[ 0 ].x = 9;
    pt[ 0 ].y = 0;
    pt[ 1 ].x = 0;
    pt[ 1 ].y = 9;
    pt[ 2 ].x = 9;
    pt[ 2 ].y = 18;

    // Create the shaped region
    mLeft = std::make_unique<wxRegion>(3, &pt[0]);

    // Create the indicator frame
    // parent is null but FramePtr ensures destruction
    mIndicator = FramePtr{ safenew wxFrame(NULL,
                                           wxID_ANY,
                                           wxEmptyString,
                                           wxDefaultPosition,
                                           wxSize(32, 32),
                                           wxFRAME_TOOL_WINDOW
                                           | wxFRAME_SHAPED
                                           | wxNO_BORDER
                                           | wxFRAME_NO_TASKBAR
                                           | wxSTAY_ON_TOP)
    };

    // Hook the creation event...only needed on GTK, but doesn't hurt for all
    mIndicator->Bind(wxEVT_CREATE,
                     &ToolManager::OnIndicatorCreate,
                     this);

    // Hook the paint event...needed for all
    mIndicator->Bind(wxEVT_PAINT,
                     &ToolManager::OnIndicatorPaint,
                     this);

    // It's a little shy
    mIndicator->Hide();
}

void ToolManager::CreateWindows()
{
    auto parent = mParent;
    auto& window = GetProjectFrame(*parent);

    // Hook the parents mouse events...using the parent helps greatly
    // under GTK
    window.Bind(wxEVT_LEFT_UP,
                &ToolManager::OnMouse,
                this);
    window.Bind(wxEVT_MOTION,
                &ToolManager::OnMouse,
                this);
    window.Bind(wxEVT_MOUSE_CAPTURE_LOST,
                &ToolManager::OnCaptureLost,
                this);

    wxWindow* topDockParent = TopPanelHook::Call(window);
    wxASSERT(topDockParent);

    // Create the top and bottom docks
    mTopDock = safenew ToolDock(this, topDockParent, TopDockID);
    mBotDock = safenew ToolDock(this, &window, BotDockID);

    // Create all of the toolbars
    // All have the project as parent window
    wxASSERT(parent);

    for (const auto& factory : RegisteredToolbarFactory::GetFactories()) {
        if (factory) {
            auto bar = factory(*parent);
            if (bar) {
                auto& slot = mBars[bar->GetSection()];
                if (slot) {
                    // Oh no, name collision of registered toolbars
                    assert(false);
                    bar->Destroy();
                    continue;
                }
                slot = std::move(bar);
            }
        } else {
            wxASSERT(false);
        }
    }

    //! Assign (non-persistent!) sequential ids to the toolbars
    ForEach([ii = 0](ToolBar* bar) mutable { bar->SetIndex(ii++); });

    // We own the timer
    mTimer.SetOwner(this);

    // Process the toolbar config settings
    ReadConfig();

    wxEvtHandler::AddFilter(this);

    mMenuManagerSubscription = CommandManager::Get(*mParent)
                               .Subscribe(*this, &ToolManager::OnMenuUpdate);
}

//
// Destructor
//

void ToolManager::Destroy()
{
    if (mTopDock || mBotDock) {  // destroy at most once
        wxEvtHandler::RemoveFilter(this);

        // Save the toolbar states
        WriteConfig();

        // This function causes the toolbars to be destroyed, so
        // clear the configuration of the ToolDocks which refer to
        // these toolbars. This change was needed to stop Audacity
        // crashing when running with Jaws on Windows 10 1703.
        mTopDock->GetConfiguration().Clear();
        mBotDock->GetConfiguration().Clear();

        mTopDock = mBotDock = nullptr; // indicate that it has been destroyed

        for (auto& pair : mBars) {
            pair.second.reset();
        }

        mIndicator.reset();
    }
}

ToolManager::~ToolManager()
{
    Destroy();
}

// This table describes the default configuration of the toolbars as
// a "tree" and must be kept in pre-order traversal.

// In fact this tree is more of a broom -- nothing properly branches except
// at the root.

// "Root" corresponds to left edge of the main window, and successive siblings
// go from top to bottom.  But in practice layout may wrap this abstract
// configuration if the window size is narrow.

static struct DefaultConfigEntry {
    Identifier barID;
    Identifier rightOf; // parent
    Identifier below;  // preceding sibling
} DefaultConfigTable [] = {
    // Top dock row, may wrap
    { wxT("Control"),           {},                     {} },
    { wxT("Tools"),             wxT("Control"),         {} },
    { wxT("Edit"),              wxT("Tools"),           {} },
    { wxT("CutCopyPaste"),      wxT("Edit"),            {} },
    { wxT("Audio Setup"),       wxT("CutCopyPaste"),    {} },
#ifdef HAS_AUDIOCOM_UPLOAD
    {
        wxT("Share Audio"),       wxT("Audio Setup"),     {}
    },
    { wxT("RecordMeter"),       wxT("Share Audio"),     {} },
    { wxT("PlayMeter"),         wxT("Share Audio"),     wxT("RecordMeter"),
    },
#else
    {
        wxT("RecordMeter"),       wxT("Audio Setup"),     {}
    },
    { wxT("PlayMeter"),         wxT("Audio Setup"),     wxT("RecordMeter"),
    },
#endif

    // start another top dock row
    {
        wxT("Device"),             {},                     wxT("Control")
    },

    // Hidden by default in top dock
    { wxT("CombinedMeter"),     {},                     {} },

    // Bottom dock
    { wxT("TimeSignature"),     {},                     {},               },
    { wxT("Snapping"),          wxT("TimeSignature"),   {},               },
    { wxT("Time"),              wxT("Snapping"),        {} },
    { wxT("Selection"),         wxT("Time"),            {} },

    { wxT("Transcription"),     wxT("Selection"),       {} },

    // Hidden by default in bottom dock
    { wxT("SpectralSelection"), {},                     {} },
};

// Static member function.
void ToolManager::OnResetToolBars(const CommandContext& context)
{
    auto& project = context.project;
    auto& toolManager = ToolManager::Get(project);

    toolManager.Reset();
    Get(project).ModifyToolbarMenus(project);
}

void ToolManager::Reset()
{
    // Disconnect all docked bars
    for ( const auto& entry : DefaultConfigTable ) {
        const auto& ndx = entry.barID;
        ToolBar* bar = GetToolBar(ndx);
        if (!bar) {
            continue;
        }

        ToolBarConfiguration::Position position {
            (entry.rightOf == Identifier{}) ? nullptr : GetToolBar(entry.rightOf),
            (entry.below == Identifier{}) ? nullptr : GetToolBar(entry.below)
        };

        bar->SetSize(20, 20);

        wxWindow* floater;
        ToolDock* dock;
        bool expose = true;

        // Disconnect the bar
        if (bar->IsDocked()) {
            bar->GetDock()->Undock(bar);
            floater = NULL;
        } else {
            floater = bar->GetParent();
        }

        // Decide which dock.
        dock = (bar->DefaultDockID() == ToolBar::TopDockID)
               ? mTopDock : mBotDock;

        // PRL: Destroy the tool frame before recreating buttons.
        // This fixes some subtle sizing problems on macOs.
        bar->Reparent(dock);
        //OK (and good) to DELETE floater, as bar is no longer in it.
        if (floater) {
            floater->Destroy();
        }

        // Recreate bar buttons (and resize it)
        bar->SetToDefaultSize();
        bar->ReCreateButtons();
        bar->EnableDisableButtons();

#if 0
        if (bar->IsResizable()) {
            bar->SetSize(bar->GetBestFittingSize());
        }
#endif

        // Hide some bars.
        expose = bar->ShownByDefault() || bar->HideAfterReset();

        // Next condition will always (?) be true, as the reset configuration is
        // with no floating toolbars.
        if (dock != NULL) {
            // when we dock, we reparent, so bar is no longer a child of floater.
            dock->Dock(bar, false, position);
            Expose(bar->GetSection(), expose);
        } else {
            // The (tool)bar has a dragger window round it, the floater.
            // in turn floater will have mParent (the entire App) as its
            // parent.

            // Maybe construct a NEW floater
            // this happens if we have just been bounced out of a dock.
            if (floater == NULL) {
                wxASSERT(mParent);
                floater = safenew ToolFrame(mParent, this, bar, wxPoint(-1, -1));
                bar->Reparent(floater);
            }

            // This bar is undocked and invisible.
            // We are doing a reset toolbars, so even the invisible undocked bars should
            // be moved somewhere sensible. Put bar near center of window.
            // If there were multiple hidden toobars the * 10 adjustment means
            // they won't overlap too much.
            floater->CentreOnParent();
            const auto index = bar->GetIndex();
            floater->Move(
                floater->GetPosition() + wxSize { index* 10 - 200, index * 10 });
            bar->SetDocked(NULL, false);
            Expose(bar->GetSection(), false);
        }
    }

    ForEach([this](auto bar){
        if (bar && bar->HideAfterReset()) {
            Expose(bar->GetSection(), false);
        }
    });
    // TODO:??
    // If audio was playing, we stopped the VU meters,
    // It would be nice to show them again, but hardly essential as
    // they will show up again on the next play.
    // SetVUMeters(AudacityProject *p);
    Updated();
}

void ToolManager::RegenerateTooltips()
{
    ForEach([](auto bar){
        if (bar) {
            bar->RegenerateTooltips();
        }
    });
}

int ToolManager::FilterEvent(wxEvent& event)
{
    // Snoop the global event stream for changes of focused window.  Remember
    // the last one of our own that is not a grabber.

    if (event.GetEventType() == wxEVT_KILL_FOCUS) {
        auto& focusEvent = static_cast<wxFocusEvent&>(event);
        auto window = focusEvent.GetWindow();
        auto top = wxGetTopLevelParent(window);
        if (auto toolFrame = dynamic_cast<ToolFrame*>(top)) {
            top = toolFrame->GetParent();
        }
        // window is that which will GET the focus
        if (window
            && !dynamic_cast<Grabber*>(window)
            && !dynamic_cast<ToolFrame*>(window)
            && top == FindProjectFrame(mParent)) {
            // Note this is a dangle-proof wxWindowRef:
            mLastFocus = window;
        }
    }

    return Event_Skip;
}

//
// Read the toolbar states
//
void ToolManager::ReadConfig()
{
    std::vector<Identifier> unordered[ DockCount ];
    std::vector<ToolBar*> dockedAndHidden;
    std::map<Identifier, bool> show;
    std::map<Identifier, int> width;
    std::map<Identifier, int> height;
    int x, y;
    int dock;
    bool someFound { false };

#if defined(__WXMAC__)
    // Disable window animation
    wxSystemOptions::SetOption(wxMAC_WINDOW_PLAIN_TRANSITION, 1);
#endif

    // Change to the bar root
    auto toolbarsGroup = gPrefs->BeginGroup("/GUI/ToolBars");

    ToolBarConfiguration::Legacy topLegacy, botLegacy;

    int vMajor, vMinor, vMicro;
    GetPreferencesVersion(vMajor, vMinor, vMicro);
    bool useLegacyDock = false;
    // note that vMajor, vMinor, and vMicro will all be zero if either it's a new audacity.cfg file
    // or the version is less than 1.3.13 (when there were no version keys according to the comments in
    // InitPreferences()). So for new audacity.cfg
    // file useLegacyDock will be true, but this doesn't matter as there are no Dock or DockV2 keys in the file yet.
    if (vMajor <= 1
        || (vMajor == 2 && (vMinor <= 1 || (vMinor == 2 && vMicro <= 1)))) { // version <= 2.2.1
        useLegacyDock = true;
    }

    // Load and apply settings for each bar
    ForEach([&](ToolBar* bar){
        //wxPoint Center = mParent->GetPosition() + (mParent->GetSize() * 0.33);
        //wxPoint Center(
        //   wxSystemSettings::GetMetric( wxSYS_SCREEN_X ) /2 ,
        //   wxSystemSettings::GetMetric( wxSYS_SCREEN_Y ) /2 );

        // Change to the bar subkey
        auto ndx = bar->GetSection();
        auto barGroup = gPrefs->BeginGroup(ndx.GET());

        const bool bShownByDefault = bar->ShownByDefault();
        const int defaultDock = bar->DefaultDockID();

        // Read in all the settings

        if (useLegacyDock) {
            gPrefs->Read(wxT("Dock"), &dock, -1);     // legacy version of DockV2
        } else {
            gPrefs->Read(wxT("DockV2"), &dock, -1);
        }

        const bool found = (dock != -1);
        if (found) {
            someFound = true;
        }
        if (!found) {
            dock = defaultDock;
        }

        ToolDock* d;
        ToolBarConfiguration::Legacy* pLegacy;
        switch (dock) {
            case TopDockID: d = mTopDock;
                pLegacy = &topLegacy;
                break;
            case BotDockID: d = mBotDock;
                pLegacy = &botLegacy;
                break;
            default:        d = nullptr;
                pLegacy = nullptr;
                break;
        }

        bool ordered = ToolBarConfiguration::Read(
            d ? &d->GetConfiguration() : nullptr,
            pLegacy,
            bar, show[ ndx ], bShownByDefault)
                       && found;

        gPrefs->Read(wxT("X"), &x, -1);
        gPrefs->Read(wxT("Y"), &y, -1);
        gPrefs->Read(wxT("W"), &width[ ndx ], -1);
        gPrefs->Read(wxT("H"), &height[ ndx ], -1);

        bar->SetVisible(show[ ndx ]);

        // Docked or floating?
        if (dock) {
            // Default to top dock if the ID isn't valid
            if (dock < NoDockID || dock > DockCount) {
                dock = TopDockID;
            }

            // Create the bar with the correct parent
            if (dock == TopDockID) {
                bar->Create(mTopDock);
            } else {
                bar->Create(mBotDock);
            }

            // Set the width and height
            if (width[ ndx ] != -1 && height[ ndx ] != -1) {
                wxSize sz(width[ ndx ], height[ ndx ]);
                bar->SetSize(sz);
                bar->ResizingDone();
            }

            // Set the width
            if (width[ ndx ] >= bar->GetSize().x) {
                wxSize sz(width[ ndx ], bar->GetSize().y);
                bar->SetSize(sz);
                bar->Layout();
            }

            // make a note of docked and hidden toolbars
            if (!show[ndx]) {
                dockedAndHidden.push_back(bar);
            }

            if (!ordered) {
                // These must go at the end
                unordered[ dock - 1 ].push_back(ndx);
            }
        } else {
            // Create the bar (with the top dock being temporary parent)
            bar->Create(mTopDock);

            // Construct a NEW floater
            wxASSERT(mParent);
            ToolFrame* f = safenew ToolFrame(mParent, this, bar, wxPoint(x, y));

            // Set the width and height
            if (width[ ndx ] != -1 && height[ ndx ] != -1) {
                wxSize sz(width[ ndx ], height[ ndx ]);
                f->SetSizeHints(sz);
                f->SetSize(sz);
                f->Layout();
                if ((x != -1) && (y != -1)) {
                    bar->SetPositioned();
                }
            }

            // Required on Linux Xfce
            wxSize msz(width[ndx], height[ndx] - 1);
            bar->GetParent()->SetMinSize(msz);

            // Inform toolbar of change
            bar->SetDocked(NULL, false);

            // Show or hide it
            Expose(bar->GetSection(), show[ ndx ]);
        }
    });

    mTopDock->GetConfiguration().PostRead(topLegacy);
    mBotDock->GetConfiguration().PostRead(botLegacy);

    // Add all toolbars to their target dock
    for ( dock = 0; dock < DockCount; dock++ ) {
        ToolDock* d = (dock + 1 == TopDockID ? mTopDock : mBotDock);

        d->LoadConfig();

        // Add all unordered toolbars
        for ( int ord = 0; ord < (int)unordered[ dock ].size(); ord++ ) {
            ToolBar* t = mBars[unordered[dock][ord]].get();

            // Dock it
            d->Dock(t, false);

            // Show or hide the bar
            Expose(t->GetSection(), show[ t->GetSection() ]);
        }
    }

    // hidden docked toolbars
    for (auto bar : dockedAndHidden) {
        bar->SetVisible(false);
        bar->GetDock()->Dock(bar, false);
        bar->Expose(false);
    }

    toolbarsGroup.Reset();

#if defined(__WXMAC__)
    // Reinstate original transition
    wxSystemOptions::SetOption(wxMAC_WINDOW_PLAIN_TRANSITION, mTransition);
#endif

    // Setup the neighbors according to the
    // default config

    for (const auto& entry : DefaultConfigTable) {
        const auto& ndx = entry.barID;
        const auto bar = GetToolBar(ndx);
        if (bar != nullptr) {
            bar->SetPreferredNeighbors(entry.rightOf, entry.below);
        }
    }

    if (!someFound) {
        Reset();
    }
}

//
// Save the toolbar states
//
void ToolManager::WriteConfig()
{
    if (!gPrefs) {
        return;
    }

    auto toolbarsGroup = gPrefs->BeginGroup("/GUI/ToolBars");

    // Save state of each bar
    ForEach([this](ToolBar* bar){
        // Change to the bar subkey
        auto sectionGroup = gPrefs->BeginGroup(bar->GetSection().GET());

        // Search both docks for toolbar order
        bool to = mTopDock->GetConfiguration().Contains(bar);
        bool bo = mBotDock->GetConfiguration().Contains(bar);

        // Save
        // Note that DockV2 was introduced in 2.2.2 to fix bug #1554. Dock is retained so that
        // the toolbar layout is not changed when opening a version before 2.2.2, and in particular
        // its value is compatible with versions 2.1.3 to 2.2.1 which have this bug.
        ToolDock* dock = bar->GetDock();     // dock for both shown and hidden toolbars
        gPrefs->Write(wxT("DockV2"), static_cast<int>(dock == mTopDock ? TopDockID : dock == mBotDock ? BotDockID : NoDockID));

        gPrefs->Write(wxT("Dock"), static_cast<int>(to ? TopDockID : bo ? BotDockID : NoDockID));

        dock = to ? mTopDock : bo ? mBotDock : nullptr;  // dock for shown toolbars
        ToolBarConfiguration::Write
            (dock ? &dock->GetConfiguration() : nullptr, bar);

        wxPoint pos(-1, -1);
        wxSize sz = bar->GetSize();
        if (!bar->IsDocked() && bar->IsPositioned()) {
            pos = bar->GetParent()->GetPosition();
            sz = bar->GetParent()->GetSize();
        }
        gPrefs->Write(wxT("X"), pos.x);
        gPrefs->Write(wxT("Y"), pos.y);
        gPrefs->Write(wxT("W"), sz.x);
        gPrefs->Write(wxT("H"), sz.y);
    });
    gPrefs->Flush();
}

//
// Return a pointer to the specified toolbar or nullptr
//
ToolBar* ToolManager::GetToolBar(const Identifier& type) const
{
    auto end = mBars.end(), iter = mBars.find(type);
    return (iter == end) ? nullptr : iter->second.get();
}

//
// Return a pointer to the top dock
//
ToolDock* ToolManager::GetTopDock()
{
    return mTopDock;
}

const ToolDock* ToolManager::GetTopDock() const
{
    return mTopDock;
}

//
// Return a pointer to the bottom dock
//
ToolDock* ToolManager::GetBotDock()
{
    return mBotDock;
}

const ToolDock* ToolManager::GetBotDock() const
{
    return mBotDock;
}

//
// Queues an EVT_TOOLBAR_UPDATED command event to notify any
// interest parties of an updated toolbar or dock layout
//
void ToolManager::Updated()
{
    // Queue an update event
    wxCommandEvent e(EVT_TOOLBAR_UPDATED);
    GetProjectFrame(*mParent).GetEventHandler()->AddPendingEvent(e);
}

//
// Return docked state of specified toolbar
//
bool ToolManager::IsDocked(Identifier type) const
{
    if (auto pBar = GetToolBar(type)) {
        return pBar->IsDocked();
    }
    return false;
}

//
// Returns the visibility of the specified toolbar
//
bool ToolManager::IsVisible(Identifier type) const
{
    if (auto pBar = GetToolBar(type)) {
        return pBar->IsVisible();
    }
    return false;

#if 0
    // If toolbar is floating
    if (!t->IsDocked()) {
        // Must return state of floater window
        return t->GetParent()->IsShown();
    }

    // Return state of docked toolbar
    return t->IsShown();
#endif
}

//
// Toggles the visible/hidden state of a toolbar
//
void ToolManager::ShowHide(Identifier type)
{
    Expose(type, !mBars[type]->IsVisible());
    Updated();
}

//
// Set the visible/hidden state of a toolbar
//
void ToolManager::Expose(Identifier type, bool show)
{
    ToolBar* t = mBars[type].get();

    // Handle docked and floaters differently
    if (t->IsDocked()) {
        t->GetDock()->Expose(t->GetSection(), show);
    } else {
        t->Expose(show);
    }
}

//
// Ask both docks to (re)layout their bars
//
void ToolManager::LayoutToolBars()
{
    // Update the layout
    if (mTopDock) {
        mTopDock->LayoutToolBars();
    }

    if (mBotDock) {
        mBotDock->LayoutToolBars();
    }
}

//
// Handle toolbar dragging
//
void ToolManager::OnMouse(wxMouseEvent& event)
{
    // Go ahead and set the event to propagate
    event.Skip();

    // Can't do anything if we're not dragging.  This also prevents
    // us from intercepting events that don't belong to us from the
    // parent since we're Connect()ed to a couple.
    if (!mClicked) {
        return;
    }

#if defined(__WXMAC__)
    // Disable window animation
    wxSystemOptions::SetOption(wxMAC_WINDOW_PLAIN_TRANSITION, 1);
#endif

    // Retrieve the event position
    wxPoint pos
        =((wxWindow*)event.GetEventObject())->ClientToScreen(event.GetPosition()) - mDragOffset;

    if (!event.LeftIsDown()) {
        // Button was released...finish the drag
        // Transition the bar to a dock
        if (!mDidDrag) {
            if (mPrevDock) {
                mPrevDock->RestoreConfiguration(mPrevConfiguration);
            }
            DoneDragging();
            return;
        } else if (mDragDock && !event.ShiftDown()) {
            // Trip over...everyone ashore that's going ashore...
            mDragDock->Dock(mDragBar, true, mDragBefore);
            Updated();
            mDragWindow->ClearBar();

            // Done with the floater
            mDragWindow->Destroy();
            mDragWindow = nullptr;
            mDragBar->Refresh(false);
        } else {
            // Calling SetDocked() to force the grabber button to popup
            mDragBar->SetDocked(NULL, false);
        }

        DoneDragging();
    } else if (event.Dragging() && pos != mLastPos) {
        if (!mDidDrag) {
            // Must set the bar afloat if it's currently docked
            mDidDrag = true;
            wxPoint mp = event.GetPosition();
            mp = GetProjectFrame(*mParent).ClientToScreen(mp);
            if (!mDragWindow) {
                // We no longer have control
                if (mPrevDock) {
                    mPrevDock->GetConfiguration().Remove(mDragBar);
                }
                UndockBar(mp);
                // Rearrange the remaining toolbars before trying to re-insert this one.
                LayoutToolBars();
            }
        }

        // Make toolbar follow the mouse
        mDragWindow->Move(pos);

        // Remember to prevent excessive movement
        mLastPos = pos;

        // Calc the top dock hittest rectangle
        wxRect tr = mTopDock->GetRect();
        tr.SetBottom(tr.GetBottom() + 10);
        tr.SetPosition(mTopDock->GetParent()->ClientToScreen(tr.GetPosition()));

        // Calc the bottom dock hittest rectangle
        wxRect br = mBotDock->GetRect();
        br.SetTop(br.GetTop() - 10);
        br.SetBottom(br.GetBottom() + 20);
        br.SetPosition(mBotDock->GetParent()->ClientToScreen(br.GetPosition()));

        // Add half the bar height.  We could use the actual bar height, but that would be confusing as a
        // bar removed at a place might not dock back there if just let go.
        // Also add 5 pixels in horizontal direction, so that a click without a move (or a very small move)
        // lands back where we started.
        pos +=  wxPoint(5, 20);

        // To find which dock, rather than test against pos, test against the whole dragger rect.
        // This means it is enough to overlap the dock to dock with it.
        wxRect barRect = mDragWindow->GetRect();
        ToolDock* dock = NULL;
        if (tr.Intersects(barRect)) {
            dock = mTopDock;
        } else if (br.Intersects(barRect)) {
            dock = mBotDock;
        }

        // Looks like we have a winner...
        if (dock) {
            wxPoint p;
            wxRect r;

            // Calculate where the bar would be placed
            mDragBefore = dock->PositionBar(mDragBar, pos, r);

            // If different than the last time, the indicator must be moved
            if (r != mBarPos) {
                wxRect dr = dock->GetRect();

                // Hide the indicator before changing the shape
                mIndicator->Hide();

                // Decide which direction the arrow should point
                if (r.GetTop() >= dr.GetHeight()) {
                    const auto& box = mDown->GetBox();
                    p.x = dr.GetLeft() + (dr.GetWidth() / 2)
                          - (box.GetWidth() / 2);
                    p.y = dr.GetBottom() - box.GetHeight();
                    mCurrent = mDown.get();
                } else {
                    // r is the rectangle of the toolbar being dragged.
                    // A tall undocked toolbar will become at most 2 tbs
                    // high when docked, so the triangular drop indicator
                    // needs to use that height, h, not the bar height
                    // for calculating where to be drawn.
                    const int tbs = toolbarSingle + toolbarGap;
                    int h = wxMin(r.GetHeight(), 2 * tbs - 1);
                    p.x = dr.GetLeft() + r.GetLeft();
                    p.y = dr.GetTop() + r.GetTop()
                          + ((h - mLeft->GetBox().GetHeight()) / 2);
                    mCurrent = mLeft.get();
                }

                // Change the shape while hidden and then show it if okay
                mIndicator->SetShape(*mCurrent);
                if (!event.ShiftDown()) {
                    mIndicator->Show();
                    mIndicator->Update();
                }

                // Move it into position
                // LL:  Do this after the Show() since KDE doesn't move the window
                //      if it's not shown.  (Do it outside if the previous IF as well)
                mIndicator->Move(dock->GetParent()->ClientToScreen(p));

                // Remember for next go round
                mBarPos = r;
            }
        } else {
            // Hide the indicator if it's still shown
            if (mBarPos.x != -1) {
                // Hide any
                mIndicator->Hide();
                mBarPos.x = -1;
                mBarPos.y = -1;
            }
        }

        // Remember to which dock the drag bar belongs.
        mDragDock = dock;
    }

#if defined(__WXMAC__)
    // Reinstate original transition
    wxSystemOptions::SetOption(wxMAC_WINDOW_PLAIN_TRANSITION, mTransition);
#endif
}

//
// Deal with NEW capture lost event
//
void ToolManager::OnCaptureLost(wxMouseCaptureLostEvent& event)
{
    // Can't do anything if we're not dragging.  This also prevents
    // us from intercepting events that don't belong to us from the
    // parent since we're Connect()ed to a couple.
    if (!mDragWindow) {
        event.Skip();
        return;
    }

    // Simulate button up
    wxMouseEvent e(wxEVT_LEFT_UP);
    e.SetEventObject(mParent);
    OnMouse(e);
}

void ToolManager::OnMenuUpdate(MenuUpdateMessage)
{
    ModifyToolbarMenus(*mParent);
}

//
// Watch for shift key changes
//
void ToolManager::OnTimer(wxTimerEvent& event)
{
    // Go ahead and set the event to propagate
    event.Skip();

    // Can't do anything if we're not dragging.  This also prevents
    // us from intercepting events that don't belong to us from the
    // parent since we're Connect()ed to a couple.
    if (!mDragWindow) {
        return;
    }

    bool state = wxGetKeyState(WXK_SHIFT);
    if (mLastState != state) {
        mLastState = state;

#if defined(__WXMAC__)
        // Disable window animation
        wxSystemOptions::SetOption(wxMAC_WINDOW_PLAIN_TRANSITION, 1);
#endif

        mIndicator->Show(!state);

#if defined(__WXMAC__)
        // Disable window animation
        wxSystemOptions::SetOption(wxMAC_WINDOW_PLAIN_TRANSITION, mTransition);
#endif
    }

    return;
}

//
// Handle Indicator paint events
//
// Really only needed for the Mac since SetBackgroundColour()
// doesn't seem to work with shaped frames.
//
void ToolManager::OnIndicatorPaint(wxPaintEvent& event)
{
    // TODO: Better to use a bitmap than a triangular region.
    wxWindow* w = (wxWindow*)event.GetEventObject();
    wxPaintDC dc(w);
    // TODO: Better (faster) to use the existing spare brush.
    wxBrush brush(theTheme.Colour(clrTrackPanelText));
    dc.SetBackground(brush);
    dc.Clear();
}

//
// Handle Indicator creation event
//
// Without this, the initial Indicator window will be a solid blue square
// until the next time it changes.
//
void ToolManager::OnIndicatorCreate(wxWindowCreateEvent& event)
{
#if defined(__WXGTK__)
    mIndicator->SetShape(*mCurrent);
#endif
    event.Skip();
}

void ToolManager::UndockBar(wxPoint mp)
{
#if defined(__WXMAC__)
    // Disable window animation
    wxSystemOptions::SetOption(wxMAC_WINDOW_PLAIN_TRANSITION, 1);
#endif

    // Adjust the starting position
    mp -= mDragOffset;

    // Inform toolbar of change
    mDragBar->SetDocked(NULL, true);
    mDragBar->SetPositioned();

    // Construct a NEW floater
    wxASSERT(mParent);
    mDragWindow = safenew ToolFrame(mParent, this, mDragBar, mp);
    mDragWindow->SetLayoutDirection(wxLayout_LeftToRight);
    // Make sure the ferry is visible
    mDragWindow->Show();

    // Notify parent of change
    Updated();

#if defined(__WXMAC__)
    // Reinstate original transition
    wxSystemOptions::SetOption(wxMAC_WINDOW_PLAIN_TRANSITION, mTransition);
#endif
}

//
// Transition a toolbar from float to dragging
//
void ToolManager::OnGrabber(GrabberEvent& event)
{
    // No need to propagate any further
    event.Skip(false);

    if (event.IsEscaping()) {
        return HandleEscapeKey();
    }

    // Remember which bar we're dragging
    mDragBar = GetToolBar(event.BarId());

    // Remember state, in case of ESCape key later
    if (mDragBar->IsDocked()) {
        mPrevDock = dynamic_cast<ToolDock*>(mDragBar->GetParent());
        wxASSERT(mPrevDock);
        mPrevSlot = mPrevDock->GetConfiguration().Find(mDragBar);
        mPrevDock->WrapConfiguration(mPrevConfiguration);
    } else {
        mPrevPosition = mDragBar->GetParent()->GetPosition();
    }

    // Calculate the drag offset
    wxPoint mp = event.GetPosition();
    mDragOffset = mp
                  - mDragBar->GetParent()->ClientToScreen(mDragBar->GetPosition())
                  + wxPoint(1, 1);

    mClicked = true;
    if (mPrevDock) {
        mDragWindow = nullptr;
    } else {
        mDragWindow = (ToolFrame*)mDragBar->GetParent();
    }

    // We want all mouse events from this point on
    auto& window = GetProjectFrame(*mParent);
    if (!window.HasCapture()) {
        window.CaptureMouse();
    }

    // Start monitoring shift key changes
    mLastState = wxGetKeyState(WXK_SHIFT);
    mTimer.Start(100);
}

void ToolManager::HandleEscapeKey()
{
    if (mDragBar) {
        if (mPrevDock) {
            // Sheriff John Stone,
            // Why don't you leave me alone?
            // Well, I feel so break up
            // I want to go home.
            mPrevDock->RestoreConfiguration(mPrevConfiguration);
            mPrevDock->Dock(mDragBar, true, mPrevSlot);
            Updated();

            // Done with the floater
            mDragWindow->ClearBar();
            mDragWindow->Destroy();
            mDragWindow = nullptr;
            mDragBar->Refresh(false);
        } else {
            // Floater remains, and returns to where it begain
            auto parent = mDragBar->GetParent();
            parent->SetPosition(mPrevPosition);
            mDragBar->SetDocked(NULL, false);
        }

        DoneDragging();
    }
}

void ToolManager::DoneDragging()
{
    // Done dragging - ensure grabber button isn't pushed
    if (mDragBar) {
        mDragBar->SetDocked(mDragBar->GetDock(), false);
    }

    // Release capture
    auto& window = GetProjectFrame(*mParent);
    if (window.HasCapture()) {
        window.ReleaseMouse();
    }

    // Hide the indicator
    mIndicator->Hide();

    mDragWindow = NULL;
    mDragDock = NULL;
    mDragBar = NULL;
    mPrevDock = NULL;
    mPrevSlot = { ToolBarConfiguration::UnspecifiedPosition };
    mPrevConfiguration.Clear();
    mLastPos.x = mBarPos.x = -1;
    mLastPos.y = mBarPos.y = -1;
    mTimer.Stop();
    mDidDrag = false;
    mClicked = false;

    RestoreFocus();
}

bool ToolManager::RestoreFocus()
{
    if (mLastFocus) {
        auto temp1 = AButton::TemporarilyAllowFocus();
        auto temp2 = ASlider::TemporarilyAllowFocus();
        auto temp3 = MeterPanelBase::TemporarilyAllowFocus();
        mLastFocus->SetFocus();
        return true;
    }
    return false;
}

void ToolManager::ModifyAllProjectToolbarMenus()
{
    for (auto pProject : AllProjects{}) {
        auto& project = *pProject;
        ModifyToolbarMenus(project);
    }
}

#include "CommandManager.h"
void ToolManager::ModifyToolbarMenus(AudacityProject& project)
{
    // Refreshes can occur during shutdown and the toolmanager may already
    // be deleted, so protect against it.
    auto& toolManager = ToolManager::Get(project);

    // Now, go through each toolbar, and call EnableDisableButtons()
    toolManager.ForEach([](auto bar){
        if (bar) {
            bar->EnableDisableButtons();
        }
    });

    // These don't really belong here, but it's easier and especially so for
    // the Edit toolbar and the sync-lock menu item.
    bool active = SyncLockTracks.Read();
    SyncLockState::Get(project).SetSyncLock(active);

    CommandManager::Get(project).UpdateCheckmarks();
}

using namespace MenuRegistry;

AttachedToolBarMenuItem::AttachedToolBarMenuItem(
    Identifier id, const CommandID& name, const TranslatableString& label_in, const Registry::OrderingHint& hint,
    std::vector< Identifier > excludeIDs)
    : mId{id}
    , mAttachedItem{
                    (MenuRegistry::FinderScope(
                         [this](AudacityProject&) -> CommandHandlerObject&
    { return *this; }),
                     MenuRegistry::Command(name, label_in,
                                           &AttachedToolBarMenuItem::OnShowToolBar,
                                           AlwaysEnabledFlag,
                                           Options {}.CheckTest([id](AudacityProject& project){
        auto& toolManager = ToolManager::Get(project);
        return toolManager.IsVisible(id);
    }))),
                    Registry::Placement { wxT("View/Other/Toolbars/Toolbars/Other"), hint }
                    },
    mExcludeIds{ std::move(excludeIDs) }
{}

void AttachedToolBarMenuItem::OnShowToolBar(const CommandContext& context)
{
    auto& project = context.project;
    auto& toolManager = ToolManager::Get(project);

    if (!toolManager.IsVisible(mId)) {
        for ( const auto excludedID : mExcludeIds ) {
            toolManager.Expose(excludedID, false);
        }
    }

    toolManager.ShowHide(mId);
    ToolManager::Get(project).ModifyToolbarMenus(project);
}
