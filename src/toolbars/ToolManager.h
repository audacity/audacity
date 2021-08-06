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
#include "ToolDock.h"

#include "../commands/CommandFunctors.h"
#include "../commands/CommandManager.h"


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

class AUDACITY_DLL_API ToolManager final
   : public wxEvtHandler
   , public wxEventFilter
   , public ClientData::Base
{

 public:
   // a hook function to break dependency of ToolManager on ProjectWindow
   using GetTopPanelHook = std::function< wxWindow*( wxWindow& ) >;
   static GetTopPanelHook SetGetTopPanelHook( const GetTopPanelHook& );

   static ToolManager &Get( AudacityProject &project );
   static const ToolManager &Get( const AudacityProject &project );

   ToolManager( AudacityProject *parent );
   ToolManager( const ToolManager & ) PROHIBITED;
   ToolManager &operator=( const ToolManager & ) PROHIBITED;
   ~ToolManager();

   void CreateWindows();

   void LayoutToolBars();

   bool IsDocked( int type );

   bool IsVisible( int type );

   void ShowHide( int type );

   void Expose( int type, bool show );

   ToolBar *GetToolBar( int type ) const;

   ToolDock *GetTopDock();
   const ToolDock *GetTopDock() const;
   ToolDock *GetBotDock();
   const ToolDock *GetBotDock() const;

   void Reset();
   static void OnResetToolBars(const CommandContext &context);

   void Destroy();
   void RegenerateTooltips();

   int FilterEvent(wxEvent &event) override;

   bool RestoreFocus();

 private:

   ToolBar *Float( ToolBar *t, wxPoint & pos );

   void OnTimer( wxTimerEvent & event );
   void OnMouse( wxMouseEvent & event );
   void OnCaptureLost( wxMouseCaptureLostEvent & event );
   void UndockBar( wxPoint mp );
   void OnGrabber( GrabberEvent & event );
   void HandleEscapeKey();
   void DoneDragging();

   void OnIndicatorCreate( wxWindowCreateEvent & event );
   void OnIndicatorPaint( wxPaintEvent & event );

   void ReadConfig();
   void WriteConfig();
   void Updated();

   AudacityProject *mParent;
   wxWindowRef mLastFocus{};

   ToolFrame *mDragWindow;
   ToolDock *mDragDock;
   ToolBar *mDragBar {};
   wxPoint mDragOffset;
   ToolBarConfiguration::Position mDragBefore {};

   wxPoint mLastPos;
   wxRect mBarPos;

   using FramePtr = Destroy_ptr<wxFrame>;
   FramePtr mIndicator;
   std::unique_ptr<wxRegion> mLeft, mDown;
   wxRegion *mCurrent;

   wxTimer mTimer;
   bool mLastState;

#if defined(__WXMAC__)
   bool mTransition;
#endif

   ToolDock *mTopDock{};
   ToolDock *mBotDock{};

   ToolBar::Holder mBars[ ToolBarCount ];

   wxPoint mPrevPosition {};
   ToolDock *mPrevDock {};
   ToolBarConfiguration::Position mPrevSlot
      { ToolBarConfiguration::UnspecifiedPosition };
   ToolBarConfiguration mPrevConfiguration;
   bool mDidDrag{};
   bool mClicked{};

 public:

   DECLARE_CLASS( ToolManager )
   DECLARE_EVENT_TABLE()
};


////////////////////////////////////////////////////////////
/// class ToolFrame
////////////////////////////////////////////////////////////

class ToolFrame final : public wxFrame
{
public:

   ToolFrame( AudacityProject *parent, ToolManager *manager, ToolBar *bar, wxPoint pos );

   ~ToolFrame();

   ToolBar *GetBar() { return mBar; }
   void ClearBar() { mBar = nullptr; }
   void LockInMinSize(ToolBar * pBar);

   //
   // Transition a toolbar from float to dragging
   //
   void OnGrabber( GrabberEvent & event );

   //
   // Handle toolbar updates
   //
   void OnToolBarUpdate( wxCommandEvent & event );

   //
   // Handle frame paint events
   //
   void OnPaint( wxPaintEvent & WXUNUSED(event) );

   void OnMotion( wxMouseEvent & event );

   void OnCaptureLost( wxMouseCaptureLostEvent & WXUNUSED(event) );

   //
   // Do not allow the window to close through keyboard accelerators
   // (like ALT+F4 on Windows)
   //
   void OnClose( wxCloseEvent & event );

   void OnKeyDown( wxKeyEvent &event );

   void Resize( const wxSize &size );

private:

   AudacityProject *const mParent;
   ToolManager *mManager;
   ToolBar *mBar;
   wxSize mMinSize;
   wxSize mOrigSize;

public:

   DECLARE_CLASS( ToolFrame )
   DECLARE_EVENT_TABLE()
};



// Construct a static instance of this class to add a menu item that shows and
// hides a toolbar
struct AUDACITY_DLL_API AttachedToolBarMenuItem : CommandHandlerObject {
   AttachedToolBarMenuItem(
      ToolBarID id, const CommandID &name, const TranslatableString &label_in,
      const Registry::OrderingHint &hint = {},
      // IDs of other toolbars not to be shown simultaneously with this one:
      std::vector< ToolBarID > excludeIds = {} );

   void OnShowToolBar(const CommandContext &context);

   const ToolBarID mId;
   const MenuTable::AttachedItem mAttachedItem;
   const std::vector< ToolBarID > mExcludeIds;
};

#endif
