/**********************************************************************

  Audacity: A Digital Audio Editor

  ToolManager.h

  Dominic Mazzoni
  Shane T. Mueller
  Leland Lucius

**********************************************************************/

#ifndef __AUDACITY_TOOLMANAGER__
#define __AUDACITY_TOOLMANAGER__

#include "../MemoryX.h"
#include <wx/defs.h>
#include <wx/frame.h>
#include <wx/timer.h>

#include "ToolDock.h"
#include "ToolBar.h"

class wxBitmap;
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
class ToolFrame;

////////////////////////////////////////////////////////////
/// class ToolManager
////////////////////////////////////////////////////////////

class ToolManager final : public wxEvtHandler
{

 public:

   ToolManager( AudacityProject *parent, wxWindow *topDockParent );
   ~ToolManager();

   void LayoutToolBars();
   void UpdatePrefs();

   bool IsDocked( int type );

   bool IsVisible( int type );

   void ShowHide( int type );

   void Expose( int type, bool show );

   ToolBar *GetToolBar( int type ) const;

   ToolDock *GetTopDock();
   ToolDock *GetBotDock();

   void Reset();
   void RegenerateTooltips();

 private:

   ToolBar *Float( ToolBar *t, wxPoint & pos );

   void OnTimer( wxTimerEvent & event );
   void OnMouse( wxMouseEvent & event );
   void OnCaptureLost( wxMouseCaptureLostEvent & event );
   void OnGrabber( GrabberEvent & event );
   void HandleEscapeKey();
   void DoneDragging();

   void OnIndicatorCreate( wxWindowCreateEvent & event );
   void OnIndicatorPaint( wxPaintEvent & event );

   void ReadConfig();
   void WriteConfig();
   void Updated();

   AudacityProject *mParent;

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

   ToolDock *mTopDock;
   ToolDock *mBotDock;

   ToolBar::Holder mBars[ ToolBarCount ];

   wxPoint mPrevPosition {};
   ToolDock *mPrevDock {};
   ToolBarConfiguration::Position mPrevSlot
      { ToolBarConfiguration::UnspecifiedPosition };
   ToolBarConfiguration mPrevConfiguration;

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

   ToolFrame( wxWindow *parent, ToolManager *manager, ToolBar *bar, wxPoint pos );

   ~ToolFrame();

   ToolBar *GetBar() { return mBar; }
   void ClearBar() { mBar = nullptr; }

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

   wxWindow *mParent;
   ToolManager *mManager;
   ToolBar *mBar;
   wxSize mMinSize;
   wxSize mOrigSize;

public:

   DECLARE_CLASS( ToolFrame )
   DECLARE_EVENT_TABLE()
};


#endif
