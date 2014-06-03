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

#include "../Audacity.h"

// For compilers that support precompilation, includes "wx/wx.h".
#include <wx/wxprec.h>

#ifndef WX_PRECOMP
#include <wx/app.h>
#include <wx/defs.h>
#include <wx/event.h>
#include <wx/frame.h>
#include <wx/gdicmn.h>
#include <wx/intl.h>
#include <wx/region.h>
#include <wx/settings.h>
#include <wx/sysopt.h>
#include <wx/timer.h>
#include <wx/utils.h>
#include <wx/window.h>
#endif  /*  */

#include <wx/minifram.h>
#include <wx/popupwin.h>

#if defined(__WXMAC__)
#include <wx/mac/uma.h>
#endif

#include "ToolManager.h"
#include "ControlToolBar.h"
#include "DeviceToolBar.h"
#include "EditToolBar.h"
#include "MeterToolBar.h"
#include "MixerToolBar.h"
#include "SelectionBar.h"
#include "ToolsToolBar.h"
#include "TranscriptionToolBar.h"

#include "../AColor.h"
#include "../AllThemeResources.h"
#include "../ImageManipulation.h"
#include "../Prefs.h"
#include "../Project.h"
#include "../Theme.h"
#include "../widgets/AButton.h"
#include "../widgets/Grabber.h"

////////////////////////////////////////////////////////////
/// Methods for ToolFrame
////////////////////////////////////////////////////////////
#define sizerW 11

//
// Constructor
//
class ToolFrame:public wxFrame
{
 public:

   ToolFrame( wxWindow *parent, ToolManager *manager, ToolBar *bar, wxPoint pos )
   : wxFrame( parent,
              bar->GetId(),
              wxEmptyString,
              pos,
              wxDefaultSize,
              wxNO_BORDER |
              wxFRAME_NO_TASKBAR |
              wxFRAME_TOOL_WINDOW |
              wxFRAME_FLOAT_ON_PARENT )
   {
      int width = bar->GetSize().x;
      int border;

      // OSX doesn't need a border, but Windows and Linux do
      border = 1;
#if defined(__WXMAC__)
      border = 0;

   // WXMAC doesn't support wxFRAME_FLOAT_ON_PARENT, so we do
   //
   // LL:  I've commented this out because if you have, for instance, the meter
   //      toolbar undocked and large and then you open a dialog like an effect,
   //      the dialog may appear behind the dialog and you can't move either one.
   //
   //      However, I'm leaving it here because I don't remember why I'd included
   //      it in the first place.
// SetWindowClass((WindowRef)d.MacGetWindowRef(), kFloatingWindowClass);
#endif

      // Save parameters
      mParent = parent;
      mManager = manager;
      mBar = bar;

      // Transfer the bar to the ferry
      bar->Reparent( this );

      // We use a sizer to maintain proper spacing
      wxBoxSizer *s = new wxBoxSizer( wxHORIZONTAL );

      // Add the bar to the sizer
      s->Add( bar, 1, wxEXPAND | wxALL, border );

      // Add space for the resize grabber
      if( bar->IsResizable() )
      {
         s->Add( sizerW, 1 );
         width += sizerW;
      }

      SetSize( width + 2, bar->GetMinSize().y + 2 );

      // Attach the sizer and resize the window to fit
      SetSizer( s );
      Layout();

      // Inform toolbar of change
      bar->SetDocked( NULL, true );

      // Make sure resizable floaters don't get any smaller than initial size
      if( bar->IsResizable() )
      {
         // Calc the minimum size of the frame
         mMinSize = bar->GetMinSize() + ( GetSize() - bar->GetSize() );
      }
   }

   //
   // Transition a toolbar from float to dragging
   //
   void OnGrabber( GrabberEvent & event )
   {
      // Pass it on to the manager since it isn't in the handling hierarchy
      mManager->ProcessEvent( event );
   }

   //
   // Handle toolbar updates
   //
   void OnToolBarUpdate( wxCommandEvent & event )
   {
      // Resize floater window to exactly contain toolbar
      mBar->GetParent()->SetClientSize( mBar->GetMinSize() );

      // Allow it to propagate to our parent
      event.Skip();
   }

   //
   // Handle frame paint events
   //
   void OnPaint( wxPaintEvent & WXUNUSED(event) )
   {
      wxPaintDC dc( this );
      wxSize sz = GetSize();
      wxRect r;

      dc.SetPen( wxColour( 90, 90, 90 ) );

#if !defined(__WXMAC__)
      dc.SetBackground(wxBrush(wxSystemSettings::GetColour(wxSYS_COLOUR_3DFACE)));
      dc.Clear();
      dc.SetBrush( *wxTRANSPARENT_BRUSH );
      dc.DrawRectangle( 0, 0, sz.GetWidth(), sz.GetHeight() );
#endif

      if( mBar->IsResizable() )
      {
         r.x = sz.x - sizerW - 2,
         r.y = sz.y - sizerW - 2;
         r.width = sizerW + 2;
         r.height = sizerW + 2;

         AColor::Line(dc, r.GetLeft(), r.GetBottom(), r.GetRight(), r.GetTop() );
         AColor::Line(dc, r.GetLeft() + 3, r.GetBottom(), r.GetRight(), r.GetTop() + 3 );
         AColor::Line(dc, r.GetLeft() + 6, r.GetBottom(), r.GetRight(), r.GetTop() + 6 );
         AColor::Line(dc, r.GetLeft() + 9, r.GetBottom(), r.GetRight(), r.GetTop() + 9 );
      }

   }

   void OnMotion( wxMouseEvent & event )
   {
      // Don't do anything if we're docked or not resizeable
      if( mBar->IsDocked() || !mBar->IsResizable() )
      {
         return;
      }

      // Retrieve the mouse position
      wxPoint pos = ClientToScreen( event.GetPosition() );
      if( HasCapture() && event.Dragging() )
      {
         wxRect rect = GetRect();

         rect.SetBottomRight( pos );
         if( rect.width < mMinSize.x )
         {
            rect.width = mMinSize.x;
         }

         if( rect.height < mMinSize.y )
         {
            rect.height = mMinSize.y;
         }

         SetMinSize( rect.GetSize() );
         SetSize( rect.GetSize() );
         Layout();
         Refresh( false );
      }
      else if( HasCapture() && event.LeftUp() )
      {
         ReleaseMouse();
      }
      else if( !HasCapture() )
      {
         wxRect rect = GetRect();
         wxRect r;

         r.x = rect.GetRight() - sizerW - 2,
         r.y = rect.GetBottom() - sizerW - 2;
         r.width = sizerW + 2;
         r.height = sizerW + 2;

         // Is left click within resize grabber?
         if( r.Contains( pos ) && !event.Leaving() )
         {
            SetCursor( wxCURSOR_SIZENWSE );
            if( event.LeftDown() )
            {
               CaptureMouse();
            }
         }
         else
         {
            SetCursor( wxCURSOR_ARROW );
         }
      }
   }

   void OnCaptureLost( wxMouseCaptureLostEvent & WXUNUSED(event) )
   {
      if( HasCapture() )
      {
         ReleaseMouse();
      }
   }

   //
   // Do not allow the window to close through keyboard accelerators
   // (like ALT+F4 on Windows)
   //
   void OnClose( wxCloseEvent & event )
   {
      event.Veto();
   }

 private:

   wxWindow *mParent;
   ToolManager *mManager;
   ToolBar *mBar;
   wxSize mMinSize;

 public:

   DECLARE_CLASS( ToolFrame );
   DECLARE_EVENT_TABLE();
};

IMPLEMENT_CLASS( ToolFrame, wxFrame );

BEGIN_EVENT_TABLE( ToolFrame, wxFrame )
   EVT_GRABBER( wxID_ANY, ToolFrame::OnGrabber )
   EVT_PAINT( ToolFrame::OnPaint )
   EVT_MOUSE_EVENTS( ToolFrame::OnMotion )
   EVT_MOUSE_CAPTURE_LOST( ToolFrame::OnCaptureLost )
   EVT_CLOSE( ToolFrame::OnClose )
   EVT_COMMAND( wxID_ANY, EVT_TOOLBAR_UPDATED, ToolFrame::OnToolBarUpdate )
END_EVENT_TABLE()

IMPLEMENT_CLASS( ToolManager, wxEvtHandler );

////////////////////////////////////////////////////////////
/// Methods for ToolManager
////////////////////////////////////////////////////////////

BEGIN_EVENT_TABLE( ToolManager, wxEvtHandler )
   EVT_GRABBER( wxID_ANY, ToolManager::OnGrabber )
   EVT_TIMER( wxID_ANY, ToolManager::OnTimer )
END_EVENT_TABLE()

//
// Constructor
//
ToolManager::ToolManager( AudacityProject *parent )
: wxEvtHandler()
{
   wxPoint pt[ 3 ];

#if defined(__WXMAC__)
   // Save original transition
   mTransition = wxSystemOptions::GetOptionInt( wxMAC_WINDOW_PLAIN_TRANSITION );
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
   mDown = new wxRegion( 3, &pt[0] );

   // Create the down arrow
   pt[ 0 ].x = 9;
   pt[ 0 ].y = 0;
   pt[ 1 ].x = 0;
   pt[ 1 ].y = 9;
   pt[ 2 ].x = 9;
   pt[ 2 ].y = 18;

   // Create the shaped region
   mLeft = new wxRegion( 3, &pt[0] );

   // Create the indicator frame
   mIndicator = new wxFrame( NULL,
                             wxID_ANY,
                             wxEmptyString,
                             wxDefaultPosition,
                             wxSize( 32, 32 ),
                             wxFRAME_TOOL_WINDOW |
                             wxFRAME_SHAPED |
                             wxNO_BORDER |
                             wxFRAME_NO_TASKBAR |
                             wxSTAY_ON_TOP );

   // Hook the creation event...only needed on GTK, but doesn't hurt for all
   mIndicator->Connect( wxEVT_CREATE,
                        wxWindowCreateEventHandler( ToolManager::OnIndicatorCreate ),
                        NULL,
                        this );

   // Hook the paint event...needed for all
   mIndicator->Connect( wxEVT_PAINT,
                        wxPaintEventHandler( ToolManager::OnIndicatorPaint ),
                        NULL,
                        this );

   // It's a little shy
   mIndicator->Hide();

   // Hook the parents mouse events...using the parent helps greatly
   // under GTK
   mParent->Connect( wxEVT_LEFT_UP,
                     wxMouseEventHandler( ToolManager::OnMouse ),
                     NULL,
                     this );
   mParent->Connect( wxEVT_MOTION,
                     wxMouseEventHandler( ToolManager::OnMouse ),
                     NULL,
                     this );
   mParent->Connect( wxEVT_MOUSE_CAPTURE_LOST,
                     wxMouseCaptureLostEventHandler( ToolManager::OnCaptureLost ),
                     NULL,
                     this );

   // Create the top and bottom docks
   mTopDock = new ToolDock( this, mParent, TopDockID );
   mBotDock = new ToolDock( this, mParent, BotDockID );

   // Create all of the toolbars
   mBars[ ToolsBarID ]         = new ToolsToolBar();
   mBars[ TransportBarID ]     = new ControlToolBar();
   mBars[ MeterBarID ]         = new MeterToolBar();
   mBars[ EditBarID ]          = new EditToolBar();
   mBars[ MixerBarID ]         = new MixerToolBar();
   mBars[ TranscriptionBarID ] = new TranscriptionToolBar();
   mBars[ SelectionBarID ]     = new SelectionBar();
   mBars[ DeviceBarID ]        = new DeviceToolBar();

   // We own the timer
   mTimer.SetOwner( this );

   // Process the toolbar config settings
   ReadConfig();
}

//
// Destructer
//
ToolManager::~ToolManager()
{
   // Save the toolbar states
   WriteConfig();

   // Remove handlers from parent
   mParent->Disconnect( wxEVT_LEFT_UP,
                        wxMouseEventHandler( ToolManager::OnMouse ),
                        NULL,
                        this );
   mParent->Disconnect( wxEVT_MOTION,
                        wxMouseEventHandler( ToolManager::OnMouse ),
                        NULL,
                        this );
   mParent->Disconnect( wxEVT_MOUSE_CAPTURE_LOST,
                        wxMouseCaptureLostEventHandler( ToolManager::OnCaptureLost ),
                        NULL,
                        this );

   // Remove our event handlers
   mIndicator->Disconnect( wxEVT_CREATE,
                           wxWindowCreateEventHandler( ToolManager::OnIndicatorCreate ),
                           NULL,
                           this );
   mIndicator->Disconnect( wxEVT_PAINT,
                           wxPaintEventHandler( ToolManager::OnIndicatorPaint ),
                           NULL,
                           this );

   // Must destroy the window since it doesn't have a parent
   mIndicator->Destroy();

   // Delete the indicator regions
   delete mLeft;
   delete mDown;
}

void ToolManager::Reset()
{
   int ndx;

   // The mInputMeter and mOutputMeter may be in use if audio is playing
   // when this happens.
   gAudioIO->SetMeters( NULL, NULL );

   // Disconnect all docked bars
   for( ndx = 0; ndx < ToolBarCount; ndx++ )
   {
      wxWindow *parent;
      ToolDock *dock;
      ToolBar *bar = mBars[ ndx ];

      // Disconnect the bar
      if( bar->IsDocked() )
      {
         bar->GetDock()->Undock( bar );
         parent = NULL;
      }
      else
      {
         parent = bar->GetParent();
      }

      if( ndx == SelectionBarID )
      {
         dock = mBotDock;

         wxCommandEvent e;
         bar->GetEventHandler()->ProcessEvent(e);
      }
      else
      {
         dock = mTopDock;
         bar->ReCreateButtons();
      }

      bar->EnableDisableButtons();
#if 0
      if( bar->IsResizable() )
      {
         bar->SetSize(bar->GetBestFittingSize());
      }
#endif
      dock->Dock( bar );

      Expose( ndx, true );

      if( parent )
      {
         parent->Destroy();
      }
   }
   // TODO:??
   // If audio was playing, we stopped the VU meters,
   // It would be nice to show them again, but hardly essential as
   // they will show up again on the next play.
   // SetVUMeters(AudacityProject *p);
   LayoutToolBars();
   Updated();
}

//
// Read the toolbar states
//
void ToolManager::ReadConfig()
{
   wxString oldpath = gPrefs->GetPath();
   wxArrayInt unordered[ DockCount ];
   int order[ DockCount ][ ToolBarCount ];
   bool show[ ToolBarCount ];
   int width[ ToolBarCount ];
   int height[ ToolBarCount ];
   int x, y;
   int dock, ord, ndx;

#if defined(__WXMAC__)
   // Disable window animation
   wxSystemOptions::SetOption( wxMAC_WINDOW_PLAIN_TRANSITION, 1 );
#endif

   // Invalidate all order entries
   for( dock = 0; dock < DockCount; dock++ )
   {
      for( ord = 0; ord < ToolBarCount; ord++ )
      {
         order[ dock ][ ord ] = NoBarID;
      }
   }

   // Change to the bar root
   gPrefs->SetPath( wxT("/GUI/ToolBars") );

   // Load and apply settings for each bar
   for( ndx = 0; ndx < ToolBarCount; ndx++ )
   {
      ToolBar *bar = mBars[ ndx ];

      // Change to the bar subkey
      gPrefs->SetPath( bar->GetSection() );

      // Read in all the settings
      gPrefs->Read( wxT("Dock"), &dock, ndx == SelectionBarID ? BotDockID : TopDockID );
      gPrefs->Read( wxT("Order"), &ord, NoBarID );
      gPrefs->Read( wxT("Show"), &show[ ndx ], true );

      gPrefs->Read( wxT("X"), &x, -1 );
      gPrefs->Read( wxT("Y"), &y, -1 );
      gPrefs->Read( wxT("W"), &width[ ndx ], -1 );
      gPrefs->Read( wxT("H"), &height[ ndx ], -1 );

      // Docked or floating?
      if( dock )
      {
         // Default to top dock if the ID isn't valid
         if( dock < NoDockID || dock > DockCount ) {
            dock = TopDockID;
         }

         // Create the bar with the correct parent
         if( dock == TopDockID )
         {
            bar->Create( mTopDock );
         }
         else
         {
            bar->Create( mBotDock );
         }

#ifdef EXPERIMENTAL_SYNC_LOCK
         // Set the width
         if( width[ ndx ] >= bar->GetSize().x )
         {
            wxSize sz( width[ ndx ], bar->GetSize().y );
            bar->SetSize( sz );
            bar->Layout();
         }
#else
         // note that this section is here because if you had been using sync-lock and now you aren't,
         // the space for the extra button is stored in audacity.cfg, and so you get an extra space
         // in the EditToolbar.
         // It is needed so that the meterToolbar size gets preserved.
         // Longer-term we should find a better fix for this.
         wxString thisBar = bar->GetSection();
         if( thisBar != wxT("Edit"))
         {
            // Set the width
            if( width[ ndx ] >= bar->GetSize().x )
            {
               wxSize sz( width[ ndx ], bar->GetSize().y );
               bar->SetSize( sz );
               bar->Layout();
            }
         }
#endif
         // Is order within range and unoccupied?
         if( ( ord >= 0 ) &&
             ( ord < ToolBarCount ) &&
             ( order[ dock - 1 ][ ord ] == NoBarID ) )
         {
            // Insert at ordered location
            order[ dock - 1 ][ ord ] = ndx;
         }
         else
         {
            // These must go at the end
            unordered[ dock - 1 ].Add( ndx );
         }
      }
      else
      {
         // Create the bar (with the top dock being temporary parent)
         bar->Create( mTopDock );

         // Construct a new floater
         ToolFrame *f = new ToolFrame( mParent, this, bar, wxPoint( x, y ) );

         // Set the width and height
         if( width[ ndx ] != -1 && height[ ndx ] != -1 )
         {
            wxSize sz( width[ ndx ], height[ ndx ] );
            f->SetSizeHints( sz );
            f->SetSize( sz );
            f->Layout();
         }

         // Show or hide it
         bar->Expose( show[ ndx ] );

         // Inform toolbar of change
         bar->SetDocked( NULL, false );
      }

      // Change back to the bar root
      //gPrefs->SetPath( wxT("..") );  <-- Causes a warning...
      // May or may not have gone into a subdirectory,
      // so use an absolute path.
      gPrefs->SetPath( wxT("/GUI/ToolBars") );
   }

   // Add all toolbars to their target dock
   for( dock = 0; dock < DockCount; dock++ )
   {
      ToolDock *d = ( dock + 1 == TopDockID ? mTopDock : mBotDock );

      // Add all ordered toolbars
      for( ord = 0; ord < ToolBarCount; ord++ )
      {
         ndx = order[ dock ][ ord ];

         // Bypass empty slots
         if( ndx != NoBarID )
         {
            ToolBar *t = mBars[ ndx ];

            // Dock it
            d->Dock( t );

            // Hide the bar
            if( !show[ t->GetId() ] )
            {
               d->ShowHide( t->GetId() );
            }
         }
      }

      // Add all unordered toolbars
      for( ord = 0; ord < (int) unordered[ dock ].GetCount(); ord++ )
      {
         ToolBar *t = mBars[ unordered[ dock ][ ord ] ];

         // Dock it
         d->Dock( t );

         // Hide the bar
         if( !show[ t->GetId() ] )
         {
            d->ShowHide( t->GetId() );
         }
      }
   }

   // Restore original config path
   gPrefs->SetPath( oldpath );

#if defined(__WXMAC__)
   // Reinstate original transition
   wxSystemOptions::SetOption( wxMAC_WINDOW_PLAIN_TRANSITION, mTransition );
#endif
}

//
// Save the toolbar states
//
void ToolManager::WriteConfig()
{
   if( !gPrefs )
   {
      return;
   }

   wxString oldpath = gPrefs->GetPath();
   int ndx;

   // Change to the bar root
   gPrefs->SetPath( wxT("/GUI/ToolBars") );

   // Save state of each bar
   for( ndx = 0; ndx < ToolBarCount; ndx++ )
   {
      ToolBar *bar = mBars[ ndx ];

      // Change to the bar subkey
      gPrefs->SetPath( bar->GetSection() );

      // Search both docks for toolbar order
      int to = mTopDock->GetOrder( bar );
      int bo = mBotDock->GetOrder( bar );

      // Save
      gPrefs->Write( wxT("Dock"), to ? TopDockID : bo ? BotDockID : NoDockID );
      gPrefs->Write( wxT("Order"), to + bo );
      gPrefs->Write( wxT("Show"), IsVisible( ndx ) );

      wxPoint pos( -1, -1 );
      wxSize sz = bar->GetSize();
      if( !bar->IsDocked() )
      {
         pos = bar->GetParent()->GetPosition();
         sz = bar->GetParent()->GetSize();
      }
      gPrefs->Write( wxT("X"), pos.x );
      gPrefs->Write( wxT("Y"), pos.y );
      gPrefs->Write( wxT("W"), sz.x );
      gPrefs->Write( wxT("H"), sz.y );

      // Kill the bar
      bar->Destroy();

      // Change back to the bar root
      gPrefs->SetPath( wxT("..") );
   }

   // Restore original config path
   gPrefs->SetPath( oldpath );
   gPrefs->Flush();
}

//
// Return a pointer to the specified toolbar
//
ToolBar *ToolManager::GetToolBar( int type ) const
{
   return mBars[ type ];
}

//
// Return a pointer to the top dock
//
ToolDock *ToolManager::GetTopDock()
{
   return mTopDock;
}

//
// Return a pointer to the bottom dock
//
ToolDock *ToolManager::GetBotDock()
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
   wxCommandEvent e( EVT_TOOLBAR_UPDATED );
   mParent->GetEventHandler()->AddPendingEvent( e );
}

//
// Return docked state of specified toolbar
//
bool ToolManager::IsDocked( int type )
{
   return mBars[ type ]->IsDocked();
}

//
// Returns the visibility of the specified toolbar
//
bool ToolManager::IsVisible( int type )
{
   ToolBar *t = mBars[ type ];

   return t->IsVisible();

#if 0
   // If toolbar is floating
   if( !t->IsDocked() )
   {
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
void ToolManager::ShowHide( int type )
{
   ToolBar *t = mBars[ type ];

   // Handle docked and floaters differently
   if( t->IsDocked() )
   {
      t->GetDock()->ShowHide( type );
   }
   else
   {
      t->Expose( !t->IsVisible() );
   }
}

//
// Set the visible/hidden state of a toolbar
//
void ToolManager::Expose( int type, bool show )
{
   ToolBar *t = mBars[ type ];

   // Handle docked and floaters differently
   if( t->IsDocked() )
   {
      t->GetDock()->Expose( type, show );
   }
   else
   {
      t->Expose( show );
   }
}

//
// Ask both docks to (re)layout their bars
//
void ToolManager::LayoutToolBars()
{
   // Update the layout
   mTopDock->LayoutToolBars();
   mBotDock->LayoutToolBars();
}

//
// Tell the toolbars that preferences have been updated
//
void ToolManager::UpdatePrefs()
{
   for( int ndx = 0; ndx < ToolBarCount; ndx++ )
   {
      ToolBar *bar = mBars[ ndx ];
      if( bar )
      {
         bar->UpdatePrefs();
      }
   }
}

//
// Handle toolbar dragging
//
void ToolManager::OnMouse( wxMouseEvent & event )
{
   // Go ahead and set the event to propagate
   event.Skip();

   // Can't do anything if we're not dragging.  This also prevents
   // us from intercepting events that don't belong to us from the
   // parent since we're Connect()ed to a couple.
   if( !mDragWindow )
   {
      return;
   }

#if defined(__WXMAC__)
   // Disable window animation
   wxSystemOptions::SetOption( wxMAC_WINDOW_PLAIN_TRANSITION, 1 );
#endif

   // Retrieve the event position
   wxPoint pos =
      ( (wxWindow *)event.GetEventObject() )->ClientToScreen( event.GetPosition() );

   // Button was released...finish the drag
   if( !event.LeftIsDown() )
   {
      // Release capture
      if( mParent->HasCapture() )
      {
         mParent->ReleaseMouse();
      }

      // Hide the indicator
      mIndicator->Hide();

      // Transition the bar to a dock
      if( mDragDock && !event.ShiftDown() )
      {
         // Trip over...everyone ashore that's going ashore...
         mDragDock->Dock( mDragBar, mDragBefore );

         // Done with the floater
         mDragWindow->Destroy();
         mDragBar->Refresh(false);
      }
      else
      {
         // Calling SetDocked() to force the grabber button to popup
         mDragBar->SetDocked( NULL, false );
      }

      // Done dragging
      mDragWindow = NULL;
      mDragDock = NULL;
      mDragBar = NULL;
      mLastPos.x = mBarPos.x = -1;
      mLastPos.y = mBarPos.y = -1;
      mTimer.Stop();
   }
   else if( event.Dragging() && pos != mLastPos )
   {
      // Make toolbar follow the mouse
      mDragWindow->Move( pos - mDragOffset );

      // Remember to prevent excessive movement
      mLastPos = pos;

      // Calc the top dock hittest rectangle
      wxRect tr = mTopDock->GetRect();
      tr.SetBottom( tr.GetBottom() + 10 );
      tr.SetPosition( mTopDock->GetParent()->ClientToScreen( tr.GetPosition() ) );

      // Calc the bottom dock hittest rectangle
      wxRect br = mBotDock->GetRect();
      br.SetTop( br.GetTop() - 10 );
      br.SetBottom( br.GetBottom() + 20 );
      br.SetPosition( mBotDock->GetParent()->ClientToScreen( br.GetPosition() ) );

      // Is mouse pointer within either dock?
      ToolDock *dock = NULL;
      if( tr.Contains( pos ) )
      {
         dock = mTopDock;
      }
      else if( br.Contains( pos ) )
      {
         dock = mBotDock;
      }

      // Looks like we have a winner...
      if( dock )
      {
         wxPoint p;
         wxRect r;

         // Calculate where the bar would be placed
         mDragBefore = dock->PositionBar( mDragBar, pos, r );

         // If different than the last time, the indicator must be moved
         if( r != mBarPos )
         {
            wxRect dr = dock->GetRect();

            // Hide the indicator before changing the shape
            mIndicator->Hide();

            // Decide which direction the arrow should point
            if( r.GetBottom() >= dr.GetHeight() )
            {
               p.x = dr.GetLeft() + ( dr.GetWidth() / 2 );
               p.y = dr.GetBottom() - mDown->GetBox().GetHeight();
               mCurrent = mDown;
            }
            else
            {
               p.x = dr.GetLeft() + r.GetLeft();
               p.y = dr.GetTop() + r.GetTop() +
                     ( ( r.GetHeight() - mLeft->GetBox().GetHeight() ) / 2 );
               mCurrent = mLeft;
            }

            // Change the shape while hidden and then show it if okay
            mIndicator->SetShape( *mCurrent );
            if( !event.ShiftDown() )
            {
               mIndicator->Show();
               mIndicator->Update();
            }

            // Move it into position
            // LL:  Do this after the Show() since KDE doesn't move the window
            //      if it's not shown.  (Do it outside if the previous IF as well)
            mIndicator->Move( dock->GetParent()->ClientToScreen( p ) );

            // Remember for next go round
            mBarPos = r;
         }
      }
      else
      {
         // Hide the indicator if it's still shown
         if( mBarPos.x != -1 )
         {
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
   wxSystemOptions::SetOption( wxMAC_WINDOW_PLAIN_TRANSITION, mTransition );
#endif
}

//
// Deal with new capture lost event
//
void ToolManager::OnCaptureLost( wxMouseCaptureLostEvent & event )
{
   // Can't do anything if we're not dragging.  This also prevents
   // us from intercepting events that don't belong to us from the
   // parent since we're Connect()ed to a couple.
   if( !mDragWindow )
   {
      event.Skip();
      return;
   }

   // Simulate button up
   wxMouseEvent e(wxEVT_LEFT_UP);
   e.SetEventObject(mParent);
   OnMouse(e);
}

//
// Watch for shift key changes
//
void ToolManager::OnTimer( wxTimerEvent & event )
{
   // Go ahead and set the event to propagate
   event.Skip();

   // Can't do anything if we're not dragging.  This also prevents
   // us from intercepting events that don't belong to us from the
   // parent since we're Connect()ed to a couple.
   if( !mDragWindow )
   {
      return;
   }

   bool state = wxGetKeyState( WXK_SHIFT );
   if( mLastState != state )
   {
      mLastState = state;

#if defined(__WXMAC__)
      // Disable window animation
      wxSystemOptions::SetOption( wxMAC_WINDOW_PLAIN_TRANSITION, 1 );
#endif

      mIndicator->Show( !state );

#if defined(__WXMAC__)
      // Disable window animation
      wxSystemOptions::SetOption( wxMAC_WINDOW_PLAIN_TRANSITION, mTransition );
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
void ToolManager::OnIndicatorPaint( wxPaintEvent & event )
{
   wxWindow *w = (wxWindow *)event.GetEventObject();
   wxPaintDC dc( w );
   dc.SetBackground( *wxBLUE_BRUSH );
   dc.Clear();
}

//
// Handle Indicator creation event
//
// Without this, the initial Indicator window will be a solid blue square
// until the next time it changes.
//
void ToolManager::OnIndicatorCreate( wxWindowCreateEvent & event )
{
#if defined(__WXGTK__)
   mIndicator->SetShape( *mCurrent );
#endif
   event.Skip();
}

//
// Transition a toolbar from float to dragging
//
void ToolManager::OnGrabber( GrabberEvent & event )
{
   // No need to propagate any further
   event.Skip( false );

   // Remember which bar we're dragging
   mDragBar = mBars[ event.GetId() ];

   // Calculate the drag offset
   wxPoint mp = event.GetPosition();
   mDragOffset = mp -
                 mDragBar->GetParent()->ClientToScreen( mDragBar->GetPosition() ) +
                 wxPoint( 1, 1 );

   // Must set the bar afloat if it's currently docked
   if( mDragBar->IsDocked() )
   {
#if defined(__WXMAC__)
      // Disable window animation
      wxSystemOptions::SetOption( wxMAC_WINDOW_PLAIN_TRANSITION, 1 );
#endif

      // Adjust the starting position
      mp -= mDragOffset;

      // Inform toolbar of change
      mDragBar->SetDocked( NULL, true );

      // Construct a new floater
      mDragWindow = new ToolFrame( mParent, this, mDragBar, mp );

      // Make sure the ferry is visible
      mDragWindow->Show();

      // Notify parent of change
      Updated();

#if defined(__WXMAC__)
      // Reinstate original transition
      wxSystemOptions::SetOption( wxMAC_WINDOW_PLAIN_TRANSITION, mTransition );
#endif
   }
   else
   {
      mDragWindow = (ToolFrame *) mDragBar->GetParent();
   }

   // We want all mouse events from this point on
   mParent->CaptureMouse();

   // Start monitoring shift key changes
   mLastState = wxGetKeyState( WXK_SHIFT );
   mTimer.Start( 100 );
}
