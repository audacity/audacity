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

#include "ToolManager.h"
#include "ControlToolBar.h"
#include "DeviceToolBar.h"
#include "EditToolBar.h"
#include "MeterToolBar.h"
#include "MixerToolBar.h"
#include "ScrubbingToolBar.h"
#include "SelectionBar.h"
#include "SpectralSelectionBar.h"
#include "ToolsToolBar.h"
#include "TranscriptionToolBar.h"

#include "../AColor.h"
#include "../AllThemeResources.h"
#include "../AudioIO.h"
#include "../ImageManipulation.h"
#include "../Prefs.h"
#include "../Project.h"
#include "../Theme.h"
#include "../widgets/AButton.h"
#include "../widgets/Grabber.h"

#include "../Experimental.h"

////////////////////////////////////////////////////////////
/// Methods for ToolFrame
////////////////////////////////////////////////////////////
#define sizerW 11

//
// Constructor
//
ToolFrame::ToolFrame
   ( wxWindow *parent, ToolManager *manager, ToolBar *bar, wxPoint pos )
   : wxFrame( parent,
          bar->GetId(),
          wxEmptyString,
          pos,
          wxDefaultSize,
          wxNO_BORDER |
          wxFRAME_NO_TASKBAR |
#if !defined(__WXMAC__) // bug1358
          wxFRAME_TOOL_WINDOW |
#endif
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
   bar->Reparent(this);
   SetMinSize(bar->GetDockedSize());

   {
      // We use a sizer to maintain proper spacing
      auto s = std::make_unique<wxBoxSizer>(wxHORIZONTAL);

      // Add the bar to the sizer
      s->Add(bar, 1, wxEXPAND | wxALL, border);

      // Add space for the resize grabber
      if (bar->IsResizable())
      {
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
   bar->SetDocked( NULL, true );

   // Make sure resizable floaters don't get any smaller than initial size
   if( bar->IsResizable() )
   {
      // Calc the minimum size of the frame
      mMinSize = bar->GetMinSize() + ( GetSize() - bar->GetSize() );
   }
}

ToolFrame::~ToolFrame()
{
   if(HasCapture())
      ReleaseMouse();
}

void ToolFrame::OnGrabber( GrabberEvent & event )
{
   // Pass it on to the manager since it isn't in the handling hierarchy
   mManager->ProcessEvent( event );
}

void ToolFrame::OnToolBarUpdate( wxCommandEvent & event )
{
   // Resize floater window to exactly contain toolbar
   if (mBar)
      mBar->GetParent()->SetClientSize( mBar->GetMinSize() );

   // Allow it to propagate to our parent
   event.Skip();
}

void ToolFrame::OnPaint( wxPaintEvent & WXUNUSED(event) )
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

   if( mBar && mBar->IsResizable() )
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

void ToolFrame::OnMotion( wxMouseEvent & event )
{
   // Don't do anything if we're docked or not resizeable
   if( !mBar || mBar->IsDocked() || !mBar->IsResizable() )
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

      Resize( rect.GetSize() );
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
         mOrigSize = GetSize();

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

void ToolFrame::OnCaptureLost( wxMouseCaptureLostEvent & WXUNUSED(event) )
{
   if( HasCapture() )
   {
      ReleaseMouse();
   }
}

void ToolFrame::OnClose( wxCloseEvent & event )
{
   event.Veto();
}

void ToolFrame::OnKeyDown( wxKeyEvent &event )
{
   event.Skip();
   if( HasCapture() && event.GetKeyCode() == WXK_ESCAPE ) {
      Resize( mOrigSize );
      ReleaseMouse();
   }
}

void ToolFrame::Resize( const wxSize &size )
{
   SetMinSize( size );
   SetSize( size );
   Layout();
   Refresh( false );
}

IMPLEMENT_CLASS( ToolFrame, wxFrame );

BEGIN_EVENT_TABLE( ToolFrame, wxFrame )
   EVT_GRABBER( wxID_ANY, ToolFrame::OnGrabber )
   EVT_PAINT( ToolFrame::OnPaint )
   EVT_MOUSE_EVENTS( ToolFrame::OnMotion )
   EVT_MOUSE_CAPTURE_LOST( ToolFrame::OnCaptureLost )
   EVT_CLOSE( ToolFrame::OnClose )
   EVT_COMMAND( wxID_ANY, EVT_TOOLBAR_UPDATED, ToolFrame::OnToolBarUpdate )
   EVT_KEY_DOWN( ToolFrame::OnKeyDown )
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
ToolManager::ToolManager( AudacityProject *parent, wxWindow *topDockParent )
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
   mDown = std::make_unique<wxRegion>( 3, &pt[0] );

   // Create the down arrow
   pt[ 0 ].x = 9;
   pt[ 0 ].y = 0;
   pt[ 1 ].x = 0;
   pt[ 1 ].y = 9;
   pt[ 2 ].x = 9;
   pt[ 2 ].y = 18;

   // Create the shaped region
   mLeft = std::make_unique<wxRegion>( 3, &pt[0] );

   // Create the indicator frame
   // parent is null but FramePtr ensures destruction
   mIndicator = FramePtr{ safenew wxFrame( NULL,
                             wxID_ANY,
                             wxEmptyString,
                             wxDefaultPosition,
                             wxSize( 32, 32 ),
                             wxFRAME_TOOL_WINDOW |
                             wxFRAME_SHAPED |
                             wxNO_BORDER |
                             wxFRAME_NO_TASKBAR |
                             wxSTAY_ON_TOP )
   };

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
   mTopDock = safenew ToolDock( this, topDockParent, TopDockID );
   mBotDock = safenew ToolDock( this, mParent, BotDockID );

   // Create all of the toolbars
   // All have the project as parent window
   wxASSERT(parent);
   mBars[ ToolsBarID ]         =  ToolBar::Holder{ safenew ToolsToolBar() };
   mBars[ TransportBarID ]     =  ToolBar::Holder{ safenew ControlToolBar() };
   mBars[ RecordMeterBarID ]   =  ToolBar::Holder{ safenew MeterToolBar( parent, RecordMeterBarID ) };
   mBars[ PlayMeterBarID ]     =  ToolBar::Holder{ safenew MeterToolBar( parent, PlayMeterBarID ) };
   mBars[ MeterBarID ]         =  ToolBar::Holder{ safenew MeterToolBar( parent, MeterBarID ) };
   mBars[ EditBarID ]          =  ToolBar::Holder{ safenew EditToolBar() };
   mBars[ MixerBarID ]         =  ToolBar::Holder{ safenew MixerToolBar() };
   mBars[ TranscriptionBarID ] =  ToolBar::Holder{ safenew TranscriptionToolBar() };
   mBars[ SelectionBarID ]     =  ToolBar::Holder{ safenew SelectionBar() };
   mBars[ DeviceBarID ]        =  ToolBar::Holder{ safenew DeviceToolBar() };
#ifdef EXPERIMENTAL_SPECTRAL_EDITING
   mBars[SpectralSelectionBarID] =  ToolBar::Holder{ safenew SpectralSelectionBar() };
#endif
   mBars[ ScrubbingBarID ]     =  ToolBar::Holder{ safenew ScrubbingToolBar() };

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
}

// This table describes the default configuration of the toolbars as
// a "tree" and must be kept in pre-order traversal.

// In fact this tree is more of a broom -- nothing properly branches except
// at the root.

// "Root" corresponds to left edge of the main window, and successive siblings
// go from top to bottom.  But in practice layout may wrap this abstract
// configuration if the window size is narrow.

static struct DefaultConfigEntry {
   int barID;
   int rightOf; // parent
   int below;   // preceding sibling
} DefaultConfigTable [] = {
   // Top dock row, may wrap
   { TransportBarID,         NoBarID,                NoBarID                },
   { ToolsBarID,             TransportBarID,         NoBarID                },
   { RecordMeterBarID,       ToolsBarID,             NoBarID                },
   { PlayMeterBarID,         RecordMeterBarID,       NoBarID                },
   { MixerBarID,             PlayMeterBarID,         NoBarID                },
   { EditBarID,              MixerBarID,             NoBarID                },
   { TranscriptionBarID,     EditBarID,              NoBarID                },

   // start another top dock row
   { ScrubbingBarID,         NoBarID,                TransportBarID         },
   { DeviceBarID,            ScrubbingBarID,         NoBarID         },

   // Hidden by default in top dock
   { MeterBarID,             NoBarID,                NoBarID                },

   // Bottom dock
   { SelectionBarID,         NoBarID,                NoBarID                },

   // Hidden by default in bottom dock
   { SpectralSelectionBarID, NoBarID,                NoBarID                },
};

void ToolManager::Reset()
{
   // Disconnect all docked bars
   for ( const auto &entry : DefaultConfigTable )
   {
      int ndx = entry.barID;
      ToolBar *bar = mBars[ ndx ].get();

      ToolBarConfiguration::Position position {
         (entry.rightOf == NoBarID) ? nullptr : mBars[ entry.rightOf ].get(),
         (entry.below == NoBarID) ? nullptr : mBars[ entry.below ].get()
      };

      wxWindow *floater;
      ToolDock *dock;
      bool expose = true;

      // Disconnect the bar
      if( bar->IsDocked() )
      {
         bar->GetDock()->Undock( bar );
         floater = NULL;
      }
      else
      {
         floater = bar->GetParent();
      }

      if (ndx == SelectionBarID 
#ifdef EXPERIMENTAL_SPECTRAL_EDITING
         || ndx == SpectralSelectionBarID
#endif
         )
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

      if( ndx == MeterBarID
#ifdef EXPERIMENTAL_SPECTRAL_EDITING
         || ndx == SpectralSelectionBarID
#endif
         || ndx == ScrubbingBarID
         )
      {
         expose = false;
      }

      if( dock != NULL )
      {
         // when we dock, we reparent, so bar is no longer a child of floater.
         dock->Dock( bar, false, position );
         Expose( ndx, expose );
         //OK (and good) to DELETE floater, as bar is no longer in it.
         if( floater )
            floater->Destroy();
      }
      else
      {
         // The (tool)bar has a dragger window round it, the floater.
         // in turn floater will have mParent (the entire App) as its
         // parent.

         // Maybe construct a NEW floater
         // this happens if we have just been bounced out of a dock.
         if( floater == NULL ) {
            wxASSERT(mParent);
            floater = safenew ToolFrame( mParent, this, bar, wxPoint(-1,-1) );
            bar->Reparent( floater );
         }

         // This bar is undocked and invisible.
         // We are doing a reset toolbars, so even the invisible undocked bars should
         // be moved somewhere sensible. Put bar near center of window.
         // If there were multiple hidden toobars the ndx * 10 adjustment means 
         // they won't overlap too much.
         floater->CentreOnParent( );
         floater->Move( floater->GetPosition() + wxSize( ndx * 10 - 200, ndx * 10 ));
         bar->SetDocked( NULL, false );
         Expose( ndx, false );
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

void ToolManager::RegenerateTooltips()
{
   for (const auto &bar : mBars) {
      if (bar)
         bar->RegenerateTooltips();
   }
}

//
// Read the toolbar states
//
void ToolManager::ReadConfig()
{
   wxString oldpath = gPrefs->GetPath();
   wxArrayInt unordered[ DockCount ];
   bool show[ ToolBarCount ];
   int width[ ToolBarCount ];
   int height[ ToolBarCount ];
   int x, y;
   int dock, ndx;
   bool someFound { false };

#if defined(__WXMAC__)
   // Disable window animation
   wxSystemOptions::SetOption( wxMAC_WINDOW_PLAIN_TRANSITION, 1 );
#endif

   // Change to the bar root
   gPrefs->SetPath( wxT("/GUI/ToolBars") );

   ToolBarConfiguration::Legacy topLegacy, botLegacy;

   // Load and apply settings for each bar
   for( ndx = 0; ndx < ToolBarCount; ndx++ )
   {
      ToolBar *bar = mBars[ ndx ].get();
      //wxPoint Center = mParent->GetPosition() + (mParent->GetSize() * 0.33);
      //wxPoint Center( 
      //   wxSystemSettings::GetMetric( wxSYS_SCREEN_X ) /2 ,
      //   wxSystemSettings::GetMetric( wxSYS_SCREEN_Y ) /2 );

      // Change to the bar subkey
      gPrefs->SetPath( bar->GetSection() );

      bool bShownByDefault = true;
      int defaultDock = TopDockID;
      
      if( ndx == SelectionBarID )
         defaultDock = BotDockID;
      if( ndx == MeterBarID )
         bShownByDefault = false;
      if( ndx == ScrubbingBarID )
         bShownByDefault = false;

#ifdef EXPERIMENTAL_SPECTRAL_EDITING
      if( ndx == SpectralSelectionBarID ){
         defaultDock = BotDockID;
         bShownByDefault = false; // Only show if asked for.  
      }
#endif

      // Read in all the settings
      gPrefs->Read( wxT("Dock"), &dock, -1);
      const bool found = (dock != -1);
      if (found)
         someFound = true;
      if (!found)
         dock = defaultDock;
      
      ToolDock *d;
      ToolBarConfiguration::Legacy *pLegacy;
      switch(dock)
      {
         case TopDockID: d = mTopDock; pLegacy = &topLegacy; break;
         case BotDockID: d = mBotDock; pLegacy = &botLegacy;  break;
         default:        d = nullptr; pLegacy = nullptr; break;
      }

      bool ordered = ToolBarConfiguration::Read
         (d ? &d->GetConfiguration() : nullptr,
          pLegacy,
          bar, show[ ndx ], bShownByDefault)
      && found;

      gPrefs->Read( wxT("X"), &x, -1 );
      gPrefs->Read( wxT("Y"), &y, -1 );
      gPrefs->Read( wxT("W"), &width[ ndx ], -1 );
      gPrefs->Read( wxT("H"), &height[ ndx ], -1 );

      bar->SetVisible( show[ ndx ] );

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

         // Set the width and height
         if( width[ ndx ] != -1 && height[ ndx ] != -1 )
         {
            wxSize sz( width[ ndx ], height[ ndx ] );
            bar->SetSize( sz );
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

         if (!ordered)
         {
            // These must go at the end
            unordered[ dock - 1 ].Add( ndx );
         }
      }
      else
      {
         // Create the bar (with the top dock being temporary parent)
         bar->Create( mTopDock );

         // Construct a NEW floater
         wxASSERT(mParent);
         ToolFrame *f = safenew ToolFrame( mParent, this, bar, wxPoint( x, y ) );

         // Set the width and height
         if( width[ ndx ] != -1 && height[ ndx ] != -1 )
         {
            wxSize sz( width[ ndx ], height[ ndx ] );
            f->SetSizeHints( sz );
            f->SetSize( sz );
            f->Layout();
            if( (x!=-1) && (y!=-1) )
               bar->SetPositioned();
         }

         // Required on Linux Xfce
         wxSize msz(width[ndx],height[ndx]-1);
         bar->GetParent()->SetMinSize(msz);

         // Inform toolbar of change
         bar->SetDocked( NULL, false );

         // Show or hide it
         Expose( ndx, show[ ndx ] );
      }

      // Change back to the bar root
      //gPrefs->SetPath( wxT("..") );  <-- Causes a warning...
      // May or may not have gone into a subdirectory,
      // so use an absolute path.
      gPrefs->SetPath( wxT("/GUI/ToolBars") );
   }

   mTopDock->GetConfiguration().PostRead(topLegacy);
   mBotDock->GetConfiguration().PostRead(botLegacy);

   // Add all toolbars to their target dock
   for( dock = 0; dock < DockCount; dock++ )
   {
      ToolDock *d = ( dock + 1 == TopDockID ? mTopDock : mBotDock );

      d->LoadConfig();

      // Add all unordered toolbars
      bool deviceWasPositioned = false;
      for( int ord = 0; ord < (int) unordered[ dock ].GetCount(); ord++ )
      {
         ToolBar *t = mBars[ unordered[ dock ][ ord ] ].get();

         if (deviceWasPositioned &&
             t->GetType() == DeviceBarID)
            continue;

         if (someFound &&
             t->GetType() == ScrubbingBarID) {
            // Special case code to put the NEW scrubbing toolbar where we
            // want it, when audacity.cfg is present from an older version
            ToolBar *lastRoot {};

            // Change from the ideal configuration to the constrained one,
            // just as when dragging and dropping
            ToolBarConfiguration dummy;
            mTopDock->WrapConfiguration(dummy);

            // Start a NEW row with just the scrubbing toolbar
            auto &configuration = mTopDock->GetConfiguration();
            for (const auto place : configuration)
               if (place.position.rightOf == nullptr)
                  lastRoot = place.pTree->pBar;
            ToolBarConfiguration::Position position {
               nullptr, lastRoot, false
            };
            mTopDock->Dock(t, false, position);

            // Reposition the device toolbar, if it was docked above,
            // right of scrubbing
            const auto deviceToolBar = mBars[ DeviceBarID ].get();
            if (deviceToolBar->GetDock() == mTopDock) {
               deviceToolBar->GetDock()->Undock(deviceToolBar);
               position = { t, nullptr };
               mTopDock->Dock(deviceToolBar, false, position);

               // Remember not to place the device toolbar again
               deviceWasPositioned = true;
            }
            Expose( t->GetId(), show[ t->GetId() ] );
            continue;
         }

         // Dock it
         d->Dock( t, false );

         // Show or hide the bar
         Expose( t->GetId(), show[ t->GetId() ] );
      }
   }

   // Restore original config path
   gPrefs->SetPath( oldpath );

#if defined(__WXMAC__)
   // Reinstate original transition
   wxSystemOptions::SetOption( wxMAC_WINDOW_PLAIN_TRANSITION, mTransition );
#endif

   if (!someFound)
      Reset();
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
      ToolBar *bar = mBars[ ndx ].get();

      // Change to the bar subkey
      gPrefs->SetPath( bar->GetSection() );

      // Search both docks for toolbar order
      bool to = mTopDock->GetConfiguration().Contains( bar );
      bool bo = mBotDock->GetConfiguration().Contains( bar );

      // Save
      gPrefs->Write( wxT("Dock"), (int) (to ? TopDockID : bo ? BotDockID : NoDockID ));
      auto dock = to ? mTopDock : bo ? mBotDock : nullptr;
      ToolBarConfiguration::Write
         (dock ? &dock->GetConfiguration() : nullptr, bar);

      wxPoint pos( -1, -1 );
      wxSize sz = bar->GetSize();
      if( !bar->IsDocked() && bar->IsPositioned() )
      {
         pos = bar->GetParent()->GetPosition();
         sz = bar->GetParent()->GetSize();
      }
      gPrefs->Write( wxT("X"), pos.x );
      gPrefs->Write( wxT("Y"), pos.y );
      gPrefs->Write( wxT("W"), sz.x );
      gPrefs->Write( wxT("H"), sz.y );

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
   return mBars[ type ].get();
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
   ToolBar *t = mBars[ type ].get();

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
   Expose( type, !mBars[ type ]->IsVisible() );
}

//
// Set the visible/hidden state of a toolbar
//
void ToolManager::Expose( int type, bool show )
{
   ToolBar *t = mBars[ type ].get();

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
      ToolBar *bar = mBars[ ndx ].get();
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
      ( (wxWindow *)event.GetEventObject() )->ClientToScreen( event.GetPosition() ) - mDragOffset;

   // Button was released...finish the drag
   if( !event.LeftIsDown() )
   {
      // Transition the bar to a dock
      if( mDragDock && !event.ShiftDown() )
      {
         // Trip over...everyone ashore that's going ashore...
         mDragDock->Dock( mDragBar, true, mDragBefore );
         mDragWindow->ClearBar();

         // Done with the floater
         mDragWindow->Destroy();
         mDragWindow = nullptr;
         mDragBar->Refresh(false);
      }
      else
      {
         // Calling SetDocked() to force the grabber button to popup
         mDragBar->SetDocked( NULL, false );
      }

      DoneDragging();
   }
   else if( event.Dragging() && pos != mLastPos )
   {
      // Make toolbar follow the mouse
      mDragWindow->Move( pos  );

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


      // Add half the bar height.  We could use the actual bar height, but that would be confusing as a 
      // bar removed at a place might not dock back there if just let go.
      // Also add 5 pixels in horizontal direction, so that a click without a move (or a very small move) 
      // lands back where we started.
      pos +=  wxPoint( 5, 20 ); 


      // To find which dock, rather than test against pos, test against the whole dragger rect.
      // This means it is enough to overlap the dock to dock with it.
      wxRect barRect = mDragWindow->GetRect();
      ToolDock *dock = NULL;
      if( tr.Intersects( barRect ) )
         dock = mTopDock;
      else if( br.Intersects( barRect ) )
         dock = mBotDock;

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
            if( r.GetTop() >= dr.GetHeight() )
            {
               const auto &box = mDown->GetBox();
               p.x = dr.GetLeft() + ( dr.GetWidth() / 2 )
                 - (box.GetWidth() / 2);
               p.y = dr.GetBottom() - box.GetHeight();
               mCurrent = mDown.get();
            }
            else
            {
               p.x = dr.GetLeft() + r.GetLeft();
               p.y = dr.GetTop() + r.GetTop() +
                  ( ( r.GetHeight() - mLeft->GetBox().GetHeight() ) / 2 );
               mCurrent = mLeft.get();
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
// Deal with NEW capture lost event
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

   if(event.IsEscaping())
      return HandleEscapeKey();

   // Remember which bar we're dragging
   mDragBar = mBars[ event.GetId() ].get();

   // Remember state, in case of ESCape key later
   if (mDragBar->IsDocked()) {
      mPrevDock = dynamic_cast<ToolDock*>(mDragBar->GetParent());
      wxASSERT(mPrevDock);
      mPrevSlot = mPrevDock->GetConfiguration().Find(mDragBar);
      mPrevDock->WrapConfiguration(mPrevConfiguration);
   }
   else
      mPrevPosition = mDragBar->GetParent()->GetPosition();

   // Calculate the drag offset
   wxPoint mp = event.GetPosition();
   mDragOffset = mp -
                 mDragBar->GetParent()->ClientToScreen( mDragBar->GetPosition() ) +
      wxPoint( 1, 1 );

   // Must set the bar afloat if it's currently docked
   if( mPrevDock )
   {
#if defined(__WXMAC__)
      // Disable window animation
      wxSystemOptions::SetOption( wxMAC_WINDOW_PLAIN_TRANSITION, 1 );
#endif

      // Adjust the starting position
      mp -= mDragOffset;

      // Inform toolbar of change
      mDragBar->SetDocked( NULL, true );
      mDragBar->SetPositioned();

      // Construct a NEW floater
      wxASSERT(mParent);
      mDragWindow = safenew ToolFrame( mParent, this, mDragBar, mp );

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
   if( !mParent->HasCapture() )
      mParent->CaptureMouse();

   // Start monitoring shift key changes
   mLastState = wxGetKeyState( WXK_SHIFT );
   mTimer.Start( 100 );
}


void ToolManager::HandleEscapeKey()
{
   if (mDragBar) {
      if(mPrevDock) {
         // Sheriff John Stone,
         // Why don't you leave me alone?
         // Well, I feel so break up
         // I want to go home.
         mPrevDock->RestoreConfiguration(mPrevConfiguration);
         mPrevDock->Dock( mDragBar, true, mPrevSlot );

         // Done with the floater
         mDragWindow->ClearBar();
         mDragWindow->Destroy();
         mDragWindow = nullptr;
         mDragBar->Refresh(false);
      }
      else {
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
   // Done dragging
   // Release capture
   if( mParent->HasCapture() )
   {
      mParent->ReleaseMouse();
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
}
