/**********************************************************************

  Audacity: A Digital Audio Editor

  ToolBar.cpp

  Dominic Mazzoni
  Shane T. Mueller
  Leland Lucius

*******************************************************************//**

\file ToolDock.cpp

  Implements ToolDock

*//*******************************************************************//**

\class ToolDock
\brief A dynamic panel where a ToolBar can be docked.

*//**********************************************************************/

#include "../Audacity.h"

// For compilers that support precompilation, includes "wx/wx.h".
#include <wx/wxprec.h>

#ifndef WX_PRECOMP
#include <wx/defs.h>
#include <wx/event.h>
#include <wx/gdicmn.h>
#include <wx/intl.h>
#include <wx/panel.h>
#include <wx/settings.h>
#include <wx/window.h>
#endif  /*  */

#include "ToolManager.h"
#include "ToolDock.h"

#include "../AColor.h"
#include "../AllThemeResources.h"
#include "../ImageManipulation.h"
#include "../Prefs.h"
#include "../Project.h"
#include "../Theme.h"
#include "../widgets/AButton.h"
#include "../widgets/Grabber.h"

IMPLEMENT_CLASS( ToolDock, wxPanel );

////////////////////////////////////////////////////////////
/// Methods for ToolDock
////////////////////////////////////////////////////////////

//
// Custom event
//
DEFINE_EVENT_TYPE( EVT_TOOLBAR_FLOAT );

BEGIN_EVENT_TABLE( ToolDock, wxPanel )
   EVT_GRABBER( wxID_ANY, ToolDock::OnGrabber )
   EVT_ERASE_BACKGROUND( ToolDock::OnErase )
   EVT_PAINT( ToolDock::OnPaint )
   EVT_SIZE( ToolDock::OnSize )
END_EVENT_TABLE()

//
// Constructor
//
ToolDock::ToolDock( ToolManager *manager, wxWindow *parent, int dockid ):
   wxPanel( parent, dockid, wxDefaultPosition, parent->GetSize() )
{
   SetLabel( _( "ToolDock" ) );
   SetName( _( "ToolDock" ) );

   // Init
   mManager = manager;

   // Use for testing gaps
   // SetOwnBackgroundColour( wxColour( 255, 0, 0 ) );
}

//
// Destructer
//
ToolDock::~ToolDock()
{
}

//
// Returns the order of the toolbar within the dock
//
int ToolDock::GetOrder( ToolBar *bar )
{
   int order = mDockedBars.Index( bar );

   if( order == wxNOT_FOUND )
   {
      return 0;
   }

   return order + 1;
}

//
// Remove the toolbar from our control
//
void ToolDock::Undock( ToolBar *bar )
{
   if( mDockedBars.Index( bar ) != wxNOT_FOUND )
   {
      mDockedBars.Remove( bar );
   }
}

//
// Handle ToolDock events
//
void ToolDock::Dock( ToolBar *bar, int before )
{
   // Adopt the toolbar into our family
   bar->Reparent( this );
   mBars[ bar->GetId() ] = bar;

   // Reset height
   bar->SetSize( bar->GetSize().x, bar->GetMinSize().y );

   // Park the new bar in the correct berth
   if( before >= 0 && before < (int)mDockedBars.GetCount() )
   {
      mDockedBars.Insert( bar, before );
   }
   else
   {
      mDockedBars.Add( bar );
   }

   // Inform toolbar of change
   bar->SetDocked( this, false );

   // Rearrange our world
   LayoutToolBars();
   Updated();
}

//
// Layout the toolbars
//
void ToolDock::LayoutToolBars()
{
   wxRect stack[ ToolBarCount + 1 ];
   wxPoint cpos, lpos;
   ToolBar *lt = NULL;
   int ndx, stkcnt = 0;
   int width, height;

   // Get size of our parent since we haven't been sized yet
   GetParent()->GetClientSize( &width, &height );
   width -= toolbarGap;
   height -= toolbarGap;

   // Get the number of docked toolbars and take a quick exit
   // if we don't have any
   int cnt = mDockedBars.GetCount();
   if( cnt == 0 )
   {
      SetMinSize( wxSize( width, toolbarGap ) );
      return;
   }

   // Set initial stack entry to maximum size
   stack[ 0 ].SetX( toolbarGap );
   stack[ 0 ].SetY( toolbarGap );
   stack[ 0 ].SetWidth( width );
   stack[ 0 ].SetHeight( height );

   // Process all docked and visible toolbars
   for( ndx = 0; ndx < cnt; ndx++ )
   {
      // Cache toolbar pointer
      ToolBar *ct = (ToolBar *)mDockedBars[ ndx ];

      // Get and cache the toolbar sizes
      wxSize sz = ct->GetSize();
      int tw = sz.GetWidth() + toolbarGap;
      int th = sz.GetHeight() + toolbarGap;

      // Will this one fit in remaining horizontal space?
      if( ( tw > stack[ stkcnt ].GetWidth() ) ||
          ( th > stack[ stkcnt ].GetHeight() ) )
      {
         // Destack entries until one is found in which this bar
         // will fit or until we run out of stacked entries
         while( stkcnt > 0 )
         {
            stkcnt--;

            // Get out if it will fit
            if( ( tw <= stack[ stkcnt ].GetWidth() ) &&
                ( th <= stack[ stkcnt ].GetHeight() ) )
            {
               break;
            }
         }
      }

      // The current stack entry position is where the bar
      // will be placed.
      cpos = stack[ stkcnt ].GetPosition();

      // We'll be using at least a portion of this stack entry, so
      // adjust the location and size.  It is possible that these
      // will become zero if this entry and the toolbar have the
      // same height.  This is what we want as it will be destacked
      // in the next iteration.
      stack[ stkcnt ].SetY(      stack[ stkcnt ].GetY()      + th );
      stack[ stkcnt ].SetHeight( stack[ stkcnt ].GetHeight() - th );

      // Calc the next possible horizontal location.
      int x = cpos.x + tw;

      // Add a new stack entry
      stkcnt++;
      stack[ stkcnt ].SetX( x );
      stack[ stkcnt ].SetY( cpos.y );
      stack[ stkcnt ].SetWidth( width - x );
      stack[ stkcnt ].SetHeight( th );

      // Position the previous toolbar
      if( ndx > 0 )
      {
         // Keep the tab order in order
         ct->MoveAfterInTabOrder( lt );

         // Place the last toolbar
         lt->SetPosition( wxPoint( lpos.x, lpos.y ) );
      }

      // Place the final toolbar
      if( ndx == cnt - 1 )
      {
         ct->SetPosition( wxPoint( cpos.x, cpos.y ) );
      }

      // Remember for next iteration
      lt = ct;
      lpos = cpos;
   }

   // Set the final size of the dock window
   SetMinSize( wxSize( -1, stack[ 0 ].GetY() ) );

   // Clean things up
   Refresh( false );
}

//
// Determine the location and bar before which a new bar would be placed
//
int ToolDock::PositionBar( ToolBar *t, wxPoint & pos, wxRect & rect )
{
   struct
   {
      wxRect rect;
      wxSize min;
   } tinfo[ ToolBarCount + 1 ];

   wxRect stack[ ToolBarCount + 1 ];
   wxPoint cpos, lpos;
   int ct, lt = 0;
   int ndx, stkcnt = 0;
   int tindx = -1;
   int cnt = mDockedBars.GetCount();
   int width, height;

   // Get size of our parent since we haven't been sized yet
   GetParent()->GetClientSize( &width, &height );
   width -= toolbarGap;
   height -= toolbarGap;

   // Set initial stack entry to maximum size
   stack[ 0 ].SetX( toolbarGap );
   stack[ 0 ].SetY( toolbarGap );
   stack[ 0 ].SetWidth( width );
   stack[ 0 ].SetHeight( height );

   // Process all docked and visible toolbars
   //
   // Careful...slightly different from above in that we expect to
   // process one more bar than is currently docked (<= in for)
   for( ndx = 0, ct = 0; ndx <= cnt; ndx++, ct++ )
   {
      // We're on the last entry...
      if( ndx == cnt )
      {
         // ...so check to see if the new bar has been placed yet
         if( tindx == -1 )
         {
            // Add the new bars' dimensions to the mix
            tinfo[ ct ].rect = t->GetRect();
            tinfo[ ct ].min = t->GetMinSize();
            tindx = ct;
         }
      }
      else
      {
         // Cache toolbar pointer
         ToolBar *b = (ToolBar *) mDockedBars[ ndx ];

         // Remember current bars' dimensions
         tinfo[ ct ].rect = b->GetRect();
         tinfo[ ct ].min = b->GetSize();

         // Insert the new bar if it hasn't already been done
         if( tindx == -1 )
         {
            wxRect r;

            // Get bar rect and make gap part of it
            r.SetPosition( b->GetParent()->ClientToScreen( b->GetPosition() ) );
            r.SetSize( b->IsResizable() ? b->GetSize() : b->GetSize() );
            r.width += toolbarGap;
            r.height += toolbarGap;

            // Does the location fall within this bar?
            if( r.Contains( pos ) || pos.y <= r.y )
            {
               // Add the new bars' dimensions to the mix
               tinfo[ ct ].rect = t->GetRect();
               tinfo[ ct ].min = t->GetSize();
               tindx = ct;
               ndx--;
            }
         }
      }

      // Get and cache the toolbar sizes
      wxSize sz = tinfo[ ct ].min;
      int tw = sz.GetWidth() + toolbarGap;
      int th = sz.GetHeight() + toolbarGap;

      // Will this one fit in remaining horizontal space?
      if( ( tw > stack[ stkcnt ].GetWidth() ) ||
          ( th > stack[ stkcnt ].GetHeight() ) )
      {
         // Destack entries until one is found in which this bar
         // will fit or until we run out of stacked entries
         while( stkcnt > 0 )
         {
            stkcnt--;

            // Get out if it will fit
            if( ( tw <= stack[ stkcnt ].GetWidth() ) &&
                ( th <= stack[ stkcnt ].GetHeight() ) )
            {
               break;
            }
         }
      }

      // The current stack entry position is where the bar
      // will be placed.
      cpos = stack[ stkcnt ].GetPosition();

      // We'll be using at least a portion of this stack entry, so
      // adjust the location and size.  It is possible that these
      // will become zero if this entry and the toolbar have the
      // same height.  This is what we want as it will be destacked
      // in the next iteration.
      stack[ stkcnt ].SetY(      stack[ stkcnt ].GetY()      + th );
      stack[ stkcnt ].SetHeight( stack[ stkcnt ].GetHeight() - th );

      // Calc the next possible horizontal location.
      int x = cpos.x + tw;

      // Add a new stack entry
      stkcnt++;
      stack[ stkcnt ].SetX( x );
      stack[ stkcnt ].SetY( cpos.y );
      stack[ stkcnt ].SetWidth( width - x );
      stack[ stkcnt ].SetHeight( th );

      // Position the previous toolbar
      if( ndx > 0 )
      {
         // Place the unstretched toolbar
         tinfo[ lt ].rect.x = lpos.x;
         tinfo[ lt ].rect.y = lpos.y;
      }

      // Place and stretch the final toolbar
      if( ndx == cnt )
      {
         tinfo[ ct ].rect.x = cpos.x;
         tinfo[ ct ].rect.y = cpos.y;
      }

      // Remember for next iteration
      lt = ct;
      lpos = cpos;
   }

   // Fill in the final position
   rect = tinfo[ tindx ].rect;

   return tindx;
}

//
// Toggles the visible/hidden state of a toolbar
//
void ToolDock::ShowHide( int type )
{
   ToolBar *t = mBars[ type ];

   // Maintain the docked array
   if( t->IsVisible() )
   {
      mDockedBars.Remove( t );
   }
   else
   {
      mDockedBars.Add( t );
   }

   // Make it (dis)appear
   t->Expose( !t->IsVisible() );

   // Update the layout
   LayoutToolBars();
   Updated();
}

//
// Set the visible/hidden state of a toolbar
//
void ToolDock::Expose( int type, bool show )
{
   ToolBar *t = mBars[ type ];

   // Maintain the docked array
   if( show )
   {
      if( mDockedBars.Index( t ) == wxNOT_FOUND )
      {
         mDockedBars.Add( t );
      }
   }
   else
   {
      if( mDockedBars.Index( t ) != wxNOT_FOUND )
      {
         mDockedBars.Remove( t );
      }
   }

   // Make it (dis)appear
   t->Expose( show );

   // Update the layout
   LayoutToolBars();
   Updated();
}

//
// Queues an EVT_TOOLBAR_UPDATED command event to notify any
// interested parties of an updated toolbar or dock layout
//
void ToolDock::Updated()
{
   // Queue an update event
   wxCommandEvent e( EVT_TOOLBAR_UPDATED, GetId() );
   GetParent()->GetEventHandler()->AddPendingEvent( e );
}

//
// Handle grabber clicking
//
void ToolDock::OnGrabber( GrabberEvent & event )
{
   ToolBar *t = mBars[ event.GetId() ];

   // Pass it on to the manager since it isn't in the handling hierarchy
   mManager->ProcessEvent( event );

   // We no longer have control
   mDockedBars.Remove( t );
}

//
// Handle sizing
//
void ToolDock::OnSize( wxSizeEvent & WXUNUSED(event) )
{
//   event.Skip();
}

//
// Prevent flicker
//
void ToolDock::OnErase( wxEraseEvent & WXUNUSED(event) )
{
   // Ignore it to prevent flashing
}

//
// Repaint toolbar gap lines
//
void ToolDock::OnPaint( wxPaintEvent & WXUNUSED(event) )
{
   // Don't use a wxBufferedPaintDC() here.  It produces a bogus
   // background on Windows and GTK.
   wxPaintDC dc( this );

   // Start with a clean background
   //
   // Under GTK, we specifically set the toolbar background to the background
   // colour in the system theme.
#if defined( __WXGTK__ )
   dc.SetBackground( wxBrush( wxSystemSettings::GetColour( wxSYS_COLOUR_BACKGROUND ) ) );
#endif
   dc.Clear();

   // Set the gap color
   AColor::Dark( &dc, false );

   // Draw the initial horizontal and vertical gaps
   wxSize sz = GetClientSize();

   AColor::Line(dc, 0, 0, sz.GetWidth(), 0 );
   AColor::Line(dc, 0, 0, 0, sz.GetHeight() );

   // Draw the gap between each bar
   int ndx, cnt = mDockedBars.GetCount();
   for( ndx = 0; ndx < cnt; ndx++ )
   {
      wxRect r = ( (ToolBar *)mDockedBars[ ndx ] )->GetRect();

      AColor::Line( dc,
                    r.GetLeft(),
                    r.GetBottom() + 1,
                    sz.GetWidth(),
                    r.GetBottom() + 1 );

      // For all bars but the last...
      if( ndx < cnt - 1 )
      {
         // ...and for bars that aren't the last in a row, draw an
         // horizontal gap line
         if( r.y == ( (ToolBar *)mDockedBars[ ndx + 1 ] )->GetRect().y )
         {
            AColor::Line(dc,
                         r.GetRight() + 1,
                         r.GetTop(),
                         r.GetRight() + 1,
                         r.GetBottom() + 1 );
         }
      }
   }
}
