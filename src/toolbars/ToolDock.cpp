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

auto ToolBarConfiguration::FindPlace(const ToolBar *bar) const
   -> Iterator
{
   return std::find_if(begin(), end(),
      [=](const Place &place){ return place.pBar == bar; }
   );
}

void ToolBarConfiguration::Insert(ToolBar *bar, Position position)
{
   if (position >= size() || position == UnspecifiedPosition)
      push_back(bar);
   else
      wxArrayPtrVoid::Insert(bar, position);
}

void ToolBarConfiguration::Remove(const ToolBar *bar)
{
   wxArrayPtrVoid::Remove(const_cast<ToolBar*>(bar));
}

void ToolBarConfiguration::Show(ToolBar *bar)
{
   // Do not assume the bar is absent, though in practice that is always so
   if (!Contains(bar))
      Insert(bar);
}

void ToolBarConfiguration::Hide(ToolBar *bar)
{
   // Future:  might hide a bar without eliminating it from the configuration
   Remove(bar);
}

bool ToolBarConfiguration::IsRightmost(const ToolBar *bar) const
{
   auto iter = FindPlace(bar);
   auto endit = end();
   if (iter == endit)
      // not present
      return true;
   if (++iter == endit)
      // Last of all
      return true;
   if (bar->GetRect().y != iter->pBar->GetRect().y)
      // Last in its row
      return true;
   return false;
}

bool ToolBarConfiguration::Read
   (ToolBarConfiguration *pConfiguration,
    ToolManager *pManager,
    Legacy *,
    ToolBar *bar, bool &visible, bool defaultVisible)
{
   bool result = true;

   if (pConfiguration) {
      int ord;
      gPrefs->Read( wxT("Order"), &ord, -1 );
      // Index was written 1-based
      --ord;
      if (ord >= ToolBarCount)
         result = false;
      else if (ord >= 0)
      {
         while(pConfiguration->size () <= ord)
            pConfiguration->push_back(nullptr);
         (*pConfiguration)[ord] = bar;
      }
   }
   // Future: might remember visibility in the configuration, not forgetting
   // positions of hidden bars.
   gPrefs->Read( wxT("Show"), &visible, defaultVisible);

   return result;
}

void ToolBarConfiguration::PostRead(Legacy &)
{
   auto b = wxArrayPtrVoid::begin();
   auto iter =
      std::remove(b, wxArrayPtrVoid::end(), nullptr);
   resize(iter - b);
}

void ToolBarConfiguration::Write
   (const ToolBarConfiguration *pConfiguration, const ToolBar *bar)
{
   if (pConfiguration) {
      auto index = pConfiguration->Index(const_cast<ToolBar*>(bar));
      if (index != wxNOT_FOUND)
         gPrefs->Write( wxT("Order"), 1 + index );
   }
   gPrefs->Write( wxT("Show"), bar->IsVisible() );
}

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
   EVT_MOUSE_EVENTS( ToolDock::OnMouseEvents )
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
// Remove the toolbar from our control
//
void ToolDock::Undock( ToolBar *bar )
{
   if( mConfiguration.Contains( bar ) )
   {
      mConfiguration.Remove( bar );
      mBars[ bar->GetId() ] = nullptr;
   }
}

//
// Handle ToolDock events
//
void ToolDock::Dock( ToolBar *bar, bool deflate, ToolBarConfiguration::Position position )
{
   // Adopt the toolbar into our family
   bar->Reparent( this );
   mBars[ bar->GetId() ] = bar;

   // Reset size
   bar->SetSize(
      // Undo the expansion that was applied when un-docking
      bar->GetSize().x - (deflate ? 2 * ToolBarFloatMargin : 0),
      // Don't need to adjust y the same way.
      bar->GetDockedSize().y
   );

   // Park the NEW bar in the correct berth
   if (!mConfiguration.Contains(bar))
      mConfiguration.Insert( bar, position );

   // Inform toolbar of change
   bar->SetDocked( this, false );

   // Rearrange our world
   LayoutToolBars();
   Updated();
}

// Initial docking of bars
void ToolDock::LoadConfig(ToolBar *bars[])
{
   // Add all ordered toolbars
   for(const auto &place : GetConfiguration()) {
      auto bar = place.pBar;
      this->Dock(bar, false);
      // Show it -- hidden bars are not (yet) ever saved as part of a
      // configuration
      Expose( bar->GetId(), true );
   }
}

//
// Layout the toolbars
//
void ToolDock::LayoutToolBars()
{
   ToolBar *lt = nullptr;

   wxRect stack[ ToolBarCount + 1 ];
   int stkcnt = 0;
   int width, height;

   // Get size of our parent since we haven't been sized yet
   GetParent()->GetClientSize( &width, &height );
   width -= toolbarGap;
   height -= toolbarGap;

   // Set initial stack entry to maximum size
   stack[ 0 ].SetX( toolbarGap );
   stack[ 0 ].SetY( toolbarGap );
   // The stack width and height are the remaining width and height.
   stack[ 0 ].SetWidth( width );
   stack[ 0 ].SetHeight( height );

   // Process all docked and visible toolbars
   for (const auto &place : GetConfiguration())
   {
      // Cache toolbar pointer
      ToolBar *ct = place.pBar;

      // Get and cache the toolbar sizes
      wxSize sz = ct->GetSize();
      int tw = sz.GetWidth() + toolbarGap;
      int th = sz.GetHeight() + toolbarGap;

      // This loop reduces stkcnt until it gives a box
      // that we fit in.
      while (stkcnt > 0)
      {
         // Get out if it will fit
         bool bTooWide = tw > stack[stkcnt].GetWidth();
         // We'd like to be able to add a tall toolbar in at the start of a row,
         // even if there isn't enough height for it.
         // If so, we'd have to at least change how we calculate 'bTooHigh'.
         bool bTooHigh = th > stack[stkcnt].GetHeight();
         //bTooHigh &= stack[stkcnt].GetWidth() < (width - toolbarGap);
         //bTooHigh = false;

         if (!bTooWide && !bTooHigh)
            break;
         stkcnt--;
      }

      // The current stack entry position is where the bar
      // will be placed.
      const auto cpos = stack[ stkcnt ].GetPosition();

      // Position the previous toolbar
      if( lt )
      {
         // Keep the tab order in order
         ct->MoveAfterInTabOrder( lt );
      }

      // Place the toolbar
      ct->SetPosition( wxPoint( cpos.x, cpos.y ) );

      // Remember for next iteration
      lt = ct;

      // We'll be using at least a portion of this stack entry, so
      // adjust the location and size.  It is possible that these
      // will become zero if this entry and the toolbar have the
      // same height.  This is what we want as it will be destacked
      // in the next iteration.
      stack[ stkcnt ].SetY(      stack[ stkcnt ].GetY()      + th );
      stack[ stkcnt ].SetHeight( stack[ stkcnt ].GetHeight() - th );

      // Calc the next possible horizontal location.
      int x = cpos.x + tw;

      // Add a NEW stack entry
      stkcnt++;
      stack[ stkcnt ].SetX( x );
      stack[ stkcnt ].SetY( cpos.y );
      stack[ stkcnt ].SetWidth( width - x );
      stack[ stkcnt ].SetHeight( th );
   }

   // Set the final size of the dock window
   SetMinSize( wxSize( stack[ 0 ].width, stack[ 0 ].GetY() ) );

   // Clean things up
   Refresh( false );
}

//
// Determine the position where a NEW bar would be placed
//
// 'rect' will be the rectangle for the dock marker.
ToolBarConfiguration::Position
   ToolDock::PositionBar( ToolBar *t, const wxPoint & pos, wxRect & rect )
{
   auto tindx = ToolBarConfiguration::UnspecifiedPosition;

   wxRect stack[ ToolBarCount + 1 ];
   int stkcnt = 0;
   int width, height;

   // Get size of our parent since we haven't been sized yet
   GetParent()->GetClientSize( &width, &height );
   width -= toolbarGap;
   height -= toolbarGap;

   // Set initial stack entry to maximum size
   stack[ 0 ].SetX( toolbarGap );
   stack[ 0 ].SetY( toolbarGap );
   // The stack width and height are the remaining width and height.
   stack[ 0 ].SetWidth( width );
   stack[ 0 ].SetHeight( height );

   // Process all docked and visible toolbars
   //
   // Careful...slightly different from above in that we expect to
   // process one more bar than is currently docked
   for ( auto iter = GetConfiguration().begin(),
         end = GetConfiguration().end();
         ; // iterate once more at end
         ++iter )
   {
      wxRect sz;

      // If last entry, then it is the
      if (iter == end)
      {
         // Add the NEW bars' dimensions to the mix
         rect = t->GetRect();
         sz = t->GetDockedSize();
         // This will break the loop
         tindx = iter->position;
      }
      else
      {
         // Cache toolbar pointer
         ToolBar *ct = iter->pBar;

         // Remember current bars ' dimensions
         sz = ct->GetSize();

         // Maybe insert the NEW bar if it hasn't already been done
         // and is in the right place.
         if (tindx == ToolBarConfiguration::UnspecifiedPosition)
         {
            wxRect r;

            // Get bar rect and make gap part of it
            r.SetPosition(ct->GetParent()->ClientToScreen(ct->GetPosition()));
            r.SetSize(ct->IsResizable() ? ct->GetSize() : ct->GetSize());
            r.width += toolbarGap;
            r.height += toolbarGap;

            // Does the location fall within this bar?
            if (r.Contains(pos) || pos.y <= r.y)
            {
               // Add the NEW bars' dimensions to the mix
               rect = t->GetRect();
               sz = t->GetDockedSize();
               tindx = iter->position;
            }
         }
      }

      // Get and cache the toolbar sizes
      int tw = sz.GetWidth() + toolbarGap;
      int th = sz.GetHeight() + toolbarGap;

      // This loop reduces stkcnt until it gives a box
      // that we fit in.
      while (stkcnt > 0)
      {
         // Get out if it will fit
         bool bTooWide = tw > stack[stkcnt].GetWidth();
         // We'd like to be able to add a tall toolbar in at the start of a row,
         // even if there isn't enough height for it.
         // If so, we'd have to at least change how we calculate 'bTooHigh'.
         bool bTooHigh = th > stack[stkcnt].GetHeight();
         //bTooHigh &= stack[stkcnt].GetWidth() < (width - toolbarGap);
         //bTooHigh = false;

         if (!bTooWide && !bTooHigh)
            break;
         stkcnt--;
      }

      // The current stack entry position is where the bar
      // will be placed.
      const auto cpos = stack[stkcnt].GetPosition();

      // If we've placed it, we're done.
      if (tindx != ToolBarConfiguration::UnspecifiedPosition)
      {
         rect.x = cpos.x;
         rect.y = cpos.y;
         break;
      }

      // We'll be using at least a portion of this stack entry, so
      // adjust the location and size.  It is possible that these
      // will become zero if this entry and the toolbar have the
      // same height.  This is (?) what we want as it will be destacked
      // in the next iteration.
      stack[stkcnt].SetY(stack[stkcnt].GetY() + th);
      stack[stkcnt].SetHeight(stack[stkcnt].GetHeight() - th);

      // Calc the next possible horizontal location.
      int x = cpos.x + tw;

      // Add a NEW stack entry
      stkcnt++;
      stack[stkcnt].SetX(x);
      stack[stkcnt].SetY(cpos.y);
      stack[stkcnt].SetWidth(width - x);
      stack[stkcnt].SetHeight(th);
   }

   // rect is decided
   return tindx;
}

//
// Set the visible/hidden state of a toolbar
//
void ToolDock::Expose( int type, bool show )
{
   ToolBar *t = mBars[ type ];

   // Maintain the docked array
   const auto shown = mConfiguration.Shows( t );
   if( show && !shown )
      mConfiguration.Show( t );
   else if( !show && shown )
      mConfiguration.Hide( t );

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
   auto pos = event.GetPosition();
   if (!event.IsEscaping()) {
      ToolBar *t = mBars[ event.GetId() ];

      // Pass it on to the manager since it isn't in the handling hierarchy
      mManager->ProcessEvent( event );

      // We no longer have control
      mConfiguration.Remove( t );
   }
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
   for (const auto &place : GetConfiguration())
   {
      auto toolbar = place.pBar;
      if (!toolbar)
         continue;

      wxRect r = toolbar->GetRect();

      AColor::Line( dc,
                   r.GetLeft(),
                   r.GetBottom() + 1,
                   sz.GetWidth(),
                   r.GetBottom() + 1 );

      // For all bars but the last...
      // ...and for bars that aren't the last in a row, draw an
      // horizontal gap line
      if (!mConfiguration.IsRightmost(toolbar)) {
         AColor::Line(dc,
            r.GetRight() + 1,
            r.GetTop(),
            r.GetRight() + 1,
            r.GetBottom() + 1 );
      }
   }
}

void ToolDock::OnMouseEvents(wxMouseEvent &event)
{
   // Do this hack so scrubber can detect mouse drags anywhere
   event.ResumePropagation(wxEVENT_PROPAGATE_MAX);
   event.Skip();
}
