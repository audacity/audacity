/**********************************************************************

  Audacity: A Digital Audio Editor

  ToolBar.cpp

  Dominic Mazzoni
  Shane T. Mueller
  Leland Lucius

  See ToolBar.h for details.

*******************************************************************//**

\file ToolBar.cpp

  Implements ToolBar

*//*******************************************************************//**

\class ToolBar
\brief Works with ToolManager and ToolDock to provide a dockable window
in which buttons can be placed.

*//**********************************************************************/


#include "ToolBar.h"

// For compilers that support precompilation, includes "wx/wx.h".
#include <wx/wxprec.h>

#include <wx/setup.h> // for wxUSE_* macros

#ifndef WX_PRECOMP
#include <wx/dcclient.h>
#include <wx/defs.h>
#include <wx/gdicmn.h>
#include <wx/image.h>
#include <wx/intl.h>
#include <wx/settings.h>
#include <wx/sizer.h>
#include <wx/sysopt.h>
#include <wx/window.h>
#endif  /*  */

#include "ToolDock.h"

#include "../AllThemeResources.h"
#include "../AColor.h"
#include "../ImageManipulation.h"
#include "../Project.h"
#include "../commands/CommandManager.h"
#include "../widgets/AButton.h"
#include "../widgets/Grabber.h"
#include "Prefs.h"

////////////////////////////////////////////////////////////
/// ToolBarResizer
////////////////////////////////////////////////////////////

//
// Width of the resize grab area
//
#define RWIDTH 4

/// \brief a wxWindow that provides the resizer for a toolbar on the 
/// right hand side.  Responsible for drawing the resizer appearance, 
/// resizing mouse events and constraining the resizing.
class ToolBarResizer final : public wxWindow
{
public:
   ToolBarResizer(ToolBar *mBar);
   virtual ~ToolBarResizer();

   // We don't need or want to accept focus.
   // Note that AcceptsFocusFromKeyboard() is overridden rather than
   // AcceptsFocus(), so that resize can be cancelled by ESC
   bool AcceptsFocusFromKeyboard() const override {return false;}

private:
   void OnErase(wxEraseEvent & event);
   void OnPaint(wxPaintEvent & event);
   void OnLeftDown(wxMouseEvent & event);
   void OnLeftUp(wxMouseEvent & event);
   void OnEnter(wxMouseEvent & event);
   void OnLeave(wxMouseEvent & event);
   void OnMotion(wxMouseEvent & event);
   void ResizeBar(const wxSize &size);
   void OnCaptureLost(wxMouseCaptureLostEvent & event);
   void OnKeyDown(wxKeyEvent &event);

private:
   ToolBar *mBar;
   wxPoint mResizeOffset;
   wxSize mOrigSize;
   wxWindowRef mOrigFocus{};

   DECLARE_EVENT_TABLE()
};

//
// Event table
//
BEGIN_EVENT_TABLE( ToolBarResizer, wxWindow )
   EVT_ERASE_BACKGROUND( ToolBarResizer::OnErase )
   EVT_PAINT( ToolBarResizer::OnPaint )
   EVT_LEFT_DOWN( ToolBarResizer::OnLeftDown )
   EVT_LEFT_UP( ToolBarResizer::OnLeftUp )
   EVT_ENTER_WINDOW( ToolBarResizer::OnEnter )
   EVT_LEAVE_WINDOW( ToolBarResizer::OnLeave )
   EVT_MOTION( ToolBarResizer::OnMotion )
   EVT_MOUSE_CAPTURE_LOST( ToolBarResizer::OnCaptureLost )
   EVT_KEY_DOWN( ToolBarResizer::OnKeyDown )
END_EVENT_TABLE();

ToolBarResizer::ToolBarResizer(ToolBar *bar)
:  wxWindow(bar, wxID_ANY, wxDefaultPosition, wxSize(RWIDTH, -1))
{
   mBar = bar;
   SetCursor( wxCURSOR_SIZEWE );
}

ToolBarResizer::~ToolBarResizer()
{
   if(HasCapture())
      ReleaseMouse();
}

//
// Handle background erasure
//
void ToolBarResizer::OnErase( wxEraseEvent & WXUNUSED(event) )
{
   // Ignore it to prevent flashing
}

//
// This draws the background of a toolbar
//
void ToolBarResizer::OnPaint( wxPaintEvent & event )
{
   wxPaintDC dc( (wxWindow *) event.GetEventObject() );

   // Start with a clean background
   //
   // Under GTK, we specifically set the toolbar background to the background
   // colour in the system theme.
#if defined( __WXGTK__ )
//   dc.SetBackground( wxBrush( wxSystemSettings::GetColour( wxSYS_COLOUR_BACKGROUND ) ) );
#endif
   dc.SetBackground( wxBrush( theTheme.Colour( clrMedium  ) ) );
   dc.Clear();

   wxSize sz = GetSize();

   AColor::Dark( &dc, false );
   AColor::Line(dc, sz.x - 4,  0, sz.x - 4, sz.y );
   AColor::Line(dc, sz.x - 1,  0, sz.x - 1, sz.y );
}

//
// Handle toolbar resizing
//
void ToolBarResizer::OnLeftDown( wxMouseEvent & event )
{
   // Go ahead and set the event to propagate
   event.Skip();

   // Retrieve the mouse position
   // Bug 1896: This is at time of processing the event, rather than at time 
   // of generation of event.  Works around event.GetPosition() giving
   // incorrect values if position of resizer is changing.
   mResizeOffset = wxGetMousePosition()-mBar->GetRect().GetBottomRight();

   mOrigSize = mBar->GetSize();

   // We want all of the mouse events
   if( !HasCapture() )
      CaptureMouse();
}

void ToolBarResizer::OnLeftUp( wxMouseEvent & event )
{
   // Go ahead and set the event to propagate
   event.Skip();

   if( HasCapture() )
   {
      ReleaseMouse();
      if (mOrigFocus)
         mOrigFocus->SetFocus();
      mOrigFocus = nullptr;
      mBar->ResizingDone();
   }
}

void ToolBarResizer::OnEnter( wxMouseEvent & /*event*/ )
{
   // Bug 1201:  On Mac, unsetting and re-setting the tooltip may be needed
   // to make it pop up when we want it.
   const auto text = GetToolTipText();
   UnsetToolTip();
   SetToolTip(text);
   if (!mOrigFocus)
      mOrigFocus = FindFocus();
}

void ToolBarResizer::OnLeave( wxMouseEvent & /*event*/ )
{
   if (!GetCapture())
      mOrigFocus = nullptr;
}

void ToolBarResizer::OnMotion( wxMouseEvent & event )
{
   // Go ahead and set the event to propagate
   event.Skip();

   if( HasCapture() && event.Dragging() )
   {
      // Retrieve the mouse position
      // Bug 1896: This is at time of processing the event, rather than at time 
      // of generation of event.  Works around event.GetPosition() giving
      // incorrect values if position of resizer is changing.
      wxPoint pos = wxGetMousePosition();

      wxRect r = mBar->GetRect();
      wxSize minsz = mBar->GetMinSize();
      wxSize maxsz = mBar->GetMaxSize();
      wxSize psz = mBar->GetParent()->GetClientSize();

      // Adjust the size based on updated mouse position.
      r.width = ( pos.x - mResizeOffset.x ) - r.x;

      // Keep it within max size, if specified
      if( maxsz != wxDefaultSize )
      {
         if( r.width > maxsz.x )
         {
            r.width = maxsz.x;
         }
         if( r.height > maxsz.y )
         {
            r.height = maxsz.y;
         }
      }

      // Constrain
      if( r.width < minsz.x )
      {
         // Don't allow resizing to go too small
         r.width = minsz.x;
      }
      else if( r.GetRight() > psz.x - 3 )
      {
         // Don't allow resizing to go too large
         //
         // The 3 magic pixels are because I'm too chicken to change the
         // calculations in ToolDock::LayoutToolBars() even though I'm
         // the one that set them up.  :-)
         r.SetRight( psz.x - 3 );
      }

      ResizeBar( r.GetSize() );
   }
}

void ToolBarResizer::ResizeBar(const wxSize &size)
{
   mBar->SetSize( size );

   // Tell everyone we've changed sizes
   mBar->Updated();

   // Refresh our world
   mBar->GetParent()->Refresh();
   mBar->GetParent()->Update();
}

void ToolBarResizer::OnCaptureLost( wxMouseCaptureLostEvent & WXUNUSED(event) )
{
   if( HasCapture() )
   {
      ReleaseMouse();
      if (mOrigFocus)
         mOrigFocus->SetFocus();
      mOrigFocus = nullptr;
   }
}

void ToolBarResizer::OnKeyDown(wxKeyEvent &event)
{
   event.Skip();
   if (HasCapture() && WXK_ESCAPE == event.GetKeyCode()) {
      ResizeBar( mOrigSize );
      ReleaseMouse();
      if (mOrigFocus)
         mOrigFocus->SetFocus();
      mOrigFocus = nullptr;
   }
}

////////////////////////////////////////////////////////////
/// Methods for ToolBar
////////////////////////////////////////////////////////////

//
// Define class to RTTI
//
IMPLEMENT_CLASS( ToolBar, wxPanelWrapper );

//
// Custom event
//
DEFINE_EVENT_TYPE(EVT_TOOLBAR_UPDATED)

//
// Event table
//
BEGIN_EVENT_TABLE( ToolBar, wxPanelWrapper )
   EVT_PAINT( ToolBar::OnPaint )
   EVT_ERASE_BACKGROUND( ToolBar::OnErase )
   EVT_MOUSE_EVENTS( ToolBar::OnMouseEvents )
END_EVENT_TABLE()

//
// Constructor
//
ToolBar::ToolBar( AudacityProject &project,
                  int type,
                  const TranslatableString &label,
                  const wxString &section,
                  bool resizable )
: wxPanelWrapper()
, mProject{ project }
{
   // Save parameters
   mType = type;
   mLabel = label;
   mSection = section;
   mResizable = resizable;

   // Initialize everything
   mParent = NULL;
   mHSizer = NULL;
   mVisible = false;
   mPositioned = false;

   mGrabber = NULL;
   mResizer = NULL;
   SetId(mType);
}

//
// Destructor
//
ToolBar::~ToolBar()
{
}

//
// Returns the toolbar title
//
TranslatableString ToolBar::GetTitle()
{
   /* i18n-hint: %s will be replaced by the name of the kind of toolbar.*/
   return XO("Audacity %s Toolbar").Format( GetLabel() );
}

//
// Returns the toolbar label
//
TranslatableString ToolBar::GetLabel()
{
   return mLabel;
}

//
// Returns the toolbar preferences section
//
wxString ToolBar::GetSection()
{
   return mSection;
}

//
// Returns the toolbar type
//
int ToolBar::GetType()
{
   return mType;
}

//
// Set the toolbar label
//
void ToolBar::SetLabel(const wxString & label)
{
   // Probably shouldn't reach this overload, but perhaps virtual function
   // dispatch will take us here from a pointer to the wxPanel base class
   mLabel = Verbatim( label );
}

void ToolBar::SetLabel(const TranslatableString & label)
{
   // Only this overload is publicly accessible when you have a pointer to
   // Toolbar or a subclass of it
   mLabel = label;
}

//
// Returns whether the toolbar is resizable or not
//
bool ToolBar::IsResizable() const
{
   return mResizable;
}

//
// Returns the dock state of the toolbar
//
bool ToolBar::IsDocked() const
{
   return const_cast<ToolBar*>(this)->GetDock() != nullptr;
}

//
// Returns the visibility of the toolbar
//
bool ToolBar::IsVisible() const
{
   return mVisible;
}

void ToolBar::SetVisible( bool bVisible )
{
   mVisible = bVisible;
}

//
// Show or hide the toolbar
//
bool ToolBar::Expose( bool show )
{
   bool was = mVisible;

   SetVisible( show );

   if( IsDocked() )
   {
      Show( show );
      if( show )
      {
         Refresh();
      }
   }
   else
   {
      wxWindow * pParent = GetParent();
      if( !IsPositioned() && show ){
         SetPositioned();
         pParent->CentreOnParent();
         pParent->Move( pParent->GetPosition() + wxSize( mType*10, mType*10 ));
      }
      pParent->Show( show );
   }

   return was;
}

//
// Initialize the toolbar
//
void ToolBar::Create( wxWindow *parent )
{
   // Save parameters
   mParent = parent;

   // Create the window and label it
   wxPanelWrapper::Create( mParent,
                    mType,
                    wxDefaultPosition,
                    wxDefaultSize,
                    wxNO_BORDER | wxTAB_TRAVERSAL,
                    GetTitle() );
   wxPanelWrapper::SetLabel( GetLabel() );

   // Go do the rest of the creation
   ReCreateButtons();

   // ToolManager depends on this appearing to be visible for proper dock construction
   mVisible = true;
}

void ToolBar::SetToDefaultSize(){
   wxSize sz;
   sz.SetHeight( -1 );
   sz.SetWidth( GetInitialWidth());
   SetSize( sz );
}

wxSize ToolBar::GetSmartDockedSize()
{
   const int tbs = toolbarSingle + toolbarGap;
   wxSize sz = GetSize();
   // 46 is the size where we switch from expanded to compact.
   if( sz.y < 46 )
      sz.y = tbs-1;
   else 
      sz.y = 2 * tbs -1;
   return sz;
}


void ToolBar::ReCreateButtons()
{
   wxSize sz3 = GetSize();
   //wxLogDebug( "x:%i y:%i",sz3.x, sz3.y);

   // SetSizer(NULL) detaches mHSizer and deletes it.
   // Do not use Detach() here, as that attempts to detach mHSizer from itself!
   SetSizer( NULL );

   // Get rid of any children we may have
   DestroyChildren();
   mGrabber = NULL;
   mResizer = NULL;
   SetLayoutDirection(wxLayout_LeftToRight);

   // Refresh the background before populating
   if (!IsDocked())
   {
      GetParent()->Refresh();
   }

   {
      // Create the main sizer
      auto ms = std::make_unique<wxBoxSizer>(wxHORIZONTAL);

      // Create the grabber and add it to the main sizer
      mGrabber = safenew Grabber(this, mType);
      ms->Add(mGrabber, 0, wxEXPAND | wxALIGN_LEFT | wxALIGN_TOP | wxRIGHT, 1);

      // Use a box sizer for laying out controls
      ms->Add((mHSizer = safenew wxBoxSizer(wxHORIZONTAL)), 1, wxEXPAND);

      // Go add all the rest of the gadgets
      Populate();

      // Add some space for the resize border
      if (IsResizable())
      {
         // Create the resizer and add it to the main sizer
         mResizer = safenew ToolBarResizer(this);
         ms->Add(mResizer, 0, wxEXPAND | wxALIGN_TOP | wxLEFT, 1);
         mResizer->SetToolTip(_("Click and drag to resize toolbar"));
      }
      
      // Set dock after possibly creating resizer.
      // (Re)Establish dock state
      SetDocked(GetDock(), false);

      // Set the sizer
      SetSizerAndFit(ms.release());
   }

   // Recalculate the height to be a multiple of toolbarSingle
   const int tbs = toolbarSingle + toolbarGap;
   wxSize sz = GetSize();
   sz.y = ( ( ( sz.y + tbs -1) / tbs ) * tbs ) - 1;

   // Set the true AND minimum sizes and do final layout
   if(IsResizable())
   {
      // JKC we're going to allow all resizable toolbars to be resized
      // to 1 unit high, typically 27 pixels.
      wxSize sz2 = sz;
      sz2.SetWidth(GetMinToolbarWidth());
      sz2.y = tbs -1;
      SetMinSize(sz2);
      
      // sz2 is now the minimum size.
      // sz3 is the size we were.

      // We're recreating buttons, and we want to preserve original size.
      // But not if that makes the size too small.

      // Size at least as big as minimum.
      if( sz3.y < sz2.y )
         sz3.y = sz2.y;
      if( sz3.x < sz2.x )
         sz3.x = sz2.x;
      SetSize(sz3);
   }
   else
   {
      SetInitialSize(sz);
   }
   Layout();
}

// The application preferences have changed, so update any elements that may
// depend on them.
void ToolBar::UpdatePrefs()
{
#if wxUSE_TOOLTIPS
   // Change the tooltip of the grabber
   if ( mGrabber )
   {
      mGrabber->SetToolTip( GetTitle() );
   }

   // Change the tooltip of the resizer
   if ( mResizer )
   {
      mResizer->SetToolTip( _("Click and drag to resize toolbar") );
      wxSizeEvent e;
      GetParent()->GetEventHandler()->AddPendingEvent( e );
      GetParent()->Refresh();
   }
#endif

   return;
}

//
// Return the pointer to the ToolBock where this bar lives
//
ToolDock *ToolBar::GetDock()
{
   return dynamic_cast<ToolDock*>(GetParent());
}

//
// Toggle the docked/floating state
//
void ToolBar::SetDocked( ToolDock *dock, bool pushed )
{
   // Remember it
//   mDock = dock;

   // Change the tooltip of the grabber
#if wxUSE_TOOLTIPS
   mGrabber->SetToolTip( GetTitle() );
#endif

   // Set the grabber button state
   mGrabber->PushButton( pushed );

   if (mResizer)
   {
      mResizer->Show(dock != NULL);
      Layout();
   }
}

//
// Notify parent of changes
//
void ToolBar::Updated()
{
   if( IsDocked() )
      GetDock()->Updated();
   else
      // Bug 2120.  Changing the choice also changes the size of the toolbar so
      // we need to update the client size, even if undocked.
      // If modifying/improving this, remember to test both changing the choice,
      // and clicking on the choice but not actually changing it.
      GetParent()->SetClientSize( GetSize() + wxSize( 2,2));
   //wxCommandEvent e( EVT_TOOLBAR_UPDATED, GetId() );
   //GetParent()->GetEventHandler()->AddPendingEvent( e );
}

//
// Returns a pointer to the main sizer
//
wxBoxSizer *ToolBar::GetSizer()
{
   return mHSizer;
}

//
// Add a window to the main sizer
//
void ToolBar::Add( wxWindow *window,
                   int proportion,
                   int flag,
                   int border,
                   wxObject* userData )
{
   mHSizer->Add( window,
                 proportion,
                 flag,
                 border,
                 userData );
}

//
// Add a child sizer to the main sizer
//
void ToolBar::Add( wxSizer *sizer,
                   int proportion,
                   int flag,
                   int border,
                   wxObject* userData )
{
   mHSizer->Add( sizer,
                 proportion,
                 flag,
                 border,
                 userData );
}

//
// Add some space to the main sizer
//
void ToolBar::Add( int width,
                   int height,
                   int proportion,
                   int flag,
                   int border,
                   wxObject* userData )
{
   mHSizer->Add( width,
                 height,
                 proportion,
                 flag,
                 border,
                 userData );
}

//
// Adds a spacer to the main sizer
//
void ToolBar::AddSpacer( int size )
{
   mHSizer->AddSpacer( size );
}

//
// Adds a strechable spacer to the main sizer
//
void ToolBar::AddStretchSpacer( int prop )
{
   mHSizer->AddStretchSpacer( prop );
}

//
// Detach a window from the main sizer
//
void ToolBar::Detach( wxWindow *window )
{
   mHSizer->Detach( window );
}

//
// Detach a child sizer from the main sizer
//
void ToolBar::Detach( wxSizer *sizer )
{
   mHSizer->Detach( sizer );
}

void ToolBar::MakeMacRecoloredImage(teBmps eBmpOut, teBmps eBmpIn )
{
   theTheme.ReplaceImage( eBmpOut, &theTheme.Image( eBmpIn ));
}

void ToolBar::MakeRecoloredImage( teBmps eBmpOut, teBmps eBmpIn )
{
   // Don't recolour the buttons...
   MakeMacRecoloredImage( eBmpOut, eBmpIn );
}

void ToolBar:: MakeButtonBackgroundsLarge()
{

   bool bUseAqua = false;

#ifdef EXPERIMENTAL_THEME_PREFS
   gPrefs->Read( wxT("/GUI/ShowMac"), &bUseAqua, false);
#endif

#ifdef USE_AQUA_THEME
   bUseAqua = !bUseAqua;
#endif

   if( bUseAqua ){
      MakeMacRecoloredImage( bmpRecoloredUpLarge,       bmpMacUpButton );
      MakeMacRecoloredImage( bmpRecoloredDownLarge,     bmpMacDownButton );
      MakeMacRecoloredImage( bmpRecoloredUpHiliteLarge, bmpMacHiliteUpButton );
      MakeMacRecoloredImage( bmpRecoloredHiliteLarge,   bmpMacHiliteButton );
   } else {
      MakeRecoloredImage( bmpRecoloredUpLarge,       bmpUpButtonLarge );
      MakeRecoloredImage( bmpRecoloredDownLarge,     bmpDownButtonLarge );
      MakeRecoloredImage( bmpRecoloredUpHiliteLarge, bmpHiliteUpButtonLarge );
      MakeRecoloredImage( bmpRecoloredHiliteLarge,   bmpHiliteButtonLarge );
   }
}

void ToolBar::MakeButtonBackgroundsSmall()
{

   bool bUseAqua = false;

#ifdef EXPERIMENTAL_THEME_PREFS
   gPrefs->Read( wxT("/GUI/ShowMac"), &bUseAqua, false);
#endif

#ifdef USE_AQUA_THEME
   bUseAqua = !bUseAqua;
#endif

   if( bUseAqua ){
      MakeMacRecoloredImage( bmpRecoloredUpSmall,       bmpMacUpButtonSmall );
      MakeMacRecoloredImage( bmpRecoloredDownSmall,     bmpMacDownButtonSmall );
      MakeMacRecoloredImage( bmpRecoloredUpHiliteSmall, bmpMacHiliteUpButtonSmall );
      MakeMacRecoloredImage( bmpRecoloredHiliteSmall,   bmpMacHiliteButtonSmall );
   } else {
      MakeRecoloredImage(    bmpRecoloredUpSmall,       bmpUpButtonSmall );
      MakeRecoloredImage(    bmpRecoloredDownSmall,     bmpDownButtonSmall );
      MakeRecoloredImage(    bmpRecoloredUpHiliteSmall, bmpHiliteUpButtonSmall );
      MakeRecoloredImage(    bmpRecoloredHiliteSmall,   bmpHiliteButtonSmall );
   }
}

/// Makes a button and its four different state bitmaps
/// @param parent            Parent window for the button.
/// @param eUp               Background for when button is Up.
/// @param eDown             Background for when button is Down.
/// @param eHilite           Background for when button is Hilit.
/// @param eStandardUp       Foreground when enabled, up.
/// @param eStandardDown     Foreground when enabled, down.
/// @param eDisabled         Foreground when disabled.
/// @param id                Windows Id.
/// @param placement         Placement position
/// @param processdownevents true iff button handles down events.
/// @param size              Size of the background.
AButton * ToolBar::MakeButton(wxWindow *parent,
                              teBmps eUp,
                              teBmps eDown,
                              teBmps eHilite,
                              teBmps eDownHi,
                              teBmps eStandardUp,
                              teBmps eStandardDown,
                              teBmps eDisabled,
                              wxWindowID id,
                              wxPoint placement,
                              bool processdownevents,
                              wxSize size)
{
   // wxMax to cater for case of image being bigger than the button.
   int xoff = wxMax( 0, (size.GetWidth() - theTheme.Image(eStandardUp).GetWidth())/2);
   int yoff = wxMax( 0, (size.GetHeight() - theTheme.Image(eStandardUp).GetHeight())/2);

   typedef std::unique_ptr<wxImage> wxImagePtr;
   wxImagePtr up2        (OverlayImage(eUp,     eStandardUp, xoff, yoff));
   wxImagePtr hilite2    (OverlayImage(eHilite, eStandardUp, xoff, yoff));
   wxImagePtr down2      (OverlayImage(eDown,   eStandardDown, xoff + 1, yoff + 1));
   wxImagePtr downHi2    (OverlayImage(eDownHi, eStandardDown, xoff + 1, yoff + 1));
   wxImagePtr disable2   (OverlayImage(eUp,     eDisabled, xoff, yoff));

   wxASSERT(parent); // to justify safenew
   AButton * button =
      safenew AButton(parent, id, placement, size, *up2, *hilite2, *down2, *downHi2,
            *disable2, processdownevents);

   return button;
}

//static
void ToolBar::MakeAlternateImages(AButton &button, int idx,
                                  teBmps eUp,
                                  teBmps eDown,
                                  teBmps eHilite,
                                  teBmps eDownHi,
                                  teBmps eStandardUp,
                                  teBmps eStandardDown,
                                  teBmps eDisabled,
                                  wxSize size)
{
   // wxMax to cater for case of image being bigger than the button.
   int xoff = wxMax( 0, (size.GetWidth() - theTheme.Image(eStandardUp).GetWidth())/2);
   int yoff = wxMax( 0, (size.GetHeight() - theTheme.Image(eStandardUp).GetHeight())/2);

   typedef std::unique_ptr<wxImage> wxImagePtr;
   wxImagePtr up        (OverlayImage(eUp,     eStandardUp, xoff, yoff));
   wxImagePtr hilite    (OverlayImage(eHilite, eStandardUp, xoff, yoff));
   wxImagePtr down      (OverlayImage(eDown,   eStandardDown, xoff + 1, yoff + 1));
   wxImagePtr downHi    (OverlayImage(eDownHi, eStandardDown, xoff + 1, yoff + 1));
   wxImagePtr disable   (OverlayImage(eUp,     eDisabled, xoff, yoff));

   button.SetAlternateImages(idx, *up, *hilite, *down, *downHi, *disable);
}

void ToolBar::SetButtonToolTip
(AudacityProject &theProject,
 AButton &button, const ComponentInterfaceSymbol commands[], size_t nCommands)
{
   TranslatableString result;
   const auto project = &theProject;
   const auto commandManager =
      project ? &CommandManager::Get( *project ) : nullptr;
   if (commandManager)
      result =
         commandManager->DescribeCommandsAndShortcuts(commands, nCommands);
   button.SetToolTip( result );
}

//
// This changes the state a button (from up to down or vice versa)
//
void ToolBar::SetButton( bool down, AButton * button )
{
   if( down )
   {
      button->PushDown();
   }
   else
   {
      button->PopUp();
   }
}

//
// Handle background erasure
//
void ToolBar::OnErase( wxEraseEvent & WXUNUSED(event) )
{
   // Ignore it to prevent flashing
}

//
// This draws the background of a toolbar
//
void ToolBar::OnPaint( wxPaintEvent & WXUNUSED(event) )
{
   wxPaintDC dc( this );

   // Themed background colour.
   dc.SetBackground( wxBrush( theTheme.Colour( clrMedium  ) ) );
   dc.Clear();

   Repaint( &dc );
}

void ToolBar::OnMouseEvents(wxMouseEvent &event)
{
   // Do this hack so scrubber can detect mouse drags anywhere
   event.ResumePropagation(wxEVENT_PROPAGATE_MAX);
   event.Skip();
}

int ToolBar::GetResizeGrabberWidth()
{
   return RWIDTH;
}

namespace {

RegisteredToolbarFactory::Functions &GetFunctions()
{
   static RegisteredToolbarFactory::Functions factories( ToolBarCount );
   return factories;
}

}

RegisteredToolbarFactory::RegisteredToolbarFactory(
   int id, const Function &function)
{
   wxASSERT( id >= 0 && id < ToolBarCount );
   GetFunctions()[ id ] = function;
}

auto RegisteredToolbarFactory::GetFactories() -> const Functions&
{
   return GetFunctions();
}
