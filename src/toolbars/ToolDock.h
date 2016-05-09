/**********************************************************************

  Audacity: A Digital Audio Editor

  ToolDock.h

  Dominic Mazzoni
  Shane T. Mueller
  Leland Lucius

**********************************************************************/

#ifndef __AUDACITY_TOOLDOCK__
#define __AUDACITY_TOOLDOCK__

#include <wx/defs.h>
#include <wx/panel.h>

#include "ToolBar.h"

class wxArrayPtrVoid;
class wxCommandEvent;
class wxEraseEvent;
class wxSizeEvent;
class wxPaintEvent;
class wxPoint;
class wxRect;
class wxWindow;

class GrabberEvent;
class ToolManager;

////////////////////////////////////////////////////////////
/// class ToolDock
////////////////////////////////////////////////////////////

//
// ToolDock IDs
//
enum
{
   NoDockID = 0,
   TopDockID,
   BotDockID,
   DockCount = 2
};

class ToolDock final : public wxPanel
{

 public:

   ToolDock( ToolManager *manager, wxWindow *parent, int dockid );
   ~ToolDock();

   bool AcceptsFocus() const override { return false; };

   void LayoutToolBars();
   void Expose( int type, bool show );
   int Find(ToolBar *bar) const;
   int GetOrder( ToolBar *bar );
   int GetBarCount();
   void Dock( ToolBar *bar, int ndx = -1 );
   void Undock( ToolBar *bar );
   int PositionBar( ToolBar *t, wxPoint & pos, wxRect & rect );

 protected:

   void OnErase( wxEraseEvent & event );
   void OnSize( wxSizeEvent & event );
   void OnPaint( wxPaintEvent & event );
   void OnGrabber( GrabberEvent & event );
   void OnMouseEvents(wxMouseEvent &event);

 private:

   void ReadConfig();
   void WriteConfig();

   int FlowLayout( int cnt,
                   wxRect boxen[],
                   wxRect ideal[],
                   int i,
                   int x,
                   int y,
                   int width,
                   int height );

   void Updated();

   int mTotalToolBarHeight;
   wxWindow *mParent;

   ToolManager *mManager;

   wxArrayPtrVoid mDockedBars;
   ToolBar *mBars[ ToolBarCount ];

 public:

   DECLARE_CLASS( ToolDock );
   DECLARE_EVENT_TABLE();
};

#endif
