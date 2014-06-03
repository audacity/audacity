/**********************************************************************

  Audacity: A Digital Audio Editor


  ToolsToolBar.h

  Dominic Mazzoni
  Shane T. Mueller
  Leland Lucius

**********************************************************************/

#ifndef __AUDACITY_TOOLS_TOOLBAR__
#define __AUDACITY_TOOLS_TOOLBAR__

#include <wx/defs.h>

#include "ToolBar.h"

#include "../Theme.h"

class wxCommandEvent;
class wxDC;
class wxGridSizer;
class wxImage;
class wxWindow;

class AButton;

// Code duplication warning: these apparently need to be in the
// same order as the enum in ToolsToolBar.cpp

enum {
   selectTool,
   envelopeTool,
   drawTool,
   zoomTool,
   slideTool,
   multiTool,
   numTools,

   firstTool = selectTool,
   lastTool = multiTool
};

class ToolsToolBar:public ToolBar {

 public:

   ToolsToolBar();
   virtual ~ToolsToolBar();

   void Create(wxWindow *parent);
   void UpdatePrefs();

   void OnTool(wxCommandEvent & evt);

   void SetCurrentTool(int tool, bool show);

   //These interrogate the state of the buttons or controls.
   int GetCurrentTool();
   bool IsDown(int tool);
   int GetDownTool();

   const wxChar * GetMessageForTool( int ToolNumber );

   void Populate();
   virtual void Repaint(wxDC * WXUNUSED(dc)) {};
   virtual void EnableDisableButtons() {};

 private:

   void RegenerateToolsTooltips();
   wxImage *MakeToolImage(wxImage *tool, wxImage *mask, int style);
   AButton *MakeTool(teBmps eTool, int id, const wxChar *label);

   AButton *mTool[numTools];
   wxGridSizer *mToolSizer;
   int mCurrentTool;

 public:

   DECLARE_CLASS(ToolsToolBar);
   DECLARE_EVENT_TABLE();
};

#endif

