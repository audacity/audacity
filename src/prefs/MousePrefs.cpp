/**********************************************************************

  Audacity: A Digital Audio Editor

  MousePrefs.cpp

  James Crook

********************************************************************//*!

\class MousePrefs
\brief A PrefsPanel that presents an interface for user to view the
   default bindings of mouse buttons to commands.

  April/2003: These are default bindings and are not yet configurable.
  They are provided to give information about what the bindings are.

  Configuration when available will be mostly used by power users
  who are unlikely to change the default bindings, but will add
  bindings (e.g. for cut, play, and their own nyquist filters)
  using currently unused combinations.

  Unlike key-bindings which are parameterless, mouse bindings
  provide parameters:

    - a single point for a click, and
    - a stream of points or a start and end point for a drag.

  If we allow a nyquist filter to be bound to the mouse, instead of
  being applied to the current selection it would be applied to the
  start and end points of the drag.

*//********************************************************************/


#include "MousePrefs.h"

#include <wx/defs.h>
#include <wx/listctrl.h>

#include "Prefs.h"
#include "ShuttleGui.h"

// The numbers of the columns of the mList.
enum
{
   ToolColumn,
   ActionColumn,
   ButtonsColumn,
   CommentColumn
};

#if defined(__WXMAC__)
#define CTRL XO("Command")
#else
#define CTRL XO("Ctrl")
#endif

/// Constructor
MousePrefs::MousePrefs(wxWindow * parent, wxWindowID winid)
:  PrefsPanel(parent, winid, XO("Mouse"))
{
   Populate();

   // See bug #2315 for discussion. This should be reviewed
   // and (possibly) removed after wx3.1.3.
   Bind(wxEVT_SHOW, &MousePrefs::OnShow, this);
}

MousePrefs::~MousePrefs()
{
}

ComponentInterfaceSymbol MousePrefs::GetSymbol() const
{
   return MOUSE_PREFS_PLUGIN_SYMBOL;
}

TranslatableString MousePrefs::GetDescription() const
{
   return XO("Preferences for Mouse");
}

ManualPageID MousePrefs::HelpPageName()
{
   return "Mouse_Preferences";
}

/// Creates the dialog and its contents.
void MousePrefs::Populate()
{
   //------------------------- Main section --------------------
   // Now construct the GUI itself.
   // Use 'eIsCreatingFromPrefs' so that the GUI is
   // initialised with values from gPrefs.
   ShuttleGui S(this, eIsCreatingFromPrefs);
   PopulateOrExchange(S);
   // ----------------------- End of main section --------------
   CreateList();
   if (mList->GetItemCount() > 0) {
      // set first item to be selected (and the focus when the
      // list first becomes the focus)
      mList->SetItemState(0, wxLIST_STATE_FOCUSED | wxLIST_STATE_SELECTED,
         wxLIST_STATE_FOCUSED | wxLIST_STATE_SELECTED);
   }
}

/// Places controls on the panel and also exchanges data with them.
void MousePrefs::PopulateOrExchange(ShuttleGui & S)
{
   S.SetBorder(2);

   S.StartStatic(XO("Mouse Bindings (default values, not configurable)"), 1);
   {
      mList = S.AddListControlReportMode();
   }
   S.EndStatic();
}

/// Creates the contents of mList
void MousePrefs::CreateList()
{
   //A dummy first column, which is then deleted, is a workaround - under Windows the first column
   //can't be right aligned.
   mList->InsertColumn(0,             wxT(""),              wxLIST_FORMAT_LEFT);
   mList->InsertColumn(ToolColumn + 1,    _("Tool"),            wxLIST_FORMAT_RIGHT);
   mList->InsertColumn(ActionColumn + 1,  _("Command Action"),  wxLIST_FORMAT_RIGHT);
   mList->InsertColumn(ButtonsColumn + 1, _("Buttons"),         wxLIST_FORMAT_LEFT);
   mList->InsertColumn(CommentColumn + 1, _("Comments"),        wxLIST_FORMAT_LEFT);
   mList->DeleteColumn(0);

   AddItem(XO("Left-Click"),        XO("Select"),   XO("Set Selection Point"));
   AddItem(XO("Left-Drag"),         XO("Select"),   XO("Set Selection Range"));
   AddItem(XO("Shift-Left-Click"),  XO("Select"),   XO("Extend Selection Range"));
   AddItem(XO("Left-Double-Click"), XO("Select"),   XO("Select Clip or Entire Track"));
#ifdef EXPERIMENTAL_SCRUBBING_SCROLL_WHEEL
   AddItem(XO("Wheel-Rotate"),      XO("Select"),   XO("Change scrub speed"));
#endif

#ifdef EXPERIMENTAL_SPECTRAL_EDITING
   // JKC: Prompt is disabled for now.  It's a toggle rather than a drag modifier.
   // more like Snap-to than anything else.
   // Spectral selection
   // AddItem(XO("ESC"),              XO("Select"),    XO("Toggle center snapping in spectrogram"));
#endif

   AddItem(XO("Left-Click"),       XO("Zoom"),      XO("Zoom in on Point"));
   AddItem(XO("Left-Drag"),        XO("Zoom"),      XO("Zoom in on a Range"), XO("same as right-drag"));
   AddItem(XO("Right-Click"),      XO("Zoom"),      XO("Zoom out one step"));
   AddItem(XO("Right-Drag"),       XO("Zoom"),      XO("Zoom in on a Range"), XO("same as left-drag"));
   AddItem(XO("Shift-Drag"),       XO("Zoom"),      XO("Zoom out on a Range"));
   AddItem(XO("Middle-Click"),     XO("Zoom"),      XO("Zoom default"));

   AddItem(XO("Left-Drag"),        XO("Time-Shift"),XO("Move clip left/right or between tracks"));
   AddItem(XO("Shift-Left-Drag"),  XO("Time-Shift"),XO("Move all clips in track left/right"));
   AddItem(CTRL + XO("-Left-Drag"),XO("Time-Shift"),XO("Move clip up/down between tracks"));

   AddItem(XO("Left-Drag"),
   /* i18n-hint: The envelope is a curve that controls the audio loudness.*/
      XO("Envelope"),
   /* i18n-hint: The envelope is a curve that controls the audio loudness.*/
      XO("Change Amplification Envelope"));

   AddItem(XO("Left-Click"),       XO("Pencil"),    XO("Change Sample"));
   AddItem(XO("Alt-Left-Click"),   XO("Pencil"),    XO("Smooth at Sample"));
   AddItem(XO("Left-Drag"),        XO("Pencil"),    XO("Change Several Samples"));
   AddItem(CTRL + XO("-Left-Drag"),XO("Pencil"),    XO("Change ONE Sample only"));

   AddItem(XO("Left-Click"),       XO("Multi"),     XO("Set Selection Point"), XO("same as select tool"));
   AddItem(XO("Left-Drag"),        XO("Multi"),     XO("Set Selection Range"), XO("same as select tool"));
   AddItem(XO("Right-Click"),      XO("Multi"),     XO("Zoom out one step"),   XO("same as zoom tool"));
   AddItem(XO("Right-Drag"),       XO("Multi"),     XO("Zoom in on a Range"),  XO("same as zoom tool"));

#ifdef EXPERIMENTAL_SPECTRAL_EDITING
   // JKC: Prompt is disabled for now.  It's a toggle rather than a drag modifier.
   // more like Snap-to than anything else.
   // Spectral selection
   // AddItem(XO("ESC"),              XO("Select"),    XO("Toggle center snapping in spectrogram"), XO("same as select tool"));
#endif

   AddItem(XO("Wheel-Rotate"),                XO("Any"),   XO("Scroll tracks up or down"));
   AddItem(XO("Shift-Wheel-Rotate"),          XO("Any"),   XO("Scroll waveform"));
   AddItem(CTRL + XO("-Wheel-Rotate"),        XO("Any"),   XO("Zoom waveform in or out"));
   AddItem(CTRL + XO("-Shift-Wheel-Rotate"),  XO("Any"),   XO("Vertical Scale Waveform (dB) range"));

   mList->SetColumnWidth(ToolColumn, wxLIST_AUTOSIZE);
   mList->SetColumnWidth(ActionColumn, wxLIST_AUTOSIZE);
   mList->SetColumnWidth(ButtonsColumn, wxLIST_AUTOSIZE);
   mList->SetColumnWidth(CommentColumn, wxLIST_AUTOSIZE);

// PRL commented out, didn't look good to me on Mac at least
/*
   // Not sure if this extra column is a good idea or not.
   // Anyway, 5 pixels wide is wide enough that some people who are curious will drag it
   // wider to see what's there (the comments show that the duplication of functions
   // is for a reason, and not just random).
   mList->SetColumnWidth(CommentColumn, 5);
   */
}

/// Adds an item to mList
void MousePrefs::AddItem(
   TranslatableString const & buttons, TranslatableString const & tool,
   TranslatableString const & action, TranslatableString const & comment)
{
   int i = mList->GetItemCount();
   mList->InsertItem(i, tool.Translation());
   mList->SetItem(i, ActionColumn, action.Translation());
   mList->SetItem(i, ButtonsColumn, buttons.Translation());

   // Add a space before the text to work around a minor bug in the
   // list control when showing narrow columns.
   mList->SetItem(i, CommentColumn, wxT(" ") + comment.Translation());
}

// See bug #2315 for discussion. This should be reviewed
// and (possibly) removed after wx3.1.3.
void MousePrefs::OnShow(wxShowEvent &event)
{
   event.Skip();

   if (event.IsShown())
   {
      mList->Refresh();
   }
}

/// Update the preferences stored on disk.
/// Currently does nothing as Mouse Preferences don't change.
bool MousePrefs::Commit()
{
// Not yet required...
//   ShuttleGui S(this, eIsSavingToPrefs);
//   PopulateOrExchange(S);
   return true;
}

namespace{
PrefsPanel::Registration sAttachment{ "Mouse",
   [](wxWindow *parent, wxWindowID winid, AudacityProject *)
   {
      wxASSERT(parent); // to justify safenew
      return safenew MousePrefs(parent, winid);
   }
};
}
