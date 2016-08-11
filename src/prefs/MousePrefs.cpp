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

#include "../Audacity.h"
#include "../Experimental.h"

#include <wx/defs.h>
#include <wx/intl.h>
#include <wx/listctrl.h>

#include "../Prefs.h"
#include "../ShuttleGui.h"
#include "MousePrefs.h"

// The numbers of the columns of the mList.
enum
{
   BlankColumn,
   ToolColumn,
   ActionColumn,
   ButtonsColumn,
   CommentColumn
};

#if defined(__WXMAC__)
#define CTRL _("Command")
#else
#define CTRL _("Ctrl")
#endif

/// Constructor
MousePrefs::MousePrefs(wxWindow * parent)
:  PrefsPanel(parent, _("Mouse"))
{
   Populate();
}

MousePrefs::~MousePrefs()
{
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
}

/// Places controls on the panel and also exchanges data with them.
void MousePrefs::PopulateOrExchange(ShuttleGui & S)
{
   S.SetBorder(2);

   S.StartStatic(_("Mouse Bindings (default values, not configurable)"), 1);
   {
      mList = S.AddListControlReportMode();
   }
   S.EndStatic();
}

/// Creates the contents of mList
void MousePrefs::CreateList()
{
   //An empty first column is a workaround - under Win98 the first column
   //can't be right aligned.
   mList->InsertColumn(BlankColumn,   wxT(""),              wxLIST_FORMAT_LEFT);
   mList->InsertColumn(ToolColumn,    _("Tool"),            wxLIST_FORMAT_RIGHT);
   mList->InsertColumn(ActionColumn,  _("Command Action"),  wxLIST_FORMAT_RIGHT);
   mList->InsertColumn(ButtonsColumn, _("Buttons"),         wxLIST_FORMAT_LEFT);
   mList->InsertColumn(CommentColumn, _("Comments"),        wxLIST_FORMAT_LEFT);

   AddItem(_("Left-Click"),        _("Select"),   _("Set Selection Point"));
   AddItem(_("Left-Drag"),         _("Select"),   _("Set Selection Range"));
   AddItem(_("Shift-Left-Click"),  _("Select"),   _("Extend Selection Range"));
   AddItem(_("Left-Double-Click"), _("Select"),   _("Select Clip or Entire Track"));
#ifdef EXPERIMENTAL_SCRUBBING_SCROLL_WHEEL
   AddItem(_("Wheel-Rotate"),      _("Select"),   _("Change scrub speed"));
#endif

#ifdef EXPERIMENTAL_SPECTRAL_EDITING
   // JKC: Prompt is disabled for now.  It's a toggle rather than a drag modifier.
   // more like Snap-to than anything else.
   // Spectral selection
   // AddItem(_("ESC"),              _("Select"),    _("Toggle center snapping in spectrogram"));
#endif

   AddItem(_("Left-Click"),       _("Zoom"),      _("Zoom in on Point"));
   AddItem(_("Left-Drag"),        _("Zoom"),      _("Zoom in on a Range"), _("same as right-drag"));
   AddItem(_("Right-Click"),      _("Zoom"),      _("Zoom out one step"));
   AddItem(_("Right-Drag"),       _("Zoom"),      _("Zoom in on a Range"), _("same as left-drag"));
   AddItem(_("Shift-Drag"),       _("Zoom"),      _("Zoom out on a Range"));
   AddItem(_("Middle-Click"),     _("Zoom"),      _("Zoom default"));

   AddItem(_("Left-Drag"),        _("Time-Shift"),_("Time shift clip or move up/down between tracks"));
   AddItem(_("Shift-Left-Drag"),  _("Time-Shift"),_("Time shift all clips in track"));
   AddItem(CTRL + _("-Left-Drag"),_("Time-Shift"),_("Move clip up/down between tracks"));

   AddItem(_("Left-Drag"),
   /* i18n-hint: The envelope is a curve that controls the audio loudness.*/
      _("Envelope"),
      _("Change Amplification Envelope"));

   AddItem(_("Left-Click"),       _("Pencil"),    _("Change Sample"));
   AddItem(_("Alt-Left-Click"),   _("Pencil"),    _("Smooth at Sample"));
   AddItem(_("Left-Drag"),        _("Pencil"),    _("Change Several Samples"));
   AddItem(CTRL + _("-Left-Drag"),_("Pencil"),    _("Change ONE Sample only"));

   AddItem(_("Left-Click"),       _("Multi"),     _("Set Selection Point"), _("same as select tool"));
   AddItem(_("Left-Drag"),        _("Multi"),     _("Set Selection Range"), _("same as select tool"));
   AddItem(_("Right-Click"),      _("Multi"),     _("Zoom out one step"),   _("same as zoom tool"));
   AddItem(_("Right-Drag"),       _("Multi"),     _("Zoom in on a Range"),  _("same as zoom tool"));

#ifdef EXPERIMENTAL_SPECTRAL_EDITING
   // JKC: Prompt is disabled for now.  It's a toggle rather than a drag modifier.
   // more like Snap-to than anything else.
   // Spectral selection
   // AddItem(_("ESC"),              _("Select"),    _("Toggle center snapping in spectrogram"), _("same as select tool"));
#endif

   AddItem(_("Wheel-Rotate"),                _("Any"),   _("Scroll tracks up or down"));
   AddItem(_("Shift-Wheel-Rotate"),          _("Any"),   _("Scroll waveform"));
   AddItem(CTRL + _("-Wheel-Rotate"),        _("Any"),   _("Zoom waveform in or out"));
   AddItem(CTRL + _("-Shift-Wheel-Rotate"),  _("Any"),   _("Waveform (dB) range"));

   mList->SetColumnWidth(BlankColumn, 0);
   mList->SetColumnWidth(ToolColumn, wxLIST_AUTOSIZE);
   mList->SetColumnWidth(ActionColumn, wxLIST_AUTOSIZE);
   mList->SetColumnWidth(ButtonsColumn, wxLIST_AUTOSIZE);

   // Not sure if this extra column is a good idea or not.
   // Anyway, 5 pixels wide is wide enough that some people who are curious will drag it
   // wider to see what's there (the comments show that the duplication of functions
   // is for a reason, and not just random).
   mList->SetColumnWidth(CommentColumn, 5);
}

/// Adds an item to mList
void MousePrefs::AddItem(wxString const & buttons, wxString const & tool,
                         wxString const & action, wxString const & comment)
{
   int i = mList->GetItemCount();
   mList->InsertItem(i, wxT(""));
   mList->SetItem(i, ToolColumn, tool);
   mList->SetItem(i, ActionColumn, action);
   mList->SetItem(i, ButtonsColumn, buttons);

   // Add a space before the text to work around a minor bug in the
   // list control when showing narrow columns.
   mList->SetItem(i, CommentColumn, wxT(" ") + comment);
}


/// Update the preferences stored on disk.
/// Currently does nothing as Mouse Preferences don't change.
bool MousePrefs::Apply()
{
// Not yet required...
//   ShuttleGui S(this, eIsSavingToPrefs);
//   PopulateOrExchange(S);
   return true;
}

PrefsPanel *MousePrefsFactory::Create(wxWindow *parent)
{
   wxASSERT(parent); // to justify safenew
   return safenew MousePrefs(parent);
}
