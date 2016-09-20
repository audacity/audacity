/**********************************************************************

  Audacity: A Digital Audio Editor

  Menus.cpp

  Dominic Mazzoni
  Brian Gunlogson
  et al.

*******************************************************************//**

\file Menus.cpp
\brief All AudacityProject functions that provide the menus.
Implements AudacityProjectCommandFunctor.

  This file implements the method that creates the menu bar, plus
  all of the methods that get called when you select an item
  from a menu.

  All of the menu bar handling is part of the class AudacityProject,
  but the event handlers for all of the menu items have been moved
  to Menus.h and Menus.cpp for clarity.

*//****************************************************************//**

\class AudacityProjectCommandFunctor
\brief AudacityProjectCommandFunctor, derived from CommandFunctor,
simplifies construction of menu items.

*//*******************************************************************/

#include "Audacity.h"
#include "Project.h"

#include <cfloat>
#include <iterator>
#include <limits>
#include <math.h>


#include <wx/defs.h>
#include <wx/docview.h>
#include <wx/msgdlg.h>
#include <wx/filedlg.h>
#include <wx/textfile.h>
#include <wx/textdlg.h>
#include <wx/progdlg.h>
#include <wx/scrolbar.h>
#include <wx/ffile.h>
#include <wx/statusbr.h>
#include <wx/utils.h>

#include "FreqWindow.h"
#include "effects/Contrast.h"
#include "TrackPanel.h"

#include "effects/EffectManager.h"

#include "AudacityApp.h"
#include "AudacityLogger.h"
#include "AudioIO.h"
#include "Dependencies.h"
#include "float_cast.h"
#include "LabelTrack.h"
#ifdef USE_MIDI
#include "import/ImportMIDI.h"
#endif // USE_MIDI
#include "import/ImportRaw.h"
#include "export/Export.h"
#include "export/ExportMultiple.h"
#include "prefs/PrefsDialog.h"
#include "prefs/PlaybackPrefs.h"
#include "ShuttleGui.h"
#include "HistoryWindow.h"
#include "LyricsWindow.h"
#include "MixerBoard.h"
#include "Internat.h"
#include "FileFormats.h"
#include "ModuleManager.h"
#include "PluginManager.h"
#include "Prefs.h"
#include "Printing.h"
#ifdef USE_MIDI
#include "NoteTrack.h"
#endif // USE_MIDI
#include "Tags.h"
#include "TimeTrack.h"
#include "Mix.h"
#include "AboutDialog.h"
#include "Benchmark.h"
#include "Screenshot.h"
#include "ondemand/ODManager.h"

#include "BatchProcessDialog.h"
#include "BatchCommands.h"
#include "prefs/BatchPrefs.h"

#include "toolbars/ToolManager.h"
#include "toolbars/ControlToolBar.h"
#include "toolbars/ToolsToolBar.h"
#include "toolbars/EditToolBar.h"
#include "toolbars/DeviceToolBar.h"
#include "toolbars/MixerToolBar.h"
#include "toolbars/TranscriptionToolBar.h"
#include "widgets/LinkingHtmlWindow.h"

#include "Experimental.h"
#include "PlatformCompatibility.h"
#include "FileNames.h"
#include "TimeDialog.h"
#include "TimerRecordDialog.h"
#include "SoundActivatedRecord.h"
#include "LabelDialog.h"

#include "FileDialog.h"
#include "SplashDialog.h"
#include "widgets/HelpSystem.h"
#include "DeviceManager.h"

#include "Snap.h"

#include "UndoManager.h"
#include "WaveTrack.h"

#if defined(EXPERIMENTAL_CRASH_REPORT)
#include <wx/debugrpt.h>
#endif

#ifdef EXPERIMENTAL_SCOREALIGN
#include "effects/ScoreAlignDialog.h"
#include "audioreader.h"
#include "scorealign.h"
#include "scorealign-glue.h"
#endif /* EXPERIMENTAL_SCOREALIGN */

#include "tracks/ui/Scrubbing.h"
#include "prefs/TracksPrefs.h"

#include "widgets/Meter.h"

enum {
   kAlignStartZero = 0,
   kAlignStartSelStart,
   kAlignStartSelEnd,
   kAlignEndSelStart,
   kAlignEndSelEnd,
   // The next two are only in one subMenu, so more easily handled at the end.
   kAlignEndToEnd,
   kAlignTogether
};

// Post Timer Recording Actions
// Ensure this matches the enum in TimerRecordDialog.cpp
enum {
   POST_TIMER_RECORD_STOPPED = -3,
   POST_TIMER_RECORD_CANCEL_WAIT,
   POST_TIMER_RECORD_CANCEL,
   POST_TIMER_RECORD_NOTHING,
   POST_TIMER_RECORD_CLOSE,
   POST_TIMER_RECORD_RESTART,
   POST_TIMER_RECORD_SHUTDOWN
};

#include "commands/CommandFunctors.h"
//
// Effects menu arrays
//
WX_DEFINE_ARRAY_PTR(const PluginDescriptor *, EffectPlugs);
static int SortEffectsByName(const PluginDescriptor **a, const PluginDescriptor **b)
{
   wxString akey = (*a)->GetName();
   wxString bkey = (*b)->GetName();

   akey += (*a)->GetPath();
   bkey += (*b)->GetPath();

   return akey.CmpNoCase(bkey);
}

static int SortEffectsByPublisher(const PluginDescriptor **a, const PluginDescriptor **b)
{
   wxString akey = (*a)->GetVendor();
   wxString bkey = (*b)->GetVendor();

   if (akey.IsEmpty())
   {
      akey = _("Uncategorized");
   }
   if (bkey.IsEmpty())
   {
      bkey = _("Uncategorized");
   }

   akey += (*a)->GetName();
   bkey += (*b)->GetName();

   akey += (*a)->GetPath();
   bkey += (*b)->GetPath();

   return akey.CmpNoCase(bkey);
}

static int SortEffectsByPublisherAndName(const PluginDescriptor **a, const PluginDescriptor **b)
{
   wxString akey = (*a)->GetVendor();
   wxString bkey = (*b)->GetVendor();

   if ((*a)->IsEffectDefault())
   {
      akey = wxEmptyString;
   }
   if ((*b)->IsEffectDefault())
   {
      bkey = wxEmptyString;
   }

   akey += (*a)->GetName();
   bkey += (*b)->GetName();

   akey += (*a)->GetPath();
   bkey += (*b)->GetPath();

   return akey.CmpNoCase(bkey);
}

static int SortEffectsByTypeAndName(const PluginDescriptor **a, const PluginDescriptor **b)
{
   wxString akey = (*a)->GetEffectFamily();
   wxString bkey = (*b)->GetEffectFamily();

   if (akey.IsEmpty())
   {
      akey = _("Uncategorized");
   }
   if (bkey.IsEmpty())
   {
      bkey = _("Uncategorized");
   }

   if ((*a)->IsEffectDefault())
   {
      akey = wxEmptyString;
   }
   if ((*b)->IsEffectDefault())
   {
      bkey = wxEmptyString;
   }

   akey += (*a)->GetName();
   bkey += (*b)->GetName();

   akey += (*a)->GetPath();
   bkey += (*b)->GetPath();

   return akey.CmpNoCase(bkey);
}

static int SortEffectsByType(const PluginDescriptor **a, const PluginDescriptor **b)
{
   wxString akey = (*a)->GetEffectFamily();
   wxString bkey = (*b)->GetEffectFamily();

   if (akey.IsEmpty())
   {
      akey = _("Uncategorized");
   }
   if (bkey.IsEmpty())
   {
      bkey = _("Uncategorized");
   }

   akey += (*a)->GetName();
   bkey += (*b)->GetName();

   akey += (*a)->GetPath();
   bkey += (*b)->GetPath();

   return akey.CmpNoCase(bkey);
}

/// CreateMenusAndCommands builds the menus, and also rebuilds them after
/// changes in configured preferences - for example changes in key-bindings
/// affect the short-cut key legend that appears beside each command,

void AudacityProject::CreateMenusAndCommands()
{
   CommandManager *c = &mCommandManager;
   wxArrayString names;
   wxArrayInt indices;

   {
      auto menubar = c->AddMenuBar(wxT("appmenu"));
      wxASSERT(menubar);

      /////////////////////////////////////////////////////////////////////////////
      // File menu
      /////////////////////////////////////////////////////////////////////////////

      c->BeginMenu(_("&File"));
      c->SetDefaultFlags(AudioIONotBusyFlag, AudioIONotBusyFlag);

      /*i18n-hint: "New" is an action (verb) to create a NEW project*/
      c->AddItem(wxT("New"), _("&New"), FN(OnNew), wxT("Ctrl+N"),
         AudioIONotBusyFlag,
         AudioIONotBusyFlag);

      /*i18n-hint: (verb)*/
      c->AddItem(wxT("Open"), _("&Open..."), FN(OnOpen), wxT("Ctrl+O"),
         AudioIONotBusyFlag,
         AudioIONotBusyFlag);

      /////////////////////////////////////////////////////////////////////////////

      CreateRecentFilesMenu(c);

      /////////////////////////////////////////////////////////////////////////////

      c->AddSeparator();

      c->AddItem(wxT("Close"), _("&Close"), FN(OnClose), wxT("Ctrl+W"));

      c->AddItem(wxT("Save"), _("&Save Project"), FN(OnSave), wxT("Ctrl+S"),
         AudioIONotBusyFlag | UnsavedChangesFlag,
         AudioIONotBusyFlag | UnsavedChangesFlag);
      c->AddItem(wxT("SaveAs"), _("Save Project &As..."), FN(OnSaveAs));
#ifdef USE_LIBVORBIS
      c->AddItem(wxT("SaveCompressed"), _("Sa&ve Compressed Copy of Project..."), FN(OnSaveCompressed));
#endif

      c->AddItem(wxT("CheckDeps"), _("Chec&k Dependencies..."), FN(OnCheckDependencies));

      c->AddSeparator();

      c->AddItem(wxT("EditMetaData"), _("Edit Me&tadata Tags..."), FN(OnEditMetadata));

      c->AddSeparator();

      /////////////////////////////////////////////////////////////////////////////

      c->BeginSubMenu(_("&Import"));

      c->AddItem(wxT("ImportAudio"), _("&Audio..."), FN(OnImport), wxT("Ctrl+Shift+I"));
      c->AddItem(wxT("ImportLabels"), _("&Labels..."), FN(OnImportLabels));
#ifdef USE_MIDI
      c->AddItem(wxT("ImportMIDI"), _("&MIDI..."), FN(OnImportMIDI));
#endif // USE_MIDI
      c->AddItem(wxT("ImportRaw"), _("&Raw Data..."), FN(OnImportRaw));

      c->EndSubMenu();

      c->AddSeparator();

      /////////////////////////////////////////////////////////////////////////////

      // Enable Export audio commands only when there are audio tracks.
      c->AddItem(wxT("Export"), _("&Export Audio..."), FN(OnExport), wxT("Ctrl+Shift+E"),
         AudioIONotBusyFlag | WaveTracksExistFlag,
         AudioIONotBusyFlag | WaveTracksExistFlag);

      // Enable Export Selection commands only when there's a selection.
      c->AddItem(wxT("ExportSel"), _("Expo&rt Selected Audio..."), FN(OnExportSelection),
         AudioIONotBusyFlag | TimeSelectedFlag | WaveTracksSelectedFlag,
         AudioIONotBusyFlag | TimeSelectedFlag | WaveTracksSelectedFlag);

      c->AddItem(wxT("ExportLabels"), _("Export &Labels..."), FN(OnExportLabels),
         AudioIONotBusyFlag | LabelTracksExistFlag,
         AudioIONotBusyFlag | LabelTracksExistFlag);
      // Enable Export audio commands only when there are audio tracks.
      c->AddItem(wxT("ExportMultiple"), _("Export &Multiple..."), FN(OnExportMultiple), wxT("Ctrl+Shift+L"),
         AudioIONotBusyFlag | WaveTracksExistFlag,
         AudioIONotBusyFlag | WaveTracksExistFlag);
#if defined(USE_MIDI)
      c->AddItem(wxT("ExportMIDI"), _("Export MI&DI..."), FN(OnExportMIDI),
         AudioIONotBusyFlag | NoteTracksSelectedFlag,
         AudioIONotBusyFlag | NoteTracksSelectedFlag);
#endif

      c->AddSeparator();
      c->AddItem(wxT("ApplyChain"), _("Appl&y Chain..."), FN(OnApplyChain),
         AudioIONotBusyFlag,
         AudioIONotBusyFlag);
      c->AddItem(wxT("EditChains"), _("Edit C&hains..."), FN(OnEditChains));

      c->AddSeparator();

      c->AddItem(wxT("PageSetup"), _("Pa&ge Setup..."), FN(OnPageSetup),
         AudioIONotBusyFlag | TracksExistFlag,
         AudioIONotBusyFlag | TracksExistFlag);
      /* i18n-hint: (verb) It's item on a menu. */
      c->AddItem(wxT("Print"), _("&Print..."), FN(OnPrint),
         AudioIONotBusyFlag | TracksExistFlag,
         AudioIONotBusyFlag | TracksExistFlag);

      c->AddSeparator();

      // On the Mac, the Exit item doesn't actually go here...wxMac will pull it out
      // and put it in the Audacity menu for us based on its ID.
      /* i18n-hint: (verb) It's item on a menu. */
      c->AddItem(wxT("Exit"), _("E&xit"), FN(OnExit), wxT("Ctrl+Q"),
         AlwaysEnabledFlag,
         AlwaysEnabledFlag);

      c->EndMenu();

      /////////////////////////////////////////////////////////////////////////////
      // Edit Menu
      /////////////////////////////////////////////////////////////////////////////

      c->BeginMenu(_("&Edit"));

      c->SetDefaultFlags(AudioIONotBusyFlag | TimeSelectedFlag | TracksSelectedFlag,
         AudioIONotBusyFlag | TimeSelectedFlag | TracksSelectedFlag);

      c->AddItem(wxT("Undo"), _("&Undo"), FN(OnUndo), wxT("Ctrl+Z"),
         AudioIONotBusyFlag | UndoAvailableFlag,
         AudioIONotBusyFlag | UndoAvailableFlag);

      // The default shortcut key for Redo is different on different platforms.
      wxString key =
#ifdef __WXMSW__
         wxT("Ctrl+Y");
#else
         wxT("Ctrl+Shift+Z");
#endif

      c->AddItem(wxT("Redo"), _("&Redo"), FN(OnRedo), key,
         AudioIONotBusyFlag | RedoAvailableFlag,
         AudioIONotBusyFlag | RedoAvailableFlag);

      ModifyUndoMenuItems();

      c->AddSeparator();

      // Basic Edit coomands
      /* i18n-hint: (verb)*/
      c->AddItem(wxT("Cut"), _("Cu&t"), FN(OnCut), wxT("Ctrl+X"),
         AudioIONotBusyFlag | CutCopyAvailableFlag,
         AudioIONotBusyFlag | CutCopyAvailableFlag);
      c->AddItem(wxT("Delete"), _("&Delete"), FN(OnDelete), wxT("Ctrl+K"));
      /* i18n-hint: (verb)*/
      c->AddItem(wxT("Copy"), _("&Copy"), FN(OnCopy), wxT("Ctrl+C"),
         AudioIONotBusyFlag | CutCopyAvailableFlag,
         AudioIONotBusyFlag | CutCopyAvailableFlag);
      /* i18n-hint: (verb)*/
      c->AddItem(wxT("Paste"), _("&Paste"), FN(OnPaste), wxT("Ctrl+V"),
         AudioIONotBusyFlag | ClipboardFlag,
         AudioIONotBusyFlag | ClipboardFlag);
      /* i18n-hint: (verb)*/
      c->AddItem(wxT("Duplicate"), _("Duplic&ate"), FN(OnDuplicate), wxT("Ctrl+D"));

      c->AddSeparator();

      c->BeginSubMenu(_("R&emove Special"));
      /* i18n-hint: (verb) Do a special kind of cut*/
      c->AddItem(wxT("SplitCut"), _("Spl&it Cut"), FN(OnSplitCut), wxT("Ctrl+Alt+X"));
      /* i18n-hint: (verb) Do a special kind of DELETE*/
      c->AddItem(wxT("SplitDelete"), _("Split D&elete"), FN(OnSplitDelete), wxT("Ctrl+Alt+K"));

      c->AddSeparator();

      /* i18n-hint: (verb)*/
      c->AddItem(wxT("Silence"), _("Silence Audi&o"), FN(OnSilence), wxT("Ctrl+L"),
         AudioIONotBusyFlag | TimeSelectedFlag | WaveTracksSelectedFlag,
         AudioIONotBusyFlag | TimeSelectedFlag | WaveTracksSelectedFlag);
      /* i18n-hint: (verb)*/
      c->AddItem(wxT("Trim"), _("Tri&m Audio"), FN(OnTrim), wxT("Ctrl+T"),
         AudioIONotBusyFlag | TimeSelectedFlag | WaveTracksSelectedFlag,
         AudioIONotBusyFlag | TimeSelectedFlag | WaveTracksSelectedFlag);
      c->EndSubMenu();

      c->AddItem(wxT("PasteNewLabel"), _("Paste Te&xt to New Label"), FN(OnPasteNewLabel), wxT("Ctrl+Alt+V"),
         AudioIONotBusyFlag, AudioIONotBusyFlag);


      c->AddSeparator();

      /////////////////////////////////////////////////////////////////////////////

      c->BeginSubMenu(_("Clip B&oundaries"));
      /* i18n-hint: (verb) It's an item on a menu. */
      c->AddItem(wxT("Split"), _("Sp&lit"), FN(OnSplit), wxT("Ctrl+I"),
         AudioIONotBusyFlag | WaveTracksSelectedFlag,
         AudioIONotBusyFlag | WaveTracksSelectedFlag);
      c->AddItem(wxT("SplitNew"), _("Split Ne&w"), FN(OnSplitNew), wxT("Ctrl+Alt+I"),
         AudioIONotBusyFlag | TimeSelectedFlag | WaveTracksSelectedFlag,
         AudioIONotBusyFlag | TimeSelectedFlag | WaveTracksSelectedFlag);
      c->AddSeparator();
      /* i18n-hint: (verb)*/
      c->AddItem(wxT("Join"), _("&Join"), FN(OnJoin), wxT("Ctrl+J"));
      c->AddItem(wxT("Disjoin"), _("Detac&h at Silences"), FN(OnDisjoin), wxT("Ctrl+Alt+J"));
      c->EndSubMenu();

      c->AddSeparator();

      /////////////////////////////////////////////////////////////////////////////

      c->BeginSubMenu(_("La&beled Audio"));
      c->SetDefaultFlags(AudioIONotBusyFlag | LabelsSelectedFlag | WaveTracksExistFlag | TimeSelectedFlag,
         AudioIONotBusyFlag | LabelsSelectedFlag | WaveTracksExistFlag | TimeSelectedFlag);

      /* i18n-hint: (verb)*/
      c->AddItem(wxT("CutLabels"), _("&Cut"), FN(OnCutLabels), wxT("Alt+X"),
         AudioIONotBusyFlag | LabelsSelectedFlag | WaveTracksExistFlag | TimeSelectedFlag | IsNotSyncLockedFlag,
         AudioIONotBusyFlag | LabelsSelectedFlag | WaveTracksExistFlag | TimeSelectedFlag | IsNotSyncLockedFlag);
      c->AddItem(wxT("DeleteLabels"), _("&Delete"), FN(OnDeleteLabels), wxT("Alt+K"),
         AudioIONotBusyFlag | LabelsSelectedFlag | WaveTracksExistFlag | TimeSelectedFlag | IsNotSyncLockedFlag,
         AudioIONotBusyFlag | LabelsSelectedFlag | WaveTracksExistFlag | TimeSelectedFlag | IsNotSyncLockedFlag);

      c->AddSeparator();

      /* i18n-hint: (verb) A special way to cut out a piece of audio*/
      c->AddItem(wxT("SplitCutLabels"), _("&Split Cut"), FN(OnSplitCutLabels), wxT("Alt+Shift+X"));
      c->AddItem(wxT("SplitDeleteLabels"), _("Sp&lit Delete"), FN(OnSplitDeleteLabels), wxT("Alt+Shift+K"));

      c->AddSeparator();


      c->AddItem(wxT("SilenceLabels"), _("Silence &Audio"), FN(OnSilenceLabels), wxT("Alt+L"));
      /* i18n-hint: (verb)*/
      c->AddItem(wxT("CopyLabels"), _("Co&py"), FN(OnCopyLabels), wxT("Alt+Shift+C"));

      c->AddSeparator();

      /* i18n-hint: (verb)*/
      c->AddItem(wxT("SplitLabels"), _("Spli&t"), FN(OnSplitLabels), wxT("Alt+I"),
         AudioIONotBusyFlag | LabelsSelectedFlag | WaveTracksExistFlag,
         AudioIONotBusyFlag | LabelsSelectedFlag | WaveTracksExistFlag);
      /* i18n-hint: (verb)*/
      c->AddItem(wxT("JoinLabels"), _("&Join"), FN(OnJoinLabels), wxT("Alt+J"));
      c->AddItem(wxT("DisjoinLabels"), _("Detac&h at Silences"), FN(OnDisjoinLabels), wxT("Alt+Shift+J"));

      c->EndSubMenu();

      /////////////////////////////////////////////////////////////////////////////

      /* i18n-hint: (verb) It's an item on a menu. */
      c->BeginSubMenu(_("&Select"));
      c->SetDefaultFlags(TracksExistFlag, TracksExistFlag);

      c->AddItem(wxT("SelectAll"), _("&All"), FN(OnSelectAll), wxT("Ctrl+A"));
      c->AddItem(wxT("SelectNone"), _("&None"), FN(OnSelectNone), wxT("Ctrl+Shift+A"));

#ifdef EXPERIMENTAL_SPECTRAL_EDITING
      c->BeginSubMenu(_("S&pectral"));
      c->AddItem(wxT("ToggleSpectralSelection"), _("To&ggle spectral selection"), FN(OnToggleSpectralSelection), wxT("Q"));
      c->AddItem(wxT("NextHigherPeakFrequency"), _("Next Higher Peak Frequency"), FN(OnNextHigherPeakFrequency));
      c->AddItem(wxT("NextLowerPeakFrequency"), _("Next Lower Peak Frequency"), FN(OnNextLowerPeakFrequency));
      c->EndSubMenu();
#endif

      c->AddItem(wxT("SetLeftSelection"), _("&Left at Playback Position"), FN(OnSetLeftSelection), wxT("["));
      c->AddItem(wxT("SetRightSelection"), _("&Right at Playback Position"), FN(OnSetRightSelection), wxT("]"));

      c->SetDefaultFlags(TracksSelectedFlag, TracksSelectedFlag);

      c->AddItem(wxT("SelStartCursor"), _("Track &Start to Cursor"), FN(OnSelectStartCursor), wxT("Shift+J"));
      c->AddItem(wxT("SelCursorEnd"), _("Cursor to Track &End"), FN(OnSelectCursorEnd), wxT("Shift+K"));

      c->AddSeparator();

      c->AddItem(wxT("SelAllTracks"), _("In All &Tracks"), FN(OnSelectAllTracks),
         wxT("Ctrl+Shift+K"),
         TracksExistFlag, TracksExistFlag);

#ifdef EXPERIMENTAL_SYNC_LOCK
      c->AddItem(wxT("SelSyncLockTracks"), _("In All S&ync-Locked Tracks"),
         FN(OnSelectSyncLockSel), wxT("Ctrl+Shift+Y"),
         TracksSelectedFlag | IsSyncLockedFlag,
         TracksSelectedFlag | IsSyncLockedFlag);
#endif

      c->EndSubMenu();

      /////////////////////////////////////////////////////////////////////////////

      c->AddItem(wxT("ZeroCross"), _("Find &Zero Crossings"), FN(OnZeroCrossing), wxT("Z"));

      /////////////////////////////////////////////////////////////////////////////

      c->BeginSubMenu(_("Mo&ve Cursor"));

      c->AddItem(wxT("CursSelStart"), _("to Selection Star&t"), FN(OnCursorSelStart));
      c->AddItem(wxT("CursSelEnd"), _("to Selection En&d"), FN(OnCursorSelEnd));

      c->AddItem(wxT("CursTrackStart"), _("to Track &Start"), FN(OnCursorTrackStart), wxT("J"));
      c->AddItem(wxT("CursTrackEnd"), _("to Track &End"), FN(OnCursorTrackEnd), wxT("K"));

      c->EndSubMenu();

      /////////////////////////////////////////////////////////////////////////////

      c->AddSeparator();

      c->AddItem(wxT("SelSave"), _("Re&gion Save"), FN(OnSelectionSave),
         WaveTracksSelectedFlag,
         WaveTracksSelectedFlag);
      c->AddItem(wxT("SelRestore"), _("Regio&n Restore"), FN(OnSelectionRestore),
         TracksExistFlag,
         TracksExistFlag);

      c->AddSeparator();

      /////////////////////////////////////////////////////////////////////////////

      c->BeginSubMenu(_("Pla&y Region"));

      c->AddItem(wxT("LockPlayRegion"), _("&Lock"), FN(OnLockPlayRegion),
         PlayRegionNotLockedFlag,
         PlayRegionNotLockedFlag);
      c->AddItem(wxT("UnlockPlayRegion"), _("&Unlock"), FN(OnUnlockPlayRegion),
         PlayRegionLockedFlag,
         PlayRegionLockedFlag);

      c->EndSubMenu();

      /////////////////////////////////////////////////////////////////////////////

#ifndef __WXMAC__
      c->AddSeparator();
#endif

      // The default shortcut key for Preferences is different on different platforms.
      key =
#ifdef __WXMAC__
         wxT("Ctrl+,");
#else
         wxT("Ctrl+P");
#endif

      c->AddItem(wxT("Preferences"), _("Pre&ferences..."), FN(OnPreferences), key,
         AudioIONotBusyFlag,
         AudioIONotBusyFlag);

      c->EndMenu();

      /////////////////////////////////////////////////////////////////////////////
      // View Menu
      /////////////////////////////////////////////////////////////////////////////

      c->BeginMenu(_("&View"));
      c->SetDefaultFlags(TracksExistFlag, TracksExistFlag);

      c->AddItem(wxT("ZoomIn"), _("Zoom &In"), FN(OnZoomIn), wxT("Ctrl+1"),
         ZoomInAvailableFlag,
         ZoomInAvailableFlag);
      c->AddItem(wxT("ZoomNormal"), _("Zoom &Normal"), FN(OnZoomNormal), wxT("Ctrl+2"));
      c->AddItem(wxT("ZoomOut"), _("Zoom &Out"), FN(OnZoomOut), wxT("Ctrl+3"),
         ZoomOutAvailableFlag,
         ZoomOutAvailableFlag);
      c->AddItem(wxT("ZoomSel"), _("&Zoom to Selection"), FN(OnZoomSel), wxT("Ctrl+E"), TimeSelectedFlag, TimeSelectedFlag);

      c->AddSeparator();
      c->AddItem(wxT("FitInWindow"), _("&Fit in Window"), FN(OnZoomFit), wxT("Ctrl+F"));
      c->AddItem(wxT("FitV"), _("Fit &Vertically"), FN(OnZoomFitV), wxT("Ctrl+Shift+F"));

      c->AddSeparator();
      c->AddItem(wxT("GoSelStart"), _("Go to Selection Sta&rt"), FN(OnGoSelStart), wxT("Ctrl+["), TimeSelectedFlag, TimeSelectedFlag);
      c->AddItem(wxT("GoSelEnd"), _("Go to Selection En&d"), FN(OnGoSelEnd), wxT("Ctrl+]"), TimeSelectedFlag, TimeSelectedFlag);

      c->AddSeparator();
      c->AddItem(wxT("CollapseAllTracks"), _("&Collapse All Tracks"), FN(OnCollapseAllTracks), wxT("Ctrl+Shift+C"));
      c->AddItem(wxT("ExpandAllTracks"), _("E&xpand All Tracks"), FN(OnExpandAllTracks), wxT("Ctrl+Shift+X"));

      c->AddSeparator();
      c->AddCheck(wxT("ShowClipping"), _("&Show Clipping"), FN(OnShowClipping),
         gPrefs->Read(wxT("/GUI/ShowClipping"), 0L), AlwaysEnabledFlag, AlwaysEnabledFlag);

      c->AddSeparator();

      // History window should be available either for UndoAvailableFlag or RedoAvailableFlag,
      // but we can't make the AddItem flags and mask have both, because they'd both have to be true for the
      // command to be enabled.
      //    If user has Undone the entire stack, RedoAvailableFlag is on but UndoAvailableFlag is off.
      //    If user has done things but not Undone anything, RedoAvailableFlag is off but UndoAvailableFlag is on.
      // So in either of those cases, (AudioIONotBusyFlag | UndoAvailableFlag | RedoAvailableFlag) mask
      // would fail.
      // The only way to fix this in the current architecture is to hack in special cases for RedoAvailableFlag
      // in AudacityProject::UpdateMenus() (ugly) and CommandManager::HandleCommandEntry() (*really* ugly --
      // shouldn't know about particular command names and flags).
      // Here's the hack that would be necessary in AudacityProject::UpdateMenus(), if somebody decides to do it:
      //    // Because EnableUsingFlags requires all the flag bits match the corresponding mask bits,
      //    // "UndoHistory" specifies only AudioIONotBusyFlag | UndoAvailableFlag, because that
      //    // covers the majority of cases where it should be enabled.
      //    // If history is not empty but we've Undone the whole stack, we also want to enable,
      //    // to show the Redo's on stack.
      //    // "UndoHistory" might already be enabled, but add this check for RedoAvailableFlag.
      //    if (flags & RedoAvailableFlag)
      //       mCommandManager.Enable(wxT("UndoHistory"), true);
      // So for now, enable the command regardless of stack. It will just show empty sometimes.
      // FOR REDESIGN, clearly there are some limitations with the flags/mask bitmaps.

      /* i18n-hint: Clicking this menu item shows the various editing steps that have been taken.*/
      c->AddItem(wxT("UndoHistory"), _("&History..."), FN(OnHistory),
         AudioIONotBusyFlag,
         AudioIONotBusyFlag);

      c->AddItem(wxT("Karaoke"), _("&Karaoke..."), FN(OnKaraoke), LabelTracksExistFlag, LabelTracksExistFlag);
      c->AddItem(wxT("MixerBoard"), _("&Mixer Board..."), FN(OnMixerBoard), WaveTracksExistFlag, WaveTracksExistFlag);

      c->AddSeparator();

      /////////////////////////////////////////////////////////////////////////////

      c->BeginSubMenu(_("&Toolbars"));

      /* i18n-hint: Clicking this menu item shows the toolbar that manages devices*/
      c->AddCheck(wxT("ShowDeviceTB"), _("&Device Toolbar"), FN(OnShowDeviceToolBar), 0, AlwaysEnabledFlag, AlwaysEnabledFlag);
      /* i18n-hint: Clicking this menu item shows the toolbar for editing*/
      c->AddCheck(wxT("ShowEditTB"), _("&Edit Toolbar"), FN(OnShowEditToolBar), 0, AlwaysEnabledFlag, AlwaysEnabledFlag);
      /* i18n-hint: Clicking this menu item shows the toolbar which has sound level meters*/
      c->AddCheck(wxT("ShowMeterTB"), _("Co&mbined Meter Toolbar"), FN(OnShowMeterToolBar), 0, AlwaysEnabledFlag, AlwaysEnabledFlag);
      /* i18n-hint: Clicking this menu item shows the toolbar with the recording level meters*/
      c->AddCheck(wxT("ShowRecordMeterTB"), _("&Recording Meter Toolbar"), FN(OnShowRecordMeterToolBar), 0, AlwaysEnabledFlag, AlwaysEnabledFlag);
      /* i18n-hint: Clicking this menu item shows the toolbar with the playback level meter*/
      c->AddCheck(wxT("ShowPlayMeterTB"), _("&Playback Meter Toolbar"), FN(OnShowPlayMeterToolBar), 0, AlwaysEnabledFlag, AlwaysEnabledFlag);
      /* i18n-hint: Clicking this menu item shows the toolbar with the mixer*/
      c->AddCheck(wxT("ShowMixerTB"), _("Mi&xer Toolbar"), FN(OnShowMixerToolBar), 0, AlwaysEnabledFlag, AlwaysEnabledFlag);
      /* i18n-hint: Clicking this menu item shows the toolbar for selecting a time range of audio*/
      c->AddCheck(wxT("ShowSelectionTB"), _("&Selection Toolbar"), FN(OnShowSelectionToolBar), 0, AlwaysEnabledFlag, AlwaysEnabledFlag);
#ifdef EXPERIMENTAL_SPECTRAL_EDITING
      /* i18n-hint: Clicking this menu item shows the toolbar for selecting a frequency range of audio*/
      c->AddCheck(wxT("ShowSpectralSelectionTB"), _("Spe&ctral Selection Toolbar"), FN(OnShowSpectralSelectionToolBar), 0, AlwaysEnabledFlag, AlwaysEnabledFlag);
#endif
      /* i18n-hint: Clicking this menu item shows a toolbar that has some tools in it*/
      c->AddCheck(wxT("ShowToolsTB"), _("T&ools Toolbar"), FN(OnShowToolsToolBar), 0, AlwaysEnabledFlag, AlwaysEnabledFlag);
      /* i18n-hint: Clicking this menu item shows the toolbar for transcription (currently just vary play speed)*/
      c->AddCheck(wxT("ShowTranscriptionTB"), _("Tra&nscription Toolbar"), FN(OnShowTranscriptionToolBar), 0, AlwaysEnabledFlag, AlwaysEnabledFlag);
      /* i18n-hint: Clicking this menu item shows the toolbar with the big buttons on it (play record etc)*/
      c->AddCheck(wxT("ShowTransportTB"), _("&Transport Toolbar"), FN(OnShowTransportToolBar), 0, AlwaysEnabledFlag, AlwaysEnabledFlag);
      /* i18n-hint: Clicking this menu item shows the toolbar that enables Scrub or Seek playback and Scrub Ruler*/
      c->AddCheck(wxT("ShowScrubbingTB"), _("Scru&b Toolbar"), FN(OnShowScrubbingToolBar), 0, AlwaysEnabledFlag, AlwaysEnabledFlag);

      c->AddSeparator();

      /* i18n-hint: (verb)*/
      c->AddItem(wxT("ResetToolbars"), _("Reset Toolb&ars"), FN(OnResetToolBars), 0, AlwaysEnabledFlag, AlwaysEnabledFlag);

      c->EndSubMenu();

      c->EndMenu();

      /////////////////////////////////////////////////////////////////////////////
      // Transport Menu
      /////////////////////////////////////////////////////////////////////////////

      /*i18n-hint: 'Transport' is the name given to the set of controls that
      play, record, pause etc. */
      c->BeginMenu(_("T&ransport"));
      c->SetDefaultFlags(CanStopAudioStreamFlag, CanStopAudioStreamFlag);

      /* i18n-hint: (verb) Start or Stop audio playback*/
      c->AddItem(wxT("PlayStop"), _("Pl&ay/Stop"), FN(OnPlayStop), wxT("Space"));
      c->AddItem(wxT("PlayStopSelect"), _("Play/Stop and &Set Cursor"), FN(OnPlayStopSelect), wxT("Shift+A"));
      c->AddItem(wxT("PlayLooped"), _("&Loop Play"), FN(OnPlayLooped), wxT("Shift+Space"),
         WaveTracksExistFlag | AudioIONotBusyFlag | CanStopAudioStreamFlag,
         WaveTracksExistFlag | AudioIONotBusyFlag | CanStopAudioStreamFlag);

      // Scrubbing sub-menu
      GetScrubber().AddMenuItems();

      c->AddItem(wxT("Pause"), _("&Pause"), FN(OnPause), wxT("P"));
      c->AddItem(wxT("SkipStart"), _("S&kip to Start"), FN(OnSkipStart), wxT("Home"),
                 AudioIONotBusyFlag, AudioIONotBusyFlag);
      c->AddItem(wxT("SkipEnd"), _("Skip to E&nd"), FN(OnSkipEnd), wxT("End"),
                 WaveTracksExistFlag | AudioIONotBusyFlag,
                 WaveTracksExistFlag | AudioIONotBusyFlag);

      c->AddSeparator();

      c->SetDefaultFlags(AudioIONotBusyFlag | CanStopAudioStreamFlag,
                         AudioIONotBusyFlag | CanStopAudioStreamFlag);
      /* i18n-hint: (verb)*/
      c->AddItem(wxT("Record"), _("&Record"), FN(OnRecord), wxT("R"));
      c->AddItem(wxT("TimerRecord"), _("&Timer Record..."), FN(OnTimerRecord), wxT("Shift+T"));
      c->AddItem(wxT("RecordAppend"), _("Appen&d Record"), FN(OnRecordAppend), wxT("Shift+R"));

      c->AddSeparator();

      c->AddCheck(wxT("PinnedHead"), _("Pinned Recording/Playback &Head"),
                  FN(OnTogglePinnedHead), 0,
                  // Switching of scrolling on and off is permitted even during transport
                  AlwaysEnabledFlag, AlwaysEnabledFlag);

      c->AddCheck(wxT("Duplex"), _("&Overdub (on/off)"), FN(OnTogglePlayRecording), 0);
      c->AddCheck(wxT("SWPlaythrough"), _("So&ftware Playthrough (on/off)"), FN(OnToggleSWPlaythrough), 0);

      // Sound Activated recording options
      c->AddCheck(wxT("SoundActivation"), _("Sound A&ctivated Recording (on/off)"), FN(OnToggleSoundActivated), 0);
      c->AddItem(wxT("SoundActivationLevel"), _("Sound Activation Le&vel..."), FN(OnSoundActivated));

#ifdef EXPERIMENTAL_AUTOMATED_INPUT_LEVEL_ADJUSTMENT
      c->AddCheck(wxT("AutomatedInputLevelAdjustmentOnOff"), _("A&utomated Recording Level Adjustment (on/off)"), FN(OnToogleAutomatedInputLevelAdjustment), 0);
#endif
      c->AddItem(wxT("RescanDevices"), _("R&escan Audio Devices"), FN(OnRescanDevices));

      c->EndMenu();

      //////////////////////////////////////////////////////////////////////////
      // Tracks Menu (formerly Project Menu)
      //////////////////////////////////////////////////////////////////////////

      c->BeginMenu(_("&Tracks"));
      c->SetDefaultFlags(AudioIONotBusyFlag, AudioIONotBusyFlag);

      //////////////////////////////////////////////////////////////////////////

      c->BeginSubMenu(_("Add &New"));

      c->AddItem(wxT("NewMonoTrack"), _("&Mono Track"), FN(OnNewWaveTrack), wxT("Ctrl+Shift+N"));
      c->AddItem(wxT("NewStereoTrack"), _("&Stereo Track"), FN(OnNewStereoTrack));
      c->AddItem(wxT("NewLabelTrack"), _("&Label Track"), FN(OnNewLabelTrack));
      c->AddItem(wxT("NewTimeTrack"), _("&Time Track"), FN(OnNewTimeTrack));

      c->EndSubMenu();

      //////////////////////////////////////////////////////////////////////////

      c->AddSeparator();

      {
         // Stereo to Mono is an oddball command that is also subject to control by the
         // plug-in manager, as if an effect.  Decide whether to show or hide it.
         const PluginID ID = EffectManager::Get().GetEffectByIdentifier(wxT("StereoToMono"));
         const PluginDescriptor *plug = PluginManager::Get().GetPlugin(ID);
         if (plug && plug->IsEnabled())
            c->AddItem(wxT("Stereo to Mono"), _("Stereo Trac&k to Mono"), FN(OnStereoToMono),
            AudioIONotBusyFlag | StereoRequiredFlag | WaveTracksSelectedFlag,
            AudioIONotBusyFlag | StereoRequiredFlag | WaveTracksSelectedFlag);
      }
      c->AddItem(wxT("MixAndRender"), _("Mi&x and Render"), FN(OnMixAndRender),
         AudioIONotBusyFlag | WaveTracksSelectedFlag,
         AudioIONotBusyFlag | WaveTracksSelectedFlag);
      c->AddItem(wxT("MixAndRenderToNewTrack"), _("Mix and Render to Ne&w Track"), FN(OnMixAndRenderToNewTrack), wxT("Ctrl+Shift+M"),
         AudioIONotBusyFlag | WaveTracksSelectedFlag,
         AudioIONotBusyFlag | WaveTracksSelectedFlag);
      c->AddItem(wxT("Resample"), _("&Resample..."), FN(OnResample),
         AudioIONotBusyFlag | WaveTracksSelectedFlag,
         AudioIONotBusyFlag | WaveTracksSelectedFlag);

      c->AddSeparator();

      c->AddItem(wxT("RemoveTracks"), _("Remo&ve Tracks"), FN(OnRemoveTracks),
         AudioIONotBusyFlag | TracksSelectedFlag,
         AudioIONotBusyFlag | TracksSelectedFlag);

      c->AddSeparator();

      c->AddItem(wxT("MuteAllTracks"), _("&Mute All Tracks"), FN(OnMuteAllTracks), wxT("Ctrl+U"));
      c->AddItem(wxT("UnMuteAllTracks"), _("&Unmute All Tracks"), FN(OnUnMuteAllTracks), wxT("Ctrl+Shift+U"));

      c->AddSeparator();

      wxArrayString alignLabelsNoSync;
      alignLabelsNoSync.Add(_("&Align End to End"));
      alignLabelsNoSync.Add(_("Align &Together"));

      wxArrayString alignLabels;
      alignLabels.Add(_("Start to &Zero"));
      alignLabels.Add(_("Start to &Cursor/Selection Start"));
      alignLabels.Add(_("Start to Selection &End"));
      alignLabels.Add(_("End to Cu&rsor/Selection Start"));
      alignLabels.Add(_("End to Selection En&d"));
      mAlignLabelsCount = alignLabels.GetCount();

      // Calling c->SetCommandFlags() after AddItemList for "Align" and "AlignMove"
      // does not correctly set flags for submenus, so do it this way.
      c->SetDefaultFlags(AudioIONotBusyFlag | TracksSelectedFlag,
         AudioIONotBusyFlag | TracksSelectedFlag);

      c->BeginSubMenu(_("&Align Tracks"));

      c->AddItemList(wxT("Align"), alignLabelsNoSync, FN(OnAlignNoSync));
      c->AddSeparator();
      c->AddItemList(wxT("Align"), alignLabels, FN(OnAlign));

      c->EndSubMenu();

      //////////////////////////////////////////////////////////////////////////

      // TODO: Can these labels be made clearer? Do we need this sub-menu at all?
      c->BeginSubMenu(_("Move Sele&ction when Aligning"));

      c->AddItemList(wxT("AlignMove"), alignLabels, FN(OnAlignMoveSel));
      c->SetCommandFlags(wxT("AlignMove"),
         AudioIONotBusyFlag | TracksSelectedFlag,
         AudioIONotBusyFlag | TracksSelectedFlag);

      c->EndSubMenu();
      c->SetDefaultFlags(AudioIONotBusyFlag, AudioIONotBusyFlag);


      //////////////////////////////////////////////////////////////////////////

#ifdef EXPERIMENTAL_SCOREALIGN
      c->AddItem(wxT("ScoreAlign"), _("Synchronize MIDI with Audio"), FN(OnScoreAlign),
         AudioIONotBusyFlag | NoteTracksSelectedFlag | WaveTracksSelectedFlag,
         AudioIONotBusyFlag | NoteTracksSelectedFlag | WaveTracksSelectedFlag);
#endif // EXPERIMENTAL_SCOREALIGN

      c->AddSeparator();

#ifdef EXPERIMENTAL_SYNC_LOCK
      c->AddCheck(wxT("SyncLock"), _("Sync-&Lock Tracks"), FN(OnSyncLock), 0,
         AlwaysEnabledFlag, AlwaysEnabledFlag);

      c->AddSeparator();
#endif

      c->AddItem(wxT("AddLabel"), _("Add Label At &Selection"), FN(OnAddLabel), wxT("Ctrl+B"),
         AlwaysEnabledFlag, AlwaysEnabledFlag);
      c->AddItem(wxT("AddLabelPlaying"), _("Add Label At &Playback Position"),
         FN(OnAddLabelPlaying),
#ifdef __WXMAC__
         wxT("Ctrl+."),
#else
         wxT("Ctrl+M"),
#endif
         AudioIOBusyFlag, 
         AudioIOBusyFlag);
      c->AddItem(wxT("EditLabels"), _("&Edit Labels..."), FN(OnEditLabels));

      c->AddSeparator();

      //////////////////////////////////////////////////////////////////////////

      c->BeginSubMenu(_("S&ort Tracks"));

      c->AddItem(wxT("SortByTime"), _("by &Start time"), FN(OnSortTime),
         TracksExistFlag,
         TracksExistFlag);
      c->AddItem(wxT("SortByName"), _("by &Name"), FN(OnSortName),
         TracksExistFlag,
         TracksExistFlag);

      c->EndSubMenu();

      //////////////////////////////////////////////////////////////////////////

      c->EndMenu();

      // All of this is a bit hacky until we can get more things connected into
      // the plugin manager...sorry! :-(

      wxArrayString defaults;

      //////////////////////////////////////////////////////////////////////////
      // Generate Menu
      //////////////////////////////////////////////////////////////////////////

      c->BeginMenu(_("&Generate"));
      c->SetDefaultFlags(AudioIONotBusyFlag, AudioIONotBusyFlag);

#ifdef EXPERIMENTAL_EFFECT_MANAGEMENT
      c->AddItem(wxT("ManageGenerators"), _("Add / Remove Plug-ins..."), FN(OnManageGenerators));
      c->AddSeparator();
#endif


      PopulateEffectsMenu(c,
         EffectTypeGenerate,
         AudioIONotBusyFlag,
         AudioIONotBusyFlag);

      c->EndMenu();

      /////////////////////////////////////////////////////////////////////////////
      // Effect Menu
      /////////////////////////////////////////////////////////////////////////////

      c->BeginMenu(_("Effe&ct"));

      wxString buildMenuLabel;
      if (!mLastEffect.IsEmpty()) {
         buildMenuLabel.Printf(_("Repeat %s"),
            EffectManager::Get().GetEffectName(mLastEffect).c_str());
      }
      else
         buildMenuLabel.Printf(_("Repeat Last Effect"));

#ifdef EXPERIMENTAL_EFFECT_MANAGEMENT
      c->AddItem(wxT("ManageEffects"), _("Add / Remove Plug-ins..."), FN(OnManageEffects));
      c->AddSeparator();
#endif

      c->AddItem(wxT("RepeatLastEffect"), buildMenuLabel, FN(OnRepeatLastEffect), wxT("Ctrl+R"),
         AudioIONotBusyFlag | TimeSelectedFlag | WaveTracksSelectedFlag | HasLastEffectFlag,
         AudioIONotBusyFlag | TimeSelectedFlag | WaveTracksSelectedFlag | HasLastEffectFlag);

      c->AddSeparator();

      PopulateEffectsMenu(c,
         EffectTypeProcess,
         AudioIONotBusyFlag | TimeSelectedFlag | WaveTracksSelectedFlag,
         IsRealtimeNotActiveFlag);

      c->EndMenu();

      //////////////////////////////////////////////////////////////////////////
      // Analyze Menu
      //////////////////////////////////////////////////////////////////////////

      c->BeginMenu(_("&Analyze"));

#ifdef EXPERIMENTAL_EFFECT_MANAGEMENT
      c->AddItem(wxT("ManageAnalyzers"), _("Add / Remove Plug-ins..."), FN(OnManageAnalyzers));
      c->AddSeparator();
#endif


      c->AddItem(wxT("ContrastAnalyser"), _("Contrast..."), FN(OnContrast), wxT("Ctrl+Shift+T"),
         AudioIONotBusyFlag | WaveTracksSelectedFlag | TimeSelectedFlag,
         AudioIONotBusyFlag | WaveTracksSelectedFlag | TimeSelectedFlag);
      c->AddItem(wxT("PlotSpectrum"), _("Plot Spectrum..."), FN(OnPlotSpectrum),
         AudioIONotBusyFlag | WaveTracksSelectedFlag | TimeSelectedFlag,
         AudioIONotBusyFlag | WaveTracksSelectedFlag | TimeSelectedFlag);

      PopulateEffectsMenu(c,
         EffectTypeAnalyze,
         AudioIONotBusyFlag | TimeSelectedFlag | WaveTracksSelectedFlag,
         IsRealtimeNotActiveFlag);

      c->EndMenu();

#ifdef __WXMAC__
      /////////////////////////////////////////////////////////////////////////////
      // poor imitation of the Mac Windows Menu
      /////////////////////////////////////////////////////////////////////////////

      {
      c->BeginMenu(_("&Window"));
      /* i18n-hint: Standard Macintosh Window menu item:  Make (the current
       * window) shrink to an icon on the dock */
      c->AddItem(wxT("MacMinimize"), _("&Minimize"), FN(OnMacMinimize),
                 wxT("Ctrl+M"), NotMinimizedFlag, NotMinimizedFlag);
      /* i18n-hint: Standard Macintosh Window menu item:  Make (the current
       * window) full sized */
      c->AddItem(wxT("MacZoom"), _("&Zoom"), FN(OnMacZoom),
                 wxT(""), NotMinimizedFlag, NotMinimizedFlag);
      c->AddSeparator();
      /* i18n-hint: Standard Macintosh Window menu item:  Make all project
       * windows un-hidden */
      c->AddItem(wxT("MacBringAllToFront"),
                 _("&Bring All to Front"), FN(OnMacBringAllToFront),
                 wxT(""), AlwaysEnabledFlag, AlwaysEnabledFlag);
      c->EndMenu();
      }
#endif

      /////////////////////////////////////////////////////////////////////////////
      // Help Menu
      /////////////////////////////////////////////////////////////////////////////

#ifdef __WXMAC__
      wxGetApp().s_macHelpMenuTitleName = _("&Help");
#endif

      c->BeginMenu(_("&Help"));
      c->SetDefaultFlags(AlwaysEnabledFlag, AlwaysEnabledFlag);

      c->AddItem(wxT("QuickHelp"), _("&Quick Help"), FN(OnQuickHelp));
      c->AddItem(wxT("Manual"), _("&Manual"), FN(OnManual));

      c->AddSeparator();

      c->AddItem(wxT("Screenshot"), _("&Screenshot Tools..."), FN(OnScreenshot));

#if IS_ALPHA
      // TODO: What should we do here?  Make benchmark a plug-in?
      // Easy enough to do.  We'd call it mod-self-test.

      c->AddItem(wxT("Benchmark"), _("&Run Benchmark..."), FN(OnBenchmark));
#endif

      c->AddSeparator();
      c->AddItem(wxT("Updates"), _("&Check for Updates..."), FN(OnCheckForUpdates));
      c->AddItem(wxT("DeviceInfo"), _("Au&dio Device Info..."), FN(OnAudioDeviceInfo),
         AudioIONotBusyFlag,
         AudioIONotBusyFlag);

      c->AddItem(wxT("Log"), _("Show &Log..."), FN(OnShowLog));

#if defined(EXPERIMENTAL_CRASH_REPORT)
      c->AddItem(wxT("CrashReport"), _("&Generate Support Data..."), FN(OnCrashReport));
#endif

#ifndef __WXMAC__
      c->AddSeparator();
#endif

      c->AddItem(wxT("About"), _("&About Audacity..."), FN(OnAbout));

      c->EndMenu();

      /////////////////////////////////////////////////////////////////////////////

      SetMenuBar(menubar.release());
   }

   c->AddGlobalCommand(wxT("PrevWindow"), _("Move backward thru active windows"), FN(PrevWindow), wxT("Alt+Shift+F6"));
   c->AddGlobalCommand(wxT("NextWindow"), _("Move forward thru active windows"), FN(NextWindow), wxT("Alt+F6"));

   c->SetDefaultFlags(AlwaysEnabledFlag, AlwaysEnabledFlag);

   c->AddCommand(wxT("PrevFrame"), _("Move backward from toolbars to tracks"), FN(PrevFrame), wxT("Ctrl+Shift+F6"));
   c->AddCommand(wxT("NextFrame"), _("Move forward from toolbars to tracks"), FN(NextFrame), wxT("Ctrl+F6"));

   c->AddCommand(wxT("SelectTool"), _("Selection Tool"), FN(OnSelectTool), wxT("F1"));
   c->AddCommand(wxT("EnvelopeTool"),_("Envelope Tool"), FN(OnEnvelopeTool), wxT("F2"));
   c->AddCommand(wxT("DrawTool"), _("Draw Tool"), FN(OnDrawTool), wxT("F3"));
   c->AddCommand(wxT("ZoomTool"), _("Zoom Tool"), FN(OnZoomTool), wxT("F4"));
   c->AddCommand(wxT("TimeShiftTool"), _("Time Shift Tool"), FN(OnTimeShiftTool), wxT("F5"));
   c->AddCommand(wxT("MultiTool"), _("Multi Tool"), FN(OnMultiTool), wxT("F6"));

   c->AddCommand(wxT("NextTool"), _("Next Tool"), FN(OnNextTool), wxT("D"));
   c->AddCommand(wxT("PrevTool"), _("Previous Tool"), FN(OnPrevTool), wxT("A"));
   /* i18n-hint: (verb) Start playing audio*/
   c->AddCommand(wxT("Play"), _("Play"), FN(OnPlayStop),
                 WaveTracksExistFlag | AudioIONotBusyFlag,
                 WaveTracksExistFlag | AudioIONotBusyFlag);
   /* i18n-hint: (verb) Stop playing audio*/
   c->AddCommand(wxT("Stop"), _("Stop"), FN(OnStop),
                 AudioIOBusyFlag,
                 AudioIOBusyFlag);

   c->SetDefaultFlags(CaptureNotBusyFlag, CaptureNotBusyFlag);

   c->AddCommand(wxT("PlayOneSec"), _("Play One Second"), FN(OnPlayOneSecond), wxT("1"),
                 CaptureNotBusyFlag,
                 CaptureNotBusyFlag);
   c->AddCommand(wxT("PlayToSelection"),_("Play To Selection"), FN(OnPlayToSelection), wxT("B"),
                 CaptureNotBusyFlag,
                 CaptureNotBusyFlag);
   c->AddCommand(wxT("PlayBeforeSelectionStart"),_("Play Before Selection Start"), FN(OnPlayBeforeSelectionStart), wxT("Shift+F5"));
   c->AddCommand(wxT("PlayAfterSelectionStart"),_("Play After Selection Start"), FN(OnPlayAfterSelectionStart), wxT("Shift+F6"));
   c->AddCommand(wxT("PlayBeforeSelectionEnd"),_("Play Before Selection End"), FN(OnPlayBeforeSelectionEnd), wxT("Shift+F7"));
   c->AddCommand(wxT("PlayAfterSelectionEnd"),_("Play After Selection End"), FN(OnPlayAfterSelectionEnd), wxT("Shift+F8"));
   c->AddCommand(wxT("PlayBeforeAndAfterSelectionStart"),_("Play Before and After Selection Start"), FN(OnPlayBeforeAndAfterSelectionStart), wxT("Ctrl+Shift+F5"));
   c->AddCommand(wxT("PlayBeforeAndAfterSelectionEnd"),_("Play Before and After Selection End"), FN(OnPlayBeforeAndAfterSelectionEnd), wxT("Ctrl+Shift+F7"));
   c->AddCommand(wxT("PlayCutPreview"), _("Play Cut Preview"), FN(OnPlayCutPreview), wxT("C"),
                 CaptureNotBusyFlag,
                 CaptureNotBusyFlag);

   c->SetDefaultFlags(AlwaysEnabledFlag, AlwaysEnabledFlag);


   c->AddCommand(wxT("SelStart"), _("Selection to Start"), FN(OnSelToStart), wxT("Shift+Home"));
   c->AddCommand(wxT("SelEnd"), _("Selection to End"), FN(OnSelToEnd), wxT("Shift+End"));

   c->AddCommand(wxT("DeleteKey"), _("DeleteKey"), FN(OnDelete), wxT("Backspace"),
                 AudioIONotBusyFlag | TracksSelectedFlag | TimeSelectedFlag,
                 AudioIONotBusyFlag | TracksSelectedFlag | TimeSelectedFlag);

   c->AddCommand(wxT("DeleteKey2"), _("DeleteKey2"), FN(OnDelete), wxT("Delete"),
                 AudioIONotBusyFlag | TracksSelectedFlag | TimeSelectedFlag,
                 AudioIONotBusyFlag | TracksSelectedFlag | TimeSelectedFlag);

   c->SetDefaultFlags(AudioIOBusyFlag, AudioIOBusyFlag);

   c->AddCommand(wxT("SeekLeftShort"), _("Short seek left during playback"), FN(OnSeekLeftShort), wxT("Left\tallowDup"));
   c->AddCommand(wxT("SeekRightShort"),_("Short seek right during playback"), FN(OnSeekRightShort), wxT("Right\tallowDup"));
   c->AddCommand(wxT("SeekLeftLong"), _("Long seek left during playback"), FN(OnSeekLeftLong), wxT("Shift+Left\tallowDup"));
   c->AddCommand(wxT("SeekRightLong"), _("Long Seek right during playback"), FN(OnSeekRightLong), wxT("Shift+Right\tallowDup"));

   c->SetDefaultFlags(TracksExistFlag | TrackPanelHasFocus,
                      TracksExistFlag | TrackPanelHasFocus);

   c->AddCommand(wxT("PrevTrack"), _("Move Focus to Previous Track"), FN(OnCursorUp), wxT("Up"));
   c->AddCommand(wxT("NextTrack"), _("Move Focus to Next Track"), FN(OnCursorDown), wxT("Down"));
   c->AddCommand(wxT("FirstTrack"), _("Move Focus to First Track"), FN(OnFirstTrack), wxT("Ctrl+Home"));
   c->AddCommand(wxT("LastTrack"), _("Move Focus to Last Track"), FN(OnLastTrack), wxT("Ctrl+End"));


   c->AddCommand(wxT("ShiftUp"), _("Move Focus to Previous and Select"), FN(OnShiftUp), wxT("Shift+Up"));
   c->AddCommand(wxT("ShiftDown"), _("Move Focus to Next and Select"), FN(OnShiftDown), wxT("Shift+Down"));


   c->AddCommand(wxT("Toggle"), _("Toggle Focused Track"), FN(OnToggle), wxT("Return"));
   c->AddCommand(wxT("ToggleAlt"), _("Toggle Focused Track"), FN(OnToggle), wxT("NUMPAD_ENTER"));

   c->AddCommand(wxT("CursorLeft"), _("Cursor Left"), FN(OnCursorLeft), wxT("Left\twantKeyup\tallowDup"));
   c->AddCommand(wxT("CursorRight"), _("Cursor Right"), FN(OnCursorRight), wxT("Right\twantKeyup\tallowDup"));
   c->AddCommand(wxT("CursorShortJumpLeft"), _("Cursor Short Jump Left"), FN(OnCursorShortJumpLeft), wxT(","));
   c->AddCommand(wxT("CursorShortJumpRight"), _("Cursor Short Jump Right"), FN(OnCursorShortJumpRight), wxT("."));
   c->AddCommand(wxT("CursorLongJumpLeft"), _("Cursor Long Jump Left"), FN(OnCursorLongJumpLeft), wxT("Shift+,"));
   c->AddCommand(wxT("CursorLongJumpRight"), _("Cursor Long Jump Right"), FN(OnCursorLongJumpRight), wxT("Shift+."));

   c->AddCommand(wxT("SelExtLeft"), _("Selection Extend Left"), FN(OnSelExtendLeft), wxT("Shift+Left\twantKeyup\tallowDup"));
   c->AddCommand(wxT("SelExtRight"), _("Selection Extend Right"), FN(OnSelExtendRight), wxT("Shift+Right\twantKeyup\tallowDup"));

   c->AddCommand(wxT("SelSetExtLeft"), _("Set (or Extend) Left Selection"), FN(OnSelSetExtendLeft));
   c->AddCommand(wxT("SelSetExtRight"), _("Set (or Extend) Right Selection"), FN(OnSelSetExtendRight));

   c->AddCommand(wxT("SelCntrLeft"), _("Selection Contract Left"), FN(OnSelContractLeft), wxT("Ctrl+Shift+Right\twantKeyup"));
   c->AddCommand(wxT("SelCntrRight"), _("Selection Contract Right"), FN(OnSelContractRight), wxT("Ctrl+Shift+Left\twantKeyup"));

   c->AddCommand(wxT("TrackPan"), _("Change pan on focused track"), FN(OnTrackPan), wxT("Shift+P"));
   c->AddCommand(wxT("TrackPanLeft"), _("Pan left on focused track"), FN(OnTrackPanLeft), wxT("Alt+Shift+Left"));
   c->AddCommand(wxT("TrackPanRight"), _("Pan right on focused track"), FN(OnTrackPanRight), wxT("Alt+Shift+Right"));
   c->AddCommand(wxT("TrackGain"), _("Change gain on focused track"), FN(OnTrackGain), wxT("Shift+G"));
   c->AddCommand(wxT("TrackGainInc"), _("Increase gain on focused track"), FN(OnTrackGainInc), wxT("Alt+Shift+Up"));
   c->AddCommand(wxT("TrackGainDec"), _("Decrease gain on focused track"), FN(OnTrackGainDec), wxT("Alt+Shift+Down"));
   c->AddCommand(wxT("TrackMenu"), _("Open menu on focused track"), FN(OnTrackMenu), wxT("Shift+M\tskipKeydown"));
   c->AddCommand(wxT("TrackMute"), _("Mute/Unmute focused track"), FN(OnTrackMute), wxT("Shift+U"));
   c->AddCommand(wxT("TrackSolo"), _("Solo/Unsolo focused track"), FN(OnTrackSolo), wxT("Shift+S"));
   c->AddCommand(wxT("TrackClose"), _("Close focused track"), FN(OnTrackClose), wxT("Shift+C"));
   c->AddCommand(wxT("TrackMoveUp"), _("Move focused track up"), FN(OnTrackMoveUp));
   c->AddCommand(wxT("TrackMoveDown"), _("Move focused track down"), FN(OnTrackMoveDown));
   c->AddCommand(wxT("TrackMoveTop"), _("Move focused track to top"), FN(OnTrackMoveTop));
   c->AddCommand(wxT("TrackMoveBottom"), _("Move focused track to bottom"), FN(OnTrackMoveBottom));

   c->SetDefaultFlags(AlwaysEnabledFlag, AlwaysEnabledFlag);

   c->AddCommand(wxT("SnapToOff"), _("Snap To Off"), FN(OnSnapToOff));
   c->AddCommand(wxT("SnapToNearest"), _("Snap To Nearest"), FN(OnSnapToNearest));
   c->AddCommand(wxT("SnapToPrior"), _("Snap To Prior"), FN(OnSnapToPrior));

   c->AddCommand(wxT("FullScreenOnOff"), _("Full screen on/off"), FN(OnFullScreen),
#ifdef __WXMAC__
      wxT("Ctrl+/"));
#else
      wxT("F11"));
#endif

   c->AddCommand(wxT("InputDevice"), _("Change recording device"), FN(OnInputDevice), wxT("Shift+I"),
                 AudioIONotBusyFlag,
                 AudioIONotBusyFlag);
   c->AddCommand(wxT("OutputDevice"), _("Change playback device"), FN(OnOutputDevice), wxT("Shift+O"),
                 AudioIONotBusyFlag,
                 AudioIONotBusyFlag);
   c->AddCommand(wxT("AudioHost"), _("Change audio host"), FN(OnAudioHost), wxT("Shift+H"),
                 AudioIONotBusyFlag,
                 AudioIONotBusyFlag);
   c->AddCommand(wxT("InputChannels"), _("Change recording channels"), FN(OnInputChannels), wxT("Shift+N"),
                 AudioIONotBusyFlag,
                 AudioIONotBusyFlag);

   c->AddCommand(wxT("OutputGain"), _("Adjust playback volume"), FN(OnOutputGain));
   c->AddCommand(wxT("OutputGainInc"), _("Increase playback volume"), FN(OnOutputGainInc));
   c->AddCommand(wxT("OutputGainDec"), _("Decrease playback volume"), FN(OnOutputGainDec));
   c->AddCommand(wxT("InputGain"), _("Adjust recording volume"), FN(OnInputGain));
   c->AddCommand(wxT("InputGainInc"), _("Increase recording volume"), FN(OnInputGainInc));
   c->AddCommand(wxT("InputGainDec"), _("Decrease recording volume"), FN(OnInputGainDec));

   c->SetDefaultFlags(CaptureNotBusyFlag, CaptureNotBusyFlag);

   c->AddCommand(wxT("PlayAtSpeed"), _("Play at speed"), FN(OnPlayAtSpeed));
   c->AddCommand(wxT("PlayAtSpeedLooped"), _("Loop Play at speed"), FN(OnPlayAtSpeedLooped));
   c->AddCommand(wxT("PlayAtSpeedCutPreview"), _("Play Cut Preview at speed"), FN(OnPlayAtSpeedCutPreview));
   c->AddCommand(wxT("SetPlaySpeed"), _("Adjust playback speed"), FN(OnSetPlaySpeed));
   c->AddCommand(wxT("PlaySpeedInc"), _("Increase playback speed"), FN(OnPlaySpeedInc));
   c->AddCommand(wxT("PlaySpeedDec"), _("Decrease playback speed"), FN(OnPlaySpeedDec));

#ifdef __WXMAC__
   /* i8n-hint: Shrink all project windows to icons on the Macintosh tooldock */
   c->AddCommand(wxT("MacMinimizeAll"), _("Minimize all projects"),
                 FN(OnMacMinimizeAll), wxT("Ctrl+Alt+M"),
                 AlwaysEnabledFlag, AlwaysEnabledFlag);
#endif

   mLastFlags = AlwaysEnabledFlag;

#if defined(__WXDEBUG__)
//   c->CheckDups();
#endif
}

void AudacityProject::PopulateEffectsMenu(CommandManager* c,
                                          EffectType type,
                                          CommandFlag batchflags,
                                          CommandFlag realflags)
{
   PluginManager & pm = PluginManager::Get();

   EffectPlugs defplugs;
   EffectPlugs optplugs;

   const PluginDescriptor *plug = pm.GetFirstPluginForEffectType(type);
   while (plug)
   {
      if ( !plug->IsEnabled() ){
         ;// don't add to menus!
      }
      else if (plug->IsEffectDefault())
      {
         defplugs.Add(plug);
      }
      else
      {
         optplugs.Add(plug);
      }
      plug = pm.GetNextPluginForEffectType(type);
   }

   wxString groupby = gPrefs->Read(wxT("/Effects/GroupBy"), wxT("name"));

   if (groupby == wxT("sortby:name"))
   {
      defplugs.Sort(SortEffectsByName);
      optplugs.Sort(SortEffectsByName);
   }
   else if (groupby == wxT("sortby:publisher:name"))
   {
      defplugs.Sort(SortEffectsByName);
      optplugs.Sort(SortEffectsByPublisherAndName);
   }
   else if (groupby == wxT("sortby:type:name"))
   {
      defplugs.Sort(SortEffectsByName);
      optplugs.Sort(SortEffectsByTypeAndName);
   }
   else if (groupby == wxT("groupby:publisher"))
   {
      defplugs.Sort(SortEffectsByPublisher);
      optplugs.Sort(SortEffectsByPublisher);
   }
   else if (groupby == wxT("groupby:type"))
   {
      defplugs.Sort(SortEffectsByType);
      optplugs.Sort(SortEffectsByType);
   }      
   else // name
   {
      defplugs.Sort(SortEffectsByName);
      optplugs.Sort(SortEffectsByName);
   }

   AddEffectMenuItems(c, defplugs, batchflags, realflags, true);

   if (defplugs.GetCount() && optplugs.GetCount())
   {
      c->AddSeparator();
   }

   AddEffectMenuItems(c, optplugs, batchflags, realflags, false);

   return;
}

void AudacityProject::AddEffectMenuItems(CommandManager *c,
                                         EffectPlugs & plugs,
                                         CommandFlag batchflags,
                                         CommandFlag realflags,
                                         bool isDefault)
{
   size_t pluginCnt = plugs.GetCount();

   wxString groupBy = gPrefs->Read(wxT("/Effects/GroupBy"), wxT("name"));

   bool grouped = false;
   if (groupBy.StartsWith(wxT("groupby")))
   {
      grouped = true;
   }

   wxArrayString groupNames;
   PluginIDList groupPlugs;
   std::vector<CommandFlag> groupFlags;
   if (grouped)
   {
      wxString last;
      wxString current;

      for (size_t i = 0; i < pluginCnt; i++)
      {
         const PluginDescriptor *plug = plugs[i];

         wxString name = plug->GetName();

         if (plug->IsEffectInteractive())
         {
            name += wxT("...");
         }

         if (groupBy == wxT("groupby:publisher"))
         {
            current = plug->GetVendor();
            if (current.IsEmpty())
            {
               current = _("Unknown");
            }
         }
         else if (groupBy == wxT("groupby:type"))
         {
            current = plug->GetEffectFamily();
            if (current.IsEmpty())
            {
               current = _("Unknown");
            }
         }

         if (current != last)
         {
            if (!last.IsEmpty())
            {
               c->BeginSubMenu(last);
            }

            AddEffectMenuItemGroup(c, groupNames, groupPlugs, groupFlags, isDefault);

            if (!last.IsEmpty())
            {
               c->EndSubMenu();
            }

            groupNames.Clear();
            groupPlugs.Clear();
            groupFlags.clear();
            last = current;
         }

         groupNames.Add(name);
         groupPlugs.Add(plug->GetID());
         groupFlags.push_back(plug->IsEffectRealtime() ? realflags : batchflags);
      }

      if (groupNames.GetCount() > 0)
      {
         c->BeginSubMenu(current);

         AddEffectMenuItemGroup(c, groupNames, groupPlugs, groupFlags, isDefault);

         c->EndSubMenu();
      }
   }
   else
   {
      for (size_t i = 0; i < pluginCnt; i++)
      {
         const PluginDescriptor *plug = plugs[i];

         wxString name = plug->GetName();

         if (plug->IsEffectInteractive())
         {
            name += wxT("...");
         }

         wxString group = wxEmptyString;
         if (groupBy == wxT("sortby:publisher:name"))
         {
            group = plug->GetVendor();
         }
         else if (groupBy == wxT("sortby:type:name"))
         {
            group = plug->GetEffectFamily();
         }

         if (plug->IsEffectDefault())
         {
            group = wxEmptyString;
         }

         if (!group.IsEmpty())
         {
            group += wxT(": ");
         }

         groupNames.Add(group + name);
         groupPlugs.Add(plug->GetID());
         groupFlags.push_back(plug->IsEffectRealtime() ? realflags : batchflags);
      }

      if (groupNames.GetCount() > 0)
      {
         AddEffectMenuItemGroup(c, groupNames, groupPlugs, groupFlags, isDefault);
      }

   }

   return;
}

void AudacityProject::AddEffectMenuItemGroup(CommandManager *c,
                                             const wxArrayString & names,
                                             const PluginIDList & plugs,
                                             const std::vector<CommandFlag> & flags,
                                             bool isDefault)
{
   int namesCnt = (int) names.GetCount();
   int perGroup;

#if defined(__WXGTK__)
   gPrefs->Read(wxT("/Effects/MaxPerGroup"), &perGroup, 15);
#else
   gPrefs->Read(wxT("/Effects/MaxPerGroup"), &perGroup, 0);
#endif

   int groupCnt = namesCnt;
   for (int i = 0; i < namesCnt; i++)
   {
      while (i + 1 < namesCnt && names[i].IsSameAs(names[i + 1]))
      {
         i++;
         groupCnt--;
      }
   }

   // The "default" effects shouldn't be broken into subgroups
   if (namesCnt > 0 && isDefault)
   {
      perGroup = 0;
   }

   int max = perGroup;
   int items = perGroup;

   if (max > groupCnt)
   {
      max = 0;
   }

   int groupNdx = 0;
   for (int i = 0; i < namesCnt; i++)
   {
      if (max > 0 && items == max)
      {
         int end = groupNdx + max;
         if (end + 1 > groupCnt)
         {
            end = groupCnt;
         }
         c->BeginSubMenu(wxString::Format(_("Plug-in %d to %d"),
                                          groupNdx + 1,
                                          end));
      }

      if (i + 1 < namesCnt && names[i].IsSameAs(names[i + 1]))
      {
         wxString name = names[i];
         c->BeginSubMenu(name);
         while (i < namesCnt && names[i].IsSameAs(name))
         {
            wxString item = PluginManager::Get().GetPlugin(plugs[i])->GetPath();
            c->AddItem(item,
                       item,
                       FNS(OnEffect, plugs[i]),
                       flags[i],
                       flags[i]);

            i++;
         }
         c->EndSubMenu();
         i--;
      }
      else
      {
         c->AddItem(names[i],
                    names[i],
                    FNS(OnEffect, plugs[i]),
                    flags[i],
                    flags[i]);
      }

      if (max > 0)
      {
         groupNdx++;
         items--;
         if (items == 0 || i + 1 == namesCnt)
         {
            c->EndSubMenu();
            items = max;
         }
      }
   }

   return;
}

void AudacityProject::CreateRecentFilesMenu(CommandManager *c)
{
   // Recent Files and Recent Projects menus

#ifdef __WXMAC__
   /* i18n-hint: This is the name of the menu item on Mac OS X only */
   mRecentFilesMenu = c->BeginSubMenu(_("Open Recent"));
#else
   /* i18n-hint: This is the name of the menu item on Windows and Linux */
   mRecentFilesMenu = c->BeginSubMenu(_("Recent &Files"));
#endif

   wxGetApp().GetRecentFiles()->UseMenu(mRecentFilesMenu);
   wxGetApp().GetRecentFiles()->AddFilesToMenu(mRecentFilesMenu);

   c->EndSubMenu();

}

void AudacityProject::ModifyUndoMenuItems()
{
   wxString desc;
   int cur = GetUndoManager()->GetCurrentState();

   if (GetUndoManager()->UndoAvailable()) {
      GetUndoManager()->GetShortDescription(cur, &desc);
      mCommandManager.Modify(wxT("Undo"),
                             wxString::Format(_("&Undo %s"),
                                              desc.c_str()));
   }
   else {
      mCommandManager.Modify(wxT("Undo"),
                             wxString::Format(_("&Undo")));
   }

   if (GetUndoManager()->RedoAvailable()) {
      GetUndoManager()->GetShortDescription(cur+1, &desc);
      mCommandManager.Modify(wxT("Redo"),
                             wxString::Format(_("&Redo %s"),
                                              desc.c_str()));
      mCommandManager.Enable(wxT("Redo"), true);
   }
   else {
      mCommandManager.Modify(wxT("Redo"),
                             wxString::Format(_("&Redo")));
      mCommandManager.Enable(wxT("Redo"), false);
   }
}

void AudacityProject::RebuildMenuBar()
{
   // On OSX, we can't rebuild the menus while a modal dialog is being shown
   // since the enabled state for menus like Quit and Preference gets out of
   // sync with wxWidgets idea of what it should be.
#if defined(__WXMAC__) && defined(__WXDEBUG__)
   {
      wxDialog *dlg = wxDynamicCast(wxGetTopLevelParent(FindFocus()), wxDialog);
      wxASSERT((!dlg || !dlg->IsModal()));
   }
#endif

   // Allow FileHistory to remove its own menu
   wxGetApp().GetRecentFiles()->RemoveMenu(mRecentFilesMenu);

   // Delete the menus, since we will soon recreate them.
   // Rather oddly, the menus don't vanish as a result of doing this.
   {
      std::unique_ptr<wxMenuBar> menuBar{ GetMenuBar() };
      DetachMenuBar();
      // menuBar gets deleted here
   }

   mCommandManager.PurgeData();

   CreateMenusAndCommands();

   ModuleManager::Get().Dispatch(MenusRebuilt);
}

void AudacityProject::RebuildOtherMenus()
{
   if (mTrackPanel) {
      mTrackPanel->BuildMenus();
   }
}

CommandFlag AudacityProject::GetFocusedFrame()
{
   wxWindow *w = FindFocus();

   while (w && mToolManager && mTrackPanel) {
      if (w == mToolManager->GetTopDock()) {
         return TopDockHasFocus;
      }

      if (w == mRuler)
         return RulerHasFocus;

      if (w == mTrackPanel) {
         return TrackPanelHasFocus;
      }

      if (w == mToolManager->GetBotDock()) {
         return BotDockHasFocus;
      }

      w = w->GetParent();
   }

   return AlwaysEnabledFlag;
}

CommandFlag AudacityProject::GetUpdateFlags()
{
   // This method determines all of the flags that determine whether
   // certain menu items and commands should be enabled or disabled,
   // and returns them in a bitfield.  Note that if none of the flags
   // have changed, it's not necessary to even check for updates.
   auto flags = AlwaysEnabledFlag;

   if (!gAudioIO->IsAudioTokenActive(GetAudioIOToken()))
      flags |= AudioIONotBusyFlag;
   else
      flags |= AudioIOBusyFlag;

   if( gAudioIO->IsPaused() )
      flags |= PausedFlag;
   else
      flags |= NotPausedFlag;

   if (!mViewInfo.selectedRegion.isPoint())
      flags |= TimeSelectedFlag;

   TrackListIterator iter(GetTracks());
   Track *t = iter.First();
   while (t) {
      flags |= TracksExistFlag;
      if (t->GetKind() == Track::Label) {
         LabelTrack *lt = (LabelTrack *) t;

         flags |= LabelTracksExistFlag;

         if (lt->GetSelected()) {
            flags |= TracksSelectedFlag;
            for (int i = 0; i < lt->GetNumLabels(); i++) {
               const LabelStruct *ls = lt->GetLabel(i);
               if (ls->getT0() >= mViewInfo.selectedRegion.t0() &&
                   ls->getT1() <= mViewInfo.selectedRegion.t1()) {
                  flags |= LabelsSelectedFlag;
                  break;
               }
            }
         }

         if (lt->IsTextSelected()) {
            flags |= CutCopyAvailableFlag;
         }
      }
      else if (t->GetKind() == Track::Wave) {
         flags |= WaveTracksExistFlag;
         if (t->GetSelected()) {
            flags |= TracksSelectedFlag;
            if (t->GetLinked()) {
               flags |= StereoRequiredFlag;
            }
            else {
               flags |= WaveTracksSelectedFlag;
            }
         }
         if( t->GetEndTime() > t->GetStartTime() )
            flags |= HasWaveDataFlag; 
      }
#if defined(USE_MIDI)
      else if (t->GetKind() == Track::Note) {
         NoteTrack *nt = (NoteTrack *) t;

         flags |= NoteTracksExistFlag;

         if (nt->GetSelected()) {
            flags |= TracksSelectedFlag;
            flags |= NoteTracksSelectedFlag;
         }
      }
#endif
      t = iter.Next();
   }

   if((msClipT1 - msClipT0) > 0.0)
      flags |= ClipboardFlag;

   if (GetUndoManager()->UnsavedChanges())
      flags |= UnsavedChangesFlag;

   if (!mLastEffect.IsEmpty())
      flags |= HasLastEffectFlag;

   if (GetUndoManager()->UndoAvailable())
      flags |= UndoAvailableFlag;

   if (GetUndoManager()->RedoAvailable())
      flags |= RedoAvailableFlag;

   if (ZoomInAvailable() && (flags & TracksExistFlag))
      flags |= ZoomInAvailableFlag;

   if (ZoomOutAvailable() && (flags & TracksExistFlag))
      flags |= ZoomOutAvailableFlag;

   if ((flags & LabelTracksExistFlag) && LabelTrack::IsTextClipSupported())
      flags |= TextClipFlag;

   flags |= GetFocusedFrame();

   double start, end;
   GetPlayRegion(&start, &end);
   if (IsPlayRegionLocked())
      flags |= PlayRegionLockedFlag;
   else if (start != end)
      flags |= PlayRegionNotLockedFlag;

   if (flags & AudioIONotBusyFlag) {
      if (flags & TimeSelectedFlag) {
         if (flags & TracksSelectedFlag) {
            flags |= CutCopyAvailableFlag;
         }
      }
   }

   if (wxGetApp().GetRecentFiles()->GetCount() > 0)
      flags |= HaveRecentFiles;

   if (IsSyncLocked())
      flags |= IsSyncLockedFlag;
   else
      flags |= IsNotSyncLockedFlag;

   if (!EffectManager::Get().RealtimeIsActive())
      flags |= IsRealtimeNotActiveFlag;

      if (!mIsCapturing)
      flags |= CaptureNotBusyFlag;

   ControlToolBar *bar = GetControlToolBar();
   if (bar->ControlToolBar::CanStopAudioStream())
      flags |= CanStopAudioStreamFlag;

   if (auto focus = wxWindow::FindFocus()) {
      while (focus && focus->GetParent())
         focus = focus->GetParent();
      if (focus && !static_cast<wxTopLevelWindow*>(focus)->IsIconized())
         flags |= NotMinimizedFlag;
   }

   return flags;
}

void AudacityProject::SelectAllIfNone()
{
   auto flags = GetUpdateFlags();
   if(!(flags & TracksSelectedFlag) ||
      (mViewInfo.selectedRegion.isPoint()))
      OnSelectAll();
}

void AudacityProject::StopIfPaused()
{
   auto flags = GetUpdateFlags();
   if( flags & PausedFlag )
      OnStop();
}

void AudacityProject::ModifyAllProjectToolbarMenus()
{
   AProjectArray::iterator i;
   for (i = gAudacityProjects.begin(); i != gAudacityProjects.end(); ++i) {
      (*i)->ModifyToolbarMenus();
   }
}

void AudacityProject::ModifyToolbarMenus()
{
   // Refreshes can occur during shutdown and the toolmanager may already
   // be deleted, so protect against it.
   if (!mToolManager) {
      return;
   }

   mCommandManager.Check(wxT("ShowScrubbingTB"),
                         mToolManager->IsVisible(ScrubbingBarID));
   mCommandManager.Check(wxT("ShowDeviceTB"),
                         mToolManager->IsVisible(DeviceBarID));
   mCommandManager.Check(wxT("ShowEditTB"),
                         mToolManager->IsVisible(EditBarID));
   mCommandManager.Check(wxT("ShowMeterTB"),
                         mToolManager->IsVisible(MeterBarID));
   mCommandManager.Check(wxT("ShowRecordMeterTB"),
                         mToolManager->IsVisible(RecordMeterBarID));
   mCommandManager.Check(wxT("ShowPlayMeterTB"),
                         mToolManager->IsVisible(PlayMeterBarID));
   mCommandManager.Check(wxT("ShowMixerTB"),
                         mToolManager->IsVisible(MixerBarID));
   mCommandManager.Check(wxT("ShowSelectionTB"),
                         mToolManager->IsVisible(SelectionBarID));
#ifdef EXPERIMENTAL_SPECTRAL_EDITING
   mCommandManager.Check(wxT("ShowSpectralSelectionTB"),
                         mToolManager->IsVisible(SpectralSelectionBarID));
#endif
   mCommandManager.Check(wxT("ShowToolsTB"),
                         mToolManager->IsVisible(ToolsBarID));
   mCommandManager.Check(wxT("ShowTranscriptionTB"),
                         mToolManager->IsVisible(TranscriptionBarID));
   mCommandManager.Check(wxT("ShowTransportTB"),
                         mToolManager->IsVisible(TransportBarID));

   // Now, go through each toolbar, and call EnableDisableButtons()
   for (int i = 0; i < ToolBarCount; i++) {
      mToolManager->GetToolBar(i)->EnableDisableButtons();
   }

   // These don't really belong here, but it's easier and especially so for
   // the Edit toolbar and the sync-lock menu item.
   bool active;
   gPrefs->Read(wxT("/AudioIO/SoundActivatedRecord"),&active, false);
   mCommandManager.Check(wxT("SoundActivation"), active);
#ifdef EXPERIMENTAL_AUTOMATED_INPUT_LEVEL_ADJUSTMENT
   gPrefs->Read(wxT("/AudioIO/AutomatedInputLevelAdjustment"),&active, false);
   mCommandManager.Check(wxT("AutomatedInputLevelAdjustmentOnOff"), active);
#endif

   active = TracksPrefs::GetPinnedHeadPreference();
   mCommandManager.Check(wxT("PinnedHead"), active);

   gPrefs->Read(wxT("/AudioIO/Duplex"),&active, true);
   mCommandManager.Check(wxT("Duplex"), active);
   gPrefs->Read(wxT("/AudioIO/SWPlaythrough"),&active, false);
   mCommandManager.Check(wxT("SWPlaythrough"), active);
   gPrefs->Read(wxT("/GUI/SyncLockTracks"), &active, false);
   SetSyncLock(active);
   mCommandManager.Check(wxT("SyncLock"), active);
}

// checkActive is a temporary hack that should be removed as soon as we
// get multiple effect preview working
void AudacityProject::UpdateMenus(bool /*checkActive*/)
{
   //ANSWER-ME: Why UpdateMenus only does active project?
   //JKC: Is this test fixing a bug when multiple projects are open?
   //so that menu states work even when different in different projects?
   if (this != GetActiveProject())
      return;

   //if (checkActive && !IsActive())
     // return;

   auto flags = GetUpdateFlags();
   auto flags2 = flags;

   // We can enable some extra items if we have select-all-on-none.
   //EXPLAIN-ME: Why is this here rather than in GetUpdateFlags()?
   //ANSWER: Because flags2 is used in the menu enable/disable.
   //The effect still needs flags to determine whether it will need
   //to actually do the 'select all' to make the command valid.
   if (mSelectAllOnNone)
   {
      if ((flags & TracksExistFlag))
      {
         flags2 |= TracksSelectedFlag;
         if ((flags & WaveTracksExistFlag))
         {
            flags2 |= TimeSelectedFlag
                   |  WaveTracksSelectedFlag
                   |  CutCopyAvailableFlag;
         }
      }
   }

   if( mStopIfWasPaused )
   {
      if( flags & PausedFlag ){
         flags2 |= AudioIONotBusyFlag;
      }
   }

   // Return from this function if nothing's changed since
   // the last time we were here.
   if (flags == mLastFlags)
      return;
   mLastFlags = flags;

   mCommandManager.EnableUsingFlags(flags2 , NoFlagsSpecifed);

   // With select-all-on-none, some items that we don't want enabled may have
   // been enabled, since we changed the flags.  Here we manually disable them.
   if (mSelectAllOnNone)
   {
      if (!(flags & TracksSelectedFlag))
      {
         mCommandManager.Enable(wxT("SplitCut"), false);

         if (!(flags & WaveTracksSelectedFlag))
         {
            mCommandManager.Enable(wxT("Split"), false);
         }
         if (!(flags & TimeSelectedFlag))
         {
            mCommandManager.Enable(wxT("ExportSel"), false);
            mCommandManager.Enable(wxT("SplitNew"), false);
            mCommandManager.Enable(wxT("Trim"), false);
            mCommandManager.Enable(wxT("SplitDelete"), false);
         }
      }
   }

#if 0
   if (flags & CutCopyAvailableFlag) {
      mCommandManager.Enable(wxT("Copy"), true);
      mCommandManager.Enable(wxT("Cut"), true);
   }
#endif

   ModifyToolbarMenus();
}

//
// Tool selection commands
//

void AudacityProject::SetTool(int tool)
{
   ToolsToolBar *toolbar = GetToolsToolBar();
   if (toolbar) {
      toolbar->SetCurrentTool(tool, true);
      mTrackPanel->Refresh(false);
   }
}

void AudacityProject::OnSelectTool()
{
   SetTool(selectTool);
}

void AudacityProject::OnZoomTool()
{
   SetTool(zoomTool);
}

void AudacityProject::OnEnvelopeTool()
{
   SetTool(envelopeTool);
}

void AudacityProject::OnTimeShiftTool()
{
   SetTool(slideTool);
}

void AudacityProject::OnDrawTool()
{
   SetTool(drawTool);
}

void AudacityProject::OnMultiTool()
{
   SetTool(multiTool);
}


void AudacityProject::OnNextTool()
{
   ToolsToolBar *toolbar = GetToolsToolBar();
   if (toolbar) {
      // Use GetDownTool() here since GetCurrentTool() can return a value that
      // doesn't represent the real tool if the Multi-tool is being used.
      toolbar->SetCurrentTool((toolbar->GetDownTool()+1)%numTools, true);
      mTrackPanel->Refresh(false);
   }
}

void AudacityProject::OnPrevTool()
{
   ToolsToolBar *toolbar = GetToolsToolBar();
   if (toolbar) {
      // Use GetDownTool() here since GetCurrentTool() can return a value that
      // doesn't represent the real tool if the Multi-tool is being used.
      toolbar->SetCurrentTool((toolbar->GetDownTool()+(numTools-1))%numTools, true);
      mTrackPanel->Refresh(false);
   }
}


//
// Audio I/O Commands
//

// TODO: Should all these functions which involve
// the toolbar actually move into ControlToolBar?

/// MakeReadyToPlay stops whatever is currently playing
/// and pops the play button up.  Then, if nothing is now
/// playing, it pushes the play button down and enables
/// the stop button.
bool AudacityProject::MakeReadyToPlay(bool loop, bool cutpreview)
{
   ControlToolBar *toolbar = GetControlToolBar();
   wxCommandEvent evt;

   // If this project is playing, stop playing
   if (gAudioIO->IsStreamActive(GetAudioIOToken())) {
      toolbar->SetPlay(false);        //Pops
      toolbar->SetStop(true);         //Pushes stop down
      toolbar->OnStop(evt);

      ::wxMilliSleep(100);
   }

   // If it didn't stop playing quickly, or if some other
   // project is playing, return
   if (gAudioIO->IsBusy())
      return false;

   ControlToolBar::PlayAppearance appearance =
      cutpreview ? ControlToolBar::PlayAppearance::CutPreview
      : loop ? ControlToolBar::PlayAppearance::Looped
      : ControlToolBar::PlayAppearance::Straight;
   toolbar->SetPlay(true, appearance);
   toolbar->SetStop(false);

   return true;
}

void AudacityProject::OnPlayOneSecond()
{
   if( !MakeReadyToPlay() )
      return;

   double pos = mTrackPanel->GetMostRecentXPos();
   GetControlToolBar()->PlayPlayRegion
      (SelectedRegion(pos - 0.5, pos + 0.5), GetDefaultPlayOptions(),
       PlayMode::oneSecondPlay);
}


/// The idea for this function (and first implementation)
/// was from Juhana Sadeharju.  The function plays the
/// sound between the current mouse position and the
/// nearest selection boundary.  This gives four possible
/// play regions depending on where the current mouse
/// position is relative to the left and right boundaries
/// of the selection region.
void AudacityProject::OnPlayToSelection()
{
   if( !MakeReadyToPlay() )
      return;

   double pos = mTrackPanel->GetMostRecentXPos();

   double t0,t1;
   // check region between pointer and the nearest selection edge
   if (fabs(pos - mViewInfo.selectedRegion.t0()) <
       fabs(pos - mViewInfo.selectedRegion.t1())) {
      t0 = t1 = mViewInfo.selectedRegion.t0();
   } else {
      t0 = t1 = mViewInfo.selectedRegion.t1();
   }
   if( pos < t1)
      t0=pos;
   else
      t1=pos;

   // JKC: oneSecondPlay mode disables auto scrolling
   // On balance I think we should always do this in this function
   // since you are typically interested in the sound EXACTLY
   // where the cursor is.
   // TODO: have 'playing attributes' such as 'with_autoscroll'
   // rather than modes, since that's how we're now using the modes.

   // An alternative, commented out below, is to disable autoscroll
   // only when playing a short region, less than or equal to a second.
//   mLastPlayMode = ((t1-t0) > 1.0) ? normalPlay : oneSecondPlay;

   GetControlToolBar()->PlayPlayRegion
      (SelectedRegion(t0, t1), GetDefaultPlayOptions(), PlayMode::oneSecondPlay);
}

// The next 4 functions provide a limited version of the
// functionality of OnPlayToSelection() for keyboard users

void AudacityProject::OnPlayBeforeSelectionStart()
{
   if( !MakeReadyToPlay() )
      return;

   double t0 = mViewInfo.selectedRegion.t0();
   double beforeLen;
   gPrefs->Read(wxT("/AudioIO/CutPreviewBeforeLen"), &beforeLen, 2.0);

   GetControlToolBar()->PlayPlayRegion(SelectedRegion(t0 - beforeLen, t0), GetDefaultPlayOptions(), PlayMode::oneSecondPlay);
}

void AudacityProject::OnPlayAfterSelectionStart()
{
   if( !MakeReadyToPlay() )
      return;

   double t0 = mViewInfo.selectedRegion.t0();
   double t1 = mViewInfo.selectedRegion.t1();
   double afterLen;
   gPrefs->Read(wxT("/AudioIO/CutPreviewAfterLen"), &afterLen, 1.0);

   if ( t1 - t0 > 0.0 && t1 - t0 < afterLen )
      GetControlToolBar()->PlayPlayRegion(SelectedRegion(t0, t1), GetDefaultPlayOptions(),
                                          PlayMode::oneSecondPlay);
   else
      GetControlToolBar()->PlayPlayRegion(SelectedRegion(t0, t0 + afterLen), GetDefaultPlayOptions(), PlayMode::oneSecondPlay);
}

void AudacityProject::OnPlayBeforeSelectionEnd()
{
   if( !MakeReadyToPlay() )
      return;

   double t0 = mViewInfo.selectedRegion.t0();
   double t1 = mViewInfo.selectedRegion.t1();
   double beforeLen;
   gPrefs->Read(wxT("/AudioIO/CutPreviewBeforeLen"), &beforeLen, 2.0);

   if ( t1 - t0 > 0.0 && t1 - t0 < beforeLen )
      GetControlToolBar()->PlayPlayRegion(SelectedRegion(t0, t1), GetDefaultPlayOptions(),
                                          PlayMode::oneSecondPlay);
   else
      GetControlToolBar()->PlayPlayRegion(SelectedRegion(t1 - beforeLen, t1), GetDefaultPlayOptions(), PlayMode::oneSecondPlay);
}


void AudacityProject::OnPlayAfterSelectionEnd()
{
   if( !MakeReadyToPlay() )
      return;

   double t1 = mViewInfo.selectedRegion.t1();
   double afterLen;
   gPrefs->Read(wxT("/AudioIO/CutPreviewAfterLen"), &afterLen, 1.0);

   GetControlToolBar()->PlayPlayRegion(SelectedRegion(t1, t1 + afterLen), GetDefaultPlayOptions(), PlayMode::oneSecondPlay);
}

void AudacityProject::OnPlayBeforeAndAfterSelectionStart()
{
   if (!MakeReadyToPlay())
      return;

   double t0 = mViewInfo.selectedRegion.t0();
   double t1 = mViewInfo.selectedRegion.t1();
   double beforeLen;
   gPrefs->Read(wxT("/AudioIO/CutPreviewBeforeLen"), &beforeLen, 2.0);
   double afterLen;
   gPrefs->Read(wxT("/AudioIO/CutPreviewAfterLen"), &afterLen, 1.0);

   if ( t1 - t0 > 0.0 && t1 - t0 < afterLen )
      GetControlToolBar()->PlayPlayRegion(SelectedRegion(t0 - beforeLen, t1), GetDefaultPlayOptions(), PlayMode::oneSecondPlay);
   else
      GetControlToolBar()->PlayPlayRegion(SelectedRegion(t0 - beforeLen, t0 + afterLen), GetDefaultPlayOptions(), PlayMode::oneSecondPlay);
}

void AudacityProject::OnPlayBeforeAndAfterSelectionEnd()
{
   if (!MakeReadyToPlay())
      return;

   double t0 = mViewInfo.selectedRegion.t0();
   double t1 = mViewInfo.selectedRegion.t1();
   double beforeLen;
   gPrefs->Read(wxT("/AudioIO/CutPreviewBeforeLen"), &beforeLen, 2.0);
   double afterLen;
   gPrefs->Read(wxT("/AudioIO/CutPreviewAfterLen"), &afterLen, 1.0);

   if ( t1 - t0 > 0.0 && t1 - t0 < beforeLen )
      GetControlToolBar()->PlayPlayRegion(SelectedRegion(t0, t1 + afterLen), GetDefaultPlayOptions(), PlayMode::oneSecondPlay);
   else
      GetControlToolBar()->PlayPlayRegion(SelectedRegion(t1 - beforeLen, t1 + afterLen), GetDefaultPlayOptions(), PlayMode::oneSecondPlay);
}


void AudacityProject::OnPlayLooped()
{
   if( !MakeReadyToPlay(true) )
      return;

   // Now play in a loop
   // Will automatically set mLastPlayMode
   GetControlToolBar()->PlayCurrentRegion(true);
}

void AudacityProject::OnPlayCutPreview()
{
   if ( !MakeReadyToPlay(false, true) )
      return;

   // Play with cut preview
   GetControlToolBar()->PlayCurrentRegion(false, true);
}

void AudacityProject::OnPlayStop()
{
   ControlToolBar *toolbar = GetControlToolBar();

   //If this project is playing, stop playing, make sure everything is unpaused.
   if (gAudioIO->IsStreamActive(GetAudioIOToken())) {
      toolbar->SetPlay(false);        //Pops
      toolbar->SetStop(true);         //Pushes stop down
      toolbar->StopPlaying();
   }
   else if (gAudioIO->IsStreamActive()) {
      //If this project isn't playing, but another one is, stop playing the old and start the NEW.

      //find out which project we need;
      AudacityProject* otherProject = NULL;
      for(unsigned i=0; i<gAudacityProjects.size(); i++) {
         if(gAudioIO->IsStreamActive(gAudacityProjects[i]->GetAudioIOToken())) {
            otherProject=gAudacityProjects[i].get();
            break;
         }
      }

      //stop playing the other project
      if(otherProject) {
         ControlToolBar *otherToolbar = otherProject->GetControlToolBar();
         otherToolbar->SetPlay(false);        //Pops
         otherToolbar->SetStop(true);         //Pushes stop down
         otherToolbar->StopPlaying();
      }

      //play the front project
      if (!gAudioIO->IsBusy()) {
         //update the playing area
         TP_DisplaySelection();
         //Otherwise, start playing (assuming audio I/O isn't busy)
         //toolbar->SetPlay(true); // Not needed as done in PlayPlayRegion.
         toolbar->SetStop(false);

         // Will automatically set mLastPlayMode
         toolbar->PlayCurrentRegion(false);
      }
   }
   else if (!gAudioIO->IsBusy()) {
      //Otherwise, start playing (assuming audio I/O isn't busy)
      //toolbar->SetPlay(true); // Not needed as done in PlayPlayRegion.
      toolbar->SetStop(false);

      // Will automatically set mLastPlayMode
      toolbar->PlayCurrentRegion(false);
   }
}

void AudacityProject::OnStop()
{
   wxCommandEvent evt;

   GetControlToolBar()->OnStop(evt);
}

void AudacityProject::OnPause()
{
   wxCommandEvent evt;

   GetControlToolBar()->OnPause(evt);
}

void AudacityProject::OnRecord()
{
   wxCommandEvent evt;
   evt.SetInt(2); // 0 is default, use 1 to set shift on, 2 to clear it

   GetControlToolBar()->OnRecord(evt);
}

void AudacityProject::OnRecordAppend()
{
   wxCommandEvent evt;
   evt.SetInt(1); // 0 is default, use 1 to set shift on, 2 to clear it

   GetControlToolBar()->OnRecord(evt);
}

// The code for "OnPlayStopSelect" is simply the code of "OnPlayStop" and "OnStopSelect" merged.
void AudacityProject::OnPlayStopSelect()
{
   ControlToolBar *toolbar = GetControlToolBar();
   wxCommandEvent evt;
   if (DoPlayStopSelect(false, false))
      toolbar->OnStop(evt);
   else if (!gAudioIO->IsBusy()) {
      //Otherwise, start playing (assuming audio I/O isn't busy)
      //toolbar->SetPlay(true); // Not needed as set in PlayPlayRegion()
      toolbar->SetStop(false);

      // Will automatically set mLastPlayMode
      toolbar->PlayCurrentRegion(false);
   }
}

bool AudacityProject::DoPlayStopSelect(bool click, bool shift)
{
   ControlToolBar *toolbar = GetControlToolBar();

   //If busy, stop playing, make sure everything is unpaused.
   if (GetScrubber().HasStartedScrubbing() ||
       gAudioIO->IsStreamActive(GetAudioIOToken())) {
      toolbar->SetPlay(false);        //Pops
      toolbar->SetStop(true);         //Pushes stop down

      // change the selection
      auto time = gAudioIO->GetStreamTime();
      auto &selection = mViewInfo.selectedRegion;
      if (shift && click) {
         // Change the region selection, as if by shift-click at the play head
         auto t0 = selection.t0(), t1 = selection.t1();
         if (time < t0)
            // Grow selection
            t0 = time;
         else if (time > t1)
            // Grow selection
            t1 = time;
         else {
            // Shrink selection, changing the nearer boundary
            if (fabs(t0 - time) < fabs(t1 - time))
               t0 = time;
            else
               t1 = time;
         }
         selection.setTimes(t0, t1);
      }
      else if (click){
         // avoid a point at negative time.
         time = wxMax( time, 0 );
         // Set a point selection, as if by a click at the play head
         selection.setTimes(time, time);
      } else
         // How stop and set cursor always worked
         // -- change t0, collapsing to point only if t1 was greater
         selection.setT0(time, false);

      ModifyState(false);           // without bWantsAutoSave
      return true;
   }
   return false;
}

void AudacityProject::OnStopSelect()
{
   wxCommandEvent evt;

   if (gAudioIO->IsStreamActive()) {
      mViewInfo.selectedRegion.setT0(gAudioIO->GetStreamTime(), false);
      GetControlToolBar()->OnStop(evt);
      ModifyState(false);           // without bWantsAutoSave
   }
}

void AudacityProject::OnToggleSoundActivated()
{
   bool pause;
   gPrefs->Read(wxT("/AudioIO/SoundActivatedRecord"), &pause, false);
   gPrefs->Write(wxT("/AudioIO/SoundActivatedRecord"), !pause);
   gPrefs->Flush();
   ModifyAllProjectToolbarMenus();
}

void AudacityProject::OnTogglePinnedHead()
{
   bool value = !TracksPrefs::GetPinnedHeadPreference();
   TracksPrefs::SetPinnedHeadPreference(value, true);
   ModifyAllProjectToolbarMenus();

   // Change what happens in case transport is in progress right now
   auto ctb = GetActiveProject()->GetControlToolBar();
   if (ctb)
      ctb->StartScrollingIfPreferred();

   auto ruler = GetRulerPanel();
   if (ruler)
      // Update button image
      ruler->UpdateButtonStates();

   auto &scrubber = GetScrubber();
   if (scrubber.HasStartedScrubbing())
      scrubber.SetScrollScrubbing(value);
}

void AudacityProject::OnTogglePlayRecording()
{
   bool Duplex;
   gPrefs->Read(wxT("/AudioIO/Duplex"), &Duplex, true);
   gPrefs->Write(wxT("/AudioIO/Duplex"), !Duplex);
   gPrefs->Flush();
   ModifyAllProjectToolbarMenus();
}

void AudacityProject::OnToggleSWPlaythrough()
{
   bool SWPlaythrough;
   gPrefs->Read(wxT("/AudioIO/SWPlaythrough"), &SWPlaythrough, false);
   gPrefs->Write(wxT("/AudioIO/SWPlaythrough"), !SWPlaythrough);
   gPrefs->Flush();
   ModifyAllProjectToolbarMenus();
}

#ifdef EXPERIMENTAL_AUTOMATED_INPUT_LEVEL_ADJUSTMENT
void AudacityProject::OnToogleAutomatedInputLevelAdjustment()
{
   bool AVEnabled;
   gPrefs->Read(wxT("/AudioIO/AutomatedInputLevelAdjustment"), &AVEnabled, false);
   gPrefs->Write(wxT("/AudioIO/AutomatedInputLevelAdjustment"), !AVEnabled);
   gPrefs->Flush();
   ModifyAllProjectToolbarMenus();
}
#endif

double AudacityProject::GetTime(const Track *t)
{
   double stime = 0.0;

   if (t->GetKind() == Track::Wave) {
      WaveTrack *w = (WaveTrack *)t;
      stime = w->GetEndTime();

      WaveClip *c;
      int ndx;
      for (ndx = 0; ndx < w->GetNumClips(); ndx++) {
         c = w->GetClipByIndex(ndx);
         if (c->GetNumSamples() == 0)
            continue;
         if (c->GetStartTime() < stime) {
            stime = c->GetStartTime();
         }
      }
   }
   else if (t->GetKind() == Track::Label) {
      LabelTrack *l = (LabelTrack *)t;
      stime = l->GetStartTime();
   }

   return stime;
}

//sort based on flags.  see Project.h for sort flags
void AudacityProject::SortTracks(int flags)
{
   size_t ndx = 0;
   int cmpValue;
   std::vector<ListOfTracks::iterator> arr;
   arr.reserve(mTracks->GetCount());
   bool lastTrackLinked = false;
   //sort by linked tracks. Assumes linked track follows owner in list.

   // First find the permutation.
   for (auto iter = mTracks->begin(), end = mTracks->end(); iter != end; ++iter) {
      const auto &track = *iter;
      if(lastTrackLinked) {
         //insert after the last track since this track should be linked to it.
         ndx++;
      }
      else {
         for (ndx = 0; ndx < arr.size(); ++ndx) {
            Track &arrTrack = **arr[ndx];
            if(flags & kAudacitySortByName) {
               //do case insensitive sort - cmpNoCase returns less than zero if the string is 'less than' its argument
               //also if we have case insensitive equality, then we need to sort by case as well
               //We sort 'b' before 'B' accordingly.  We uncharacteristically use greater than for the case sensitive
               //compare because 'b' is greater than 'B' in ascii.
               cmpValue = track->GetName().CmpNoCase(arrTrack.GetName());
               if (cmpValue < 0 ||
                   (0 == cmpValue && track->GetName().CompareTo(arrTrack.GetName()) > 0) )
                  break;
            }
            //sort by time otherwise
            else if(flags & kAudacitySortByTime) {
               //we have to search each track and all its linked ones to fine the minimum start time.
               double time1, time2, tempTime;
               const Track* tempTrack;
               size_t candidatesLookedAt;

               candidatesLookedAt = 0;
               tempTrack = &*track;
               time1 = time2 = std::numeric_limits<double>::max(); //TODO: find max time value. (I don't think we have one yet)
               while(tempTrack) {
                  tempTime = GetTime(tempTrack);
                  time1 = std::min(time1, tempTime);
                  if(tempTrack->GetLinked())
                     tempTrack = tempTrack->GetLink();
                  else
                     tempTrack = NULL;
               }

               //get candidate's (from sorted array) time
               tempTrack = &arrTrack;
               while(tempTrack) {
                  tempTime = GetTime(tempTrack);
                  time2 = std::min(time2, tempTime);
                  if(tempTrack->GetLinked() && (ndx+candidatesLookedAt < arr.size()-1) ) {
                     candidatesLookedAt++;
                     tempTrack = &**arr[ndx+candidatesLookedAt];
                  }
                  else
                     tempTrack = NULL;
               }

               if (time1 < time2)
                  break;

               ndx+=candidatesLookedAt;
            }
         }
      }
      arr.insert(arr.begin() + ndx, iter);

      lastTrackLinked = track->GetLinked();
   }

   // Now apply the permutation
   mTracks->Permute(arr);
}

void AudacityProject::OnSortTime()
{
   SortTracks(kAudacitySortByTime);

   PushState(_("Tracks sorted by time"), _("Sort by Time"));

   mTrackPanel->Refresh(false);
}

void AudacityProject::OnSortName()
{
   SortTracks(kAudacitySortByName);

   PushState(_("Tracks sorted by name"), _("Sort by Name"));

   mTrackPanel->Refresh(false);
}

void AudacityProject::OnSkipStart()
{
   wxCommandEvent evt;

   GetControlToolBar()->OnRewind(evt);
   ModifyState(false);
}

void AudacityProject::OnSkipEnd()
{
   wxCommandEvent evt;

   GetControlToolBar()->OnFF(evt);
   ModifyState(false);
}

void AudacityProject::OnSeekLeftShort()
{
   OnCursorLeft( false, false );
}

void AudacityProject::OnSeekRightShort()
{
   OnCursorRight( false, false );
}

void AudacityProject::OnSeekLeftLong()
{
   OnCursorLeft( true, false );
}

void AudacityProject::OnSeekRightLong()
{
   OnCursorRight( true, false );
}

void AudacityProject::OnSelToStart()
{
   Rewind(true);
   ModifyState(false);
}

void AudacityProject::OnSelToEnd()
{
   SkipEnd(true);
   ModifyState(false);
}

void AudacityProject::OnCursorUp()
{
   mTrackPanel->OnPrevTrack( false );
}

void AudacityProject::OnCursorDown()
{
   mTrackPanel->OnNextTrack( false );
}

void AudacityProject::OnFirstTrack()
{
   mTrackPanel->OnFirstTrack();
}

void AudacityProject::OnLastTrack()
{
   mTrackPanel->OnLastTrack();
}

void AudacityProject::OnShiftUp()
{
   mTrackPanel->OnPrevTrack( true );
}

void AudacityProject::OnShiftDown()
{
   mTrackPanel->OnNextTrack( true );
}

void AudacityProject::OnToggle()
{
   mTrackPanel->OnToggle( );
}

void AudacityProject::OnCursorLeft(const wxEvent * evt)
{
   OnCursorLeft( false, false, evt->GetEventType() == wxEVT_KEY_UP );
}

void AudacityProject::OnCursorRight(const wxEvent * evt)
{
   OnCursorRight( false, false, evt->GetEventType() == wxEVT_KEY_UP );
}

void AudacityProject::OnCursorShortJumpLeft()
{
   OnCursorMove( false, true, false );
}

void AudacityProject::OnCursorShortJumpRight()
{
   OnCursorMove( true, true, false );
}

void AudacityProject::OnCursorLongJumpLeft()
{
   OnCursorMove( false, true, true );
}

void AudacityProject::OnCursorLongJumpRight()
{
   OnCursorMove( true, true, true );
}

void AudacityProject::OnSelSetExtendLeft()
{
   OnBoundaryMove( true, false);
}

void AudacityProject::OnSelSetExtendRight()
{
   OnBoundaryMove( false, false);
}

void AudacityProject::OnSelExtendLeft(const wxEvent * evt)
{
   OnCursorLeft( true, false, evt->GetEventType() == wxEVT_KEY_UP );
}

void AudacityProject::OnSelExtendRight(const wxEvent * evt)
{
   OnCursorRight( true, false, evt->GetEventType() == wxEVT_KEY_UP );
}

void AudacityProject::OnSelContractLeft(const wxEvent * evt)
{
   OnCursorRight( true, true, evt->GetEventType() == wxEVT_KEY_UP );
}

void AudacityProject::OnSelContractRight(const wxEvent * evt)
{
   OnCursorLeft( true, true, evt->GetEventType() == wxEVT_KEY_UP );
}

//this pops up a dialog which allows the left selection to be set.
//If playing/recording is happening, it sets the left selection at
//the current play position.
void AudacityProject::OnSetLeftSelection()
{
   bool bSelChanged = false;
   if ((GetAudioIOToken() > 0) && gAudioIO->IsStreamActive(GetAudioIOToken()))
   {
      double indicator = gAudioIO->GetStreamTime();
      mViewInfo.selectedRegion.setT0(indicator, false);
      bSelChanged = true;
   }
   else
   {
      wxString fmt = GetSelectionFormat();
      TimeDialog dlg(this, _("Set Left Selection Boundary"),
         fmt, mRate, mViewInfo.selectedRegion.t0(), _("Position"));

      if (wxID_OK == dlg.ShowModal())
      {
         //Get the value from the dialog
         mViewInfo.selectedRegion.setT0(
            std::max(0.0, dlg.GetTimeValue()), false);
         bSelChanged = true;
      }
   }

   if (bSelChanged)
   {
      ModifyState(false);
      mTrackPanel->Refresh(false);
   }
}


void AudacityProject::OnSetRightSelection()
{
   bool bSelChanged = false;
   if ((GetAudioIOToken() > 0) && gAudioIO->IsStreamActive(GetAudioIOToken()))
   {
      double indicator = gAudioIO->GetStreamTime();
      mViewInfo.selectedRegion.setT1(indicator, false);
      bSelChanged = true;
   }
   else
   {
      wxString fmt = GetSelectionFormat();
      TimeDialog dlg(this, _("Set Right Selection Boundary"),
         fmt, mRate, mViewInfo.selectedRegion.t1(), _("Position"));

      if (wxID_OK == dlg.ShowModal())
      {
         //Get the value from the dialog
         mViewInfo.selectedRegion.setT1(
            std::max(0.0, dlg.GetTimeValue()), false);
         bSelChanged = true;
      }
   }

   if (bSelChanged)
   {
      ModifyState(false);
      mTrackPanel->Refresh(false);
   }
}

void AudacityProject::NextOrPrevFrame(bool forward)
{
   // Focus won't take in a dock unless at least one descendant window
   // accepts focus.  Tell controls to take focus for the duration of this
   // function, only.  Outside of this, they won't steal the focus when
   // clicked.
   auto temp1 = AButton::TemporarilyAllowFocus();
   auto temp2 = ASlider::TemporarilyAllowFocus();
   auto temp3 = Meter::TemporarilyAllowFocus();


   // Define the set of windows we rotate among.
   static const unsigned rotationSize = 3u;

   wxWindow *const begin [rotationSize] = {
      GetTopPanel(),
      GetTrackPanel(),
      mToolManager->GetBotDock(),
   };
   const auto end = begin + rotationSize;

   // helper functions
   auto IndexOf = [&](wxWindow *pWindow) {
      return std::find(begin, end, pWindow) - begin;
   };

   auto FindAncestor = [&]() {
      wxWindow *pWindow = wxWindow::FindFocus();
      unsigned index = rotationSize;
      while ( pWindow &&
              (rotationSize == (index = IndexOf(pWindow) ) ) )
         pWindow = pWindow->GetParent();
      return index;
   };

   const auto idx = FindAncestor();
   if (idx == rotationSize)
      return;

   auto idx2 = idx;
   auto increment = (forward ? 1 : rotationSize - 1);

   while( idx != (idx2 = (idx2 + increment) % rotationSize) ) {
      wxWindow *toFocus = begin[idx2];
      toFocus->SetFocus();
      if ( FindAncestor() == idx2 )
         // The focus took!
         break;
      // else, one of the tool docks might be empty because all bars were
      // dragged off it.  Skip it and try another.
   }
}

void AudacityProject::NextFrame()
{
   NextOrPrevFrame(true);
}

void AudacityProject::PrevFrame()
{
   NextOrPrevFrame(false);
}

void AudacityProject::NextWindow()
{
   wxWindow *w = wxGetTopLevelParent(wxWindow::FindFocus());
   const auto & list = GetChildren();
   auto iter = list.begin(), end = list.end();

   // If the project window has the current focus, start the search with the first child
   if (w == this)
   {
   }
   // Otherwise start the search with the current window's next sibling
   else
   {
      // Find the window in this projects children.  If the window with the
      // focus isn't a child of this project (like when a dialog is created
      // without specifying a parent), then we'll get back NULL here.
      while (iter != end && *iter != w)
         ++iter;
      if (iter != end)
         ++iter;
   }

   // Search for the next toplevel window
   for (; iter != end; ++iter)
   {
      // If it's a toplevel, visible (we have hidden windows) and is enabled,
      // then we're done.  The IsEnabled() prevents us from moving away from 
      // a modal dialog because all other toplevel windows will be disabled.
      w = *iter;
      if (w->IsTopLevel() && w->IsShown() && w->IsEnabled())
      {
         break;
      }
   }

   // Ran out of siblings, so make the current project active
   if ((iter == end) && IsEnabled())
   {
      w = this;
   }

   // And make sure it's on top (only for floating windows...project window will not raise)
   // (Really only works on Windows)
   w->Raise();

#ifdef __WXMAC__
   // bug 868
   // Simulate a TAB key press before continuing, else the cycle of
   // navigation among top level windows stops because the keystrokes don't
   // go to the CommandManager.
   if (dynamic_cast<wxDialog*>(w)) {
      w->NavigateIn();
   }
#endif
}

void AudacityProject::PrevWindow()
{
   wxWindow *w = wxGetTopLevelParent(wxWindow::FindFocus());
   const auto & list = GetChildren();
   auto iter = list.rbegin(), end = list.rend();

   // If the project window has the current focus, start the search with the last child
   if (w == this)
   {
   }
   // Otherwise start the search with the current window's previous sibling
   else
   {
      while (iter != end && *iter != w)
         ++iter;
      if (iter != end)
         ++iter;
   }

   // Search for the previous toplevel window
   for (; iter != end; ++iter)
   {
      // If it's a toplevel and is visible (we have come hidden windows), then we're done
      w = *iter;
      if (w->IsTopLevel() && w->IsShown() && IsEnabled())
      {
         break;
      }
   }

   // Ran out of siblings, so make the current project active
   if ((iter == end) && IsEnabled())
   {
      w = this;
   }

   // And make sure it's on top (only for floating windows...project window will not raise)
   // (Really only works on Windows)
   w->Raise();

#ifdef __WXMAC__
   // bug 868
   // Simulate a TAB key press before continuing, else the cycle of
   // navigation among top level windows stops because the keystrokes don't
   // go to the CommandManager.
   if (dynamic_cast<wxDialog*>(w)) {
      w->NavigateIn();
   }
#endif
}

//The following methods operate controls on specified tracks,
//This will pop up the track panning dialog for specified track
void AudacityProject::OnTrackPan()
{
   Track *const track = mTrackPanel->GetFocusedTrack();
   if (!track || (track->GetKind() != Track::Wave)) {
      return;
   }
   const auto wt = static_cast<WaveTrack*>(track);

   LWSlider *slider = mTrackPanel->GetTrackInfo()->PanSlider(wt);
   if (slider->ShowDialog()) {
      SetTrackPan(wt, slider);
   }
}

void AudacityProject::OnTrackPanLeft()
{
   Track *const track = mTrackPanel->GetFocusedTrack();
   if (!track || (track->GetKind() != Track::Wave)) {
      return;
   }
   const auto wt = static_cast<WaveTrack*>(track);

   LWSlider *slider = mTrackPanel->GetTrackInfo()->PanSlider(wt);
   slider->Decrease(1);
   SetTrackPan(wt, slider);
}

void AudacityProject::OnTrackPanRight()
{
   Track *const track = mTrackPanel->GetFocusedTrack();
   if (!track || (track->GetKind() != Track::Wave)) {
      return;
   }
   const auto wt = static_cast<WaveTrack*>(track);

   LWSlider *slider = mTrackPanel->GetTrackInfo()->PanSlider(wt);
   slider->Increase(1);
   SetTrackPan(wt, slider);
}

void AudacityProject::OnTrackGain()
{
   /// This will pop up the track gain dialog for specified track
   Track *const track = mTrackPanel->GetFocusedTrack();
   if (!track || (track->GetKind() != Track::Wave)) {
      return;
   }
   const auto wt = static_cast<WaveTrack*>(track);

   LWSlider *slider = mTrackPanel->GetTrackInfo()->GainSlider(wt);
   if (slider->ShowDialog()) {
      SetTrackGain(wt, slider);
   }
}

void AudacityProject::OnTrackGainInc()
{
   Track *const track = mTrackPanel->GetFocusedTrack();
   if (!track || (track->GetKind() != Track::Wave)) {
      return;
   }
   const auto wt = static_cast<WaveTrack*>(track);

   LWSlider *slider = mTrackPanel->GetTrackInfo()->GainSlider(wt);
   slider->Increase(1);
   SetTrackGain(wt, slider);
}

void AudacityProject::OnTrackGainDec()
{
   Track *const track = mTrackPanel->GetFocusedTrack();
   if (!track || (track->GetKind() != Track::Wave)) {
      return;
   }
   const auto wt = static_cast<WaveTrack*>(track);

   LWSlider *slider = mTrackPanel->GetTrackInfo()->GainSlider(wt);
   slider->Decrease(1);
   SetTrackGain(wt, slider);
}

void AudacityProject::OnTrackMenu()
{
   mTrackPanel->OnTrackMenu();
}

void AudacityProject::OnTrackMute()
{
   Track *t = NULL;
   if (!t) {
      t = mTrackPanel->GetFocusedTrack();
      if (!t || (t->GetKind() != Track::Wave))
         return;
   }
   DoTrackMute(t, false);
}

void AudacityProject::OnTrackSolo()
{
   Track *t = NULL;
   if (!t)
   {
      t = mTrackPanel->GetFocusedTrack();
      if (!t || (t->GetKind() != Track::Wave))
         return;
   }
   DoTrackSolo(t, false);
}

void AudacityProject::OnTrackClose()
{
   Track *t = mTrackPanel->GetFocusedTrack();
   if (!t)
      return;

   if (IsAudioActive())
   {
      this->TP_DisplayStatusMessage(_("Can't delete track with active audio"));
      wxBell();
      return;
   }

   RemoveTrack(t);

   GetTrackPanel()->UpdateViewIfNoTracks();
   GetTrackPanel()->Refresh(false);
}

void AudacityProject::OnTrackMoveUp()
{
   Track *const focusedTrack = mTrackPanel->GetFocusedTrack();
   if (mTracks->CanMoveUp(focusedTrack)) {
      MoveTrack(focusedTrack, OnMoveUpID);
      mTrackPanel->Refresh(false);
   }
}

void AudacityProject::OnTrackMoveDown()
{
   Track *const focusedTrack = mTrackPanel->GetFocusedTrack();
   if (mTracks->CanMoveDown(focusedTrack)) {
      MoveTrack(focusedTrack, OnMoveDownID);
      mTrackPanel->Refresh(false);
   }
}

void AudacityProject::OnTrackMoveTop()
{
   Track *const focusedTrack = mTrackPanel->GetFocusedTrack();
   if (mTracks->CanMoveUp(focusedTrack)) {
      MoveTrack(focusedTrack, OnMoveTopID);
      mTrackPanel->Refresh(false);
   }
}

void AudacityProject::OnTrackMoveBottom()
{
   Track *const focusedTrack = mTrackPanel->GetFocusedTrack();
   if (mTracks->CanMoveDown(focusedTrack)) {
      MoveTrack(focusedTrack, OnMoveBottomID);
      mTrackPanel->Refresh(false);
   }
}

/// Move a track up, down, to top or to bottom.

void AudacityProject::MoveTrack(Track* target, MoveChoice choice)
{
   wxString direction;

   switch (choice)
   {
   case OnMoveTopID:
      /* i18n-hint: where the track is moving to.*/
      direction = _("to Top");

      while (mTracks->CanMoveUp(target)) {
         if (mTracks->Move(target, true)) {
            MixerBoard* pMixerBoard = this->GetMixerBoard(); // Update mixer board.
            if (pMixerBoard && (target->GetKind() == Track::Wave))
               pMixerBoard->MoveTrackCluster((WaveTrack*)target, true);
         }
      }
      break;
   case OnMoveBottomID:
      /* i18n-hint: where the track is moving to.*/
      direction = _("to Bottom");

      while (mTracks->CanMoveDown(target)) {
         if (mTracks->Move(target, false)) {
            MixerBoard* pMixerBoard = this->GetMixerBoard(); // Update mixer board.
            if (pMixerBoard && (target->GetKind() == Track::Wave))
               pMixerBoard->MoveTrackCluster((WaveTrack*)target, false);
         }
      }
      break;
   default:
      bool bUp = (OnMoveUpID == choice);
      /* i18n-hint: a direction.*/
      direction = bUp ? _("Up") : _("Down");

      if (mTracks->Move(target, bUp)) {
         MixerBoard* pMixerBoard = this->GetMixerBoard();
         if (pMixerBoard && (target->GetKind() == Track::Wave)) {
            pMixerBoard->MoveTrackCluster((WaveTrack*)target, bUp);
         }
      }
   }

   /* i18n-hint: Past tense of 'to move', as in 'moved audio track up'.*/
   wxString longDesc = (_("Moved"));
   /* i18n-hint: The direction of movement will be up, down, to top or to bottom.. */
   wxString shortDesc = (_("Move Track"));

   longDesc = (wxString::Format(wxT("%s '%s' %s"), longDesc.c_str(),
      target->GetName().c_str(), direction.c_str()));
   shortDesc = (wxString::Format(wxT("%s %s"), shortDesc.c_str(), direction.c_str()));

   PushState(longDesc, shortDesc);
   GetTrackPanel()->Refresh(false);
}

void AudacityProject::OnInputDevice()
{
   DeviceToolBar *tb = GetDeviceToolBar();
   if (tb) {
      tb->ShowInputDialog();
   }
}

void AudacityProject::OnOutputDevice()
{
   DeviceToolBar *tb = GetDeviceToolBar();
   if (tb) {
      tb->ShowOutputDialog();
   }
}

void AudacityProject::OnAudioHost()
{
   DeviceToolBar *tb = GetDeviceToolBar();
   if (tb) {
      tb->ShowHostDialog();
   }
}

void AudacityProject::OnInputChannels()
{
   DeviceToolBar *tb = GetDeviceToolBar();
   if (tb) {
      tb->ShowChannelsDialog();
   }
}

void AudacityProject::OnOutputGain()
{
   MixerToolBar *tb = GetMixerToolBar();
   if (tb) {
      tb->ShowOutputGainDialog();
   }
}

void AudacityProject::OnInputGain()
{
   MixerToolBar *tb = GetMixerToolBar();
   if (tb) {
      tb->ShowInputGainDialog();
   }
}

void AudacityProject::OnOutputGainInc()
{
   MixerToolBar *tb = GetMixerToolBar();
   if (tb) {
      tb->AdjustOutputGain(1);
   }
}

void AudacityProject::OnOutputGainDec()
{
   MixerToolBar *tb = GetMixerToolBar();
   if (tb) {
      tb->AdjustOutputGain(-1);
   }
}

void AudacityProject::OnInputGainInc()
{
   MixerToolBar *tb = GetMixerToolBar();
   if (tb) {
      tb->AdjustInputGain(1);
   }
}

void AudacityProject::OnInputGainDec()
{
   MixerToolBar *tb = GetMixerToolBar();
   if (tb) {
      tb->AdjustInputGain(-1);
   }
}

void AudacityProject::OnPlayAtSpeed()
{
   TranscriptionToolBar *tb = GetTranscriptionToolBar();
   if (tb) {
      tb->PlayAtSpeed(false, false);
   }
}

void AudacityProject::OnPlayAtSpeedLooped()
{
   TranscriptionToolBar *tb = GetTranscriptionToolBar();
   if (tb) {
      tb->PlayAtSpeed(true, false);
   }
}

void AudacityProject::OnPlayAtSpeedCutPreview()
{
   TranscriptionToolBar *tb = GetTranscriptionToolBar();
   if (tb) {
      tb->PlayAtSpeed(false, true);
   }
}

void AudacityProject::OnSetPlaySpeed()
{
   TranscriptionToolBar *tb = GetTranscriptionToolBar();
   if (tb) {
      tb->ShowPlaySpeedDialog();
   }
}

void AudacityProject::OnPlaySpeedInc()
{
   TranscriptionToolBar *tb = GetTranscriptionToolBar();
   if (tb) {
      tb->AdjustPlaySpeed(0.1f);
   }
}

void AudacityProject::OnPlaySpeedDec()
{
   TranscriptionToolBar *tb = GetTranscriptionToolBar();
   if (tb) {
      tb->AdjustPlaySpeed(-0.1f);
   }
}

double AudacityProject::NearestZeroCrossing(double t0)
{
   // Window is 1/100th of a second.
   int windowSize = (int)(GetRate() / 100);
   float *dist = new float[windowSize];
   int i, j;

   for(i=0; i<windowSize; i++)
      dist[i] = 0.0;

   TrackListIterator iter(GetTracks());
   Track *track = iter.First();
   while (track) {
      if (!track->GetSelected() || track->GetKind() != (Track::Wave)) {
         track = iter.Next();
         continue;
      }
      WaveTrack *one = (WaveTrack *)track;
      int oneWindowSize = (int)(one->GetRate() / 100);
      float *oneDist = new float[oneWindowSize];
      auto s = one->TimeToLongSamples(t0);
      // fillTwo to ensure that missing values are treated as 2, and hence do not
      // get used as zero crossings.
      one->Get((samplePtr)oneDist, floatSample,
               s - oneWindowSize/2, oneWindowSize, fillTwo);

      // Start by penalizing downward motion.  We prefer upward
      // zero crossings.
      if (oneDist[1] - oneDist[0] < 0)
         oneDist[0] = oneDist[0]*6 + (oneDist[0] > 0 ? 0.3 : -0.3);
      for(i=1; i<oneWindowSize; i++)
         if (oneDist[i] - oneDist[i-1] < 0)
            oneDist[i] = oneDist[i]*6 + (oneDist[i] > 0 ? 0.3 : -0.3);

      // Taking the absolute value -- apply a tiny LPF so square waves work.
      float newVal, oldVal = oneDist[0];
      oneDist[0] = fabs(.75 * oneDist[0] + .25 * oneDist[1]);
      for(i=1; i<oneWindowSize-1; i++)
      {
         newVal = fabs(.25 * oldVal + .5 * oneDist[i] + .25 * oneDist[i+1]);
         oldVal = oneDist[i];
         oneDist[i] = newVal;
      }
      oneDist[oneWindowSize-1] = fabs(.25 * oldVal +
            .75 * oneDist[oneWindowSize-1]);

      // TODO: The mixed rate zero crossing code is broken,
      // if oneWindowSize > windowSize we'll miss out some
      // samples - so they will still be zero, so we'll use them.
      for(i=0; i<windowSize; i++) {
         if (windowSize != oneWindowSize)
            j = i * (oneWindowSize-1) / (windowSize-1);
         else
            j = i;

         dist[i] += oneDist[j];
         // Apply a small penalty for distance from the original endpoint
         dist[i] += 0.1 * (abs(i - windowSize/2)) / float(windowSize/2);
      }

      delete [] oneDist;
      track = iter.Next();
   }

   // Find minimum
   int argmin = 0;
   float min = 3.0;
   for(i=0; i<windowSize; i++) {
      if (dist[i] < min) {
         argmin = i;
         min = dist[i];
      }
   }

   delete [] dist;

   return t0 + (argmin - windowSize/2)/GetRate();
}

void AudacityProject::OnZeroCrossing()
{
   const double t0 = NearestZeroCrossing(mViewInfo.selectedRegion.t0());
   if (mViewInfo.selectedRegion.isPoint())
      mViewInfo.selectedRegion.setTimes(t0, t0);
   else {
      const double t1 = NearestZeroCrossing(mViewInfo.selectedRegion.t1());
      mViewInfo.selectedRegion.setTimes(t0, t1);
   }

   ModifyState(false);

   mTrackPanel->Refresh(false);
}

//
// Effect Menus
//

/// OnEffect() takes a PluginID and has the EffectManager execute the assocated effect.
///
/// At the moment flags are used only to indicate whether to prompt for parameters,
/// whether to save the state to history and whether to allow 'Repeat Last Effect'.
bool AudacityProject::OnEffect(const PluginID & ID, int flags)
{
   const PluginDescriptor *plug = PluginManager::Get().GetPlugin(ID);
   if (!plug)
      return false;

   EffectType type = plug->GetEffectType();

   // Make sure there's no activity since the effect is about to be applied
   // to the project's tracks.  Mainly for Apply during RTP, but also used
   // for batch commands
   if (flags & OnEffectFlags::kConfigured)
   {
      OnStop();
      SelectAllIfNone();
   }

   wxGetApp().SetMissingAliasedFileWarningShouldShow(true);

   TrackListIterator iter(GetTracks());
   Track *t = iter.First();
   WaveTrack *newTrack{};
   wxWindow *focus = wxWindow::FindFocus();

   //double prevEndTime = mTracks->GetEndTime();
   int count = 0;
   bool clean = true;
   while (t) {
      if (t->GetSelected() && t->GetKind() == (Track::Wave)) {
         if (t->GetEndTime() != 0.0) clean = false;
         count++;
      }
      t = iter.Next();
   }

   if (count == 0) {
      // No tracks were selected...
      if (type == EffectTypeGenerate) {
         // Create a NEW track for the generated audio...
         newTrack = static_cast<WaveTrack*>(mTracks->Add(mTrackFactory->NewWaveTrack()));
         newTrack->SetSelected(true);
      }
   }

   EffectManager & em = EffectManager::Get();

   bool success = em.DoEffect(ID, this, mRate,
                               GetTracks(), GetTrackFactory(),
                               &mViewInfo.selectedRegion,
                               (flags & OnEffectFlags::kConfigured) == 0);

   if (!success) {
      if (newTrack) {
         mTracks->Remove(newTrack);
         mTrackPanel->Refresh(false);
      }

      // For now, we're limiting realtime preview to a single effect, so
      // make sure the menus reflect that fact that one may have just been
      // opened.
      UpdateMenus(false);

      return false;
   }

   if (em.GetSkipStateFlag())
      flags = flags | OnEffectFlags::kSkipState;

   if (!(flags & OnEffectFlags::kSkipState))
   {
      wxString shortDesc = em.GetEffectName(ID);
      wxString longDesc = em.GetEffectDescription(ID);
      PushState(longDesc, shortDesc);
   }

   if (!(flags & OnEffectFlags::kDontRepeatLast))
   {
      // Only remember a successful effect, don't rmemeber insert,
      // or analyze effects.
      if (type == EffectTypeProcess) {
         wxString shortDesc = em.GetEffectName(ID);
         mLastEffect = ID;
         wxString lastEffectDesc;
         /* i18n-hint: %s will be the name of the effect which will be
          * repeated if this menu item is chosen */
         lastEffectDesc.Printf(_("Repeat %s"), shortDesc.c_str());
         mCommandManager.Modify(wxT("RepeatLastEffect"), lastEffectDesc);
      }
   }

   //STM:
   //The following automatically re-zooms after sound was generated.
   // IMO, it was disorienting, removing to try out without re-fitting
   //mchinen:12/14/08 reapplying for generate effects
   if (type == EffectTypeGenerate)
   {
      if (count == 0 || (clean && mViewInfo.selectedRegion.t0() == 0.0))
         OnZoomFit();
         //  mTrackPanel->Refresh(false);
   }
   RedrawProject();
   if (focus != NULL) {
      focus->SetFocus();
   }
   mTrackPanel->EnsureVisible(mTrackPanel->GetFirstSelectedTrack());

   mTrackPanel->Refresh(false);

   return true;
}

void AudacityProject::OnRepeatLastEffect(int WXUNUSED(index))
{
   if (!mLastEffect.IsEmpty())
   {
      OnEffect(mLastEffect, OnEffectFlags::kConfigured);
   }
}




void AudacityProject::OnManagePluginsMenu(EffectType type)
{
   if (PluginManager::Get().ShowManager(this, type))
   {
      for (size_t i = 0; i < gAudacityProjects.size(); i++) {
         AudacityProject *p = gAudacityProjects[i].get();

         p->RebuildMenuBar();
#if defined(__WXGTK__)
         // Workaround for:
         //
         //   http://bugzilla.audacityteam.org/show_bug.cgi?id=458
         //
         // This workaround should be removed when Audacity updates to wxWidgets 3.x which has a fix.
         wxRect r = p->GetRect();
         p->SetSize(wxSize(1,1));
         p->SetSize(r.GetSize());
#endif
      }
   }
}

void AudacityProject::OnManageGenerators()
{
   OnManagePluginsMenu(EffectTypeGenerate);
}

void AudacityProject::OnManageEffects()
{
   OnManagePluginsMenu(EffectTypeProcess);
}

void AudacityProject::OnManageAnalyzers()
{
   OnManagePluginsMenu(EffectTypeAnalyze);
}



void AudacityProject::OnStereoToMono(int WXUNUSED(index))
{
   OnEffect(EffectManager::Get().GetEffectByIdentifier(wxT("StereoToMono")),
            OnEffectFlags::kConfigured);
}

//
// File Menu
//

void AudacityProject::OnNew()
{
   CreateNewAudacityProject();
}

void AudacityProject::OnOpen()
{
   OpenFiles(this);
}

void AudacityProject::OnClose()
{
   mMenuClose = true;
   Close();
}

void AudacityProject::OnSave()
{
   Save();
}

void AudacityProject::OnSaveAs()
{
   SaveAs();
}

#ifdef USE_LIBVORBIS
   void AudacityProject::OnSaveCompressed()
   {
      SaveAs(true);
   }
#endif

void AudacityProject::OnCheckDependencies()
{
   ShowDependencyDialogIfNeeded(this, false);
}

void AudacityProject::OnExit()
{
   QuitAudacity();
}

void AudacityProject::OnExportLabels()
{
   Track *t;
   int numLabelTracks = 0;

   TrackListIterator iter(GetTracks());

   wxString fName = _("labels.txt");
   t = iter.First();
   while (t) {
      if (t->GetKind() == Track::Label)
      {
         numLabelTracks++;
         fName = t->GetName();
      }
      t = iter.Next();
   }

   if (numLabelTracks == 0) {
      wxMessageBox(_("There are no label tracks to export."));
      return;
   }

   fName = FileSelector(_("Export Labels As:"),
                        wxEmptyString,
                        fName,
                        wxT("txt"),
                        wxT("*.txt"),
                        wxFD_SAVE | wxFD_OVERWRITE_PROMPT | wxRESIZE_BORDER,
                        this);

   if (fName == wxT(""))
      return;

   // Move existing files out of the way.  Otherwise wxTextFile will
   // append to (rather than replace) the current file.

   if (wxFileExists(fName)) {
#ifdef __WXGTK__
      wxString safetyFileName = fName + wxT("~");
#else
      wxString safetyFileName = fName + wxT(".bak");
#endif

      if (wxFileExists(safetyFileName))
         wxRemoveFile(safetyFileName);

      wxRename(fName, safetyFileName);
   }

   wxTextFile f(fName);
   f.Create();
   f.Open();
   if (!f.IsOpened()) {
      wxMessageBox(_("Couldn't write to file: ") + fName);
      return;
   }

   t = iter.First();
   while (t) {
      if (t->GetKind() == Track::Label)
         ((LabelTrack *) t)->Export(f);

      t = iter.Next();
   }

   f.Write();
   f.Close();
}


#ifdef USE_MIDI
void AudacityProject::OnExportMIDI(){
   TrackListIterator iter(GetTracks());
   Track *t = iter.First();
   int numNoteTracksSelected = 0;
   NoteTrack *nt = NULL;

   // Iterate through once to make sure that there is
   // exactly one NoteTrack selected.
   while (t) {
      if (t->GetSelected()) {
         if(t->GetKind() == Track::Note) {
            numNoteTracksSelected++;
            nt = (NoteTrack *) t;
         }
      }
      t = iter.Next();
   }

   if(numNoteTracksSelected > 1){
      wxMessageBox(wxString::Format(wxT(
         "Please select only one MIDI track at a time.")));
      return;
   }

   wxASSERT(nt);
   if (!nt)
      return;

   while(true){

      wxString fName = wxT("");

      fName = FileSelector(_("Export MIDI As:"),
         wxEmptyString,
         fName,
         wxT(".mid|.gro"),
         _("MIDI file (*.mid)|*.mid|Allegro file (*.gro)|*.gro"),
         wxFD_SAVE | wxFD_OVERWRITE_PROMPT | wxRESIZE_BORDER,
         this);

      if (fName == wxT(""))
         return;

      if(!fName.Contains(wxT("."))) {
         fName = fName + wxT(".mid");
      }

      // Move existing files out of the way.  Otherwise wxTextFile will
      // append to (rather than replace) the current file.

      if (wxFileExists(fName)) {
#ifdef __WXGTK__
         wxString safetyFileName = fName + wxT("~");
#else
         wxString safetyFileName = fName + wxT(".bak");
#endif

         if (wxFileExists(safetyFileName))
            wxRemoveFile(safetyFileName);

         wxRename(fName, safetyFileName);
      }

      if(fName.EndsWith(wxT(".mid")) || fName.EndsWith(wxT(".midi"))) {
         nt->ExportMIDI(fName);
      } else if(fName.EndsWith(wxT(".gro"))) {
         nt->ExportAllegro(fName);
      } else {
         wxString msg = _("You have selected a filename with an unrecognized file extension.\nDo you want to continue?");
         wxString title = _("Export MIDI");
         int id = wxMessageBox(msg, title, wxYES_NO);
         if (id == wxNO) {
            continue;
         } else if (id == wxYES) {
            nt->ExportMIDI(fName);
         }
      }
      break;
   }
}
#endif // USE_MIDI


void AudacityProject::OnExport()
{
   Exporter e;

   wxGetApp().SetMissingAliasedFileWarningShouldShow(true);
   e.Process(this, false, 0.0, mTracks->GetEndTime());
}

void AudacityProject::OnExportSelection()
{
   Exporter e;

   wxGetApp().SetMissingAliasedFileWarningShouldShow(true);
   e.SetFileDialogTitle( _("Export Selected Audio") );
   e.Process(this, true, mViewInfo.selectedRegion.t0(),
      mViewInfo.selectedRegion.t1());
}

void AudacityProject::OnExportMultiple()
{
   ExportMultiple em(this);

   wxGetApp().SetMissingAliasedFileWarningShouldShow(true);
   em.ShowModal();
}

void AudacityProject::OnPreferences()
{
   GlobalPrefsDialog dialog(this /* parent */ );

   if (!dialog.ShowModal()) {
      // Canceled
      return;
   }

   // LL:  Moved from PrefsDialog since wxWidgets on OSX can't deal with
   //      rebuilding the menus while the PrefsDialog is still in the modal
   //      state.
   for (size_t i = 0; i < gAudacityProjects.size(); i++) {
      AudacityProject *p = gAudacityProjects[i].get();

      p->RebuildMenuBar();
      p->RebuildOtherMenus();
#if defined(__WXGTK__)
      // Workaround for:
      //
      //   http://bugzilla.audacityteam.org/show_bug.cgi?id=458
      //
      // This workaround should be removed when Audacity updates to wxWidgets 3.x which has a fix.
      wxRect r = p->GetRect();
      p->SetSize(wxSize(1,1));
      p->SetSize(r.GetSize());
#endif
   }
}

void AudacityProject::OnPageSetup()
{
   HandlePageSetup(this);
}

void AudacityProject::OnPrint()
{
   HandlePrint(this, GetName(), GetTracks());
}

//
// Edit Menu
//

void AudacityProject::OnUndo()
{
   if (!GetUndoManager()->UndoAvailable()) {
      wxMessageBox(_("Nothing to undo"));
      return;
   }

   // can't undo while dragging
   if (mTrackPanel->IsMouseCaptured()) {
      return;
   }

   const UndoState &state = GetUndoManager()->Undo(&mViewInfo.selectedRegion);
   PopState(state);

   mTrackPanel->SetFocusedTrack(NULL);
   mTrackPanel->EnsureVisible(mTrackPanel->GetFirstSelectedTrack());

   RedrawProject();

   if (mHistoryWindow)
      mHistoryWindow->UpdateDisplay();

   if (mMixerBoard)
      // Mixer board may need to change for selection state and pan/gain
      mMixerBoard->Refresh();

   ModifyUndoMenuItems();
}

void AudacityProject::OnRedo()
{
   if (!GetUndoManager()->RedoAvailable()) {
      wxMessageBox(_("Nothing to redo"));
      return;
   }
   // Can't redo whilst dragging
   if (mTrackPanel->IsMouseCaptured()) {
      return;
   }

   const UndoState &state = GetUndoManager()->Redo(&mViewInfo.selectedRegion);
   PopState(state);

   mTrackPanel->SetFocusedTrack(NULL);
   mTrackPanel->EnsureVisible(mTrackPanel->GetFirstSelectedTrack());

   RedrawProject();

   if (mHistoryWindow)
      mHistoryWindow->UpdateDisplay();

   if (mMixerBoard)
      // Mixer board may need to change for selection state and pan/gain
      mMixerBoard->Refresh();

   ModifyUndoMenuItems();
}

void AudacityProject::OnCut()
{
   TrackListIterator iter(GetTracks());
   Track *n = iter.First();

   // This doesn't handle cutting labels, it handles
   // cutting the _text_ inside of labels, i.e. if you're
   // in the middle of editing the label text and select "Cut".

   while (n) {
      if (n->GetSelected()) {
         if (n->GetKind() == Track::Label) {
            if (((LabelTrack *)n)->CutSelectedText()) {
               mTrackPanel->Refresh(false);
               return;
            }
         }
      }
      n = iter.Next();
   }

   ClearClipboard();
   n = iter.First();
   while (n) {
      if (n->GetSelected()) {
         Track::Holder dest;
#if defined(USE_MIDI)
         if (n->GetKind() == Track::Note)
            // Since portsmf has a built-in cut operator, we use that instead
            dest = n->Cut(mViewInfo.selectedRegion.t0(),
                   mViewInfo.selectedRegion.t1());
         else
#endif
            dest = n->Copy(mViewInfo.selectedRegion.t0(),
                    mViewInfo.selectedRegion.t1());

         if (dest) {
            dest->SetChannel(n->GetChannel());
            dest->SetLinked(n->GetLinked());
            dest->SetName(n->GetName());
            msClipboard->Add(std::move(dest));
         }
      }
      n = iter.Next();
   }

   n = iter.First();
   while (n) {
      // We clear from selected and sync-lock selected tracks.
      if (n->GetSelected() || n->IsSyncLockSelected()) {
         switch (n->GetKind())
         {
#if defined(USE_MIDI)
            case Track::Note:
               //if NoteTrack, it was cut, so do not clear anything
            break;
#endif
            case Track::Wave:
               if (gPrefs->Read(wxT("/GUI/EnableCutLines"), (long)0)) {
                  ((WaveTrack*)n)->ClearAndAddCutLine(
                     mViewInfo.selectedRegion.t0(),
                     mViewInfo.selectedRegion.t1());
                  break;
               }

               // Fall through

            default:
               n->Clear(mViewInfo.selectedRegion.t0(),
                        mViewInfo.selectedRegion.t1());
            break;
         }
      }
      n = iter.Next();
   }

   msClipT0 = mViewInfo.selectedRegion.t0();
   msClipT1 = mViewInfo.selectedRegion.t1();
   msClipProject = this;

   PushState(_("Cut to the clipboard"), _("Cut"));

   RedrawProject();

   mViewInfo.selectedRegion.collapseToT0();
}


void AudacityProject::OnSplitCut()
{
   TrackListIterator iter(GetTracks());
   Track *n = iter.First();

   ClearClipboard();
   while (n) {
      if (n->GetSelected()) {
         Track::Holder dest;
         if (n->GetKind() == Track::Wave)
         {
            dest = ((WaveTrack*)n)->SplitCut(
               mViewInfo.selectedRegion.t0(),
               mViewInfo.selectedRegion.t1());
         }
         else
         {
            dest = n->Copy(mViewInfo.selectedRegion.t0(),
                    mViewInfo.selectedRegion.t1());
            n->Silence(mViewInfo.selectedRegion.t0(),
                       mViewInfo.selectedRegion.t1());
         }
         if (dest) {
            dest->SetChannel(n->GetChannel());
            dest->SetLinked(n->GetLinked());
            dest->SetName(n->GetName());
            msClipboard->Add(std::move(dest));
         }
      }
      n = iter.Next();
   }

   msClipT0 = mViewInfo.selectedRegion.t0();
   msClipT1 = mViewInfo.selectedRegion.t1();
   msClipProject = this;

   PushState(_("Split-cut to the clipboard"), _("Split Cut"));

   RedrawProject();
}


void AudacityProject::OnCopy()
{

   TrackListIterator iter(GetTracks());

   Track *n = iter.First();

   while (n) {
      if (n->GetSelected()) {
         if (n->GetKind() == Track::Label) {
            if (((LabelTrack *)n)->CopySelectedText()) {
               //mTrackPanel->Refresh(false);
               return;
            }
         }
      }
      n = iter.Next();
   }

   ClearClipboard();
   n = iter.First();
   while (n) {
      if (n->GetSelected()) {
         auto dest = n->Copy(mViewInfo.selectedRegion.t0(),
                 mViewInfo.selectedRegion.t1());
         if (dest) {
            dest->SetChannel(n->GetChannel());
            dest->SetLinked(n->GetLinked());
            dest->SetName(n->GetName());
            msClipboard->Add(std::move(dest));
         }
      }
      n = iter.Next();
   }

   msClipT0 = mViewInfo.selectedRegion.t0();
   msClipT1 = mViewInfo.selectedRegion.t1();
   msClipProject = this;

   //Make sure the menus/toolbar states get updated
   mTrackPanel->Refresh(false);
}

void AudacityProject::OnPaste()
{
   // Handle text paste (into active label) first.
   if (this->HandlePasteText())
      return;

   // If nothing's selected, we just insert NEW tracks.
   if (this->HandlePasteNothingSelected())
      return;

   // Otherwise, paste into the selected tracks.
   double t0 = mViewInfo.selectedRegion.t0();
   double t1 = mViewInfo.selectedRegion.t1();

   TrackListIterator iter(GetTracks());
   TrackListIterator clipIter(msClipboard.get());

   Track *n = iter.First();
   const Track *c = clipIter.First();
   if (c == NULL)
      return;
   Track *ff = NULL;
   const Track *tmpSrc = NULL;
   const Track *tmpC = NULL;
   const Track *prev = NULL;

   bool bAdvanceClipboard = true;
   bool bPastedSomething = false;
   bool bTrackTypeMismatch = false;

   while (n && c) {
      if (n->GetSelected()) {
         bAdvanceClipboard = true;
         if (tmpC)
            c = tmpC;
         if (c->GetKind() != n->GetKind()) {
            if (!bTrackTypeMismatch) {
               tmpSrc = prev;
               tmpC = c;
            }
            bTrackTypeMismatch = true;
            bAdvanceClipboard = false;
            c = tmpSrc;

            // If the types still don't match...
            while (c && c->GetKind() != n->GetKind()) {
               prev = c;
               c = clipIter.Next();
            }
         }

         // Handle case where the first track in clipboard
         // is of different type than the first selected track
         if (!c) {
            c = tmpC;
            while (n && (c->GetKind() != n->GetKind() || !n->GetSelected()))
            {
               // Must perform sync-lock adjustment before incrementing n
               if (n->IsSyncLockSelected()) {
                  bPastedSomething |= n->SyncLockAdjust(t1, t0+(msClipT1 - msClipT0));
               }
               n = iter.Next();
            }
            if (!n)
               c = NULL;
         }

         // The last possible case for cross-type pastes: triggered when we try to
         // paste 1+ tracks from one type into 1+ tracks of another type. If
         // there's a mix of types, this shouldn't run.
         if (!c) {
            wxMessageBox(
               _("Pasting one type of track into another is not allowed."),
               _("Error"), wxICON_ERROR, this);
            c = n;//so we don't trigger any !c conditions on our way out
            break;
         }

         // When trying to copy from stereo to mono track, show error and exit
         // TODO: Automatically offer user to mix down to mono (unfortunately
         //       this is not easy to implement
         if (c->GetLinked() && !n->GetLinked())
         {
            wxMessageBox(
               _("Copying stereo audio into a mono track is not allowed."),
               _("Error"), wxICON_ERROR, this);
            break;
         }

         if (!ff)
            ff = n;

         Maybe<WaveTrack::Locker> locker;
         if (msClipProject != this && c->GetKind() == Track::Wave)
            locker.create(static_cast<const WaveTrack*>(c));

         if (c->GetKind() == Track::Wave && n && n->GetKind() == Track::Wave)
         {
            bPastedSomething |=
               ((WaveTrack*)n)->ClearAndPaste(t0, t1, (WaveTrack*)c, true, true);
         }
         else if (c->GetKind() == Track::Label &&
                  n && n->GetKind() == Track::Label)
         {
            ((LabelTrack *)n)->Clear(t0, t1);

            // To be (sort of) consistent with Clear behavior, we'll only shift
            // them if sync-lock is on.
            if (IsSyncLocked())
               ((LabelTrack *)n)->ShiftLabelsOnInsert(msClipT1 - msClipT0, t0);

            bPastedSomething |= ((LabelTrack *)n)->PasteOver(t0, c);
         }
         else
         {
            bPastedSomething |= n->Paste(t0, c);
         }

         // When copying from mono to stereo track, paste the wave form
         // to both channels
         if (n->GetLinked() && !c->GetLinked())
         {
            n = iter.Next();

            if (n->GetKind() == Track::Wave) {
               bPastedSomething |= ((WaveTrack *)n)->ClearAndPaste(t0, t1, c, true, true);
            }
            else
            {
               n->Clear(t0, t1);
               bPastedSomething |= n->Paste(t0, c);
            }
         }

         if (bAdvanceClipboard){
            prev = c;
            c = clipIter.Next();
         }
      } // if (n->GetSelected())
      else if (n->IsSyncLockSelected())
      {
         bPastedSomething |=  n->SyncLockAdjust(t1, t0 + msClipT1 - msClipT0);
      }

      n = iter.Next();
   }

   // This block handles the cases where our clipboard is smaller
   // than the amount of selected destination tracks. We take the
   // last wave track, and paste that one into the remaining
   // selected tracks.
   if ( n && !c )
   {
      TrackListOfKindIterator clipWaveIter(Track::Wave, msClipboard.get());
      c = clipWaveIter.Last();

      while (n) {
         if (n->GetSelected() && n->GetKind()==Track::Wave) {
            if (c && c->GetKind() == Track::Wave) {
               bPastedSomething |=
                  ((WaveTrack *)n)->ClearAndPaste(t0, t1, (WaveTrack *)c, true, true);
            }
            else {
               auto tmp = mTrackFactory->NewWaveTrack( ((WaveTrack*)n)->GetSampleFormat(), ((WaveTrack*)n)->GetRate());
               bool bResult = tmp->InsertSilence(0.0, msClipT1 - msClipT0); // MJS: Is this correct?
               wxASSERT(bResult); // TO DO: Actually handle this.
               wxUnusedVar(bResult);
               tmp->Flush();

               bPastedSomething |=
                  ((WaveTrack *)n)->ClearAndPaste(t0, t1, tmp.get(), true, true);
            }
         }
         else if (n->GetKind() == Track::Label && n->GetSelected())
         {
            ((LabelTrack *)n)->Clear(t0, t1);

            // As above, only shift labels if sync-lock is on.
            if (IsSyncLocked())
               ((LabelTrack *)n)->ShiftLabelsOnInsert(msClipT1 - msClipT0, t0);
         }
         else if (n->IsSyncLockSelected())
         {
            n->SyncLockAdjust(t1, t0 + msClipT1 - msClipT0);
         }

         n = iter.Next();
      }
   }

   // TODO: What if we clicked past the end of the track?

   if (bPastedSomething)
   {
      mViewInfo.selectedRegion.setT1(t0 + msClipT1 - msClipT0);

      PushState(_("Pasted from the clipboard"), _("Paste"));

      RedrawProject();

      if (ff)
         mTrackPanel->EnsureVisible(ff);
   }
}

// Handle text paste (into active label), if any. Return true if did paste.
// (This was formerly the first part of overly-long OnPaste.)
bool AudacityProject::HandlePasteText()
{
   TrackListOfKindIterator iterLabelTrack(Track::Label, GetTracks());
   LabelTrack* pLabelTrack = (LabelTrack*)(iterLabelTrack.First());
   while (pLabelTrack)
   {
      // Does this track have an active label?
      if (pLabelTrack->IsSelected()) {

         // Yes, so try pasting into it
         if (pLabelTrack->PasteSelectedText(mViewInfo.selectedRegion.t0(),
                                            mViewInfo.selectedRegion.t1()))
         {
            PushState(_("Pasted text from the clipboard"), _("Paste"));

            // Make sure caret is in view
            int x;
            if (pLabelTrack->CalcCursorX(&x)) {
               mTrackPanel->ScrollIntoView(x);
            }

            // Redraw everyting (is that necessary???) and bail
            RedrawProject();
            return true;
         }
      }
      pLabelTrack = (LabelTrack *) iterLabelTrack.Next();
   }
   return false;
}

// Return true if nothing selected, regardless of paste result.
// If nothing was selected, create and paste into NEW tracks.
// (This was formerly the second part of overly-long OnPaste.)
bool AudacityProject::HandlePasteNothingSelected()
{
   // First check whether anything's selected.
   bool bAnySelected = false;
   TrackListIterator iterTrack(GetTracks());
   Track* pTrack = iterTrack.First();
   while (pTrack) {
      if (pTrack->GetSelected())
      {
         bAnySelected = true;
         break;
      }
      pTrack = iterTrack.Next();
   }

   if (bAnySelected)
      return false;
   else
   {
      TrackListIterator iterClip(msClipboard.get());
      Track* pClip = iterClip.First();
      if (!pClip)
         return true; // nothing to paste

      Track::Holder pNewTrack;
      Track* pFirstNewTrack = NULL;
      while (pClip) {
         Maybe<WaveTrack::Locker> locker;
         if ((msClipProject != this) && (pClip->GetKind() == Track::Wave))
            locker.create(static_cast<const WaveTrack*>(pClip));

         switch (pClip->GetKind()) {
         case Track::Wave:
            {
               WaveTrack *w = (WaveTrack *)pClip;
               pNewTrack = mTrackFactory->NewWaveTrack(w->GetSampleFormat(), w->GetRate());
            }
            break;
         #ifdef USE_MIDI
            case Track::Note:
               pNewTrack = mTrackFactory->NewNoteTrack();
               break;
            #endif // USE_MIDI
         case Track::Label:
            pNewTrack = mTrackFactory->NewLabelTrack();
            break;
         case Track::Time:
            pNewTrack = mTrackFactory->NewTimeTrack();
            break;
         default:
            pClip = iterClip.Next();
            continue;
         }
         wxASSERT(pClip);

         pNewTrack->SetLinked(pClip->GetLinked());
         pNewTrack->SetChannel(pClip->GetChannel());
         pNewTrack->SetName(pClip->GetName());

         bool bResult = pNewTrack->Paste(0.0, pClip);
         wxASSERT(bResult); // TO DO: Actually handle this.
         wxUnusedVar(bResult);

         if (!pFirstNewTrack)
            pFirstNewTrack = pNewTrack.get();

         pNewTrack->SetSelected(true);
         mTracks->Add(std::move(pNewTrack));

         pClip = iterClip.Next();
      }

      // Select some pasted samples, which is probably impossible to get right
      // with various project and track sample rates.
      // So do it at the sample rate of the project
      AudacityProject *p = GetActiveProject();
      double projRate = p->GetRate();
      double quantT0 = QUANTIZED_TIME(msClipT0, projRate);
      double quantT1 = QUANTIZED_TIME(msClipT1, projRate);
      mViewInfo.selectedRegion.setTimes(
         0.0,   // anywhere else and this should be
                // half a sample earlier
         quantT1 - quantT0);

      PushState(_("Pasted from the clipboard"), _("Paste"));

      RedrawProject();

      if (pFirstNewTrack)
         mTrackPanel->EnsureVisible(pFirstNewTrack);

      return true;
   }
}


// Creates a NEW label in each selected label track with text from the system
// clipboard
void AudacityProject::OnPasteNewLabel()
{
   bool bPastedSomething = false;

   SelectedTrackListOfKindIterator iter(Track::Label, GetTracks());
   Track *t = iter.First();
   if (!t)
   {
      // If there are no selected label tracks, try to choose the first label
      // track after some other selected track
      TrackListIterator iter1(GetTracks());
      for (Track *t1 = iter1.First(); t1; t1 = iter1.Next()) {
         if (t1->GetSelected()) {
            // Look for a label track
            while (0 != (t1 = iter1.Next())) {
               if (t1->GetKind() == Track::Label) {
                  t = t1;
                  break;
               }
            }
            if (t) break;
         }
      }

      // If no match found, add one
      if (!t) {
         t = mTracks->Add(GetTrackFactory()->NewLabelTrack());
      }

      // Select this track so the loop picks it up
      t->SetSelected(true);
   }

   LabelTrack *plt = NULL; // the previous track
   for (Track *t = iter.First(); t; t = iter.Next())
   {
      LabelTrack *lt = (LabelTrack *)t;

      // Unselect the last label, so we'll have just one active label when
      // we're done
      if (plt)
         plt->Unselect();

      // Add a NEW label, paste into it
      // Paul L:  copy whatever defines the selected region, not just times
      lt->AddLabel(mViewInfo.selectedRegion);
      if (lt->PasteSelectedText(mViewInfo.selectedRegion.t0(),
                                mViewInfo.selectedRegion.t1()))
         bPastedSomething = true;

      // Set previous track
      plt = lt;
   }

   // plt should point to the last label track pasted to -- ensure it's visible
   // and set focus
   if (plt) {
      mTrackPanel->EnsureVisible(plt);
      mTrackPanel->SetFocus();
   }

   if (bPastedSomething) {
      PushState(_("Pasted from the clipboard"), _("Paste Text to New Label"));

      // Is this necessary? (carried over from former logic in OnPaste())
      RedrawProject();
   }
}

void AudacityProject::OnPasteOver() // not currently in use it appears
{
   if((msClipT1 - msClipT0) > 0.0)
   {
      mViewInfo.selectedRegion.setT1(
         mViewInfo.selectedRegion.t0() + (msClipT1 - msClipT0));
         // MJS: pointless, given what we do in OnPaste?
   }
   OnPaste();

   return;
}

void AudacityProject::OnTrim()
{
   if (mViewInfo.selectedRegion.isPoint())
      return;

   TrackListIterator iter(GetTracks());
   Track *n = iter.First();

   while (n) {
      if (n->GetSelected()) {
         switch (n->GetKind())
         {
#if defined(USE_MIDI)
            case Track::Note:
               ((NoteTrack*)n)->Trim(mViewInfo.selectedRegion.t0(),
                                     mViewInfo.selectedRegion.t1());
            break;
#endif

            case Track::Wave:
               //Delete the section before the left selector
               ((WaveTrack*)n)->Trim(mViewInfo.selectedRegion.t0(),
                                     mViewInfo.selectedRegion.t1());
            break;

            default:
            break;
         }
      }
      n = iter.Next();
   }

   PushState(wxString::Format(_("Trim selected audio tracks from %.2f seconds to %.2f seconds"),
       mViewInfo.selectedRegion.t0(), mViewInfo.selectedRegion.t1()),
       _("Trim Audio"));

   RedrawProject();
}

void AudacityProject::OnDelete()
{
   Clear();
}

void AudacityProject::OnSplitDelete()
{
   TrackListIterator iter(GetTracks());

   Track *n = iter.First();

   while (n) {
      if (n->GetSelected()) {
         if (n->GetKind() == Track::Wave)
         {
            ((WaveTrack*)n)->SplitDelete(mViewInfo.selectedRegion.t0(),
                                         mViewInfo.selectedRegion.t1());
         }
         else {
            n->Silence(mViewInfo.selectedRegion.t0(),
                       mViewInfo.selectedRegion.t1());
         }
      }
      n = iter.Next();
   }

   PushState(wxString::Format(_("Split-deleted %.2f seconds at t=%.2f"),
                              mViewInfo.selectedRegion.duration(),
                              mViewInfo.selectedRegion.t0()),
             _("Split Delete"));

   RedrawProject();
}

void AudacityProject::OnDisjoin()
{
   TrackListIterator iter(GetTracks());

   Track *n = iter.First();

   while (n) {
      if (n->GetSelected()) {
         if (n->GetKind() == Track::Wave)
         {
            ((WaveTrack*)n)->Disjoin(mViewInfo.selectedRegion.t0(),
                                     mViewInfo.selectedRegion.t1());
         }
      }
      n = iter.Next();
   }

   PushState(wxString::Format(_("Detached %.2f seconds at t=%.2f"),
                              mViewInfo.selectedRegion.duration(),
                              mViewInfo.selectedRegion.t0()),
             _("Detach"));

   RedrawProject();
}

void AudacityProject::OnJoin()
{
   TrackListIterator iter(GetTracks());

   Track *n = iter.First();

   while (n) {
      if (n->GetSelected()) {
         if (n->GetKind() == Track::Wave)
         {
            ((WaveTrack*)n)->Join(mViewInfo.selectedRegion.t0(),
                                  mViewInfo.selectedRegion.t1());
         }
      }
      n = iter.Next();
   }

   PushState(wxString::Format(_("Joined %.2f seconds at t=%.2f"),
                              mViewInfo.selectedRegion.duration(),
                              mViewInfo.selectedRegion.t0()),
             _("Join"));

   RedrawProject();
}

void AudacityProject::OnSilence()
{
   SelectedTrackListOfKindIterator iter(Track::Wave, GetTracks());

   for (Track *n = iter.First(); n; n = iter.Next())
      n->Silence(mViewInfo.selectedRegion.t0(), mViewInfo.selectedRegion.t1());

   PushState(wxString::
             Format(_("Silenced selected tracks for %.2f seconds at %.2f"),
                    mViewInfo.selectedRegion.duration(),
                    mViewInfo.selectedRegion.t0()),
             _("Silence"));

   mTrackPanel->Refresh(false);
}

void AudacityProject::OnDuplicate()
{
   TrackListIterator iter(GetTracks());

   Track *l = iter.Last();
   Track *n = iter.First();

   while (n) {
      if (n->GetSelected()) {
         auto dest = n->Copy(mViewInfo.selectedRegion.t0(),
                 mViewInfo.selectedRegion.t1());
         if (dest) {
            dest->Init(*n);
            dest->SetOffset(wxMax(mViewInfo.selectedRegion.t0(), n->GetOffset()));
            mTracks->Add(std::move(dest));
         }
      }

      if (n == l) {
         break;
      }

      n = iter.Next();
   }

   PushState(_("Duplicated"), _("Duplicate"));

   RedrawProject();
}

void AudacityProject::OnCutLabels()
{
  if( mViewInfo.selectedRegion.isPoint() )
     return;

  // Because of grouping the copy may need to operate on different tracks than
  // the clear, so we do these actions separately.
  EditClipboardByLabel( &WaveTrack::CopyNonconst );

  if( gPrefs->Read( wxT( "/GUI/EnableCutLines" ), ( long )0 ) )
     EditByLabel( &WaveTrack::ClearAndAddCutLine, true );
  else
     EditByLabel( &WaveTrack::Clear, true );

  msClipProject = this;

  mViewInfo.selectedRegion.collapseToT0();

  PushState(
   /* i18n-hint: (verb) past tense.  Audacity has just cut the labeled audio regions.*/
     _( "Cut labeled audio regions to clipboard" ),
  /* i18n-hint: (verb)*/
     _( "Cut Labeled Audio" ) );

  RedrawProject();
}

void AudacityProject::OnSplitCutLabels()
{
  if( mViewInfo.selectedRegion.isPoint() )
     return;

  EditClipboardByLabel( &WaveTrack::SplitCut );

  msClipProject = this;

  PushState(
   /* i18n-hint: (verb) Audacity has just split cut the labeled audio regions*/
     _( "Split Cut labeled audio regions to clipboard" ),
  /* i18n-hint: (verb) Do a special kind of cut on the labels*/
        _( "Split Cut Labeled Audio" ) );

  RedrawProject();
}

void AudacityProject::OnCopyLabels()
{
  if( mViewInfo.selectedRegion.isPoint() )
     return;

  EditClipboardByLabel( &WaveTrack::CopyNonconst );

  msClipProject = this;

  PushState( _( "Copied labeled audio regions to clipboard" ),
  /* i18n-hint: (verb)*/
     _( "Copy Labeled Audio" ) );

  mTrackPanel->Refresh( false );
}

void AudacityProject::OnDeleteLabels()
{
  if( mViewInfo.selectedRegion.isPoint() )
     return;

  EditByLabel( &WaveTrack::Clear, true );

  mViewInfo.selectedRegion.collapseToT0();

  PushState(
   /* i18n-hint: (verb) Audacity has just deleted the labeled audio regions*/
     _( "Deleted labeled audio regions" ),
  /* i18n-hint: (verb)*/
     _( "Delete Labeled Audio" ) );

  RedrawProject();
}

void AudacityProject::OnSplitDeleteLabels()
{
  if( mViewInfo.selectedRegion.isPoint() )
     return;

  EditByLabel( &WaveTrack::SplitDelete, false );

  PushState(
  /* i18n-hint: (verb) Audacity has just done a special kind of DELETE on the labeled audio regions */
     _( "Split Deleted labeled audio regions" ),
  /* i18n-hint: (verb) Do a special kind of DELETE on labeled audio regions*/
     _( "Split Delete Labeled Audio" ) );

  RedrawProject();
}

void AudacityProject::OnSilenceLabels()
{
  if( mViewInfo.selectedRegion.isPoint() )
     return;

  EditByLabel( &WaveTrack::Silence, false );

  PushState(
   /* i18n-hint: (verb)*/
     _( "Silenced labeled audio regions" ),
  /* i18n-hint: (verb)*/
     _( "Silence Labeled Audio" ) );

  mTrackPanel->Refresh( false );
}

void AudacityProject::OnSplitLabels()
{
  EditByLabel( &WaveTrack::Split, false );

  PushState(
   /* i18n-hint: (verb) past tense.  Audacity has just split the labeled audio (a point or a region)*/
     _( "Split labeled audio (points or regions)" ),
  /* i18n-hint: (verb)*/
     _( "Split Labeled Audio" ) );

  RedrawProject();
}

void AudacityProject::OnJoinLabels()
{
  if( mViewInfo.selectedRegion.isPoint() )
     return;

  EditByLabel( &WaveTrack::Join, false );

  PushState(
   /* i18n-hint: (verb) Audacity has just joined the labeled audio (points or regions)*/
     _( "Joined labeled audio (points or regions)" ),
  /* i18n-hint: (verb)*/
     _( "Join Labeled Audio" ) );

  RedrawProject();
}

void AudacityProject::OnDisjoinLabels()
{
  if( mViewInfo.selectedRegion.isPoint() )
     return;

  EditByLabel( &WaveTrack::Disjoin, false );

  PushState(
   /* i18n-hint: (verb) Audacity has just detached the labeled audio regions.
      This message appears in history and tells you about something
      Audacity has done.*/
   _( "Detached labeled audio regions" ),
   /* i18n-hint: (verb)*/
     _( "Detach Labeled Audio" ) );

  RedrawProject();
}

void AudacityProject::OnSplit()
{
   TrackListIterator iter(GetTracks());

   double sel0 = mViewInfo.selectedRegion.t0();
   double sel1 = mViewInfo.selectedRegion.t1();

   for (Track* n=iter.First(); n; n = iter.Next())
   {
      if (n->GetKind() == Track::Wave)
      {
         WaveTrack* wt = (WaveTrack*)n;
         if (wt->GetSelected())
            wt->Split( sel0, sel1 );
      }
   }

   PushState(_("Split"), _("Split"));
   mTrackPanel->Refresh(false);
#if 0
//ANSWER-ME: Do we need to keep this commented out OnSplit() code?
// This whole section no longer used...
   /*
    * Previous (pre-multiclip) implementation of "Split" command
    * This does work only when a range is selected!
    *
   TrackListIterator iter(mTracks);

   Track *n = iter.First();
   Track *dest;

   TrackList newTracks;

   while (n) {
      if (n->GetSelected()) {
         double sel0 = mViewInfo.selectedRegion.t0();
         double sel1 = mViewInfo.selectedRegion.t1();

         dest = NULL;
         n->Copy(sel0, sel1, &dest);
         if (dest) {
            dest->Init(*n);
            dest->SetOffset(wxMax(sel0, n->GetOffset()));

            if (sel1 >= n->GetEndTime())
               n->Clear(sel0, sel1);
            else if (sel0 <= n->GetOffset()) {
               n->Clear(sel0, sel1);
               n->SetOffset(sel1);
            } else
               n->Silence(sel0, sel1);

            newTracks.Add(dest);
         }
      }
      n = iter.Next();
   }

   TrackListIterator nIter(&newTracks);
   n = nIter.First();
   while (n) {
      mTracks->Add(n);
      n = nIter.Next();
   }

   PushState(_("Split"), _("Split"));

   FixScrollbars();
   mTrackPanel->Refresh(false);
   */
#endif
}

void AudacityProject::OnSplitNew()
{
   TrackListIterator iter(GetTracks());
   Track *l = iter.Last();

   for (Track *n = iter.First(); n; n = iter.Next()) {
      if (n->GetSelected()) {
         Track::Holder dest;
         double newt0 = 0, newt1 = 0;
         double offset = n->GetOffset();
         if (n->GetKind() == Track::Wave) {
            const auto wt = static_cast<WaveTrack*>(n);
            // Clips must be aligned to sample positions or the NEW clip will not fit in the gap where it came from
            offset = wt->LongSamplesToTime(wt->TimeToLongSamples(offset));
            newt0 = wt->LongSamplesToTime(wt->TimeToLongSamples(mViewInfo.selectedRegion.t0()));
            newt1 = wt->LongSamplesToTime(wt->TimeToLongSamples(mViewInfo.selectedRegion.t1()));
            dest = wt->SplitCut(newt0, newt1);
         }
#if 0
         // LL:  For now, just skip all non-wave tracks since the other do not
         //      yet support proper splitting.
         else {
            dest = n->Cut(mViewInfo.selectedRegion.t0(),
                   mViewInfo.selectedRegion.t1());
         }
#endif
         if (dest) {
            dest->SetChannel(n->GetChannel());
            dest->SetLinked(n->GetLinked());
            dest->SetName(n->GetName());
            dest->SetOffset(wxMax(newt0, offset));
            mTracks->Add(std::move(dest));
         }
      }

      if (n == l) {
         break;
      }
   }

   PushState(_("Split to new track"), _("Split New"));

   RedrawProject();
}

void AudacityProject::OnSelectAll()
{
   TrackListIterator iter(GetTracks());

   Track *t = iter.First();
   while (t) {
      t->SetSelected(true);
      t = iter.Next();
   }
   mViewInfo.selectedRegion.setTimes(
      mTracks->GetMinOffset(), mTracks->GetEndTime());

   ModifyState(false);

   mTrackPanel->Refresh(false);
   if (mMixerBoard)
      mMixerBoard->Refresh(false);
}

void AudacityProject::OnSelectNone()
{
   this->SelectNone();
   mViewInfo.selectedRegion.collapseToT0();
   ModifyState(false);
}

#ifdef EXPERIMENTAL_SPECTRAL_EDITING
void AudacityProject::OnToggleSpectralSelection()
{
   mTrackPanel->ToggleSpectralSelection();
   mTrackPanel->Refresh(false);
   ModifyState(false);
}

void AudacityProject::DoNextPeakFrequency(bool up)
{
   // Find the first selected wave track that is in a spectrogram view.
   WaveTrack *pTrack = 0;
   SelectedTrackListOfKindIterator iter(Track::Wave, GetTracks());
   for (Track *t = iter.First(); t; t = iter.Next()) {
      WaveTrack *const wt = static_cast<WaveTrack*>(t);
      const int display = wt->GetDisplay();
      if (display == WaveTrack::Spectrum) {
         pTrack = wt;
         break;
      }
   }

   if (pTrack) {
      mTrackPanel->SnapCenterOnce(pTrack, up);
      mTrackPanel->Refresh(false);
      ModifyState(false);
   }
}

void AudacityProject::OnNextHigherPeakFrequency()
{
   DoNextPeakFrequency(true);
}


void AudacityProject::OnNextLowerPeakFrequency()
{
   DoNextPeakFrequency(false);
}
#endif

void AudacityProject::OnSelectCursorEnd()
{
   double maxEndOffset = -1000000.0;

   TrackListIterator iter(GetTracks());
   Track *t = iter.First();

   while (t) {
      if (t->GetSelected()) {
         if (t->GetEndTime() > maxEndOffset)
            maxEndOffset = t->GetEndTime();
      }

      t = iter.Next();
   }

   mViewInfo.selectedRegion.setT1(maxEndOffset);

   ModifyState(false);

   mTrackPanel->Refresh(false);
}

void AudacityProject::OnSelectStartCursor()
{
   double minOffset = 1000000.0;

   TrackListIterator iter(GetTracks());
   Track *t = iter.First();

   while (t) {
      if (t->GetSelected()) {
         if (t->GetOffset() < minOffset)
            minOffset = t->GetOffset();
      }

      t = iter.Next();
   }

   mViewInfo.selectedRegion.setT0(minOffset);

   ModifyState(false);

   mTrackPanel->Refresh(false);
}

void AudacityProject::OnSelectSyncLockSel()
{
   bool selected = false;
   TrackListIterator iter(GetTracks());
   for (Track *t = iter.First(); t; t = iter.Next())
   {
      if (t->IsSyncLockSelected()) {
         t->SetSelected(true);
         selected = true;
      }
   }

   if (selected)
      ModifyState(false);

   mTrackPanel->Refresh(false);
   if (mMixerBoard)
      mMixerBoard->Refresh(false);
}

void AudacityProject::OnSelectAllTracks()
{
   TrackListIterator iter(GetTracks());
   for (Track *t = iter.First(); t; t = iter.Next()) {
      t->SetSelected(true);
   }

   ModifyState(false);

   mTrackPanel->Refresh(false);
   if (mMixerBoard)
      mMixerBoard->Refresh(false);
}

//
// View Menu
//

void AudacityProject::OnZoomIn()
{
   ZoomInByFactor( 2.0 );
}

double AudacityProject::GetScreenEndTime() const
{
   return mTrackPanel->GetScreenEndTime();
}

void AudacityProject::ZoomInByFactor( double ZoomFactor )
{
   // LLL: Handling positioning differently when audio is active
   if (gAudioIO->IsStreamActive(GetAudioIOToken()) != 0) {
      ZoomBy(ZoomFactor);
      mTrackPanel->ScrollIntoView(gAudioIO->GetStreamTime());
      mTrackPanel->Refresh(false);
      return;
   }

   // DMM: Here's my attempt to get logical zooming behavior
   // when there's a selection that's currently at least
   // partially on-screen

   const double endTime = GetScreenEndTime();
   const double duration = endTime - mViewInfo.h;

   bool selectionIsOnscreen =
      (mViewInfo.selectedRegion.t0() < endTime) &&
      (mViewInfo.selectedRegion.t1() >= mViewInfo.h);

   bool selectionFillsScreen =
      (mViewInfo.selectedRegion.t0() < mViewInfo.h) &&
      (mViewInfo.selectedRegion.t1() > endTime);

   if (selectionIsOnscreen && !selectionFillsScreen) {
      // Start with the center of the selection
      double selCenter = (mViewInfo.selectedRegion.t0() +
                          mViewInfo.selectedRegion.t1()) / 2;

      // If the selection center is off-screen, pick the
      // center of the part that is on-screen.
      if (selCenter < mViewInfo.h)
         selCenter = mViewInfo.h +
                     (mViewInfo.selectedRegion.t1() - mViewInfo.h) / 2;
      if (selCenter > endTime)
         selCenter = endTime -
            (endTime - mViewInfo.selectedRegion.t0()) / 2;

      // Zoom in
      ZoomBy(ZoomFactor);
      const double newDuration = GetScreenEndTime() - mViewInfo.h;

      // Recenter on selCenter
      TP_ScrollWindow(selCenter - newDuration / 2);
      return;
   }


   double origLeft = mViewInfo.h;
   double origWidth = duration;
   ZoomBy(ZoomFactor);

   const double newDuration = GetScreenEndTime() - mViewInfo.h;
   double newh = origLeft + (origWidth - newDuration) / 2;

   // MM: Commented this out because it was confusing users
   /*
   // make sure that the *right-hand* end of the selection is
   // no further *left* than 1/3 of the way across the screen
   if (mViewInfo.selectedRegion.t1() < newh + mViewInfo.screen / 3)
      newh = mViewInfo.selectedRegion.t1() - mViewInfo.screen / 3;

   // make sure that the *left-hand* end of the selection is
   // no further *right* than 2/3 of the way across the screen
   if (mViewInfo.selectedRegion.t0() > newh + mViewInfo.screen * 2 / 3)
      newh = mViewInfo.selectedRegion.t0() - mViewInfo.screen * 2 / 3;
   */

   TP_ScrollWindow(newh);
}

void AudacityProject::OnZoomOut()
{
   ZoomOutByFactor( 1 /2.0 );
}


void AudacityProject::ZoomOutByFactor( double ZoomFactor )
{
   //Zoom() may change these, so record original values:
   const double origLeft = mViewInfo.h;
   const double origWidth = GetScreenEndTime() - origLeft;

   ZoomBy(ZoomFactor);
   const double newWidth = GetScreenEndTime() - mViewInfo.h;

   const double newh = origLeft + (origWidth - newWidth) / 2;
   // newh = (newh > 0) ? newh : 0;
   TP_ScrollWindow(newh);

}

// this is unused:
#if 0
static double OldZooms[2]={ 44100.0/512.0, 4410.0/512.0 };
void AudacityProject::OnZoomToggle()
{
   double origLeft = mViewInfo.h;
   double origWidth = mViewInfo.screen;

   float f;
   // look at percentage difference.  We add a small fudge factor
   // to avoid testing for zero divisor.
   f = mViewInfo.zoom / (OldZooms[0] + 0.0001f);
   // If old zoom is more than 10 percent different, use it.
   if( (0.90f > f) || (f >1.10) ){
      OldZooms[1]=OldZooms[0];
      OldZooms[0]=mViewInfo.zoom;
   }
   Zoom( OldZooms[1] );
   double newh = origLeft + (origWidth - mViewInfo.screen) / 2;
   TP_ScrollWindow(newh);
}
#endif


void AudacityProject::OnZoomNormal()
{
   Zoom(ZoomInfo::GetDefaultZoom());
   mTrackPanel->Refresh(false);
}

void AudacityProject::OnZoomFit()
{
   const double end = mTracks->GetEndTime();
   const double start = mViewInfo.bScrollBeyondZero
      ? std::min(mTracks->GetStartTime(), 0.0)
      : 0;
   const double len = end - start;

   if (len <= 0.0)
      return;

   int w;
   mTrackPanel->GetTracksUsableArea(&w, NULL);
   w -= 10;

   Zoom(w / len);
   TP_ScrollWindow(start);
}

void AudacityProject::DoZoomFitV()
{
   int height, count;

   mTrackPanel->GetTracksUsableArea(NULL, &height);

   height -= 28;

   count = 0;
   TrackListIterator iter(GetTracks());
   Track *t = iter.First();
   while (t) {
      if ((t->GetKind() == Track::Wave) &&
          !t->GetMinimized())
         count++;
      else
         height -= t->GetHeight();

      t = iter.Next();
   }

   if (count == 0)
      return;

   height = height / count;

   if (height < 40)
      height = 40;

   TrackListIterator iter2(GetTracks());
   t = iter2.First();
   while (t) {
      if ((t->GetKind() == Track::Wave) &&
          !t->GetMinimized())
         t->SetHeight(height);
      t = iter2.Next();
   }
}

void AudacityProject::OnZoomFitV()
{
   this->DoZoomFitV();

   mVsbar->SetThumbPosition(0);
   RedrawProject();
   ModifyState(true);
}

void AudacityProject::OnZoomSel()
{
   const double lowerBound =
      std::max(mViewInfo.selectedRegion.t0(), ScrollingLowerBoundTime());
   const double denom =
      mViewInfo.selectedRegion.t1() - lowerBound;
   if (denom <= 0.0)
      return;

   // LL:  The "-1" is just a hack to get around an issue where zooming to
   //      selection doesn't actually get the entire selected region within the
   //      visible area.  This causes a problem with scrolling at end of playback
   //      where the selected region may be scrolled off the left of the screen.
   //      I know this isn't right, but until the real rounding or 1-off issue is
   //      found, this will have to work.
   // PRL:  Did I fix this?  I am not sure, so I leave the hack in place.
   //      Fixes might have resulted from commits
   //      1b8f44d0537d987c59653b11ed75a842b48896ea and
   //      e7c7bb84a966c3b3cc4b3a9717d5f247f25e7296
   int width;
   mTrackPanel->GetTracksUsableArea(&width, NULL);
   Zoom((width - 1) / denom);
   TP_ScrollWindow(mViewInfo.selectedRegion.t0());
}  

void AudacityProject::OnGoSelStart()
{
   if (mViewInfo.selectedRegion.isPoint())
      return;

   TP_ScrollWindow(mViewInfo.selectedRegion.t0() - ((GetScreenEndTime() - mViewInfo.h) / 2));
}

void AudacityProject::OnGoSelEnd()
{
   if (mViewInfo.selectedRegion.isPoint())
      return;

   TP_ScrollWindow(mViewInfo.selectedRegion.t1() - ((GetScreenEndTime() - mViewInfo.h) / 2));
}

void AudacityProject::OnShowClipping()
{
   bool checked = !gPrefs->Read(wxT("/GUI/ShowClipping"), 0L);
   gPrefs->Write(wxT("/GUI/ShowClipping"), checked);
   gPrefs->Flush();
   mCommandManager.Check(wxT("ShowClipping"), checked);
   mTrackPanel->UpdatePrefs();
   mTrackPanel->Refresh(false);
}

void AudacityProject::OnHistory()
{
   if (!mHistoryWindow)
      mHistoryWindow = safenew HistoryWindow(this, GetUndoManager());
   mHistoryWindow->Show();
   mHistoryWindow->Raise();
   mHistoryWindow->UpdateDisplay();
}

void AudacityProject::OnKaraoke()
{
   if (!mLyricsWindow)
      mLyricsWindow = safenew LyricsWindow(this);
   mLyricsWindow->Show();
   UpdateLyrics();
   mLyricsWindow->Raise();
}

void AudacityProject::OnMixerBoard()
{
   if (!mMixerBoardFrame)
   {
      mMixerBoardFrame = safenew MixerBoardFrame(this);
      mMixerBoard = mMixerBoardFrame->mMixerBoard;
   }
   mMixerBoardFrame->Show();
   mMixerBoardFrame->Raise();
   mMixerBoardFrame->SetFocus();
}

void AudacityProject::OnPlotSpectrum()
{
   if (!mFreqWindow) {
      wxPoint where;

      where.x = 150;
      where.y = 150;

      mFreqWindow = safenew FreqWindow(this, -1, _("Frequency Analysis"), where);
   }

   mFreqWindow->Show(true);
   mFreqWindow->Raise();
   mFreqWindow->SetFocus();
}

void AudacityProject::OnContrast()
{
   // All of this goes away when the Contrast Dialog is converted to a module
   if(!mContrastDialog)
   {
      wxPoint where;
      where.x = 150;
      where.y = 150;

      mContrastDialog = safenew ContrastDialog(this, -1, _("Contrast Analysis (WCAG 2 compliance)"), where);
   }

   mContrastDialog->CentreOnParent();
   mContrastDialog->Show();
}


void AudacityProject::OnShowTransportToolBar()
{
   mToolManager->ShowHide(TransportBarID);
   ModifyToolbarMenus();
}

void AudacityProject::OnShowDeviceToolBar()
{
   mToolManager->ShowHide( DeviceBarID );
   ModifyToolbarMenus();
}

void AudacityProject::OnShowEditToolBar()
{
   mToolManager->ShowHide( EditBarID );
   ModifyToolbarMenus();
}

void AudacityProject::OnShowMeterToolBar()
{
   if( !mToolManager->IsVisible( MeterBarID ) )
   {
      mToolManager->Expose( PlayMeterBarID, false );
      mToolManager->Expose( RecordMeterBarID, false ); 
   }
   mToolManager->ShowHide( MeterBarID );
   ModifyToolbarMenus();
}

void AudacityProject::OnShowRecordMeterToolBar()
{
   if( !mToolManager->IsVisible( RecordMeterBarID ) )
   {
      mToolManager->Expose( MeterBarID, false );
   }
   mToolManager->ShowHide( RecordMeterBarID );
   ModifyToolbarMenus();
}

void AudacityProject::OnShowPlayMeterToolBar()
{
   if( !mToolManager->IsVisible( PlayMeterBarID ) )
   {
      mToolManager->Expose( MeterBarID, false );
   }
   mToolManager->ShowHide( PlayMeterBarID );
   ModifyToolbarMenus();
}

void AudacityProject::OnShowMixerToolBar()
{
   mToolManager->ShowHide( MixerBarID );
   ModifyToolbarMenus();
}

void AudacityProject::OnShowScrubbingToolBar()
{
   mToolManager->ShowHide( ScrubbingBarID );
   ModifyToolbarMenus();
}

void AudacityProject::OnShowSelectionToolBar()
{
   mToolManager->ShowHide( SelectionBarID );
   ModifyToolbarMenus();
}

#ifdef EXPERIMENTAL_SPECTRAL_EDITING
void AudacityProject::OnShowSpectralSelectionToolBar()
{
   mToolManager->ShowHide( SpectralSelectionBarID );
   ModifyToolbarMenus();
}
#endif

void AudacityProject::OnShowToolsToolBar()
{
   mToolManager->ShowHide( ToolsBarID );
   ModifyToolbarMenus();
}

void AudacityProject::OnShowTranscriptionToolBar()
{
   mToolManager->ShowHide( TranscriptionBarID );
   ModifyToolbarMenus();
}

void AudacityProject::OnResetToolBars()
{
   mToolManager->Reset();
   ModifyToolbarMenus();
}

//
// Project Menu
//

void AudacityProject::OnImport()
{
   // An import trigger for the alias missing dialog might not be intuitive, but
   // this serves to track the file if the users zooms in and such.
   wxGetApp().SetMissingAliasedFileWarningShouldShow(true);

   wxArrayString selectedFiles = ShowOpenDialog(wxT(""));
   if (selectedFiles.GetCount() == 0) {
      gPrefs->Write(wxT("/LastOpenType"),wxT(""));
      gPrefs->Flush();
      return;
   }

   gPrefs->Write(wxT("/NewImportingSession"), true);

   //sort selected files by OD status.  Load non OD first so user can edit asap.
   //first sort selectedFiles.
   selectedFiles.Sort(CompareNoCaseFileName);
   ODManager::Pauser pauser;

   for (size_t ff = 0; ff < selectedFiles.GetCount(); ff++) {
      wxString fileName = selectedFiles[ff];

      wxString path = ::wxPathOnly(fileName);
      gPrefs->Write(wxT("/DefaultOpenPath"), path);

      Import(fileName);
   }

   gPrefs->Write(wxT("/LastOpenType"),wxT(""));

   gPrefs->Flush();

   HandleResize(); // Adjust scrollers for NEW track sizes.
}

void AudacityProject::OnImportLabels()
{
   wxString path = gPrefs->Read(wxT("/DefaultOpenPath"),::wxGetCwd());

   wxString fileName =
       FileSelector(_("Select a text file containing labels..."),
                    path,     // Path
                    wxT(""),       // Name
                    wxT(".txt"),   // Extension
                    _("Text files (*.txt)|*.txt|All files|*"),
                    wxRESIZE_BORDER,        // Flags
                    this);    // Parent

   if (fileName != wxT("")) {
      path =::wxPathOnly(fileName);
      gPrefs->Write(wxT("/DefaultOpenPath"), path);
      gPrefs->Flush();

      wxTextFile f;

      f.Open(fileName);
      if (!f.IsOpened()) {
         wxMessageBox(_("Could not open file: ") + fileName);
         return;
      }

      auto newTrack = GetTrackFactory()->NewLabelTrack();
      wxString sTrackName;
      wxFileName::SplitPath(fileName, NULL, NULL, &sTrackName, NULL);
      newTrack->SetName(sTrackName);

      newTrack->Import(f);

      SelectNone();
      newTrack->SetSelected(true);
      mTracks->Add(std::move(newTrack));

      PushState(wxString::
                Format(_("Imported labels from '%s'"), fileName.c_str()),
                _("Import Labels"));

      RedrawProject();
   }
}

#ifdef USE_MIDI
void AudacityProject::OnImportMIDI()
{
   wxString path = gPrefs->Read(wxT("/DefaultOpenPath"),::wxGetCwd());

   wxString fileName = FileSelector(_("Select a MIDI file..."),
                                    path,     // Path
                                    wxT(""),       // Name
                                    wxT(""),       // Extension
                                    _("MIDI and Allegro files (*.mid;*.midi;*.gro)|*.mid;*.midi;*.gro|MIDI files (*.mid;*.midi)|*.mid;*.midi|Allegro files (*.gro)|*.gro|All files|*"),
                                    wxRESIZE_BORDER,        // Flags
                                    this);    // Parent

   if (fileName != wxT("")) {
      path =::wxPathOnly(fileName);
      gPrefs->Write(wxT("/DefaultOpenPath"), path);
      gPrefs->Flush();

      DoImportMIDI(fileName);
   }
}

void AudacityProject::DoImportMIDI(const wxString &fileName)
{
   auto newTrack = GetTrackFactory()->NewNoteTrack();

   if (::ImportMIDI(fileName, newTrack.get())) {

      SelectNone();
      auto pTrack = mTracks->Add(std::move(newTrack));
      pTrack->SetSelected(true);

      PushState(wxString::Format(_("Imported MIDI from '%s'"),
         fileName.c_str()), _("Import MIDI"));

      RedrawProject();
      mTrackPanel->EnsureVisible(pTrack);
   }
}
#endif // USE_MIDI

void AudacityProject::OnImportRaw()
{
   wxString path = gPrefs->Read(wxT("/DefaultOpenPath"),::wxGetCwd());

   wxString fileName =
       FileSelector(_("Select any uncompressed audio file..."),
                    path,     // Path
                    wxT(""),       // Name
                    wxT(""),       // Extension
                    _("All files|*"),
                    wxRESIZE_BORDER,        // Flags
                    this);    // Parent

   if (fileName == wxT(""))
      return;

   path =::wxPathOnly(fileName);
   gPrefs->Write(wxT("/DefaultOpenPath"), path);
   gPrefs->Flush();

   TrackHolders newTracks;

   ::ImportRaw(this, fileName, GetTrackFactory(), newTracks);

   if (newTracks.size() <= 0)
      return;

   AddImportedTracks(fileName, std::move(newTracks));
   HandleResize(); // Adjust scrollers for NEW track sizes.
}

void AudacityProject::OnEditMetadata()
{
   (void)DoEditMetadata(_("Edit Metadata Tags"), _("Metadata Tags"), true);
}

bool AudacityProject::DoEditMetadata
(const wxString &title, const wxString &shortUndoDescription, bool force)
{
   // Back up my tags
   auto newTags = mTags->Duplicate();

   if (newTags->ShowEditDialog(this, title, force)) {
      if (*mTags != *newTags) {
         // Commit the change to project state only now.
         mTags = newTags;
         PushState(title, shortUndoDescription);
      }

      return true;
   }

   return false;
}

void AudacityProject::HandleMixAndRender(bool toNewTrack)
{
   wxGetApp().SetMissingAliasedFileWarningShouldShow(true);

   WaveTrack::Holder uNewLeft, uNewRight;
   MixAndRender(GetTracks(), GetTrackFactory(), mRate, mDefaultFormat, 0.0, 0.0, uNewLeft, uNewRight);

   if (uNewLeft) {
      // Remove originals, get stats on what tracks were mixed

      TrackListIterator iter(GetTracks());
      Track *t = iter.First();
      int selectedCount = 0;
      wxString firstName;

      while (t) {
         if (t->GetSelected() && (t->GetKind() == Track::Wave)) {
            if (selectedCount==0)
               firstName = t->GetName();

            // Add one to the count if it's an unlinked track, or if it's the first
            // in a stereo pair
            if (t->GetLinked() || !t->GetLink())
                selectedCount++;

                if (!toNewTrack) {
                   t = iter.RemoveCurrent();
                } else {
                   t = iter.Next();
                };
         }
         else
            t = iter.Next();
      }

      // Add NEW tracks

      auto pNewLeft = mTracks->Add(std::move(uNewLeft));
      decltype(pNewLeft) pNewRight{};
      if (uNewRight)
         pNewRight = mTracks->Add(std::move(uNewRight));

      // If we're just rendering (not mixing), keep the track name the same
      if (selectedCount==1) {
         pNewLeft->SetName(firstName);
         if (pNewRight)
            pNewRight->SetName(firstName);
      }

      // Smart history/undo message
      if (selectedCount==1) {
         wxString msg;
         msg.Printf(_("Rendered all audio in track '%s'"), firstName.c_str());
         /* i18n-hint: Convert the audio into a more usable form, so apply
          * panning and amplification and write to some external file.*/
         PushState(msg, _("Render"));
      }
      else {
         wxString msg;
         if (pNewRight)
            msg.Printf(_("Mixed and rendered %d tracks into one new stereo track"),
                       selectedCount);
         else
            msg.Printf(_("Mixed and rendered %d tracks into one new mono track"),
                       selectedCount);
         PushState(msg, _("Mix and Render"));
      }

      mTrackPanel->SetFocus();
      mTrackPanel->SetFocusedTrack(pNewLeft);
      mTrackPanel->EnsureVisible(pNewLeft);
      RedrawProject();
   }
}

void AudacityProject::OnMixAndRender()
{
   HandleMixAndRender(false);
}

void AudacityProject::OnMixAndRenderToNewTrack()
{
   HandleMixAndRender(true);
}

void AudacityProject::OnSelectionSave()
{
   mRegionSave =  mViewInfo.selectedRegion;
}

void AudacityProject::OnSelectionRestore()
{
   if ((mRegionSave.t0() == 0.0) &&
       (mRegionSave.t1() == 0.0))
      return;

   mViewInfo.selectedRegion = mRegionSave;

   ModifyState(false);

   mTrackPanel->Refresh(false);
}

void AudacityProject::OnCursorTrackStart()
{
   double minOffset = 1000000.0;

   TrackListIterator iter(GetTracks());
   Track *t = iter.First();

   while (t) {
      if (t->GetSelected()) {
         if (t->GetOffset() < minOffset)
            minOffset = t->GetOffset();
      }

      t = iter.Next();
   }

   if (minOffset < 0.0) minOffset = 0.0;
   mViewInfo.selectedRegion.setTimes(minOffset, minOffset);
   ModifyState(false);
   mTrackPanel->ScrollIntoView(mViewInfo.selectedRegion.t0());
   mTrackPanel->Refresh(false);
}

void AudacityProject::OnCursorTrackEnd()
{
   double maxEndOffset = -1000000.0;
   double thisEndOffset = 0.0;

   TrackListIterator iter(GetTracks());
   Track *t = iter.First();

   while (t) {
      if (t->GetSelected()) {
         thisEndOffset = t->GetEndTime();
         if (thisEndOffset > maxEndOffset)
            maxEndOffset = thisEndOffset;
      }

      t = iter.Next();
   }

   mViewInfo.selectedRegion.setTimes(maxEndOffset, maxEndOffset);
   ModifyState(false);
   mTrackPanel->ScrollIntoView(mViewInfo.selectedRegion.t1());
   mTrackPanel->Refresh(false);
}

void AudacityProject::OnCursorSelStart()
{
   mViewInfo.selectedRegion.collapseToT0();
   ModifyState(false);
   mTrackPanel->ScrollIntoView(mViewInfo.selectedRegion.t0());
   mTrackPanel->Refresh(false);
}

void AudacityProject::OnCursorSelEnd()
{
   mViewInfo.selectedRegion.collapseToT1();
   ModifyState(false);
   mTrackPanel->ScrollIntoView(mViewInfo.selectedRegion.t1());
   mTrackPanel->Refresh(false);
}

void AudacityProject::HandleAlign(int index, bool moveSel)
{
   TrackListIterator iter(GetTracks());
   wxString action;
   wxString shortAction;
   double offset;
   double minOffset = DBL_MAX;
   double maxEndOffset = 0.0;
   double leftOffset = 0.0;
   bool bRightChannel = false;
   double avgOffset = 0.0;
   int numSelected = 0;
   Track *t = iter.First();
   double delta = 0.0;
   double newPos = -1.0;
   wxArrayDouble trackStartArray;
   wxArrayDouble trackEndArray;
   double firstTrackOffset=0.0f;

   while (t) {
      // We only want Wave and Note tracks here.
#if defined(USE_MIDI)
      if (t->GetSelected() && ((t->GetKind() == Track::Wave) ||
                               (t->GetKind() == Track::Note))) {
#else
      if (t->GetSelected() && (t->GetKind() == Track::Wave)) {
#endif
         offset = t->GetOffset();
         if (t->GetLinked()) {   // Left channel of stereo track.
            leftOffset = offset;
            bRightChannel = true; // next track is the right channel.
         } else {
            if (bRightChannel) {
               // Align channel with earlier start  time
               offset = (offset < leftOffset)? offset : leftOffset;
               leftOffset = 0.0;
               bRightChannel = false;
            }
            avgOffset += offset;
            if (numSelected == 0) {
               firstTrackOffset = offset; // For Align End to End.
            }
            numSelected++;
         }
         trackStartArray.Add(t->GetStartTime());
         trackEndArray.Add(t->GetEndTime());

         if (offset < minOffset)
            minOffset = offset;
         if (t->GetEndTime() > maxEndOffset)
            maxEndOffset = t->GetEndTime();
      }
      t = iter.Next();
   }

   avgOffset /= numSelected;  // numSelected is mono/stereo tracks not channels.

   switch(index) {
   case kAlignStartZero:
      delta = -minOffset;
      action = _("start to zero");
      shortAction = _("Start");
      break;
   case kAlignStartSelStart:
      delta = mViewInfo.selectedRegion.t0() - minOffset;
      action = _("start to cursor/selection start");
      shortAction = _("Start");
      break;
   case kAlignStartSelEnd:
      delta = mViewInfo.selectedRegion.t1() - minOffset;
      action = _("start to selection end");
      shortAction = _("Start");
      break;
   case kAlignEndSelStart:
      delta = mViewInfo.selectedRegion.t0() - maxEndOffset;
      action = _("end to cursor/selection start");
      shortAction = _("End");
      break;
   case kAlignEndSelEnd:
      delta = mViewInfo.selectedRegion.t1() - maxEndOffset;
      action = _("end to selection end");
      shortAction = _("End");
      break;
   // index set in alignLabelsNoSync
   case kAlignEndToEnd:
      newPos = firstTrackOffset;
      action = _("end to end");
      shortAction = _("End to End");
      break;
   case kAlignTogether:
      newPos = avgOffset;
      action = _("together");
      shortAction = _("Together");
   }

   if ((unsigned)index >= mAlignLabelsCount) { // This is an alignLabelsNoSync command.
      TrackListIterator iter(GetTracks());
      Track *t = iter.First();
      double leftChannelStart = 0.0;
      double leftChannelEnd = 0.0;
      double rightChannelStart = 0.0;
      double rightChannelEnd = 0.0;
      int arrayIndex = 0;
      while (t) {
         // This shifts different tracks in different ways, so no sync-lock move.
         // Only align Wave and Note tracks end to end.
#if defined(USE_MIDI)
         if (t->GetSelected() && ((t->GetKind() == Track::Wave) ||
                                  (t->GetKind() == Track::Note))) {
#else
         if (t->GetSelected() && (t->GetKind() == Track::Wave)) {
#endif
            t->SetOffset(newPos);   // Move the track

            if (t->GetLinked()) {   // Left channel of stereo track.
               leftChannelStart = trackStartArray[arrayIndex];
               leftChannelEnd = trackEndArray[arrayIndex];
               rightChannelStart = trackStartArray[1+arrayIndex];
               rightChannelEnd = trackEndArray[1+arrayIndex];
               bRightChannel = true;   // next track is the right channel.
               // newPos is the offset for the earlier channel.
               // If right channel started first, offset the left channel.
               if (rightChannelStart < leftChannelStart) {
                  t->SetOffset(newPos + leftChannelStart - rightChannelStart);
               }
               arrayIndex++;
            } else {
               if (bRightChannel) {
                  // If left channel started first, offset the right channel.
                  if (leftChannelStart < rightChannelStart) {
                     t->SetOffset(newPos + rightChannelStart - leftChannelStart);
                  }
                  if (index == kAlignEndToEnd) {
                     // Now set position for start of next track.
                     newPos += wxMax(leftChannelEnd, rightChannelEnd) - wxMin(leftChannelStart, rightChannelStart);
                  }
                  bRightChannel = false;
               } else { // Mono track
                  if (index == kAlignEndToEnd) {
                     newPos += (trackEndArray[arrayIndex] - trackStartArray[arrayIndex]);
                  }
               }
               arrayIndex++;
            }
         }
         t = iter.Next();
      }
      if (index == kAlignEndToEnd) {
         OnZoomFit();
      }
   }

   if (delta != 0.0) {
      TrackListIterator iter(GetTracks());
      Track *t = iter.First();

      while (t) {
         // For a fixed-distance shift move sync-lock selected tracks also.
         if (t->GetSelected() || t->IsSyncLockSelected()) {
            t->SetOffset(t->GetOffset() + delta);
         }
         t = iter.Next();
      }
   }

   if (moveSel) {
      mViewInfo.selectedRegion.move(delta);
      action = wxString::Format(_("Aligned/Moved %s"), action.c_str());
      shortAction = wxString::Format(_("Align %s/Move"),shortAction.c_str());
      PushState(action, shortAction);
   } else {
      action = wxString::Format(_("Aligned %s"), action.c_str());
      shortAction = wxString::Format(_("Align %s"),shortAction.c_str());
      PushState(action, shortAction);
   }

   RedrawProject();
}

void AudacityProject::OnAlignNoSync(int index)
{
   // Add length of alignLabels array so that we can handle this in AudacityProject::HandleAlign.
   HandleAlign(index + mAlignLabelsCount, false);
}

void AudacityProject::OnAlign(int index)
{
   HandleAlign(index, false);
}

void AudacityProject::OnAlignMoveSel(int index)
{
   HandleAlign(index, true);
}

#ifdef EXPERIMENTAL_SCOREALIGN
// rough relative amount of time to compute one
//    frame of audio or midi, or one cell of matrix, or one iteration
//    of smoothing, measured on a 1.9GHz Core 2 Duo in 32-bit mode
//    (see COLLECT_TIMING_DATA below)
#define AUDIO_WORK_UNIT 0.004F
#define MIDI_WORK_UNIT 0.0001F
#define MATRIX_WORK_UNIT 0.000002F
#define SMOOTHING_WORK_UNIT 0.000001F

// Write timing data to a file; useful for calibrating AUDIO_WORK_UNIT,
// MIDI_WORK_UNIT, MATRIX_WORK_UNIT, and SMOOTHING_WORK_UNIT coefficients
// Data is written to timing-data.txt; look in
//     audacity-src/win/Release/modules/
#define COLLECT_TIMING_DATA

// Audacity Score Align Progress class -- progress reports come here
class ASAProgress final : public SAProgress {
 private:
   float mTotalWork;
   float mFrames[2];
   long mTotalCells; // how many matrix cells?
   long mCellCount; // how many cells so far?
   long mPrevCellCount; // cell_count last reported with Update()
   Maybe<ProgressDialog> mProgress;
   #ifdef COLLECT_TIMING_DATA
      FILE *mTimeFile;
      wxDateTime mStartTime;
      long iterations;
   #endif

 public:
   ASAProgress() {
      smoothing = false;
      #ifdef COLLECT_TIMING_DATA
         mTimeFile = fopen("timing-data.txt", "w");
      #endif
   }
   ~ASAProgress() {
      #ifdef COLLECT_TIMING_DATA
         fclose(mTimeFile);
      #endif
   }
   void set_phase(int i) override {
      float work[2]; // chromagram computation work estimates
      float work2, work3 = 0; // matrix and smoothing work estimates
      SAProgress::set_phase(i);
      #ifdef COLLECT_TIMING_DATA
         long ms = 0;
         wxDateTime now = wxDateTime::UNow();
         fprintf(mTimeFile, "Phase %d begins at %s\n",
                 i, now.FormatTime().c_str());
         if (i != 0)
            ms = now.Subtract(mStartTime).GetMilliseconds().ToLong();
         mStartTime = now;
      #endif
      if (i == 0) {
         mCellCount = 0;
         for (int j = 0; j < 2; j++) {
            mFrames[j] = durations[j] / frame_period;
         }
         mTotalWork = 0;
         for (int j = 0; j < 2; j++) {
             work[j] =
                (is_audio[j] ? AUDIO_WORK_UNIT : MIDI_WORK_UNIT) * mFrames[j];
             mTotalWork += work[j];
         }
         mTotalCells = mFrames[0] * mFrames[1];
         work2 = mTotalCells * MATRIX_WORK_UNIT;
         mTotalWork += work2;
         // arbitarily assume 60 iterations to fit smooth segments and
         // per frame per iteration is SMOOTHING_WORK_UNIT
         if (smoothing) {
            work3 =
               wxMax(mFrames[0], mFrames[1]) * SMOOTHING_WORK_UNIT * 40;
            mTotalWork += work3;
         }
         #ifdef COLLECT_TIMING_DATA
            fprintf(mTimeFile, " mTotalWork (an estimate) = %g\n", mTotalWork);
            fprintf(mTimeFile, " work0 = %g, frames %g, is_audio %d\n",
                    work[0], mFrames[0], is_audio[0]);
            fprintf(mTimeFile, " work1 = %g, frames %g, is_audio %d\n",
                    work[1], mFrames[1], is_audio[1]);
            fprintf(mTimeFile, "work2 = %g, work3 = %g\n", work2, work3);
         #endif
         mProgress.create(_("Synchronize MIDI with Audio"),
                               _("Synchronizing MIDI and Audio Tracks"));
      } else if (i < 3) {
         fprintf(mTimeFile,
               "Phase %d took %d ms for %g frames, coefficient = %g s/frame\n",
               i - 1, ms, mFrames[i - 1], (ms * 0.001) / mFrames[i - 1]);
      } else if (i == 3) {
        fprintf(mTimeFile,
                "Phase 2 took %d ms for %d cells, coefficient = %g s/cell\n",
                ms, mCellCount, (ms * 0.001) / mCellCount);
      } else if (i == 4) {
        fprintf(mTimeFile, "Phase 3 took %d ms for %d iterations on %g frames, coefficient = %g s per frame per iteration\n",
                ms, iterations, wxMax(mFrames[0], mFrames[1]),
                (ms * 0.001) / (wxMax(mFrames[0], mFrames[1]) * iterations));
      }
   }
   bool set_feature_progress(float s) override {
      float work;
      if (phase == 0) {
         float f = s / frame_period;
         work = (is_audio[0] ? AUDIO_WORK_UNIT : MIDI_WORK_UNIT) * f;
      } else if (phase == 1) {
         float f = s / frame_period;
         work = (is_audio[0] ? AUDIO_WORK_UNIT : MIDI_WORK_UNIT) * mFrames[0] +
                (is_audio[1] ? AUDIO_WORK_UNIT : MIDI_WORK_UNIT) * f;
      }
      int updateResult = mProgress->Update((int)(work), (int)(mTotalWork));
      return (updateResult == eProgressSuccess);
   }
   bool set_matrix_progress(int cells) override {
      mCellCount += cells;
      float work =
             (is_audio[0] ? AUDIO_WORK_UNIT : MIDI_WORK_UNIT) * mFrames[0] +
             (is_audio[1] ? AUDIO_WORK_UNIT : MIDI_WORK_UNIT) * mFrames[1];
      work += mCellCount * MATRIX_WORK_UNIT;
      int updateResult = mProgress->Update((int)(work), (int)(mTotalWork));
      return (updateResult == eProgressSuccess);
   }
   bool set_smoothing_progress(int i) override {
      iterations = i;
      float work =
             (is_audio[0] ? AUDIO_WORK_UNIT : MIDI_WORK_UNIT) * mFrames[0] +
             (is_audio[1] ? AUDIO_WORK_UNIT : MIDI_WORK_UNIT) * mFrames[1] +
             MATRIX_WORK_UNIT * mFrames[0] * mFrames[1];
      work += i * wxMax(mFrames[0], mFrames[1]) * SMOOTHING_WORK_UNIT;
      int updateResult = mProgress->Update((int)(work), (int)(mTotalWork));
      return (updateResult == eProgressSuccess);
   }
};


long mixer_process(void *mixer, float **buffer, long n)
{
   Mixer *mix = (Mixer *) mixer;
   long frame_count = mix->Process(std::max(0L, n));
   *buffer = (float *) mix->GetBuffer();
   return frame_count;
}

void AudacityProject::OnScoreAlign()
{
   TrackListIterator iter(GetTracks());
   Track *t = iter.First();
   int numWaveTracksSelected = 0;
   int numNoteTracksSelected = 0;
   int numOtherTracksSelected = 0;
   NoteTrack *nt;
   double endTime = 0.0;

   // Iterate through once to make sure that there is exactly
   // one WaveTrack and one NoteTrack selected.
   while (t) {
      if (t->GetSelected()) {
         if (t->GetKind() == Track::Wave) {
            numWaveTracksSelected++;
            WaveTrack *wt = (WaveTrack *) t;
            endTime = endTime > wt->GetEndTime() ? endTime : wt->GetEndTime();
         } else if(t->GetKind() == Track::Note) {
            numNoteTracksSelected++;
            nt = (NoteTrack *) t;
         } else numOtherTracksSelected++;
      }
      t = iter.Next();
   }

   if(numWaveTracksSelected == 0 ||
      numNoteTracksSelected != 1 ||
      numOtherTracksSelected != 0){
      wxMessageBox(wxString::Format(wxT("Please select at least one audio track and one MIDI track.")));
      return;
   }

   // Creating the dialog also stores dialog into gScoreAlignDialog so
   // that it can be delted by CloseScoreAlignDialog() either here or
   // if the program is quit by the user while the dialog is up.
   ScoreAlignParams params;

   // safe because the class maintains a global resource pointer
   safenew ScoreAlignDialog(params);

   CloseScoreAlignDialog();

   if (params.mStatus != wxID_OK) return;

   // We're going to do it.
   //pushing the state before the change is wrong (I think)
   //PushState(_("Sync MIDI with Audio"), _("Sync MIDI with Audio"));
   // Make a copy of the note track in case alignment is canceled or fails
   auto holder = nt->Duplicate();
   auto alignedNoteTrack = static_cast<NoteTrack*>(holder.get());
   // Duplicate() on note tracks serializes seq to a buffer, but we need
   // the seq, so Duplicate again and discard the track with buffer. The
   // test is here in case Duplicate() is changed in the future.
   if (alignedNoteTrack->GetSequence() == NULL) {
      holder = alignedNoteTrack->Duplicate();
      alignedNoteTrack = static_cast<NoteTrack*>(holder.get());
      assert(alignedNoteTrack->GetSequence());
   }
   // Remove offset from NoteTrack because audio is
   // mixed starting at zero and incorporating clip offsets.
   if (alignedNoteTrack->GetOffset() < 0) {
      // remove the negative offset data before alignment
      nt->Clear(alignedNoteTrack->GetOffset(), 0);
   } else if (alignedNoteTrack->GetOffset() > 0) {
      alignedNoteTrack->Shift(alignedNoteTrack->GetOffset());
   }
   alignedNoteTrack->SetOffset(0);

   WaveTrackConstArray waveTracks =
      mTracks->GetWaveTrackConstArray(true /* selectionOnly */);

   int result;
   {
      Mixer mix(
         waveTracks,              // const WaveTrackConstArray &inputTracks
         Mixer::WarpOptions{ mTracks->GetTimeTrack() }, // const WarpOptions &warpOptions
         0.0,                     // double startTime
         endTime,                 // double stopTime
         2,                       // int numOutChannels
         44100u,                   // size_t outBufferSize
         true,                    // bool outInterleaved
         mRate,                   // double outRate
         floatSample,             // sampleFormat outFormat
         true,                    // bool highQuality = true
         NULL);                   // MixerSpec *mixerSpec = NULL

      ASAProgress progress;

      // There's a lot of adjusting made to incorporate the note track offset into
      // the note track while preserving the position of notes within beats and
      // measures. For debugging, you can see just the pre-scorealign note track
      // manipulation by setting SKIP_ACTUAL_SCORE_ALIGNMENT. You could then, for
      // example, save the modified note track in ".gro" form to read the details.
      //#define SKIP_ACTUAL_SCORE_ALIGNMENT 1
#ifndef SKIP_ACTUAL_SCORE_ALIGNMENT
      result = scorealign((void *) &mix, &mixer_process,
         2 /* channels */, 44100.0 /* srate */, endTime,
         alignedNoteTrack->GetSequence(), &progress, params);
#else
      result = SA_SUCCESS;
#endif
   }

   if (result == SA_SUCCESS) {
      mTracks->Replace(nt, std::move(holder));
      RedrawProject();
      wxMessageBox(wxString::Format(
         _("Alignment completed: MIDI from %.2f to %.2f secs, Audio from %.2f to %.2f secs."),
         params.mMidiStart, params.mMidiEnd,
         params.mAudioStart, params.mAudioEnd));
      PushState(_("Sync MIDI with Audio"), _("Sync MIDI with Audio"));
   } else if (result == SA_TOOSHORT) {
      wxMessageBox(wxString::Format(
         _("Alignment error: input too short: MIDI from %.2f to %.2f secs, Audio from %.2f to %.2f secs."),
         params.mMidiStart, params.mMidiEnd,
         params.mAudioStart, params.mAudioEnd));
   } else if (result == SA_CANCEL) {
      // wrong way to recover...
      //GetActiveProject()->OnUndo(); // recover any changes to note track
      return; // no message when user cancels alignment
   } else {
      //GetActiveProject()->OnUndo(); // recover any changes to note track
      wxMessageBox(_("Internal error reported by alignment process."));
   }
}
#endif /* EXPERIMENTAL_SCOREALIGN */


void AudacityProject::OnNewWaveTrack()
{
   auto t = mTracks->Add(mTrackFactory->NewWaveTrack(mDefaultFormat, mRate));
   SelectNone();

   t->SetSelected(true);

   PushState(_("Created new audio track"), _("New Track"));

   RedrawProject();
   mTrackPanel->EnsureVisible(t);
}

void AudacityProject::OnNewStereoTrack()
{
   auto t = mTracks->Add(mTrackFactory->NewWaveTrack(mDefaultFormat, mRate));
   t->SetChannel(Track::LeftChannel);
   SelectNone();

   t->SetSelected(true);
   t->SetLinked (true);

   t = mTracks->Add(mTrackFactory->NewWaveTrack(mDefaultFormat, mRate));
   t->SetChannel(Track::RightChannel);

   t->SetSelected(true);

   PushState(_("Created new stereo audio track"), _("New Track"));

   RedrawProject();
   mTrackPanel->EnsureVisible(t);
}

void AudacityProject::OnNewLabelTrack()
{
   auto t = mTracks->Add(GetTrackFactory()->NewLabelTrack());

   SelectNone();

   t->SetSelected(true);

   PushState(_("Created new label track"), _("New Track"));

   RedrawProject();
   mTrackPanel->EnsureVisible(t);
}

void AudacityProject::OnNewTimeTrack()
{
   if (mTracks->GetTimeTrack()) {
      wxMessageBox(_("This version of Audacity only allows one time track for each project window."));
      return;
   }

   auto t = mTracks->AddToHead(mTrackFactory->NewTimeTrack());

   SelectNone();

   t->SetSelected(true);

   PushState(_("Created new time track"), _("New Track"));

   RedrawProject();
   mTrackPanel->EnsureVisible(t);
}

void AudacityProject::OnTimerRecord()
{
   // MY: Due to improvements in how Timer Recording saves and/or exports
   // it is now safer to disable Timer Recording when there is more than
   // one open project.
   if (GetOpenProjectCount() > 1) {
      wxMessageBox(_("Timer Recording cannot be used with more than one open project.\n\nPlease close any additional projects and try again."),
                   _("Timer Recording"),
                   wxICON_INFORMATION | wxOK);
      return;
   }

   // MY: If the project has unsaved changes then we no longer allow access
   // to Timer Recording.  This decision has been taken as the safest approach
   // preventing issues surrounding "dirty" projects when Automatic Save/Export
   // is used in Timer Recording.
   if ((GetUndoManager()->UnsavedChanges()) && (ProjectHasTracks() || mEmptyCanBeDirty)) {
      wxMessageBox(_("Timer Recording cannot be used while you have unsaved changes.\n\nPlease save or close this project and try again."),
                   _("Timer Recording"),
                   wxICON_INFORMATION | wxOK);
      return;
   }
   // We use this variable to display "Current Project" in the Timer Recording save project field
   bool bProjectSaved = IsProjectSaved();

   //we break the prompting and waiting dialogs into two sections
   //because they both give the user a chance to click cancel
   //and therefore remove the newly inserted track.

   TimerRecordDialog dialog(this, bProjectSaved); /* parent, project saved? */
   int modalResult = dialog.ShowModal();
   if (modalResult == wxID_CANCEL)
   {
      // Cancelled before recording - don't need to do anyting.
   }
   else
   {
      int iTimerRecordingOutcome = dialog.RunWaitDialog();
      switch (iTimerRecordingOutcome) {
      case POST_TIMER_RECORD_CANCEL_WAIT:
         // Canceled on the wait dialog
         if (GetUndoManager()->UndoAvailable()) {
            // MY: We need to roll back what changes we have made here
            OnUndo();
         }
      break;
      case POST_TIMER_RECORD_CANCEL:
         // RunWaitDialog() shows the "wait for start" as well as "recording" dialog
         // if it returned POST_TIMER_RECORD_CANCEL it means the user cancelled while the recording, so throw out the fresh track.
         // However, we can't undo it here because the PushState() is called in TrackPanel::OnTimer(),
         // which is blocked by this function.
         // so instead we mark a flag to undo it there.
         mTimerRecordCanceled = true;
      break;
      case POST_TIMER_RECORD_NOTHING:
         // No action required
      break;
      case POST_TIMER_RECORD_CLOSE:
         // Quit Audacity
         exit(0);
      break;
      case POST_TIMER_RECORD_RESTART:
         // Restart System
#ifdef __WINDOWS__
         system("shutdown /r /f /t 30");
#endif
      break;
      case POST_TIMER_RECORD_SHUTDOWN:
         // Shutdown System
#ifdef __WINDOWS__
         system("shutdown /s /f /t 30");
#endif
      break;
      }
   }
}

void AudacityProject::OnSoundActivated()
{
   SoundActivatedRecord dialog(this /* parent */ );
   dialog.ShowModal();
}

void AudacityProject::OnRescanDevices()
{
   DeviceManager::Instance()->Rescan();
}

int AudacityProject::DoAddLabel(const SelectedRegion &region, bool preserveFocus)
{
   LabelTrack *lt = NULL;

   // If the focused track is a label track, use that
   Track *const pFocusedTrack = mTrackPanel->GetFocusedTrack();
   Track *t = pFocusedTrack;
   if (t && t->GetKind() == Track::Label) {
      lt = (LabelTrack *) t;
   }

   // Otherwise look for a label track after the focused track
   if (!lt) {
      TrackListIterator iter(GetTracks());
      if (t)
         iter.StartWith(t);
      else
         t = iter.First();

      while (t && !lt) {
         if (t->GetKind() == Track::Label)
            lt = (LabelTrack *) t;

         t = iter.Next();
      }
   }

   // If none found, start a NEW label track and use it
   if (!lt) {
      lt = static_cast<LabelTrack*>
         (mTracks->Add(GetTrackFactory()->NewLabelTrack()));
   }

// LLL: Commented as it seemed a little forceful to remove users
//      selection when adding the label.  This does not happen if
//      you select several tracks and the last one of those is a
//      label track...typing a label will not clear the selections.
//
//   SelectNone();
   lt->SetSelected(true);

   int focusTrackNumber = -1;
   if (pFocusedTrack && preserveFocus) {
      // Must remember the track to re-focus after finishing a label edit.
      // do NOT identify it by a pointer, which might dangle!  Identify
      // by position.
      TrackListIterator iter(GetTracks());
      Track *track = iter.First();
      do
         ++focusTrackNumber;
      while (track != pFocusedTrack &&
             NULL != (track = iter.Next()));
      if (!track)
         // How could we not find it?
         focusTrackNumber = -1;
   }

   int index = lt->AddLabel(region, wxString(), focusTrackNumber);

   PushState(_("Added label"), _("Label"));

   RedrawProject();
   mTrackPanel->EnsureVisible((Track *)lt);
   mTrackPanel->SetFocus();

   return index;
}

void AudacityProject::OnSyncLock()
{
   bool bSyncLockTracks;
   gPrefs->Read(wxT("/GUI/SyncLockTracks"), &bSyncLockTracks, false);
   gPrefs->Write(wxT("/GUI/SyncLockTracks"), !bSyncLockTracks);
   gPrefs->Flush();

   // Toolbar, project sync-lock handled within
   ModifyAllProjectToolbarMenus();

   mTrackPanel->Refresh(false);
}

void AudacityProject::OnAddLabel()
{
   DoAddLabel(mViewInfo.selectedRegion);
}

void AudacityProject::OnAddLabelPlaying()
{
   if (GetAudioIOToken()>0 &&
       gAudioIO->IsStreamActive(GetAudioIOToken())) {
      double indicator = gAudioIO->GetStreamTime();
      DoAddLabel(SelectedRegion(indicator, indicator), true);
   }
}

void AudacityProject::DoEditLabels(LabelTrack *lt, int index)
{
   wxString format = GetSelectionFormat(),
      freqFormat = GetFrequencySelectionFormatName();

   LabelDialog dlg(this, *GetTrackFactory(), GetTracks(),
                   lt, index,
                   mViewInfo, mRate,
                   format, freqFormat);

   if (dlg.ShowModal() == wxID_OK) {
      PushState(_("Edited labels"), _("Label"));
      RedrawProject();
   }
}

void AudacityProject::OnEditLabels()
{
   DoEditLabels();
}

void AudacityProject::OnApplyChain()
{
   BatchProcessDialog dlg(this);
   dlg.ShowModal();
   ModifyUndoMenuItems();
}

void AudacityProject::OnEditChains()
{
   EditChainsDialog dlg(this);
   dlg.ShowModal();
}

void AudacityProject::OnRemoveTracks()
{
   TrackListIterator iter(GetTracks());
   Track *t = iter.First();
   Track *f = NULL;
   Track *l = NULL;

   while (t) {
      if (t->GetSelected()) {
         if (mMixerBoard && (t->GetKind() == Track::Wave))
            mMixerBoard->RemoveTrackCluster((WaveTrack*)t);
         if (!f)
            f = l;         // Capture the track preceeding the first removed track
         t = iter.RemoveCurrent();
      }
      else {
         l = t;
         t = iter.Next();
      }
   }

   // All tracks but the last were removed...try to use the last track
   if (!f)
      f = l;

   // Try to use the first track after the removal or, if none,
   // the track preceeding the removal
   if (f) {
      t = mTracks->GetNext(f, true);
      if (t)
         f = t;
   }

   // If we actually have something left, then make sure it's seen
   if (f)
      mTrackPanel->EnsureVisible(f);

   PushState(_("Removed audio track(s)"), _("Remove Track"));

   mTrackPanel->UpdateViewIfNoTracks();
   mTrackPanel->Refresh(false);

   if (mMixerBoard)
      mMixerBoard->Refresh(true);
}

//
// Help Menu
//

void AudacityProject::OnAbout()
{
#ifdef __WXMAC__
   // Modeless dialog, consistent with other Mac applications
   wxCommandEvent dummy;
   wxGetApp().OnMenuAbout(dummy);
#else
   // Windows and Linux still modal.
   AboutDialog dlog(this);
   dlog.ShowModal();
#endif
}

void AudacityProject::OnHelpWelcome()
{
   SplashDialog::Show2( this );
}

void AudacityProject::OnQuickHelp()
{
   HelpSystem::ShowHelpDialog(
      this,
      wxT("Quick_Help"));
}

void AudacityProject::OnManual()
{
   HelpSystem::ShowHelpDialog(
      this,
      wxT("Main_Page"));
}

void AudacityProject::OnCheckForUpdates()
{
   ::OpenInDefaultBrowser( wxString( wxT("http://audacityteam.org/download/?from_ver=")) + AUDACITY_VERSION_STRING );
}

void AudacityProject::OnShowLog()
{
   AudacityLogger *logger = wxGetApp().GetLogger();
   if (logger) {
      logger->Show();
   }
}

void AudacityProject::OnBenchmark()
{
   ::RunBenchmark(this);
}

#if defined(EXPERIMENTAL_CRASH_REPORT)
void AudacityProject::OnCrashReport()
{
// Change to "1" to test a real crash
#if 0
   char *p = 0;
   *p = 1234;
#endif
   wxGetApp().GenerateCrashReport(wxDebugReport::Context_Current);
}
#endif

void AudacityProject::OnScreenshot()
{
   ::OpenScreenshotTools();
}

void AudacityProject::OnAudioDeviceInfo()
{
   wxString info = gAudioIO->GetDeviceInfo();

   wxDialogWrapper dlg(this, wxID_ANY, wxString(_("Audio Device Info")));
   dlg.SetName(dlg.GetTitle());
   ShuttleGui S(&dlg, eIsCreating);

   wxTextCtrl *text;
   S.StartVerticalLay();
   {
      S.SetStyle(wxTE_MULTILINE | wxTE_READONLY);
      text = S.Id(wxID_STATIC).AddTextWindow(info);
      S.AddStandardButtons(eOkButton | eCancelButton);
   }
   S.EndVerticalLay();

   dlg.FindWindowById(wxID_OK)->SetLabel(_("&Save"));
   dlg.SetSize(350, 450);

   if (dlg.ShowModal() == wxID_OK)
   {
      wxString fName = FileSelector(_("Save Device Info"),
                                    wxEmptyString,
                                    wxT("deviceinfo.txt"),
                                    wxT("txt"),
                                    wxT("*.txt"),
                                    wxFD_SAVE | wxFD_OVERWRITE_PROMPT | wxRESIZE_BORDER,
                                    this);
      if (!fName.IsEmpty())
      {
         if (!text->SaveFile(fName))
         {
            wxMessageBox(_("Unable to save device info"), _("Save Device Info"));
         }
      }
   }
}

void AudacityProject::OnSeparator()
{

}

void AudacityProject::OnCollapseAllTracks()
{
   TrackListIterator iter(GetTracks());
   Track *t = iter.First();

   while (t)
   {
      t->SetMinimized(true);
      t = iter.Next();
   }

   ModifyState(true);
   RedrawProject();
}

void AudacityProject::OnExpandAllTracks()
{
   TrackListIterator iter(GetTracks());
   Track *t = iter.First();

   while (t)
   {
      t->SetMinimized(false);
      t = iter.Next();
   }

   ModifyState(true);
   RedrawProject();
}


void AudacityProject::OnMuteAllTracks()
{
   TrackListIterator iter(GetTracks());
   Track *t = iter.First();

   while (t)
   {
      if (t->GetKind() == Track::Wave)
         t->SetMute(true);

      t = iter.Next();
   }

   ModifyState(true);
   RedrawProject();
   if (mMixerBoard)
      mMixerBoard->UpdateMute();
}

void AudacityProject::OnUnMuteAllTracks()
{
   TrackListIterator iter(GetTracks());
   Track *t = iter.First();

   while (t)
   {
      t->SetMute(false);
      t = iter.Next();
   }

   ModifyState(true);
   RedrawProject();
   if (mMixerBoard)
      mMixerBoard->UpdateMute();
}

void AudacityProject::OnLockPlayRegion()
{
   double start, end;
   GetPlayRegion(&start, &end);
   if (start >= mTracks->GetEndTime()) {
       wxMessageBox(_("Cannot lock region beyond\nend of project."),
                    _("Error"));
   }
   else {
      mLockPlayRegion = true;
      mRuler->Refresh(false);
   }
}

void AudacityProject::OnUnlockPlayRegion()
{
   mLockPlayRegion = false;
   mRuler->Refresh(false);
}

void AudacityProject::OnResample()
{
   TrackListIterator iter(GetTracks());

   int newRate;

   while (true)
   {
      wxDialogWrapper dlg(this, wxID_ANY, wxString(_("Resample")));
      dlg.SetName(dlg.GetTitle());
      ShuttleGui S(&dlg, eIsCreating);
      wxString rate;
      wxArrayString rates;
      wxComboBox *cb;

      rate.Printf(wxT("%ld"), lrint(mRate));

      rates.Add(wxT("8000"));
      rates.Add(wxT("11025"));
      rates.Add(wxT("16000"));
      rates.Add(wxT("22050"));
      rates.Add(wxT("32000"));
      rates.Add(wxT("44100"));
      rates.Add(wxT("48000"));
      rates.Add(wxT("88200"));
      rates.Add(wxT("96000"));
      rates.Add(wxT("176400"));
      rates.Add(wxT("192000"));
      rates.Add(wxT("352800"));
      rates.Add(wxT("384000"));

      S.StartVerticalLay(true);
      {
         S.AddSpace(-1, 15);

         S.StartHorizontalLay(wxCENTER, false);
         {
            cb = S.AddCombo(_("New sample rate (Hz):"),
                            rate,
                            &rates);
         }
         S.EndHorizontalLay();

         S.AddSpace(-1, 15);

         S.AddStandardButtons();
      }
      S.EndVerticalLay();

      dlg.Layout();
      dlg.Fit();
      dlg.Center();

      if (dlg.ShowModal() != wxID_OK)
      {
         return;  // user cancelled dialog
      }

      long lrate;
      if (cb->GetValue().ToLong(&lrate) && lrate >= 1 && lrate <= 1000000)
      {
         newRate = (int)lrate;
         break;
      }

      wxMessageBox(_("The entered value is invalid"), _("Error"),
                   wxICON_ERROR, this);
   }

   int ndx = 0;
   for (Track *t = iter.First(); t; t = iter.Next())
   {
      wxString msg;

      msg.Printf(_("Resampling track %d"), ++ndx);

      ProgressDialog progress(_("Resample"), msg);

      if (t->GetSelected() && t->GetKind() == Track::Wave)
         if (!((WaveTrack*)t)->Resample(newRate, &progress))
            break;
   }

   PushState(_("Resampled audio track(s)"), _("Resample Track"));
   RedrawProject();

   // Need to reset
   FinishAutoScroll();
}

void AudacityProject::OnSnapToOff()
{
   SetSnapTo(SNAP_OFF);
}

void AudacityProject::OnSnapToNearest()
{
   SetSnapTo(SNAP_NEAREST);
}

void AudacityProject::OnSnapToPrior()
{
   SetSnapTo(SNAP_PRIOR);
}

void AudacityProject::OnFullScreen()
{
   if(wxTopLevelWindow::IsFullScreen())
      wxTopLevelWindow::ShowFullScreen(false);
   else
      wxTopLevelWindow::ShowFullScreen(true);
}

void AudacityProject::OnCursorLeft(bool shift, bool ctrl, bool keyup)
{
   // PRL:  What I found and preserved, strange though it be:
   // During playback:  jump depends on preferences and is independent of the zoom
   // and does not vary if the key is held
   // Else: jump depends on the zoom and gets bigger if the key is held
   int snapToTime = GetSnapTo();
   double quietSeekStepPositive = 1.0; // pixels
   double audioSeekStepPositive = shift ? mSeekLong : mSeekShort;
   SeekLeftOrRight
      (true, shift, ctrl, keyup, snapToTime, true, false,
       quietSeekStepPositive, true,
       audioSeekStepPositive, false);
}

void AudacityProject::OnCursorRight(bool shift, bool ctrl, bool keyup)
{
   // PRL:  What I found and preserved, strange though it be:
   // During playback:  jump depends on preferences and is independent of the zoom
   // and does not vary if the key is held
   // Else: jump depends on the zoom and gets bigger if the key is held
   int snapToTime = GetSnapTo();
   double quietSeekStepPositive = 1.0; // pixels
   double audioSeekStepPositive = shift ? mSeekLong : mSeekShort;
   SeekLeftOrRight
      (false, shift, ctrl, keyup, snapToTime, true, false,
       quietSeekStepPositive, true,
       audioSeekStepPositive, false);
}

// Handle small cursor and play head movements
void AudacityProject::SeekLeftOrRight
(bool leftward, bool shift, bool ctrl, bool keyup,
 int snapToTime, bool mayAccelerateQuiet, bool mayAccelerateAudio,
 double quietSeekStepPositive, bool quietStepIsPixels,
 double audioSeekStepPositive, bool audioStepIsPixels)
{
   if (keyup)
   {
      if (IsAudioActive())
      {
         return;
      }

      ModifyState(false);
      return;
   }

   // If the last adjustment was very recent, we are
   // holding the key down and should move faster.
   const wxLongLong curtime = ::wxGetLocalTimeMillis();
   enum { MIN_INTERVAL = 50 };
   const bool fast = (curtime - mLastSelectionAdjustment < MIN_INTERVAL);

   // How much faster should the cursor move if shift is down?
   enum { LARGER_MULTIPLIER = 4 };
   int multiplier = (fast && mayAccelerateQuiet) ? LARGER_MULTIPLIER : 1;
   if (leftward)
      multiplier = -multiplier;

   if (shift && ctrl)
   {
      mLastSelectionAdjustment = curtime;

      // Contract selection
      // Reduce and constrain (counter-intuitive)
      if (leftward) {
         const double t1 = mViewInfo.selectedRegion.t1();
         mViewInfo.selectedRegion.setT1(
            std::max(mViewInfo.selectedRegion.t0(),
               snapToTime
               ? GridMove(t1, multiplier)
               : quietStepIsPixels
                  ? mViewInfo.OffsetTimeByPixels(
                        t1, (int)(multiplier * quietSeekStepPositive))
                  : t1 +  multiplier * quietSeekStepPositive
         ));

         // Make sure it's visible.
         GetTrackPanel()->ScrollIntoView(mViewInfo.selectedRegion.t1());
      }
      else {
         const double t0 = mViewInfo.selectedRegion.t0();
         mViewInfo.selectedRegion.setT0(
            std::min(mViewInfo.selectedRegion.t1(),
               snapToTime
               ? GridMove(t0, multiplier)
               : quietStepIsPixels
                  ? mViewInfo.OffsetTimeByPixels(
                     t0, (int)(multiplier * quietSeekStepPositive))
                  : t0 + multiplier * quietSeekStepPositive
         ));

         // Make sure NEW position is in view.
         GetTrackPanel()->ScrollIntoView(mViewInfo.selectedRegion.t0());
      }
      GetTrackPanel()->Refresh(false);
   }
   else if (IsAudioActive()) {
#ifdef EXPERIMENTAL_IMPROVED_SEEKING
      if (gAudioIO->GetLastPlaybackTime() < mLastSelectionAdjustment) {
         // Allow time for the last seek to output a buffer before
         // discarding samples again
         // Do not advance mLastSelectionAdjustment
         return;
      }
#endif
      mLastSelectionAdjustment = curtime;

      // Ignore the multiplier for the quiet case
      multiplier = (fast && mayAccelerateAudio) ? LARGER_MULTIPLIER : 1;
      if (leftward)
         multiplier = -multiplier;

      // If playing, reposition
      double seconds;
      if (audioStepIsPixels) {
         const double streamTime = gAudioIO->GetStreamTime();
         const double newTime =
            mViewInfo.OffsetTimeByPixels(streamTime, (int)(audioSeekStepPositive));
         seconds = newTime - streamTime;
      }
      else
         seconds = multiplier * audioSeekStepPositive;
      gAudioIO->SeekStream(seconds);
      return;
   }
   else if (shift)
   {
      mLastSelectionAdjustment = curtime;

      // Extend selection
      // Expand and constrain
      if (leftward) {
         const double t0 = mViewInfo.selectedRegion.t0();
         mViewInfo.selectedRegion.setT0(
            std::max(0.0,
               snapToTime
               ? GridMove(t0, multiplier)
               : quietStepIsPixels
                  ? mViewInfo.OffsetTimeByPixels(
                        t0, (int)(multiplier * quietSeekStepPositive))
                  : t0 + multiplier * quietSeekStepPositive
         ));

         // Make sure it's visible.
         GetTrackPanel()->ScrollIntoView(mViewInfo.selectedRegion.t0());
      }
      else {
         const double end = mTracks->GetEndTime();
         const double t1 = mViewInfo.selectedRegion.t1();
         mViewInfo.selectedRegion.setT1(
            std::min(end,
               snapToTime
               ? GridMove(t1, multiplier)
               : quietStepIsPixels
                  ? mViewInfo.OffsetTimeByPixels(
                        t1, (int)(multiplier * quietSeekStepPositive))
                  : t1 + multiplier * quietSeekStepPositive
         ));

         // Make sure NEW position is in view.
         GetTrackPanel()->ScrollIntoView(mViewInfo.selectedRegion.t1());
      }
      GetTrackPanel()->Refresh(false);
   }
   else
   {
      mLastSelectionAdjustment = curtime;

      // Move the cursor
      // Already in cursor mode?
      if (mViewInfo.selectedRegion.isPoint())
      {
         // Move and constrain
         const double end = mTracks->GetEndTime();
         const double t0 = mViewInfo.selectedRegion.t0();
         mViewInfo.selectedRegion.setT0(
            std::max(0.0,
               std::min(end,
                  snapToTime
                  ? GridMove(t0, multiplier)
                  : quietStepIsPixels
                     ? mViewInfo.OffsetTimeByPixels(
                          t0, (int)(multiplier * quietSeekStepPositive))
                     : t0 + multiplier * quietSeekStepPositive)),
            false // do not swap selection boundaries
         );
         mViewInfo.selectedRegion.collapseToT0();

         // Move the visual cursor, avoiding an unnecessary complete redraw
         GetTrackPanel()->DrawOverlays(false);
         GetRulerPanel()->DrawOverlays(false);

         // This updates the selection shown on the selection bar, and the play region
         TP_DisplaySelection();
      }
      else
      {
         // Transition to cursor mode.
         if (leftward)
            mViewInfo.selectedRegion.collapseToT0();
         else
            mViewInfo.selectedRegion.collapseToT1();
         GetTrackPanel()->Refresh(false);
      }

      // Make sure NEW position is in view
      GetTrackPanel()->ScrollIntoView(mViewInfo.selectedRegion.t1());
   }
}

// Handles moving a selection edge with the keyboard in snap-to-time mode;
// returns the moved value.
// Will move at least minPix pixels -- set minPix positive to move forward,
// negative to move backward.
double AudacityProject::GridMove(double t, int minPix)
{
   NumericConverter nc(NumericConverter::TIME, GetSelectionFormat(), t, GetRate());

   // Try incrementing/decrementing the value; if we've moved far enough we're
   // done
   double result;
   minPix >= 0 ? nc.Increment() : nc.Decrement();
   result = nc.GetValue();
   if (std::abs(mViewInfo.TimeToPosition(result) - mViewInfo.TimeToPosition(t))
       >= abs(minPix))
       return result;

   // Otherwise, move minPix pixels, then snap to the time.
   result = mViewInfo.OffsetTimeByPixels(t, minPix);
   nc.SetValue(result);
   result = nc.GetValue();
   return result;
}

void AudacityProject::OnBoundaryMove(bool left, bool boundaryContract)
{
  // Move the left/right selection boundary, to either expand or contract the selection
  // left=true: operate on left boundary; left=false: operate on right boundary
  // boundaryContract=true: contract region; boundaryContract=false: expand region.

   // If the last adjustment was very recent, we are
   // holding the key down and should move faster.
   wxLongLong curtime = ::wxGetLocalTimeMillis();
   int pixels = 1;
   if( curtime - mLastSelectionAdjustment < 50 )
   {
      pixels = 4;
   }
   mLastSelectionAdjustment = curtime;

   if (IsAudioActive())
   {
      double indicator = gAudioIO->GetStreamTime();
      if (left)
         mViewInfo.selectedRegion.setT0(indicator, false);
      else
         mViewInfo.selectedRegion.setT1(indicator);

      ModifyState(false);
      GetTrackPanel()->Refresh(false);
   }
   else
   {
      // BOUNDARY MOVEMENT
      // Contract selection from the right to the left
      if( boundaryContract )
      {
         if (left) {
            // Reduce and constrain left boundary (counter-intuitive)
            // Move the left boundary by at most the desired number of pixels,
            // but not past the right
            mViewInfo.selectedRegion.setT0(
               std::min(mViewInfo.selectedRegion.t1(),
                  mViewInfo.OffsetTimeByPixels(
                     mViewInfo.selectedRegion.t0(),
                     pixels)));

            // Make sure it's visible
            GetTrackPanel()->ScrollIntoView(mViewInfo.selectedRegion.t0());
         }
         else
         {
            // Reduce and constrain right boundary (counter-intuitive)
            // Move the right boundary by at most the desired number of pixels,
            // but not past the left
            mViewInfo.selectedRegion.setT1(
               std::max(mViewInfo.selectedRegion.t0(),
                  mViewInfo.OffsetTimeByPixels(
                     mViewInfo.selectedRegion.t1(),
                     -pixels)));

            // Make sure it's visible
            GetTrackPanel()->ScrollIntoView(mViewInfo.selectedRegion.t1());
         }
      }
      // BOUNDARY MOVEMENT
      // Extend selection toward the left
      else
      {
         if (left) {
            // Expand and constrain left boundary
            mViewInfo.selectedRegion.setT0(
               std::max(0.0,
                  mViewInfo.OffsetTimeByPixels(
                     mViewInfo.selectedRegion.t0(),
                     -pixels)));

            // Make sure it's visible
            GetTrackPanel()->ScrollIntoView(mViewInfo.selectedRegion.t0());
         }
         else
         {
            // Expand and constrain right boundary
            const double end = mTracks->GetEndTime();
            mViewInfo.selectedRegion.setT1(
               std::min(end,
                  mViewInfo.OffsetTimeByPixels(
                     mViewInfo.selectedRegion.t1(),
                     pixels)));

            // Make sure it's visible
            GetTrackPanel()->ScrollIntoView(mViewInfo.selectedRegion.t1());
         }
      }
      GetTrackPanel()->Refresh( false );
      ModifyState(false);
   }
}

// Move the cursor forward or backward, while paused or while playing.
// forward=true: Move cursor forward; forward=false: Move cursor backwards
// jump=false: Move cursor determined by zoom; jump=true: Use seek times
// longjump=false: Use mSeekShort; longjump=true: Use mSeekLong
void AudacityProject::OnCursorMove(bool forward, bool jump, bool longjump )
{
   // PRL:  nobody calls this yet with !jump

   double positiveSeekStep;
   bool byPixels;
   if (jump) {
      if (!longjump) {
         positiveSeekStep = mSeekShort;
      } else {
         positiveSeekStep = mSeekLong;
      }
      byPixels = false;
   } else {
      positiveSeekStep = 1.0;
      byPixels = true;
   }
   bool mayAccelerate = !jump;
   SeekLeftOrRight
      (!forward, false, false, false,
       0, mayAccelerate, mayAccelerate,
       positiveSeekStep, byPixels,
       positiveSeekStep, byPixels);

   ModifyState(false);
}
