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

#include "Project.h"
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
#include "widgets/TimeTextCtrl.h"
#include "ShuttleGui.h"
#include "HistoryWindow.h"
#include "LyricsWindow.h"
#include "MixerBoard.h"
#include "Internat.h"
#include "FileFormats.h"
#include "LoadModules.h"
#include "Prefs.h"
#include "Printing.h"
#ifdef USE_MIDI
#include "NoteTrack.h"
#endif // USE_MIDI
#include "Tags.h"
#include "Mix.h"
#include "AboutDialog.h"
#include "Benchmark.h"
#include "Screenshot.h"
#include "ondemand/ODManager.h"

#include "Resample.h"
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

#include "Experimental.h"
#include "PlatformCompatibility.h"
#include "FileNames.h"
#include "TimeDialog.h"
#include "TimerRecordDialog.h"
#include "SoundActivatedRecord.h"
#include "LabelDialog.h"
#include "effects/Contrast.h"

#include "FileDialog.h"
#include "SplashDialog.h"
#include "widgets/HelpSystem.h"
#include "DeviceManager.h"

#include "CaptureEvents.h"
#include "Snap.h"

#ifdef EXPERIMENTAL_SCOREALIGN
#include "effects/ScoreAlignDialog.h"
#include "audioreader.h"
#include "scorealign.h"
#include "scorealign-glue.h"
#endif /* EXPERIMENTAL_SCOREALIGN */

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


AudacityProjectCommandFunctor::AudacityProjectCommandFunctor(AudacityProject *project,
                              audCommandFunction commandFunction)
{
   mProject = project;
   mCommandFunction = commandFunction;
   mCommandKeyFunction = NULL;
   mCommandListFunction = NULL;
}

AudacityProjectCommandFunctor::AudacityProjectCommandFunctor(AudacityProject *project,
                              audCommandKeyFunction commandFunction)
{
   mProject = project;
   mCommandFunction = NULL;
   mCommandKeyFunction = commandFunction;
   mCommandListFunction = NULL;
}

AudacityProjectCommandFunctor::AudacityProjectCommandFunctor(AudacityProject *project,
                              audCommandListFunction commandFunction)
{
   mProject = project;
   mCommandFunction = NULL;
   mCommandKeyFunction = NULL;
   mCommandListFunction = commandFunction;
}

AudacityProjectCommandFunctor::AudacityProjectCommandFunctor(AudacityProject *project,
                              audCommandListFunction commandFunction,
                              wxArrayInt explicitIndices)
{
   mProject = project;
   mCommandFunction = NULL;
   mCommandKeyFunction = NULL;
   mCommandListFunction = commandFunction;
   mExplicitIndices = explicitIndices;
}

void AudacityProjectCommandFunctor::operator()(int index, const wxEvent * evt)
{
   if (mCommandListFunction && mExplicitIndices.GetCount() > 0)
      (mProject->*(mCommandListFunction)) (mExplicitIndices[index]);
   else if (mCommandListFunction)
      (mProject->*(mCommandListFunction)) (index);
   else if (mCommandKeyFunction)
      (mProject->*(mCommandKeyFunction)) (evt);
   else
      (mProject->*(mCommandFunction)) ();
}


#define FN(X) new AudacityProjectCommandFunctor(this, &AudacityProject:: X )
#define FNI(X, I) new AudacityProjectCommandFunctor(this, &AudacityProject:: X, I)

/// CreateMenusAndCommands builds the menus, and also rebuilds them after
/// changes in configured preferences - for example changes in key-bindings
/// affect the short-cut key legend that appears beside each command,

void AudacityProject::CreateMenusAndCommands()
{
   CommandManager *c = &mCommandManager;
   EffectManager& em = EffectManager::Get();
   EffectArray *effects;
   wxArrayString names;
   wxArrayInt indices;

   wxMenuBar *menubar = c->AddMenuBar(wxT("appmenu"));

   /////////////////////////////////////////////////////////////////////////////
   // File menu
   /////////////////////////////////////////////////////////////////////////////

   c->BeginMenu(_("&File"));
   c->SetDefaultFlags(AudioIONotBusyFlag, AudioIONotBusyFlag);

   /*i18n-hint: "New" is an action (verb) to create a new project*/
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
   c->AddItem(wxT("SaveCompressed"), _("Save Compressed Copy of Project..."), FN(OnSaveCompressed));
#endif

   c->AddItem(wxT("CheckDeps"), _("Chec&k Dependencies..."), FN(OnCheckDependencies));

   c->AddSeparator();

   c->AddItem(wxT("EditMetaData"), _("Edit Me&tadata..."), FN(OnEditMetadata));

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
   c->AddItem(wxT("ExportMIDI"),   _("Export MIDI..."), FN(OnExportMIDI),
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
   /* i18n-hint: (verb)*/
   c->AddItem(wxT("Copy"), _("&Copy"), FN(OnCopy), wxT("Ctrl+C"),
              AudioIONotBusyFlag | CutCopyAvailableFlag,
              AudioIONotBusyFlag | CutCopyAvailableFlag);
   /* i18n-hint: (verb)*/
   c->AddItem(wxT("Paste"), _("&Paste"), FN(OnPaste), wxT("Ctrl+V"),
              AudioIONotBusyFlag | ClipboardFlag,
              AudioIONotBusyFlag | ClipboardFlag);
   c->AddItem(wxT("Delete"), _("&Delete"), FN(OnDelete), wxT("Ctrl+K"));

   c->AddSeparator();

   c->BeginSubMenu(_("R&emove Special"));
   /* i18n-hint: (verb) Do a special kind of cut*/
   c->AddItem(wxT("SplitCut"), _("Spl&it Cut"), FN(OnSplitCut), wxT("Ctrl+Alt+X"));
   /* i18n-hint: (verb) Do a special kind of delete*/
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

   /* i18n-hint: (verb)*/
   c->AddItem(wxT("Duplicate"), _("Duplic&ate"), FN(OnDuplicate), wxT("Ctrl+D"));

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
              AudioIONotBusyFlag | LabelsSelectedFlag | WaveTracksExistFlag |  TimeSelectedFlag | IsNotSyncLockedFlag,
              AudioIONotBusyFlag | LabelsSelectedFlag | WaveTracksExistFlag |  TimeSelectedFlag | IsNotSyncLockedFlag);
   c->AddItem(wxT("DeleteLabels"), _("&Delete"), FN(OnDeleteLabels), wxT("Alt+K"),
              AudioIONotBusyFlag | LabelsSelectedFlag | WaveTracksExistFlag |  TimeSelectedFlag | IsNotSyncLockedFlag,
              AudioIONotBusyFlag | LabelsSelectedFlag | WaveTracksExistFlag |  TimeSelectedFlag | IsNotSyncLockedFlag);

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
   c->AddItem(wxT("JoinLabels"), _("&Join"),  FN(OnJoinLabels), wxT("Alt+J"));
   c->AddItem(wxT("DisjoinLabels"), _("Detac&h at Silences"), FN(OnDisjoinLabels), wxT("Alt+Shift+J"));

   c->EndSubMenu();

   /////////////////////////////////////////////////////////////////////////////

   /* i18n-hint: (verb) It's an item on a menu. */
   c->BeginSubMenu(_("&Select"));
   c->SetDefaultFlags(TracksExistFlag, TracksExistFlag);

   c->AddItem(wxT("SelectAll"), _("&All"), FN(OnSelectAll), wxT("Ctrl+A"));
   c->AddItem(wxT("SelectNone"), _("&None"), FN(OnSelectNone), wxT("Ctrl+Shift+A"));

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
   c->AddCheck(wxT("ShowMeterTB"), _("&Meter Toolbar"), FN(OnShowMeterToolBar), 0, AlwaysEnabledFlag, AlwaysEnabledFlag);
   /* i18n-hint: Clicking this menu item shows the toolbar with the mixer*/
   c->AddCheck(wxT("ShowMixerTB"), _("Mi&xer Toolbar"), FN(OnShowMixerToolBar), 0, AlwaysEnabledFlag, AlwaysEnabledFlag);
   /* i18n-hint: Clicking this menu item shows the toolbar for selecting audio*/
   c->AddCheck(wxT("ShowSelectionTB"), _("&Selection Toolbar"), FN(OnShowSelectionToolBar), 0, AlwaysEnabledFlag, AlwaysEnabledFlag);
   /* i18n-hint: Clicking this menu item shows a toolbar that has some tools in it*/
   c->AddCheck(wxT("ShowToolsTB"), _("T&ools Toolbar"), FN(OnShowToolsToolBar), 0, AlwaysEnabledFlag, AlwaysEnabledFlag);
   /* i18n-hint: Clicking this menu item shows the toolbar for transcription (currently just vary play speed)*/
   c->AddCheck(wxT("ShowTranscriptionTB"), _("Transcri&ption Toolbar"), FN(OnShowTranscriptionToolBar), 0, AlwaysEnabledFlag, AlwaysEnabledFlag);
   /* i18n-hint: Clicking this menu item shows the toolbar with the big buttons on it (play record etc)*/
   c->AddCheck(wxT("ShowTransportTB"), _("&Transport Toolbar"), FN(OnShowTransportToolBar), 0, AlwaysEnabledFlag, AlwaysEnabledFlag);

   c->AddSeparator();

   /* i18n-hint: (verb)*/
   c->AddItem(wxT("ResetToolbars"), _("&Reset Toolbars"), FN(OnResetToolBars), 0, AlwaysEnabledFlag, AlwaysEnabledFlag);

   c->EndSubMenu();

   /////////////////////////////////////////////////////////////////////////////

   /* i18n-hint: Usually keep the ! at the start.  It means this option is hidden.
    * Simplified View toggles the showing and hiding of 'hidden' menu items that start
    * with !.  If your translation file is for a special use, that is if it is for a
    * simplified view with hidden menu items, then leave the ! out here, so that the
    * user can show/hide some of the menu items. */
   c->AddCheck(wxT("SimplifiedView"), _("!Simplified View"), FN(OnSimplifiedView),
               mCommandManager.mbHideFlaggedItems ? 1 : 0, AlwaysEnabledFlag, AlwaysEnabledFlag);

   c->EndMenu();

   /////////////////////////////////////////////////////////////////////////////
   // Transport Menu
   /////////////////////////////////////////////////////////////////////////////

   /*i18n-hint: 'Transport' is the name given to the set of controls that
   play, record, pause etc. */
   c->BeginMenu(_("T&ransport"));
   c->SetDefaultFlags(AudioIONotBusyFlag, AudioIONotBusyFlag);

   /* i18n-hint: (verb) Start or Stop audio playback*/
   c->AddItem(wxT("PlayStop"), _("Pl&ay/Stop"), FN(OnPlayStop), wxT("Space"),
              AlwaysEnabledFlag,
              AlwaysEnabledFlag);
   c->AddItem(wxT("PlayStopSelect"), _("Play/Stop and &Set Cursor"), FN(OnPlayStopSelect), wxT("Shift+A"),
              AlwaysEnabledFlag,
              AlwaysEnabledFlag);
   c->AddItem(wxT("PlayLooped"), _("&Loop Play"), FN(OnPlayLooped), wxT("Shift+Space"),
              WaveTracksExistFlag | AudioIONotBusyFlag,
              WaveTracksExistFlag | AudioIONotBusyFlag);
   c->AddItem(wxT("Pause"), _("&Pause"), FN(OnPause), wxT("P"),
              AlwaysEnabledFlag,
              AlwaysEnabledFlag);
   c->AddItem(wxT("SkipStart"), _("S&kip to Start"), FN(OnSkipStart), wxT("Home"),
              AudioIONotBusyFlag,
              AudioIONotBusyFlag);
   c->AddItem(wxT("SkipEnd"), _("Skip to E&nd"), FN(OnSkipEnd), wxT("End"),
              WaveTracksExistFlag | AudioIONotBusyFlag,
              WaveTracksExistFlag | AudioIONotBusyFlag);

   c->AddSeparator();

   /* i18n-hint: (verb)*/
   c->AddItem(wxT("Record"), _("&Record"), FN(OnRecord), wxT("R"));
   c->AddItem(wxT("TimerRecord"), _("&Timer Record..."), FN(OnTimerRecord), wxT("Shift+T"));
   c->AddItem(wxT("RecordAppend"), _("Appen&d Record"), FN(OnRecordAppend), wxT("Shift+R"));

   c->AddSeparator();

   c->AddCheck(wxT("Duplex"), _("&Overdub (on/off)"), FN(OnTogglePlayRecording), 0);
   c->AddCheck(wxT("SWPlaythrough"), _("So&ftware Playthrough (on/off)"), FN(OnToggleSWPlaythrough), 0);

   // Sound Activated recording options
   c->AddCheck(wxT("SoundActivation"), _("Sound A&ctivated Recording (on/off)"), FN(OnToggleSoundActivated), 0);
   c->AddItem(wxT("SoundActivationLevel"), _("Sound Activation Le&vel..."), FN(OnSoundActivated));

#ifdef AUTOMATED_INPUT_LEVEL_ADJUSTMENT
   c->AddCheck(wxT("AutomatedInputLevelAdjustmentOnOff"), _("A&utomated Input Level Adjustment (on/off)"), FN(OnToogleAutomatedInputLevelAdjustment), 0);
#endif
   c->AddItem(wxT("RescanDevices"), _("R&escan Audio Devices"), FN(OnRescanDevices));

   //////////////////////////////////////////////////////////////////////////
   // Tracks Menu (formerly Project Menu)
   //////////////////////////////////////////////////////////////////////////

   c->BeginMenu(_("&Tracks"));
   c->SetDefaultFlags(AudioIONotBusyFlag, AudioIONotBusyFlag);

   //////////////////////////////////////////////////////////////////////////

   c->BeginSubMenu(_("Add &New"));

   c->AddItem(wxT("NewAudioTrack"),  _("&Audio Track"), FN(OnNewWaveTrack), wxT("Ctrl+Shift+N"));
   c->AddItem(wxT("NewStereoTrack"), _("&Stereo Track"), FN(OnNewStereoTrack));
   c->AddItem(wxT("NewLabelTrack"),  _("&Label Track"), FN(OnNewLabelTrack));
   c->AddItem(wxT("NewTimeTrack"),   _("&Time Track"), FN(OnNewTimeTrack));

   c->EndSubMenu();

   //////////////////////////////////////////////////////////////////////////

   c->AddSeparator();

   c->AddItem(wxT("Stereo to Mono"), _("Stereo Trac&k to Mono"), FN(OnStereoToMono),
              AudioIONotBusyFlag | StereoRequiredFlag | WaveTracksSelectedFlag,
              AudioIONotBusyFlag | StereoRequiredFlag | WaveTracksSelectedFlag);
   c->AddItem(wxT("MixAndRender"), _("Mi&x and Render"), FN(OnMixAndRender),
              AudioIONotBusyFlag | WaveTracksSelectedFlag,
              AudioIONotBusyFlag | WaveTracksSelectedFlag);
   c->AddItem(wxT("MixAndRenderToNewTrack"), _("Mix and Render to New Track"), FN(OnMixAndRenderToNewTrack), wxT("Ctrl+Shift+M"),
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
              0, AudioIONotBusyFlag);
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

   //////////////////////////////////////////////////////////////////////////
   // Generate Menu
   //////////////////////////////////////////////////////////////////////////

   c->BeginMenu(_("&Generate"));
   c->SetDefaultFlags(AudioIONotBusyFlag, AudioIONotBusyFlag);

#ifndef EFFECT_CATEGORIES

   effects = em.GetEffects(INSERT_EFFECT | BUILTIN_EFFECT);
   if (effects->GetCount()) {
      names.Clear();
      for (size_t i = 0; i < effects->GetCount(); i++) {
         names.Add((*effects)[i]->GetEffectName());
      }
      c->AddItemList(wxT("Generate"), names, FN(OnGenerateEffect));
   }
   delete effects;

   effects = em.GetEffects(INSERT_EFFECT | PLUGIN_EFFECT);
   if (effects->GetCount()) {
      c->AddSeparator();
      names.Clear();
      for (size_t i = 0; i < effects->GetCount(); i++) {
         names.Add((*effects)[i]->GetEffectName());
      }
      c->AddItemList(wxT("GeneratePlugin"), names, FN(OnGeneratePlugin), true);
   }
   delete effects;

#else

   int flags;

   flags = INSERT_EFFECT | BUILTIN_EFFECT | PLUGIN_EFFECT;
   EffectCategory* ac =
      em.LookupCategory(wxT("http://lv2plug.in/ns/lv2core#GeneratorPlugin"));
   CategorySet roots = ac->GetSubCategories();
   EffectSet generators = ac->GetEffects();
   EffectSet topLevel = CreateEffectSubmenus(c, roots, flags, 0);
   std::copy(generators.begin(), generators.end(),
             std::insert_iterator<EffectSet>(topLevel, topLevel.begin()));
   AddEffectsToMenu(c, topLevel);

   // Add all uncategorised effects in a special submenu
   EffectSet unsorted =
      em.GetUnsortedEffects(flags);
   if (unsorted.size() > 0) {
      c->AddSeparator();
      c->BeginSubMenu(_("Unsorted"));
      names.Clear();
      indices.Clear();
      EffectSet::const_iterator iter;
      for (iter = unsorted.begin(); iter != unsorted.end(); ++iter) {
         names.Add((*iter)->GetEffectName());
         indices.Add((*iter)->GetID());
      }
      c->AddItemList(wxT("Generate"), names,
                     FNI(OnProcessAny, indices), true);
      c->EndSubMenu();
   }

#endif // EFFECT_CATEGORIES

   c->EndMenu();

   /////////////////////////////////////////////////////////////////////////////
   // Effect Menu
   /////////////////////////////////////////////////////////////////////////////

   c->BeginMenu(_("Effe&ct"));
   c->SetDefaultFlags(AudioIONotBusyFlag | TimeSelectedFlag | WaveTracksSelectedFlag,
                      AudioIONotBusyFlag | TimeSelectedFlag | WaveTracksSelectedFlag);

   wxString buildMenuLabel;
   if (mLastEffectType != 0) {
      buildMenuLabel.Printf(_("Repeat %s"), mLastEffect->GetEffectName().c_str());
   }
   else
      buildMenuLabel.Printf(_("Repeat Last Effect"));

   c->AddItem(wxT("RepeatLastEffect"), buildMenuLabel, FN(OnRepeatLastEffect), wxT("Ctrl+R"),
              AudioIONotBusyFlag | TimeSelectedFlag | WaveTracksSelectedFlag | HasLastEffectFlag,
              AudioIONotBusyFlag | TimeSelectedFlag | WaveTracksSelectedFlag | HasLastEffectFlag);

   c->AddSeparator();

   int additionalEffects = ADVANCED_EFFECT;

   // this is really ugly but we need to keep all the old code to get any
   // effects at all in the menu when EFFECT_CATEGORIES is undefined

#ifndef EFFECT_CATEGORIES

   effects = em.GetEffects(PROCESS_EFFECT | BUILTIN_EFFECT | additionalEffects);
   if (effects->GetCount()) {
      names.Clear();
      for (size_t i = 0; i < effects->GetCount(); i++) {
         names.Add((*effects)[i]->GetEffectName());
      }
      c->AddItemList(wxT("Effect"), names, FN(OnProcessEffect));
   }
   delete effects;

   effects = em.GetEffects(PROCESS_EFFECT | PLUGIN_EFFECT);
   if (effects->GetCount()) {
      c->AddSeparator();
      names.Clear();
      for (size_t i = 0; i < effects->GetCount(); i++) {
         names.Add((*effects)[i]->GetEffectName());
      }
      c->AddItemList(wxT("EffectPlugin"), names, FN(OnProcessPlugin), true);
   }
   delete effects;

   c->EndMenu();

#else
   int flags = PROCESS_EFFECT | BUILTIN_EFFECT | PLUGIN_EFFECT | ADVANCED_EFFECT;
   // The categories form a DAG, so we start at the roots (the categories
   // without incoming links)
   CategorySet roots = em.GetRootCategories();
   EffectSet topLevel = CreateEffectSubmenus(c, roots, flags, 0);
   AddEffectsToMenu(c, topLevel);

   // Add all uncategorised effects in a special submenu
   EffectSet unsorted =
      em.GetUnsortedEffects(flags);
   if (unsorted.size() > 0) {
      c->AddSeparator();
      c->BeginSubMenu(_("Unsorted"));
      names.Clear();
      indices.Clear();
      EffectSet::const_iterator iter;
      for (iter = unsorted.begin(); iter != unsorted.end(); ++iter) {
         names.Add((*iter)->GetEffectName());
         indices.Add((*iter)->GetID());
      }
      c->AddItemList(wxT("Effect"), names, FNI(OnProcessAny, indices), true);
      c->EndSubMenu();
   }
   c->EndMenu();

#endif

   //////////////////////////////////////////////////////////////////////////
   // Analyze Menu
   //////////////////////////////////////////////////////////////////////////

   c->BeginMenu(_("&Analyze"));

   c->AddItem(wxT("ContrastAnalyser"), _("Contrast..."), FN(OnContrast), wxT("Ctrl+Shift+T"),
              AudioIONotBusyFlag | WaveTracksSelectedFlag | TimeSelectedFlag,
              AudioIONotBusyFlag | WaveTracksSelectedFlag | TimeSelectedFlag);
   c->AddItem(wxT("PlotSpectrum"), _("Plot Spectrum..."), FN(OnPlotSpectrum),
              AudioIONotBusyFlag | WaveTracksSelectedFlag | TimeSelectedFlag,
              AudioIONotBusyFlag | WaveTracksSelectedFlag | TimeSelectedFlag);

#ifndef EFFECT_CATEGORIES

   effects = em.GetEffects(ANALYZE_EFFECT | BUILTIN_EFFECT);
   if (effects->GetCount()) {
      names.Clear();
      for (size_t i = 0; i < effects->GetCount(); i++) {
         names.Add((*effects)[i]->GetEffectName());
      }
      c->AddItemList(wxT("Analyze"), names, FN(OnAnalyzeEffect));
   }
   delete effects;

   effects = em.GetEffects(ANALYZE_EFFECT | PLUGIN_EFFECT);
   if (effects->GetCount()) {
      c->AddSeparator();
      names.Clear();
      for (size_t i = 0; i < effects->GetCount(); i++) {
         names.Add((*effects)[i]->GetEffectName());
      }
      c->AddItemList(wxT("AnalyzePlugin"), names, FN(OnAnalyzePlugin), true);
   }
   delete effects;

#else

   flags = ANALYZE_EFFECT | BUILTIN_EFFECT | PLUGIN_EFFECT;
   EffectCategory* ac =
      em.LookupCategory(wxT("http://lv2plug.in/ns/lv2core#AnalyserPlugin"));
   CategorySet roots = ac->GetSubCategories();
   EffectSet analyzers = ac->GetEffects();
   EffectSet topLevel = CreateEffectSubmenus(c, roots, flags, 0);
   std::copy(analyzers.begin(), analyzers.end(),
             std::insert_iterator<EffectSet>(topLevel, topLevel.begin()));
   AddEffectsToMenu(c, topLevel);

   // Add all uncategorised effects in a special submenu
   EffectSet unsorted =
      em.GetUnsortedEffects(flags);
   if (unsorted.size() > 0) {
      c->AddSeparator();
      c->BeginSubMenu(_("Unsorted"));
      names.Clear();
      indices.Clear();
      EffectSet::const_iterator iter;
      for (iter = unsorted.begin(); iter != unsorted.end(); ++iter) {
         names.Add((*iter)->GetEffectName());
         indices.Add((*iter)->GetID());
      }
      c->AddItemList(wxT("Analyze"), names,
                     FNI(OnProcessAny, indices), true);
      c->EndSubMenu();
   }

#endif // EFFECT_CATEGORIES

   c->EndMenu();

   /////////////////////////////////////////////////////////////////////////////
   // Help Menu
   /////////////////////////////////////////////////////////////////////////////

   #ifdef __WXMAC__
   wxGetApp().s_macHelpMenuTitleName = _("&Help");
   #endif

   c->BeginMenu(_("&Help"));
   c->SetDefaultFlags(AlwaysEnabledFlag, AlwaysEnabledFlag);

   c->AddItem(wxT("About"), _("&About Audacity..."), FN(OnAbout));

   c->AddItem(wxT("QuickHelp"), _("&Quick Help (in web browser)"), FN(OnQuickHelp));
   c->AddItem(wxT("Manual"), _("&Manual (in web browser)"), FN(OnManual));

   c->AddSeparator();

   c->AddItem(wxT("Screenshot"), _("&Screenshot Tools..."), FN(OnScreenshot));

#if IS_ALPHA
   // TODO: What should we do here?  Make benchmark a plug-in?
   // Easy enough to do.  We'd call it mod-self-test.
   c->AddSeparator();

   c->AddItem(wxT("Benchmark"), _("&Run Benchmark..."), FN(OnBenchmark));
#endif

   c->AddSeparator();

   c->AddItem(wxT("DeviceInfo"), _("Au&dio Device Info..."), FN(OnAudioDeviceInfo));
   c->AddItem(wxT("Log"), _("Show &Log..."), FN(OnShowLog));

   c->EndMenu();

   /////////////////////////////////////////////////////////////////////////////

   SetMenuBar(menubar);

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
   c->AddCommand(wxT("PlayOneSec"), _("Play One Second"), FN(OnPlayOneSecond), wxT("1"));
   c->AddCommand(wxT("PlayToSelection"),_("Play To Selection"), FN(OnPlayToSelection), wxT("B"));
   c->AddCommand(wxT("PlayCutPreview"), _("Play Cut Preview"), FN(OnPlayCutPreview), wxT("C"));

   c->AddCommand(wxT("SelStart"), _("Selection to Start"), FN(OnSelToStart), wxT("Shift+Home"));
   c->AddCommand(wxT("SelEnd"), _("Selection to End"), FN(OnSelToEnd), wxT("Shift+End"));

   c->AddCommand(wxT("DeleteKey"), _("DeleteKey"), FN(OnDelete), wxT("Backspace"),
                 AudioIONotBusyFlag | TracksSelectedFlag | TimeSelectedFlag,
                 AudioIONotBusyFlag | TracksSelectedFlag | TimeSelectedFlag);

   c->AddCommand(wxT("DeleteKey2"), _("DeleteKey2"), FN(OnDelete), wxT("Delete"),
                 AudioIONotBusyFlag | TracksSelectedFlag | TimeSelectedFlag,
                 AudioIONotBusyFlag | TracksSelectedFlag | TimeSelectedFlag);

   c->SetDefaultFlags(AudioIOBusyFlag, AudioIOBusyFlag);

   c->AddCommand(wxT("SeekLeftShort"), _("Short seek left during playback"), FN(OnSeekLeftShort), wxT("Left\tallowdup"));
   c->AddCommand(wxT("SeekRightShort"),_("Short seek right during playback"), FN(OnSeekRightShort), wxT("Right\tallowdup"));
   c->AddCommand(wxT("SeekLeftLong"), _("Long seek left during playback"), FN(OnSeekLeftLong), wxT("Shift+Left\tallowdup"));
   c->AddCommand(wxT("SeekRightLong"), _("Long Seek right during playback"), FN(OnSeekRightLong), wxT("Shift+Right\tallowdup"));

   c->SetDefaultFlags(TracksExistFlag | TrackPanelHasFocus,
                      TracksExistFlag | TrackPanelHasFocus);

   c->AddCommand(wxT("PrevTrack"), _("Move Focus to Previous Track"), FN(OnCursorUp), wxT("Up"));
   c->AddCommand(wxT("ShiftUp"), _("Move Focus to Previous and Select"), FN(OnShiftUp), wxT("Shift+Up"));
   c->AddCommand(wxT("NextTrack"), _("Move Focus to Next Track"), FN(OnCursorDown), wxT("Down"));
   c->AddCommand(wxT("ShiftDown"), _("Move Focus to Next and Select"), FN(OnShiftDown), wxT("Shift+Down"));
   c->AddCommand(wxT("Toggle"), _("Toggle Focused Track"), FN(OnToggle), wxT("Return"));
   c->AddCommand(wxT("ToggleAlt"), _("Toggle Focused Track"), FN(OnToggle), wxT("NUMPAD_ENTER"));

   c->AddCommand(wxT("CursorLeft"), _("Cursor Left"), FN(OnCursorLeft), wxT("Left\twantevent\tallowdup"));
   c->AddCommand(wxT("CursorRight"), _("Cursor Right"), FN(OnCursorRight), wxT("Right\twantevent\tallowdup"));
   c->AddCommand(wxT("CursorShortJumpLeft"), _("Cursor Short Jump Left"), FN(OnCursorShortJumpLeft), wxT(","));
   c->AddCommand(wxT("CursorShortJumpRight"), _("Cursor Short Jump Right"), FN(OnCursorShortJumpRight), wxT("."));
   c->AddCommand(wxT("CursorLongJumpLeft"), _("Cursor Long Jump Left"), FN(OnCursorLongJumpLeft), wxT("Shift+,"));
   c->AddCommand(wxT("CursorLongJumpRight"), _("Cursor Long Jump Right"), FN(OnCursorLongJumpRight), wxT("Shift+."));

   c->AddCommand(wxT("SelExtLeft"), _("Selection Extend Left"), FN(OnSelExtendLeft), wxT("Shift+Left\twantevent\tallowdup"));
   c->AddCommand(wxT("SelExtRight"), _("Selection Extend Right"), FN(OnSelExtendRight), wxT("Shift+Right\twantevent\tallowdup"));

   c->AddCommand(wxT("SelSetExtLeft"), _("Set (or Extend) Left Selection"), FN(OnSelSetExtendLeft));
   c->AddCommand(wxT("SelSetExtRight"), _("Set (or Extend) Right Selection"), FN(OnSelSetExtendRight));

   c->AddCommand(wxT("SelCntrLeft"), _("Selection Contract Left"), FN(OnSelContractLeft), wxT("Ctrl+Shift+Right\twantevent"));
   c->AddCommand(wxT("SelCntrRight"), _("Selection Contract Right"), FN(OnSelContractRight), wxT("Ctrl+Shift+Left\twantevent"));

   c->AddCommand(wxT("TrackPan"), _("Change pan on focused track"), FN(OnTrackPan), wxT("Shift+P"));
   c->AddCommand(wxT("TrackPanLeft"), _("Pan left on focused track"), FN(OnTrackPanLeft), wxT("Alt+Shift+Left"));
   c->AddCommand(wxT("TrackPanRight"), _("Pan right on focused track"), FN(OnTrackPanRight), wxT("Alt+Shift+Right"));
   c->AddCommand(wxT("TrackGain"), _("Change gain on focused track"), FN(OnTrackGain), wxT("Shift+G"));
   c->AddCommand(wxT("TrackGainInc"), _("Increase gain on focused track"), FN(OnTrackGainInc), wxT("Alt+Shift+Up"));
   c->AddCommand(wxT("TrackGainDec"), _("Decrease gain on focused track"), FN(OnTrackGainDec), wxT("Alt+Shift+Down"));
   c->AddCommand(wxT("TrackMenu"), _("Open menu on focused track"), FN(OnTrackMenu), wxT("Shift+M"));
   c->AddCommand(wxT("TrackMute"), _("Mute/Unmute focused track"), FN(OnTrackMute), wxT("Shift+U"));
   c->AddCommand(wxT("TrackSolo"), _("Solo/Unsolo focused track"), FN(OnTrackSolo), wxT("Shift+S"));
   c->AddCommand(wxT("TrackClose"), _("Close focused track"), FN(OnTrackClose), wxT("Shift+C"));

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

   c->AddCommand(wxT("InputDevice"), _("Change input device"), FN(OnInputDevice), wxT("Shift+I"),
                 AudioIONotBusyFlag,
                 AudioIONotBusyFlag);
   c->AddCommand(wxT("OutputDevice"), _("Change output device"), FN(OnOutputDevice), wxT("Shift+O"),
                 AudioIONotBusyFlag,
                 AudioIONotBusyFlag);
   c->AddCommand(wxT("AudioHost"), _("Change audio host"), FN(OnAudioHost), wxT("Shift+H"),
                 AudioIONotBusyFlag,
                 AudioIONotBusyFlag);
   c->AddCommand(wxT("InputChannels"), _("Change input channels"), FN(OnInputChannels), wxT("Shift+N"),
                 AudioIONotBusyFlag,
                 AudioIONotBusyFlag);

   c->AddCommand(wxT("OutputGain"), _("Adjust output gain"), FN(OnOutputGain));
   c->AddCommand(wxT("OutputGainInc"), _("Increase output gain"), FN(OnOutputGainInc));
   c->AddCommand(wxT("OutputGainDec"), _("Decrease output gain"), FN(OnOutputGainDec));
   c->AddCommand(wxT("InputGain"), _("Adjust input gain"), FN(OnInputGain));
   c->AddCommand(wxT("InputGainInc"), _("Increase input gain"), FN(OnInputGainInc));
   c->AddCommand(wxT("InputGainDec"), _("Decrease input gain"), FN(OnInputGainDec));

   c->AddCommand(wxT("PlayAtSpeed"), _("Play at speed"), FN(OnPlayAtSpeed));
   c->AddCommand(wxT("SetPlaySpeed"), _("Adjust playback speed"), FN(OnSetPlaySpeed));
   c->AddCommand(wxT("PlaySpeedInc"), _("Increase playback speed"), FN(OnPlaySpeedInc));
   c->AddCommand(wxT("PlaySpeedDec"), _("Decrease playback speed"), FN(OnPlaySpeedDec));

   mLastFlags = 0;

   mSel0save = 0;
   mSel1save = 0;

#if defined(__WXDEBUG__)
//   c->CheckDups();
#endif
}

#ifdef EFFECT_CATEGORIES

EffectSet AudacityProject::CreateEffectSubmenus(CommandManager* c,
                                                const CategorySet& categories,
                                                int flags,
                                                unsigned submenuThreshold) {
   EffectSet topLevel;

   CategorySet::const_iterator iter;
   for (iter = categories.begin(); iter != categories.end(); ++iter) {

      EffectSet effects = (*iter)->GetAllEffects(flags);

      // If the subgraph for this category only contains a single effect,
      // add it directly in this menu
      if (effects.size() <= submenuThreshold)
         topLevel.insert(effects.begin(), effects.end());

      // If there are more than one effect, add a submenu for the category
      else if (effects.size() > 0) {

         EffectSet directEffects = (*iter)->GetEffects(flags);
         CategorySet subCategories = (*iter)->GetSubCategories();
         CategorySet nonEmptySubCategories;

         CategorySet::const_iterator sci;
         for (sci = subCategories.begin(); sci != subCategories.end(); ++sci) {
            if ((*sci)->GetAllEffects(flags).size() > 0)
               nonEmptySubCategories.insert(*sci);
         }

         // If there are no direct effects and only one subcategory,
         // add the contents of that subcategory directly in this menu.
         if (directEffects.size() == 0 && nonEmptySubCategories.size() == 1) {
            EffectSet a = CreateEffectSubmenus(c, nonEmptySubCategories,
                                               flags);
            topLevel.insert(a.begin(), a.end());
         }

         // Else, add submenus for the subcategories
         else {
            c->BeginSubMenu((*iter)->GetName());
            EffectSet a = CreateEffectSubmenus(c, subCategories, flags);
            a.insert(directEffects.begin(), directEffects.end());
            AddEffectsToMenu(c, a);
            c->EndSubMenu();
         }
      }
   }

   return topLevel;
}

void AudacityProject::AddEffectsToMenu(CommandManager* c,
                                       const EffectSet& effects) {
   wxArrayString names;
   wxArrayInt indices;
   EffectSet::const_iterator iter;
   for (iter = effects.begin(); iter != effects.end(); ++iter) {
      names.Add((*iter)->GetEffectName());
      indices.Add((*iter)->GetID());
   }
   c->AddItemList(wxT("Effects"), names, FNI(OnProcessAny, indices), true);
}

#endif

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
   int cur = mUndoManager.GetCurrentState();

   if (mUndoManager.UndoAvailable()) {
      mUndoManager.GetShortDescription(cur, &desc);
      mCommandManager.Modify(wxT("Undo"),
                             wxString::Format(_("&Undo %s"),
                                              desc.c_str()));
      // LL:  Hackage Alert!!!
      //
      // On the Mac, all menu state changes are ignored if a modal
      // dialog is displayed.
      //
      // An example of this is when applying chains where the "Undo"
      // menu state should change when each command executes.  But,
      // since the state changes are ignored, the "Undo" menu item
      // will never get enabled.  And unfortunately, this will cause
      // the menu item to be permanently disabled since the recorded
      // state is enabled (even though it isn't) causing the routines
      // to ignore the new enable request.
      //
      // So, the workaround is to transition the item back to disabled
      // and then to enabled.  (Sorry, I couldn't find a better way of
      // doing it.)
      //
      // See src/mac/carbon/menuitem.cpp, wxMenuItem::Enable() for more
      // info.
      mCommandManager.Enable(wxT("Undo"), false);
      mCommandManager.Enable(wxT("Undo"), true);
   }
   else {
      mCommandManager.Modify(wxT("Undo"),
                             wxString::Format(_("&Undo")));
      // LL: See above
      mCommandManager.Enable(wxT("Undo"), true);
      mCommandManager.Enable(wxT("Undo"), false);
   }

   if (mUndoManager.RedoAvailable()) {
      mUndoManager.GetShortDescription(cur+1, &desc);
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
   wxMenuBar *menuBar = GetMenuBar();
   DetachMenuBar();
   delete menuBar;

   mCommandManager.PurgeData();

   CreateMenusAndCommands();

   ModuleManager::Dispatch(MenusRebuilt);
}

void AudacityProject::RebuildOtherMenus()
{
   if (mTrackPanel) {
      mTrackPanel->BuildMenus();
   }
}

int AudacityProject::GetFocusedFrame()
{
   wxWindow *w = FindFocus();

   while (w && mToolManager && mTrackPanel) {
      if (w == mToolManager->GetTopDock()) {
         return TopDockHasFocus;
      }

      if (w == mTrackPanel) {
         return TrackPanelHasFocus;
      }

      if (w == mToolManager->GetBotDock()) {
         return BotDockHasFocus;
      }

      w = w->GetParent();
   }

   return 0;
}

wxUint32 AudacityProject::GetUpdateFlags()
{
   // This method determines all of the flags that determine whether
   // certain menu items and commands should be enabled or disabled,
   // and returns them in a bitfield.  Note that if none of the flags
   // have changed, it's not necessary to even check for updates.
   wxUint32 flags = 0;

   if (!gAudioIO->IsAudioTokenActive(GetAudioIOToken()))
      flags |= AudioIONotBusyFlag;
   else
      flags |= AudioIOBusyFlag;

   if (mViewInfo.sel1 > mViewInfo.sel0)
      flags |= TimeSelectedFlag;

   TrackListIterator iter(mTracks);
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
               if (ls->t >= mViewInfo.sel0 && ls->t1 <= mViewInfo.sel1) {
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

   if (mUndoManager.UnsavedChanges())
      flags |= UnsavedChangesFlag;

   if (mLastEffect != NULL)
      flags |= HasLastEffectFlag;

   if (mUndoManager.UndoAvailable())
      flags |= UndoAvailableFlag;

   if (mUndoManager.RedoAvailable())
      flags |= RedoAvailableFlag;

   if (GetZoom() < gMaxZoom && (flags & TracksExistFlag))
      flags |= ZoomInAvailableFlag;

   if (GetZoom() > gMinZoom && (flags & TracksExistFlag))
      flags |= ZoomOutAvailableFlag;

   if ((flags & LabelTracksExistFlag) && LabelTrack::IsTextClipSupported())
      flags |= TextClipFlag;

   flags |= GetFocusedFrame();

   if (IsPlayRegionLocked())
      flags |= PlayRegionLockedFlag;
   else
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

   return flags;
}

void AudacityProject::SelectAllIfNone()
{
   wxUint32 flags = GetUpdateFlags();
   if(((flags & TracksSelectedFlag) ==0) || (mViewInfo.sel0 >= mViewInfo.sel1))
      OnSelectAll();
}

void AudacityProject::ModifyAllProjectToolbarMenus()
{
   AProjectArray::iterator i;
   for (i = gAudacityProjects.begin(); i != gAudacityProjects.end(); ++i) {
      ((AudacityProject *)*i)->ModifyToolbarMenus();
   }
}

void AudacityProject::ModifyToolbarMenus()
{
   // Refreshes can occur during shutdown and the toolmanager may already
   // be deleted, so protect against it.
   if (!mToolManager) {
      return;
   }

   mCommandManager.Check(wxT("ShowTransportTB"),
                         mToolManager->IsVisible(TransportBarID));
   mCommandManager.Check(wxT("ShowDeviceTB"),
                         mToolManager->IsVisible(DeviceBarID));
   mCommandManager.Check(wxT("ShowEditTB"),
                         mToolManager->IsVisible(EditBarID));
   mCommandManager.Check(wxT("ShowMeterTB"),
                         mToolManager->IsVisible(MeterBarID));
   mCommandManager.Check(wxT("ShowMixerTB"),
                         mToolManager->IsVisible(MixerBarID));
   mCommandManager.Check(wxT("ShowSelectionTB"),
                         mToolManager->IsVisible(SelectionBarID));
   mCommandManager.Check(wxT("ShowToolsTB"),
                         mToolManager->IsVisible(ToolsBarID));
   mCommandManager.Check(wxT("ShowTranscriptionTB"),
                         mToolManager->IsVisible(TranscriptionBarID));

   // Now, go through each toolbar, and call EnableDisableButtons()
   for (int i = 0; i < ToolBarCount; i++) {
      mToolManager->GetToolBar(i)->EnableDisableButtons();
   }

   // These don't really belong here, but it's easier and especially so for
   // the Edit toolbar and the sync-lock menu item.
   bool active;
   gPrefs->Read(wxT("/AudioIO/SoundActivatedRecord"),&active, false);
   mCommandManager.Check(wxT("SoundActivation"), active);
#ifdef AUTOMATED_INPUT_LEVEL_ADJUSTMENT
   gPrefs->Read(wxT("/AudioIO/AutomatedInputLevelAdjustment"),&active, false);
   mCommandManager.Check(wxT("AutomatedInputLevelAdjustmentOnOff"), active);
#endif
   gPrefs->Read(wxT("/AudioIO/Duplex"),&active, true);
   mCommandManager.Check(wxT("Duplex"), active);
   gPrefs->Read(wxT("/AudioIO/SWPlaythrough"),&active, false);
   mCommandManager.Check(wxT("SWPlaythrough"), active);
   gPrefs->Read(wxT("/GUI/SyncLockTracks"), &active, false);
   SetSyncLock(active);
   mCommandManager.Check(wxT("SyncLock"), active);
}

void AudacityProject::UpdateMenus()
{
   //ANSWER-ME: Why UpdateMenus only does active project?
   //JKC: Is this test fixing a bug when multiple projects are open?
   //so that menu states work even when different in different projects?
   if (this != GetActiveProject())
      return;

   if (!IsActive())
      return;

   wxUint32 flags = GetUpdateFlags();
   wxUint32 flags2 = flags;

   // We can enable some extra items if we have select-all-on-none.
   //EXPLAIN-ME: Why is this here rather than in GetUpdateFlags()?
   if (mSelectAllOnNone)
   {
      if ((flags & TracksExistFlag) != 0)
      {
         flags2 |= TracksSelectedFlag;
         if ((flags & WaveTracksExistFlag) != 0 )
         {
            flags2 |= TimeSelectedFlag
                   |  WaveTracksSelectedFlag
                   |  CutCopyAvailableFlag;
         }
      }
   }

   // Return from this function if nothing's changed since
   // the last time we were here.
   if (flags == mLastFlags)
      return;
   mLastFlags = flags;

   mCommandManager.EnableUsingFlags(flags2 , 0xFFFFFFFF);

   // With select-all-on-none, some items that we don't want enabled may have
   // been enabled, since we changed the flags.  Here we manually disable them.
   if (mSelectAllOnNone)
   {
      if ((flags & TracksSelectedFlag) == 0)
      {
         mCommandManager.Enable(wxT("SplitCut"), false);

         if ((flags & WaveTracksSelectedFlag) == 0)
         {
            mCommandManager.Enable(wxT("Split"), false);
         }
         if ((flags & TimeSelectedFlag) == 0)
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
bool AudacityProject::MakeReadyToPlay()
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

   toolbar->SetPlay(true);
   toolbar->SetStop(false);

   return true;
}

void AudacityProject::OnPlayOneSecond()
{
   if( !MakeReadyToPlay() )
      return;

   double pos = mTrackPanel->GetMostRecentXPos();
   mLastPlayMode = oneSecondPlay;
   GetControlToolBar()->PlayPlayRegion(pos - 0.5, pos + 0.5);
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
   if (fabs(pos - mViewInfo.sel0) < fabs(pos - mViewInfo.sel1)) {
      t0 = t1 = mViewInfo.sel0;
   } else {
      t0 = t1 = mViewInfo.sel1;
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
   mLastPlayMode = oneSecondPlay;

   // An alternative, commented out below, is to disable autoscroll
   // only when playing a short region, less than or equal to a second.
//   mLastPlayMode = ((t1-t0) > 1.0) ? normalPlay : oneSecondPlay;

   GetControlToolBar()->PlayPlayRegion(t0, t1);
}

void AudacityProject::OnPlayLooped()
{
   if( !MakeReadyToPlay() )
      return;

   // Now play in a loop
   // Will automatically set mLastPlayMode
   GetControlToolBar()->PlayCurrentRegion(true);
}

void AudacityProject::OnPlayCutPreview()
{
   if ( !MakeReadyToPlay() )
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
      //If this project isn't playing, but another one is, stop playing the old and start the new.

      //find out which project we need;
      AudacityProject* otherProject = NULL;
      for(unsigned i=0; i<gAudacityProjects.GetCount(); i++) {
         if(gAudioIO->IsStreamActive(gAudacityProjects[i]->GetAudioIOToken())) {
            otherProject=gAudacityProjects[i];
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
         toolbar->SetPlay(true);
         toolbar->SetStop(false);

         // Will automatically set mLastPlayMode
         toolbar->PlayCurrentRegion(false);
      }
   }
   else if (!gAudioIO->IsBusy()) {
      //Otherwise, start playing (assuming audio I/O isn't busy)
      toolbar->SetPlay(true);
      toolbar->SetStop(false);

      // Will automatically set mLastPlayMode
      toolbar->PlayCurrentRegion(false);
   }
}

void AudacityProject::OnStop()
{
   wxCommandEvent evt;

   if (gAudioIO->IsStreamActive())
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
   wxCommandEvent evt;
   ControlToolBar *toolbar = GetControlToolBar();

   //If busy, stop playing, make sure everything is unpaused.
   if (gAudioIO->IsStreamActive(GetAudioIOToken())) {
      toolbar->SetPlay(false);        //Pops
      toolbar->SetStop(true);         //Pushes stop down
      mViewInfo.sel0 = gAudioIO->GetStreamTime();
      if( mViewInfo.sel1 < mViewInfo.sel0 ) {
         mViewInfo.sel1 = mViewInfo.sel0;
      }
      ModifyState(false);           // without bWantsAutoSave
      toolbar->OnStop(evt);
   }
   else if (!gAudioIO->IsBusy()) {
      //Otherwise, start playing (assuming audio I/O isn't busy)
      toolbar->SetPlay(true);
      toolbar->SetStop(false);

      // Will automatically set mLastPlayMode
      toolbar->PlayCurrentRegion(false);
   }
}

void AudacityProject::OnStopSelect()
{
   wxCommandEvent evt;

   if (gAudioIO->IsStreamActive()) {
      mViewInfo.sel0 = gAudioIO->GetStreamTime();
      if( mViewInfo.sel1 < mViewInfo.sel0 ) {
         mViewInfo.sel1 = mViewInfo.sel0;
      }
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

void AudacityProject::OnTogglePlayRecording()
{
   bool Duplex;
   gPrefs->Read(wxT("/AudioIO/Duplex"), &Duplex, false);
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

#ifdef AUTOMATED_INPUT_LEVEL_ADJUSTMENT
void AudacityProject::OnToogleAutomatedInputLevelAdjustment()
{
   bool AVEnabled;
   gPrefs->Read(wxT("/AudioIO/AutomatedInputLevelAdjustment"), &AVEnabled, false);
   gPrefs->Write(wxT("/AudioIO/AutomatedInputLevelAdjustment"), !AVEnabled);
   gPrefs->Flush();
   ModifyAllProjectToolbarMenus();
}
#endif

double AudacityProject::GetTime(Track *t)
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
   int ndx = 0;
   int cmpValue;
   wxArrayPtrVoid arr;
   TrackListIterator iter(mTracks);
   Track *track = iter.First();
   bool lastTrackLinked = false;
   //sort by linked tracks. Assumes linked track follows owner in list.
   while (track) {
      if(lastTrackLinked) {
         //insert after the last track since this track should be linked to it.
         ndx++;
      }
      else {
         for (ndx = 0; ndx < (int)arr.GetCount(); ndx++) {
            if(flags & kAudacitySortByName){
               //do case insensitive sort - cmpNoCase returns less than zero if the string is 'less than' its argument
               //also if we have case insensitive equality, then we need to sort by case as well
               //We sort 'b' before 'B' accordingly.  We uncharacteristically use greater than for the case sensitive
               //compare because 'b' is greater than 'B' in ascii.
               cmpValue = track->GetName().CmpNoCase(((Track *) arr[ndx])->GetName());
               if (cmpValue < 0 ||
                   (0 == cmpValue && track->GetName().CompareTo(((Track *) arr[ndx])->GetName()) > 0) )
                  break;
            }
            //sort by time otherwise
            else if(flags & kAudacitySortByTime){
               //we have to search each track and all its linked ones to fine the minimum start time.
               double time1,time2,tempTime;
               Track* tempTrack;
               int    candidatesLookedAt;

               candidatesLookedAt = 0;
               tempTrack = track;
               time1=time2=std::numeric_limits<double>::max(); //TODO: find max time value. (I don't think we have one yet)
               while(tempTrack){
                  tempTime = GetTime(tempTrack);
                  time1 = time1<tempTime? time1:tempTime;
                  if(tempTrack->GetLinked())
                     tempTrack = tempTrack->GetLink();
                  else
                     tempTrack = NULL;
               }

               //get candidate's (from sorted array) time
               tempTrack = (Track *) arr[ndx];
               while(tempTrack){
                  tempTime = GetTime(tempTrack);
                  time2 = time2<tempTime? time2:tempTime;
                  if(tempTrack->GetLinked() && (ndx+candidatesLookedAt <  (int)arr.GetCount()-1) ) {
                     candidatesLookedAt++;
                     tempTrack = (Track*) arr[ndx+candidatesLookedAt];
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
      arr.Insert(track, ndx);

      lastTrackLinked = track->GetLinked();
      track = iter.RemoveCurrent();
   }

   for (ndx = 0; ndx < (int)arr.GetCount(); ndx++) {
      mTracks->Add((Track *)arr[ndx]);
   }
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
   mTrackPanel->OnCursorLeft( false, false );
}

void AudacityProject::OnSeekRightShort()
{
   mTrackPanel->OnCursorRight( false, false );
}

void AudacityProject::OnSeekLeftLong()
{
   mTrackPanel->OnCursorLeft( true, false );
}

void AudacityProject::OnSeekRightLong()
{
   mTrackPanel->OnCursorRight( true, false );
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

void AudacityProject::OnShiftUp()
{
   mTrackPanel->OnPrevTrack( true );
}

void AudacityProject::OnCursorDown()
{
   mTrackPanel->OnNextTrack( false );
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
   mTrackPanel->OnCursorLeft( false, false, evt->GetEventType() == wxEVT_KEY_UP );
}

void AudacityProject::OnCursorRight(const wxEvent * evt)
{
   mTrackPanel->OnCursorRight( false, false, evt->GetEventType() == wxEVT_KEY_UP );
}

void AudacityProject::OnCursorShortJumpLeft()
{
   mTrackPanel->OnCursorMove( false, true, false );
}

void AudacityProject::OnCursorShortJumpRight()
{
   mTrackPanel->OnCursorMove( true, true, false );
}

void AudacityProject::OnCursorLongJumpLeft()
{
   mTrackPanel->OnCursorMove( false, true, true );
}

void AudacityProject::OnCursorLongJumpRight()
{
   mTrackPanel->OnCursorMove( true, true, true );
}

void AudacityProject::OnSelSetExtendLeft()
{
   mTrackPanel->OnBoundaryMove( true, false);
}

void AudacityProject::OnSelSetExtendRight()
{
   mTrackPanel->OnBoundaryMove( false, false);
}

void AudacityProject::OnSelExtendLeft(const wxEvent * evt)
{
   mTrackPanel->OnCursorLeft( true, false, evt->GetEventType() == wxEVT_KEY_UP );
}

void AudacityProject::OnSelExtendRight(const wxEvent * evt)
{
   mTrackPanel->OnCursorRight( true, false, evt->GetEventType() == wxEVT_KEY_UP );
}

void AudacityProject::OnSelContractLeft(const wxEvent * evt)
{
   mTrackPanel->OnCursorRight( true, true, evt->GetEventType() == wxEVT_KEY_UP );
}

void AudacityProject::OnSelContractRight(const wxEvent * evt)
{
   mTrackPanel->OnCursorLeft( true, true, evt->GetEventType() == wxEVT_KEY_UP );
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
      mViewInfo.sel0 = indicator;
      bSelChanged = true;
   }
   else
   {
      wxString fmt = GetSelectionFormat();
      TimeDialog dlg(this, _("Set Left Selection Boundary"),
         fmt, mRate, mViewInfo.sel0, _("Position"));

      if (wxID_OK == dlg.ShowModal())
      {
         //Get the value from the dialog
         mViewInfo.sel0 = dlg.GetTimeValue();

         //Make sure it is 'legal'
         if (mViewInfo.sel0 < 0.0)
            mViewInfo.sel0 = 0.0;

         bSelChanged = true;
      }
   }

   if (mViewInfo.sel1 < mViewInfo.sel0)
   {
      mViewInfo.sel1 = mViewInfo.sel0;
      bSelChanged = true;
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
      mViewInfo.sel1 = indicator;
      bSelChanged = true;
   }
   else
   {
      wxString fmt = GetSelectionFormat();
      TimeDialog dlg(this, _("Set Right Selection Boundary"),
         fmt, mRate, mViewInfo.sel1, _("Position"));

      if (wxID_OK == dlg.ShowModal())
      {
         //Get the value from the dialog
         mViewInfo.sel1 = dlg.GetTimeValue();

         //Make sure it is 'legal'
         if(mViewInfo.sel1 < 0)
            mViewInfo.sel1 = 0;

         bSelChanged = true;
      }
   }

   if (mViewInfo.sel0 >  mViewInfo.sel1)
   {
      mViewInfo.sel0 = mViewInfo.sel1;
      bSelChanged = true;
   }

   if (bSelChanged)
   {
      ModifyState(false);
      mTrackPanel->Refresh(false);
   }
}

void AudacityProject::NextFrame()
{
   switch( GetFocusedFrame() )
   {
      case TopDockHasFocus:
         mTrackPanel->SetFocus();
      break;

      case TrackPanelHasFocus:
         mToolManager->GetBotDock()->SetFocus();
      break;

      case BotDockHasFocus:
         mToolManager->GetTopDock()->SetFocus();
      break;
   }
}

void AudacityProject::PrevFrame()
{
   switch( GetFocusedFrame() )
   {
      case TopDockHasFocus:
         mToolManager->GetBotDock()->SetFocus();
      break;

      case TrackPanelHasFocus:
         mToolManager->GetTopDock()->SetFocus();
      break;

      case BotDockHasFocus:
         mTrackPanel->SetFocus();
      break;
   }
}

void AudacityProject::OnTrackPan()
{
   mTrackPanel->OnTrackPan();
}

void AudacityProject::OnTrackPanLeft()
{
   mTrackPanel->OnTrackPanLeft();
}

void AudacityProject::OnTrackPanRight()
{
   mTrackPanel->OnTrackPanRight();
}

void AudacityProject::OnTrackGain()
{
   mTrackPanel->OnTrackGain();
}

void AudacityProject::OnTrackGainInc()
{
   mTrackPanel->OnTrackGainInc();
}

void AudacityProject::OnTrackGainDec()
{
   mTrackPanel->OnTrackGainDec();
}

void AudacityProject::OnTrackMenu()
{
   // LLL:  There's a slight problem on Windows that I was not able to track
   //       down to the actual cause.  I "think" it might be a problem in wxWidgets
   //       on Windows, but I'm not sure.
   //
   //       Let's say the user has SHIFT+M assigned as the keyboard shortcut for
   //       bringing up the track menu.  If there is only 1 wave track and the user
   //       uses the shortcut, the menu is display and immediately disappears.  But,
   //       if there are 2 or more wave tracks, then the menu is displayed.
   //
   //       However, what is actually happening is that the popup menu is processing
   //       the "M" as the menu item to select after the menu is displayed.  With only
   //       1 track, the only (enabled) menu item that begins with "M" is Mono and
   //       that's what gets selected.
   //
   //       With 2+ wave tracks, there's 2 menu items that begin with "M" and Mono
   //       is only highlighted by not selected, so the menu doesn't get dismissed.
   //
   //       While the 1 or 2 track example above is a way to recreate the issue, the
   //       real problem is when there's only one enabled menu item that begins with
   //       the selected shortcut key.
   //
   //       The workaround is to queue a context menu event, allowing the key press
   //       event to complete.
   wxContextMenuEvent e(wxEVT_CONTEXT_MENU, GetId());
   mTrackPanel->AddPendingEvent(e);
}

void AudacityProject::OnTrackMute()
{
   mTrackPanel->OnTrackMute(false);
}

void AudacityProject::OnTrackSolo()
{
   mTrackPanel->OnTrackSolo(false);
}

void AudacityProject::OnTrackClose()
{
   mTrackPanel->OnTrackClose();
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
      tb->PlayAtSpeed();
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

   TrackListIterator iter(mTracks);
   Track *track = iter.First();
   while (track) {
      if (!track->GetSelected() || track->GetKind() != (Track::Wave)) {
         track = iter.Next();
         continue;
      }
      WaveTrack *one = (WaveTrack *)track;
      int oneWindowSize = (int)(one->GetRate() / 100);
      float *oneDist = new float[oneWindowSize];
      sampleCount s = one->TimeToLongSamples(t0);
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
   if (mViewInfo.sel0 == mViewInfo.sel1)
      mViewInfo.sel0 = mViewInfo.sel1 =
         NearestZeroCrossing(mViewInfo.sel0);
   else {
      mViewInfo.sel0 = NearestZeroCrossing(mViewInfo.sel0);
      mViewInfo.sel1 = NearestZeroCrossing(mViewInfo.sel1);

      if (mViewInfo.sel1 < mViewInfo.sel0)
         mViewInfo.sel1 = mViewInfo.sel0;
   }

   ModifyState(false);

   mTrackPanel->Refresh(false);
}

//
// Effect Menus
//

void AudacityProject::OnEffect(int type, int index)
{
   EffectArray *effects;
   Effect *f = NULL;

   effects = EffectManager::Get().GetEffects(type);

   f = (*effects)[index];
   delete effects;

   if (!f)
      return;
   //TIDY-ME: Effect Type parameters serve double duty.
   // The type parameter is over used.
   // It is being used:
   //  (a) to filter the list of effects
   //  (b) to specify whether to prompt for parameters.
   OnEffect( type, f );
}

/// OnEffect() takes an Effect and executes it.
///
/// At the moment flags are used only to indicate
/// whether to prompt for parameters or whether to
/// use the most recently stored values.
///
/// At some point we should change to specifying a
/// parameter source - one of:
///   + Prompt
///   + Use previous values
///   + Parse from a string that is passed in
///
/// DanH: I've added the third option as a temporary measure. I think this
///       should eventually be done by having effects as Command objects.
bool AudacityProject::OnEffect(int type,
                               Effect * f,
                               wxString params,
                               bool saveState)
{
   wxGetApp().SetMissingAliasedFileWarningShouldShow(true);

   TrackListIterator iter(mTracks);
   Track *t = iter.First();
   WaveTrack *newTrack = NULL;
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
      if (f->GetEffectFlags() & INSERT_EFFECT) {
         // Create a new track for the generated audio...
         newTrack = mTrackFactory->NewWaveTrack();
         mTracks->Add(newTrack);
         newTrack->SetSelected(true);
      }
      else {
         wxMessageBox(_("You must select a track first."));
         return false;
      }
   }

   if (f->DoEffect(this, type, mRate, mTracks, mTrackFactory,
                   &mViewInfo.sel0, &mViewInfo.sel1, params)) {
      if (saveState)
      {
         wxString longDesc = f->GetEffectDescription();
         wxString shortDesc = f->GetEffectName();

         if (shortDesc.Length() > 3 && shortDesc.Right(3)==wxT("..."))
            shortDesc = shortDesc.Left(shortDesc.Length()-3);

         PushState(longDesc, shortDesc);

         // Only remember a successful effect, don't rmemeber insert,
         // or analyze effects.
         if ((f->GetEffectFlags() & (INSERT_EFFECT | ANALYZE_EFFECT))==0) {
            mLastEffect = f;
            mLastEffectType = type;
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
      if ( f->GetEffectFlags() & INSERT_EFFECT)
      {
            if (count == 0 || (clean && mViewInfo.sel0 == 0.0)) OnZoomFit();
          //  mTrackPanel->Refresh(false);
      }
      RedrawProject();
      if (focus != NULL) {
         focus->SetFocus();
      }
      mTrackPanel->EnsureVisible(mTrackPanel->GetFirstSelectedTrack());

      mTrackPanel->Refresh(false);
   } else {
      if (newTrack) {
         mTracks->Remove(newTrack);
         delete newTrack;
         mTrackPanel->Refresh(false);
      }
      return false;
   }
   return true;
}

void AudacityProject::OnGenerateEffect(int index)
{
   OnEffect(BUILTIN_EFFECT | INSERT_EFFECT, index);
}

void AudacityProject::OnGeneratePlugin(int index)
{
   OnEffect(PLUGIN_EFFECT | INSERT_EFFECT, index);
}

void AudacityProject::OnRepeatLastEffect(int WXUNUSED(index))
{
   if (mLastEffect != NULL) {
      // Setting the CONFIGURED_EFFECT bit prevents
      // prompting for parameters.
      OnEffect(mLastEffectType | CONFIGURED_EFFECT, mLastEffect);
   }
}

void AudacityProject::OnProcessAny(int index)
{
   Effect* e = EffectManager::Get().GetEffect(index);
   OnEffect(ALL_EFFECTS, e);
}

void AudacityProject::OnProcessEffect(int index)
{
   int additionalEffects=ADVANCED_EFFECT;
   OnEffect(BUILTIN_EFFECT | PROCESS_EFFECT | additionalEffects, index);
}

void AudacityProject::OnStereoToMono(int WXUNUSED(index))
{
   OnEffect(ALL_EFFECTS,
            EffectManager::Get().GetEffectByIdentifier(wxT("StereoToMono")));
}

void AudacityProject::OnProcessPlugin(int index)
{
   OnEffect(PLUGIN_EFFECT | PROCESS_EFFECT, index);
}

void AudacityProject::OnAnalyzeEffect(int index)
{
   OnEffect(BUILTIN_EFFECT | ANALYZE_EFFECT, index);
}

void AudacityProject::OnAnalyzePlugin(int index)
{
   OnEffect(PLUGIN_EFFECT | ANALYZE_EFFECT, index);
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

   TrackListIterator iter(mTracks);

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
                        NULL,
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
   TrackListIterator iter(mTracks);
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
         NULL,
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
   e.Process(this, true, mViewInfo.sel0, mViewInfo.sel1);
}

void AudacityProject::OnExportMultiple()
{
   ExportMultiple em(this);

   wxGetApp().SetMissingAliasedFileWarningShouldShow(true);
   em.ShowModal();
}

void AudacityProject::OnPreferences()
{
   PrefsDialog dialog(this /* parent */ );

   if (!dialog.ShowModal()) {
      // Canceled
      return;
   }

   // LL:  Moved from PrefsDialog since wxWidgets on OSX can't deal with
   //      rebuilding the menus while the PrefsDialog is still in the modal
   //      state.
   for (size_t i = 0; i < gAudacityProjects.GetCount(); i++) {
      AudacityProject *p = gAudacityProjects[i];

      p->RebuildMenuBar();
      p->RebuildOtherMenus();
#if defined(__WXGTK__)
      // Workaround for:
      //
      //   http://bugzilla.audacityteam.org/show_bug.cgi?id=458
      //
      // This should be removed with wxWidgets 2.8.13 is released.
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
   HandlePrint(this, GetName(), mTracks);
}

//
// Edit Menu
//

void AudacityProject::OnUndo()
{
   if (!mUndoManager.UndoAvailable()) {
      wxMessageBox(_("Nothing to undo"));
      return;
   }

   // can't undo while dragging
   if (mTrackPanel->IsMouseCaptured()) {
      return;
   }

   TrackList *l = mUndoManager.Undo(&mViewInfo.sel0, &mViewInfo.sel1);
   PopState(l);

   mTrackPanel->SetFocusedTrack(NULL);
   mTrackPanel->EnsureVisible(mTrackPanel->GetFirstSelectedTrack());

   RedrawProject();

   if (mHistoryWindow)
      mHistoryWindow->UpdateDisplay();

   ModifyUndoMenuItems();
}

void AudacityProject::OnRedo()
{
   if (!mUndoManager.RedoAvailable()) {
      wxMessageBox(_("Nothing to redo"));
      return;
   }

   TrackList *l = mUndoManager.Redo(&mViewInfo.sel0, &mViewInfo.sel1);
   PopState(l);

   mTrackPanel->SetFocusedTrack(NULL);
   mTrackPanel->EnsureVisible(mTrackPanel->GetFirstSelectedTrack());

   RedrawProject();

   if (mHistoryWindow)
      mHistoryWindow->UpdateDisplay();

   ModifyUndoMenuItems();
}

void AudacityProject::OnCut()
{
   TrackListIterator iter(mTracks);
   Track *n = iter.First();
   Track *dest;

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
         dest = NULL;
#if defined(USE_MIDI)
         if (n->GetKind() == Track::Note)
            // Since portsmf has a built-in cut operator, we use that instead
            n->Cut(mViewInfo.sel0, mViewInfo.sel1, &dest);
         else
#endif
            n->Copy(mViewInfo.sel0, mViewInfo.sel1, &dest);

         if (dest) {
            dest->SetChannel(n->GetChannel());
            dest->SetLinked(n->GetLinked());
            dest->SetName(n->GetName());
            msClipboard->Add(dest);
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
                  ((WaveTrack*)n)->ClearAndAddCutLine(mViewInfo.sel0,
                                                      mViewInfo.sel1);
                  break;
               }

               // Fall through

            default:
               n->Clear(mViewInfo.sel0, mViewInfo.sel1);
            break;
         }
      }
      n = iter.Next();
   }

   msClipT0 = mViewInfo.sel0;
   msClipT1 = mViewInfo.sel1;
   msClipProject = this;

   PushState(_("Cut to the clipboard"), _("Cut"));

   RedrawProject();

   mViewInfo.sel1 = mViewInfo.sel0;
}


void AudacityProject::OnSplitCut()
{
   TrackListIterator iter(mTracks);
   Track *n = iter.First();
   Track *dest;

   ClearClipboard();
   n = iter.First();
   while (n) {
      if (n->GetSelected()) {
         dest = NULL;
         if (n->GetKind() == Track::Wave)
         {
            ((WaveTrack*)n)->SplitCut(mViewInfo.sel0, mViewInfo.sel1, &dest);
         } else
         {
            n->Copy(mViewInfo.sel0, mViewInfo.sel1, &dest);
            n->Silence(mViewInfo.sel0, mViewInfo.sel1);
         }
         if (dest) {
            dest->SetChannel(n->GetChannel());
            dest->SetLinked(n->GetLinked());
            dest->SetName(n->GetName());
            msClipboard->Add(dest);
         }
      }
      n = iter.Next();
   }

   msClipT0 = mViewInfo.sel0;
   msClipT1 = mViewInfo.sel1;
   msClipProject = this;

   PushState(_("Split-cut to the clipboard"), _("Split Cut"));

   RedrawProject();
}


void AudacityProject::OnCopy()
{

   TrackListIterator iter(mTracks);

   Track *n = iter.First();
   Track *dest;

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
         dest = NULL;
         n->Copy(mViewInfo.sel0, mViewInfo.sel1, &dest);
         if (dest) {
            dest->SetChannel(n->GetChannel());
            dest->SetLinked(n->GetLinked());
            dest->SetName(n->GetName());
            msClipboard->Add(dest);
         }
      }
      n = iter.Next();
   }

   msClipT0 = mViewInfo.sel0;
   msClipT1 = mViewInfo.sel1;
   msClipProject = this;

   //Make sure the menus/toolbar states get updated
   mTrackPanel->Refresh(false);
}

void AudacityProject::OnPaste()
{
   // Handle text paste (into active label) first.
   if (this->HandlePasteText())
      return;

   // If nothing's selected, we just insert new tracks.
   if (this->HandlePasteNothingSelected())
      return;

   // Otherwise, paste into the selected tracks.
   double t0 = mViewInfo.sel0;
   double t1 = mViewInfo.sel1;

   TrackListIterator iter(mTracks);
   TrackListIterator clipIter(msClipboard);

   Track *n = iter.First();
   Track *c = clipIter.First();
   if (c == NULL)
      return;
   Track *f = NULL;
   Track *tmpSrc = NULL;
   Track *tmpC = NULL;
   Track *prev = NULL;

   bool bAdvanceClipboard = true;
   bool bPastedSomething = false;
   bool bTrackTypeMismatch = false;

   while (n && c) {
      if (n->GetSelected()) {
         bAdvanceClipboard = true;
         if (tmpC) c = tmpC;
         if (c->GetKind() != n->GetKind()){
            if (!bTrackTypeMismatch) {
               tmpSrc = prev;
               tmpC = c;
            }
            bTrackTypeMismatch = true;
            bAdvanceClipboard = false;
            c = tmpSrc;

            // If the types still don't match...
            while (c && c->GetKind() != n->GetKind()){
               prev = c;
               c = clipIter.Next();
            }
         }

         // Handle case where the first track in clipboard
         // is of different type than the first selected track
         if (!c){
            c = tmpC;
            while (n && (c->GetKind() != n->GetKind() || !n->GetSelected()))
            {
               // Must perform sync-lock adjustment before incrementing n
               if (n->IsSyncLockSelected()) {
                  bPastedSomething |= n->SyncLockAdjust(t1, t0+(msClipT1 - msClipT0));
               }
               n = iter.Next();
            }
            if (!n) c = NULL;
         }

         // The last possible case for cross-type pastes: triggered when we try to
         // paste 1+ tracks from one type into 1+ tracks of another type. If
         // there's a mix of types, this shouldn't run.
         if (!c){
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

         if (!f)
            f = n;

         if (msClipProject != this && c->GetKind() == Track::Wave)
            ((WaveTrack *) c)->Lock();

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
               //printf("Checking to see if we need to pre-clear the track\n");
               if (!((WaveTrack *) n)->IsEmpty(t0, t1)) {
                  ((WaveTrack *) n)->Clear(t0, t1);
               }
               bPastedSomething |= ((WaveTrack *)n)->Paste(t0, c);
            }
            else
            {
               n->Clear(t0, t1);
               bPastedSomething |= n->Paste(t0, c);
            }
         }

         if (msClipProject != this && c->GetKind() == Track::Wave)
            ((WaveTrack *) c)->Unlock();

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
      TrackListOfKindIterator clipWaveIter(Track::Wave, msClipboard);
      c = clipWaveIter.Last();

      while (n){
         if (n->GetSelected() && n->GetKind()==Track::Wave){
            if (c && c->GetKind() == Track::Wave){
               bPastedSomething |=
                  ((WaveTrack *)n)->ClearAndPaste(t0, t1, (WaveTrack *)c, true, true);
            }else{
               WaveTrack *tmp;
               tmp = mTrackFactory->NewWaveTrack( ((WaveTrack*)n)->GetSampleFormat(), ((WaveTrack*)n)->GetRate());
               bool bResult = tmp->InsertSilence(0.0, msClipT1 - msClipT0); // MJS: Is this correct?
               wxASSERT(bResult); // TO DO: Actually handle this.
               tmp->Flush();

               bPastedSomething |=
                  ((WaveTrack *)n)->ClearAndPaste(t0, t1, tmp, true, true);

               delete tmp;
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
      mViewInfo.sel1 = t0 + msClipT1 - msClipT0;

      PushState(_("Pasted from the clipboard"), _("Paste"));

      RedrawProject();

      if (f)
         mTrackPanel->EnsureVisible(f);
   }
}

// Handle text paste (into active label), if any. Return true if did paste.
// (This was formerly the first part of overly-long OnPaste.)
bool AudacityProject::HandlePasteText()
{
   TrackListOfKindIterator iterLabelTrack(Track::Label, mTracks);
   LabelTrack* pLabelTrack = (LabelTrack*)(iterLabelTrack.First());
   while (pLabelTrack)
   {
      // Does this track have an active label?
      if (pLabelTrack->IsSelected()) {

         // Yes, so try pasting into it
         if (pLabelTrack->PasteSelectedText(mViewInfo.sel0, mViewInfo.sel1))
         {
            PushState(_("Pasted text from the clipboard"), _("Paste"));

            // Make sure caret is in view
            int x;
            if (pLabelTrack->CalcCursorX(this, &x)) {
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
// If nothing was selected, create and paste into new tracks.
// (This was formerly the second part of overly-long OnPaste.)
bool AudacityProject::HandlePasteNothingSelected()
{
   // First check whether anything's selected.
   bool bAnySelected = false;
   TrackListIterator iterTrack(mTracks);
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
      TrackListIterator iterClip(msClipboard);
      Track* pClip = iterClip.First();
      if (!pClip)
         return true; // nothing to paste

      Track* pNewTrack;
      Track* pFirstNewTrack = NULL;
      while (pClip) {
         if ((msClipProject != this) && (pClip->GetKind() == Track::Wave))
            ((WaveTrack*)pClip)->Lock();

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
         mTracks->Add(pNewTrack);
         pNewTrack->SetSelected(true);

         if (msClipProject != this && pClip->GetKind() == Track::Wave)
            ((WaveTrack *) pClip)->Unlock();

         if (!pFirstNewTrack)
            pFirstNewTrack = pNewTrack;

         pClip = iterClip.Next();
      }

      // Select some pasted samples, which is probably impossible to get right
      // with various project and track sample rates.
      // So do it at the sample rate of the project
      AudacityProject *p = GetActiveProject();
      double projRate = p->GetRate();
      double quantT0 = QUANTIZED_TIME(msClipT0, projRate);
      double quantT1 = QUANTIZED_TIME(msClipT1, projRate);
      mViewInfo.sel0 = 0.0;   // anywhere else and this should be half a sample earlier
      mViewInfo.sel1 = quantT1 - quantT0;

      PushState(_("Pasted from the clipboard"), _("Paste"));

      RedrawProject();

      if (pFirstNewTrack)
         mTrackPanel->EnsureVisible(pFirstNewTrack);

      return true;
   }
}


// Creates a new label in each selected label track with text from the system
// clipboard
void AudacityProject::OnPasteNewLabel()
{
   bool bPastedSomething = false;

   SelectedTrackListOfKindIterator iter(Track::Label, mTracks);
   Track *t = iter.First();
   if (!t)
   {
      // If there are no selected label tracks, try to choose the first label
      // track after some other selected track
      TrackListIterator iter1(mTracks);
      for (Track *t1 = iter1.First(); t1; t1 = iter1.Next()) {
         if (t1->GetSelected()) {
            // Look for a label track
            while ((t1 = iter1.Next())) {
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
         t = new LabelTrack(mDirManager);
         mTracks->Add(t);
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

      // Add a new label, paste into it
      lt->AddLabel(mViewInfo.sel0, mViewInfo.sel1);
      if (lt->PasteSelectedText(mViewInfo.sel0, mViewInfo.sel1))
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
      mViewInfo.sel1 = mViewInfo.sel0 + (msClipT1 - msClipT0); // MJS: pointless, given what we do in OnPaste?
   }
   OnPaste();

   return;
}

void AudacityProject::OnTrim()
{
   if (mViewInfo.sel0 >= mViewInfo.sel1)
      return;

   TrackListIterator iter(mTracks);
   Track *n = iter.First();

   while (n) {
      if (n->GetSelected()) {
         switch (n->GetKind())
         {
#if defined(USE_MIDI)
            case Track::Note:
               ((NoteTrack*)n)->Trim(mViewInfo.sel0, mViewInfo.sel1);
            break;
#endif

            case Track::Wave:
               //Delete the section before the left selector
               ((WaveTrack*)n)->Trim(mViewInfo.sel0, mViewInfo.sel1);
            break;

            default:
            break;
         }
      }
      n = iter.Next();
   }

   PushState(wxString::Format(_("Trim selected audio tracks from %.2f seconds to %.2f seconds"),
       mViewInfo.sel0, mViewInfo.sel1),
       _("Trim Audio"));

   RedrawProject();
}

void AudacityProject::OnDelete()
{
   Clear();
}

void AudacityProject::OnSplitDelete()
{
   TrackListIterator iter(mTracks);

   Track *n = iter.First();

   while (n) {
      if (n->GetSelected()) {
         if (n->GetKind() == Track::Wave)
         {
            ((WaveTrack*)n)->SplitDelete(mViewInfo.sel0, mViewInfo.sel1);
         }
         else {
            n->Silence(mViewInfo.sel0, mViewInfo.sel1);
         }
      }
      n = iter.Next();
   }

   PushState(wxString::Format(_("Split-deleted %.2f seconds at t=%.2f"),
                              mViewInfo.sel1 - mViewInfo.sel0,
                              mViewInfo.sel0),
             _("Split Delete"));

   RedrawProject();
}

void AudacityProject::OnDisjoin()
{
   TrackListIterator iter(mTracks);

   Track *n = iter.First();

   while (n) {
      if (n->GetSelected()) {
         if (n->GetKind() == Track::Wave)
         {
            ((WaveTrack*)n)->Disjoin(mViewInfo.sel0, mViewInfo.sel1);
         }
      }
      n = iter.Next();
   }

   PushState(wxString::Format(_("Detached %.2f seconds at t=%.2f"),
                              mViewInfo.sel1 - mViewInfo.sel0,
                              mViewInfo.sel0),
             _("Detach"));

   RedrawProject();
}

void AudacityProject::OnJoin()
{
   TrackListIterator iter(mTracks);

   Track *n = iter.First();

   while (n) {
      if (n->GetSelected()) {
         if (n->GetKind() == Track::Wave)
         {
            ((WaveTrack*)n)->Join(mViewInfo.sel0, mViewInfo.sel1);
         }
      }
      n = iter.Next();
   }

   PushState(wxString::Format(_("Joined %.2f seconds at t=%.2f"),
                              mViewInfo.sel1 - mViewInfo.sel0,
                              mViewInfo.sel0),
             _("Join"));

   RedrawProject();
}

void AudacityProject::OnSilence()
{
   SelectedTrackListOfKindIterator iter(Track::Wave, mTracks);

   for (Track *n = iter.First(); n; n = iter.Next())
      n->Silence(mViewInfo.sel0, mViewInfo.sel1);

   PushState(wxString::
             Format(_("Silenced selected tracks for %.2f seconds at %.2f"),
                    mViewInfo.sel1 - mViewInfo.sel0, mViewInfo.sel0),
             _("Silence"));

   mTrackPanel->Refresh(false);
}

void AudacityProject::OnDuplicate()
{
   TrackListIterator iter(mTracks);

   Track *l = iter.Last();
   Track *n = iter.First();

   while (n) {
      if (n->GetSelected()) {
         Track *dest = NULL;
         n->Copy(mViewInfo.sel0, mViewInfo.sel1, &dest);
         if (dest) {
            dest->Init(*n);
            dest->SetOffset(wxMax(mViewInfo.sel0, n->GetOffset()));
            mTracks->Add(dest);
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
  if( mViewInfo.sel0 >= mViewInfo.sel1 )
     return;

  // Because of grouping the copy may need to operate on different tracks than
  // the clear, so we do these actions separately.
  EditClipboardByLabel( &WaveTrack::Copy );

  if( gPrefs->Read( wxT( "/GUI/EnableCutLines" ), ( long )0 ) )
     EditByLabel( &WaveTrack::ClearAndAddCutLine, true );
  else
     EditByLabel( &WaveTrack::Clear, true );

  msClipProject = this;

  mViewInfo.sel1 = mViewInfo.sel0;

  PushState(
   /* i18n-hint: (verb) past tense.  Audacity has just cut the labeled audio regions.*/
     _( "Cut labeled audio regions to clipboard" ),
  /* i18n-hint: (verb)*/
     _( "Cut Labeled Audio" ) );

  RedrawProject();
}

void AudacityProject::OnSplitCutLabels()
{
  if( mViewInfo.sel0 >= mViewInfo.sel1 )
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
  if( mViewInfo.sel0 >= mViewInfo.sel1 )
     return;

  EditClipboardByLabel( &WaveTrack::Copy );

  msClipProject = this;

  PushState( _( "Copied labeled audio regions to clipboard" ),
  /* i18n-hint: (verb)*/
     _( "Copy Labeled Audio" ) );

  mTrackPanel->Refresh( false );
}

void AudacityProject::OnDeleteLabels()
{
  if( mViewInfo.sel0 >= mViewInfo.sel1 )
     return;

  EditByLabel( &WaveTrack::Clear, true );

  mViewInfo.sel1 = mViewInfo.sel0;

  PushState(
   /* i18n-hint: (verb) Audacity has just deleted the labeled audio regions*/
     _( "Deleted labeled audio regions" ),
  /* i18n-hint: (verb)*/
     _( "Delete Labeled Audio" ) );

  RedrawProject();
}

void AudacityProject::OnSplitDeleteLabels()
{
  if( mViewInfo.sel0 >= mViewInfo.sel1 )
     return;

  EditByLabel( &WaveTrack::SplitDelete, false );

  PushState(
  /* i18n-hint: (verb) Audacity has just done a special kind of delete on the labeled audio regions */
     _( "Split Deleted labeled audio regions" ),
  /* i18n-hint: (verb) Do a special kind of delete on labeled audio regions*/
     _( "Split Delete Labeled Audio" ) );

  RedrawProject();
}

void AudacityProject::OnSilenceLabels()
{
  if( mViewInfo.sel0 >= mViewInfo.sel1 )
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
  if( mViewInfo.sel0 >= mViewInfo.sel1 )
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
  if( mViewInfo.sel0 >= mViewInfo.sel1 )
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
   TrackListIterator iter(mTracks);

   double sel0 = mViewInfo.sel0;
   double sel1 = mViewInfo.sel1;

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
         double sel0 = mViewInfo.sel0;
         double sel1 = mViewInfo.sel1;

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
   TrackListIterator iter(mTracks);
   Track *l = iter.Last();

   for (Track *n = iter.First(); n; n = iter.Next()) {
      if (n->GetSelected()) {
         Track *dest = NULL;
         double offset = n->GetOffset();
         if (n->GetKind() == Track::Wave) {
            ((WaveTrack*)n)->SplitCut(mViewInfo.sel0, mViewInfo.sel1, &dest);
         }
#if 0
         // LL:  For now, just skip all non-wave tracks since the other do not
         //      yet support proper splitting.
         else {
            n->Cut(mViewInfo.sel0, mViewInfo.sel1, &dest);
         }
#endif
         if (dest) {
            dest->SetChannel(n->GetChannel());
            dest->SetLinked(n->GetLinked());
            dest->SetName(n->GetName());
            dest->SetOffset(wxMax(mViewInfo.sel0, offset));
            mTracks->Add(dest);
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
   TrackListIterator iter(mTracks);

   Track *t = iter.First();
   while (t) {
      t->SetSelected(true);
      t = iter.Next();
   }
   mViewInfo.sel0 = mTracks->GetMinOffset();
   mViewInfo.sel1 = mTracks->GetEndTime();

   ModifyState(false);

   mTrackPanel->Refresh(false);
   if (mMixerBoard)
      mMixerBoard->Refresh(false);
}

void AudacityProject::OnSelectNone()
{
   this->SelectNone();
   mViewInfo.sel1 = mViewInfo.sel0;
   ModifyState(false);
}

void AudacityProject::OnSelectCursorEnd()
{
   double maxEndOffset = -1000000.0;

   TrackListIterator iter(mTracks);
   Track *t = iter.First();

   while (t) {
      if (t->GetSelected()) {
         if (t->GetEndTime() > maxEndOffset)
            maxEndOffset = t->GetEndTime();
      }

      t = iter.Next();
   }

   mViewInfo.sel1 = maxEndOffset;

   ModifyState(false);

   mTrackPanel->Refresh(false);
}

void AudacityProject::OnSelectStartCursor()
{
   double minOffset = 1000000.0;

   TrackListIterator iter(mTracks);
   Track *t = iter.First();

   while (t) {
      if (t->GetSelected()) {
         if (t->GetOffset() < minOffset)
            minOffset = t->GetOffset();
      }

      t = iter.Next();
   }

   mViewInfo.sel0 = minOffset;

   ModifyState(false);

   mTrackPanel->Refresh(false);
}

void AudacityProject::OnSelectSyncLockSel()
{
   bool selected = false;
   TrackListIterator iter(mTracks);
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
   TrackListIterator iter(mTracks);
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

void AudacityProject::ZoomInByFactor( double ZoomFactor )
{
   // LLL: Handling positioning differently when audio is active
   if (gAudioIO->IsStreamActive(GetAudioIOToken()) != 0) {
      Zoom(mViewInfo.zoom * ZoomFactor);
      mTrackPanel->ScrollIntoView(gAudioIO->GetStreamTime());
      mTrackPanel->Refresh(false);
      return;
   }

   // DMM: Here's my attempt to get logical zooming behavior
   // when there's a selection that's currently at least
   // partially on-screen

   bool selectionIsOnscreen =
      (mViewInfo.sel0 < mViewInfo.h + mViewInfo.screen) &&
      (mViewInfo.sel1 >= mViewInfo.h);

   bool selectionFillsScreen =
      (mViewInfo.sel0 < mViewInfo.h) &&
      (mViewInfo.sel1 > mViewInfo.h + mViewInfo.screen);

   if (selectionIsOnscreen && !selectionFillsScreen) {
      // Start with the center of the selection
      double selCenter = (mViewInfo.sel0 + mViewInfo.sel1) / 2;

      // If the selection center is off-screen, pick the
      // center of the part that is on-screen.
      if (selCenter < mViewInfo.h)
         selCenter = mViewInfo.h + (mViewInfo.sel1 - mViewInfo.h) / 2;
      if (selCenter > mViewInfo.h + mViewInfo.screen)
         selCenter = mViewInfo.h + mViewInfo.screen -
            (mViewInfo.h + mViewInfo.screen - mViewInfo.sel0) / 2;

      // Zoom in
      Zoom(mViewInfo.zoom *= ZoomFactor);

      // Recenter on selCenter
      TP_ScrollWindow(selCenter - mViewInfo.screen / 2);
      return;
   }


   double origLeft = mViewInfo.h;
   double origWidth = mViewInfo.screen;
   Zoom(mViewInfo.zoom *= ZoomFactor);

   double newh = origLeft + (origWidth - mViewInfo.screen) / 2;

   // MM: Commented this out because it was confusing users
   /*
   // make sure that the *right-hand* end of the selection is
   // no further *left* than 1/3 of the way across the screen
   if (mViewInfo.sel1 < newh + mViewInfo.screen / 3)
      newh = mViewInfo.sel1 - mViewInfo.screen / 3;

   // make sure that the *left-hand* end of the selection is
   // no further *right* than 2/3 of the way across the screen
   if (mViewInfo.sel0 > newh + mViewInfo.screen * 2 / 3)
      newh = mViewInfo.sel0 - mViewInfo.screen * 2 / 3;
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
   double origLeft = mViewInfo.h;
   double origWidth = mViewInfo.screen;

   Zoom(mViewInfo.zoom *=ZoomFactor);

   double newh = origLeft + (origWidth - mViewInfo.screen) / 2;
   // newh = (newh > 0) ? newh : 0;
   TP_ScrollWindow(newh);

}

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


void AudacityProject::OnZoomNormal()
{
   Zoom(44100.0 / 512.0);
   mTrackPanel->Refresh(false);
}

void AudacityProject::OnZoomFit()
{
   double len = mTracks->GetEndTime();

   if (len <= 0.0)
      return;

   int w, h;
   mTrackPanel->GetTracksUsableArea(&w, &h);
   w -= 10;

   Zoom(w / len);
   TP_ScrollWindow(0.0);
}

void AudacityProject::DoZoomFitV()
{
   int width, height, count;

   mTrackPanel->GetTracksUsableArea(&width, &height);

   height -= 28;

   count = 0;
   TrackListIterator iter(mTracks);
   Track *t = iter.First();
   while (t) {
      if (t->GetKind() == Track::Wave)
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

   TrackListIterator iter2(mTracks);
   t = iter2.First();
   while (t) {
      if (t->GetKind() == Track::Wave)
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
   if (mViewInfo.sel1 <= mViewInfo.sel0)
      return;

   // LL:  The "-1" is just a hack to get around an issue where zooming to
   //      selection doesn't actually get the entire selected region within the
   //      visible area.  This causes a problem with scrolling at end of playback
   //      where the selected region may be scrolled off the left of the screen.
   //      I know this isn't right, but until the real rounding or 1-off issue is
   //      found, this will have to work.
   Zoom(((mViewInfo.zoom * mViewInfo.screen) - 1) / (mViewInfo.sel1 - mViewInfo.sel0));
   TP_ScrollWindow(mViewInfo.sel0);
}

void AudacityProject::OnGoSelStart()
{
   if (mViewInfo.sel1 <= mViewInfo.sel0)
      return;

   TP_ScrollWindow(mViewInfo.sel0 - (mViewInfo.screen / 2));
}

void AudacityProject::OnGoSelEnd()
{
   if (mViewInfo.sel1 <= mViewInfo.sel0)
      return;

   TP_ScrollWindow(mViewInfo.sel1 - (mViewInfo.screen / 2));
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
      mHistoryWindow = new HistoryWindow(this, &mUndoManager);
   mHistoryWindow->Show();
   mHistoryWindow->Raise();
   mHistoryWindow->UpdateDisplay();
}

void AudacityProject::OnKaraoke()
{
   if (!mLyricsWindow)
      mLyricsWindow = new LyricsWindow(this);
   mLyricsWindow->Show();
   UpdateLyrics();
   mLyricsWindow->Raise();
}

void AudacityProject::OnMixerBoard()
{
   if (!mMixerBoardFrame)
   {
      mMixerBoardFrame = new MixerBoardFrame(this);
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

      mFreqWindow = new FreqWindow(this, -1, _("Frequency Analysis"), where);
   }

   wxCommandEvent dummy;
   mFreqWindow->OnReplot(dummy);
   mFreqWindow->Show(true);
   mFreqWindow->Raise();
   mFreqWindow->SetFocus();
}

void AudacityProject::OnContrast()
{
   InitContrastDialog(NULL);
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
   mToolManager->ShowHide( MeterBarID );
   ModifyToolbarMenus();
}

void AudacityProject::OnShowMixerToolBar()
{
   mToolManager->ShowHide( MixerBarID );
   ModifyToolbarMenus();
}

void AudacityProject::OnShowSelectionToolBar()
{
   mToolManager->ShowHide( SelectionBarID );
   ModifyToolbarMenus();
}

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

void AudacityProject::OnSimplifiedView()
{
   mCommandManager.mbHideFlaggedItems = !mCommandManager.mbHideFlaggedItems;
   mCommandManager.Check(wxT("SimplifiedView"), mCommandManager.mbHideFlaggedItems );
   RebuildMenuBar();
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
   ODManager::Pause();

   for (size_t ff = 0; ff < selectedFiles.GetCount(); ff++) {
      wxString fileName = selectedFiles[ff];

      wxString path = ::wxPathOnly(fileName);
      gPrefs->Write(wxT("/DefaultOpenPath"), path);

      Import(fileName);
   }

   gPrefs->Write(wxT("/LastOpenType"),wxT(""));

   gPrefs->Flush();

   HandleResize(); // Adjust scrollers for new track sizes.
   ODManager::Resume();
}

void AudacityProject::OnImportLabels()
{
   wxString path = gPrefs->Read(wxT("/DefaultOpenPath"),::wxGetCwd());

   wxString fileName =
       FileSelector(_("Select a text file containing labels..."),
                    path,     // Path
                    wxT(""),       // Name
                    wxT(".txt"),   // Extension
                    _("Text files (*.txt)|*.txt|All files (*.*)|*.*"),
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

      LabelTrack *newTrack = new LabelTrack(mDirManager);
      wxString sTrackName;
      wxFileName::SplitPath(fileName, NULL, NULL, &sTrackName, NULL);
      newTrack->SetName(sTrackName);

      newTrack->Import(f);

      SelectNone();
      mTracks->Add(newTrack);
      newTrack->SetSelected(true);

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
                                    _("MIDI and Allegro files (*.mid;*.midi;*.gro)|*.mid;*.midi;*.gro|MIDI files (*.mid;*.midi)|*.mid;*.midi|Allegro files (*.gro)|*.gro|All files (*.*)|*.*"),
                                    wxRESIZE_BORDER,        // Flags
                                    this);    // Parent

   if (fileName != wxT("")) {
      path =::wxPathOnly(fileName);
      gPrefs->Write(wxT("/DefaultOpenPath"), path);
      gPrefs->Flush();

      NoteTrack *newTrack = new NoteTrack(mDirManager);

      if (::ImportMIDI(fileName, newTrack)) {

         SelectNone();
         mTracks->Add(newTrack);
         newTrack->SetSelected(true);

         PushState(wxString::Format(_("Imported MIDI from '%s'"),
                                    fileName.c_str()), _("Import MIDI"));

         RedrawProject();
         mTrackPanel->EnsureVisible(newTrack);
      }
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
                    _("All files (*)|*"),
                    wxRESIZE_BORDER,        // Flags
                    this);    // Parent

   if (fileName == wxT(""))
      return;

   path =::wxPathOnly(fileName);
   gPrefs->Write(wxT("/DefaultOpenPath"), path);
   gPrefs->Flush();

   Track **newTracks;
   int numTracks;

   numTracks = ::ImportRaw(this, fileName, mTrackFactory, &newTracks);

   if (numTracks <= 0)
      return;

   AddImportedTracks(fileName, newTracks, numTracks);
   HandleResize(); // Adjust scrollers for new track sizes.
}

void AudacityProject::OnEditMetadata()
{
   if (mTags->ShowEditDialog(this, _("Edit Metadata Tags"), true))
      PushState(_("Edit Metadata Tags"), _("Edit Metadata"));
}

void AudacityProject::HandleMixAndRender(bool toNewTrack)
{
   WaveTrack *newLeft = NULL;
   WaveTrack *newRight = NULL;

   wxGetApp().SetMissingAliasedFileWarningShouldShow(true);

   if (::MixAndRender(mTracks, mTrackFactory, mRate, mDefaultFormat, 0.0, 0.0,
                      &newLeft, &newRight)) {

      // Remove originals, get stats on what tracks were mixed

      TrackListIterator iter(mTracks);
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
                   t = iter.RemoveCurrent(true);
                } else {
                   t = iter.Next();
                };
         }
         else
            t = iter.Next();
      }

      // Add new tracks

      mTracks->Add(newLeft);
      if (newRight)
         mTracks->Add(newRight);

      // If we're just rendering (not mixing), keep the track name the same
      if (selectedCount==1) {
         newLeft->SetName(firstName);
         if (newRight)
            newRight->SetName(firstName);
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
         if (newRight)
            msg.Printf(_("Mixed and rendered %d tracks into one new stereo track"),
                       selectedCount);
         else
            msg.Printf(_("Mixed and rendered %d tracks into one new mono track"),
                       selectedCount);
         PushState(msg, _("Mix and Render"));
      }

      mTrackPanel->SetFocus();
      mTrackPanel->SetFocusedTrack(newLeft);
      mTrackPanel->EnsureVisible(newLeft);
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
   mSel0save = mViewInfo.sel0;
   mSel1save = mViewInfo.sel1;
}

void AudacityProject::OnSelectionRestore()
{
   if ((mSel0save == 0.0) && (mSel1save == 0.0))
      return;

   mViewInfo.sel0 = mSel0save;
   mViewInfo.sel1 = mSel1save;

   ModifyState(false);

   mTrackPanel->Refresh(false);
}

void AudacityProject::OnCursorTrackStart()
{
   double minOffset = 1000000.0;

   TrackListIterator iter(mTracks);
   Track *t = iter.First();

   while (t) {
      if (t->GetSelected()) {
         if (t->GetOffset() < minOffset)
            minOffset = t->GetOffset();
      }

      t = iter.Next();
   }

   if (minOffset < 0.0) minOffset = 0.0;
   mViewInfo.sel0 = minOffset;
   mViewInfo.sel1 = minOffset;
   ModifyState(false);
   mTrackPanel->ScrollIntoView(mViewInfo.sel0);
   mTrackPanel->Refresh(false);
}

void AudacityProject::OnCursorTrackEnd()
{
   double maxEndOffset = -1000000.0;
   double thisEndOffset = 0.0;

   TrackListIterator iter(mTracks);
   Track *t = iter.First();

   while (t) {
      if (t->GetSelected()) {
         thisEndOffset = t->GetEndTime();
         if (thisEndOffset > maxEndOffset)
            maxEndOffset = thisEndOffset;
      }

      t = iter.Next();
   }

   mViewInfo.sel0 = maxEndOffset;
   mViewInfo.sel1 = maxEndOffset;
   ModifyState(false);
   mTrackPanel->ScrollIntoView(mViewInfo.sel1);
   mTrackPanel->Refresh(false);
}

void AudacityProject::OnCursorSelStart()
{
   mViewInfo.sel1 = mViewInfo.sel0;
   ModifyState(false);
   mTrackPanel->ScrollIntoView(mViewInfo.sel0);
   mTrackPanel->Refresh(false);
}

void AudacityProject::OnCursorSelEnd()
{
   mViewInfo.sel0 = mViewInfo.sel1;
   ModifyState(false);
   mTrackPanel->ScrollIntoView(mViewInfo.sel1);
   mTrackPanel->Refresh(false);
}

void AudacityProject::HandleAlign(int index, bool moveSel)
{
   TrackListIterator iter(mTracks);
   wxString action;
   wxString shortAction;
   double offset;
   double minOffset = 1000000000.0;
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
      delta = mViewInfo.sel0 - minOffset;
      action = _("start to cursor/selection start");
      shortAction = _("Start");
      break;
   case kAlignStartSelEnd:
      delta = mViewInfo.sel1 - minOffset;
      action = _("start to selection end");
      shortAction = _("Start");
      break;
   case kAlignEndSelStart:
      delta = mViewInfo.sel0 - maxEndOffset;
      action = _("end to cursor/selection start");
      shortAction = _("End");
      break;
   case kAlignEndSelEnd:
      delta = mViewInfo.sel1 - maxEndOffset;
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
      TrackListIterator iter(mTracks);
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
      TrackListIterator iter(mTracks);
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
      mViewInfo.sel0 += delta;
      mViewInfo.sel1 += delta;
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
class ASAProgress : public SAProgress {
 private:
   float mTotalWork;
   float mFrames[2];
   long mTotalCells; // how many matrix cells?
   long mCellCount; // how many cells so far?
   long mPrevCellCount; // cell_count last reported with Update()
   ProgressDialog *mProgress;
   #ifdef COLLECT_TIMING_DATA
      FILE *mTimeFile;
      wxDateTime mStartTime;
      long iterations;
   #endif

 public:
   ASAProgress() {
      smoothing = false;
      mProgress = NULL;
      #ifdef COLLECT_TIMING_DATA
         mTimeFile = fopen("timing-data.txt", "w");
      #endif
   }
   ~ASAProgress() {
      delete mProgress;
      #ifdef COLLECT_TIMING_DATA
         fclose(mTimeFile);
      #endif
   }
   virtual void set_phase(int i) {
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
         mProgress = new ProgressDialog(_("Synchronize MIDI with Audio"),
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
   virtual bool set_feature_progress(float s) {
      float work;
      if (phase == 0) {
         float f = s / frame_period;
         work = (is_audio[0] ? AUDIO_WORK_UNIT : MIDI_WORK_UNIT) * f;
      } else if (phase == 1) {
         float f = s / frame_period;
         work = (is_audio[0] ? AUDIO_WORK_UNIT : MIDI_WORK_UNIT) * mFrames[0] +
                (is_audio[1] ? AUDIO_WORK_UNIT : MIDI_WORK_UNIT) * f;
      }
      int updateResult = mProgress->Update(int(work), int(mTotalWork));
      return (updateResult == eProgressSuccess);
   }
   virtual bool set_matrix_progress(int cells) {
      mCellCount += cells;
      float work =
             (is_audio[0] ? AUDIO_WORK_UNIT : MIDI_WORK_UNIT) * mFrames[0] +
             (is_audio[1] ? AUDIO_WORK_UNIT : MIDI_WORK_UNIT) * mFrames[1];
      work += mCellCount * MATRIX_WORK_UNIT;
      int updateResult = mProgress->Update(int(work), int(mTotalWork));
      return (updateResult == eProgressSuccess);
   }
   virtual bool set_smoothing_progress(int i) {
      iterations = i;
      float work =
             (is_audio[0] ? AUDIO_WORK_UNIT : MIDI_WORK_UNIT) * mFrames[0] +
             (is_audio[1] ? AUDIO_WORK_UNIT : MIDI_WORK_UNIT) * mFrames[1] +
             MATRIX_WORK_UNIT * mFrames[0] * mFrames[1];
      work += i * wxMax(mFrames[0], mFrames[1]) * SMOOTHING_WORK_UNIT;
      int updateResult = mProgress->Update(int(work), int(mTotalWork));
      return (updateResult == eProgressSuccess);
   }
};


long mixer_process(void *mixer, float **buffer, long n)
{
   Mixer *mix = (Mixer *) mixer;
   long frame_count = mix->Process(n);
   *buffer = (float *) mix->GetBuffer();
   return frame_count;
}

void AudacityProject::OnScoreAlign()
{
   TrackListIterator iter(mTracks);
   Track *t = iter.First();
   int numWaveTracksSelected = 0;
   int numNoteTracksSelected = 0;
   int numOtherTracksSelected = 0;
   NoteTrack *nt;
   NoteTrack *alignedNoteTrack;
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
   ScoreAlignDialog *dlog = new ScoreAlignDialog(NULL, params);
   CloseScoreAlignDialog();

   if (params.mStatus != wxID_OK) return;

   // We're going to do it.
   //pushing the state before the change is wrong (I think)
   //PushState(_("Sync MIDI with Audio"), _("Sync MIDI with Audio"));
   // Make a copy of the note track in case alignment is canceled or fails
   alignedNoteTrack = (NoteTrack *) nt->Duplicate();
   // Duplicate() on note tracks serializes seq to a buffer, but we need
   // the seq, so Duplicate again and discard the track with buffer. The
   // test is here in case Duplicate() is changed in the future.
   if (alignedNoteTrack->GetSequence() == NULL) {
      NoteTrack *temp = (NoteTrack *) alignedNoteTrack->Duplicate();
      delete alignedNoteTrack;
      alignedNoteTrack = temp;
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

   WaveTrack **waveTracks;
   mTracks->GetWaveTracks(true /* selectionOnly */,
                          &numWaveTracksSelected, &waveTracks);

   Mixer *mix = new Mixer(numWaveTracksSelected,   // int numInputTracks
                          waveTracks,              // WaveTrack **inputTracks
                          mTracks->GetTimeTrack(), // TimeTrack *timeTrack
                          0.0,                     // double startTime
                          endTime,                 // double stopTime
                          2,                       // int numOutChannels
                          44100,                   // int outBufferSize
                          true,                    // bool outInterleaved
                          mRate,                   // double outRate
                          floatSample,             // sampleFormat outFormat
                          true,                    // bool highQuality = true
                          NULL);                   // MixerSpec *mixerSpec = NULL
   delete [] waveTracks;

   ASAProgress *progress = new ASAProgress;

   // There's a lot of adjusting made to incorporate the note track offset into
   // the note track while preserving the position of notes within beats and
   // measures. For debugging, you can see just the pre-scorealign note track
   // manipulation by setting SKIP_ACTUAL_SCORE_ALIGNMENT. You could then, for
   // example, save the modified note track in ".gro" form to read the details.
   //#define SKIP_ACTUAL_SCORE_ALIGNMENT 1
#ifndef SKIP_ACTUAL_SCORE_ALIGNMENT
   int result = scorealign((void *) mix, &mixer_process,
                           2 /* channels */, 44100.0 /* srate */, endTime,
                           alignedNoteTrack->GetSequence(), progress, params);
#else
   int result = SA_SUCCESS;
#endif

   delete progress;
   delete mix;

   if (result == SA_SUCCESS) {
      mTracks->Replace(nt, alignedNoteTrack, true);
      RedrawProject();
      wxMessageBox(wxString::Format(
         _("Alignment completed: MIDI from %.2f to %.2f secs, Audio from %.2f to %.2f secs."),
         params.mMidiStart, params.mMidiEnd,
         params.mAudioStart, params.mAudioEnd));
      PushState(_("Sync MIDI with Audio"), _("Sync MIDI with Audio"));
   } else if (result == SA_TOOSHORT) {
      delete alignedNoteTrack;
      wxMessageBox(wxString::Format(
         _("Alignment error: input too short: MIDI from %.2f to %.2f secs, Audio from %.2f to %.2f secs."),
         params.mMidiStart, params.mMidiEnd,
         params.mAudioStart, params.mAudioEnd));
   } else if (result == SA_CANCEL) {
      // wrong way to recover...
      //GetActiveProject()->OnUndo(); // recover any changes to note track
      delete alignedNoteTrack;
      return; // no message when user cancels alignment
   } else {
      //GetActiveProject()->OnUndo(); // recover any changes to note track
      delete alignedNoteTrack;
      wxMessageBox(_("Internal error reported by alignment process."));
   }
}
#endif /* EXPERIMENTAL_SCOREALIGN */


void AudacityProject::OnNewWaveTrack()
{
   WaveTrack *t = mTrackFactory->NewWaveTrack(mDefaultFormat, mRate);
   SelectNone();

   mTracks->Add(t);
   t->SetSelected(true);

   PushState(_("Created new audio track"), _("New Track"));

   RedrawProject();
   mTrackPanel->EnsureVisible(t);
}

void AudacityProject::OnNewStereoTrack()
{
   WaveTrack *t = mTrackFactory->NewWaveTrack(mDefaultFormat, mRate);
   t->SetChannel(Track::LeftChannel);
   SelectNone();

   mTracks->Add(t);
   t->SetSelected(true);
   t->SetLinked (true);

   t = mTrackFactory->NewWaveTrack(mDefaultFormat, mRate);
   t->SetChannel(Track::RightChannel);

   mTracks->Add(t);
   t->SetSelected(true);

   PushState(_("Created new stereo audio track"), _("New Track"));

   RedrawProject();
   mTrackPanel->EnsureVisible(t);
}

void AudacityProject::OnNewLabelTrack()
{
   LabelTrack *t = new LabelTrack(mDirManager);

   SelectNone();

   mTracks->Add(t);
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

   TimeTrack *t = new TimeTrack(mDirManager);

   SelectNone();

   mTracks->AddToHead(t);
   t->SetSelected(true);

   PushState(_("Created new time track"), _("New Track"));

   RedrawProject();
   mTrackPanel->EnsureVisible(t);
}

void AudacityProject::OnTimerRecord()
{
   //we break the prompting and waiting dialogs into two sections
   //because they both give the user a chance to click cancel
   //and therefore remove the newly inserted track.

   TimerRecordDialog dialog(this /* parent */ );
   int modalResult = dialog.ShowModal();
   if (modalResult == wxID_CANCEL)
   {
      // Cancelled before recording - don't need to do anyting.
   }
   else if(!dialog.RunWaitDialog())
   {
      //RunWaitDialog() shows the "wait for start" as well as "recording" dialog
      //if it returned false it means the user cancelled while the recording, so throw out the fresh track.
      //However, we can't undo it here because the PushState() is called in TrackPanel::OnTimer(),
      //which is blocked by this function.
      //so instead we mark a flag to undo it there.
      mTimerRecordCanceled = true;
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

int AudacityProject::DoAddLabel(double left, double right)
{
   LabelTrack *lt = NULL;

   // If the focused track is a label track, use that
   Track *t = mTrackPanel->GetFocusedTrack();
   if (t && t->GetKind() == Track::Label) {
      lt = (LabelTrack *) t;
   }

   // Otherwise look for a label track after the focused track
   if (!lt) {
      TrackListIterator iter(mTracks);
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

   // If none found, start a new label track and use it
   if (!lt) {
      lt = new LabelTrack(mDirManager);
      mTracks->Add(lt);
   }

// LLL: Commented as it seemed a little forceful to remove users
//      selection when adding the label.  This does not happen if
//      you select several tracks and the last one of those is a
//      label track...typing a label will not clear the selections.
//
//   SelectNone();
   lt->SetSelected(true);

   int index = lt->AddLabel(left, right);

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
   DoAddLabel(mViewInfo.sel0, mViewInfo.sel1);
}

void AudacityProject::OnAddLabelPlaying()
{
   if (GetAudioIOToken()>0 &&
       gAudioIO->IsStreamActive(GetAudioIOToken())) {
      double indicator = gAudioIO->GetStreamTime();
      DoAddLabel(indicator, indicator);
   }
}

void AudacityProject::OnEditLabels()
{
   wxString format = GetSelectionFormat();

   LabelDialog dlg(this, mDirManager, mTracks, mViewInfo, mRate, format);

   if (dlg.ShowModal() == wxID_OK) {
      PushState(_("Edited labels"), _("Label"));
      RedrawProject();
   }
}

void AudacityProject::OnApplyChain()
{
   BatchProcessDialog dlg(this);
   dlg.ShowModal();

   // LL:  See comments in ModifyUndoMenuItems() for info about this...
   //
   // Refresh the Undo menu.
   ModifyUndoMenuItems();
}

void AudacityProject::OnEditChains()
{
   EditChainsDialog dlg(this);
   dlg.ShowModal();
}

wxString AudacityProject::BuildCleanFileName(wxString fileName, wxString extension)
{
   wxFileName newFileName(fileName);
   wxString justName = newFileName.GetName();
   wxString pathName = newFileName.GetPath(wxPATH_GET_VOLUME | wxPATH_GET_SEPARATOR);

   if (justName == wxT("")) {
      wxDateTime now = wxDateTime::Now();
      int year = now.GetYear();
      wxDateTime::Month month = now.GetMonth();
      wxString monthName = now.GetMonthName(month);
      int dom = now.GetDay();
      int hour = now.GetHour();
      int minute = now.GetMinute();
      int second = now.GetSecond();
      justName = wxString::Format(wxT("%d-%s-%02d-%02d-%02d-%02d"),
           year, monthName.c_str(), dom, hour, minute, second);

//      SetName(cleanedFileName);
//      bool isStereo;
//      double endTime = project->mTracks->GetEndTime();
//      double startTime = 0.0;
      //OnSelectAll();
      pathName = gPrefs->Read(wxT("/DefaultOpenPath"), ::wxGetCwd());
      ::wxMessageBox(wxString::Format(wxT("Export recording to %s\n/cleaned/%s%s"),
                                      pathName.c_str(), justName.c_str(), extension.c_str()),
                     wxT("Export recording"),
                  wxOK | wxCENTRE);
      pathName += wxT("/");
   }
   wxString cleanedName = pathName;
   cleanedName += wxT("cleaned");
   bool flag  = ::wxFileName::FileExists(cleanedName);
   if (flag == true) {
      ::wxMessageBox(wxT("Cannot create directory 'cleaned'. \nFile already exists that is not a directory"));
      return wxT("");
   }
   ::wxFileName::Mkdir(cleanedName, 0777, wxPATH_MKDIR_FULL); // make sure it exists

   cleanedName += wxT("/");
   cleanedName += justName;
   cleanedName += extension;
   wxGetApp().AddFileToHistory(cleanedName);

   return cleanedName;
}

void AudacityProject::OnRemoveTracks()
{
   TrackListIterator iter(mTracks);
   Track *t = iter.First();
   Track *f = NULL;
   Track *l = NULL;

   while (t) {
      if (t->GetSelected()) {
         if (mMixerBoard && (t->GetKind() == Track::Wave))
            mMixerBoard->RemoveTrackCluster((WaveTrack*)t);
         if (!f)
            f = l;         // Capture the track preceeding the first removed track
         t = iter.RemoveCurrent(true);
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
   AboutDialog dlog(this);
   dlog.ShowModal();
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

void AudacityProject::OnScreenshot()
{
   ::OpenScreenshotTools();
}

void AudacityProject::OnAudioDeviceInfo()
{
   wxString info = gAudioIO->GetDeviceInfo();
   HelpSystem::ShowInfoDialog( this,
      _("Audio Device Info"),
      wxT(""),
      info,
      350,450);
}

void AudacityProject::OnSeparator()
{

}

void AudacityProject::OnCollapseAllTracks()
{
   TrackListIterator iter(mTracks);
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
   TrackListIterator iter(mTracks);
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
   TrackListIterator iter(mTracks);
   Track *t = iter.First();

   while (t)
   {
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
   TrackListIterator iter(mTracks);
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
   mLockPlayRegion = true;
   mRuler->Refresh(false);
}

void AudacityProject::OnUnlockPlayRegion()
{
   mLockPlayRegion = false;
   mRuler->Refresh(false);
}

void AudacityProject::OnResample()
{
   TrackListIterator iter(mTracks);

   int newRate;

   while (true)
   {
      wxDialog dlg(this, wxID_ANY, wxString(_("Resample")));
      ShuttleGui S(&dlg, eIsCreating);
      wxString rate;
      wxArrayString rates;
      wxComboBox *cb;

      rate.Printf(wxT("%d"), lrint(mRate));

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
         S.StartHorizontalLay(wxCENTER, false);
         {
            cb = S.AddCombo(_("New sample rate (Hz):"),
                            rate,
                            &rates);
         }
         S.EndHorizontalLay();
         S.AddStandardButtons();
      }
      S.EndVerticalLay();

      dlg.SetSize(dlg.GetSizer()->GetMinSize());
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

