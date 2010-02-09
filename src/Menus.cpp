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

#include "Project.h"
#include "effects/EffectManager.h"

#include "AudacityApp.h"
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
#ifdef EXPERIMENTAL_LYRICS_WINDOW
   #include "LyricsWindow.h"
#endif
#ifdef EXPERIMENTAL_MIXER_BOARD
   #include "MixerBoard.h"
#endif
#include "Internat.h"
#include "FileFormats.h"
#include "LoadModules.h"	
#include "Prefs.h"
#include "Printing.h"
#include "UploadDialog.h"
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
#include "widgets/ErrorDialog.h"

#include "CaptureEvents.h"

#ifdef EXPERIMENTAL_SCOREALIGN
#include "audioreader.h"
#include "scorealign.h"
#include "scorealign-glue.h"
#endif /* EXPERIMENTAL_SCOREALIGN */

enum {
   kAlignZero=0,
   kAlignCursor,
   kAlignSelStart,
   kAlignSelEnd,
   kAlignEndCursor,
   kAlignEndSelStart,
   kAlignEndSelEnd,
   kAlign
};

typedef void (AudacityProject::*audCommandFunction)();
typedef void (AudacityProject::*audCommandListFunction)(int);

class AudacityProjectCommandFunctor:public CommandFunctor
{
public:
   AudacityProjectCommandFunctor(AudacityProject *project,
                                 audCommandFunction commandFunction)
   {
      mProject = project;
      mCommandFunction = commandFunction;
      mCommandListFunction = NULL;
   }

   AudacityProjectCommandFunctor(AudacityProject *project,
                                 audCommandListFunction commandFunction)
   {
      mProject = project;
      mCommandFunction = NULL;
      mCommandListFunction = commandFunction;
   }

   AudacityProjectCommandFunctor(AudacityProject *project,
                                 audCommandListFunction commandFunction,
                                 wxArrayInt explicitIndices)
   {
      mProject = project;
      mCommandFunction = NULL;
      mCommandListFunction = commandFunction;
      mExplicitIndices = explicitIndices;
   }

   virtual void operator()(int index = 0)
   {
      if (mCommandListFunction && mExplicitIndices.GetCount() > 0)
         (mProject->*(mCommandListFunction)) (mExplicitIndices[index]);
      else if (mCommandListFunction)
         (mProject->*(mCommandListFunction)) (index);
      else
         (mProject->*(mCommandFunction)) ();
   }

private:
   AudacityProject *mProject;
   audCommandFunction mCommandFunction;
   audCommandListFunction mCommandListFunction;
   wxArrayInt mExplicitIndices;
};

#define FN(X) new AudacityProjectCommandFunctor(this, &AudacityProject:: X )
#define FNI(X, I) new AudacityProjectCommandFunctor(this, &AudacityProject:: X, I)

/// CreateMenusAndCommands builds the menus, and also rebuilds them after
/// changes in configured preferences - for example changes in key-bindings
/// affect the short-cut key legend that appears beside each command,
/// and changes in 'CleanSpeech Mode' customise the menus to a restricted
/// subset
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

   c->AddItem(wxT("New"), _("&New"), FN(OnNew), wxT("Ctrl+N"),
              AudioIONotBusyFlag,
              AudioIONotBusyFlag);

   c->AddItem(wxT("Open"), _("&Open..."), FN(OnOpen), wxT("Ctrl+O"),
              AudioIONotBusyFlag,
              AudioIONotBusyFlag);

   /////////////////////////////////////////////////////////////////////////////

   CreateRecentFilesMenu(c);

   /////////////////////////////////////////////////////////////////////////////

   c->AddSeparator();

   c->AddItem(wxT("Close"), _("&Close"), FN(OnClose), wxT("Ctrl+W"));

   if (!mCleanSpeechMode) {
      c->AddItem(wxT("Save"), _("&Save Project"), FN(OnSave), wxT("Ctrl+S"),
                 AudioIONotBusyFlag | UnsavedChangesFlag,
                 AudioIONotBusyFlag | UnsavedChangesFlag);
      c->AddItem(wxT("SaveAs"), _("Save Project &As..."), FN(OnSaveAs));
#ifdef USE_LIBVORBIS
      c->AddItem(wxT("SaveCompressed"), _("Save Compressed Copy of Project..."), FN(OnSaveCompressed));
#endif
   }

   c->AddItem(wxT("CheckDeps"), _("Chec&k Dependencies..."), FN(OnCheckDependencies));

   c->AddSeparator();

   c->AddItem(wxT("EditMetaData"), _("Open Me&tadata Editor..."), FN(OnEditMetadata));

   c->AddSeparator();

   /////////////////////////////////////////////////////////////////////////////

   if (!mCleanSpeechMode) {
      
      c->BeginSubMenu(_("&Import"));

      c->AddItem(wxT("ImportAudio"), _("&Audio..."), FN(OnImport), wxT("Ctrl+Shift+I"));
      c->AddItem(wxT("ImportLabels"), _("&Labels..."), FN(OnImportLabels));
#ifdef USE_MIDI
      c->AddItem(wxT("ImportMIDI"), _("&MIDI..."), FN(OnImportMIDI));
#endif // USE_MIDI
      c->AddItem(wxT("ImportRaw"), _("&Raw Data..."), FN(OnImportRaw));

      c->EndSubMenu();

      c->AddSeparator();
   }

   /////////////////////////////////////////////////////////////////////////////

   // Enable Export commands only when there are tracks
   c->AddItem(wxT("Export"), _("&Export..."), FN(OnExport),
              AudioIONotBusyFlag | WaveTracksExistFlag,
              AudioIONotBusyFlag | WaveTracksExistFlag);

   // Enable Export Selection commands only when there's a selection
   c->AddItem(wxT("ExportSel"), _("Expo&rt Selection..."), FN(OnExportSelection),
              AudioIONotBusyFlag | TimeSelectedFlag | WaveTracksSelectedFlag,
              AudioIONotBusyFlag | TimeSelectedFlag | WaveTracksSelectedFlag);
   
   if (!mCleanSpeechMode) {

      c->AddSeparator();

      c->AddItem(wxT("ExportLabels"), _("Export &Labels..."), FN(OnExportLabels),
                 AudioIONotBusyFlag | LabelTracksExistFlag,
                 AudioIONotBusyFlag | LabelTracksExistFlag);
      c->AddItem(wxT("ExportMultiple"), _("Export &Multiple..."), FN(OnExportMultiple),
                 AudioIONotBusyFlag | TracksExistFlag,
                 AudioIONotBusyFlag | TracksExistFlag);
#if defined(USE_MIDI)
      c->AddItem(wxT("ExportMIDI"),   _("Export MIDI..."), FN(OnExportMIDI),
                 AudioIONotBusyFlag | NoteTracksSelectedFlag,
                 AudioIONotBusyFlag | NoteTracksSelectedFlag);
#endif
   }

   c->AddItem(wxT("ApplyChain"), _("Appl&y Chain..."), FN(OnApplyChain),
              AudioIONotBusyFlag,
              AudioIONotBusyFlag);
   c->AddItem(wxT("EditChains"), _("Edit C&hains..."), FN(OnEditChains));

   if (mCleanSpeechMode) {
      c->AddItem(wxT("ExportCcSettings"), _("Export CleanSpeech &Presets..."), FN(OnExportCleanSpeechPresets));
      c->AddItem(wxT("ImportCcSettings"), _("I&mport CleanSpeech Presets..."), FN(OnImportCleanSpeechPresets));
   }
   else {

#ifdef EXPERIMENTAL_FTP
      c->AddSeparator();

      c->AddItem(wxT("Upload File"), _("&Upload File..."), FN(OnUpload));
#endif

      c->AddSeparator();

      c->AddItem(wxT("PageSetup"), _("Pa&ge Setup..."), FN(OnPageSetup),
                 AudioIONotBusyFlag | TracksExistFlag,
                 AudioIONotBusyFlag | TracksExistFlag);
      c->AddItem(wxT("Print"), _("&Print..."), FN(OnPrint),
                 AudioIONotBusyFlag | TracksExistFlag,
                 AudioIONotBusyFlag | TracksExistFlag);
   }
   
   c->AddSeparator();

   // On the Mac, the Exit item doesn't actually go here...wxMac will pull it out
   // and put it in the Audacity menu for us based on its ID.
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
              
   ModifyUndoMenus();

   c->AddSeparator();

   c->AddItem(wxT("Cut"), _("Cu&t"), FN(OnCut), wxT("Ctrl+X"),
              AudioIONotBusyFlag | CutCopyAvailableFlag,
              AudioIONotBusyFlag | CutCopyAvailableFlag);
   c->AddItem(wxT("SplitCut"), _("Spl&it Cut"), FN(OnSplitCut), wxT("Ctrl+Alt+X"));
   c->AddItem(wxT("Copy"), _("&Copy"), FN(OnCopy), wxT("Ctrl+C"),
              AudioIONotBusyFlag | CutCopyAvailableFlag,
              AudioIONotBusyFlag | CutCopyAvailableFlag);
   c->AddItem(wxT("Paste"), _("&Paste"), FN(OnPaste), wxT("Ctrl+V"),
              AudioIONotBusyFlag | ClipboardFlag,
              AudioIONotBusyFlag | ClipboardFlag);
   c->AddItem(wxT("PasteNewLabel"), _("Paste Te&xt to New Label"), FN(OnPasteNewLabel), wxT("Ctrl+Alt+V"),
              AudioIONotBusyFlag, AudioIONotBusyFlag);
   c->AddItem(wxT("Trim"), _("Tri&m"), FN(OnTrim), wxT("Ctrl+T"));

   c->AddSeparator();

   c->AddItem(wxT("Delete"), _("&Delete"), FN(OnDelete), wxT("Ctrl+K"));
   c->AddItem(wxT("SplitDelete"), _("Split D&elete"), FN(OnSplitDelete), wxT("Ctrl+Alt+K"));
   c->AddItem(wxT("Silence"), _("Silence Audi&o"), FN(OnSilence), wxT("Ctrl+L"));

   c->AddSeparator();

   c->AddItem(wxT("Split"), _("Sp&lit"), FN(OnSplit), wxT("Ctrl+I"),
              AudioIONotBusyFlag | WaveTracksSelectedFlag,
              AudioIONotBusyFlag | WaveTracksSelectedFlag);

   c->AddItem(wxT("SplitNew"), _("Split Ne&w"), FN(OnSplitNew), wxT("Ctrl+Alt+I"),
              AudioIONotBusyFlag | TimeSelectedFlag | WaveTracksSelectedFlag,
              AudioIONotBusyFlag | TimeSelectedFlag | WaveTracksSelectedFlag);

   c->AddItem(wxT("Join"), _("&Join"), FN(OnJoin), wxT("Ctrl+J"));
   c->AddItem(wxT("Disjoin"), _("Detac&h at Silences"), FN(OnDisjoin), wxT("Ctrl+Alt+J"));
   c->AddItem(wxT("Duplicate"), _("Duplic&ate"), FN(OnDuplicate), wxT("Ctrl+D"));

   // An anomaly... StereoToMono is added here for CleanSpeech, 
   // which doesn't have a Tracks menu, but is under Tracks for normal Audacity.
   if (mCleanSpeechMode) {
      c->AddItem(wxT("Stereo to Mono"), _("Stereo Trac&k to Mono"), FN(OnStereoToMono),
                 AudioIONotBusyFlag | StereoRequiredFlag | WaveTracksSelectedFlag,
                 AudioIONotBusyFlag | StereoRequiredFlag | WaveTracksSelectedFlag);
   }

   c->AddSeparator();

   /////////////////////////////////////////////////////////////////////////////

   c->BeginSubMenu(_("La&beled Regions"));
   c->SetDefaultFlags(AudioIONotBusyFlag | LabelsSelectedFlag | TimeSelectedFlag,
                      AudioIONotBusyFlag | LabelsSelectedFlag | TimeSelectedFlag);

   c->AddItem(wxT("CutLabels"), _("&Cut"), FN(OnCutLabels), wxT("Alt+X"),
              AudioIONotBusyFlag | LabelsSelectedFlag | TimeSelectedFlag | LinkingDisabledFlag,
              AudioIONotBusyFlag | LabelsSelectedFlag | TimeSelectedFlag | LinkingDisabledFlag);
   c->AddItem(wxT("SplitCutLabels"), _("&Split Cut"), FN(OnSplitCutLabels), wxT("Shift+Alt+X"));
   c->AddItem(wxT("CopyLabels"), _("Co&py"), FN(OnCopyLabels), wxT("Shift+Alt+C"));

   c->AddSeparator();

   c->AddItem(wxT("DeleteLabels"), _("&Delete"), FN(OnDeleteLabels), wxT("Alt+K"),
              AudioIONotBusyFlag | LabelsSelectedFlag | TimeSelectedFlag | LinkingDisabledFlag,
              AudioIONotBusyFlag | LabelsSelectedFlag | TimeSelectedFlag | LinkingDisabledFlag);
   c->AddItem(wxT("SplitDeleteLabels"), _("Sp&lit Delete"), FN(OnSplitDeleteLabels), wxT("Shift+Alt+K"));
   c->AddItem(wxT("SilenceLabels"), _("Silence &Audio"), FN(OnSilenceLabels), wxT("Alt+L"));

   c->AddSeparator();

   c->AddItem(wxT("SplitLabels"), _("Spli&t"), FN(OnSplitLabels), wxT("Alt+I"),
              AudioIONotBusyFlag | LabelsSelectedFlag,
              AudioIONotBusyFlag | LabelsSelectedFlag);
   c->AddItem(wxT("JoinLabels"), _("&Join"),  FN(OnJoinLabels), wxT("Alt+J"));
   c->AddItem(wxT("DisjoinLabels"), _("Detac&h at Silences"), FN(OnDisjoinLabels), wxT("Shift+Alt+J"));

   c->EndSubMenu();

   /////////////////////////////////////////////////////////////////////////////

   c->BeginSubMenu(_("&Select"));
   c->SetDefaultFlags(TracksExistFlag, TracksExistFlag);

   c->AddItem(wxT("SelectAll"), _("&All"), FN(OnSelectAll), wxT("Ctrl+A"));
   c->AddItem(wxT("SelectNone"), _("&None"), FN(OnSelectNone), wxT("Ctrl+Shift+A"));

   c->AddItem(wxT("SetLeftSelection"), _("&Left at Playback Position"), FN(OnSetLeftSelection), wxT("["));
   c->AddItem(wxT("SetRightSelection"), _("&Right at Playback Position"), FN(OnSetRightSelection), wxT("]"));

   c->SetDefaultFlags(TracksSelectedFlag, TracksSelectedFlag);

   c->AddItem(wxT("SelStartCursor"), _("Track &Start to Cursor"), FN(OnSelectStartCursor), wxT("Shift+J"));
   c->AddItem(wxT("SelCursorEnd"), _("Cursor to Track &End"), FN(OnSelectCursorEnd), wxT("Shift+K"));

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
              TimeSelectedFlag | WaveTracksSelectedFlag,
              TimeSelectedFlag | WaveTracksSelectedFlag);
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

   c->AddItem(wxT("FitInWindow"), _("&Fit in Window"), FN(OnZoomFit), wxT("Ctrl+F"));
   c->AddItem(wxT("FitV"), _("Fit &Vertically"), FN(OnZoomFitV), wxT("Ctrl+Shift+F"));
   c->AddItem(wxT("ZoomSel"), _("&Zoom to Selection"), FN(OnZoomSel), wxT("Ctrl+E"), TimeSelectedFlag, TimeSelectedFlag);

   c->AddSeparator();

   c->AddItem(wxT("CollapseAllTracks"), _("&Collapse All Tracks"), FN(OnCollapseAllTracks), wxT("Ctrl+Shift+C"));
   c->AddItem(wxT("ExpandAllTracks"), _("E&xpand All Tracks"), FN(OnExpandAllTracks), wxT("Ctrl+Shift+X"));

   c->AddSeparator();

   c->AddCheck(wxT("ShowClipping"), _("&Show Clipping"), FN(OnShowClipping),
               gPrefs->Read(wxT("/GUI/ShowClipping"), 0L), AlwaysEnabledFlag, AlwaysEnabledFlag);

   c->AddSeparator();

   c->AddItem(wxT("UndoHistory"), _("&History..."), FN(OnHistory),
              AudioIONotBusyFlag | UndoAvailableFlag,
              AudioIONotBusyFlag | UndoAvailableFlag);

   #ifdef EXPERIMENTAL_LYRICS_WINDOW
      c->AddItem(wxT("Karaoke"), _("&Karaoke..."), FN(OnKaraoke), LabelTracksExistFlag, LabelTracksExistFlag); 
   #endif
   #ifdef EXPERIMENTAL_MIXER_BOARD
      c->AddItem(wxT("MixerBoard"), _("&Mixer Board..."), FN(OnMixerBoard), WaveTracksExistFlag, WaveTracksExistFlag);
   #endif

   c->AddSeparator();

   /////////////////////////////////////////////////////////////////////////////

   c->BeginSubMenu(_("&Toolbars"));

   c->AddCheck(wxT("ShowControlTB"), _("&Control Toolbar"), FN(OnShowControlToolBar), 0, AlwaysEnabledFlag, AlwaysEnabledFlag);
   c->AddCheck(wxT("ShowDeviceTB"), _("&Device Toolbar"), FN(OnShowDeviceToolBar), 0, AlwaysEnabledFlag, AlwaysEnabledFlag);
   c->AddCheck(wxT("ShowEditTB"), _("&Edit Toolbar"), FN(OnShowEditToolBar), 0, AlwaysEnabledFlag, AlwaysEnabledFlag);
   c->AddCheck(wxT("ShowMeterTB"), _("&Meter Toolbar"), FN(OnShowMeterToolBar), 0, AlwaysEnabledFlag, AlwaysEnabledFlag);
   c->AddCheck(wxT("ShowMixerTB"), _("Mi&xer Toolbar"), FN(OnShowMixerToolBar), 0, AlwaysEnabledFlag, AlwaysEnabledFlag);
   c->AddCheck(wxT("ShowSelectionTB"), _("&Selection Toolbar"), FN(OnShowSelectionToolBar), 0, AlwaysEnabledFlag, AlwaysEnabledFlag);
   c->AddCheck(wxT("ShowToolsTB"), _("T&ools Toolbar"), FN(OnShowToolsToolBar), 0, AlwaysEnabledFlag, AlwaysEnabledFlag);
   c->AddCheck(wxT("ShowTranscriptionTB"), _("Transcri&ption Toolbar"), FN(OnShowTranscriptionToolBar), 0, AlwaysEnabledFlag, AlwaysEnabledFlag);

   c->AddSeparator();

   c->AddItem(wxT("ResetToolbars"), _("&Reset Toolbars"), FN(OnResetToolBars), 0, AlwaysEnabledFlag, AlwaysEnabledFlag);

   c->EndSubMenu();

   /////////////////////////////////////////////////////////////////////////////

   c->AddCheck(wxT("SimplifiedView"), _("!Simplified View"), FN(OnSimplifiedView),
               mCommandManager.mbHideFlaggedItems ? 1 : 0);

   c->EndMenu();

   /////////////////////////////////////////////////////////////////////////////
   // Transport Menu
   /////////////////////////////////////////////////////////////////////////////

   /*i18n-hint: 'Transport' is the name given to the set of controls that
   play, record, pause etc. */
   c->BeginMenu(_("T&ransport"));
   c->SetDefaultFlags(AudioIONotBusyFlag, AudioIONotBusyFlag);

   c->AddItem(wxT("Play"), _("Play"), FN(OnPlayStop));
   c->AddItem(wxT("PlayLooped"), _("&Loop Play"), FN(OnPlayLooped), wxT("Shift+Space"));
   c->AddItem(wxT("Pause"), _("&Pause"), FN(OnPause), wxT("P"),
              AudioIOBusyFlag,
              AudioIOBusyFlag);
   c->AddItem(wxT("Stop"), _("&Stop"), FN(OnStop),
              AudioIOBusyFlag,
              AudioIOBusyFlag);
   c->AddItem(wxT("SkipStart"), _("Skip to Start"), FN(OnSkipStart), wxT("Home"));
   c->AddItem(wxT("SkipEnd"), _("Skip to End"), FN(OnSkipEnd), wxT("End"));

   c->AddSeparator();

   c->AddItem(wxT("Record"), _("&Record"), FN(OnRecord), wxT("R"));
   c->AddItem(wxT("TimerRecord"), _("&Timer Record..."), FN(OnTimerRecord), wxT("Shift+T"));
   c->AddItem(wxT("RecordAppend"), _("Append Record"), FN(OnRecordAppend), wxT("Shift+R"));

   c->AddSeparator();

   c->AddCheck(wxT("Duplex"), _("Overdub (on/off)"), FN(OnTogglePlayRecording), 0);
   c->AddCheck(wxT("SWPlaythrough"), _("Software Playthrough (on/off)"), FN(OnToggleSWPlaythrough), 0);

   // Sound Activated recording options
   c->AddCheck(wxT("SoundActivation"), _("Sound Activated Recording (on/off)"), FN(OnToggleSoundActivated), 0);
   c->AddItem(wxT("SoundActivationLevel"), _("Sound Activation Level..."), FN(OnSoundActivated));

#ifdef AUTOMATED_INPUT_LEVEL_ADJUSTMENT
   c->AddCheck(wxT("AutomatedInputLevelAdjustmentOnOff"), _("Automated Input Level Adjustment (on/off)"), FN(OnToogleAutomatedInputLevelAdjustment), 0);
#endif

   if (!mCleanSpeechMode) {

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

      // StereoToMono moves to the Edit menu when in CleanSpeech mode.
      // It belongs here normally, because it is a kind of mix-down.
      c->AddItem(wxT("Stereo to Mono"), _("Stereo Trac&k to Mono"), FN(OnStereoToMono),
                 AudioIONotBusyFlag | StereoRequiredFlag | WaveTracksSelectedFlag,
                 AudioIONotBusyFlag | StereoRequiredFlag | WaveTracksSelectedFlag);
      c->AddItem(wxT("MixAndRender"), _("Mi&x and Render"), FN(OnMixAndRender),
                 AudioIONotBusyFlag | WaveTracksSelectedFlag,
                 AudioIONotBusyFlag | WaveTracksSelectedFlag);
      c->AddCommand(wxT("MixAndRenderToNewTrack"), _("Mix and Render to New Track"), FN(OnMixAndRenderToNewTrack), wxT("Ctrl+Shift+M"),
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
      c->AddItem(wxT("UnMuteAllTracks"), _("&UnMute All Tracks"), FN(OnUnMuteAllTracks), wxT("Ctrl+Shift+U"));

      c->AddSeparator();
   
      wxArrayString alignLabels;
      alignLabels.Add(_("Align with &Zero"));
      alignLabels.Add(_("Align with &Cursor"));
      alignLabels.Add(_("Align with Selection &Start"));
      alignLabels.Add(_("Align with Selection &End"));
      alignLabels.Add(_("Align End with Cu&rsor"));
      alignLabels.Add(_("Align End with Selection Star&t"));
      alignLabels.Add(_("Align End with Selection En&d"));
      alignLabels.Add(_("Align Tracks To&gether"));
   
      c->BeginSubMenu(_("&Align Tracks"));

      c->AddItemList(wxT("Align"), alignLabels, FN(OnAlign));
      c->SetCommandFlags(wxT("Align"),
                         AudioIONotBusyFlag | TracksSelectedFlag,
                         AudioIONotBusyFlag | TracksSelectedFlag);

      c->EndSubMenu();
   
      //////////////////////////////////////////////////////////////////////////

      alignLabels.RemoveAt(7); // Can't align together and move cursor
   
      //////////////////////////////////////////////////////////////////////////

      c->BeginSubMenu(_("Ali&gn and Move Cursor"));

      c->AddItemList(wxT("AlignMove"), alignLabels, FN(OnAlignMoveSel));
      c->SetCommandFlags(wxT("AlignMove"),
                         AudioIONotBusyFlag | TracksSelectedFlag,
                         AudioIONotBusyFlag | TracksSelectedFlag);

      c->EndSubMenu();

      //////////////////////////////////////////////////////////////////////////

#ifdef EXPERIMENTAL_SCOREALIGN
      c->AddItem(wxT("ScoreAlign"), _("Synchronize MIDI with Audio"), FN(OnScoreAlign),
                 AudioIONotBusyFlag | NoteTracksSelectedFlag | WaveTracksSelectedFlag,
                 AudioIONotBusyFlag | NoteTracksSelectedFlag | WaveTracksSelectedFlag);
#endif // EXPERIMENTAL_SCOREALIGN

      c->AddSeparator();

#ifdef EXPERIMENTAL_LINKING
      c->AddCheck(wxT("StickyLabels"), _("&Link Audio and Label Tracks"), FN(OnStickyLabel), 0);

      c->AddSeparator();
#endif

      c->AddItem(wxT("AddLabel"), _("Add Label At &Selection"), FN(OnAddLabel), wxT("Ctrl+B"),
                 AlwaysEnabledFlag,
                 AlwaysEnabledFlag);
      c->AddItem(wxT("AddLabelPlaying"), _("Add Label At &Playback Position"), FN(OnAddLabelPlaying), wxT("Ctrl+M"),
                 0,
                 AudioIONotBusyFlag);
      c->AddItem(wxT("EditLabels"), _("&Edit Labels"), FN(OnEditLabels));

      c->AddSeparator();   

      //////////////////////////////////////////////////////////////////////////

      c->BeginSubMenu(_("S&ort tracks"));

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
   }        

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

   // Cleanspeech doesn't have advanced effects
   int additionalEffects = mCleanSpeechMode ? 0 : ADVANCED_EFFECT;

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

   if (!mCleanSpeechMode) {
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
   }

   c->EndMenu();

#else
   
   // We want plugins and builtins in the same menus, but we don't want plugins
   // or "advanced" effects if we are building CleanSpeech
   int flags = PROCESS_EFFECT | BUILTIN_EFFECT;
   if (!mCleanSpeechMode) {
      flags |= PLUGIN_EFFECT | ADVANCED_EFFECT;
   }

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

   if (!mCleanSpeechMode) {

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
   }

   /////////////////////////////////////////////////////////////////////////////
   // Help Menu
   /////////////////////////////////////////////////////////////////////////////

   #ifdef __WXMAC__
   wxGetApp().s_macHelpMenuTitleName = _("&Help");
   #endif

   c->BeginMenu(_("&Help"));
   c->SetDefaultFlags(AlwaysEnabledFlag, AlwaysEnabledFlag);

   if (mCleanSpeechMode) {
   	c->AddItem(wxT("About"), _("&About Audacity CleanSpeech..."), FN(OnAbout));
   }
   else {
      c->AddItem(wxT("About"), _("&About Audacity..."), FN(OnAbout));
   }

   c->AddItem(wxT("QuickHelp"), _("&Quick Help (in web browser)"), FN(OnQuickHelp));
   c->AddItem(wxT("Manual"), _("&Manual (in web browser)"), FN(OnManual));
   c->AddItem(wxT("Log"), _("Show &Log..."), FN(OnLog));

   if (!mCleanSpeechMode) {

      c->AddSeparator();   

      c->AddItem(wxT("Screenshot"), _("&Screenshot Tools..."), FN(OnScreenshot));

#if IS_BETA
      c->AddSeparator();   

      c->AddItem(wxT("Benchmark"), _("&Run Benchmark..."), FN(OnBenchmark));
#endif

      c->AddSeparator();   

      c->AddItem(wxT("DeviceInfo"), _("&Audio Device Info..."), FN(OnAudioDeviceInfo));
   }

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

   c->AddCommand(wxT("PlayStop"), _("Play/Stop"), FN(OnPlayStop), wxT("Space"));
   c->AddCommand(wxT("PlayStopSelect"), _("Play/Stop and Set Cursor"), FN(OnPlayStopSelect), wxT("Shift+A"));
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

   c->AddCommand(wxT("CursorLeft"), _("Cursor Left"), FN(OnCursorLeft), wxT("Left\tallowdup"));
   c->AddCommand(wxT("CursorRight"), _("Cursor Right"), FN(OnCursorRight), wxT("Right\tallowdup"));
   c->AddCommand(wxT("CursorShortJumpLeft"), _("Cursor Short Jump Left"), FN(OnCursorShortJumpLeft), wxT(","));
   c->AddCommand(wxT("CursorShortJumpRight"), _("Cursor Short Jump Right"), FN(OnCursorShortJumpRight), wxT("."));
   c->AddCommand(wxT("CursorLongJumpLeft"), _("Cursor Long Jump Left"), FN(OnCursorLongJumpLeft), wxT("Shift+,"));
   c->AddCommand(wxT("CursorLongJumpRight"), _("Cursor Long Jump Right"), FN(OnCursorLongJumpRight), wxT("Shift+."));

   c->AddCommand(wxT("SelExtLeft"), _("Selection Extend Left"), FN(OnSelExtendLeft), wxT("Shift+Left\tallowdup"));
   c->AddCommand(wxT("SelExtRight"), _("Selection Extend Right"), FN(OnSelExtendRight), wxT("Shift+Right\tallowdup"));

   c->AddCommand(wxT("SelSetExtLeft"), _("Set (or Extend) Left Selection"), FN(OnSelSetExtendLeft));
   c->AddCommand(wxT("SelSetExtRight"), _("Set (or Extend) Right Selection"), FN(OnSelSetExtendRight));

   c->AddCommand(wxT("SelCntrLeft"), _("Selection Contract Left"), FN(OnSelContractLeft), wxT("Ctrl+Shift+Right"));
   c->AddCommand(wxT("SelCntrRight"), _("Selection Contract Right"), FN(OnSelContractRight), wxT("Ctrl+Shift+Left"));

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

   c->AddCommand(wxT("SnapToOn"), _("Snap To On"), FN(OnSnapToOn));
   c->AddCommand(wxT("SnapToOff"), _("Snap To Off"), FN(OnSnapToOff));

   c->SetDefaultFlags(AlwaysEnabledFlag, AlwaysEnabledFlag);

   c->AddCommand(wxT("FullScreenOnOff"), _("Full screen on/off"), FN(OnFullScreen), wxT("F11"));

   c->AddCommand(wxT("OutputGain"), _("Adjust output gain"), FN(OnOutputGain));
   c->AddCommand(wxT("OutputGainInc"), _("Increase output gain"), FN(OnOutputGainInc));
   c->AddCommand(wxT("OutputGainDec"), _("Decrease output gain"), FN(OnOutputGainDec));
   c->AddCommand(wxT("InputGain"), _("Adjust input gain"), FN(OnInputGain));
   c->AddCommand(wxT("InputGainInc"), _("Increase input gain"), FN(OnInputGainInc));
   c->AddCommand(wxT("InputGainDec"), _("Decrease input gain"), FN(OnInputGainDec));

   c->AddCommand(wxT("InputSource"), _("Adjust input source"), FN(OnInputSource),
                 AudioIONotBusyFlag,
                 AudioIONotBusyFlag);

   c->AddCommand(wxT("PlayAtSpeed"), _("Play at speed"), FN(OnPlayAtSpeed));
   c->AddCommand(wxT("SetPlaySpeed"), _("Adjust playback speed"), FN(OnSetPlaySpeed));
   c->AddCommand(wxT("PlaySpeedInc"), _("Increase playback speed"), FN(OnPlaySpeedInc));
   c->AddCommand(wxT("PlaySpeedDec"), _("Decrease playback speed"), FN(OnPlaySpeedDec));

   mLastFlags = 0;

   mSel0save = 0;
   mSel1save = 0;

#if defined(__WXDEBUG__)
   c->CheckDups();
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

void AudacityProject::ModifyUndoMenus()
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
      wxDialog *d = wxDynamicCast(wxGetTopLevelParent(FindFocus()), wxDialog);
      wxASSERT((!d || !d->IsModal()));
   }
#endif

// Under Windows we delete the menus, since we will soon recreate them.
// rather oddly, the menus don't vanish as a result of doing this.
// Under Linux we can't delete them as this crashes gtk2....
// FIXME: So we have a memory leak of menu items under linux?  Oops.  
#ifdef __WXMSW__
   wxMenuBar *menuBar = GetMenuBar();

   // msmeyer: The following two lines make gtk2 crash on Linux
   DetachMenuBar();
   delete menuBar;
#endif

   /*
   // msmeyer: This also makes gtk2 crash on Linux
   for (int i = menuBar->GetMenuCount()-1; i >= 0; i--)
      delete menuBar->Remove(i);

   // msmeyer: However, this doesn't seem to matter, because CommandManager
   // knows how to properly rebuild menus, even when the menu bar is already
   // populated. So we just don't mess with the menus at this stage.
   */
  
   mCommandManager.PurgeData();

   wxGetApp().GetRecentFiles()->RemoveMenu(mRecentFilesMenu);

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

   if (GetAudioIOToken() == 0 ||
       !gAudioIO->IsAudioTokenActive(GetAudioIOToken()))
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

   if(msClipLen > 0.0)
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

   if (!IsSticky())
      flags |= LinkingDisabledFlag;

   return flags;
}

void AudacityProject::SelectAllIfNone()
{
   wxUint32 flags = GetUpdateFlags();
   if(((flags & TracksSelectedFlag) ==0) || (mViewInfo.sel0 >= mViewInfo.sel1))
      OnSelectAll();
}


void AudacityProject::ModifyToolbarMenus()
{
   // Refreshes can occur during shutdown and the toolmanager may already
   // be deleted, so protect against it.
   if (!mToolManager) {
      return;
   }

   mCommandManager.Check(wxT("ShowControlTB"),
                        mToolManager->IsVisible(ControlBarID));
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
   // the Edit toolbar and the sticky menu item.
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
   mCommandManager.Check(wxT("StickyLabels"), mStickyFlag);
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

   // Return from this function if nothing's changed since
   // the last time we were here.
   wxUint32 flags = GetUpdateFlags();
   wxUint32 flags2 = flags;

   // We can enable some extra items if we have select-all-on-none
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
//    toolbar->StopPlaying();
      mViewInfo.sel0 = gAudioIO->GetStreamTime();
      if( mViewInfo.sel1 < mViewInfo.sel0 ) {
         mViewInfo.sel1 = mViewInfo.sel0;
      }
      GetControlToolBar()->OnStop(evt);
      ModifyState();
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
   }

   ModifyState();
}

void AudacityProject::OnToggleSoundActivated()
{
   bool pause;
   gPrefs->Read(wxT("/AudioIO/SoundActivatedRecord"), &pause, false);
   gPrefs->Write(wxT("/AudioIO/SoundActivatedRecord"), !pause);
   ModifyToolbarMenus();
}

void AudacityProject::OnTogglePlayRecording()
{
   bool Duplex;
   gPrefs->Read(wxT("/AudioIO/Duplex"), &Duplex, false);
   gPrefs->Write(wxT("/AudioIO/Duplex"), !Duplex);
   ModifyToolbarMenus();
}

void AudacityProject::OnToggleSWPlaythrough()
{
   bool SWPlaythrough;
   gPrefs->Read(wxT("/AudioIO/SWPlaythrough"), &SWPlaythrough, false);
   gPrefs->Write(wxT("/AudioIO/SWPlaythrough"), !SWPlaythrough);
   ModifyToolbarMenus();
}

#ifdef AUTOMATED_INPUT_LEVEL_ADJUSTMENT
void AudacityProject::OnToogleAutomatedInputLevelAdjustment()
{
   bool AVEnabled;
   gPrefs->Read(wxT("/AudioIO/AutomatedInputLevelAdjustment"), &AVEnabled, false);
   gPrefs->Write(wxT("/AudioIO/AutomatedInputLevelAdjustment"), !AVEnabled);
   ModifyToolbarMenus();
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

void AudacityProject::OnSortTime()
{
   int ndx;

   wxArrayPtrVoid warr, marr;
   TrackListIterator iter(mTracks);
   Track *track = iter.First();
   while (track) {
      if (track->GetKind() == Track::Wave) {
         for (ndx = 0; ndx < (int)warr.GetCount(); ndx++) {
            if (GetTime(track) < GetTime((Track *) warr[ndx])) {
               break;
            }
         }
         warr.Insert(track, ndx);
      }
      else {
         for (ndx = 0; ndx < (int)marr.GetCount(); ndx++) {
            if (GetTime(track) < GetTime((Track *) marr[ndx])) {
               break;
            }
         }
         marr.Insert(track, ndx);
      }
      track = iter.RemoveCurrent();
   }

   for (ndx = 0; ndx < (int)marr.GetCount(); ndx++) {
      mTracks->Add((Track *)marr[ndx]);
   }

   for (ndx = 0; ndx < (int)warr.GetCount(); ndx++) {
      mTracks->Add((Track *)warr[ndx]);
   }

   PushState(_("Tracks sorted by time"), _("Sort By Time"));

   mTrackPanel->Refresh(false);
}

void AudacityProject::OnSortName()
{
   int ndx;

   wxArrayPtrVoid arr;
   TrackListIterator iter(mTracks);
   Track *track = iter.First();
   while (track) {
      for (ndx = 0; ndx < (int)arr.GetCount(); ndx++) {
         if (track->GetName() < ((Track *) arr[ndx])->GetName()) {
            break;
         }
      }
      arr.Insert(track, ndx);
      track = iter.RemoveCurrent();
   }

   for (ndx = 0; ndx < (int)arr.GetCount(); ndx++) {
      mTracks->Add((Track *)arr[ndx]);
   }

   PushState(_("Tracks sorted by name"), _("Sort By Name"));

   mTrackPanel->Refresh(false);
}

void AudacityProject::OnSkipStart()
{
   wxCommandEvent evt;

   GetControlToolBar()->OnRewind(evt);
}

void AudacityProject::OnSkipEnd()
{
   wxCommandEvent evt;

   GetControlToolBar()->OnFF(evt);
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
   ModifyState();
}

void AudacityProject::OnSelToEnd()
{
   SkipEnd(true);
   ModifyState();
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

void AudacityProject::OnCursorLeft()
{
   mTrackPanel->OnCursorLeft( false, false );
}

void AudacityProject::OnCursorRight()
{
   mTrackPanel->OnCursorRight( false, false );
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

void AudacityProject::OnSelExtendLeft()
{
   mTrackPanel->OnCursorLeft( true, false );
}

void AudacityProject::OnSelExtendRight()
{
   mTrackPanel->OnCursorRight( true, false );
}

void AudacityProject::OnSelContractLeft()
{
   mTrackPanel->OnCursorRight( true, true );
}

void AudacityProject::OnSelContractRight()
{
   mTrackPanel->OnCursorLeft( true, true );
}

//this pops up a dialog which allows the left selection to be set.
//If playing/recording is happening, it sets the left selection at
//the current play position.
void AudacityProject::OnSetLeftSelection()
{
   if (GetAudioIOToken()>0 &&
       gAudioIO->IsStreamActive(GetAudioIOToken()))
      {
         double indicator = gAudioIO->GetStreamTime();
         mViewInfo.sel0 = indicator;
      }
   else
      {
         wxString fmt = gPrefs->Read(wxT("/SelectionFormat"), wxT(""));

         TimeDialog D(this, _("Set Left Selection Boundary"), _("Position"));
         D.SetSampleRate(mRate);
         D.SetFormatString(fmt);
         D.SetTimeValue(mViewInfo.sel0);
         if(wxID_OK==D.ShowModal() )
            {
               //Get the value from the dialog
               mViewInfo.sel0 = D.GetTimeValue();
               
               //Make sure it is 'legal'
               if(mViewInfo.sel0 < 0.0)
                  mViewInfo.sel0 = 0.0;
            }
      }
   
   if(mViewInfo.sel1 < mViewInfo.sel0)
      mViewInfo.sel1 = mViewInfo.sel0;

   ModifyState();

   mTrackPanel->Refresh(false);
}


void AudacityProject::OnSetRightSelection()
{
   if (GetAudioIOToken()>0 &&
       gAudioIO->IsStreamActive(GetAudioIOToken())) 
      {
         double indicator = gAudioIO->GetStreamTime();
         mViewInfo.sel1 = indicator;
      }
   else
      {
         wxString fmt = gPrefs->Read(wxT("/SelectionFormat"), wxT(""));

         TimeDialog D(this, _("Set Right Selection Boundary"), _("Position"));
         D.SetSampleRate(mRate);
         D.SetFormatString(fmt);
         D.SetTimeValue(mViewInfo.sel1);
         if(wxID_OK==D.ShowModal() )
            {
               //Get the value from the dialog
               mViewInfo.sel1 = D.GetTimeValue();
               
               //Make sure it is 'legal'
               if(mViewInfo.sel1 < 0)
                  mViewInfo.sel1 = 0;
            }
      }

   if(mViewInfo.sel0 >  mViewInfo.sel1)
      mViewInfo.sel0 = mViewInfo.sel1;

   ModifyState();
   
   mTrackPanel->Refresh(false);
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

void AudacityProject::OnInputSource()
{
   MixerToolBar *tb = GetMixerToolBar();
   if (tb) {
      tb->ShowInputSourceDialog();
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
      one->Get((samplePtr)oneDist, floatSample,
               s - oneWindowSize/2, oneWindowSize);

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

      for(i=0; i<windowSize; i++) {
         if (windowSize != oneWindowSize)
            j = i * (oneWindowSize-1) / (windowSize-1);
         else
            j = i;

         dist[i] += oneDist[j];
         // Apply a small penalty for distance from the original endpoint
         dist[i] += 0.1 * (abs(i - windowSize/2)) / float(windowSize/2);
      }

      track = iter.Next();
   }

   // Find minimum
   int argmin = 0;
   float min = 1.0;
   for(i=0; i<windowSize; i++) {
      if (dist[i] < min) {
         argmin = i;
         min = dist[i];
      }
   }

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

   ModifyState();

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

void AudacityProject::OnRepeatLastEffect(int index)
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
   // set additionalEffects to zero to exclude the advanced effects.
   if( mCleanSpeechMode )
       additionalEffects = 0;
   OnEffect(BUILTIN_EFFECT | PROCESS_EFFECT | additionalEffects, index);
}

void AudacityProject::OnStereoToMono(int index)
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

void AudacityProject::OnUpload()
{
   //if (mTags->ShowEditDialog(this, wxT("Edit ID3 Tags (for MP3 exporting)")))
   //   PushState(wxT("Edit ID3 Tags"), wxT("Edit ID3 Tags"));

   UploadDialog dlog(this);
   dlog.ShowModal();
}

void AudacityProject::OnExportLabels()
{
   Track *t;
   int numLabelTracks = 0;

   TrackListIterator iter(mTracks);

   t = iter.First();
   while (t) {
      if (t->GetKind() == Track::Label)
         numLabelTracks++;
      t = iter.Next();
   }

   if (numLabelTracks == 0) {
      wxMessageBox(_("There are no label tracks to export."));
      return;
   }

   wxString fName = _("labels.txt");

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
#ifdef __WXMAC__
   wxFile *temp = new wxFile();
   temp->Create(fName);
   delete temp;
#else
   f.Create();
#endif
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

#ifdef __WXMAC__
   f.Write(wxTextFileType_Mac);
#else
   f.Write();
#endif
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

   assert(nt);

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

   e.Process(this, false, 0.0, mTracks->GetEndTime());
}

void AudacityProject::OnExportSelection()
{
   Exporter e;

   e.Process(this, true, mViewInfo.sel0, mViewInfo.sel1);
}

void AudacityProject::OnExportMultiple()
{
   ExportMultiple em(this);
   
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

   TrackList *l = mUndoManager.Undo(&mViewInfo.sel0, &mViewInfo.sel1);
   PopState(l);

   mTrackPanel->SetFocusedTrack(NULL);
   mTrackPanel->EnsureVisible(mTrackPanel->GetFirstSelectedTrack());

   RedrawProject();

   if (mHistoryWindow)
      mHistoryWindow->UpdateDisplay();

   ModifyUndoMenus();   
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

   ModifyUndoMenus();
}

void AudacityProject::OnCut()
{
   TrackAndGroupIterator iter(mTracks);
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
         switch (n->GetKind())
         {
#if defined(USE_MIDI)
            case Track::Note:
               // Since portsmf has a built-in cut operator, we use that instead
               n->Cut(mViewInfo.sel0, mViewInfo.sel1, &dest);
            break;
#endif
            default:
               n->Copy(mViewInfo.sel0, mViewInfo.sel1, &dest);
            break;
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

   n = iter.First();
   while (n) {
      if (n->GetSelected()) {
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
      // Selected wave and label tracks may need group iteration
      if (IsSticky() && n->GetSelected() &&
            (n->GetKind() == Track::Wave || n->GetKind() == Track::Label))
         n = iter.NextGroup();
      else
         n = iter.Next();
   }

   msClipLen = (mViewInfo.sel1 - mViewInfo.sel0);
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

   msClipLen = (mViewInfo.sel1 - mViewInfo.sel0);
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

   msClipLen = (mViewInfo.sel1 - mViewInfo.sel0);
   msClipProject = this;

   //Make sure the menus/toolbar states get updated
   mTrackPanel->Refresh(false);
}

void AudacityProject::OnPaste()
{
   // Handle text pastes (into active labels) first
   TrackListOfKindIterator iterlt(Track::Label, mTracks);

   LabelTrack *lt = (LabelTrack *) iterlt.First();
   if (lt) {

      while (lt) {
         // Does this track have an active label?
         if (lt->IsSelected()) {

            // Yes, so try pasting into it
            if (lt->PasteSelectedText(mViewInfo.sel0, mViewInfo.sel1)) {
               PushState(_("Pasted text from the clipboard"), _("Paste"));

               // Make sure caret is in view
               int x;
               if (lt->CalcCursorX(this, &x)) {
                  mTrackPanel->ScrollIntoView(x);
               }

               // Redraw everyting (is that necessary???) and bail
               RedrawProject();
               return;
            }
         }

         // Find the next one
         lt = (LabelTrack *) iterlt.Next();
      }
   }

   // If nothing's selected, we just insert new tracks...so first
   // check to see if anything's selected
   
   TrackListIterator iter2(mTracks);
   Track *countTrack = iter2.First();

   int numSelected = 0;

   while (countTrack) {
      if (countTrack->GetSelected()) {
         numSelected++;
      }
      countTrack = iter2.Next();
   }

   //If nothing's selected
   if (numSelected == 0) {
      TrackListIterator clipIter(msClipboard);
      Track *c = clipIter.First();
      if(c == NULL)  // if there is nothing to paste
         return;
      Track *n;
      Track *f = NULL;

      while (c) {
         if (msClipProject != this && c->GetKind() == Track::Wave)
            ((WaveTrack *) c)->Lock();
         
         switch(c->GetKind()) {
         case Track::Wave: {
            WaveTrack *w = (WaveTrack *)c;
            n = mTrackFactory->NewWaveTrack(w->GetSampleFormat(), w->GetRate());
            } break;
         #ifdef USE_MIDI
         case Track::Note:
            n = mTrackFactory->NewNoteTrack();
            break;
         #endif // USE_MIDI
         case Track::Label:
            n = mTrackFactory->NewLabelTrack();
            break;
            
         case Track::Time:
            n = mTrackFactory->NewTimeTrack();
            break;

         default:
            c = clipIter.Next();
            continue;
         }

         if (!f)
            f = n;

         n->SetLinked(c->GetLinked());
         n->SetChannel(c->GetChannel());
         n->SetName(c->GetName());

         n->Paste(0.0, c);
         mTracks->Add(n);
         n->SetSelected(true);         
         
         if (msClipProject != this && c->GetKind() == Track::Wave)
            ((WaveTrack *) c)->Unlock();
         
         c = clipIter.Next();
      }

      mViewInfo.sel0 = 0.0;
      mViewInfo.sel1 = msClipLen;

      PushState(_("Pasted from the clipboard"), _("Paste"));
      
      RedrawProject();

      if (f)
         mTrackPanel->EnsureVisible(f);

      return;
   }

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
   
   bool pastedSomething = false;
   bool trackTypeMismatch = false;
   bool advanceClipboard = true;

   // Keeps track of whether n would be the first WaveTrack in its group to
   // receive data from the paste.
   bool firstInGroup = true;

   while (n && c) {
      if (n->GetSelected()) {
         advanceClipboard = true;
         if (tmpC) c = tmpC;
         if (c->GetKind() != n->GetKind()){
            if (!trackTypeMismatch) {
               tmpSrc = prev;
               tmpC = c;
            }
            trackTypeMismatch = true;
            advanceClipboard = false;
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
            while (n && (c->GetKind() != n->GetKind()) )
            {
               if (n && n->GetKind() == Track::Label)
                  firstInGroup = true;
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
            // If not the first in group we set useHandlePaste to true
            pastedSomething = ((WaveTrack*)n)->ClearAndPaste(t0, t1,
                  (WaveTrack*)c, true, true, NULL, false, !firstInGroup);
            firstInGroup = firstInGroup && !pastedSomething;
         }
         else if (c->GetKind() == Track::Label &&
                  n && n->GetKind() == Track::Label)
         {
            // AWD: LabelTrack::Paste() doesn't shift future labels (and
            // WaveTrack::HandleGroupPaste() doesn't adjust selected group
            // tracks, so some other track's paste hasn't done it either).  To
            // be (sort of) consistent with Clear behavior, we'll only shift
            // them if linking is on and we have already pasted into a wave
            // track in this group.
            if (IsSticky() && !firstInGroup)
            {
               ((LabelTrack *)n)->ShiftLabelsOnClear(t0, t1);
               ((LabelTrack *)n)->ShiftLabelsOnInsert(msClipLen, t0);
            }

            pastedSomething = n->Paste(t0, c);
         }
         else
         {
            pastedSomething = n->Paste(t0, c);
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

               // firstInGroup should always be false here, unless pasting to
               // the first channel failed
               if (firstInGroup)
               {
                  pastedSomething = ((WaveTrack *)n)->Paste(t0, c);
                  firstInGroup = !pastedSomething;
               }
               else
               {
                  pastedSomething = ((WaveTrack *)n)->HandlePaste(t0, c);
               }
            }
            else {
               n->Clear(t0, t1);
               n->Paste(t0, c);
            }
         }
         
         if (msClipProject != this && c->GetKind() == Track::Wave)
            ((WaveTrack *) c)->Unlock();

         if (advanceClipboard){
            prev = c;
            c = clipIter.Next();
         }
      }

      if (n && n->GetKind() == Track::Label)
         firstInGroup = true;
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
               pastedSomething = ((WaveTrack *)n)->ClearAndPaste(t0, t1,
                     (WaveTrack *)c, true, true, NULL, false, !firstInGroup);
               firstInGroup = firstInGroup && !pastedSomething;
            }else{
               WaveTrack *tmp;
               tmp = mTrackFactory->NewWaveTrack( ((WaveTrack*)n)->GetSampleFormat(), ((WaveTrack*)n)->GetRate());
               tmp->InsertSilence(0.0, msClipLen);
               tmp->Flush();

               pastedSomething = ((WaveTrack *)n)->ClearAndPaste(t0, t1,
                     tmp, true, true, NULL, false, !firstInGroup);
               firstInGroup = firstInGroup && !pastedSomething;

               delete tmp;
            }
         }
         else if (n->GetSelected() && n->GetKind() == Track::Label)
         {
            // Make room in label tracks as necessary
            if (IsSticky() && !firstInGroup)
            {
               ((LabelTrack *)n)->ShiftLabelsOnClear(t0, t1);
               ((LabelTrack *)n)->ShiftLabelsOnInsert(msClipLen, t0);
            }
         }

         if (n && n->GetKind() == Track::Label)
            firstInGroup = true;
         n = iter.Next();
      }
   }
   
   // TODO: What if we clicked past the end of the track?

   if (pastedSomething)
   {
      mViewInfo.sel0 = t0;
      mViewInfo.sel1 = t0 + msClipLen;

      PushState(_("Pasted from the clipboard"), _("Paste"));

      RedrawProject();

      if (f)
         mTrackPanel->EnsureVisible(f);
   }
}

// Creates a new label in each selected label track with text from the system
// clipboard
void AudacityProject::OnPasteNewLabel()
{
   bool pastedSomething = false;

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
         pastedSomething = true;

      // Set previous track
      plt = lt;
   }

   // plt should point to the last label track pasted to -- ensure it's visible
   // and set focus
   if (plt) {
      mTrackPanel->EnsureVisible(plt);
      mTrackPanel->SetFocus();
   }

   if (pastedSomething) {
      PushState(_("Pasted from the clipboard"), _("Paste Text to New Label"));

      // Is this necessary? (carried over from former logic in OnPaste())
      RedrawProject();
   }
}

void AudacityProject::OnPasteOver()
{
   if(msClipLen>0.0)
   {
      mViewInfo.sel1=mViewInfo.sel0+msClipLen;
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
      if ((n->GetKind() == Track::Wave) && n->GetSelected()) {
         //Delete the section before the left selector
        ((WaveTrack*)n)->Trim(mViewInfo.sel0, mViewInfo.sel1);
      }
      n = iter.Next();
   }

   PushState(_("Trim file to selection"), _("Trim"));

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
  
  PushState( _( "Cut labeled regions to the clipboard" ), _( "Cut Labels" ) );

  RedrawProject();
}

void AudacityProject::OnSplitCutLabels()
{
  if( mViewInfo.sel0 >= mViewInfo.sel1 )
     return;

  EditClipboardByLabel( &WaveTrack::SplitCut );
  
  msClipProject = this;

  PushState( _( "SplitCut labeled regions to the clipboard" ), 
        _( "Split Cut Labels" ) );

  RedrawProject();
}

void AudacityProject::OnCopyLabels()
{
  if( mViewInfo.sel0 >= mViewInfo.sel1 )
     return;

  EditClipboardByLabel( &WaveTrack::Copy );
  
  msClipProject = this;
  
  PushState( _( "Copied labeled regions to the clipboard" ), _( "Copy Labels" ) );

  mTrackPanel->Refresh( false );
}

void AudacityProject::OnDeleteLabels()
{
  if( mViewInfo.sel0 >= mViewInfo.sel1 )
     return;
  
  EditByLabel( &WaveTrack::Clear, true );

  mViewInfo.sel1 = mViewInfo.sel0;
  
  PushState( _( "Deleted labeled regions" ), _( "Delete Labels" ) );

  RedrawProject();
}

void AudacityProject::OnSplitDeleteLabels()
{
  if( mViewInfo.sel0 >= mViewInfo.sel1 )
     return;
  
  EditByLabel( &WaveTrack::SplitDelete, false );
  
  PushState( _( "Split Deleted labeled regions" ), _( "Split Delete Labels" ) );

  RedrawProject();
}

void AudacityProject::OnSilenceLabels()
{
  if( mViewInfo.sel0 >= mViewInfo.sel1 )
     return;
  
  EditByLabel( &WaveTrack::Silence, false );
  
  PushState( _( "Silenced labeled regions" ), _( "Silence Labels" ) );

  mTrackPanel->Refresh( false );
}

void AudacityProject::OnSplitLabels()
{
  EditByLabel( &WaveTrack::Split, false );
  
  PushState( _( "Split labeled regions" ), _( "Split Labels" ) );

  RedrawProject();
}

void AudacityProject::OnJoinLabels()
{
  if( mViewInfo.sel0 >= mViewInfo.sel1 )
     return;
  
  EditByLabel( &WaveTrack::Join, false );
  
  PushState( _( "Joined labeled regions" ), _( "Join Labels" ) );

  RedrawProject();
}

void AudacityProject::OnDisjoinLabels()
{
  if( mViewInfo.sel0 >= mViewInfo.sel1 )
     return;
  
  EditByLabel( &WaveTrack::Disjoin, false );
  
  PushState( _( "Detached labeled regions" ), _( "Detach Labels" ) );

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

void AudacityProject::OnSplitLabelsToTracks()
{
   TrackListIterator iter(mTracks);

   Track *n = iter.First();
   Track *srcRight = 0;
   Track *srcLeft = 0;
   bool stereo = false;
   LabelTrack *label = 0;

   while(n) {
      if(n->GetSelected()) {
         if(n->GetKind() == Track::Wave) {
            if(n->GetLinked() == true) {
               stereo = true;
               srcLeft = n;
               srcRight  = iter.Next();
            }
            else {
               srcRight = n;
               stereo = false;
            }
         }
         else if(n->GetKind() == Track::Label)
            label = (LabelTrack*)n;  // cast necessary to call LabelTrack specific methods
      }
      n = iter.Next();
   }

   // one new track for every label, from that label to the next
   
   TrackList newTracks;

   for(int i = 0; i < label->GetNumLabels(); i++) {
      wxString name = label->GetLabel(i)->title;
      double begin = label->GetLabel(i)->t;
      double end;

      // if on the last label, extend to the end of the wavetrack
      if(i == label->GetNumLabels() - 1) {
         if(stereo)
            end = wxMax(srcLeft->GetEndTime(), srcRight->GetEndTime());
         else
            end = srcLeft->GetEndTime();
      }
      else
         end = label->GetLabel(i+1)->t;

      Track *destLeft = 0;
      Track *destRight = 0;

      srcLeft->Copy(begin, end, &destLeft);
      if (destLeft) {
         destLeft->Init(*srcLeft);
         destLeft->SetOffset(wxMax(begin, srcLeft->GetOffset()));
         destLeft->SetName(name);
         
         mTracks->Add(destLeft);
      }

      if(stereo) {
         srcRight->Copy(begin, end, &destRight);
         if (destRight) {
            destRight->Init(*srcRight);
            destRight->SetOffset(wxMax(begin, srcRight->GetOffset()));
            destRight->SetName(name);
            
            mTracks->Add(destRight);
         }
         else if(destLeft)
            // account for possibility of a non-aligned linked track, which could
            // cause the left channel to be eligible for creating a new track,
            // but not the right.
            destLeft->SetLinked(false);
      }
   }

   PushState(_("Split at labels"), _("Split at labels"));

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

   ModifyState();
   
   mTrackPanel->Refresh(false);
   #ifdef EXPERIMENTAL_MIXER_BOARD
      if (mMixerBoard)
         mMixerBoard->Refresh(false);
   #endif
}

void AudacityProject::OnSelectNone()
{
   this->SelectNone();
   mViewInfo.sel1 = mViewInfo.sel0;
   ModifyState();
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

   ModifyState();
   
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

   ModifyState();
   
   mTrackPanel->Refresh(false);
}

//
// View Menu
//

void AudacityProject::OnZoomIn()
{
   // LLL: Handling positioning differently when audio is active
   if (gAudioIO->IsStreamActive(GetAudioIOToken()) != 0) {
      Zoom(mViewInfo.zoom * 2.0);
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
      Zoom(mViewInfo.zoom *= 2.0);

      // Recenter on selCenter
      TP_ScrollWindow(selCenter - mViewInfo.screen / 2);
      return;
   }


   double origLeft = mViewInfo.h;
   double origWidth = mViewInfo.screen;
   Zoom(mViewInfo.zoom *= 2.0);
   
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
   //Zoom() may change these, so record original values:
   double origLeft = mViewInfo.h;
   double origWidth = mViewInfo.screen;

   Zoom(mViewInfo.zoom /= 2.0);

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
   ModifyState();
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

void AudacityProject::OnShowClipping()
{
   bool checked = !gPrefs->Read(wxT("/GUI/ShowClipping"), 0L);
   gPrefs->Write(wxT("/GUI/ShowClipping"), checked);
   mCommandManager.Check(wxT("ShowClipping"), checked);
   mTrackPanel->UpdatePrefs();
   mTrackPanel->Refresh(false);
}

void AudacityProject::OnHistory()
{
   if (!mHistoryWindow)
      mHistoryWindow = new HistoryWindow(this, &mUndoManager);

   mHistoryWindow->Show(true);
   mHistoryWindow->UpdateDisplay();
}

#ifdef EXPERIMENTAL_LYRICS_WINDOW
   void AudacityProject::OnKaraoke()
   {
      if (!mLyricsWindow)
         mLyricsWindow = new LyricsWindow(this);
      wxASSERT(mLyricsWindow);
      mLyricsWindow->Show();
      mLyricsWindow->Raise();
   }
#endif
#ifdef EXPERIMENTAL_MIXER_BOARD
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
#endif

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


void AudacityProject::OnShowControlToolBar()
{
   mToolManager->ShowHide( ControlBarID );
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
   wxArrayString selectedFiles = ShowOpenDialog(wxT(""));
   if (selectedFiles.GetCount() == 0) {
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

      wxTextFile f;

      f.Open(fileName);
      if (!f.IsOpened()) {
         wxMessageBox(_("Could not open file: ") + fileName);
         return;
      }

      LabelTrack *newTrack = new LabelTrack(mDirManager);

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
   if (mTags->ShowEditDialog(this, _("Edit the metadata tags"), true)) {
      PushState(_("Edit Metadata tags"), _("Edit Metadata"));
   }
}

void AudacityProject::HandleMixAndRender(bool toNewTrack)
{
   WaveTrack *newLeft = NULL;
   WaveTrack *newRight = NULL;

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
   mViewInfo.sel0 = mSel0save;
   mViewInfo.sel1 = mSel1save;

   ModifyState();
   
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
   ModifyState();
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
   ModifyState();
   mTrackPanel->ScrollIntoView(mViewInfo.sel1);
   mTrackPanel->Refresh(false);
}

void AudacityProject::OnCursorSelStart()
{
   mViewInfo.sel1 = mViewInfo.sel0;
   ModifyState();
   mTrackPanel->ScrollIntoView(mViewInfo.sel0);
   mTrackPanel->Refresh(false);
}

void AudacityProject::OnCursorSelEnd()
{
   mViewInfo.sel0 = mViewInfo.sel1;
   ModifyState();
   mTrackPanel->ScrollIntoView(mViewInfo.sel1);
   mTrackPanel->Refresh(false);
}

void AudacityProject::HandleAlign(int index, bool moveSel)
{
   TrackListIterator iter(mTracks);
   wxString action;
   double offset;
   double minOffset = 1000000000.0;
   double maxEndOffset = 0.0;
   double avgOffset = 0.0;
   int numSelected = 0;
   Track *t = iter.First();
   double delta = 0.0;
   double newPos = -1.0;

   while (t) {
      if (t->GetSelected()) {
         numSelected++;

         offset = t->GetOffset();
         avgOffset += offset;
         if (offset < minOffset)
            minOffset = offset;
         if (t->GetEndTime() > maxEndOffset)
            maxEndOffset = t->GetEndTime();
      }
      t = iter.Next();
   }

   avgOffset /= numSelected;

   switch(index) {
   case kAlignZero:
      delta = -minOffset;
      action = _("Aligned with zero");
      break;
   case kAlignCursor:
      delta = mViewInfo.sel0 - minOffset;
      action = _("Aligned cursor");
      break;
   case kAlignSelStart:
      delta = mViewInfo.sel0 - minOffset;
      action = _("Aligned with selection start");
      break;
   case kAlignSelEnd:
      delta = mViewInfo.sel1 - minOffset;
      action = _("Aligned with selection end");
      break;
   case kAlignEndCursor:
      delta = mViewInfo.sel0 - maxEndOffset;
      action = _("Aligned end with cursor");
      break;
   case kAlignEndSelStart:
      delta = mViewInfo.sel0 - maxEndOffset;
      action = _("Aligned end with selection start");
      break;
   case kAlignEndSelEnd:
      delta = mViewInfo.sel1 - maxEndOffset;
      action = _("Aligned end with selection end");
      break;
   case kAlign:
      newPos = avgOffset;
      action = _("Aligned");
      break;
   }

   if (newPos >= 0.0) {
      TrackListIterator iter(mTracks);
      Track *t = iter.First();
      
      while (t) {
         if (t->GetSelected()) {
            t->SetOffset(newPos);
         }
         t = iter.Next();
      }
   }

   if (delta != 0.0) {
      TrackListIterator iter(mTracks);
      Track *t = iter.First();
      
      while (t) {
         if (t->GetSelected()) {
            t->SetOffset(t->GetOffset() + delta);
         }
         t = iter.Next();
      }
   }

   if (moveSel) {
      mViewInfo.sel0 += delta;
      mViewInfo.sel1 += delta;
   }

   PushState(action, _("Align"));

   RedrawProject();
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

   // debugging/testing
   //float *buffer;
   //long buffer_len = mixer_process((void *) mix, &buffer, 4096);
   //while (buffer_len) 
   //   buffer_len = mixer_process((void *) mix, &buffer, 4096);
   
   scorealign((void *) mix, &mixer_process,
              2 /* channels */, 44100.0 /* srate */, endTime, nt->GetSequence());

   delete mix;

   PushState(_("Sync MIDI with Audio"), _("Sync MIDI with Audio"));

   RedrawProject();

   wxMessageBox(_("Alignment completed."));
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
      wxMessageBox(_("The version of Audacity you are using does not support multiple time tracks."));
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

int AudacityProject::DoAddLabel(double left, double right)
{
   TrackListIterator iter(mTracks);
   LabelTrack *lt = NULL;

   bool selfound = false;
   Track *t = iter.First();
   while (t) {
      if (t->GetKind() == Track::Label) {
         if (selfound || t->GetSelected()) {
            lt = (LabelTrack *) t;
            break;
         }
         else if (!IsSticky())
            lt = (LabelTrack *) t;
      }
      else if (t->GetSelected())
         selfound = true;

      t = iter.Next();
   }

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

void AudacityProject::OnStickyLabel()
{
   SetStickyFlag(!GetStickyFlag());
   EditToolBar *toolbar = GetEditToolBar();
   toolbar->EnableDisableButtons();
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
   LabelDialog d(this, mDirManager, mTracks, mViewInfo, mRate);
   
   if (d.ShowModal() == wxID_OK) {
      PushState(_("Edited labels"), _("Label"));
      RedrawProject();
   }
}

// #define PRESET_FORMAT 20050501
#define PRESET_FORMAT 20050428
// #define PRESET_COUNT  16
#define PRESET_COUNT  14
void AudacityProject::OnExportCleanSpeechPresets()
{
   wxString userdatadir = FileNames::DataDir();
   #ifdef __WXMSW__
   wxString presetsDefaultLoc = userdatadir + wxT("\\presets");
   #else
   wxString presetsDefaultLoc = userdatadir + wxT("/presets");
   #endif
   wxString path = gPrefs->Read(wxT("/Directories/PresetsDir"), presetsDefaultLoc);

   wxString nameOnly;
   wxString fName;
   wxString extension = wxT(".csp");
   bool fileOkay;

   do {
      fileOkay = true;

      fName = FileSelector(_("Save CleanSpeech Preset File As:"),
                           path,
                           wxT("*.csp"),       // default file extension
                           extension,
                           _("CleanSpeech Presets (*.csp)|*.csp"),
                           wxFD_SAVE | wxFD_OVERWRITE_PROMPT | wxRESIZE_BORDER);

      if (fName.empty()) { // if cancel selected
         return;
      }
      if (fName.Length() >= 256) {
         wxMessageBox(_("Sorry, pathnames longer than 256 characters not supported."));
         fileOkay = false;
         continue;
      }
      ::wxSplitPath(fName, &path, &nameOnly, &extension);
      wxFFile presetsFile(fName, wxT("wb"));

      bool flag = presetsFile.IsOpened();

      if (flag == true) {
         int preset[PRESET_COUNT];
         preset[0]  = PRESET_FORMAT;
         preset[1]  = PRESET_COUNT;

         preset[2]  = gPrefs->Read(wxT("/CsPresets/ClickThresholdLevel"), 200L);
         preset[3]  = gPrefs->Read(wxT("/CsPresets/ClickWidth"), 20L);
         preset[4]  = gPrefs->Read(wxT("/CsPresets/LevellerDbChoiceIndex"), 10L);
         preset[5]  = gPrefs->Read(wxT("/CsPresets/LevellerNumPasses"), 2L);
         preset[6]  = gPrefs->Read(wxT("/CsPresets/Noise_Level"), 3L);
         preset[7]  = gPrefs->Read(wxT("/CsPresets/Norm_AmpDbGain"), 1L);
         preset[8]  = gPrefs->Read(wxT("/CsPresets/Norm_RemoveDcOffset"), 1L);
         preset[9]  = gPrefs->Read(wxT("/CsPresets/SpikeDbChoiceIndex"), 13L);
         preset[10] = gPrefs->Read(wxT("/CsPresets/SpikeMaxDurationMs"), SKIP_EFFECT_MILLISECOND);
         preset[11] = gPrefs->Read(wxT("/Effects/TruncateSilence/DbChoiceIndex"), 8L);
         preset[12] = gPrefs->Read(wxT("/Effects/TruncateSilence/LongestAllowedSilentMs"), 1000L);
//         preset[14] = gPrefs->Read(wxT("/GUI/Save128HqMasterAfter"), 0L);
//         preset[15] = gPrefs->Read(wxT("/GUI/Save128HqMasterBefore"), 0L);

         int expectedCount = wxGetApp().GetCleanSpeechNoiseGateExpectedCount();
         float* pNoiseGate = wxGetApp().GetCleanSpeechNoiseGate();
         double noiseGateSum = 0.0;
         int lenNoiseGate = expectedCount / sizeof(float);
         for (int i = 0; i < lenNoiseGate; ++i) {
            noiseGateSum += fabs(pNoiseGate[i]);
         }
         int noiseCheckSum = abs((int)noiseGateSum);
         preset[13] = noiseCheckSum;
         gPrefs->Write(wxT("/Validate/NoiseGateSum"), noiseCheckSum);

         int lenPreset = sizeof(preset);
         int count = presetsFile.Write(preset, lenPreset);
         count = presetsFile.Write(pNoiseGate, expectedCount);

         presetsFile.Close();
      }
      else {
         wxMessageBox(_("Problem encountered exporting presets."),
                     _("Unable to export"),
                     wxOK | wxICON_WARNING);
         fileOkay = false;
         continue;
      }
   } while(!fileOkay);
}

void AudacityProject::OnImportCleanSpeechPresets()
{
   wxString userdatadir = FileNames::DataDir();
   #ifdef __WXMSW__
   wxString presetsDefaultLoc = userdatadir + wxT("\\presets");
   #else
   wxString presetsDefaultLoc = userdatadir + wxT("/presets");
   #endif

   wxString path = gPrefs->Read(wxT("/Directories/PresetsDir"), presetsDefaultLoc);
   wxString extension = wxT(".csp");
   wxString fName;
   bool fileOkay;

   do {
      fileOkay = true;

      fName = FileSelector(wxT("Open CleanSpeech Preset File:"),
                           path,
                           wxT("*.csp"),       // default file name
                           extension,
                           wxT("CleanSpeech Presets (*.csp)|*.csp"),
                           wxFD_OPEN | wxRESIZE_BORDER);

      if (fName.empty()) { // if cancel selected
         return;
      }
      wxFFile presetsFile(fName, wxT("rb"));
      bool flag = presetsFile.IsOpened();
      if (flag == true) {
         int preset[PRESET_COUNT];
         int lenPreset = sizeof(preset);
         int count = presetsFile.Read(preset, lenPreset);
         if (preset[0] != PRESET_FORMAT) {
            wxMessageBox(wxString::Format(wxT("Preset may be invalid or corrupted.\nExpected format %d ... found %d"), PRESET_FORMAT, preset[0]),
                         wxT("Error opening preset"),
                         wxOK | wxCENTRE | wxICON_WARNING, this);
            return;
         }
         int expectedCount = wxGetApp().GetCleanSpeechNoiseGateExpectedCount();
         float* pNoiseGate = wxGetApp().GetCleanSpeechNoiseGate();
         count = presetsFile.Read(pNoiseGate, expectedCount);

         gPrefs->Write(wxT("/CsPresets/ClickThresholdLevel"), preset[2]);
         gPrefs->Write(wxT("/CsPresets/ClickWidth"), preset[3]);
         gPrefs->Write(wxT("/CsPresets/LevellerDbChoiceIndex"), preset[4]);
         gPrefs->Write(wxT("/CsPresets/LevellerNumPasses"), preset[5]);
         gPrefs->Write(wxT("/CsPresets/Noise_Level"), preset[6]);
         gPrefs->Write(wxT("/CsPresets/Norm_AmpDbGain"), preset[7]);
         gPrefs->Write(wxT("/CsPresets/Norm_RemoveDcOffset"), preset[8]);
         gPrefs->Write(wxT("/CsPresets/SpikeDbChoiceIndex"), preset[9]);
         gPrefs->Write(wxT("/CsPresets/SpikeMaxDurationMs"), preset[10]);
         gPrefs->Write(wxT("/Effects/TruncateSilence/DbChoiceIndex"), preset[11]);
         gPrefs->Write(wxT("/Effects/TruncateSilence/LongestAllowedSilentMs"), preset[12]);
//         gPrefs->Write(wxT("/GUI/Save128HqMasterAfter"), preset[14]);
//         gPrefs->Write(wxT("/GUI/Save128HqMasterBefore"), preset[15]);

         double noiseGateSum = 0.0;
         int lenNoiseGate = expectedCount / sizeof(float);
         for (int i = 0; i < lenNoiseGate; ++i) {
            noiseGateSum += fabs(pNoiseGate[i]);
         }
         preset[13] = abs((int)noiseGateSum);

         presetsFile.Close();
      }
      else {
         wxMessageBox(wxT("Problem encountered importing presets."),
                     wxT("Unable to import"),
                     wxOK | wxICON_WARNING);
         fileOkay = false;
         continue;
      }
   } while(!fileOkay);
}

void AudacityProject::OnApplyChain()
{
   BatchProcessDialog d(this);
   d.ShowModal();

   // LL:  See comments in ModifyUndoMenus() for info about this...
   //
   // Refresh the Undo menu.
   ModifyUndoMenus();
}

void AudacityProject::OnEditChains()
{
   EditChainsDialog d(this);
   d.ShowModal();
}

wxString AudacityProject::BuildCleanFileName(wxString fileName)
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
      ::wxMessageBox(wxString::Format(wxT("Export recording to %s\n/cleaned/%s.mp3"), 
         pathName.c_str(), justName.c_str()),
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
   cleanedName += wxT(".mp3");
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
         #ifdef EXPERIMENTAL_MIXER_BOARD
            if (mMixerBoard && (t->GetKind() == Track::Wave))
               mMixerBoard->RemoveTrackCluster((WaveTrack*)t);
         #endif
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

   mTrackPanel->Refresh(false);
   #ifdef EXPERIMENTAL_MIXER_BOARD
      if (mMixerBoard)
         mMixerBoard->Refresh(true);
   #endif
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
   ShowHelpDialog( 
      this, 
      FileNames::HtmlHelpIndexFile(true),
      wxT("http://manual.audacityteam.org/index.php?title=Quick_Help" ));
}

void AudacityProject::OnManual()
{
   ShowHelpDialog( 
      this, 
      FileNames::HtmlHelpIndexFile(false),
      wxT("http://manual.audacityteam.org/index.php?title=Main_Page" ));
}

void AudacityProject::OnLog()
{
   wxGetApp().mLogger->Show();
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
   wxTextCtrl *tc;

   wxDialog dlg(this, wxID_ANY,
                wxString(wxT("Audio Device Info")),
                wxDefaultPosition, wxDefaultSize,
                wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER );

   ShuttleGui S(&dlg, eIsCreating);

   S.StartHorizontalLay(wxEXPAND, true);
   {
      S.SetStyle( wxTE_READONLY|wxHSCROLL|wxTE_MULTILINE );
      tc = S.AddTextWindow(wxT(""));
      tc->WriteText(info);
   }
   S.EndHorizontalLay();
   
   S.AddStandardButtons(eOkButton);

   dlg.Center();
   dlg.ShowModal();
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

   ModifyState();
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

   ModifyState();
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

   ModifyState();
   RedrawProject();
   #ifdef EXPERIMENTAL_MIXER_BOARD
      if (mMixerBoard)
         mMixerBoard->UpdateMute();
   #endif
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

   ModifyState();
   RedrawProject();
   #ifdef EXPERIMENTAL_MIXER_BOARD
      if (mMixerBoard)
         mMixerBoard->UpdateMute();
   #endif
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
      rates.Add(wxT("96000"));

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

void AudacityProject::OnSnapToOn()
{
   SetSnapTo(true);
}

void AudacityProject::OnSnapToOff()
{
   SetSnapTo(false);
}

void AudacityProject::OnFullScreen()
{
   if(wxTopLevelWindow::IsFullScreen())
      wxTopLevelWindow::ShowFullScreen(false);
   else
      wxTopLevelWindow::ShowFullScreen(true);
}

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: e8ab21c6-d9b9-4d35-b4c2-ff90c1781b85

