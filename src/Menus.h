/**********************************************************************

  Audacity: A Digital Audio Editor

  Menus.h

  Dominic Mazzoni

**********************************************************************/
#ifndef __AUDACITY_MENUS__
#define __AUDACITY_MENUS__

#include "Experimental.h"


// These are all member functions of class AudacityProject.
// Vaughan, 2010-08-05:
//    Note that this file is included in a "public" section of Project.h.
//    Most of these methods do not need to be public, and because
//    we do not subclass AudacityProject, they should be "private."
//    Because the ones that need to be public are intermixed,
//    I've added "private" in just a few cases.

private:
void CreateMenusAndCommands();

void PopulateEffectsMenu(CommandManager *c, EffectType type,
                         CommandFlag batchflags, CommandFlag realflags);
void AddEffectMenuItems(CommandManager *c, EffectPlugs & plugs,
                        CommandFlag batchflags, CommandFlag realflags, bool isDefault);
void AddEffectMenuItemGroup(CommandManager *c, const wxArrayString & names,
                            const PluginIDList & plugs,
                            const std::vector<CommandFlag> & flags, bool isDefault);
void CreateRecentFilesMenu(CommandManager *c);
void ModifyUndoMenuItems();
void ModifyToolbarMenus();
// Calls ModifyToolbarMenus() on all projects
void ModifyAllProjectToolbarMenus();

CommandFlag GetFocusedFrame();
CommandFlag GetUpdateFlags();

double NearestZeroCrossing(double t0);

public:
//Adds label and returns index of label in labeltrack.
int DoAddLabel(const SelectedRegion& region, bool preserveFocus = false);

private:

        // Selecting a tool from the keyboard

void SetTool(int tool);
void OnSelectTool();
void OnZoomTool();
void OnEnvelopeTool();
void OnTimeShiftTool();
void OnDrawTool();
void OnMultiTool();

void OnNextTool();
void OnPrevTool();

public:
        // Audio I/O Commands

void OnStop();
void OnPause();
void OnRecord();
void OnRecordAppend();
void OnStopSelect();
void OnSkipStart();
void OnSkipEnd();
void OnSeekLeftShort();
void OnSeekRightShort();
void OnSeekLeftLong();
void OnSeekRightLong();

        // Different posibilities for playing sound

bool MakeReadyToPlay(bool loop = false, bool cutpreview = false); // Helper function that sets button states etc.
void OnPlayStop();
bool DoPlayStopSelect(bool click, bool shift);
void OnPlayStopSelect();
void OnPlayOneSecond();
void OnPlayToSelection();
void OnPlayBeforeSelectionStart();
void OnPlayAfterSelectionStart();
void OnPlayBeforeSelectionEnd();
void OnPlayAfterSelectionEnd();
void OnPlayBeforeAndAfterSelectionStart();
void OnPlayBeforeAndAfterSelectionEnd();
void OnPlayLooped();
void OnPlayCutPreview();

        // Wave track control

void OnTrackPan();
void OnTrackPanLeft();
void OnTrackPanRight();
void OnTrackGain();
void OnTrackGainInc();
void OnTrackGainDec();
void OnTrackMenu();
void OnTrackMute();
void OnTrackSolo();
void OnTrackClose();
void OnTrackMoveUp();
void OnTrackMoveDown();
void OnTrackMoveTop();
void OnTrackMoveBottom();

enum MoveChoice { OnMoveUpID, OnMoveDownID, OnMoveTopID, OnMoveBottomID };
void MoveTrack(Track* target, MoveChoice choice);

        // Device control
void OnInputDevice();
void OnOutputDevice();
void OnAudioHost();
void OnInputChannels();

        // Mixer control

void OnOutputGain();
void OnInputGain();
void OnOutputGainInc();
void OnOutputGainDec();
void OnInputGainInc();
void OnInputGainDec();

        // Transcription control

void OnPlayAtSpeed();
void OnPlayAtSpeedLooped();
void OnPlayAtSpeedCutPreview();
void OnSetPlaySpeed();
void OnPlaySpeedInc();
void OnPlaySpeedDec();

        // Moving track focus commands

void OnCursorUp();
void OnCursorDown();
void OnFirstTrack();
void OnLastTrack();

        // Selection-Editing Commands

void OnShiftUp();
void OnShiftDown();
void OnToggle();

void OnCursorLeft(const wxEvent * evt);
void OnCursorRight(const wxEvent * evt);
void OnSelExtendLeft(const wxEvent * evt);
void OnSelExtendRight(const wxEvent * evt);
void OnSelContractLeft(const wxEvent * evt);
void OnSelContractRight(const wxEvent * evt);

void OnCursorShortJumpLeft();
void OnCursorShortJumpRight();
void OnCursorLongJumpLeft();
void OnCursorLongJumpRight();
void OnSelSetExtendLeft();
void OnSelSetExtendRight();

void OnSetLeftSelection();
void OnSetRightSelection();

void OnSelToStart();
void OnSelToEnd();

void OnZeroCrossing();

void OnLockPlayRegion();
void OnUnlockPlayRegion();

double GetTime(const Track *t);
void OnSortTime();
void OnSortName();

void OnSnapToOff();
void OnSnapToNearest();
void OnSnapToPrior();
void OnFullScreen();

static void DoMacMinimize(AudacityProject *project);
void OnMacMinimize();
void OnMacMinimizeAll();
void OnMacZoom();
void OnMacBringAllToFront();

        // File Menu

void OnNew();
void OnOpen();
void OnClose();
void OnSave();
void OnSaveAs();
#ifdef USE_LIBVORBIS
   void OnSaveCompressed();
#endif

void OnCheckDependencies();

void OnExport();
void OnExportSelection();
void OnExportMultiple();
void OnExportLabels();
void OnExportMIDI();

void OnPreferences();

void OnPageSetup();
void OnPrint();

void OnExit();

        // Edit Menu

public:
void OnUndo();
void OnRedo();

void OnCut();
void OnSplitCut();
void OnCopy();

void OnPaste();
private:
bool HandlePasteText(); // Handle text paste (into active label), if any. Return true if pasted.
bool HandlePasteNothingSelected(); // Return true if nothing selected, regardless of paste result.
public:

void OnPasteNewLabel();
void OnPasteOver();
void OnTrim();

void OnDelete();
void OnSplitDelete();
void OnSilence();

void OnSplit();
void OnSplitNew();
void OnJoin();
void OnDisjoin();
void OnDuplicate();

void OnCutLabels();
void OnSplitCutLabels();
void OnCopyLabels();
void OnDeleteLabels();
void OnSplitDeleteLabels();
void OnSilenceLabels();
void OnSplitLabels();
void OnJoinLabels();
void OnDisjoinLabels();

void OnSelectAll();
void OnSelectNone();
#ifdef EXPERIMENTAL_SPECTRAL_EDITING
void OnToggleSpectralSelection();
void DoNextPeakFrequency(bool up);
void OnNextHigherPeakFrequency();
void OnNextLowerPeakFrequency();
#endif
void OnSelectCursorEnd();
void OnSelectStartCursor();
void OnSelectSyncLockSel();
void OnSelectAllTracks();

        // View Menu

void OnZoomIn();
void OnZoomOut();
// void OnZoomToggle();
void OnZoomNormal();
void OnZoomFit();
void OnZoomFitV();
void DoZoomFitV();
void OnZoomSel();
void OnGoSelStart();
void OnGoSelEnd();

void OnExpandAllTracks();
void OnCollapseAllTracks();

void OnMuteAllTracks();
void OnUnMuteAllTracks();

void OnShowClipping();

void OnHistory();

void OnKaraoke();
void OnMixerBoard();

void OnPlotSpectrum();
void OnContrast();

void OnShowTransportToolBar();
void OnShowDeviceToolBar();
void OnShowEditToolBar();
void OnShowMeterToolBar();
void OnShowRecordMeterToolBar();
void OnShowPlayMeterToolBar();
void OnShowMixerToolBar();
void OnShowSelectionToolBar();
#ifdef EXPERIMENTAL_SPECTRAL_EDITING
void OnShowSpectralSelectionToolBar();
#endif
void OnShowScrubbingToolBar();
void OnShowToolsToolBar();
void OnShowTranscriptionToolBar();
void OnResetToolBars();

        // Transport Menu

void OnSoundActivated();
void OnToggleSoundActivated();
void OnTogglePinnedHead();
void OnTogglePlayRecording();
void OnToggleSWPlaythrough();
#ifdef EXPERIMENTAL_AUTOMATED_INPUT_LEVEL_ADJUSTMENT
   void OnToogleAutomatedInputLevelAdjustment();
#endif
void OnRescanDevices();

// Import Submenu
void OnImport();
void OnImportLabels();
void OnImportMIDI();
void DoImportMIDI(const wxString &fileName);
void OnImportRaw();

void OnEditMetadata();
bool DoEditMetadata(const wxString &title, const wxString &shortUndoDescription, bool force);

void OnMixAndRender();
void OnMixAndRenderToNewTrack();
void HandleMixAndRender(bool toNewTrack);

private:
   SelectedRegion mRegionSave{};
public:
void OnSelectionSave();
void OnSelectionRestore();

void OnCursorTrackStart();
void OnCursorTrackEnd();
void OnCursorSelStart();
void OnCursorSelEnd();

void OnAlignNoSync(int index);
void OnAlign(int index);
void OnAlignMoveSel(int index);
void HandleAlign(int index, bool moveSel);
size_t mAlignLabelsCount;

#ifdef EXPERIMENTAL_SCOREALIGN
void OnScoreAlign();
#endif // EXPERIMENTAL_SCOREALIGN

// Tracks menu
void OnNewWaveTrack();
void OnNewStereoTrack();
void OnNewLabelTrack();
void OnNewTimeTrack();
void OnTimerRecord();
void OnRemoveTracks();
void OnSyncLock();
void OnAddLabel();
void OnAddLabelPlaying();
void DoEditLabels(LabelTrack *lt = nullptr, int index = -1);
void OnEditLabels();

        // Effect Menu

class OnEffectFlags
{
public:
   // No flags specified
   static const int kNone = 0x00;
   // Flag used to disable prompting for configuration parameteres.
   static const int kConfigured = 0x01;
   // Flag used to disable saving the state after processing.
   static const int kSkipState  = 0x02;
   // Flag used to disable "Repeat Last Effect"
   static const int kDontRepeatLast = 0x04;
};

bool OnEffect(const PluginID & ID, int flags = OnEffectFlags::kNone);
void OnRepeatLastEffect(int index);
void OnApplyChain();
void OnEditChains();
void OnStereoToMono(int index);
void OnManagePluginsMenu(EffectType Type);
void OnManageGenerators();
void OnManageEffects();
void OnManageAnalyzers();

        // Help Menu

void OnAbout();
void OnQuickHelp();
void OnManual();
void OnCheckForUpdates();
void OnShowLog();
void OnHelpWelcome();
void OnBenchmark();
#if defined(EXPERIMENTAL_CRASH_REPORT)
void OnCrashReport();
#endif
void OnScreenshot();
void OnAudioDeviceInfo();

       //

void OnSeparator();

      // Keyboard navigation

void NextOrPrevFrame(bool next);
void PrevFrame();
void NextFrame();

void PrevWindow();
void NextWindow();

void OnResample();

private:
void OnCursorLeft(bool shift, bool ctrl, bool keyup = false);
void OnCursorRight(bool shift, bool ctrl, bool keyup = false);
void OnCursorMove(bool forward, bool jump, bool longjump);
void OnBoundaryMove(bool left, bool boundaryContract);

// Handle small cursor and play head movements
void SeekLeftOrRight
(bool left, bool shift, bool ctrl, bool keyup,
 int snapToTime, bool mayAccelerateQuiet, bool mayAccelerateAudio,
 double quietSeekStepPositive, bool quietStepIsPixels,
 double audioSeekStepPositive, bool audioStepIsPixels);

// Helper for moving by keyboard with snap-to-grid enabled
double GridMove(double t, int minPix);

// Make sure we return to "public" for subsequent declarations in Project.h.
public:

#endif



