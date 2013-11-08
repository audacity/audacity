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

#ifdef EFFECT_CATEGORIES

/** Generate submenus for the categories that contain more than one effect
    and return the effects from the categories that do not contain more than
    submenuThreshold effects so the caller can add them to the current menu. */
EffectSet CreateEffectSubmenus(CommandManager* c, 
                               const CategorySet& categories, int flags,
                               unsigned submenuThreshold = 1);

/** Add the set of effects to the current menu. */
void AddEffectsToMenu(CommandManager* c, const EffectSet& effects);

#endif

void CreateRecentFilesMenu(CommandManager *c);
void ModifyUndoMenuItems();
void ModifyToolbarMenus();
// Calls ModifyToolbarMenus() on all projects
void ModifyAllProjectToolbarMenus();

int GetFocusedFrame();
wxUint32 GetUpdateFlags();

double NearestZeroCrossing(double t0);

public:
//Adds label and returns index of label in labeltrack.
int DoAddLabel(double left, double right);

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

bool MakeReadyToPlay(); // Helper function that sets button states etc.
void OnPlayStop();
void OnPlayStopSelect();
void OnPlayOneSecond();
void OnPlayToSelection();
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
void OnSetPlaySpeed();
void OnPlaySpeedInc();
void OnPlaySpeedDec();

        // Selection-Editing Commands

void OnCursorUp();
void OnShiftUp();
void OnCursorDown();
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

double GetTime(Track *t);
void OnSortTime();
void OnSortName();

void OnSnapToOff();
void OnSnapToNearest();
void OnSnapToPrior();
void OnFullScreen();

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

#ifdef EXPERIMENTAL_FTP
   void OnUpload();
#endif

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
void OnSelectCursorEnd();
void OnSelectStartCursor();
void OnSelectSyncLockSel();
void OnSelectAllTracks();

        // View Menu

void OnZoomIn();
void OnZoomOut();
void OnZoomToggle();
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
void OnShowMixerToolBar();
void OnShowSelectionToolBar();
void OnShowToolsToolBar();
void OnShowTranscriptionToolBar();
void OnResetToolBars();
void OnSimplifiedView();

        // Transport Menu

void OnSoundActivated();
void OnToggleSoundActivated();
void OnTogglePlayRecording();
void OnToggleSWPlaythrough();
#ifdef AUTOMATED_INPUT_LEVEL_ADJUSTMENT
   void OnToogleAutomatedInputLevelAdjustment();
#endif
void OnRescanDevices();

// Import Submenu
void OnImport();
void OnImportLabels();
void OnImportMIDI();
void OnImportRaw();

void OnEditMetadata();

void OnMixAndRender();
void OnMixAndRenderToNewTrack();
void HandleMixAndRender(bool toNewTrack);

private:
double mSel0save;
double mSel1save;
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
void OnEditLabels();

        // Effect Menu

bool OnEffect(int type, Effect * f, wxString params = wxEmptyString, bool saveState = true);
void OnEffect(int type, int index);
void OnGenerateEffect(int index);
void OnGeneratePlugin(int index);
void OnRepeatLastEffect(int index);
void OnProcessAny(int index);
void OnProcessEffect(int index);
void OnProcessPlugin(int index);
void OnAnalyzeEffect(int index);
void OnAnalyzePlugin(int index);
void OnApplyChain();
void OnEditChains();
void OnStereoToMono(int index);
wxString BuildCleanFileName(wxString fileName, wxString extension);

        // Help Menu

void OnAbout();
void OnQuickHelp();
void OnManual();
void OnShowLog();
void OnHelpWelcome();
void OnBenchmark();
void OnScreenshot();
void OnAudioDeviceInfo();

       //

void OnSeparator();

      // Keyboard navigation

void PrevFrame();
void NextFrame();

void OnResample();

// Make sure we return to "public" for subsequent declarations in Project.h.
public:

#endif



