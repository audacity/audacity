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

public:
// If checkActive, do not do complete flags testing on an
// inactive project as it is needlessly expensive.
CommandFlag GetUpdateFlags(bool checkActive = false);

//Adds label and returns index of label in labeltrack.
int DoAddLabel(const SelectedRegion& region, bool preserveFocus = false);

private:
double NearestZeroCrossing(double t0);

        // Selecting a tool from the keyboard

void SetTool(int tool);
void OnSelectTool(const CommandContext &);
void OnZoomTool(const CommandContext &);
void OnEnvelopeTool(const CommandContext &);
void OnTimeShiftTool(const CommandContext &);
void OnDrawTool(const CommandContext &);
void OnMultiTool(const CommandContext &);

void OnNextTool(const CommandContext &);
void OnPrevTool(const CommandContext &);

public:
        // Audio I/O Commands

void OnStop(const CommandContext &);
void OnPause(const CommandContext &);
void OnRecord(const CommandContext &);
void OnRecord2ndChoice(const CommandContext &);
void OnStopSelect(const CommandContext &);
void OnSkipStart(const CommandContext &);
void OnSkipEnd(const CommandContext &);
void OnSeekLeftShort(const CommandContext &);
void OnSeekRightShort(const CommandContext &);
void OnSeekLeftLong(const CommandContext &);
void OnSeekRightLong(const CommandContext &);

        // Different posibilities for playing sound

bool MakeReadyToPlay(bool loop = false, bool cutpreview = false); // Helper function that sets button states etc.
void OnPlayStop(const CommandContext &);
bool DoPlayStopSelect(bool click, bool shift);
void OnPlayStopSelect(const CommandContext &);
void OnPlayOneSecond(const CommandContext &);
void OnPlayToSelection(const CommandContext &);
void OnPlayBeforeSelectionStart(const CommandContext &);
void OnPlayAfterSelectionStart(const CommandContext &);
void OnPlayBeforeSelectionEnd(const CommandContext &);
void OnPlayAfterSelectionEnd(const CommandContext &);
void OnPlayBeforeAndAfterSelectionStart(const CommandContext &);
void OnPlayBeforeAndAfterSelectionEnd(const CommandContext &);
void OnPlayLooped(const CommandContext &);
void OnPlayCutPreview(const CommandContext &);

        // Wave track control

void OnTrackPan(const CommandContext &);
void OnTrackPanLeft(const CommandContext &);
void OnTrackPanRight(const CommandContext &);
void OnTrackGain(const CommandContext &);
void OnTrackGainInc(const CommandContext &);
void OnTrackGainDec(const CommandContext &);
void OnTrackMenu(const CommandContext &);
void OnTrackMute(const CommandContext &);
void OnTrackSolo(const CommandContext &);
void OnTrackClose(const CommandContext &);
void OnTrackMoveUp(const CommandContext &);
void OnTrackMoveDown(const CommandContext &);
void OnTrackMoveTop(const CommandContext &);
void OnTrackMoveBottom(const CommandContext &);

enum MoveChoice { OnMoveUpID, OnMoveDownID, OnMoveTopID, OnMoveBottomID };
void MoveTrack(Track* target, MoveChoice choice);

        // Device control
void OnInputDevice(const CommandContext &);
void OnOutputDevice(const CommandContext &);
void OnAudioHost(const CommandContext &);
void OnInputChannels(const CommandContext &);

        // Mixer control

void OnOutputGain(const CommandContext &);
void OnInputGain(const CommandContext &);
void OnOutputGainInc(const CommandContext &);
void OnOutputGainDec(const CommandContext &);
void OnInputGainInc(const CommandContext &);
void OnInputGainDec(const CommandContext &);

        // Transcription control

void OnPlayAtSpeed(const CommandContext &);
void OnPlayAtSpeedLooped(const CommandContext &);
void OnPlayAtSpeedCutPreview(const CommandContext &);
void OnSetPlaySpeed(const CommandContext &);
void OnPlaySpeedInc(const CommandContext &);
void OnPlaySpeedDec(const CommandContext &);

        // Moving track focus commands

void OnPrevTrack( bool shift );
void OnNextTrack( bool shift );
void OnCursorUp(const CommandContext &);
void OnCursorDown(const CommandContext &);
void OnFirstTrack(const CommandContext &);
void OnLastTrack(const CommandContext &);

        // Selection-Editing Commands

void OnShiftUp(const CommandContext &);
void OnShiftDown(const CommandContext &);
void OnToggle(const CommandContext &);

void HandleListSelection(Track *t, bool shift, bool ctrl, bool modifyState);

void OnCursorLeft(const CommandContext &);
void OnCursorRight(const CommandContext &);
void OnSelExtendLeft(const CommandContext &);
void OnSelExtendRight(const CommandContext &);
void OnSelContractLeft(const CommandContext &);
void OnSelContractRight(const CommandContext &);

public:
static double OnClipMove
   (ViewInfo &viewInfo, Track *track,
    TrackList &trackList, bool syncLocked, bool right);

void DoClipLeftOrRight(bool right, bool keyUp );
void OnClipLeft(const CommandContext &);
void OnClipRight(const CommandContext &);

void OnCursorShortJumpLeft(const CommandContext &);
void OnCursorShortJumpRight(const CommandContext &);
void OnCursorLongJumpLeft(const CommandContext &);
void OnCursorLongJumpRight(const CommandContext &);
void OnSelSetExtendLeft(const CommandContext &);
void OnSelSetExtendRight(const CommandContext &);

void OnSetLeftSelection(const CommandContext &);
void OnSetRightSelection(const CommandContext &);

void OnSelToStart(const CommandContext &);
void OnSelToEnd(const CommandContext &);

void OnMoveToNextLabel(const CommandContext &);
void OnMoveToPrevLabel(const CommandContext &);
void OnMoveToLabel(bool next);

void OnZeroCrossing(const CommandContext &);

void OnLockPlayRegion(const CommandContext &);
void OnUnlockPlayRegion(const CommandContext &);

double GetTime(const Track *t);
void OnSortTime(const CommandContext &);
void OnSortName(const CommandContext &);

void OnSnapToOff(const CommandContext &);
void OnSnapToNearest(const CommandContext &);
void OnSnapToPrior(const CommandContext &);
void OnFullScreen(const CommandContext &);

static void DoMacMinimize(AudacityProject *project);
void OnMacMinimize(const CommandContext &);
void OnMacMinimizeAll(const CommandContext &);
void OnMacZoom(const CommandContext &);
void OnMacBringAllToFront(const CommandContext &);

        // File Menu

void OnNew(const CommandContext &);
void OnOpen(const CommandContext &);
void OnClose(const CommandContext &);
void OnSave(const CommandContext &);
void OnSaveAs(const CommandContext &);
#ifdef USE_LIBVORBIS
   void OnSaveCompressed(const CommandContext &);
#endif

void OnCheckDependencies(const CommandContext &);

void OnExport(const wxString & Format);
void OnExportAudio(const CommandContext &);
void OnExportMp3(const CommandContext &);
void OnExportWav(const CommandContext &);
void OnExportOgg(const CommandContext &);
void OnExportSelection(const CommandContext &);
void OnExportMultiple(const CommandContext &);
void OnExportLabels(const CommandContext &);
void OnExportMIDI(const CommandContext &);

void OnPreferences(const CommandContext &);

void OnPageSetup(const CommandContext &);
void OnPrint(const CommandContext &);

void OnExit(const CommandContext &);

        // Edit Menu

public:
void OnUndo(const CommandContext &);
void OnRedo(const CommandContext &);

private:
static void FinishCopy(const Track *n, Track *dest);
static void FinishCopy(const Track *n, Track::Holder &&dest, TrackList &list);

public:
void OnCut(const CommandContext &);
void OnSplitCut(const CommandContext &);
void OnCopy(const CommandContext &);

void OnPaste(const CommandContext &);
private:
bool HandlePasteText(); // Handle text paste (into active label), if any. Return true if pasted.
bool HandlePasteNothingSelected(); // Return true if nothing selected, regardless of paste result.
public:

void OnPasteNewLabel(const CommandContext &);
void OnPasteOver(const CommandContext &);
void OnTrim(const CommandContext &);

void OnDelete(const CommandContext &);
void OnSplitDelete(const CommandContext &);
void OnSilence(const CommandContext &);

void OnSplit(const CommandContext &);
void OnSplitNew(const CommandContext &);
void OnJoin(const CommandContext &);
void OnDisjoin(const CommandContext &);
void OnDuplicate(const CommandContext &);

void OnCutLabels(const CommandContext &);
void OnSplitCutLabels(const CommandContext &);
void OnCopyLabels(const CommandContext &);
void OnDeleteLabels(const CommandContext &);
void OnSplitDeleteLabels(const CommandContext &);
void OnSilenceLabels(const CommandContext &);
void OnSplitLabels(const CommandContext &);
void OnJoinLabels(const CommandContext &);
void OnDisjoinLabels(const CommandContext &);

void OnSelectTimeAndTracks(bool bAllTime, bool bAllTracks);
void OnSelectAllTime(const CommandContext &);
void OnSelectAllTracks(const CommandContext &);
void OnSelectAll(const CommandContext &);
void OnSelectSomething(const CommandContext &);
void OnSelectNone(const CommandContext &);
private:
int CountSelectedWaveTracks();
int CountSelectedTracks();
public:
#ifdef EXPERIMENTAL_SPECTRAL_EDITING
// For toggling of spectral seletion
double mLastF0;
double mLastF1;
void OnToggleSpectralSelection(const CommandContext &);
void DoNextPeakFrequency(bool up);
void OnNextHigherPeakFrequency(const CommandContext &);
void OnNextLowerPeakFrequency(const CommandContext &);
#endif
void OnSelectCursorEnd(const CommandContext &);
void OnSelectStartCursor(const CommandContext &);
void OnSelectPrevClipBoundaryToCursor(const CommandContext &);
void OnSelectCursorToNextClipBoundary(const CommandContext &);
void OnSelectClipBoundary(bool next);
struct FoundTrack {
   const WaveTrack* waveTrack{};
   int trackNumber{};
   bool channel{};

   wxString ComposeTrackName() const;
};
struct FoundClip : FoundTrack {
   bool found{};
   double startTime{};
   double endTime{};
   int index{};
};
FoundClip FindNextClip(const WaveTrack* wt, double t0, double t1);
FoundClip FindPrevClip(const WaveTrack* wt, double t0, double t1);
int FindClips(double t0, double t1, bool next, std::vector<FoundClip>& results);
bool ChannelsHaveSameClipBoundaries(const WaveTrack* wt);
void OnSelectPrevClip(const CommandContext &);
void OnSelectNextClip(const CommandContext &);
void OnSelectClip(bool next);
void OnSelectCursorStoredCursor(const CommandContext &);
void OnSelectSyncLockSel(const CommandContext &);

void OnZoomIn(const CommandContext &);
void OnZoomOut(const CommandContext &);
void OnZoomToggle(const CommandContext &);
void OnZoomNormal(const CommandContext &);
void OnZoomFit(const CommandContext &);
void OnZoomFitV(const CommandContext &);
void DoZoomFitV();
void OnZoomSel(const CommandContext &);
void OnGoSelStart(const CommandContext &);
void OnGoSelEnd(const CommandContext &);

void OnExpandAllTracks(const CommandContext &);
void OnCollapseAllTracks(const CommandContext &);

void OnPanTracks(float PanValue);
void OnPanLeft(const CommandContext &);
void OnPanRight(const CommandContext &);
void OnPanCenter(const CommandContext &);

void OnMuteAllTracks(const CommandContext &);
void OnUnMuteAllTracks(const CommandContext &);

void OnShowClipping(const CommandContext &);
void OnShowExtraMenus(const CommandContext &);

void OnHistory(const CommandContext &);

void OnKaraoke(const CommandContext &);
void OnMixerBoard(const CommandContext &);

void OnPlotSpectrum(const CommandContext &);
void OnContrast(const CommandContext &);

void OnShowTransportToolBar(const CommandContext &);
void OnShowDeviceToolBar(const CommandContext &);
void OnShowEditToolBar(const CommandContext &);
void OnShowMeterToolBar(const CommandContext &);
void OnShowRecordMeterToolBar(const CommandContext &);
void OnShowPlayMeterToolBar(const CommandContext &);
void OnShowMixerToolBar(const CommandContext &);
void OnShowSelectionToolBar(const CommandContext &);
#ifdef EXPERIMENTAL_SPECTRAL_EDITING
void OnShowSpectralSelectionToolBar(const CommandContext &);
#endif
void OnShowScrubbingToolBar(const CommandContext &);
void OnShowToolsToolBar(const CommandContext &);
void OnShowTranscriptionToolBar(const CommandContext &);
void OnResetToolBars(const CommandContext &);

        // Transport Menu

void OnSoundActivated(const CommandContext &);
void OnToggleSoundActivated(const CommandContext &);
void OnTogglePinnedHead(const CommandContext &);
void OnTogglePlayRecording(const CommandContext &);
void OnToggleSWPlaythrough(const CommandContext &);
#ifdef EXPERIMENTAL_AUTOMATED_INPUT_LEVEL_ADJUSTMENT
   void OnToggleAutomatedInputLevelAdjustment(const CommandContext &);
#endif
void OnRescanDevices(const CommandContext &);

// Import Submenu
void OnImport(const CommandContext &);
void OnImportLabels(const CommandContext &);
void OnImportMIDI(const CommandContext &);

// return null on failure; if success, return the given project, or a NEW
// one, if the given was null; create no NEW project if failure
static AudacityProject *DoImportMIDI(
   AudacityProject *pProject, const wxString &fileName);

void OnImportRaw(const CommandContext &);

void OnEditMetadata(const CommandContext &);
bool DoEditMetadata(const wxString &title, const wxString &shortUndoDescription, bool force);

void OnMixAndRender(const CommandContext &);
void OnMixAndRenderToNewTrack(const CommandContext &);
void HandleMixAndRender(bool toNewTrack);

private:
   SelectedRegion mRegionSave{};
   bool mCursorPositionHasBeenStored{false};
   double mCursorPositionStored;
public:
void OnSelectionSave(const CommandContext &);
void OnSelectionRestore(const CommandContext &);
void OnCursorPositionStore(const CommandContext &);

void OnCursorTrackStart(const CommandContext &);
void OnCursorTrackEnd(const CommandContext &);
void OnCursorSelStart(const CommandContext &);
void OnCursorSelEnd(const CommandContext &);
   struct FoundClipBoundary : FoundTrack {
   int nFound{};    // 0, 1, or 2
   double time{};
   int index1{};
   bool clipStart1{};
   int index2{};
   bool clipStart2{};
};
FoundClipBoundary FindNextClipBoundary(const WaveTrack* wt, double time);
FoundClipBoundary FindPrevClipBoundary(const WaveTrack* wt, double time);
double AdjustForFindingStartTimes(const std::vector<const WaveClip*>& clips, double time);
double AdjustForFindingEndTimes(const std::vector<const WaveClip*>& clips, double time);
int FindClipBoundaries(double time, bool next, std::vector<FoundClipBoundary>& results);
void OnCursorNextClipBoundary(const CommandContext &);
void OnCursorPrevClipBoundary(const CommandContext &);
void OnCursorClipBoundary(bool next);
static wxString ClipBoundaryMessage(const std::vector<FoundClipBoundary>& results);

void OnAlignNoSync(const CommandContext &);
void OnAlign(const CommandContext &);
//void OnAlignMoveSel(int index);
void HandleAlign(int index, bool moveSel);
size_t mAlignLabelsCount;

#ifdef EXPERIMENTAL_SCOREALIGN
void OnScoreAlign(const CommandContext &);
#endif // EXPERIMENTAL_SCOREALIGN

// Tracks menu
void OnNewWaveTrack(const CommandContext &);
void OnNewStereoTrack(const CommandContext &);
void OnNewLabelTrack(const CommandContext &);
void OnNewTimeTrack(const CommandContext &);
void OnTimerRecord(const CommandContext &);
void OnRemoveTracks(const CommandContext &);
void OnMoveSelectionWithTracks(const CommandContext &);
void OnSyncLock(const CommandContext &);
void OnAddLabel(const CommandContext &);
void OnAddLabelPlaying(const CommandContext &);
void DoEditLabels(LabelTrack *lt = nullptr, int index = -1);
void OnEditLabels(const CommandContext &);
void OnToggleTypeToCreateLabel(const CommandContext &);

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

bool DoEffect(const PluginID & ID, int flags);
void OnEffect(const CommandContext &);
void OnRepeatLastEffect(const CommandContext &);
void OnApplyChain(const CommandContext &);
void OnEditChains(const CommandContext &);
void OnStereoToMono(const CommandContext &);
void OnManagePluginsMenu(EffectType Type);
static void RebuildAllMenuBars();
void OnManageGenerators(const CommandContext &);
void OnManageEffects(const CommandContext &);
void OnManageAnalyzers(const CommandContext &);



        // Help Menu

void OnAbout(const CommandContext &);
void OnQuickHelp(const CommandContext &);
void OnManual(const CommandContext &);
void OnCheckForUpdates(const CommandContext &);
void MayCheckForUpdates();
void OnShowLog(const CommandContext &);
void OnHelpWelcome(const CommandContext &);
void OnBenchmark(const CommandContext &);
#if defined(EXPERIMENTAL_CRASH_REPORT)
void OnCrashReport(const CommandContext &);
#endif
void OnSimulateRecordingErrors(const CommandContext &);
void OnDetectUpstreamDropouts(const CommandContext &);
void OnScreenshot(const CommandContext &);
void OnAudioDeviceInfo(const CommandContext &);
#ifdef EXPERIMENTAL_MIDI_OUT
void OnMidiDeviceInfo(const CommandContext &);
#endif

       //

void OnSeparator(const CommandContext &);

      // Keyboard navigation

void NextOrPrevFrame(bool next);
void PrevFrame(const CommandContext &);
void NextFrame(const CommandContext &);

void PrevWindow(const CommandContext &);
void NextWindow(const CommandContext &);

void OnResample(const CommandContext &);

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



