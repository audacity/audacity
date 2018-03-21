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
void PopulateMacrosMenu( CommandManager* c, CommandFlag flags );
void PopulateEffectsMenu(CommandManager *c, EffectType type,
                         CommandFlag batchflags, CommandFlag realflags);
void AddEffectMenuItems(CommandManager *c,
                        std::vector<const PluginDescriptor*> & plugs,
                        CommandFlag batchflags, CommandFlag realflags, bool isDefault);
void AddEffectMenuItemGroup(CommandManager *c, const wxArrayString & names,
                            const std::vector<bool> &vHasDialog,
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
int DialogForLabelName(const wxString& initialValue, wxString& value);

private:
double NearestZeroCrossing(double t0);

        // Selecting a tool from the keyboard

void SetTool(int tool);
void OnSelectTool(const CommandContext &context );
void OnZoomTool(const CommandContext &context );
void OnEnvelopeTool(const CommandContext &context );
void OnTimeShiftTool(const CommandContext &context );
void OnDrawTool(const CommandContext &context );
void OnMultiTool(const CommandContext &context );

void OnNextTool(const CommandContext &context );
void OnPrevTool(const CommandContext &context );

public:
        // Audio I/O Commands

void OnStop(const CommandContext &context );
void OnPause(const CommandContext &context );
void OnRecord(const CommandContext &context );
void OnRecord2ndChoice(const CommandContext &context );
void OnStopSelect(const CommandContext &context );
void OnSkipStart(const CommandContext &context );
void OnSkipEnd(const CommandContext &context );
void OnSeekLeftShort(const CommandContext &context );
void OnSeekRightShort(const CommandContext &context );
void OnSeekLeftLong(const CommandContext &context );
void OnSeekRightLong(const CommandContext &context );

        // Different posibilities for playing sound

bool MakeReadyToPlay(bool loop = false, bool cutpreview = false); // Helper function that sets button states etc.
void OnPlayStop(const CommandContext &context );
bool DoPlayStopSelect(bool click, bool shift);
void OnPlayStopSelect(const CommandContext &context );
void OnPlayOneSecond(const CommandContext &context );
void OnPlayToSelection(const CommandContext &context );
void OnPlayBeforeSelectionStart(const CommandContext &context );
void OnPlayAfterSelectionStart(const CommandContext &context );
void OnPlayBeforeSelectionEnd(const CommandContext &context );
void OnPlayAfterSelectionEnd(const CommandContext &context );
void OnPlayBeforeAndAfterSelectionStart(const CommandContext &context );
void OnPlayBeforeAndAfterSelectionEnd(const CommandContext &context );
void OnPlayLooped(const CommandContext &context );
void OnPlayCutPreview(const CommandContext &context );

        // Wave track control

void OnTrackPan(const CommandContext &context );
void OnTrackPanLeft(const CommandContext &context );
void OnTrackPanRight(const CommandContext &context );
void OnTrackGain(const CommandContext &context );
void OnTrackGainInc(const CommandContext &context );
void OnTrackGainDec(const CommandContext &context );
void OnTrackMenu(const CommandContext &context );
void OnTrackMute(const CommandContext &context );
void OnTrackSolo(const CommandContext &context );
void OnTrackClose(const CommandContext &context );
void OnTrackMoveUp(const CommandContext &context );
void OnTrackMoveDown(const CommandContext &context );
void OnTrackMoveTop(const CommandContext &context );
void OnTrackMoveBottom(const CommandContext &context );

enum MoveChoice { OnMoveUpID, OnMoveDownID, OnMoveTopID, OnMoveBottomID };
void MoveTrack(Track* target, MoveChoice choice);

        // Device control
void OnInputDevice(const CommandContext &context );
void OnOutputDevice(const CommandContext &context );
void OnAudioHost(const CommandContext &context );
void OnInputChannels(const CommandContext &context );

        // Mixer control

void OnOutputGain(const CommandContext &context );
void OnInputGain(const CommandContext &context );
void OnOutputGainInc(const CommandContext &context );
void OnOutputGainDec(const CommandContext &context );
void OnInputGainInc(const CommandContext &context );
void OnInputGainDec(const CommandContext &context );

        // Transcription control

void OnPlayAtSpeed(const CommandContext &context );
void OnPlayAtSpeedLooped(const CommandContext &context );
void OnPlayAtSpeedCutPreview(const CommandContext &context );
void OnSetPlaySpeed(const CommandContext &context );
void OnPlaySpeedInc(const CommandContext &context );
void OnPlaySpeedDec(const CommandContext &context );

        // Moving track focus commands

void OnPrevTrack( bool shift );
void OnNextTrack( bool shift );
void OnCursorUp(const CommandContext &context );
void OnCursorDown(const CommandContext &context );
void OnFirstTrack(const CommandContext &context );
void OnLastTrack(const CommandContext &context );

        // Selection-Editing Commands

void OnShiftUp(const CommandContext &context );
void OnShiftDown(const CommandContext &context );
void OnToggle(const CommandContext &context );

void HandleListSelection(Track *t, bool shift, bool ctrl, bool modifyState);

void OnCursorLeft(const CommandContext &context );
void OnCursorRight(const CommandContext &context );
void OnSelExtendLeft(const CommandContext &context );
void OnSelExtendRight(const CommandContext &context );
void OnSelContractLeft(const CommandContext &context );
void OnSelContractRight(const CommandContext &context );

public:
static double OnClipMove
   (ViewInfo &viewInfo, Track *track,
    TrackList &trackList, bool syncLocked, bool right);

void DoClipLeftOrRight(bool right, bool keyUp );
void OnClipLeft(const CommandContext &context );
void OnClipRight(const CommandContext &context );

void OnCursorShortJumpLeft(const CommandContext &context );
void OnCursorShortJumpRight(const CommandContext &context );
void OnCursorLongJumpLeft(const CommandContext &context );
void OnCursorLongJumpRight(const CommandContext &context );
void OnSelSetExtendLeft(const CommandContext &context );
void OnSelSetExtendRight(const CommandContext &context );

void OnSetLeftSelection(const CommandContext &context );
void OnSetRightSelection(const CommandContext &context );

void OnSelToStart(const CommandContext &context );
void OnSelToEnd(const CommandContext &context );

void OnMoveToNextLabel(const CommandContext &context );
void OnMoveToPrevLabel(const CommandContext &context );
void OnMoveToLabel(bool next);

void OnZeroCrossing(const CommandContext &context );

void OnLockPlayRegion(const CommandContext &context );
void OnUnlockPlayRegion(const CommandContext &context );

double GetTime(const Track *t);
void OnSortTime(const CommandContext &context );
void OnSortName(const CommandContext &context );

void OnSnapToOff(const CommandContext &context );
void OnSnapToNearest(const CommandContext &context );
void OnSnapToPrior(const CommandContext &context );
void OnFullScreen(const CommandContext &context );

static void DoMacMinimize(AudacityProject *project);
void OnMacMinimize(const CommandContext &context );
void OnMacMinimizeAll(const CommandContext &context );
void OnMacZoom(const CommandContext &context );
void OnMacBringAllToFront(const CommandContext &context );

        // File Menu

void OnNew(const CommandContext &context );
void OnOpen(const CommandContext &context );
void OnClose(const CommandContext &context );
void OnSave(const CommandContext &context );
void OnSaveAs(const CommandContext &context );
#ifdef USE_LIBVORBIS
   void OnSaveCompressed(const CommandContext &context );
#endif

void OnCheckDependencies(const CommandContext &context );

void OnExport(const wxString & Format);
void OnExportAudio(const CommandContext &context );
void OnExportMp3(const CommandContext &context );
void OnExportWav(const CommandContext &context );
void OnExportOgg(const CommandContext &context );
void OnExportSelection(const CommandContext &context );
void OnExportMultiple(const CommandContext &context );
void OnExportLabels(const CommandContext &context );
void OnExportMIDI(const CommandContext &context );

void OnPreferences(const CommandContext &context );
void OnReloadPreferences(const CommandContext &context );

void OnPageSetup(const CommandContext &context );
void OnPrint(const CommandContext &context );

void OnExit(const CommandContext &context );

        // Edit Menu

public:
void OnUndo(const CommandContext &context );
void OnRedo(const CommandContext &context );

private:
static void FinishCopy(const Track *n, Track *dest);
static void FinishCopy(const Track *n, Track::Holder &&dest, TrackList &list);

public:
void OnCut(const CommandContext &context );
void OnSplitCut(const CommandContext &context );
void OnCopy(const CommandContext &context );

void OnPaste(const CommandContext &context );
private:
bool HandlePasteText(); // Handle text paste (into active label), if any. Return true if pasted.
bool HandlePasteNothingSelected(); // Return true if nothing selected, regardless of paste result.
public:

void OnPasteNewLabel(const CommandContext &context );
void OnPasteOver(const CommandContext &context );
void OnTrim(const CommandContext &context );

void OnDelete(const CommandContext &context );
void OnSplitDelete(const CommandContext &context );
void OnSilence(const CommandContext &context );

void OnSplit(const CommandContext &context );
void OnSplitNew(const CommandContext &context );
void OnJoin(const CommandContext &context );
void OnDisjoin(const CommandContext &context );
void OnDuplicate(const CommandContext &context );

void OnCutLabels(const CommandContext &context );
void OnSplitCutLabels(const CommandContext &context );
void OnCopyLabels(const CommandContext &context );
void OnDeleteLabels(const CommandContext &context );
void OnSplitDeleteLabels(const CommandContext &context );
void OnSilenceLabels(const CommandContext &context );
void OnSplitLabels(const CommandContext &context );
void OnJoinLabels(const CommandContext &context );
void OnDisjoinLabels(const CommandContext &context );

void OnSelectTimeAndTracks(bool bAllTime, bool bAllTracks);
void OnSelectAllTime(const CommandContext &context );
void OnSelectAllTracks(const CommandContext &context );
void OnSelectAll(const CommandContext &context );
void OnSelectSomething(const CommandContext &context );
void OnSelectNone(const CommandContext &context );
private:
int CountSelectedWaveTracks();
int CountSelectedTracks();
public:
#ifdef EXPERIMENTAL_SPECTRAL_EDITING
// For toggling of spectral seletion
double mLastF0;
double mLastF1;
void OnToggleSpectralSelection(const CommandContext &context );
void DoNextPeakFrequency(bool up);
void OnNextHigherPeakFrequency(const CommandContext &context );
void OnNextLowerPeakFrequency(const CommandContext &context );
#endif
void OnSelectCursorEnd(const CommandContext &context );
void OnSelectStartCursor(const CommandContext &context );
void OnSelectTrackStartToEnd(const CommandContext &context );
void OnSelectPrevClipBoundaryToCursor(const CommandContext &context );
void OnSelectCursorToNextClipBoundary(const CommandContext &context );
void OnSelectClipBoundary(bool next);
struct FoundTrack {
   const WaveTrack* waveTrack{};
   int trackNum{};
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
void OnSelectPrevClip(const CommandContext &context );
void OnSelectNextClip(const CommandContext &context );
void OnSelectClip(bool next);
void OnSelectCursorStoredCursor(const CommandContext &context );
void OnSelectSyncLockSel(const CommandContext &context );

void OnZoomIn(const CommandContext &context );
void OnZoomOut(const CommandContext &context );
void OnZoomToggle(const CommandContext &context );
void OnZoomNormal(const CommandContext &context );
void OnZoomFit(const CommandContext &context );
void OnZoomFitV(const CommandContext &context );
void DoZoomFitV();
void OnZoomSel(const CommandContext &context );
void OnGoSelStart(const CommandContext &context );
void OnGoSelEnd(const CommandContext &context );

void OnExpandAllTracks(const CommandContext &context );
void OnCollapseAllTracks(const CommandContext &context );

void OnPanTracks(float PanValue);
void OnPanLeft(const CommandContext &context );
void OnPanRight(const CommandContext &context );
void OnPanCenter(const CommandContext &context );

void OnMuteAllTracks(const CommandContext &context );
void OnUnmuteAllTracks(const CommandContext &context );

void OnShowClipping(const CommandContext &context );
void OnShowExtraMenus(const CommandContext &context );

void OnHistory(const CommandContext &context );

void OnKaraoke(const CommandContext &context );
void OnMixerBoard(const CommandContext &context );

void OnPlotSpectrum(const CommandContext &context );
void OnContrast(const CommandContext &context );

void OnShowTransportToolBar(const CommandContext &context );
void OnShowDeviceToolBar(const CommandContext &context );
void OnShowEditToolBar(const CommandContext &context );
void OnShowMeterToolBar(const CommandContext &context );
void OnShowRecordMeterToolBar(const CommandContext &context );
void OnShowPlayMeterToolBar(const CommandContext &context );
void OnShowMixerToolBar(const CommandContext &context );
void OnShowSelectionToolBar(const CommandContext &context );
#ifdef EXPERIMENTAL_SPECTRAL_EDITING
void OnShowSpectralSelectionToolBar(const CommandContext &context );
#endif
void OnShowScrubbingToolBar(const CommandContext &context );
void OnShowToolsToolBar(const CommandContext &context );
void OnShowTranscriptionToolBar(const CommandContext &context );
void OnResetToolBars(const CommandContext &context );

#if defined(EXPERIMENTAL_EFFECTS_RACK)
void OnShowEffectsRack(const CommandContext &context );
#endif

        // Transport Menu

void OnSoundActivated(const CommandContext &context );
void OnToggleSoundActivated(const CommandContext &context );
void OnTogglePinnedHead(const CommandContext &context );
void OnTogglePlayRecording(const CommandContext &context );
void OnToggleSWPlaythrough(const CommandContext &context );
#ifdef EXPERIMENTAL_AUTOMATED_INPUT_LEVEL_ADJUSTMENT
   void OnToggleAutomatedInputLevelAdjustment(const CommandContext &context );
#endif
void OnRescanDevices(const CommandContext &context );

// Import Submenu
void OnImport(const CommandContext &context );
void OnImportLabels(const CommandContext &context );
void OnImportMIDI(const CommandContext &context );

// return null on failure; if success, return the given project, or a NEW
// one, if the given was null; create no NEW project if failure
static AudacityProject *DoImportMIDI(
   AudacityProject *pProject, const wxString &fileName);

void OnImportRaw(const CommandContext &context );

void OnEditMetadata(const CommandContext &context );
bool DoEditMetadata(const wxString &title, const wxString &shortUndoDescription, bool force);

void OnMixAndRender(const CommandContext &context );
void OnMixAndRenderToNewTrack(const CommandContext &context );
void HandleMixAndRender(bool toNewTrack);

private:
   SelectedRegion mRegionSave{};
   bool mCursorPositionHasBeenStored{false};
   double mCursorPositionStored;
public:
void OnSelectionSave(const CommandContext &context );
void OnSelectionRestore(const CommandContext &context );
void OnCursorPositionStore(const CommandContext &context );

void OnCursorTrackStart(const CommandContext &context );
void OnCursorTrackEnd(const CommandContext &context );
void OnCursorSelStart(const CommandContext &context );
void OnCursorSelEnd(const CommandContext &context );
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
void OnCursorNextClipBoundary(const CommandContext &context );
void OnCursorPrevClipBoundary(const CommandContext &context );
void OnCursorClipBoundary(bool next);
static wxString ClipBoundaryMessage(const std::vector<FoundClipBoundary>& results);

void OnAlignNoSync(const CommandContext &context );
void OnAlign(const CommandContext &context );
//void OnAlignMoveSel(int index);
void HandleAlign(int index, bool moveSel);
size_t mAlignLabelsCount;

#ifdef EXPERIMENTAL_SCOREALIGN
void OnScoreAlign(const CommandContext &context );
#endif // EXPERIMENTAL_SCOREALIGN

// Tracks menu
void OnNewWaveTrack(const CommandContext &context );
void OnNewStereoTrack(const CommandContext &context );
void OnNewLabelTrack(const CommandContext &context );
void OnNewTimeTrack(const CommandContext &context );
void OnTimerRecord(const CommandContext &context );
void OnRemoveTracks(const CommandContext &context );
void OnMoveSelectionWithTracks(const CommandContext &context );
void OnSyncLock(const CommandContext &context );
void OnAddLabel(const CommandContext &context );
void OnAddLabelPlaying(const CommandContext &context );
void DoEditLabels(LabelTrack *lt = nullptr, int index = -1);
void OnEditLabels(const CommandContext &context );
void OnToggleTypeToCreateLabel(const CommandContext &context );

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

bool DoEffect(const PluginID & ID, const CommandContext & context, int flags);
void OnEffect(const CommandContext &context );
void OnRepeatLastEffect(const CommandContext &context );
bool DoAudacityCommand(const PluginID & ID, const CommandContext &, int flags);
void OnApplyMacroDirectly(const CommandContext &context );
void OnApplyMacrosPalette(const CommandContext &context );
void OnManageMacros(const CommandContext &context );
void OnStereoToMono(const CommandContext &context );
void OnAudacityCommand(const CommandContext &context );
void OnManagePluginsMenu(EffectType Type);
static void RebuildAllMenuBars();
void OnManageGenerators(const CommandContext &context );
void OnManageEffects(const CommandContext &context );
void OnManageAnalyzers(const CommandContext &context );
void OnManageTools(const CommandContext &context );



        // Help Menu

void OnAbout(const CommandContext &context );
void OnQuickHelp(const CommandContext &context );
void OnManual(const CommandContext &context );
void OnCheckForUpdates(const CommandContext &context );
void MayCheckForUpdates();
void OnShowLog(const CommandContext &context );
void OnHelpWelcome(const CommandContext &context );
void OnBenchmark(const CommandContext &context );
#if defined(EXPERIMENTAL_CRASH_REPORT)
void OnCrashReport(const CommandContext &context );
#endif
void OnSimulateRecordingErrors(const CommandContext &context );
void OnDetectUpstreamDropouts(const CommandContext &context );
void OnScreenshot(const CommandContext &context );
void OnAudioDeviceInfo(const CommandContext &context );
#ifdef EXPERIMENTAL_MIDI_OUT
void OnMidiDeviceInfo(const CommandContext &context );
#endif

       //

void OnSeparator(const CommandContext &context );

      // Keyboard navigation

void NextOrPrevFrame(bool next);
void PrevFrame(const CommandContext &context );
void NextFrame(const CommandContext &context );

void PrevWindow(const CommandContext &context );
void NextWindow(const CommandContext &context );

void OnResample(const CommandContext &context );

private:
enum SelectionOperation {
    SELECTION_EXTEND,
    SELECTION_CONTRACT,
    CURSOR_MOVE
};

enum CursorDirection {
   DIRECTION_LEFT = -1,
   DIRECTION_RIGHT = +1
};

enum TimeUnit {
    TIME_UNIT_SECONDS,
    TIME_UNIT_PIXELS
};

bool OnlyHandleKeyUp( const CommandContext &context );
void OnCursorMove(double seekStep);
void OnBoundaryMove(int step);

// Handle small cursor and play head movements
void SeekLeftOrRight
(double direction, SelectionOperation operation);

void SeekWhenAudioActive(double seekStep);
void SeekWhenAudioInactive
(double seekStep, TimeUnit timeUnit,
 SelectionOperation operation);
void MoveWhenAudioInactive
(double seekStep, TimeUnit timeUnit);



double OffsetTime(double t, double offset, TimeUnit timeUnit, int snapToTime);

// Helper for moving by keyboard with snap-to-grid enabled
double GridMove(double t, int minPix);

// Make sure we return to "public" for subsequent declarations in Project.h.
public:


#endif



