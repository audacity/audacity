/**********************************************************************

  Audacity: A Digital Audio Editor

  Menus.h

  Dominic Mazzoni

**********************************************************************/
#ifndef __AUDACITY_MENUS__
#define __AUDACITY_MENUS__

#include "Experimental.h"

#include <memory>
#include <vector>
#include <wx/event.h>
#include "SelectedRegion.h"
#include "commands/CommandFunctors.h"

class AudacityProject;
class CommandContext;
class CommandManager;
class LabelTrack;
class PluginDescriptor;
class Track;
class TrackList;
class ViewInfo;
class WaveClip;
class WaveTrack;

enum CommandFlag : unsigned long long;
enum EffectType : int;

typedef wxString PluginID;
typedef wxArrayString PluginIDList;

class PrefsListener
{
public:
   virtual ~PrefsListener();
   virtual void UpdatePrefs(); // default is no-op
};

struct MenuCommandHandler final
   : public CommandHandlerObject // MUST be the first base class!
   , public PrefsListener
{
   MenuCommandHandler();
   ~MenuCommandHandler();

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

        // Moving track focus commands

void DoPrevTrack( AudacityProject &project, bool shift );
void DoNextTrack( AudacityProject &project, bool shift );
void OnCursorUp(const CommandContext &context );
void OnCursorDown(const CommandContext &context );
void OnFirstTrack(const CommandContext &context );
void OnLastTrack(const CommandContext &context );

        // Selection-Editing Commands

void OnShiftUp(const CommandContext &context );
void OnShiftDown(const CommandContext &context );
void OnToggle(const CommandContext &context );


void OnFullScreen(const CommandContext &context );

// File Menu

void OnCheckDependencies(const CommandContext &context );

// Edit Menu


public:

// Effect Menu

struct OnEffectFlags
{

   // No flags specified
   static const int kNone = 0x00;
   // Flag used to disable prompting for configuration parameteres.
   static const int kConfigured = 0x01;
   // Flag used to disable saving the state after processing.
   static const int kSkipState  = 0x02;
   // Flag used to disable "Repeat Last Effect"
   static const int kDontRepeatLast = 0x04;
};

static void RebuildAllMenuBars();

// Help Menu

void OnAbout(const CommandContext &context );
void OnQuickHelp(const CommandContext &context );
void OnQuickFix(const CommandContext &context );
void OnManual(const CommandContext &context );
void OnCheckForUpdates(const CommandContext &context );
void MayCheckForUpdates(AudacityProject &project);
void OnShowLog(const CommandContext &context );
void OnHelpWelcome(const CommandContext &context );
#if defined(EXPERIMENTAL_CRASH_REPORT)
void OnCrashReport(const CommandContext &context );
#endif
void OnAudioDeviceInfo(const CommandContext &context );
#ifdef EXPERIMENTAL_MIDI_OUT
void OnMidiDeviceInfo(const CommandContext &context );
#endif

// Keyboard navigation

void NextOrPrevFrame(AudacityProject &project, bool next);
void OnPrevFrame(const CommandContext &context );
void OnNextFrame(const CommandContext &context );

void OnPrevWindow(const CommandContext &context );
void OnNextWindow(const CommandContext &context );

public:
   bool mCircularTrackNavigation{};

   void UpdatePrefs() override;
};

class MenuCreator
{
public:
   MenuCreator();
   ~MenuCreator();
   void CreateMenusAndCommands(AudacityProject &project);
   void RebuildMenuBar(AudacityProject &project);

public:
   CommandFlag mLastFlags;
   
   // Last effect applied to this project
   PluginID mLastEffect{};
};

class MenuManager : public MenuCreator
{
public:
   static void ModifyUndoMenuItems(AudacityProject &project);
   static void ModifyToolbarMenus(AudacityProject &project);
   // Calls ModifyToolbarMenus() on all projects
   static void ModifyAllProjectToolbarMenus();

   // checkActive is a temporary hack that should be removed as soon as we
   // get multiple effect preview working
   void UpdateMenus(AudacityProject &project, bool checkActive = true);

   // If checkActive, do not do complete flags testing on an
   // inactive project as it is needlessly expensive.
   CommandFlag GetUpdateFlags(AudacityProject &project, bool checkActive = false);
   void UpdatePrefs();

   // Command Handling
   bool ReportIfActionNotAllowed
      ( AudacityProject &project,
        const wxString & Name, CommandFlag & flags, CommandFlag flagsRqd, CommandFlag mask );
   bool TryToMakeActionAllowed
      ( AudacityProject &project,
        CommandFlag & flags, CommandFlag flagsRqd, CommandFlag mask );


private:
   CommandFlag GetFocusedFrame(AudacityProject &project);

   // 0 is grey out, 1 is Autoselect, 2 is Give warnings.
   int  mWhatIfNoSelection;
   bool mStopIfWasPaused;
};


MenuCommandHandler &GetMenuCommandHandler(AudacityProject &project);
MenuManager &GetMenuManager(AudacityProject &project);

// Exported helper functions from various menu handling source files
namespace FileActions {
AudacityProject *DoImportMIDI(
   AudacityProject *pProject, const wxString &fileName );
}

namespace EditActions {
bool DoEditMetadata(
   AudacityProject &project,
   const wxString &title, const wxString &shortUndoDescription, bool force );
void DoReloadPreferences( AudacityProject & );
void DoUndo( AudacityProject &project );
}

namespace SelectActions {
void DoListSelection(
   AudacityProject &project, Track *t,
   bool shift, bool ctrl, bool modifyState );
void DoSelectAll( AudacityProject &project );
void DoSelectSomething( AudacityProject &project );
}

namespace ViewActions {
void DoZoomFit( AudacityProject &project );
void DoZoomFitV( AudacityProject &project );
}

namespace TransportActions {
bool DoPlayStopSelect( AudacityProject &project, bool click, bool shift );
void DoPlayStopSelect( AudacityProject &project );
void DoStop( AudacityProject & );
void DoPause( AudacityProject & );
void DoLockPlayRegion( AudacityProject & );
void DoUnlockPlayRegion( AudacityProject & );
void DoTogglePinnedHead( AudacityProject & );
void DoRecord( AudacityProject & );
}

namespace TrackActions {
   enum MoveChoice {
      OnMoveUpID, OnMoveDownID, OnMoveTopID, OnMoveBottomID
   };
/// Move a track up, down, to top or to bottom.
void DoMoveTrack( AudacityProject &project, Track* target, MoveChoice choice );
void DoRemoveTracks( AudacityProject & );
}

namespace PluginActions {
bool DoEffect(
   const PluginID & ID, const CommandContext & context, unsigned flags );
bool DoAudacityCommand(
   const PluginID & ID, const CommandContext & context, unsigned flags );
}

#endif



