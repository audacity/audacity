/**********************************************************************

  Audacity: A Digital Audio Editor

  Menus.h

  Dominic Mazzoni

**********************************************************************/
#ifndef __AUDACITY_MENUS__
#define __AUDACITY_MENUS__

#include "audacity/Types.h"

#include <wx/string.h> // member variable

class wxArrayString;
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
typedef wxArrayString PluginIDs;

class PrefsListener
{
public:
   virtual ~PrefsListener();
   virtual void UpdatePrefs(); // default is no-op
};

class MenuCreator
{
public:
   MenuCreator();
   ~MenuCreator();
   void CreateMenusAndCommands(AudacityProject &project);
   void RebuildMenuBar(AudacityProject &project);

   static void RebuildAllMenuBars();

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
   CommandFlag GetUpdateFlags(
      AudacityProject &project, bool checkActive = false);
   void UpdatePrefs();

   // Command Handling
   bool ReportIfActionNotAllowed(
      AudacityProject &project,
      const wxString & Name, CommandFlag & flags, CommandFlag flagsRqd,
      CommandFlag mask );
   bool TryToMakeActionAllowed(
      AudacityProject &project,
      CommandFlag & flags, CommandFlag flagsRqd, CommandFlag mask );


private:
   CommandFlag GetFocusedFrame(AudacityProject &project);

   // 0 is grey out, 1 is Autoselect, 2 is Give warnings.
   int  mWhatIfNoSelection;
   bool mStopIfWasPaused;
};


MenuManager &GetMenuManager(AudacityProject &project);

// Exported helper functions from various menu handling source files


/// Namespace for functions for File menu
namespace FileActions {
AudacityProject *DoImportMIDI(
   AudacityProject *pProject, const FilePath &fileName );
}

/// Namespace for functions for Edit menu
namespace EditActions {
bool DoEditMetadata(
   AudacityProject &project,
   const wxString &title, const wxString &shortUndoDescription, bool force );
void DoReloadPreferences( AudacityProject & );
void DoUndo( AudacityProject &project );
}

/// Namespace for functions for Select menu
namespace SelectActions {
void DoListSelection(
   AudacityProject &project, Track *t,
   bool shift, bool ctrl, bool modifyState );
void DoSelectAll( AudacityProject &project );
void DoSelectSomething( AudacityProject &project );
}

/// Namespace for functions for View menu
namespace ViewActions {
double GetZoomOfToFit( const AudacityProject &project );
void DoZoomFit( AudacityProject &project );
void DoZoomFitV( AudacityProject &project );
}

/// Namespace for functions for Transport menu
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

/// Namespace for functions for Track menu
namespace TrackActions {
   enum MoveChoice {
      OnMoveUpID, OnMoveDownID, OnMoveTopID, OnMoveBottomID
   };
/// Move a track up, down, to top or to bottom.
void DoMoveTrack( AudacityProject &project, Track* target, MoveChoice choice );
// "exclusive" mute means mute the chosen track and unmute all others.
void DoTrackMute( AudacityProject &project, Track *pTrack, bool exclusive );
// Type of solo (standard or simple) follows the set preference, unless
// exclusive == true, which causes the opposite behavior.
void DoTrackSolo( AudacityProject &project, Track *pTrack, bool exclusive );
void DoRemoveTrack( AudacityProject &project, Track * toRemove );
void DoRemoveTracks( AudacityProject & );
}


/// Namespace for helper functions to do with plug ins
namespace PluginActions {
   enum : unsigned {
      // No flags specified
      kNone = 0x00,
      // Flag used to disable prompting for configuration parameteres.
      kConfigured = 0x01,
      // Flag used to disable saving the state after processing.
      kSkipState  = 0x02,
      // Flag used to disable "Repeat Last Effect"
      kDontRepeatLast = 0x04,
   };
bool DoEffect(
   const PluginID & ID, const CommandContext & context, unsigned flags );
bool DoAudacityCommand(
   const PluginID & ID, const CommandContext & context, unsigned flags );
}

/// Namespace for functions for Help menu
namespace HelpActions {
void DoHelpWelcome( AudacityProject & );
void DoShowLog( AudacityProject& );
}

#endif
