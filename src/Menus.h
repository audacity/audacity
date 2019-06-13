/**********************************************************************

  Audacity: A Digital Audio Editor

  Menus.h

  Dominic Mazzoni

**********************************************************************/
#ifndef __AUDACITY_MENUS__
#define __AUDACITY_MENUS__

#include "audacity/Types.h"

#include <wx/string.h> // member variable
#include "Prefs.h"
#include "ClientData.h"
#include "commands/CommandFlag.h"

class wxArrayString;
class wxCommandEvent;
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

enum EffectType : int;

typedef wxString PluginID;
typedef wxArrayString PluginIDs;

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

class MenuManager final
   : public MenuCreator
   , public ClientData::Base
   , private PrefsListener
{
public:

   static MenuManager &Get( AudacityProject &project );
   static const MenuManager &Get( const AudacityProject &project );

   MenuManager( AudacityProject &project );
   ~MenuManager();

   static void ModifyUndoMenuItems(AudacityProject &project);
   static void ModifyToolbarMenus(AudacityProject &project);
   // Calls ModifyToolbarMenus() on all projects
   static void ModifyAllProjectToolbarMenus();

   // checkActive is a temporary hack that should be removed as soon as we
   // get multiple effect preview working
   void UpdateMenus( bool checkActive = true );

   // If checkActive, do not do complete flags testing on an
   // inactive project as it is needlessly expensive.
   CommandFlag GetUpdateFlags( bool checkActive = false );
   void UpdatePrefs() override;

   // Command Handling
   bool ReportIfActionNotAllowed(
      const wxString & Name, CommandFlag & flags, CommandFlag flagsRqd );
   bool TryToMakeActionAllowed(
      CommandFlag & flags, CommandFlag flagsRqd );


private:
   void TellUserWhyDisallowed(const wxString & Name, CommandFlag flagsGot,
      CommandFlag flagsRequired);

   void OnUndoRedo( wxCommandEvent &evt );

   AudacityProject &mProject;

public:
   // 0 is grey out, 1 is Autoselect, 2 is Give warnings.
   int  mWhatIfNoSelection;
   bool mStopIfWasPaused;
};


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

/// Namespace for functions for View menu
namespace ViewActions {
double GetZoomOfToFit( const AudacityProject &project );
void DoZoomFit( AudacityProject &project );
}

/// Namespace for functions for Transport menu
namespace TransportActions {
void StopIfPaused( AudacityProject &project );
bool DoPlayStopSelect( AudacityProject &project, bool click, bool shift );
void DoPlayStopSelect( AudacityProject &project );
void DoStop( AudacityProject & );
void DoPause( AudacityProject & );
void DoLockPlayRegion( AudacityProject & );
void DoUnlockPlayRegion( AudacityProject & );
void DoTogglePinnedHead( AudacityProject & );
void DoRecord( AudacityProject & );
}

/// Namespace for functions for Help menu
namespace HelpActions {
void DoHelpWelcome( AudacityProject & );
}

#endif
