/**********************************************************************

  Sneedacity: A Digital Audio Editor

  Menus.h

  Dominic Mazzoni

**********************************************************************/
#ifndef __SNEEDACITY_MENUS__
#define __SNEEDACITY_MENUS__

#include "Identifier.h"

#include <wx/string.h> // member variable
#include "Prefs.h"
#include "ClientData.h"
#include "commands/CommandFlag.h"

class wxArrayString;
class wxCommandEvent;
class SneedacityProject;
class CommandContext;
class CommandManager;
class Track;
class TrackList;
class ViewInfo;

enum EffectType : int;

typedef wxString PluginID;
typedef wxString MacroID;
typedef wxArrayString PluginIDs;

namespace Registry{ class Visitor; }

class SNEEDACITY_DLL_API MenuCreator
{
public:
   MenuCreator();
   ~MenuCreator();
   void CreateMenusAndCommands(SneedacityProject &project);
   void RebuildMenuBar(SneedacityProject &project);

   static void RebuildAllMenuBars();

public:
   CommandFlag mLastFlags;
   
   // Last effect applied to this project
   PluginID mLastGenerator{};
   PluginID mLastEffect{};
   PluginID mLastAnalyzer{};
   int mLastAnalyzerRegistration;
   int mLastAnalyzerRegisteredId;
   PluginID mLastTool{};
   int mLastToolRegistration;
   int mLastToolRegisteredId;
   enum {
      repeattypenone = 0,
      repeattypeplugin = 1,
      repeattypeunique = 2,
      repeattypeapplymacro = 3
   };
   unsigned mRepeatGeneratorFlags;
   unsigned mRepeatEffectFlags;
   unsigned mRepeatAnalyzerFlags;
   unsigned mRepeatToolFlags;
};

struct ToolbarMenuVisitor;

class SNEEDACITY_DLL_API MenuManager final
   : public MenuCreator
   , public ClientData::Base
   , private PrefsListener
{
public:

   static MenuManager &Get( SneedacityProject &project );
   static const MenuManager &Get( const SneedacityProject &project );

   explicit
   MenuManager( SneedacityProject &project );
   MenuManager( const MenuManager & ) PROHIBITED;
   MenuManager &operator=( const MenuManager & ) PROHIBITED;
   ~MenuManager();

   static void Visit( ToolbarMenuVisitor &visitor );

   static void ModifyUndoMenuItems(SneedacityProject &project);
   static void ModifyToolbarMenus(SneedacityProject &project);
   // Calls ModifyToolbarMenus() on all projects
   static void ModifyAllProjectToolbarMenus();

   // checkActive is a temporary hack that should be removed as soon as we
   // get multiple effect preview working
   void UpdateMenus( bool checkActive = true );

   // If checkActive, do not do complete flags testing on an
   // inactive project as it is needlessly expensive.
   CommandFlag GetUpdateFlags( bool checkActive = false ) const;
   void UpdatePrefs() override;

   // Command Handling
   bool ReportIfActionNotAllowed(
      const TranslatableString & Name, CommandFlag & flags, CommandFlag flagsRqd );
   bool TryToMakeActionAllowed(
      CommandFlag & flags, CommandFlag flagsRqd );


private:
   void TellUserWhyDisallowed(const TranslatableString & Name, CommandFlag flagsGot,
      CommandFlag flagsRequired);

   void OnUndoRedo( wxCommandEvent &evt );

   SneedacityProject &mProject;

public:
   // 0 is grey out, 1 is Autoselect, 2 is Give warnings.
   int  mWhatIfNoSelection;
   bool mStopIfWasPaused;
};

#endif
