

#include "../AudioIO.h"
#include "../BatchProcessDialog.h"
#include "../Benchmark.h"
#include "../CommonCommandFlags.h"
#include "../Journal.h"
#include "../Menus.h"
#include "../PluginManager.h"
#include "../PluginRegistrationDialog.h"
#include "Prefs.h"
#include "Project.h"
#include "../ProjectSettings.h"
#include "../ProjectWindow.h"
#include "../ProjectWindows.h"
#include "../ProjectSelectionManager.h"
#include "../toolbars/ToolManager.h"
#include "../Screenshot.h"
#include "TempDirectory.h"
#include "../UndoManager.h"
#include "../commands/CommandContext.h"
#include "../commands/CommandManager.h"
#include "../commands/ScreenshotCommand.h"
#include "../effects/EffectManager.h"
#include "../effects/EffectUI.h"
#include "../effects/RealtimeEffectManager.h"
#include "../prefs/EffectsPrefs.h"
#include "../prefs/PrefsDialog.h"
#include "../widgets/AudacityMessageBox.h"

#include <wx/log.h>

// private helper classes and functions
namespace {

AttachedWindows::RegisteredFactory sMacrosWindowKey{
   []( AudacityProject &parent ) -> wxWeakRef< wxWindow > {
      auto &window = ProjectWindow::Get( parent );
      return safenew MacrosWindow(
         &window, parent, true
      );
   }
};

bool ShowManager(
   PluginManager &pm, wxWindow *parent, EffectType type)
{
   pm.CheckForUpdates();

   PluginRegistrationDialog dlg(parent, type);
   return dlg.ShowModal() == wxID_OK;
}

void DoManagePluginsMenu(AudacityProject &project, EffectType type)
{
   auto &window = GetProjectFrame( project );
   auto &pm = PluginManager::Get();
   if (ShowManager(pm, &window, type))
      MenuCreator::RebuildAllMenuBars();
}

bool CompareEffectsByName(const PluginDescriptor *a, const PluginDescriptor *b)
{
   return
      std::make_pair( a->GetSymbol().Translation(), a->GetPath() ) <
      std::make_pair( b->GetSymbol().Translation(), b->GetPath() );
}

bool CompareEffectsByPublisher(
   const PluginDescriptor *a, const PluginDescriptor *b)
{
   auto &em = EffectManager::Get();
   
   auto akey = em.GetVendorName(a->GetID());
   auto bkey = em.GetVendorName(b->GetID());

   if (akey.empty())
      akey = XO("Uncategorized");
   if (bkey.empty())
      bkey = XO("Uncategorized");

   return
      std::make_tuple(
         akey.Translation(), a->GetSymbol().Translation(), a->GetPath() ) <
      std::make_tuple(
         bkey.Translation(), b->GetSymbol().Translation(), b->GetPath() );
}

bool CompareEffectsByPublisherAndName(
   const PluginDescriptor *a, const PluginDescriptor *b)
{
   auto &em = EffectManager::Get();
   auto akey = em.GetVendorName(a->GetID());
   auto bkey = em.GetVendorName(b->GetID());

   if (a->IsEffectDefault())
      akey = {};
   if (b->IsEffectDefault())
      bkey = {};

   return
      std::make_tuple(
         akey.Translation(), a->GetSymbol().Translation(), a->GetPath() ) <
      std::make_tuple(
         bkey.Translation(), b->GetSymbol().Translation(), b->GetPath() );
}

bool CompareEffectsByTypeAndName(
   const PluginDescriptor *a, const PluginDescriptor *b)
{
   auto &em = EffectManager::Get();
   auto akey = em.GetEffectFamilyName(a->GetID());
   auto bkey = em.GetEffectFamilyName(b->GetID());

   if (akey.empty())
      akey = XO("Uncategorized");
   if (bkey.empty())
      bkey = XO("Uncategorized");

   if (a->IsEffectDefault())
      akey = {};
   if (b->IsEffectDefault())
      bkey = {};

   return
      std::make_tuple(
         akey.Translation(), a->GetSymbol().Translation(), a->GetPath() ) <
      std::make_tuple(
         bkey.Translation(), b->GetSymbol().Translation(), b->GetPath() );
}

bool CompareEffectsByType(const PluginDescriptor *a, const PluginDescriptor *b)
{
   auto &em = EffectManager::Get();
   auto akey = em.GetEffectFamilyName(a->GetID());
   auto bkey = em.GetEffectFamilyName(b->GetID());

   if (akey.empty())
      akey = XO("Uncategorized");
   if (bkey.empty())
      bkey = XO("Uncategorized");

   return
      std::make_tuple(
         akey.Translation(), a->GetSymbol().Translation(), a->GetPath() ) <
      std::make_tuple(
         bkey.Translation(), b->GetSymbol().Translation(), b->GetPath() );
}

// Forward-declared function has its definition below with OnEffect in view
void AddEffectMenuItemGroup(
   MenuTable::BaseItemPtrs &table,
   const TranslatableStrings & names,
   const PluginIDs & plugs,
   const std::vector<CommandFlag> & flags,
   bool isDefault);

void AddEffectMenuItems(
   MenuTable::BaseItemPtrs &table,
   std::vector<const PluginDescriptor*> & plugs,
   CommandFlag batchflags,
   CommandFlag realflags,
   bool isDefault)
{
   size_t pluginCnt = plugs.size();

   auto groupBy = EffectsGroupBy.Read();

   bool grouped = false;
   if (groupBy.StartsWith(wxT("groupby")))
   {
      grouped = true;
   }

   // Some weird special case stuff just for Noise Reduction so that there is
   // more informative help
   const auto getBatchFlags = [&]( const PluginDescriptor *plug ){
      if ( plug->GetSymbol().Msgid() == XO( "Noise Reduction" ) )
         return
            ( batchflags | NoiseReductionTimeSelectedFlag() ) & ~TimeSelectedFlag();
      return batchflags;
   };

   TranslatableStrings groupNames;
   PluginIDs groupPlugs;
   std::vector<CommandFlag> groupFlags;
   if (grouped)
   {
      TranslatableString last;
      TranslatableString current;

      for (size_t i = 0; i < pluginCnt; i++)
      {
         const PluginDescriptor *plug = plugs[i];

         auto name = plug->GetSymbol().Msgid();

         if (plug->IsEffectInteractive())
            name += XO("...");

         if (groupBy == wxT("groupby:publisher"))
         {
            current = EffectManager::Get().GetVendorName(plug->GetID());
            if (current.empty())
            {
               current = XO("Unknown");
            }
         }
         else if (groupBy == wxT("groupby:type"))
         {
            current = EffectManager::Get().GetEffectFamilyName(plug->GetID());
            if (current.empty())
            {
               current = XO("Unknown");
            }
         }

         if (current != last)
         {
            using namespace MenuTable;
            BaseItemPtrs temp;
            bool bInSubmenu = !last.empty() && (groupNames.size() > 1);

            AddEffectMenuItemGroup(temp,
               groupNames,
               groupPlugs, groupFlags, isDefault);

            table.push_back( MenuOrItems( wxEmptyString,
               ( bInSubmenu ? last : TranslatableString{} ), std::move( temp )
            ) );

            groupNames.clear();
            groupPlugs.clear();
            groupFlags.clear();
            last = current;
         }

         groupNames.push_back( name );
         groupPlugs.push_back(plug->GetID());
         groupFlags.push_back(
            plug->IsEffectRealtime() ? realflags : getBatchFlags( plug ) );
      }

      if (groupNames.size() > 0)
      {
         using namespace MenuTable;
         BaseItemPtrs temp;
         bool bInSubmenu = groupNames.size() > 1;

         AddEffectMenuItemGroup(temp,
            groupNames, groupPlugs, groupFlags, isDefault);

         table.push_back( MenuOrItems( wxEmptyString,
            ( bInSubmenu ? current : TranslatableString{} ), std::move( temp )
         ) );
      }
   }
   else
   {
      for (size_t i = 0; i < pluginCnt; i++)
      {
         const PluginDescriptor *plug = plugs[i];

         auto name = plug->GetSymbol().Msgid();

         if (plug->IsEffectInteractive())
            name += XO("...");

         TranslatableString group;
         if (groupBy == wxT("sortby:publisher:name"))
         {
            group = EffectManager::Get().GetVendorName(plug->GetID());
         }
         else if (groupBy == wxT("sortby:type:name"))
         {
            group = EffectManager::Get().GetEffectFamilyName(plug->GetID());
         }

         if (plug->IsEffectDefault())
         {
            group = {};
         }

         groupNames.push_back(
            group.empty()
               ? name
               : XO("%s: %s").Format( group, name )
         );

         groupPlugs.push_back(plug->GetID());
         groupFlags.push_back(
            plug->IsEffectRealtime() ? realflags : getBatchFlags( plug ) );
      }

      if (groupNames.size() > 0)
      {
         AddEffectMenuItemGroup(
            table, groupNames, groupPlugs, groupFlags, isDefault);
      }

   }

   return;
}

/// The effects come from a plug in list
/// This code iterates through the list, adding effects into
/// the menu.
MenuTable::BaseItemPtrs PopulateEffectsMenu(
   EffectType type,
   CommandFlag batchflags,
   CommandFlag realflags)
{
   MenuTable::BaseItemPtrs result;
   PluginManager & pm = PluginManager::Get();

   std::vector<const PluginDescriptor*> defplugs;
   std::vector<const PluginDescriptor*> optplugs;

   EffectManager & em = EffectManager::Get();
   for (auto &plugin : pm.EffectsOfType(type)) {
      auto plug = &plugin;
      if( plug->IsInstantiated() && em.IsHidden(plug->GetID()) )
         continue;
      if ( !plug->IsEnabled() ){
         ;// don't add to menus!
      }
      else if (plug->IsEffectDefault()
#ifdef EXPERIMENTAL_DA
         // Move Nyquist prompt into nyquist group.
         && (plug->GetSymbol() !=
               ComponentInterfaceSymbol("Nyquist Effects Prompt"))
         && (plug->GetSymbol() != ComponentInterfaceSymbol("Nyquist Tools Prompt"))
         && (plug->GetSymbol() != ComponentInterfaceSymbol(NYQUIST_PROMPT_ID))
#endif
         )
         defplugs.push_back(plug);
      else
         optplugs.push_back(plug);
   }

   wxString groupby = EffectsGroupBy.Read();

   using Comparator = bool(*)(const PluginDescriptor*, const PluginDescriptor*);
   Comparator comp1, comp2;
   if (groupby == wxT("sortby:name"))
      comp1 = comp2 = CompareEffectsByName;
   else if (groupby == wxT("sortby:publisher:name"))
      comp1 = CompareEffectsByName, comp2 = CompareEffectsByPublisherAndName;
   else if (groupby == wxT("sortby:type:name"))
      comp1 = CompareEffectsByName, comp2 = CompareEffectsByTypeAndName;
   else if (groupby == wxT("groupby:publisher"))
      comp1 = comp2 = CompareEffectsByPublisher;
   else if (groupby == wxT("groupby:type"))
      comp1 = comp2 = CompareEffectsByType;
   else // name
      comp1 = comp2 = CompareEffectsByName;

   std::sort( defplugs.begin(), defplugs.end(), comp1 );
   std::sort( optplugs.begin(), optplugs.end(), comp2 );

   MenuTable::BaseItemPtrs section1;
   AddEffectMenuItems( section1, defplugs, batchflags, realflags, true );

   MenuTable::BaseItemPtrs section2;
   AddEffectMenuItems( section2, optplugs, batchflags, realflags, false );

   bool section = !section1.empty() && !section2.empty();
   result.push_back( MenuTable::Items( "", std::move( section1 ) ) );
   if ( section )
      result.push_back( MenuTable::Section( "", std::move( section2 ) ) );
   else
      result.push_back( MenuTable::Items( "", std::move( section2 ) ) );

   return result;
}

// Forward-declared function has its definition below with OnApplyMacroDirectly
// in view
MenuTable::BaseItemPtrs PopulateMacrosMenu( CommandFlag flags  );

}

namespace PluginActions {

// Menu handler functions

struct Handler : CommandHandlerObject {

void OnResetConfig(const CommandContext &context)
{
   auto &project = context.project;
   auto &menuManager = MenuManager::Get(project);
   menuManager.mLastAnalyzerRegistration = MenuCreator::repeattypenone;
   menuManager.mLastToolRegistration = MenuCreator::repeattypenone;
   menuManager.mLastGenerator = "";
   menuManager.mLastEffect = "";
   menuManager.mLastAnalyzer = "";
   menuManager.mLastTool = "";

   ResetPreferences();

   // Directory will be reset on next restart.
   FileNames::UpdateDefaultPath(FileNames::Operation::Temp, TempDirectory::DefaultTempDir());

   // There are many more things we could reset here.
   // Beeds discussion as to which make sense to.
   // Maybe in future versions?
   // - Reset Effects
   // - Reset Recording and Playback volumes
   // - Reset Selection formats (and for spectral too)
   // - Reset Play-at-speed speed to x1
   // - Stop playback/recording and unapply pause.
   // - Set Zoom sensibly.
   gPrefs->Write("/GUI/SyncLockTracks", 0);
   gPrefs->Write("/AudioIO/SoundActivatedRecord", 0);
   gPrefs->Write("/SelectionToolbarMode", 0);
   gPrefs->Flush();
   DoReloadPreferences(project);
   ToolManager::OnResetToolBars(context);

   // These are necessary to preserve the newly correctly laid out toolbars.
   // In particular the Device Toolbar ends up short on next restart, 
   // if they are left out.
   gPrefs->Write(wxT("/PrefsVersion"), wxString(wxT(AUDACITY_PREFS_VERSION_STRING)));

   // write out the version numbers to the prefs file for future checking
   gPrefs->Write(wxT("/Version/Major"), AUDACITY_VERSION);
   gPrefs->Write(wxT("/Version/Minor"), AUDACITY_RELEASE);
   gPrefs->Write(wxT("/Version/Micro"), AUDACITY_REVISION);

   gPrefs->Flush();

   ProjectSelectionManager::Get( project )
      .AS_SetSnapTo(gPrefs->ReadLong("/SnapTo", SNAP_OFF));
   ProjectSelectionManager::Get( project )
      .AS_SetRate(gPrefs->ReadDouble("/DefaultProjectSampleRate", 44100.0));
}

void OnManageGenerators(const CommandContext &context)
{
   auto &project = context.project;
   DoManagePluginsMenu(project, EffectTypeGenerate);
}

void OnEffect(const CommandContext &context)
{
   // using GET to interpret parameter as a PluginID
   EffectUI::DoEffect(context.parameter.GET(), context, 0);
}

void OnManageEffects(const CommandContext &context)
{
   auto &project = context.project;
   DoManagePluginsMenu(project, EffectTypeProcess);
}

void OnAnalyzer2(wxCommandEvent& evt) { return; }

void OnRepeatLastGenerator(const CommandContext &context)
{
   auto& menuManager = MenuManager::Get(context.project);
   auto lastEffect = menuManager.mLastGenerator;
   if (!lastEffect.empty())
   {
      EffectUI::DoEffect(
         lastEffect, context, menuManager.mRepeatGeneratorFlags | EffectManager::kRepeatGen);
   }
}

void OnRepeatLastEffect(const CommandContext &context)
{
   auto& menuManager = MenuManager::Get(context.project);
   auto lastEffect = menuManager.mLastEffect;
   if (!lastEffect.empty())
   {
      EffectUI::DoEffect(
         lastEffect, context, menuManager.mRepeatEffectFlags);
   }
}

void OnRepeatLastAnalyzer(const CommandContext& context)
{
   auto& menuManager = MenuManager::Get(context.project);
   switch (menuManager.mLastAnalyzerRegistration) {
   case MenuCreator::repeattypeplugin:
     {
       auto lastEffect = menuManager.mLastAnalyzer;
       if (!lastEffect.empty())
       {
         EffectUI::DoEffect(
            lastEffect, context, menuManager.mRepeatAnalyzerFlags);
       }
     }
      break;
   case MenuCreator::repeattypeunique:
      CommandManager::Get(context.project).DoRepeatProcess(context,
         menuManager.mLastAnalyzerRegisteredId);
      break;
   }
}

void OnRepeatLastTool(const CommandContext& context)
{
   auto& menuManager = MenuManager::Get(context.project);
   switch (menuManager.mLastToolRegistration) {
     case MenuCreator::repeattypeplugin:
     {
        auto lastEffect = menuManager.mLastTool;
        if (!lastEffect.empty())
        {
           EffectUI::DoEffect(
              lastEffect, context, menuManager.mRepeatToolFlags);
        }
     }
       break;
     case MenuCreator::repeattypeunique:
        CommandManager::Get(context.project).DoRepeatProcess(context,
           menuManager.mLastToolRegisteredId);
        break;
     case MenuCreator::repeattypeapplymacro:
        OnApplyMacroDirectlyByName(context, menuManager.mLastTool);
        break;
   }
}


void OnManageAnalyzers(const CommandContext &context)
{
   auto &project = context.project;
   DoManagePluginsMenu(project, EffectTypeAnalyze);
}

void OnManageTools(const CommandContext &context )
{
   auto &project = context.project;
   DoManagePluginsMenu(project, EffectTypeTool);
}

void OnManageMacros(const CommandContext &context )
{
   auto &project = context.project;
   CommandManager::Get(project).RegisterLastTool(context);  //Register Macros as Last Tool
   auto macrosWindow = &GetAttachedWindows(project)
      .AttachedWindows::Get< MacrosWindow >( sMacrosWindowKey );
   if (macrosWindow) {
      macrosWindow->Show();
      macrosWindow->Raise();
      macrosWindow->UpdateDisplay( true );
   }
}

void OnApplyMacrosPalette(const CommandContext &context )
{
   auto &project = context.project;
   CommandManager::Get(project).RegisterLastTool(context);  //Register Palette as Last Tool
   auto macrosWindow = &GetAttachedWindows(project)
      .AttachedWindows::Get< MacrosWindow >( sMacrosWindowKey );
   if (macrosWindow) {
      macrosWindow->Show();
      macrosWindow->Raise();
      macrosWindow->UpdateDisplay( false );
   }
}

void OnScreenshot(const CommandContext &context )
{
   CommandManager::Get(context.project).RegisterLastTool(context);  //Register Screenshot as Last Tool
   ::OpenScreenshotTools( context.project );
}

void OnBenchmark(const CommandContext &context)
{
   auto &project = context.project;
   CommandManager::Get(project).RegisterLastTool(context);  //Register Run Benchmark as Last Tool
   auto &window = GetProjectFrame( project );
   ::RunBenchmark( &window, project);
}

void OnSimulateRecordingErrors(const CommandContext &context)
{
   auto &project = context.project;
   auto &commandManager = CommandManager::Get( project );

   auto gAudioIO = AudioIO::Get();
   bool &setting = gAudioIO->mSimulateRecordingErrors;
   commandManager.Check(wxT("SimulateRecordingErrors"), !setting);
   setting = !setting;
}

void OnDetectUpstreamDropouts(const CommandContext &context)
{
   auto &project = context.project;
   auto &commandManager = CommandManager::Get( project );

   auto gAudioIO = AudioIO::Get();
   bool &setting = gAudioIO->mDetectUpstreamDropouts;
   commandManager.Check(wxT("DetectUpstreamDropouts"), !setting);
   setting = !setting;
}

void OnWriteJournal(const CommandContext &)
{
   auto OnMessage =
      /* i18n-hint a "journal" is a text file that records
       the user's interactions with the application */
      XO("A journal will be recorded after Audacity restarts.");
   auto OffMessage =
      /* i18n-hint a "journal" is a text file that records
       the user's interactions with the application */
      XO("No journal will be recorded after Audacity restarts.");

   using namespace Journal;
   bool enabled = RecordEnabled();
   if ( SetRecordEnabled(!enabled) )
      enabled = !enabled;
   if ( enabled )
      AudacityMessageBox( OnMessage );
   else
      AudacityMessageBox( OffMessage );
}

void OnApplyMacroDirectly(const CommandContext &context )
{
   const MacroID& Name = context.parameter.GET();
   OnApplyMacroDirectlyByName(context, Name);
}
void OnApplyMacroDirectlyByName(const CommandContext& context, const MacroID& Name)
{
   auto &project = context.project;
   auto &window = ProjectWindow::Get( project );
   //wxLogDebug( "Macro was: %s", context.parameter);
   ApplyMacroDialog dlg( &window, project );
   //const auto &Name = context.parameter;

// We used numbers previously, but macros could get renumbered, making
// macros containing macros unpredictable.
#ifdef MACROS_BY_NUMBERS
   long item=0;
   // Take last three letters (of e.g. Macro007) and convert to a number.
   Name.Mid( Name.length() - 3 ).ToLong( &item, 10 );
   dlg.ApplyMacroToProject( item, false );
#else
   dlg.ApplyMacroToProject( Name, false );
#endif
   /* i18n-hint: %s will be the name of the macro which will be
    * repeated if this menu item is chosen */
   MenuManager::ModifyUndoMenuItems( project );

   TranslatableString desc;
   EffectManager& em = EffectManager::Get();
   auto shortDesc = em.GetCommandName(Name);
   auto& undoManager = UndoManager::Get(project);
   auto& commandManager = CommandManager::Get(project);
   int cur = undoManager.GetCurrentState();
   if (undoManager.UndoAvailable()) {
       undoManager.GetShortDescription(cur, &desc);
       commandManager.Modify(wxT("RepeatLastTool"), XXO("&Repeat %s")
          .Format(desc));
       auto& menuManager = MenuManager::Get(project);
       menuManager.mLastTool = Name;
       menuManager.mLastToolRegistration = MenuCreator::repeattypeapplymacro;
   }

}

void OnAudacityCommand(const CommandContext & ctx)
{
   // using GET in a log message for devs' eyes only
   wxLogDebug( "Command was: %s", ctx.parameter.GET());
   // Not configured, so prompt user.
   MacroCommands::DoAudacityCommand(
      EffectManager::Get().GetEffectByIdentifier(ctx.parameter),
      ctx, EffectManager::kNone);
}

}; // struct Handler

} // namespace

static CommandHandlerObject &findCommandHandler(AudacityProject &) {
   // Handler is not stateful.  Doesn't need a factory registered with
   // AudacityProject.
   static PluginActions::Handler instance;
   return instance;
};

// Menu definitions? ...

#define FN(X) (& PluginActions::Handler :: X)

// ... buf first some more helper definitions, which use FN
namespace {

void AddEffectMenuItemGroup(
   MenuTable::BaseItemPtrs &table,
   const TranslatableStrings & names,
   const PluginIDs & plugs,
   const std::vector<CommandFlag> & flags,
   bool isDefault)
{
   const int namesCnt = (int) names.size();
   int perGroup;

#if defined(__WXGTK__)
   gPrefs->Read(wxT("/Effects/MaxPerGroup"), &perGroup, 15);
#else
   gPrefs->Read(wxT("/Effects/MaxPerGroup"), &perGroup, 0);
#endif

   int groupCnt = namesCnt;
   for (int i = 0; i < namesCnt; i++)
   {
      // compare full translations not msgids!
      while (i + 1 < namesCnt && names[i].Translation() == names[i + 1].Translation())
      {
         i++;
         groupCnt--;
      }
   }

   // The "default" effects shouldn't be broken into subgroups
   if (namesCnt > 0 && isDefault)
   {
      perGroup = 0;
   }

   int max = perGroup;
   int items = perGroup;

   if (max > groupCnt)
   {
      max = 0;
   }

   using namespace MenuTable;
   // This finder scope may be redundant, but harmless
   auto scope = FinderScope( findCommandHandler );
   auto pTable = &table;
   BaseItemPtrs temp1;

   int groupNdx = 0;
   for (int i = 0; i < namesCnt; i++)
   {
      if (max > 0 && items == max)
      {
         // start collecting items for the next submenu
         pTable = &temp1;
      }

      // compare full translations not msgids!
      if (i + 1 < namesCnt && names[i].Translation() == names[i + 1].Translation())
      {
         // collect a sub-menu for like-named items
         const auto name = names[i];
         const auto translation = name.Translation();
         BaseItemPtrs temp2;
         // compare full translations not msgids!
         while (i < namesCnt && names[i].Translation() == translation)
         {
            const PluginDescriptor *plug =
               PluginManager::Get().GetPlugin(plugs[i]);
            wxString item = plug->GetPath();
            if( plug->GetPluginType() == PluginTypeEffect )
               temp2.push_back( Command( item,
                  Verbatim( item ),
                  FN(OnEffect),
                  flags[i],
                  CommandManager::Options{}
                     .IsEffect()
                     .AllowInMacros()
                     .Parameter( plugs[i] ) ) );

            i++;
         }
         pTable->push_back( Menu( wxEmptyString, name, std::move( temp2 ) ) );
         i--;
      }
      else
      {
         // collect one item
         const PluginDescriptor *plug =
            PluginManager::Get().GetPlugin(plugs[i]);
         if( plug->GetPluginType() == PluginTypeEffect )
            pTable->push_back( Command(
               // Call Debug() not MSGID() so that any concatenated "..." is
               // included in the identifier, preserving old behavior, and
               // avoiding the collision of the "Silence" command and the
               // "Silence..." generator
               names[i].Debug(), // names[i].MSGID(),
               names[i],
               FN(OnEffect),
               flags[i],
               CommandManager::Options{}
                  .IsEffect()
                  .AllowInMacros()
                  .Parameter( plugs[i] ) ) );
      }

      if (max > 0)
      {
         items--;
         if (items == 0 || i + 1 == namesCnt)
         {
            int end = groupNdx + max;
            if (end + 1 > groupCnt)
            {
               end = groupCnt;
            }
            // Done collecting
            table.push_back( Menu( wxEmptyString,
               XXO("Plug-in %d to %d").Format( groupNdx + 1, end ),
               std::move( temp1 )
            ) );
            items = max;
            pTable = &table;
            groupNdx += max;
         }
      }
   }

   return;
}

MenuTable::BaseItemPtrs PopulateMacrosMenu( CommandFlag flags  )
{
   MenuTable::BaseItemPtrs result;
   auto names = MacroCommands::GetNames(); // these names come from filenames
   int i;

   // This finder scope may be redundant, but harmless
   auto scope = MenuTable::FinderScope( findCommandHandler );
   for (i = 0; i < (int)names.size(); i++) {
      auto MacroID = ApplyMacroDialog::MacroIdOfName( names[i] );
      result.push_back( MenuTable::Command( MacroID,
         Verbatim( names[i] ), // file name verbatim
         FN(OnApplyMacroDirectly),
         flags,
         CommandManager::Options{}.AllowInMacros()
      ) );
   }

   return result;
}

}

// Menu definitions

// Under /MenuBar
namespace {
using namespace MenuTable;

const ReservedCommandFlag&
   HasLastGeneratorFlag() { static ReservedCommandFlag flag{
      [](const AudacityProject &project){
         return !MenuManager::Get( project ).mLastGenerator.empty();
      }
   }; return flag; }

BaseItemSharedPtr GenerateMenu()
{
   // All of this is a bit hacky until we can get more things connected into
   // the plugin manager...sorry! :-(

   using Options = CommandManager::Options;

   static BaseItemSharedPtr menu{
   ( FinderScope{ findCommandHandler },
   Menu( wxT("Generate"), XXO("&Generate"),
#ifdef EXPERIMENTAL_EFFECT_MANAGEMENT
      Section( "Manage",
         Command( wxT("ManageGenerators"), XXO("Add / Remove Plug-ins..."),
            FN(OnManageGenerators), AudioIONotBusyFlag() )
      ),
#endif

      Section("RepeatLast",
         // Delayed evaluation:
         [](AudacityProject &project)
         {
            const auto &lastGenerator = MenuManager::Get(project).mLastGenerator;
            TranslatableString buildMenuLabel;
            if (!lastGenerator.empty())
               buildMenuLabel = XO("Repeat %s")
                  .Format(EffectManager::Get().GetCommandName(lastGenerator));
            else
               buildMenuLabel = XO("Repeat Last Generator");

            return Command(wxT("RepeatLastGenerator"), buildMenuLabel,
               FN(OnRepeatLastGenerator),
               AudioIONotBusyFlag() |
                   HasLastGeneratorFlag(),
               Options{}.IsGlobal(), findCommandHandler);
         }
      ),

      Section( "Generators",
         // Delayed evaluation:
         [](AudacityProject &)
         { return Items( wxEmptyString, PopulateEffectsMenu(
            EffectTypeGenerate,
            AudioIONotBusyFlag(),
            AudioIONotBusyFlag())
         ); }
      )
   ) ) };
   return menu;
}

static const ReservedCommandFlag
&IsRealtimeNotActiveFlag() { static ReservedCommandFlag flag{
   [](const AudacityProject &){
      return !RealtimeEffectManager::Get().RealtimeIsActive();
   }
}; return flag; }  //lll

AttachedItem sAttachment1{
   wxT(""),
   Shared( GenerateMenu() )
};

const ReservedCommandFlag&
   HasLastEffectFlag() { static ReservedCommandFlag flag{
      [](const AudacityProject &project) {
         return !MenuManager::Get(project).mLastEffect.empty();
      }
   }; return flag;
}

BaseItemSharedPtr EffectMenu()
{
   // All of this is a bit hacky until we can get more things connected into
   // the plugin manager...sorry! :-(

   static BaseItemSharedPtr menu{
   ( FinderScope{ findCommandHandler },
   Menu( wxT("Effect"), XXO("Effe&ct"),
#ifdef EXPERIMENTAL_EFFECT_MANAGEMENT
      Section( "Manage",
         Command( wxT("ManageEffects"), XXO("Add / Remove Plug-ins..."),
            FN(OnManageEffects), AudioIONotBusyFlag() )
      ),
#endif

      Section( "RepeatLast",
         // Delayed evaluation:
         [](AudacityProject &project)
         {
            const auto &lastEffect = MenuManager::Get(project).mLastEffect;
            TranslatableString buildMenuLabel;
            if (!lastEffect.empty())
               buildMenuLabel = XO("Repeat %s")
                  .Format( EffectManager::Get().GetCommandName(lastEffect) );
            else
               buildMenuLabel = XO("Repeat Last Effect");

            return Command( wxT("RepeatLastEffect"), buildMenuLabel,
               FN(OnRepeatLastEffect),
               AudioIONotBusyFlag() | TimeSelectedFlag() |
                  WaveTracksSelectedFlag() | HasLastEffectFlag(),
               wxT("Ctrl+R"), findCommandHandler );
         }
      ),

      Section( "Effects",
         // Delayed evaluation:
         [](AudacityProject &)
         { return Items( wxEmptyString, PopulateEffectsMenu(
            EffectTypeProcess,
            AudioIONotBusyFlag() | TimeSelectedFlag() | WaveTracksSelectedFlag(),
            IsRealtimeNotActiveFlag() )
         ); }
      )
   ) ) };
   return menu;
}

AttachedItem sAttachment2{
   wxT(""),
   Shared( EffectMenu() )
};

const ReservedCommandFlag&
   HasLastAnalyzerFlag() { static ReservedCommandFlag flag{
      [](const AudacityProject &project) {
         if (MenuManager::Get(project).mLastAnalyzerRegistration == MenuCreator::repeattypeunique) return true;
         return !MenuManager::Get(project).mLastAnalyzer.empty();
      }
   }; return flag;
}

BaseItemSharedPtr AnalyzeMenu()
{
   // All of this is a bit hacky until we can get more things connected into
   // the plugin manager...sorry! :-(

   using Options = CommandManager::Options;

   static BaseItemSharedPtr menu{
   ( FinderScope{ findCommandHandler },
   Menu( wxT("Analyze"), XXO("&Analyze"),
#ifdef EXPERIMENTAL_EFFECT_MANAGEMENT
      Section( "Manage",
         Command( wxT("ManageAnalyzers"), XXO("Add / Remove Plug-ins..."),
            FN(OnManageAnalyzers), AudioIONotBusyFlag() )
      ),
#endif

      Section("RepeatLast",
         // Delayed evaluation:
         [](AudacityProject &project)
         {
            const auto &lastAnalyzer = MenuManager::Get(project).mLastAnalyzer;
            TranslatableString buildMenuLabel;
            if (!lastAnalyzer.empty())
               buildMenuLabel = XO("Repeat %s")
                  .Format(EffectManager::Get().GetCommandName(lastAnalyzer));
            else
               buildMenuLabel = XO("Repeat Last Analyzer");

            return Command(wxT("RepeatLastAnalyzer"), buildMenuLabel,
               FN(OnRepeatLastAnalyzer),
               AudioIONotBusyFlag() | TimeSelectedFlag() |
                  WaveTracksSelectedFlag() | HasLastAnalyzerFlag(),
               Options{}.IsGlobal(), findCommandHandler);
         }
      ),

      Section( "Analyzers",
         Items( "Windows" ),

         // Delayed evaluation:
         [](AudacityProject&)
         { return Items( wxEmptyString, PopulateEffectsMenu(
            EffectTypeAnalyze,
            AudioIONotBusyFlag() | TimeSelectedFlag() | WaveTracksSelectedFlag(),
            IsRealtimeNotActiveFlag() )
         ); }
      )
   ) ) };
   return menu;
}

AttachedItem sAttachment3{
   wxT(""),
   Shared( AnalyzeMenu() )
};

const ReservedCommandFlag&
   HasLastToolFlag() { static ReservedCommandFlag flag{
      [](const AudacityProject &project) {
      auto& menuManager = MenuManager::Get(project);
         if (menuManager.mLastToolRegistration == MenuCreator::repeattypeunique) return true;
         return !menuManager.mLastTool.empty();
      }
   }; return flag;
}

BaseItemSharedPtr ToolsMenu()
{
   using Options = CommandManager::Options;

   static BaseItemSharedPtr menu{
   ( FinderScope{ findCommandHandler },
   Menu( wxT("Tools"), XXO("T&ools"),
      Section( "Manage",
   #ifdef EXPERIMENTAL_EFFECT_MANAGEMENT
         Command( wxT("ManageTools"), XXO("Add / Remove Plug-ins..."),
            FN(OnManageTools), AudioIONotBusyFlag() ),

         //Separator(),

   #endif

         Section( "RepeatLast",
         // Delayed evaluation:
         [](AudacityProject &project)
         {
            const auto &lastTool = MenuManager::Get(project).mLastTool;
            TranslatableString buildMenuLabel;
            if (!lastTool.empty())
               buildMenuLabel = XO("Repeat %s")
                  .Format( EffectManager::Get().GetCommandName(lastTool) );
            else
               buildMenuLabel = XO("Repeat Last Tool");

            return Command( wxT("RepeatLastTool"), buildMenuLabel,
               FN(OnRepeatLastTool),
               AudioIONotBusyFlag() |
                  HasLastToolFlag(),
               Options{}.IsGlobal(), findCommandHandler );
         }
      ),

      Command( wxT("ManageMacros"), XXO("&Macros..."),
            FN(OnManageMacros), AudioIONotBusyFlag() ),

         Menu( wxT("Macros"), XXO("&Apply Macro"),
            // Palette has no access key to ensure first letter navigation of
            // sub menu
            Section( "",
               Command( wxT("ApplyMacrosPalette"), XXO("Palette..."),
                  FN(OnApplyMacrosPalette), AudioIONotBusyFlag() )
            ),

            Section( "",
               // Delayed evaluation:
               [](AudacityProject&)
               { return Items( wxEmptyString, PopulateMacrosMenu( AudioIONotBusyFlag() ) ); }
            )
         )
      ),

      Section( "Other",
         Command( wxT("ConfigReset"), XXO("Reset &Configuration"),
            FN(OnResetConfig),
            AudioIONotBusyFlag() ),

         Command( wxT("FancyScreenshot"), XXO("&Screenshot..."),
            FN(OnScreenshot), AudioIONotBusyFlag() ),

   // PRL: team consensus for 2.2.0 was, we let end users have this diagnostic,
   // as they used to in 1.3.x
   //#ifdef IS_ALPHA
         // TODO: What should we do here?  Make benchmark a plug-in?
         // Easy enough to do.  We'd call it mod-self-test.
         Command( wxT("Benchmark"), XXO("&Run Benchmark..."),
            FN(OnBenchmark), AudioIONotBusyFlag() )
   //#endif
      ),

      Section( "Tools",
         // Delayed evaluation:
         [](AudacityProject&)
         { return Items( wxEmptyString, PopulateEffectsMenu(
            EffectTypeTool,
            AudioIONotBusyFlag(),
            AudioIONotBusyFlag() )
         ); }
      )

#ifdef IS_ALPHA
      ,
      Section( "",
         Command( wxT("SimulateRecordingErrors"),
            XXO("Simulate Recording Errors"),
            FN(OnSimulateRecordingErrors),
            AudioIONotBusyFlag(),
            Options{}.CheckTest(
               [](AudacityProject&){
                  return AudioIO::Get()->mSimulateRecordingErrors; } ) ),
         Command( wxT("DetectUpstreamDropouts"),
            XXO("Detect Upstream Dropouts"),
            FN(OnDetectUpstreamDropouts),
            AudioIONotBusyFlag(),
            Options{}.CheckTest(
               [](AudacityProject&){
                  return AudioIO::Get()->mDetectUpstreamDropouts; } ) )
      )
#endif

#if defined(IS_ALPHA) || defined(END_USER_JOURNALLING)
      ,
      Section( "",
         Command( wxT("WriteJournal"),
            /* i18n-hint a "journal" is a text file that records
             the user's interactions with the application */
            XXO("Write Journal"),
            FN(OnWriteJournal),
            AlwaysEnabledFlag,
            Options{}.CheckTest( [](AudacityProject&){
               return Journal::RecordEnabled(); } ) )
      )
#endif

   ) ) };
   return menu;
}

AttachedItem sAttachment4{
   wxT(""),
   Shared( ToolsMenu() )
};

BaseItemSharedPtr ExtraScriptablesIMenu()
{
   // These are the more useful to VI user Scriptables.
   static BaseItemSharedPtr menu{
   ( FinderScope{ findCommandHandler },
   // i18n-hint: Scriptables are commands normally used from Python, Perl etc.
   Menu( wxT("Scriptables1"), XXO("Script&ables I"),
      // Note that the PLUGIN_SYMBOL must have a space between words,
      // whereas the short-form used here must not.
      // (So if you did write "CompareAudio" for the PLUGIN_SYMBOL name, then
      // you would have to use "Compareaudio" here.)
      Command( wxT("SelectTime"), XXO("Select Time..."), FN(OnAudacityCommand),
         AudioIONotBusyFlag() ),
      Command( wxT("SelectFrequencies"), XXO("Select Frequencies..."),
         FN(OnAudacityCommand),
         AudioIONotBusyFlag() ),
      Command( wxT("SelectTracks"), XXO("Select Tracks..."),
         FN(OnAudacityCommand),
         AudioIONotBusyFlag() ),
      Command( wxT("SetTrackStatus"), XXO("Set Track Status..."),
         FN(OnAudacityCommand),
         AudioIONotBusyFlag() ),
      Command( wxT("SetTrackAudio"), XXO("Set Track Audio..."),
         FN(OnAudacityCommand),
         AudioIONotBusyFlag() ),
      Command( wxT("SetTrackVisuals"), XXO("Set Track Visuals..."),
         FN(OnAudacityCommand),
         AudioIONotBusyFlag() ),
      Command( wxT("GetPreference"), XXO("Get Preference..."),
         FN(OnAudacityCommand),
         AudioIONotBusyFlag() ),
      Command( wxT("SetPreference"), XXO("Set Preference..."),
         FN(OnAudacityCommand),
         AudioIONotBusyFlag() ),
      Command( wxT("SetClip"), XXO("Set Clip..."), FN(OnAudacityCommand),
         AudioIONotBusyFlag() ),
      Command( wxT("SetEnvelope"), XXO("Set Envelope..."),
         FN(OnAudacityCommand),
         AudioIONotBusyFlag() ),
      Command( wxT("SetLabel"), XXO("Set Label..."), FN(OnAudacityCommand),
         AudioIONotBusyFlag() ),
      Command( wxT("SetProject"), XXO("Set Project..."), FN(OnAudacityCommand),
         AudioIONotBusyFlag() )
   ) ) };
   return menu;
}

AttachedItem sAttachment5{
   wxT("Optional/Extra/Part2"),
   Shared( ExtraScriptablesIMenu() )
};

BaseItemSharedPtr ExtraScriptablesIIMenu()
{
   // Less useful to VI users.
   static BaseItemSharedPtr menu{
   ( FinderScope{ findCommandHandler },
   // i18n-hint: Scriptables are commands normally used from Python, Perl etc.
   Menu( wxT("Scriptables2"), XXO("Scripta&bles II"),
      Command( wxT("Select"), XXO("Select..."), FN(OnAudacityCommand),
         AudioIONotBusyFlag() ),
      Command( wxT("SetTrack"), XXO("Set Track..."), FN(OnAudacityCommand),
         AudioIONotBusyFlag() ),
      Command( wxT("GetInfo"), XXO("Get Info..."), FN(OnAudacityCommand),
         AudioIONotBusyFlag() ),
      Command( wxT("Message"), XXO("Message..."), FN(OnAudacityCommand),
         AudioIONotBusyFlag() ),
      Command( wxT("Help"), XXO("Help..."), FN(OnAudacityCommand),
         AudioIONotBusyFlag() ),
      Command( wxT("Import2"), XXO("Import..."), FN(OnAudacityCommand),
         AudioIONotBusyFlag() ),
      Command( wxT("Export2"), XXO("Export..."), FN(OnAudacityCommand),
         AudioIONotBusyFlag() ),
      Command( wxT("OpenProject2"), XXO("Open Project..."),
         FN(OnAudacityCommand),
         AudioIONotBusyFlag() ),
      Command( wxT("SaveProject2"), XXO("Save Project..."),
         FN(OnAudacityCommand),
         AudioIONotBusyFlag() ),
      Command( wxT("Drag"), XXO("Move Mouse..."), FN(OnAudacityCommand),
         AudioIONotBusyFlag() ),
      Command( wxT("CompareAudio"), XXO("Compare Audio..."),
         FN(OnAudacityCommand),
         AudioIONotBusyFlag() ),
      // i18n-hint: Screenshot in the help menu has a much bigger dialog.
      Command( wxT("Screenshot"), XXO("Screenshot (short format)..."),
         FN(OnAudacityCommand),
         AudioIONotBusyFlag() )
   ) ) };
   return menu;
}

AttachedItem sAttachment6{
   wxT("Optional/Extra/Part2"),
   Shared( ExtraScriptablesIIMenu() )
};

}

#undef FN
