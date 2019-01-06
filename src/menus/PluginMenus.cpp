#include "../Audacity.h"
#include "../Experimental.h"

#include "../AudioIO.h"
#include "../BatchProcessDialog.h"
#include "../Benchmark.h"
#include "../CommonCommandFlags.h"
#include "../FreqWindow.h"
#include "../Menus.h"
#include "../PluginManager.h"
#include "../Prefs.h"
#include "../Project.h"
#include "../ProjectWindow.h"
#include "../Screenshot.h"
#include "../commands/CommandContext.h"
#include "../commands/CommandManager.h"
#include "../commands/ScreenshotCommand.h"
#include "../effects/Contrast.h"
#include "../effects/EffectManager.h"
#include "../effects/RealtimeEffectManager.h"
#include "../prefs/EffectsPrefs.h"

// private helper classes and functions
namespace {

AudacityProject::AttachedWindows::RegisteredFactory sContrastDialogKey{
   []( AudacityProject &parent ) -> wxWeakRef< wxWindow > {
      auto &window = ProjectWindow::Get( parent );
      return safenew ContrastDialog(
         &window, -1, _("Contrast Analysis (WCAG 2 compliance)"),
         wxPoint{ 150, 150 }
      );
   }
};

AudacityProject::AttachedWindows::RegisteredFactory sFrequencyWindowKey{
   []( AudacityProject &parent ) -> wxWeakRef< wxWindow > {
      auto &window = ProjectWindow::Get( parent );
      return safenew FrequencyPlotDialog(
         &window, -1, _("Frequency Analysis"),
         wxPoint{ 150, 150 }
      );
   }
};

AudacityProject::AttachedWindows::RegisteredFactory sMacrosWindowKey{
   []( AudacityProject &parent ) -> wxWeakRef< wxWindow > {
      auto &window = ProjectWindow::Get( parent );
      return safenew MacrosWindow(
         &window, true
      );
   }
};

void DoManagePluginsMenu(AudacityProject &project, EffectType type)
{
   auto &window = GetProjectFrame( project );
   if (PluginManager::Get().ShowManager(&window, type))
      MenuCreator::RebuildAllMenuBars();
}

bool CompareEffectsByName(const PluginDescriptor *a, const PluginDescriptor *b)
{
   auto akey = a->GetSymbol().Translation();
   auto bkey = b->GetSymbol().Translation();

   akey += a->GetPath();
   bkey += b->GetPath();

   return akey.CmpNoCase(bkey) < 0;
}

bool CompareEffectsByPublisher(
   const PluginDescriptor *a, const PluginDescriptor *b)
{
   auto &em = EffectManager::Get();
   auto akey = em.GetVendorName(a->GetID());
   auto bkey = em.GetVendorName(b->GetID());

   if (akey.empty())
   {
      akey = _("Uncategorized");
   }
   if (bkey.empty())
   {
      bkey = _("Uncategorized");
   }

   akey += a->GetSymbol().Translation();
   bkey += b->GetSymbol().Translation();

   akey += a->GetPath();
   bkey += b->GetPath();

   return akey.CmpNoCase(bkey) < 0;
}

bool CompareEffectsByPublisherAndName(
   const PluginDescriptor *a, const PluginDescriptor *b)
{
   auto &em = EffectManager::Get();
   auto akey = em.GetVendorName(a->GetID());
   auto bkey = em.GetVendorName(b->GetID());

   if (a->IsEffectDefault())
   {
      akey = wxEmptyString;
   }
   if (b->IsEffectDefault())
   {
      bkey = wxEmptyString;
   }

   akey += a->GetSymbol().Translation();
   bkey += b->GetSymbol().Translation();

   akey += a->GetPath();
   bkey += b->GetPath();

   return akey.CmpNoCase(bkey) < 0;
}

bool CompareEffectsByTypeAndName(
   const PluginDescriptor *a, const PluginDescriptor *b)
{
   auto &em = EffectManager::Get();
   auto akey = em.GetEffectFamilyName(a->GetID());
   auto bkey = em.GetEffectFamilyName(b->GetID());

   if (akey.empty())
   {
      akey = _("Uncategorized");
   }
   if (bkey.empty())
   {
      bkey = _("Uncategorized");
   }

   if (a->IsEffectDefault())
   {
      akey = wxEmptyString;
   }
   if (b->IsEffectDefault())
   {
      bkey = wxEmptyString;
   }

   akey += a->GetSymbol().Translation();
   bkey += b->GetSymbol().Translation();

   akey += a->GetPath();
   bkey += b->GetPath();

   return akey.CmpNoCase(bkey) < 0;
}

bool CompareEffectsByType(const PluginDescriptor *a, const PluginDescriptor *b)
{
   auto &em = EffectManager::Get();
   auto akey = em.GetEffectFamilyName(a->GetID());
   auto bkey = em.GetEffectFamilyName(b->GetID());

   if (akey.empty())
   {
      akey = _("Uncategorized");
   }
   if (bkey.empty())
   {
      bkey = _("Uncategorized");
   }

   akey += a->GetSymbol().Translation();
   bkey += b->GetSymbol().Translation();

   akey += a->GetPath();
   bkey += b->GetPath();

   return akey.CmpNoCase(bkey) < 0;
}

// Forward-declared function has its definition below with OnEffect in view
void AddEffectMenuItemGroup(
   MenuTable::BaseItemPtrs &table,
   const wxArrayString & names,
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
            ( batchflags | NoiseReductionTimeSelectedFlag ) & ~TimeSelectedFlag;
      return batchflags;
   };

   wxArrayString groupNames;
   PluginIDs groupPlugs;
   std::vector<CommandFlag> groupFlags;
   if (grouped)
   {
      wxString last;
      wxString current;

      for (size_t i = 0; i < pluginCnt; i++)
      {
         const PluginDescriptor *plug = plugs[i];

         auto name = plug->GetSymbol().Translation();

         if (plug->IsEffectInteractive())
            name += _("...");

         if (groupBy == wxT("groupby:publisher"))
         {
            current = EffectManager::Get().GetVendorName(plug->GetID());
            if (current.empty())
            {
               current = _("Unknown");
            }
         }
         else if (groupBy == wxT("groupby:type"))
         {
            current = EffectManager::Get().GetEffectFamilyName(plug->GetID());
            if (current.empty())
            {
               current = _("Unknown");
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

            table.push_back( MenuOrItems(
               ( bInSubmenu ? last : wxString{} ), std::move( temp )
            ) );

            groupNames.clear();
            groupPlugs.clear();
            groupFlags.clear();
            last = current;
         }

         groupNames.push_back(name);
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

         table.push_back( MenuOrItems(
            ( bInSubmenu ? current : wxString{} ), std::move( temp )
         ) );
      }
   }
   else
   {
      for (size_t i = 0; i < pluginCnt; i++)
      {
         const PluginDescriptor *plug = plugs[i];

         auto name = plug->GetSymbol().Translation();

         if (plug->IsEffectInteractive())
            name += _("...");

         wxString group;
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
            group = wxEmptyString;
         }

         groupNames.push_back(
            group.empty()
               ? name
               : wxString::Format(_("%s: %s"), group, name)
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
   const PluginDescriptor *plug = pm.GetFirstPluginForEffectType(type);
   while (plug)
   {
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
         && (plug->GetSymbol() != ComponentInterfaceSymbol("Nyquist Prompt"))
#endif
         )
         defplugs.push_back(plug);
      else
         optplugs.push_back(plug);
      plug = pm.GetNextPluginForEffectType(type);
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

   AddEffectMenuItems( result, defplugs, batchflags, realflags, true );

   if (defplugs.size() && optplugs.size())
      result.push_back( MenuTable::Separator() );

   AddEffectMenuItems( result, optplugs, batchflags, realflags, false );

   return result;
}

// Forward-declared function has its definition below with OnApplyMacroDirectly
// in view
MenuTable::BaseItemPtrs PopulateMacrosMenu( CommandFlag flags  );

}

namespace PluginActions {

// Menu handler functions

struct Handler : CommandHandlerObject {

void OnManageGenerators(const CommandContext &context)
{
   auto &project = context.project;
   DoManagePluginsMenu(project, EffectTypeGenerate);
}

void OnEffect(const CommandContext &context)
{
   // using GET to interpret parameter as a PluginID
   EffectManager::DoEffect(context.parameter.GET(), context, 0);
}

void OnManageEffects(const CommandContext &context)
{
   auto &project = context.project;
   DoManagePluginsMenu(project, EffectTypeProcess);
}

void OnRepeatLastEffect(const CommandContext &context)
{
   auto lastEffect = MenuManager::Get(context.project).mLastEffect;
   if (!lastEffect.empty())
   {
      EffectManager::DoEffect(
         lastEffect, context, EffectManager::kConfigured );
   }
}

void OnManageAnalyzers(const CommandContext &context)
{
   auto &project = context.project;
   DoManagePluginsMenu(project, EffectTypeAnalyze);
}

void OnContrast(const CommandContext &context)
{
   auto &project = context.project;
   auto contrastDialog =
      &project.AttachedWindows::Get< ContrastDialog >( sContrastDialogKey );

   contrastDialog->CentreOnParent();
   if( ScreenshotCommand::MayCapture( contrastDialog ) )
      return;
   contrastDialog->Show();
}

void OnPlotSpectrum(const CommandContext &context)
{
   auto &project = context.project;
   auto freqWindow =
      &project.AttachedWindows::Get< FrequencyPlotDialog >( sFrequencyWindowKey );

   if( ScreenshotCommand::MayCapture( freqWindow ) )
      return;
   freqWindow->Show(true);
   freqWindow->Raise();
   freqWindow->SetFocus();
}

void OnManageTools(const CommandContext &context )
{
   auto &project = context.project;
   DoManagePluginsMenu(project, EffectTypeTool);
}

void OnManageMacros(const CommandContext &context )
{
   auto &project = context.project;
   auto macrosWindow =
      &project.AttachedWindows::Get< MacrosWindow >( sMacrosWindowKey );
   if (macrosWindow) {
      macrosWindow->Show();
      macrosWindow->Raise();
      macrosWindow->UpdateDisplay( true );
   }
}

void OnApplyMacrosPalette(const CommandContext &context )
{
   auto &project = context.project;
   auto macrosWindow =
      &project.AttachedWindows::Get< MacrosWindow >( sMacrosWindowKey );
   if (macrosWindow) {
      macrosWindow->Show();
      macrosWindow->Raise();
      macrosWindow->UpdateDisplay( false );
   }
}

void OnScreenshot(const CommandContext &WXUNUSED(context) )
{
   ::OpenScreenshotTools();
}

void OnBenchmark(const CommandContext &context)
{
   auto &project = context.project;
   auto &window = GetProjectFrame( project );
   ::RunBenchmark(&window);
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

void OnApplyMacroDirectly(const CommandContext &context )
{
   auto &project = context.project;
   auto &window = ProjectWindow::Get( project );

   //wxLogDebug( "Macro was: %s", context.parameter);
   ApplyMacroDialog dlg( &window );
   const auto &Name = context.parameter;

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
   MenuManager::ModifyUndoMenuItems( project );
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

#define FN(X) findCommandHandler, \
   static_cast<CommandFunctorPointer>(& PluginActions::Handler :: X)

// ... buf first some more helper definitions, which use FN
namespace {

void AddEffectMenuItemGroup(
   MenuTable::BaseItemPtrs &table,
   const wxArrayString & names,
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
      while (i + 1 < namesCnt && names[i] == names[i + 1])
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

      if (i + 1 < namesCnt && names[i] == names[i + 1])
      {
         // collect a sub-menu for like-named items
         const auto name = names[i];
         BaseItemPtrs temp2;
         while (i < namesCnt && names[i] == name)
         {
            const PluginDescriptor *plug =
               PluginManager::Get().GetPlugin(plugs[i]);
            wxString item = plug->GetPath();
            if( plug->GetPluginType() == PluginTypeEffect )
               temp2.push_back( Command( item,
                  TranslatableString{ item },
                  FN(OnEffect),
                  flags[i],
                  CommandManager::Options{}
                     .IsEffect()
                     .AllowInMacros()
                     .Parameter( plugs[i] ) ) );

            i++;
         }
         pTable->push_back( Menu( name, std::move( temp2 ) ) );
         i--;
      }
      else
      {
         // collect one item
         const PluginDescriptor *plug =
            PluginManager::Get().GetPlugin(plugs[i]);
         if( plug->GetPluginType() == PluginTypeEffect )
            pTable->push_back( Command( names[i],
               TranslatableString{ names[i] },
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
            table.push_back( Menu(
               wxString::Format(_("Plug-in %d to %d"), groupNdx + 1, end),
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

   for (i = 0; i < (int)names.size(); i++) {
      auto MacroID = ApplyMacroDialog::MacroIdOfName( names[i] );
      result.push_back( MenuTable::Command( MacroID,
         TranslatableString{ names[i] }, // file name verbatim
         FN(OnApplyMacroDirectly),
         flags,
         CommandManager::Options{}.AllowInMacros()
      ) );
   }

   return result;
}

}

// Menu definitions

MenuTable::BaseItemPtr GenerateMenu( AudacityProject & )
{
   using namespace MenuTable;
   // All of this is a bit hacky until we can get more things connected into
   // the plugin manager...sorry! :-(

   return Menu( _("&Generate"),
#ifdef EXPERIMENTAL_EFFECT_MANAGEMENT
      Command( wxT("ManageGenerators"), XXO("Add / Remove Plug-ins..."),
         FN(OnManageGenerators), AudioIONotBusyFlag ),

      Separator(),

#endif

      Items( PopulateEffectsMenu(
         EffectTypeGenerate,
         AudioIONotBusyFlag,
         AudioIONotBusyFlag) )
   );
}

const ReservedCommandFlag
   IsRealtimeNotActiveFlag{
      [](const AudacityProject &){
         return !RealtimeEffectManager::Get().RealtimeIsActive();
      }
   };  //lll

MenuTable::BaseItemPtr EffectMenu( AudacityProject &project )
{
   using namespace MenuTable;
   // All of this is a bit hacky until we can get more things connected into
   // the plugin manager...sorry! :-(

   const auto &lastEffect = MenuManager::Get(project).mLastEffect;
   TranslatableString buildMenuLabel;
   if (!lastEffect.empty())
      buildMenuLabel = XO("Repeat %s")
         .Format( EffectManager::Get().GetCommandName(lastEffect) );
   else
      buildMenuLabel = XO("Repeat Last Effect");

   return Menu( _("Effe&ct"),
#ifdef EXPERIMENTAL_EFFECT_MANAGEMENT
      Command( wxT("ManageEffects"), XXO("Add / Remove Plug-ins..."),
         FN(OnManageEffects), AudioIONotBusyFlag ),

      Separator(),

#endif
      Command( wxT("RepeatLastEffect"), buildMenuLabel,
         FN(OnRepeatLastEffect),
         AudioIONotBusyFlag | TimeSelectedFlag |
            WaveTracksSelectedFlag | HasLastEffectFlag,
         wxT("Ctrl+R") ),

      Separator(),

      Items( PopulateEffectsMenu(
         EffectTypeProcess,
         AudioIONotBusyFlag | TimeSelectedFlag | WaveTracksSelectedFlag,
         IsRealtimeNotActiveFlag ) )
   );
}

MenuTable::BaseItemPtr AnalyzeMenu( AudacityProject & )
{
   using namespace MenuTable;
   // All of this is a bit hacky until we can get more things connected into
   // the plugin manager...sorry! :-(

   return Menu( _("&Analyze"),
#ifdef EXPERIMENTAL_EFFECT_MANAGEMENT
      Command( wxT("ManageAnalyzers"), XXO("Add / Remove Plug-ins..."),
         FN(OnManageAnalyzers), AudioIONotBusyFlag ),

      Separator(),

#endif

      Command( wxT("ContrastAnalyser"), XXO("Contrast..."), FN(OnContrast),
         AudioIONotBusyFlag | WaveTracksSelectedFlag | TimeSelectedFlag,
         wxT("Ctrl+Shift+T") ),
      Command( wxT("PlotSpectrum"), XXO("Plot Spectrum..."), FN(OnPlotSpectrum),
         AudioIONotBusyFlag | WaveTracksSelectedFlag | TimeSelectedFlag ),

      Items( PopulateEffectsMenu(
         EffectTypeAnalyze,
         AudioIONotBusyFlag | TimeSelectedFlag | WaveTracksSelectedFlag,
         IsRealtimeNotActiveFlag ) )
   );
}

MenuTable::BaseItemPtr ToolsMenu( AudacityProject & )
{
   using namespace MenuTable;
   using Options = CommandManager::Options;
   auto gAudioIO = AudioIO::Get();

   return Menu( _("T&ools"),

#ifdef EXPERIMENTAL_EFFECT_MANAGEMENT
      Command( wxT("ManageTools"), XXO("Add / Remove Plug-ins..."),
         FN(OnManageTools), AudioIONotBusyFlag ),

      //Separator(),

#endif

      Command( wxT("ManageMacros"), XXO("&Macros..."),
         FN(OnManageMacros), AudioIONotBusyFlag ),

      Menu( _("&Apply Macro"),
         // Palette has no access key to ensure first letter navigation of
         // sub menu
         Command( wxT("ApplyMacrosPalette"), XXO("Palette..."),
            FN(OnApplyMacrosPalette), AudioIONotBusyFlag ),

         Separator(),

         Items( PopulateMacrosMenu( AudioIONotBusyFlag ) )
      ),

      Separator(),

      Command( wxT("FancyScreenshot"), XXO("&Screenshot..."),
         FN(OnScreenshot), AudioIONotBusyFlag ),

// PRL: team consensus for 2.2.0 was, we let end users have this diagnostic,
// as they used to in 1.3.x
//#ifdef IS_ALPHA
      // TODO: What should we do here?  Make benchmark a plug-in?
      // Easy enough to do.  We'd call it mod-self-test.
      Command( wxT("Benchmark"), XXO("&Run Benchmark..."),
         FN(OnBenchmark), AudioIONotBusyFlag ),
//#endif

      Separator(),

      Items( PopulateEffectsMenu(
         EffectTypeTool,
         AudioIONotBusyFlag,
         AudioIONotBusyFlag ) )

#ifdef IS_ALPHA
      ,

      Separator(),

      Command( wxT("SimulateRecordingErrors"),
         XXO("Simulate Recording Errors"),
         FN(OnSimulateRecordingErrors),
         AudioIONotBusyFlag,
         Options{}.CheckState( gAudioIO->mSimulateRecordingErrors ) ),
      Command( wxT("DetectUpstreamDropouts"),
         XXO("Detect Upstream Dropouts"),
         FN(OnDetectUpstreamDropouts),
         AudioIONotBusyFlag,
         Options{}.CheckState( gAudioIO->mDetectUpstreamDropouts ) )
#endif
   );
}

MenuTable::BaseItemPtr ExtraScriptablesIMenu( AudacityProject & )
{
   using namespace MenuTable;

   // These are the more useful to VI user Scriptables.
   // i18n-hint: Scriptables are commands normally used from Python, Perl etc.
   return Menu( _("Script&ables I"),
      // Note that the PLUGIN_SYMBOL must have a space between words,
      // whereas the short-form used here must not.
      // (So if you did write "CompareAudio" for the PLUGIN_SYMBOL name, then
      // you would have to use "Compareaudio" here.)
      Command( wxT("SelectTime"), XXO("Select Time..."), FN(OnAudacityCommand),
         AudioIONotBusyFlag ),
      Command( wxT("SelectFrequencies"), XXO("Select Frequencies..."),
         FN(OnAudacityCommand),
         AudioIONotBusyFlag ),
      Command( wxT("SelectTracks"), XXO("Select Tracks..."),
         FN(OnAudacityCommand),
         AudioIONotBusyFlag ),
      Command( wxT("SetTrackStatus"), XXO("Set Track Status..."),
         FN(OnAudacityCommand),
         AudioIONotBusyFlag ),
      Command( wxT("SetTrackAudio"), XXO("Set Track Audio..."),
         FN(OnAudacityCommand),
         AudioIONotBusyFlag ),
      Command( wxT("SetTrackVisuals"), XXO("Set Track Visuals..."),
         FN(OnAudacityCommand),
         AudioIONotBusyFlag ),
      Command( wxT("GetPreference"), XXO("Get Preference..."),
         FN(OnAudacityCommand),
         AudioIONotBusyFlag ),
      Command( wxT("SetPreference"), XXO("Set Preference..."),
         FN(OnAudacityCommand),
         AudioIONotBusyFlag ),
      Command( wxT("SetClip"), XXO("Set Clip..."), FN(OnAudacityCommand),
         AudioIONotBusyFlag ),
      Command( wxT("SetEnvelope"), XXO("Set Envelope..."),
         FN(OnAudacityCommand),
         AudioIONotBusyFlag ),
      Command( wxT("SetLabel"), XXO("Set Label..."), FN(OnAudacityCommand),
         AudioIONotBusyFlag ),
      Command( wxT("SetProject"), XXO("Set Project..."), FN(OnAudacityCommand),
         AudioIONotBusyFlag )
   );
}

MenuTable::BaseItemPtr ExtraScriptablesIIMenu( AudacityProject & )
{
   using namespace MenuTable;

   // Less useful to VI users.
   return Menu( _("Scripta&bles II"),
      Command( wxT("Select"), XXO("Select..."), FN(OnAudacityCommand),
         AudioIONotBusyFlag ),
      Command( wxT("SetTrack"), XXO("Set Track..."), FN(OnAudacityCommand),
         AudioIONotBusyFlag ),
      Command( wxT("GetInfo"), XXO("Get Info..."), FN(OnAudacityCommand),
         AudioIONotBusyFlag ),
      Command( wxT("Message"), XXO("Message..."), FN(OnAudacityCommand),
         AudioIONotBusyFlag ),
      Command( wxT("Help"), XXO("Help..."), FN(OnAudacityCommand),
         AudioIONotBusyFlag ),
      Command( wxT("Import2"), XXO("Import..."), FN(OnAudacityCommand),
         AudioIONotBusyFlag ),
      Command( wxT("Export2"), XXO("Export..."), FN(OnAudacityCommand),
         AudioIONotBusyFlag ),
      Command( wxT("OpenProject2"), XXO("Open Project..."),
         FN(OnAudacityCommand),
         AudioIONotBusyFlag ),
      Command( wxT("SaveProject2"), XXO("Save Project..."),
         FN(OnAudacityCommand),
         AudioIONotBusyFlag ),
      Command( wxT("Drag"), XXO("Move Mouse..."), FN(OnAudacityCommand),
         AudioIONotBusyFlag ),
      Command( wxT("CompareAudio"), XXO("Compare Audio..."),
         FN(OnAudacityCommand),
         AudioIONotBusyFlag ),
      // i18n-hint: Screenshot in the help menu has a much bigger dialog.
      Command( wxT("Screenshot"), XXO("Screenshot (short format)..."),
         FN(OnAudacityCommand),
         AudioIONotBusyFlag )
   );
}

#undef FN
