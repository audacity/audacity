#include "../Audacity.h"
#include "../Experimental.h"

#include "../AudacityApp.h"
#include "../AudioIO.h"
#include "../BatchProcessDialog.h"
#include "../Benchmark.h"
#include "../FreqWindow.h"
#include "../Menus.h"
#include "../PluginManager.h"
#include "../Prefs.h"
#include "../Project.h"
#include "../Screenshot.h"
#include "../TrackPanel.h"
#include "../WaveTrack.h"
#include "../commands/CommandContext.h"
#include "../commands/CommandManager.h"
#include "../commands/ScreenshotCommand.h"
#include "../effects/Contrast.h"
#include "../effects/EffectManager.h"

// private helper classes and functions
namespace {

void DoManagePluginsMenu
(AudacityProject &project, EffectType type)
{
   if (PluginManager::Get().ShowManager(&project, type))
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
   const std::vector<bool> &vHasDialog,
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

   wxString groupBy = gPrefs->Read(wxT("/Effects/GroupBy"), wxT("name"));

   bool grouped = false;
   if (groupBy.StartsWith(wxT("groupby")))
   {
      grouped = true;
   }

   std::vector<bool> vHasDialog;
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

         bool hasDialog = plug->GetSymbol().Msgid().Contains("...");
         auto name = plug->GetSymbol().Translation();

         if (plug->IsEffectInteractive())
         {
            name += wxT("...");
         }

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
               groupNames, vHasDialog,
               groupPlugs, groupFlags, isDefault);

            table.push_back( MenuOrItems(
               ( bInSubmenu ? last : wxString{} ), std::move( temp )
            ) );

            groupNames.clear();
            vHasDialog.clear();
            groupPlugs.clear();
            groupFlags.clear();
            last = current;
         }

         groupNames.push_back(name);
         vHasDialog.push_back(hasDialog);
         groupPlugs.push_back(plug->GetID());
         groupFlags.push_back(
            plug->IsEffectRealtime() ? realflags : batchflags);
      }

      if (groupNames.size() > 0)
      {
         using namespace MenuTable;
         BaseItemPtrs temp;
         bool bInSubmenu = groupNames.size() > 1;

         AddEffectMenuItemGroup(temp,
            groupNames, vHasDialog, groupPlugs, groupFlags, isDefault);

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

         bool hasDialog = plug->GetSymbol().Msgid().Contains("...");
         auto name = plug->GetSymbol().Translation();

         if (plug->IsEffectInteractive())
         {
            name += wxT("...");
         }

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

         if (!group.empty())
         {
            group += wxT(": ");
         }

         groupNames.push_back(group + name);
         vHasDialog.push_back(hasDialog);
         groupPlugs.push_back(plug->GetID());
         groupFlags.push_back(plug->IsEffectRealtime() ? realflags : batchflags);
      }

      if (groupNames.size() > 0)
      {
         AddEffectMenuItemGroup(
            table, groupNames, vHasDialog, groupPlugs, groupFlags, isDefault);
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

   const PluginDescriptor *plug = pm.GetFirstPluginForEffectType(type);
   while (plug)
   {
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

   wxString groupby = gPrefs->Read(wxT("/Effects/GroupBy"), wxT("name"));

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

// exported helper functions

/// DoEffect() takes a PluginID and has the EffectManager execute the associated
/// effect.
///
/// At the moment flags are used only to indicate whether to prompt for
//  parameters, whether to save the state to history and whether to allow
/// 'Repeat Last Effect'.
bool DoEffect(
   const PluginID & ID, const CommandContext &context, unsigned flags )
{
   AudacityProject &project = context.project;
   auto tracks = project.GetTracks();
   auto trackPanel = project.GetTrackPanel();
   auto trackFactory = project.GetTrackFactory();
   auto rate = project.GetRate();
   auto &selectedRegion = project.GetViewInfo().selectedRegion;
   auto commandManager = project.GetCommandManager();

   const PluginDescriptor *plug = PluginManager::Get().GetPlugin(ID);
   if (!plug)
      return false;

   EffectType type = plug->GetEffectType();

   // Make sure there's no activity since the effect is about to be applied
   // to the project's tracks.  Mainly for Apply during RTP, but also used
   // for batch commands
   if (flags & kConfigured)
   {
      TransportActions::DoStop(project);
      project.SelectAllIfNone();
   }

   wxGetApp().SetMissingAliasedFileWarningShouldShow(true);

   auto nTracksOriginally = project.GetTrackCount();
   wxWindow *focus = wxWindow::FindFocus();
   wxWindow *parent = nullptr;
   if (focus != nullptr) {
      parent = focus->GetParent();
   }

   bool success = false;
   auto cleanup = finally( [&] {

      if (!success) {
         // For now, we're limiting realtime preview to a single effect, so
         // make sure the menus reflect that fact that one may have just been
         // opened.
         GetMenuManager(project).UpdateMenus(project, false);
      }

   } );

   int count = 0;
   bool clean = true;
   for (auto t : tracks->Selected< const WaveTrack >()) {
      if (t->GetEndTime() != 0.0)
         clean = false;
      count++;
   }

   EffectManager & em = EffectManager::Get();

   success = em.DoEffect(ID, &project, rate,
      tracks, trackFactory, &selectedRegion,
      (flags & kConfigured) == 0);

   if (!success)
      return false;

   if (em.GetSkipStateFlag())
      flags = flags | kSkipState;

   if (!(flags & kSkipState))
   {
      wxString shortDesc = em.GetCommandName(ID);
      wxString longDesc = em.GetCommandDescription(ID);
      project.PushState(longDesc, shortDesc);
   }

   if (!(flags & kDontRepeatLast))
   {
      // Only remember a successful effect, don't remember insert,
      // or analyze effects.
      if (type == EffectTypeProcess) {
         wxString shortDesc = em.GetCommandName(ID);
         GetMenuManager(project).mLastEffect = ID;
         wxString lastEffectDesc;
         /* i18n-hint: %s will be the name of the effect which will be
          * repeated if this menu item is chosen */
         lastEffectDesc.Printf(_("Repeat %s"), shortDesc);
         commandManager->Modify(wxT("RepeatLastEffect"), lastEffectDesc);
      }
   }

   //STM:
   //The following automatically re-zooms after sound was generated.
   // IMO, it was disorienting, removing to try out without re-fitting
   //mchinen:12/14/08 reapplying for generate effects
   if (type == EffectTypeGenerate)
   {
      if (count == 0 || (clean && selectedRegion.t0() == 0.0))
         ViewActions::DoZoomFit(project);
         //  trackPanel->Refresh(false);
   }
   project.RedrawProject();
   if (focus != nullptr && focus->GetParent()==parent) {
      focus->SetFocus();
   }

   // A fix for Bug 63
   // New tracks added?  Scroll them into view so that user sees them.
   // Don't care what track type.  An analyser might just have added a
   // Label track and we want to see it.
   if( project.GetTrackCount() > nTracksOriginally ){
      // 0.0 is min scroll position, 1.0 is max scroll position.
      trackPanel->VerticalScroll( 1.0 );
   }  else {
      trackPanel->EnsureVisible(trackPanel->GetFirstSelectedTrack());
      trackPanel->Refresh(false);
   }

   return true;
}

/// DoAudacityCommand() takes a PluginID and executes the assocated command.
///
/// At the moment flags are used only to indicate whether to prompt for
/// parameters
bool DoAudacityCommand(
   const PluginID & ID, const CommandContext & context, unsigned flags )
{
   auto &project = context.project;
   const PluginDescriptor *plug = PluginManager::Get().GetPlugin(ID);
   if (!plug)
      return false;

   if (flags & kConfigured)
   {
      TransportActions::DoStop(project);
//    SelectAllIfNone();
   }

   EffectManager & em = EffectManager::Get();
   bool success = em.DoAudacityCommand(ID, 
      context,
      &project,
      (flags & kConfigured) == 0);

   if (!success)
      return false;

/*
   if (em.GetSkipStateFlag())
      flags = flags | OnEffectFlags::kSkipState;

   if (!(flags & OnEffectFlags::kSkipState))
   {
      wxString shortDesc = em.GetCommandName(ID);
      wxString longDesc = em.GetCommandDescription(ID);
      PushState(longDesc, shortDesc);
   }
*/
   project.RedrawProject();
   return true;
}

// Menu handler functions

struct Handler : CommandHandlerObject {

void OnManageGenerators(const CommandContext &context)
{
   auto &project = context.project;
   DoManagePluginsMenu(project, EffectTypeGenerate);
}

void OnEffect(const CommandContext &context)
{
   DoEffect(context.parameter, context, 0);
}

void OnManageEffects(const CommandContext &context)
{
   auto &project = context.project;
   DoManagePluginsMenu(project, EffectTypeProcess);
}

void OnRepeatLastEffect(const CommandContext &context)
{
   auto lastEffect = GetMenuManager(context.project).mLastEffect;
   if (!lastEffect.empty())
   {
      DoEffect( lastEffect, context, kConfigured );
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
   auto contrastDialog = project.GetContrastDialog(true);


   contrastDialog->CentreOnParent();
   if( ScreenshotCommand::MayCapture( contrastDialog ) )
      return;
   contrastDialog->Show();
}

void OnPlotSpectrum(const CommandContext &context)
{
   auto &project = context.project;
   auto freqWindow = project.GetFreqWindow(true);


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
   project.GetMacrosWindow( true, true );
}

void OnApplyMacrosPalette(const CommandContext &context )
{
   auto &project = context.project;
   project.GetMacrosWindow( false, true );
}

void OnScreenshot(const CommandContext &WXUNUSED(context) )
{
   ::OpenScreenshotTools();
}

void OnBenchmark(const CommandContext &context)
{
   auto &project = context.project;
   ::RunBenchmark(&project);
}

void OnSimulateRecordingErrors(const CommandContext &context)
{
   auto &project = context.project;
   auto commandManager = project.GetCommandManager();

   bool &setting = gAudioIO->mSimulateRecordingErrors;
   commandManager->Check(wxT("SimulateRecordingErrors"), !setting);
   setting = !setting;
}

void OnDetectUpstreamDropouts(const CommandContext &context)
{
   auto &project = context.project;
   auto commandManager = project.GetCommandManager();

   bool &setting = gAudioIO->mDetectUpstreamDropouts;
   commandManager->Check(wxT("DetectUpstreamDropouts"), !setting);
   setting = !setting;
}

void OnApplyMacroDirectly(const CommandContext &context )
{
   auto &project = context.project;

   //wxLogDebug( "Macro was: %s", context.parameter);
   ApplyMacroDialog dlg( &project );
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
   wxLogDebug( "Command was: %s", ctx.parameter);
   // Not configured, so prompt user.
   DoAudacityCommand(EffectManager::Get().GetEffectByIdentifier(ctx.parameter),
      ctx, kNone);
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
#define XXO(X) _(X), wxString{X}.Contains("...")

// ... buf first some more helper definitions, which use FN
namespace {

void AddEffectMenuItemGroup(
   MenuTable::BaseItemPtrs &table,
   const wxArrayString & names,
   const std::vector<bool> &vHasDialog,
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
                  item,
                  item.Contains("..."),
                  FN(OnEffect),
                  flags[i],
                  CommandManager::Options{}
                     .IsEffect().Parameter( plugs[i] ) ) );

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
               names[i],
               vHasDialog[i],
               FN(OnEffect),
               flags[i],
               CommandManager::Options{}
                  .IsEffect().Parameter( plugs[i] ) ) );
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
         }
         groupNdx++;
      }
   }

   return;
}

MenuTable::BaseItemPtrs PopulateMacrosMenu( CommandFlag flags  )
{
   MenuTable::BaseItemPtrs result;
   auto names = MacroCommands::GetNames();
   int i;

   for (i = 0; i < (int)names.size(); i++) {
      auto MacroID = ApplyMacroDialog::MacroIdOfName( names[i] );
      result.push_back( MenuTable::Command( MacroID,
         names[i], false, FN(OnApplyMacroDirectly),
         flags ) );
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

MenuTable::BaseItemPtr EffectMenu( AudacityProject &project )
{
   using namespace MenuTable;
   // All of this is a bit hacky until we can get more things connected into
   // the plugin manager...sorry! :-(

   const auto &lastEffect = GetMenuManager(project).mLastEffect;
   wxString buildMenuLabel;
   if (!lastEffect.empty()) {
      buildMenuLabel.Printf(_("Repeat %s"),
         EffectManager::Get().GetCommandName(lastEffect));
   }
   else
      buildMenuLabel = _("Repeat Last Effect");

   return Menu( _("Effe&ct"),
#ifdef EXPERIMENTAL_EFFECT_MANAGEMENT
      Command( wxT("ManageEffects"), XXO("Add / Remove Plug-ins..."),
         FN(OnManageEffects), AudioIONotBusyFlag ),

      Separator(),

#endif
      Command( wxT("RepeatLastEffect"), buildMenuLabel, false,
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

#undef XXO
#undef FN
