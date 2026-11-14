/**********************************************************************

  Audacity: A Digital Audio Editor

  MacroCommands.cpp

  Dominic Mazzoni
  James Crook

********************************************************************//*!

\class MacroCommands
\brief Maintains the list of commands for batch/macro
processing.  See also MacrosWindow and ApplyMacroDialog.

*//*******************************************************************/
#define wxLOG_COMPONENT "MacroCommands"

#include "BatchCommands.h"
#include "DoEffect.h"
#include "EffectAndCommandPluginManager.h"

#include <wx/datetime.h>
#include <wx/defs.h>
#include <wx/dir.h>
#include <wx/frame.h>
#include <wx/log.h>
#include <wx/textfile.h>
#include <wx/time.h>

#include "Effect.h"
#include "EffectManager.h"
#include "FileNames.h"
#include "PluginManager.h"
#include "Prefs.h"
#include "Project.h"
#include "ProjectHistory.h"
#include "ProjectSettings.h"
#include "ProjectWindows.h"
#include "SelectFile.h"
#include "SelectUtilities.h"
#include "SettingsVisitor.h"
#include "Track.h"
#include "UndoManager.h"
#include "effects/EffectPresetDialog.h"
#include "effects/EffectUI.h"
#include "effects/EffectUIServices.h"

#include "AllThemeResources.h"

#include "AudacityMessageBox.h"

#include "CommandContext.h"
#include "commands/CommandDispatch.h"
#include "CommandManager.h"

MacroCommands::MacroCommands(AudacityProject& project)
    : mProject{project}
{
    ResetMacro();

    auto names = GetNames();
    auto defaults = GetNamesOfDefaultMacros();

    for ( size_t i = 0; i < defaults.size(); i++) {
        wxString name = defaults[i];
        if (!make_iterator_range(names).contains(name)) {
            AddMacro(name);
            RestoreMacro(name);
            WriteMacro(name);
        }
    }
}

static const auto MP3Conversion = XO("MP3 Conversion");
static const auto FadeEnds      = XO("Fade Ends");

wxArrayStringEx MacroCommands::GetNamesOfDefaultMacros()
{
    return {
        MP3Conversion.Translation(),
        FadeEnds.Translation(),
    };
}

void MacroCommands::RestoreMacro(const wxString& name)
{
// TIDY-ME: Effects change their name with localisation.
// Commands (at least currently) don't.  Messy.
    ResetMacro();
    if (name == MP3Conversion.Translation()) {
        AddToMacro(wxT("Normalize"));
        AddToMacro(wxT("ExportMP3"));
    } else if (name == FadeEnds.Translation()) {
        AddToMacro(wxT("Select"), wxT("Start=\"0\" End=\"1\""));
        AddToMacro(wxT("FadeIn"));
        AddToMacro(wxT("Select"), wxT("Start=\"0\" End=\"1\" RelativeTo=\"ProjectEnd\""));
        AddToMacro(wxT("FadeOut"));
        AddToMacro(wxT("Select"), wxT("Start=\"0\" End=\"0\""));
    }
}

CommandID MacroCommands::GetCommand(int index)
{
    if (index < 0 || index >= (int)mCommandMacro.size()) {
        return wxT("");
    }

    return mCommandMacro[index];
}

wxString MacroCommands::GetParams(int index)
{
    if (index < 0 || index >= (int)mParamsMacro.size()) {
        return wxT("");
    }

    return mParamsMacro[index];
}

int MacroCommands::GetCount()
{
    return (int)mCommandMacro.size();
}

wxString MacroCommands::ReadMacro(const wxString& macro, wxWindow* parent)
{
    // Clear any previous macro
    ResetMacro();

    // Build the filename
    wxFileName name(FileNames::MacroDir(), macro, wxT("txt"));

    // But, ask the user for the real name if we're importing
    if (parent) {
        FilePath fn = SelectFile(FileNames::Operation::_None,
                                 XO("Import Macro"),
                                 wxEmptyString,
                                 name.GetName(),
                                 wxT("txt"),
                                 { FileNames::TextFiles },
                                 wxFD_OPEN | wxRESIZE_BORDER,
                                 parent);

        // User canceled...
        if (fn.empty()) {
            return wxEmptyString;
        }

        wxFileName check(fn);
        check.SetPath(name.GetPath());
        if (check.FileExists()) {
            int id = AudacityMessageBox(
                XO("Macro %s already exists. Would you like to replace it?").Format(check.GetName()),
                XO("Import Macro"),
                wxYES_NO);
            if (id == wxNO) {
                return wxEmptyString;
            }
        }

        name.Assign(fn);
    }

    // Set the file name
    wxTextFile tf(name.GetFullPath());

    // Open and check
    tf.Open();
    if (!tf.IsOpened()) {
        // wxTextFile will display any errors
        return wxEmptyString;
    }

    // Load commands from the file
    int lines = tf.GetLineCount();
    if (lines > 0) {
        for (int i = 0; i < lines; i++) {
            // Find the command name terminator...ignore line if not found
            int splitAt = tf[i].Find(wxT(':'));
            if (splitAt < 0) {
                continue;
            }

            // Parse and clean
            wxString cmd = tf[i].Left(splitAt).Strip(wxString::both);
            wxString parm = tf[i].Mid(splitAt + 1).Strip(wxString::trailing);

            // Add to lists
            mCommandMacro.push_back(cmd);
            mParamsMacro.push_back(parm);
        }
    }

    // Done with the file
    tf.Close();

    // Write to macro directory if importing
    if (parent) {
        return WriteMacro(name.GetName());
    }

    return name.GetName();
}

wxString MacroCommands::WriteMacro(const wxString& macro, wxWindow* parent)
{
    // Build the default filename
    wxFileName name(FileNames::MacroDir(), macro, wxT("txt"));

    // But, ask the user for the real name if we're exporting
    if (parent) {
        FilePath fn = SelectFile(FileNames::Operation::_None,
                                 XO("Export Macro"),
                                 wxEmptyString,
                                 name.GetName(),
                                 wxT("txt"),
                                 { FileNames::TextFiles },
                                 wxFD_SAVE | wxFD_OVERWRITE_PROMPT | wxRESIZE_BORDER,
                                 parent);

        // User canceled...
        if (fn.empty()) {
            return wxEmptyString;
        }

        name.Assign(fn);
    }

    // Set the file name
    wxTextFile tf(name.GetFullPath());

    // Create the file (Create() doesn't leave the file open)
    if (!tf.Exists()) {
        tf.Create();
    }

    // Open it
    tf.Open();

    if (!tf.IsOpened()) {
        // wxTextFile will display any errors
        return wxEmptyString;
    }

    // Start with a clean slate
    tf.Clear();

    // Copy over the commands
    int lines = mCommandMacro.size();
    for (int i = 0; i < lines; i++) {
        // using GET to serialize macro definition to a text file
        tf.AddLine(mCommandMacro[i].GET() + wxT(":") + mParamsMacro[ i ]);
    }

    // Write the macro
    tf.Write();

    // Done with the file
    tf.Close();

    return name.GetName();
}

bool MacroCommands::AddMacro(const wxString& macro)
{
    // Build the filename
    wxFileName name(FileNames::MacroDir(), macro, wxT("txt"));

    // Set the file name
    wxTextFile tf(name.GetFullPath());

    // Create it..Create will display errors
    return tf.Create();
}

bool MacroCommands::DeleteMacro(const wxString& macro)
{
    // Build the filename
    wxFileName name(FileNames::MacroDir(), macro, wxT("txt"));

    // Delete it...wxRemoveFile will display errors
    auto result = wxRemoveFile(name.GetFullPath());

    // Delete any legacy chain that it shadowed
    auto oldPath = wxFileName{ FileNames::LegacyChainDir(), macro, wxT("txt") };
    wxRemoveFile(oldPath.GetFullPath()); // Don't care about this return value

    return result;
}

bool MacroCommands::RenameMacro(const wxString& oldmacro, const wxString& newmacro)
{
    // Build the filenames
    wxFileName oname(FileNames::MacroDir(), oldmacro, wxT("txt"));
    wxFileName nname(FileNames::MacroDir(), newmacro, wxT("txt"));

    // Rename it...wxRenameFile will display errors
    return wxRenameFile(oname.GetFullPath(), nname.GetFullPath());
}

// Gets all commands that are valid for this mode.
MacroCommandsCatalog::MacroCommandsCatalog(const AudacityProject* project)
{
    if (!project) {
        return;
    }

    Entries commands;

    PluginManager& pm = PluginManager::Get();
    EffectManager& em = EffectManager::Get();
    {
        for (auto& plug
             : pm.PluginsOfType(PluginTypeEffect | PluginTypeAudacityCommand)) {
            auto command = pm.GetCommandIdentifier(plug.GetID());
            if (!command.empty()) {
                commands.push_back({
                    { command, plug.GetSymbol().Msgid() },
                    plug.GetPluginType() == PluginTypeEffect
                    ? XO("Effect") : XO("Menu Command (With Parameters)")
                });
            }
        }
    }

    auto& manager = CommandManager::Get(*project);
    TranslatableStrings mLabels;
    CommandIDs mNames;
    std::vector<bool> vExcludeFromMacros;
    mLabels.clear();
    mNames.clear();
    manager.GetAllCommandLabels(mLabels, vExcludeFromMacros, true);
    manager.GetAllCommandNames(mNames, true);

    const bool english = wxGetLocale()->GetCanonicalName().StartsWith(wxT("en"));

    for (size_t i=0; i < mNames.size(); i++) {
        if (!vExcludeFromMacros[i]) {
            auto label = mLabels[i];
            label.Strip();
            bool suffix;
            if (!english) {
                suffix = false;
            } else {
                // We'll disambiguate if the squashed name is short and shorter than the internal name.
                // Otherwise not.
                // This means we won't have repetitive items like "Cut (Cut)"
                // But we will show important disambiguation like "All (SelectAll)" and "By Date (SortByDate)"
                // Disambiguation is no longer essential as the details box will show it.
                // PRL:  I think this reasoning applies only when locale is English.
                // For other locales, show the (CamelCaseCodeName) always.  Or, never?
                wxString squashed = label.Translation();
                squashed.Replace(" ", "");

                // uh oh, using GET for dubious comparison of (lengths of)
                // user-visible name and internal CommandID!
                // and doing this only for English locale!
                suffix = squashed.length() < wxMin(18, mNames[i].GET().length());
            }

            if (suffix) {
                // uh oh, using GET to expose CommandID to the user, as a
                // disambiguating suffix on a name, but this is only ever done if
                // the locale is English!
                // PRL:  In case this logic does get fixed for other locales,
                // localize even this punctuation format.  I'm told Chinese actually
                // prefers slightly different parenthesis characters
                label.Join(XO("(%s)").Format(mNames[i].GET()), wxT(" "));
            }

            // Bug 2294.  The Close command pulls the rug out from under
            // batch processing, because it destroys the project.
            // So it is UNSAFE for scripting, and therefore excluded from
            // the catalog.
            if (mNames[i] == "Close") {
                continue;
            }

            commands.push_back(
            {
                {
                    mNames[i], // Internal name.
                    label // User readable name
                },
                XO("Menu Command (No Parameters)")
            }
                );
        }
    }

    // Sort commands by their user-visible names.
    // PRL:  What exactly should happen if first members of pairs are not unique?
    // I'm not sure, but at least I can sort stably for a better defined result.
    auto less
        =[](const Entry& a, const Entry& b)
    {
        return a.name.StrippedTranslation()
               < b.name.StrippedTranslation();
    };
    std::stable_sort(commands.begin(), commands.end(), less);

    // Now uniquify by friendly name
    auto equal
        =[](const Entry& a, const Entry& b)
    {
        return a.name.StrippedTranslation()
               == b.name.StrippedTranslation();
    };
    std::unique_copy(
        commands.begin(), commands.end(), std::back_inserter(mCommands), equal);
}

// binary search
auto MacroCommandsCatalog::ByFriendlyName(const TranslatableString& friendlyName) const
-> Entries::const_iterator
{
    const auto less = [](const Entry& entryA, const Entry& entryB)
    {
        return entryA.name.StrippedTranslation()
               < entryB.name.StrippedTranslation();
    };
    auto range = std::equal_range(
        begin(), end(), Entry { { {}, friendlyName }, {} }, less
        );
    if (range.first != range.second) {
        wxASSERT_MSG(range.first + 1 == range.second,
                     "Non-unique user-visible command name");
        return range.first;
    } else {
        return end();
    }
}

// linear search
auto MacroCommandsCatalog::ByCommandId(const CommandID& commandId) const
-> Entries::const_iterator
{
    // Maybe this too should have a uniqueness check?
    return std::find_if(begin(), end(),
                        [&](const Entry& entry)
    { return entry.name.Internal() == commandId; });
}

// linear search
auto MacroCommandsCatalog::ByTranslation(const wxString& translation) const
-> Entries::const_iterator
{
    return std::find_if(begin(), end(),
                        [&](const Entry& entry)
    { return entry.name.Translation() == translation; });
}

wxString MacroCommands::GetCurrentParamsFor(const CommandID& command)
{
    const PluginID& ID = PluginManager::Get().GetByCommandIdentifier(command);
    if (ID.empty()) {
        return wxEmptyString; // effect not found.
    }

    return EffectAndCommandPluginManager::Get().GetEffectParameters(ID);
}

wxString MacroCommands::PromptForParamsFor(
    const CommandID& command, const wxString& params, AudacityProject& project)
{
    const PluginID& ID = PluginManager::Get().GetByCommandIdentifier(command);
    if (ID.empty()) {
        return wxEmptyString; // effect not found
    }
    wxString res = params;
    auto cleanup = EffectAndCommandPluginManager::Get().SetBatchProcessing(ID);
    if (EffectAndCommandPluginManager::Get().SetEffectParameters(ID, params)) {
        auto dialogInvoker
            =[&](
                  Effect& effect, EffectSettings& settings,
                  std::shared_ptr<EffectInstance>& pInstance) -> bool {
            const auto pServices = dynamic_cast<EffectUIServices*>(&effect);
            return pServices && pServices->ShowHostInterface(effect,
                                                             GetProjectFrame(project), EffectUI::DialogFactory,
                                                             pInstance,
                                                             *std::make_shared<SimpleEffectSettingsAccess>(settings),
                                                             effect.IsBatchProcessing()) != 0;
        };
        if (EffectAndCommandPluginManager::Get().PromptUser(
                ID, project, std::move(dialogInvoker))) {
            res = EffectAndCommandPluginManager::Get().GetEffectParameters(ID);
        }
    }
    return res;
}

wxString MacroCommands::PromptForPresetFor(const CommandID& command, const wxString& params, wxWindow* parent)
{
    const PluginID& ID = PluginManager::Get().GetByCommandIdentifier(command);
    if (ID.empty()) {
        return wxEmptyString; // effect not found.
    }

    EffectManager::EffectPresetDialog dialog
        =[parent](
              EffectPlugin& effect,
              const wxString& preset) -> std::optional<wxString> {
        EffectPresetsDialog dlg(parent, &effect);
        dlg.Layout();
        dlg.Fit();
        dlg.SetSize(dlg.GetMinSize());
        dlg.CenterOnParent();
        dlg.SetSelected(preset);

        if (dlg.ShowModal()) {
            return std::make_optional(dlg.GetSelected());
        } else {
            return {}
        }
    };
    wxString preset
        =EffectManager::Get().GetPreset(ID, params, std::move(dialog));

    // Preset will be empty if the user cancelled the dialog, so return the original
    // parameter value.
    if (preset.empty()) {
        return params;
    }

    return preset;
}

bool MacroCommands::ApplyEffectCommand(
    const PluginID& ID, const TranslatableString& friendlyCommand,
    const CommandID& command, const wxString& params,
    const CommandContext& Context)
{
    static_cast<void>(command);//compiler food.

    //Possibly end processing here, if in batch-debug
    if (ReportAndSkip(friendlyCommand, params)) {
        return true;
    }

    const PluginDescriptor* plug = PluginManager::Get().GetPlugin(ID);
    if (!plug) {
        return false;
    }

    AudacityProject* project = &mProject;

    // IF nothing selected, THEN select everything depending
    // on preferences setting.
    // (most effects require that you have something selected).
    if (plug->GetPluginType() != PluginTypeAudacityCommand) {
        if (!SelectUtilities::SelectAllIfNoneAndAllowed(*project)) {
            AudacityMessageBox(
                // i18n-hint: %s will be replaced by the name of an action, such as "Remove Tracks".
                XO("\"%s\" requires one or more tracks to be selected.").Format(friendlyCommand));
            return false;
        }
    }

    bool res = false;

    auto cleanup = EffectAndCommandPluginManager::Get().SetBatchProcessing(ID);

    // transfer the parameters to the effect...
    if (EffectAndCommandPluginManager::Get().SetEffectParameters(ID, params)) {
        if (plug->GetPluginType() == PluginTypeAudacityCommand) {
            // and apply the effect...
            res = CommandDispatch::DoAudacityCommand(ID,
                                                     Context,
                                                     EffectManager::kConfigured
                                                     | EffectManager::kSkipState
                                                     | EffectManager::kDontRepeatLast);
        } else {
            // and apply the effect...
            res = EffectUI::DoEffect(
                ID, Context.project,
                EffectManager::kConfigured | EffectManager::kSkipState
                | EffectManager::kDontRepeatLast);
        }
    }

    return res;
}

bool MacroCommands::ApplyCommand(const TranslatableString& friendlyCommand,
                                 const CommandID& command, const wxString& params,
                                 CommandContext const* const pContext)
{
    // Test for an effect.
    const PluginID& ID = PluginManager::Get().GetByCommandIdentifier(command);
    if (!ID.empty()) {
        if (pContext) {
            return ApplyEffectCommand(
                ID, friendlyCommand, command, params, *pContext);
        }
        const CommandContext context(mProject);
        return ApplyEffectCommand(
            ID, friendlyCommand, command, params, context);
    }

    if (pContext) {
        assert(&pContext->project == &GetProject());
        if (CommandDispatch::HandleTextualCommand(
                command, *pContext, AlwaysEnabledFlag, true)) {
            return true;
        }
        pContext->Status(wxString::Format(
                             _("Your batch command of %s was not recognized."), friendlyCommand.Translation()));
        return false;
    } else {
        const CommandContext context(mProject);
        if (CommandDispatch::HandleTextualCommand(
                command, context, AlwaysEnabledFlag, true)) {
            return true;
        }
    }

    AudacityMessageBox(
        XO("Your batch command of %s was not recognized.")
        .Format(friendlyCommand));

    return false;
}

bool MacroCommands::ApplyCommandInBatchMode(
    const TranslatableString& friendlyCommand,
    const CommandID& command, const wxString& params,
    CommandContext const* pContext)
{
    assert(!pContext || &pContext->project == &GetProject());
    AudacityProject* project = &mProject;
    auto& settings = ProjectSettings::Get(*project);
    // Recalc flags and enable items that may have become enabled.
    CommandManager::Get(*project).UpdateMenus(false);
    // enter batch mode...
    project->mBatchMode++;
    auto cleanup = finally([&] {
        // exit batch mode...
        project->mBatchMode--;
    });

    return ApplyCommand(friendlyCommand, command, params, pContext);
}

static int MacroReentryCount = 0;
// ApplyMacro returns true on success, false otherwise.
// Any error reporting to the user in setting up the macro
// has already been done.
bool MacroCommands::ApplyMacro(
    const MacroCommandsCatalog& catalog, const wxString& filename)
{
    // Check for reentrant ApplyMacro commands.
    // We'll allow 1 level of reentry, but not more.
    // And we treat ignoring deeper levels as a success.
    if (MacroReentryCount > 1) {
        return true;
    }

    // Restore the reentry counter (to zero) when we exit.
    auto cleanup1 = valueRestorer(MacroReentryCount);
    MacroReentryCount++;

    AudacityProject* proj = &mProject;
    bool res = false;

    // Only perform this group on initial entry.  They should not be done
    // while recursing.
    if (MacroReentryCount == 1) {
        mFileName = filename;

        TranslatableString longDesc, shortDesc;
        wxString name = gPrefs->Read(wxT("/Batch/ActiveMacro"), wxEmptyString);
        if (name.empty()) {
            /* i18n-hint: active verb in past tense */
            longDesc = XO("Applied Macro");
            shortDesc = XO("Apply Macro");
        } else {
            /* i18n-hint: active verb in past tense */
            longDesc = XO("Applied Macro '%s'").Format(name);
            shortDesc = XO("Apply '%s'").Format(name);
        }

        // Save the project state before making any changes.  It will be rolled
        // back if an error occurs.
        // It also causes any calls to ModifyState (such as by simple
        // view-changing commands) to append changes to this state, not to the
        // previous state in history.  See Bug 2076
        if (proj) {
            ProjectHistory::Get(*proj).PushState(longDesc, shortDesc);
        }
    }

    // Upon exit of the top level apply, roll back the state if an error occurs.
    auto cleanup2 = finally([&, macroReentryCount = MacroReentryCount] {
        if (macroReentryCount == 1 && !res && proj) {
            // Be sure that exceptions do not escape this destructor
            GuardedCall([&]{
                // Macro failed or was cancelled; revert to the previous state
                auto& history = ProjectHistory::Get(*proj);
                history.RollbackState();
                // The added undo state is now vacuous.  Remove it (Bug 2759)
                auto& undoManager = UndoManager::Get(*proj);
                undoManager.Undo(
                    [&]( const UndoStackElem& elem ){
                    history.PopState(elem.state);
                });
                undoManager.AbandonRedo();
            });
        }
    });

    mAbort = false;

    // Is tracing enabled?
    bool trace;
    gPrefs->Read(wxT("/EnableMacroTracing"), &trace, false);

    // If so, then block most other messages while running the macro
    wxLogLevel prevLevel = wxLog::GetComponentLevel("");
    if (trace) {
        wxLog::SetComponentLevel("",  wxLOG_FatalError);
        wxLog::SetComponentLevel(wxLOG_COMPONENT,  wxLOG_Info);
    }

    size_t i = 0;
    for (; i < mCommandMacro.size(); i++) {
        const auto& command = mCommandMacro[i];
        auto iter = catalog.ByCommandId(command);
        const auto friendly = (iter == catalog.end())
                              ?
                              // uh oh, using GET to expose an internal name to the user!
                              // in default of any better friendly name
                              Verbatim(command.GET())
                              : iter->name.Msgid().Stripped();

        wxTimeSpan before;
        if (trace) {
            before = wxTimeSpan(0, 0, 0, wxGetUTCTimeMillis());
        }

        bool success = ApplyCommandInBatchMode(friendly, command, mParamsMacro[i]);

        if (trace) {
            auto after = wxTimeSpan(0, 0, 0, wxGetUTCTimeMillis());
            wxLogMessage(wxT("Macro line #%ld took %s : %s:%s"),
                         i + 1,
                         (after - before).Format(wxT("%H:%M:%S.%l")),
                         command.GET(),
                         mParamsMacro[i]);
        }

        if (!success || mAbort) {
            break;
        }
    }

    // Restore message level
    if (trace) {
        wxLog::SetComponentLevel("", prevLevel);
    }

    res = (i == mCommandMacro.size());
    if (!res) {
        return false;
    }

    if (MacroReentryCount == 1) {
        mFileName.Empty();

        if (proj) {
            ProjectHistory::Get(*proj).ModifyState(true);
        }
    }

    return true;
}

// AbortBatch() allows a premature terminatation of a batch.
void MacroCommands::AbortBatch()
{
    mAbort = true;
}

void MacroCommands::AddToMacro(const CommandID& command, int before)
{
    AddToMacro(command, GetCurrentParamsFor(command), before);
}

void MacroCommands::AddToMacro(const CommandID& command, const wxString& params, int before)
{
    if (before == -1) {
        before = (int)mCommandMacro.size();
    }

    mCommandMacro.insert(mCommandMacro.begin() + before, command);
    mParamsMacro.insert(mParamsMacro.begin() + before, params);
}

void MacroCommands::DeleteFromMacro(int index)
{
    if (index < 0 || index >= (int)mCommandMacro.size()) {
        return;
    }

    mCommandMacro.erase(mCommandMacro.begin() + index);
    mParamsMacro.erase(mParamsMacro.begin() + index);
}

void MacroCommands::ResetMacro()
{
    mCommandMacro.clear();
    mParamsMacro.clear();
}

// ReportAndSkip() is a diagnostic function that avoids actually
// applying the requested effect if in batch-debug mode.
bool MacroCommands::ReportAndSkip(
    const TranslatableString& friendlyCommand, const wxString& params)
{
    int bDebug;
    gPrefs->Read(wxT("/Batch/Debug"), &bDebug, 0);
    if (bDebug == 0) {
        return false;
    }

    //TODO: Add a cancel button to these, and add the logic so that we can abort.
    if (!params.empty()) {
        AudacityMessageBox(
            XO("Apply %s with parameter(s)\n\n%s")
            .Format(friendlyCommand, params),
            XO("Test Mode"));
    } else {
        AudacityMessageBox(
            XO("Apply %s").Format(friendlyCommand),
            XO("Test Mode"));
    }
    return true;
}

void MacroCommands::MigrateLegacyChains()
{
    static bool done = false;
    if (!done) {
        // Check once per session at most

        // Copy chain files from the old Chains into the new Macros directory,
        // but only if like-named files are not already present in Macros.

        // Leave the old copies in place, in case a user wants to go back to
        // an old Audacity version.  They will have their old chains intact, but
        // won't have any edits they made to the copy that now lives in Macros
        // which old Audacity will not read.

        const auto oldDir = FileNames::LegacyChainDir();
        FilePaths files;
        wxDir::GetAllFiles(oldDir, &files, wxT("*.txt"), wxDIR_FILES);

        // add a dummy path component to be overwritten by SetFullName
        wxFileName newDir{ FileNames::MacroDir(), wxT("x") };

        for (const auto& file : files) {
            auto name = wxFileName{ file }.GetFullName();
            newDir.SetFullName(name);
            const auto newPath = newDir.GetFullPath();
            if (!wxFileExists(newPath)) {
                FileNames::DoCopyFile(file, newPath);
            }
        }
        done = true;
    }
    // To do:  use std::once
}

wxArrayString MacroCommands::GetNames()
{
    MigrateLegacyChains();

    wxArrayString names;
    FilePaths files;
    wxDir::GetAllFiles(FileNames::MacroDir(), &files, wxT("*.txt"), wxDIR_FILES);
    size_t i;

    wxFileName ff;
    for (i = 0; i < files.size(); i++) {
        ff = (files[i]);
        names.push_back(ff.GetName());
    }

    std::sort(names.begin(), names.end());

    return names;
}

bool MacroCommands::IsFixed(const wxString& name)
{
    auto defaults = GetNamesOfDefaultMacros();
    if (make_iterator_range(defaults).contains(name)) {
        return true;
    }
    return false;
}

void MacroCommands::Split(const wxString& str, wxString& command, wxString& param)
{
    int splitAt;

    command.Empty();
    param.Empty();

    if (str.empty()) {
        return;
    }

    splitAt = str.Find(wxT(':'));
    if (splitAt < 0) {
        return;
    }

    command = str.Mid(0, splitAt);
    param = str.Mid(splitAt + 1);

    return;
}

wxString MacroCommands::Join(const wxString& command, const wxString& param)
{
    return command + wxT(": ") + param;
}
