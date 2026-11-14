/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2018 Audacity Team
   License: wxWidgets

   James Crook

******************************************************************//**

\file GetInfoCommand.cpp
\brief Contains definitions for GetInfoCommand class.
This class now lists
- Commands
- Menus
- Tracks
- Clips
- Labels
- Boxes
- Selection

*//*******************************************************************/

#include "GetInfoCommand.h"

#include "CommandDispatch.h"
#include "../CommonCommandFlags.h"
#include "LoadCommands.h"
#include "Project.h"
#include "../ProjectWindows.h"
#include "CommandManager.h"
#include "CommandTargets.h"
#include "EffectAndCommandPluginManager.h"
#include "../widgets/Overlay.h"
#include "TrackFocus.h"
#include "../TrackPanel.h"
#include "WaveClip.h"
#include "../tracks/playabletrack/wavetrack/ui/WaveformAppearance.h"
#include "ViewInfo.h"
#include "WaveTrack.h"
#include "WaveformSettings.h"
#include "prefs/WaveformScale.h"
#include "LabelTrack.h"
#include "NoteTrack.h"
#include "TimeTrack.h"
#include "Envelope.h"
#include "ProjectAudioIO.h"
#include "AudioIO.h"

#include "SelectCommand.h"
#include "ShuttleGui.h"
#include "CommandContext.h"

#include "../prefs/PrefsDialog.h"
#include "SettingsVisitor.h"
#include "PluginManager.h"
#include "../tracks/ui/ChannelView.h"
#include "ShuttleGui.h"

#include <wx/frame.h>
#include <wx/log.h>
#include <wx/menu.h>

const ComponentInterfaceSymbol GetInfoCommand::Symbol
{ XO("Get Info") };

namespace {
BuiltinCommandsModule::Registration< GetInfoCommand > reg;
}
enum {
    kCommands,
    //kCommandsPlus,
    kMenus,
    kPreferences,
    kTracks,
    kClips,
    kEnvelopes,
    kLabels,
    kBoxes,
    kSelection,
    nTypes
};

static const EnumValueSymbol kTypes[nTypes] =
{
    { XO("Commands") },
    //{ wxT("CommandsPlus"), XO("Commands Plus") },
    { XO("Menus") },
    { XO("Preferences") },
    { XO("Tracks") },
    { XO("Clips") },
    { XO("Envelopes") },
    { XO("Labels") },
    { XO("Boxes") },
    { XO("Selection") },
};

enum {
    kJson,
    kLisp,
    kBrief,
    nFormats
};

static const EnumValueSymbol kFormats[nFormats] =
{
    // These are acceptable dual purpose internal/visible names

    /* i18n-hint JavaScript Object Notation */
    { XO("JSON") },
    /* i18n-hint name of a computer programming language */
    { XO("LISP") },
    { XO("Brief") }
};

template<bool Const>
bool GetInfoCommand::VisitSettings(SettingsVisitorBase<Const>& S)
{
    S.DefineEnum(mInfoType, wxT("Type"), 0, kTypes, nTypes);
    S.DefineEnum(mFormat, wxT("Format"), 0, kFormats, nFormats);
    return true;
}

bool GetInfoCommand::VisitSettings(SettingsVisitor& S)
{ return VisitSettings<false>(S); }

bool GetInfoCommand::VisitSettings(ConstSettingsVisitor& S)
{ return VisitSettings<true>(S); }

void GetInfoCommand::PopulateOrExchange(ShuttleGui& S)
{
    S.AddSpace(0, 5);

    S.StartMultiColumn(2, wxALIGN_CENTER);
    {
        S.TieChoice(XXO("Type:"),
                    mInfoType, Msgids(kTypes, nTypes));
        S.TieChoice(XXO("Format:"),
                    mFormat, Msgids(kFormats, nFormats));
    }
    S.EndMultiColumn();
}

bool GetInfoCommand::Apply(const CommandContext& context)
{
    if (mFormat == kJson) {
        return ApplyInner(context);
    }

    if (mFormat == kLisp) {
        CommandContext LispyContext(
            context.project,
            std::make_unique<LispifiedCommandOutputTargets>(*context.pOutput.get())
            );
        return ApplyInner(LispyContext);
    }

    if (mFormat == kBrief) {
        CommandContext BriefContext(
            context.project,
            std::make_unique<BriefCommandOutputTargets>(*context.pOutput.get())
            );
        return ApplyInner(BriefContext);
    }

    return false;
}

bool GetInfoCommand::ApplyInner(const CommandContext& context)
{
    switch (mInfoType) {
    // flag of 1 to include parameterless commands.
    case kCommands: return SendCommands(context, 1);
    case kMenus: return SendMenus(context);
    case kPreferences: return SendPreferences(context);
    case kTracks: return SendTracks(context);
    case kClips: return SendClips(context);
    case kEnvelopes: return SendEnvelopes(context);
    case kLabels: return SendLabels(context);
    case kBoxes: return SendBoxes(context);
    case kSelection: return SendSelection(context);
    default:
        context.Status("Command options not recognised");
    }
    return false;
}

bool GetInfoCommand::SendMenus(const CommandContext& context)
{
    wxMenuBar* pBar = GetProjectFrame(context.project).GetMenuBar();
    if (!pBar) {
        wxLogDebug("No menus");
        return false;
    }

    size_t cnt = pBar->GetMenuCount();
    size_t i;
    wxString Label;
    context.StartArray();
    for (i=0; i < cnt; i++) {
        Label = pBar->GetMenuLabelText(i);
        context.StartStruct();
        context.AddItem(0, "depth");
        context.AddItem(0, "flags");
        context.AddItem(Label, "label");
        context.AddItem("", "accel");
        context.EndStruct();
        ExploreMenu(context, pBar->GetMenu(i), pBar->GetId(), 1);
    }
    context.EndArray();
    return true;
}

#include "Prefs.h"

namespace {
/**************************************************************************//**
\brief Shuttle that retrieves a JSON format definition of a command's parameters.
********************************************************************************/
class ShuttleGuiGetDefinition : public ShuttleGui, public CommandMessageTargetDecorator
{
public:
    ShuttleGuiGetDefinition(wxWindow* pParent, CommandMessageTarget& target);
    virtual ~ShuttleGuiGetDefinition();

    wxCheckBox* TieCheckBox(
        const TranslatableString& Prompt, const BoolSetting& Setting) override;
    wxCheckBox* TieCheckBoxOnRight(
        const TranslatableString& Prompt, const BoolSetting& Setting) override;

    wxChoice* TieChoice(const TranslatableString& Prompt, ChoiceSetting& choiceSetting) override;

    wxChoice* TieNumberAsChoice(const TranslatableString& Prompt, IntSetting& Setting, const TranslatableStrings& Choices,
                                const std::vector<int>* pInternalChoices, int iNoMatchSelector) override;

    wxTextCtrl* TieTextBox(
        const TranslatableString& Prompt, const StringSetting& Setting, const int nChars) override;
    wxTextCtrl* TieIntegerTextBox(
        const TranslatableString& Prompt, const IntSetting& Setting, const int nChars) override;
    wxTextCtrl* TieNumericTextBox(
        const TranslatableString& Prompt, const DoubleSetting& Setting, const int nChars, bool acceptEnter) override;
    wxSlider* TieSlider(
        const TranslatableString& Prompt, const IntSetting& Setting, const int max, const int min = 0) override;
    wxSpinCtrl* TieSpinCtrl(
        const TranslatableString& Prompt, const IntSetting& Setting, const int max, const int min) override;
};

ShuttleGuiGetDefinition::ShuttleGuiGetDefinition(
    wxWindow* pParent, CommandMessageTarget& target)
    : ShuttleGui(pParent, eIsGettingMetadata),
    CommandMessageTargetDecorator(target)
{
}

ShuttleGuiGetDefinition::~ShuttleGuiGetDefinition(void)
{
}

wxCheckBox* ShuttleGuiGetDefinition::TieCheckBox(
    const TranslatableString& Prompt,
    const BoolSetting& Setting)
{
    StartStruct();
    AddItem(Setting.GetPath(), "id");
    AddItem(Prompt.Translation(), "prompt");
    AddItem("bool", "type");
    AddBool(Setting.GetDefault(), "default");
    EndStruct();
    return ShuttleGui::TieCheckBox(Prompt, Setting);
}

wxCheckBox* ShuttleGuiGetDefinition::TieCheckBoxOnRight(
    const TranslatableString& Prompt,
    const BoolSetting& Setting)
{
    StartStruct();
    AddItem(Setting.GetPath(), "id");
    AddItem(Prompt.Translation(), "prompt");
    AddItem("bool", "type");
    AddBool(Setting.GetDefault(), "default");
    EndStruct();
    return ShuttleGui::TieCheckBoxOnRight(Prompt, Setting);
}

wxChoice* ShuttleGuiGetDefinition::TieChoice(const TranslatableString& Prompt,
                                             ChoiceSetting& choiceSetting)
{
    StartStruct();
    AddItem(choiceSetting.Key(), "id");
    AddItem(Prompt.Translation(), "prompt");
    AddItem("enum", "type");
    AddItem(choiceSetting.Default().Internal(), "default");
    StartField("enum");
    StartArray();
    for ( const auto& choice : choiceSetting.GetSymbols().GetInternals()) {
        AddItem(choice);
    }
    EndArray();
    EndField();
    EndStruct();
    return ShuttleGui::TieChoice(Prompt, choiceSetting);
}

wxChoice* ShuttleGuiGetDefinition::TieNumberAsChoice(
    const TranslatableString& Prompt,
    IntSetting& Setting,
    const TranslatableStrings& Choices,
    const std::vector<int>* pInternalChoices, int iNoMatchSelector)
{
    // Come here for controls that present non-exhaustive choices among some
    //  numbers, with an associated control that allows arbitrary entry of an
    // "Other..."
    StartStruct();
    AddItem(Setting.GetPath(), "id");
    AddItem(Prompt.Translation(), "prompt");
    AddItem("number", "type");  // not "enum" !
    AddItem(Setting.GetDefault(), "default");
    EndStruct();
    return ShuttleGui::TieNumberAsChoice(
        Prompt, Setting, Choices, pInternalChoices, iNoMatchSelector);
}

wxTextCtrl* ShuttleGuiGetDefinition::TieTextBox(
    const TranslatableString& Prompt,
    const StringSetting& Setting,
    const int nChars)
{
    StartStruct();
    AddItem(Setting.GetPath(), "id");
    AddItem(Prompt.Translation(), "prompt");
    AddItem("string", "type");
    AddItem(Setting.GetDefault(), "default");
    EndStruct();
    return ShuttleGui::TieTextBox(Prompt, Setting, nChars);
}

wxTextCtrl* ShuttleGuiGetDefinition::TieIntegerTextBox(
    const TranslatableString& Prompt,
    const IntSetting& Setting,
    const int nChars)
{
    StartStruct();
    AddItem(Setting.GetPath(), "id");
    AddItem(Prompt.Translation(), "prompt");
    AddItem("number", "type");
    AddItem(Setting.GetDefault(), "default");
    EndStruct();
    return ShuttleGui::TieIntegerTextBox(Prompt, Setting, nChars);
}

wxTextCtrl* ShuttleGuiGetDefinition::TieNumericTextBox(
    const TranslatableString& Prompt,
    const DoubleSetting& Setting,
    const int nChars, bool acceptEnter)
{
    StartStruct();
    AddItem(Setting.GetPath(), "id");
    AddItem(Prompt.Translation(), "prompt");
    AddItem("number", "type");
    AddItem(Setting.GetDefault(), "default");
    EndStruct();
    return ShuttleGui::TieNumericTextBox(Prompt, Setting, nChars, acceptEnter);
}

wxSlider* ShuttleGuiGetDefinition::TieSlider(
    const TranslatableString& Prompt,
    const IntSetting& Setting,
    const int max,
    const int min)
{
    StartStruct();
    AddItem(Setting.GetPath(), "id");
    AddItem(Prompt.Translation(), "prompt");
    AddItem("number", "type");
    AddItem(Setting.GetDefault(), "default");
    EndStruct();
    return ShuttleGui::TieSlider(Prompt, Setting, max, min);
}

wxSpinCtrl* ShuttleGuiGetDefinition::TieSpinCtrl(
    const TranslatableString& Prompt,
    const IntSetting& Setting,
    const int max,
    const int min)
{
    StartStruct();
    AddItem(Setting.GetPath(), "id");
    AddItem(Prompt.Translation(), "prompt");
    AddItem("number", "type");
    AddItem(Setting.GetDefault(), "default");
    EndStruct();
    return ShuttleGui::TieSpinCtrl(Prompt, Setting, max, min);
}
}

bool GetInfoCommand::SendPreferences(const CommandContext& context)
{
    context.StartArray();
    auto pWin = &GetProjectFrame(context.project);
    GlobalPrefsDialog dialog(pWin, &context.project);
    // wxCommandEvent Evt;
    //dialog.Show();
    ShuttleGuiGetDefinition S(pWin, *((context.pOutput)->mStatusTarget));
    dialog.ShuttleAll(S);
    context.EndArray();
    return true;
}

/**
 Send the list of commands.
 */
bool GetInfoCommand::SendCommands(const CommandContext& context, int flags)
{
    context.StartArray();
    PluginManager& pm = PluginManager::Get();
    {
        for (auto& plug
             : pm.PluginsOfType(PluginTypeEffect | PluginTypeAudacityCommand)) {
            auto command = pm.GetCommandIdentifier(plug.GetID());
            if (!command.empty()) {
                EffectAndCommandPluginManager::Get().GetCommandDefinition(
                    plug.GetID(), context, flags);
            }
        }
    }
    context.EndArray();
    return true;
}

bool GetInfoCommand::SendBoxes(const CommandContext& context)
{
    //context.Status("Boxes");
    auto pWin = &GetProjectFrame(context.project);

    context.StartArray();
    wxRect R = pWin->GetScreenRect();

    //R.SetPosition( wxPoint(0,0) );

    //wxString Name = pWin->GetName();
    context.StartStruct();
    context.AddItem(0, "depth");
    context.AddItem("Audacity Window", "name");
    context.StartField("box");
    context.StartArray();
    context.AddItem(R.GetLeft());
    context.AddItem(R.GetTop());
    context.AddItem(R.GetRight());
    context.AddItem(R.GetBottom());
    context.EndArray();
    context.EndField();
    context.EndStruct();

    ExploreAdornments(context, pWin->GetPosition() + wxSize(6, -1), pWin, pWin->GetId(), 1);
    ExploreWindows(context, pWin->GetPosition() + wxSize(6, -1), pWin, pWin->GetId(), 1);
    context.EndArray();
    return true;
}

bool GetInfoCommand::SendTracks(const CommandContext& context)
{
    auto& tracks = TrackList::Get(context.project);
    context.StartArray();
    for (auto trk : tracks) {
        auto& trackFocus = TrackFocus::Get(context.project);
        Track* fTrack = trackFocus.Get();

        context.StartStruct();
        context.AddItem(trk->GetName(), "name");
        context.AddBool((trk == fTrack), "focused");
        context.AddBool(trk->GetSelected(), "selected");
        //JKC: Possibly add later...
        //context.AddItem(ChannelView::GetChannelGroupHeight(*trk), "height");
        trk->TypeSwitch([&] (const WaveTrack& t) {
            float vzmin, vzmax;
            WaveformScale::Get(t).GetDisplayBounds(vzmin, vzmax);
            context.AddItem("wave", "kind");
            context.AddItem(t.GetStartTime(), "start");
            context.AddItem(t.GetEndTime(), "end");
            context.AddItem(t.GetPan(), "pan");
            context.AddItem(t.GetVolume(), "volume");
            context.AddItem(t.NChannels(), "channels");
            context.AddBool(t.GetSolo(), "solo");
            context.AddBool(t.GetMute(), "mute");
            context.AddItem(vzmin, "VZoomMin");
            context.AddItem(vzmax, "VZoomMax");
        },
#if defined(USE_MIDI)
                        [&](const NoteTrack&) {
            context.AddItem("note", "kind");
        },
#endif
                        [&](const LabelTrack&) {
            context.AddItem("label", "kind");
        },
                        [&](const TimeTrack&) {
            context.AddItem("time", "kind");
        }
                        );
        context.EndStruct();
    }
    context.EndArray();
    return true;
}

bool GetInfoCommand::SendClips(const CommandContext& context)
{
    auto& tracks = TrackList::Get(context.project);
    int i=0;
    context.StartArray();
    for (auto t : tracks) {
        t->TypeSwitch([&](WaveTrack& waveTrack) {
            for (const auto pInterval : waveTrack.Intervals()) {
                context.StartStruct();
                context.AddItem((double)i, "track");
                context.AddItem(pInterval->GetPlayStartTime(), "start");
                context.AddItem(pInterval->GetPlayEndTime(), "end");
                // Assuming same colors, look at only left channel
                const auto& colors
                    =WaveColorAttachment::Get(**pInterval->Channels().begin());
                context.AddItem(colors.GetColorIndex(), "color");
                context.AddItem(pInterval->GetName(), "name");
                context.EndStruct();
            }
        });
        // Per track numbering counts all tracks
        i++;
    }
    context.EndArray();

    return true;
}

bool GetInfoCommand::SendEnvelopes(const CommandContext& context)
{
    auto& tracks = TrackList::Get(context.project);
    int i=0;
    int j=0;
    context.StartArray();
    for (auto t : tracks) {
        t->TypeSwitch([&](WaveTrack& waveTrack) {
            auto ptrs = waveTrack.SortedIntervalArray();
            j = 0;
            for (auto& pClip : ptrs) {
                context.StartStruct();
                context.AddItem((double)i, "track");
                context.AddItem((double)j, "clip");
                context.AddItem(pClip->GetPlayStartTime(), "start");
                const auto pEnv = &pClip->GetEnvelope();
                context.StartField("points");
                context.StartArray();
                double offset = pEnv->GetOffset();
                for (size_t k = 0; k < pEnv->GetNumberOfPoints(); k++) {
                    context.StartStruct();
                    context.AddItem((*pEnv)[k].GetT() + offset, "t");
                    context.AddItem((*pEnv)[k].GetVal(), "y");
                    context.EndStruct();
                }
                context.EndArray();
                context.EndField();
                context.AddItem(pClip->GetPlayEndTime(), "end");
                context.EndStruct();
                j++;
            }
        });
        // Per track numbering counts all tracks
        i++;
    }
    context.EndArray();

    return true;
}

bool GetInfoCommand::SendLabels(const CommandContext& context)
{
    auto& tracks = TrackList::Get(context.project);
    int i=0;
    context.StartArray();
    for (auto t : tracks) {
        t->TypeSwitch([&](LabelTrack& labelTrack) {
#ifdef VERBOSE_LABELS_FORMATTING
            for (int nn = 0; nn < (int)labelTrack->mLabels.size(); nn++) {
                const auto& label = labelTrack->mLabels[nn];
                context.StartStruct();
                context.AddItem((double)i, "track");
                context.AddItem(label.getT0(), "start");
                context.AddItem(label.getT1(), "end");
                context.AddItem(label.title, "text");
                context.EndStruct();
            }
#else
            context.StartArray();
            context.AddItem((double)i); // Track number.
            context.StartArray();
            for ( const auto& label : labelTrack.GetLabels()) {
                context.StartArray();
                context.AddItem(label.getT0()); // start
                context.AddItem(label.getT1()); // end
                context.AddItem(label.title); //text.
                context.EndArray();
            }
            context.EndArray();
            context.EndArray();
#endif
        });
        // Per track numbering counts all tracks
        i++;
    }
    context.EndArray();

    return true;
}

bool GetInfoCommand::SendSelection(const CommandContext& context)
{
    context.StartStruct();

    const auto& selectedRegion = ViewInfo::Get(context.project).selectedRegion;

    context.AddItem(selectedRegion.t0(), "Start"); // Send selection start position
    context.AddItem(selectedRegion.t1(), "End");   // Send cselection end position

    context.EndStruct();

    return true;
}

/*******************************************************************
The various Explore functions are called from the Send functions,
and may be recursive.  'Send' is the top level.
*******************************************************************/

void GetInfoCommand::ExploreMenu(const CommandContext& context, wxMenu* pMenu, int Id, int depth)
{
    static_cast<void>(Id);//compiler food.
    if (!pMenu) {
        return;
    }

    auto& commandManager = CommandManager::Get(context.project);

    wxMenuItemList list = pMenu->GetMenuItems();
    size_t lcnt = list.size();
    wxMenuItem* item;
    wxString Label;
    wxString Accel;
    CommandID Name;

    for (size_t lndx = 0; lndx < lcnt; lndx++) {
        item = list.Item(lndx)->GetData();
        Label = item->GetItemLabelText();
        Name = commandManager.GetNameFromNumericID(item->GetId());
        Accel = item->GetItemLabel();
        if (Accel.Contains("\t")) {
            Accel = Accel.AfterLast('\t');
        } else {
            Accel = "";
        }
        if (item->IsSeparator()) {
            Label = "----";
        }
        int flags = 0;
        if (item->IsSubMenu()) {
            flags +=1;
        }
        if (item->IsCheck() && item->IsChecked()) {
            flags +=2;
        }

        context.StartStruct();
        context.AddItem(depth, "depth");
        context.AddItem(flags, "flags");
        context.AddItem(Label, "label");
        context.AddItem(Accel, "accel");
        if (!Name.empty()) {
            // using GET to expose CommandID in results of GetInfoCommand...
            // PRL asks, is that all right?
            context.AddItem(Name.GET(), "id");// It is called Scripting ID outside Audacity.
        }
        context.EndStruct();

        if (item->IsSubMenu()) {
            pMenu = item->GetSubMenu();
            ExploreMenu(context, pMenu, item->GetId(), depth + 1);
        }
    }
}

void GetInfoCommand::ExploreAdornments(const CommandContext& context,
                                       wxPoint WXUNUSED(P), wxWindow* pWin, int WXUNUSED(Id), int depth)
{
    // Dang! wxMenuBar returns bogus screen rect.
    // We're going to have to fake it instead.
    //wxMenuBar * pBar = context.GetProject()->GetMenuBar();
    //wxRect R = pBar->GetScreenRect();
    //R.SetPosition( R.GetPosition() - P );

    wxRect R1 = pWin->GetScreenRect();
    wxSize s = pWin->GetWindowBorderSize();
    wxRect R(2, 32, R1.GetWidth() - s.GetWidth() * 2 - 16, 22);

    context.StartStruct();
    context.AddItem(depth, "depth");
    context.AddItem("MenuBar", "label");
    context.StartField("box");
    context.StartArray();
    context.AddItem(R.GetLeft());
    context.AddItem(R.GetTop());
    context.AddItem(R.GetRight());
    context.AddItem(R.GetBottom());
    context.EndArray();
    context.EndField();
    context.EndStruct();
}

void GetInfoCommand::ExploreTrackPanel(const CommandContext& context,
                                       wxPoint P, int depth)
{
    AudacityProject* pProj = &context.project;
    auto& tp = TrackPanel::Get(*pProj);
    wxRect panelRect{ {}, tp.GetSize() };
    for (auto pTrack : TrackList::Get(*pProj)) {
        for (auto pChannel : pTrack->Channels()) {
            auto rulers = tp.FindRulerRects(*pChannel);
            for (auto& R : rulers) {
                if (!R.Intersects(panelRect)) {
                    continue;
                }
                R.SetPosition(R.GetPosition() + P);
                context.StartStruct();
                context.AddItem(depth, "depth");
                context.AddItem("VRuler", "label");
                context.StartField("box");
                context.StartArray();
                context.AddItem(R.GetLeft());
                context.AddItem(R.GetTop());
                context.AddItem(R.GetRight());
                context.AddItem(R.GetBottom());
                context.EndArray();
                context.EndField();
                context.EndStruct();
            }
        }
    }
}

void GetInfoCommand::ExploreWindows(const CommandContext& context,
                                    wxPoint P, wxWindow* pWin, int Id, int depth)
{
    static_cast<void>(Id);//Compiler food.

    if (pWin->GetName() == "Track Panel") {
        wxRect R = pWin->GetScreenRect();
        ExploreTrackPanel(context, R.GetPosition() - P, depth);
        return;
    }
    wxWindowList list = pWin->GetChildren();
    size_t lcnt = list.size();

    for (size_t lndx = 0; lndx < lcnt; lndx++) {
        wxWindow* item = list[lndx];
        if (!item->IsShown()) {
            continue;
        }
        wxRect R = item->GetScreenRect();
        R.SetPosition(R.GetPosition() - P);
        wxString Name = item->GetName();
        // Ignore staticLine and StaticBitmap.
        if (Name.StartsWith("static")) {
            continue;
        }
        // Ignore anonymous panels.
        if (Name == "panel") {
            continue;
        }
        if (Name.empty()) {
            Name = wxString("*") + item->GetToolTipText();
        }

        context.StartStruct();
        context.AddItem(depth, "depth");
        context.AddItem(Name, "label");
        context.AddItem(item->GetId(), "id");
        context.StartField("box");
        context.StartArray();
        context.AddItem(R.GetLeft());
        context.AddItem(R.GetTop());
        context.AddItem(R.GetRight());
        context.AddItem(R.GetBottom());
        context.EndArray();
        context.EndField();
        context.EndStruct();

        ExploreWindows(context, P, item, item->GetId(), depth + 1);
    }
}

namespace {
using namespace MenuRegistry;

// Register menu items

AttachedItem sAttachment{
    // Note that the PLUGIN_SYMBOL must have a space between words,
    // whereas the short-form used here must not.
    // (So if you did write "Compare Audio" for the PLUGIN_SYMBOL name, then
    // you would have to use "CompareAudio" here.)
    Command(wxT("GetInfo"), XXO("Get Info..."),
            CommandDispatch::OnAudacityCommand, AudioIONotBusyFlag()),
    wxT("Optional/Extra/Part2/Scriptables2")
};
}
