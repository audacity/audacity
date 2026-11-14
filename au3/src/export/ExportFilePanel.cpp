#include "ExportFilePanel.h"

#include <numeric>
#include <wx/stattext.h>
#include <wx/button.h>
#include <wx/choice.h>
#include <wx/textctrl.h>
#include <wx/radiobut.h>
#include <wx/regex.h>
#include <wx/wupdlock.h>

#include "Export.h"
#include "ExportMixerDialog.h"
#include "ProjectRate.h"
#include "Mix.h"
#include "WaveTrack.h"

#include "ShuttleGui.h"
#include "ExportOptionsHandler.h"
#include "ExportPluginRegistry.h"
#include "ExportUtils.h"
#include "WindowAccessible.h"

#if wxUSE_ACCESSIBILITY
#include "WindowAccessible.h"
#endif

wxDEFINE_EVENT(AUDACITY_EXPORT_FORMAT_CHANGE_EVENT, wxCommandEvent);

namespace {
const ExportOptionsEditor::SampleRateList
    DefaultRates {
    8000,
    11025,
    16000,
    22050,
    32000,
    44100,
    48000,
    88200,
    96000,
    176400,
    192000,
    352800,
    384000
};

enum
{
    FolderBrowseID = wxID_HIGHEST,

    FormatID,

    AudioMixModeMonoID,
    AudioMixModeStereoID,
    AudioMixModeCustomID,

    AudioChannelsConfigureID,

    SampleRateID,
};

class CustomSampleRateDialog final : public wxDialogWrapper
{
    enum {
        CustomSampleRateID = wxID_HIGHEST
    };
public:
    CustomSampleRateDialog(wxWindow* parent, int defaultSampleRate = 44100)
        : wxDialogWrapper(parent, wxID_ANY, XO("Custom Sample Rate"), wxDefaultPosition, { -1, 160 })
        , mSampleRate(defaultSampleRate)
    {
        ShuttleGui S(this, eIsCreating);
        S.SetBorder(5);
        S.StartHorizontalLay(wxEXPAND);
        {
            S.StartMultiColumn(2, wxALIGN_CENTER_VERTICAL);
            {
                S.Id(CustomSampleRateID).AddNumericTextBox(XO("New sample rate (Hz):"), wxString::Format("%d", mSampleRate), 0);
            }
            S.EndMultiColumn();
        }
        S.EndHorizontalLay();

        S.AddStandardButtons();
    }

    int GetSampleRate() const noexcept
    {
        return mSampleRate;
    }

private:

    void OnSampleRateChange(wxCommandEvent& event)
    {
        long rate;
        if (event.GetString().ToLong(&rate)) {
            mSampleRate = static_cast<int>(rate);
        }
    }

    int mSampleRate;

    DECLARE_EVENT_TABLE();
};
}

BEGIN_EVENT_TABLE(CustomSampleRateDialog, wxDialogWrapper)
EVT_TEXT(CustomSampleRateID, CustomSampleRateDialog::OnSampleRateChange)
END_EVENT_TABLE()

BEGIN_EVENT_TABLE(ExportFilePanel, wxPanelWrapper)
EVT_BUTTON(FolderBrowseID, ExportFilePanel::OnFolderBrowse)

EVT_CHOICE(FormatID, ExportFilePanel::OnFormatChange)

EVT_RADIOBUTTON(AudioMixModeMonoID, ExportFilePanel::OnChannelsChange)
EVT_RADIOBUTTON(AudioMixModeStereoID, ExportFilePanel::OnChannelsChange)
EVT_RADIOBUTTON(AudioMixModeCustomID, ExportFilePanel::OnChannelsChange)

EVT_BUTTON(AudioChannelsConfigureID, ExportFilePanel::OnChannelsConfigure)

EVT_CHOICE(SampleRateID, ExportFilePanel::OnSampleRateChange)
END_EVENT_TABLE()

ExportFilePanel::ExportFilePanel(AudacityProject& project,
                                 bool monoStereoMode,
                                 wxWindow* parent,
                                 wxWindowID winid)
    : wxPanelWrapper(parent, winid)
    , mMonoStereoMode(monoStereoMode)
    , mProject(project)
{
    ShuttleGui S(this, eIsCreating);
    PopulateOrExchange(S);
}

ExportFilePanel::~ExportFilePanel() = default;

void ExportFilePanel::PopulateOrExchange(ShuttleGui& S)
{
    TranslatableStrings formats;
    if (S.GetMode() == eIsCreating) {
        for (auto [plugin, formatIndex] : ExportPluginRegistry::Get()) {
            auto formatInfo = plugin->GetFormatInfo(formatIndex);
            formats.push_back(formatInfo.description);
        }
    }

    S.SetBorder(5);
    S.StartMultiColumn(3, wxEXPAND);
    {
        S.SetStretchyCol(1);

        mFullName = S.AddTextBox(XO("File &Name:"), {}, 0);
        mFullName->Bind(wxEVT_KILL_FOCUS, &ExportFilePanel::OnFullNameFocusKill, this);
        S.AddSpace(1);

        mFolder = S.AddTextBox(XO("Fo&lder:"), {}, 0);
        S.Id(FolderBrowseID).AddButton(XO("&Browse..."));

        mFormat = S.Id(FormatID).AddChoice(XO("&Format:"), formats);
        S.AddSpace(1);
    }
    S.EndMultiColumn();

    S.SetBorder(5);
    S.StartStatic(XO("Audio options"));
    {
        S.StartTwoColumn();
        {
            if (auto prompt = S.AddPrompt(XO("Channels"))) {
                prompt->SetMinSize({ 140, -1 });
            }

            S.StartHorizontalLay(wxALIGN_LEFT);
            {
                S.SetBorder(2);

                const int channels = 2;

                mMono = S.Id(AudioMixModeMonoID).AddRadioButton(XO("M&ono"), 1, channels);
                mStereo = S.Id(AudioMixModeStereoID).AddRadioButtonToGroup(XO("&Stereo"), 2, channels);
                if (!mMonoStereoMode) {
                    //i18n-hint refers to custom channel mapping configuration
                    mCustomMapping = S.Id(AudioMixModeCustomID).AddRadioButtonToGroup(XO("Custom mappin&g"), 0, true);
                    mCustomizeChannels = S.Id(AudioChannelsConfigureID)
                                         //i18n-hint accessibility hint, refers to export channel configuration
                                         .Name(XO("Configure custom mapping"))
                                         .AddButton(XO("Configure"));
#if wxUSE_ACCESSIBILITY
                    safenew WindowAccessible(mCustomizeChannels);
#endif
                }
            }
            S.EndHorizontalLay();

            S.SetBorder(5);

            if (auto prompt = S.AddPrompt(XO("Sample &Rate"))) {
                prompt->SetMinSize({ 140, -1 });
            }

            S.StartHorizontalLay(wxALIGN_LEFT);
            {
                mRates = S.Id(SampleRateID).AddChoice({}, {});
            }
            S.EndHorizontalLay();
        }
        S.EndTwoColumn();

        mAudioOptionsPanel = S.StartPanel();
        {
        }
        S.EndPanel();
    }
    S.EndStatic();
}

void ExportFilePanel::Init(const wxFileName& filename,
                           int sampleRate,
                           const wxString& format,
                           int channels,
                           const ExportProcessor::Parameters& parameters,
                           const MixerOptions::Downmix* mixerSpec)
{
    mFolder->SetValue(filename.GetPath());
    mFullName->SetValue(filename.GetFullName());
    mSampleRate = sampleRate;

    auto selectedFormatIndex = 0;
    if (!format.empty()) {
        auto counter = 0;
        for (auto [plugin, formatIndex] : ExportPluginRegistry::Get()) {
            if (plugin->GetFormatInfo(formatIndex).format.IsSameAs(format)) {
                selectedFormatIndex = counter;
                break;
            }
            ++counter;
        }
    }

    if (mixerSpec != nullptr) {
        assert(!mMonoStereoMode);
        *mMixerSpec = *mixerSpec;
        mCustomMapping->SetValue(true);
    } else {
        int numChannels = channels;
        if (numChannels == 0) {
            numChannels = 1;
            const auto waveTracks
                =ExportUtils::FindExportWaveTracks(
                      TrackList::Get(mProject),
                      false);
            for (const auto track : waveTracks) {
                if (track->NChannels() >= 2 || track->GetPan() != .0f) {
                    numChannels = 2;
                    break;
                }
            }
        }
        if (numChannels == 1) {
            mMono->SetValue(true);
        } else {
            mStereo->SetValue(true);
        }
    }

    mFormat->SetSelection(selectedFormatIndex);

    ChangeFormat(selectedFormatIndex);

    if (!parameters.empty()) {
        mOptionsHandler->SetParameters(parameters);
    }

    if (mCustomizeChannels != nullptr) {
        mCustomizeChannels->Enable(mCustomMapping->GetValue());
    }
}

// Used as part of fix for issue #4960
void ExportFilePanel::SetInitialFocus()
{
    mFullName->SetFocus();
    mFullName->SelectAll();
}

void ExportFilePanel::SetCustomMappingEnabled(bool enabled)
{
    if (mMonoStereoMode) {
        return;
    }

    if (!enabled && mCustomMapping->GetValue()) {
        if (mStereo->IsEnabled()) {
            mStereo->SetValue(true);
        } else {
            mMono->SetValue(true);
        }
    }
    mCustomMapping->Enable(enabled);
    mCustomizeChannels->Enable(enabled);
}

wxString ExportFilePanel::GetPath() const
{
    return mFolder->GetValue();
}

wxString ExportFilePanel::GetFullName()
{
    ValidateAndFixExt();
    return mFullName->GetValue();
}

const ExportPlugin* ExportFilePanel::GetPlugin() const
{
    return mSelectedPlugin;
}

int ExportFilePanel::GetFormat() const
{
    return mSelectedFormatIndex;
}

int ExportFilePanel::GetSampleRate() const
{
    return mSampleRate;
}

std::optional<ExportProcessor::Parameters> ExportFilePanel::GetParameters() const
{
    if (mOptionsHandler->TransferDataFromEditor()) {
        return { mOptionsHandler->GetParameters() }
    }
    return std::nullopt;
}

int ExportFilePanel::GetChannels() const
{
    if (mCustomMapping != nullptr && mCustomMapping->GetValue()) {
        return 0;
    }
    return mMono->GetValue() ? 1 : 2;
}

MixerOptions::Downmix* ExportFilePanel::GetMixerSpec() const
{
    return mMixerSpec.get();
}

void ExportFilePanel::ValidateAndFixExt()
{
    if (mSelectedPlugin == nullptr) {
        return;
    }

    const auto formatInfo = mSelectedPlugin->GetFormatInfo(mSelectedFormatIndex);
    if (formatInfo.extensions.empty()) {
        return;
    }

    wxFileName filename;
    filename.SetFullName(mFullName->GetValue());
    const auto desiredExt = filename.GetExt().Trim();

    //See https://github.com/audacity/audacity/issues/5823
    //check if extension is valid, i.e. does not contain whitespace characters.
    //Otherwise everything after '.'(if present) is considered to be a part of name.
    if (wxRegEx{ R"(^[^ ]+$)" }.Matches(desiredExt)) {
        auto it = std::find_if(
            formatInfo.extensions.begin(),
            formatInfo.extensions.end(),
            // if typed extension uses different case (e.g. MP3 instead of mp3)
            // we'll reset the file extension to one provided by FormatInfo
            [&](const auto& ext) { return desiredExt.IsSameAs(ext, false); });

        if (it == formatInfo.extensions.end()) {
            it = formatInfo.extensions.begin();
        }

        if (!it->empty() && !it->IsSameAs(filename.GetExt())) {
            filename.SetExt(*it);
            mFullName->SetValue(filename.GetFullName());
        }
    } else if (!formatInfo.extensions.front().empty()) {
        auto fullname = filename.GetFullName();
        if (!fullname.EndsWith(".")) {
            fullname.Append(".");
        }
        fullname.Append(formatInfo.extensions.front());
        filename.SetFullName(fullname);
        mFullName->SetValue(filename.GetFullName());
    }
}

void ExportFilePanel::OnFullNameFocusKill(wxFocusEvent& event)
{
    //When user has finished typing make sure that file extension
    //is one of extensions supplied by FormatInfo

    event.Skip();

    ValidateAndFixExt();
}

void ExportFilePanel::OnFormatChange(wxCommandEvent& event)
{
    ChangeFormat(event.GetInt());
    event.Skip();
}

void ExportFilePanel::OnSampleRateChange(wxCommandEvent& event)
{
    const auto clientData = event.GetClientData();
    if (clientData == nullptr) {
        CustomSampleRateDialog dialog(this, mSampleRate);
        if (dialog.ShowModal() == wxID_OK
            && dialog.GetSampleRate() > 0) {
            mSampleRate = dialog.GetSampleRate();
        }
        UpdateSampleRateList();
    } else {
        mSampleRate = *reinterpret_cast<const int*>(&clientData);
    }
}

void ExportFilePanel::OnFolderBrowse(wxCommandEvent& event)
{
    FileNames::FileTypes fileTypes;

    for (auto [plugin, formatIndex] : ExportPluginRegistry::Get()) {
        const auto formatInfo = plugin->GetFormatInfo(formatIndex);
        fileTypes.emplace_back(formatInfo.description, formatInfo.extensions);
    }
    wxFileDialog fd(this, _("Choose a location to save the exported files"),
                    mFolder->GetValue(),
                    mFullName->GetValue(),
                    FileNames::FormatWildcard(fileTypes),
                    wxFD_SAVE);
    fd.SetFilterIndex(mFormat->GetSelection());

    if (fd.ShowModal() == wxID_OK) {
        wxFileName filepath(fd.GetPath());
        mFolder->SetValue(filepath.GetPath());
        mFullName->SetValue(filepath.GetFullName());
        const auto selectedFormat = fd.GetFilterIndex();
        if (selectedFormat != mFormat->GetSelection()) {
            mFormat->SetSelection(selectedFormat);
            ChangeFormat(selectedFormat);
        }
    }
}

void ExportFilePanel::OnChannelsChange(wxCommandEvent& event)
{
    if (mCustomizeChannels != nullptr) {
        mCustomizeChannels->Enable(event.GetId() == AudioMixModeCustomID);
    }
}

void ExportFilePanel::OnChannelsConfigure(wxCommandEvent& event)
{
    //Configure for all tracks, but some channels may turn out to be silent
    //if exported region does not contain audio samples
    auto waveTracks = TrackList::Get(mProject).Any<const WaveTrack>();

    auto mixerSpec = std::make_unique<MixerOptions::Downmix>(*mMixerSpec);

    ExportMixerDialog md(waveTracks,
                         mixerSpec.get(),
                         nullptr,
                         1,
                         XO("Advanced Mixing Options"));
    if (md.ShowModal() == wxID_OK) {
        mMixerSpec.swap(mixerSpec);
    }
}

void ExportFilePanel::ChangeFormat(int index)
{
    mSelectedPlugin = nullptr;

    wxWindowUpdateLocker wndupdlck(mAudioOptionsPanel);

    auto formatCounter = 0;

    for (auto [plugin, formatIndex] : ExportPluginRegistry::Get()) {
        if (formatCounter != index) {
            ++formatCounter;
            continue;
        }

        mOptionsChangeSubscription.Reset();

        mSelectedPlugin = plugin;
        mSelectedFormatIndex = formatIndex;

        ValidateAndFixExt();

        mAudioOptionsPanel->SetSizer(nullptr);
        mAudioOptionsPanel->DestroyChildren();

        ShuttleGui S(mAudioOptionsPanel, eIsCreating, true, { 1, 1 });
        mOptionsHandler = std::make_unique<ExportOptionsHandler>(S, *plugin, formatIndex);
        mOptionsChangeSubscription = mOptionsHandler->Subscribe(*this, &ExportFilePanel::OnOptionsHandlerEvent);

        const auto formatInfo = plugin->GetFormatInfo(formatIndex);
        UpdateMaxChannels(formatInfo.maxChannels);

        UpdateSampleRateList();

        mAudioOptionsPanel->Layout();

        wxPostEvent(GetParent(), wxCommandEvent { AUDACITY_EXPORT_FORMAT_CHANGE_EVENT, GetId() });

        return;
    }
}

void ExportFilePanel::OnOptionsHandlerEvent(const ExportOptionsHandlerEvent& e)
{
    switch (e.type) {
    case ExportOptionsHandlerEvent::SampleRateListChange:
        UpdateSampleRateList();
        break;
    case ExportOptionsHandlerEvent::FormatInfoChange:
    {
        const auto formatInfo = mSelectedPlugin->GetFormatInfo(mSelectedFormatIndex);
        ValidateAndFixExt();
        UpdateMaxChannels(formatInfo.maxChannels);
    } break;
    }
}

void ExportFilePanel::UpdateMaxChannels(unsigned maxChannels)
{
    if (maxChannels < 2 && mStereo->GetValue()) {
        mMono->SetValue(true);
    }
    mStereo->Enable(maxChannels > 1);
    if (!mMonoStereoMode) {
        const auto mixerMaxChannels = std::clamp(
            maxChannels,
            // JKC: This is an attempt to fix a 'watching brief' issue, where the slider is
            // sometimes not slidable.  My suspicion is that a mixer may incorrectly
            // state the number of channels - so we assume there are always at least two.
            // The downside is that if someone is exporting to a mono device, the dialog
            // will allow them to output to two channels. Hmm.  We may need to revisit this.
            // STF (April 2016): AMR (narrowband) and MP3 may export 1 channel.
            1u,
            MaxExportChannels);
        if (!mMixerSpec || mMixerSpec->GetMaxNumChannels() != mixerMaxChannels) {
            auto waveTracks = TrackList::Get(mProject).Any<const WaveTrack>();
            mMixerSpec = std::make_unique<MixerOptions::Downmix>(
                waveTracks.sum([](const auto track) { return track->NChannels(); }),
                mixerMaxChannels);
        }
    }
}

void ExportFilePanel::UpdateSampleRateList()
{
    auto availableRates = mOptionsHandler->GetSampleRateList();
    std::sort(availableRates.begin(), availableRates.end());

    const auto* rates = availableRates.empty() ? &DefaultRates : &availableRates;

    mRates->Clear();

    void* clientData;
    int customRate = mSampleRate;
    int selectedItemIndex = 0;
    //Prefer lowest possible sample rate that is not less than mSampleRate.
    //Initialize with highest value, so that if all available rates are less
    //than mSampleRate then we will choose highest rate
    int preferredRate = rates->back();
    int preferredItemIndex = rates->size() - 1;
    for (auto rate : *rates) {
        *reinterpret_cast<int*>(&clientData) = rate;
        const auto itemIndex
            =mRates->Append(
                  XO("%d Hz").Format(rate).Translation(),
                  clientData);
        if (rate == mSampleRate) {
            customRate = 0;
            selectedItemIndex = itemIndex;
        }
        if (rate >= mSampleRate && rate < preferredRate) {
            preferredItemIndex = itemIndex;
            preferredRate = rate;
        }
    }

    if (rates == &DefaultRates) {
        if (customRate != 0) {
            *reinterpret_cast<int*>(&clientData) = customRate;
            selectedItemIndex
                =mRates->Append(
                      XO("%d Hz (custom)").Format(customRate).Translation(),
                      clientData);
        }
        mRates->Append(_("Other..."));
    } else if (customRate != 0) {//sample rate not in the list
        auto selectedRate = (*rates)[preferredItemIndex];
        mSampleRate = selectedRate;
        selectedItemIndex = preferredItemIndex;
    }
    mRates->SetSelection(selectedItemIndex);
}
