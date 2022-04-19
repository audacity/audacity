//
// Created by lalitsc on 4/19/22.
//

#ifdef USE_LIBOPUS

#include "../ShuttleGui.h"
#include "../widgets/AudacityMessageBox.h"
#include "../widgets/ProgressDialog.h"
#include "Export.h"
#include "FileIO.h"
#include "ProjectRate.h"
#include "ProjectStatus.h"
#include <opus/opus.h>

#define SAMPLES_PER_RUN 8192u

class ExportOpusOptions final : public wxPanelWrapper {
public:
    ExportOpusOptions(wxWindow *parent, int format);
    virtual ~ExportOpusOptions();

    void PopulateOrExchange(ShuttleGui &S);
    bool TransferDataToWindow() override;
    bool TransferDataFromWindow() override;
};

ExportOpusOptions::ExportOpusOptions(wxWindow *parent, int WXUNUSED(format))
    : wxPanelWrapper(parent, wxID_ANY) {
    ShuttleGui S(this, eIsCreatingFromPrefs);
    PopulateOrExchange(S);

    TransferDataToWindow();
}

ExportOpusOptions::~ExportOpusOptions() {
    TransferDataFromWindow();
}

ChoiceSetting OpusSamplingRate{
        wxT("/FileFormats/OpusSamplingRate"),
        {ByColumns,
         {XO("8000"),
          XO("12000"),
          XO("16000"),
          XO("24000"),
          XO("48000")},
         {wxT("8000"),
          wxT("12000"),
          wxT("16000"),
          wxT("24000"),
          wxT("48000")}}};

void ExportOpusOptions::PopulateOrExchange(ShuttleGui &S) {
    S.StartVerticalLay();
    {
        S.StartHorizontalLay(wxCENTER);
        {
            S.StartMultiColumn(2, wxCENTER);
            {
                S.TieChoice(XXO("Sampling rate:"), OpusSamplingRate);
            }
            S.EndMultiColumn();
        }
        S.EndHorizontalLay();
    }
    S.EndVerticalLay();
}

bool ExportOpusOptions::TransferDataToWindow() {
    return true;
}

bool ExportOpusOptions::TransferDataFromWindow() {
    ShuttleGui S(this, eIsSavingToPrefs);
    PopulateOrExchange(S);

    gPrefs->Flush();

    return true;
}

class ExportOpus final : public ExportPlugin {
public:
    ExportOpus();

    // Required

    void OptionsCreate(ShuttleGui &S, int format) override;
    ProgressResult Export(AudacityProject *project,
                          std::unique_ptr<ProgressDialog> &pDialog,
                          unsigned channels,
                          const wxFileNameWrapper &fName,
                          bool selectedOnly,
                          double t0,
                          double t1,
                          MixerSpec *mixerSpec = NULL,
                          const Tags *metadata = NULL,
                          int subformat = 0) override;
};

ExportOpus::ExportOpus() : ExportPlugin() {
    AddFormat();
    SetFormat(wxT("OPUS"), 0);
    AddExtension(wxT("opus"), 0);
    SetMaxChannels(2, 0);
    SetCanMetaData(true, 0);
    SetDescription(XO("Opus Files"), 0);
}

void ExportOpus::OptionsCreate(ShuttleGui &S, int format) {
    S.AddWindow(safenew ExportOpusOptions{S.GetParent(), format});
}

ProgressResult ExportOpus::Export(AudacityProject *project, std::unique_ptr<ProgressDialog> &pDialog,
                                  unsigned int channels, const wxFileNameWrapper &fName, bool selectedOnly, double t0,
                                  double t1, MixerSpec *mixerSpec, const Tags *metadata, int subformat) {
    double rate = ProjectRate::Get(*project).GetRate();
    const auto &tracks = TrackList::Get(*project);

    wxLogNull logNo;// temporarily disable wxWidgets error messages
    auto updateResult = ProgressResult::Success;

    FileIO outFile(fName, FileIO::Output);

    if (!outFile.IsOpened()) {
        AudacityMessageBox(XO("Unable to open target file for writing"));
        return ProgressResult::Cancelled;
    }

    int errorCode;
    OpusEncoder *encoder = opus_encoder_create((opus_int32) rate, (int) channels, OPUS_APPLICATION_AUDIO, &errorCode);

    if (errorCode != OPUS_OK) {
        AudacityMessageBox(XO("Unable to export"));
        return ProgressResult::Cancelled;
    }

    errorCode = opus_encoder_ctl(encoder, OPUS_SET_BITRATE(128000));

    if (errorCode != OPUS_OK) {
        AudacityMessageBox(XO("Unable to export"));
        return ProgressResult::Cancelled;
    }

    auto mixer = CreateMixer(tracks, selectedOnly,
                             t0, t1,
                             channels, SAMPLES_PER_RUN, false,
                             rate, floatSample, mixerSpec);

    InitProgress(pDialog, fName,
                 selectedOnly
                         ? XO("Exporting the selected audio as Opus")
                         : XO("Exporting the audio as Opus"));
    auto &progress = *pDialog;

    unsigned char encodedBits[SAMPLES_PER_RUN];

    while (updateResult == ProgressResult::Success) {
        auto samplesThisRun = mixer->Process(SAMPLES_PER_RUN);

        if (samplesThisRun == 0) {//stop encoding
            break;
        }

        auto mixedBuffer = mixer->GetBuffer();

        int bytesEncoded = opus_encode(encoder, (opus_int16 *) mixedBuffer, SAMPLE_SIZE(int16Sample) / 2, encodedBits, SAMPLES_PER_RUN);

        if (bytesEncoded < 0) {
            AudacityMessageBox(XO("Unable to export"));
            return ProgressResult::Cancelled;
        }

        if (outFile.Write(encodedBits, SAMPLES_PER_RUN).GetLastError()) {
            ShowDiskFullExportErrorDialog(fName);
            return ProgressResult::Cancelled;
        }

        updateResult = progress.Update(mixer->MixGetCurrentTime() - t0, t1 - t0);
    }

    opus_encoder_destroy(encoder);

    return updateResult;
}

#endif