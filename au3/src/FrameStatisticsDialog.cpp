/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  FrameStatisticsDialog.cpp

  Dmitry Vedenko

**********************************************************************/
#include "FrameStatisticsDialog.h"

#include "MemoryX.h"
#include "FrameStatistics.h"

#include "ShuttleGui.h"
#include "wxPanelWrapper.h"

#include <string>

#include <wx/stattext.h>

namespace {
class Dialog : public wxDialogWrapper
{
public:

    Dialog()
        : wxDialogWrapper(nullptr, wxID_ANY, Verbatim("Frame Statistics"))
    {
        ShuttleGui S(this, eIsCreating);

        S.Style(wxNO_BORDER | wxTAB_TRAVERSAL).Prop(true).StartPanel();
        {
            S.StartVerticalLay(true);
            {
                S.AddFixedText(Verbatim("Track Panel Rendering"));
                AddSection(S, FrameStatistics::SectionID::TrackPanel);
                S.AddFixedText(Verbatim("Waveform Rendering (per clip)"));
                AddSection(S, FrameStatistics::SectionID::WaveformView);
                S.AddFixedText(Verbatim("WaveDataCache Lookups"));
                AddSection(S, FrameStatistics::SectionID::WaveDataCache);
                S.AddFixedText(Verbatim("WaveBitmapCache Preprocess"));
                AddSection(S, FrameStatistics::SectionID::WaveBitmapCachePreprocess);
                S.AddFixedText(Verbatim("WaveBitmapCache Lookups"));
                AddSection(S, FrameStatistics::SectionID::WaveBitmapCache);
            }
            S.EndVerticalLay();
        }
        S.EndPanel();

        Layout();
        Fit();

        mStatisticsUpdated = FrameStatistics::Subscribe(
            [this](FrameStatistics::SectionID sectionID) {
            mSections[size_t(sectionID)].Dirty = true;
        });

        Bind(
            wxEVT_IDLE,
            [this](wxIdleEvent& evt)
        {
            for (size_t i = 0; i < size_t(FrameStatistics::SectionID::Count);
                 ++i) {
                if (mSections[i].Dirty) {
                    SectionUpdated(FrameStatistics::SectionID(i));
                }
            }
        });
    }

private:
    void AddSection(ShuttleGui& S, FrameStatistics::SectionID sectionID)
    {
        S.StartMultiColumn(2, wxEXPAND);
        {
            S.AddFixedText(Verbatim("Last:"));
            mSections[size_t(sectionID)].Last = S.AddVariableText({});

            S.AddFixedText(Verbatim("Min:"));
            mSections[size_t(sectionID)].Min = S.AddVariableText({});

            S.AddFixedText(Verbatim("Max:"));
            mSections[size_t(sectionID)].Max = S.AddVariableText({});

            S.AddFixedText(Verbatim("Avg:"));
            mSections[size_t(sectionID)].Avg = S.AddVariableText({});

            S.AddFixedText(Verbatim("Events:"));
            mSections[size_t(sectionID)].Events = S.AddVariableText({});
        }
        S.EndMultiColumn();

        SectionUpdated(sectionID);
    }

    wxString FormatTime(FrameStatistics::Duration duration)
    {
        using namespace std::chrono;

        const auto mcs = duration_cast<microseconds>(duration);

        return std::to_string(mcs.count() / 1000.0) + " ms";
    }

    void SectionUpdated(FrameStatistics::SectionID sectionID)
    {
        Section& section = mSections[size_t(sectionID)];
        const auto& profilerSection = FrameStatistics::GetSection(sectionID);

        if (profilerSection.GetEventsCount() > 0) {
            section.Last->SetLabel(FormatTime(profilerSection.GetLastDuration()));
            section.Min->SetLabel(FormatTime(profilerSection.GetMinDuration()));
            section.Max->SetLabel(FormatTime(profilerSection.GetMaxDuration()));
            section.Avg->SetLabel(FormatTime(profilerSection.GetAverageDuration()));
        } else {
            section.Last->SetLabel(L"n/a");
            section.Min->SetLabel(L"n/a");
            section.Max->SetLabel(L"n/a");
            section.Avg->SetLabel(L"n/a");
        }

        section.Events->SetLabel(std::to_string(profilerSection.GetEventsCount()));

        section.Dirty = false;
    }

    struct Section final
    {
        wxStaticText* Last;
        wxStaticText* Min;
        wxStaticText* Max;
        wxStaticText* Avg;
        wxStaticText* Events;

        bool Dirty { true };
    };

    Section mSections[size_t(FrameStatistics::SectionID::Count)];

    Observer::Subscription mStatisticsUpdated;
};

Destroy_ptr<Dialog> sDialog;
}

void FrameStatisticsDialog::Show(bool show)
{
    if (!show) {
        if (sDialog != nullptr) {
            sDialog->Show(false);
        }

        return;
    }

    if (sDialog == nullptr) {
        sDialog.reset(safenew Dialog);
    }

    sDialog->Show(true);
}

void FrameStatisticsDialog::Destroy()
{
    sDialog.reset();
}
