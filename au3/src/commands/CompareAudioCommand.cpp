/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2018 Audacity Team
   License: wxwidgets

   Dan Horgan
   James Crook

******************************************************************//**

\file CompareAudioCommand.cpp
\brief Contains definitions for CompareAudioCommand class

\class CompareAudioCommand
\brief Returns information about the amount of audio that is about a certain
threshold of difference in two selected tracks

*//*******************************************************************/

#include "CompareAudioCommand.h"

#include "CommandDispatch.h"
#include "MenuRegistry.h"
#include "../CommonCommandFlags.h"
#include "LoadCommands.h"
#include "ViewInfo.h"
#include "WaveTrack.h"

#include <float.h>

#include "SettingsVisitor.h"
#include "ShuttleGui.h"
#include "AudacityMessageBox.h"
#include "../widgets/valnum.h"
#include "CommandContext.h"

const ComponentInterfaceSymbol CompareAudioCommand::Symbol
{ XO("Compare Audio") };

namespace {
BuiltinCommandsModule::Registration< CompareAudioCommand > reg;
}

extern void RegisterCompareAudio(Registrar& R)
{
    R.AddCommand(std::make_unique<CompareAudioCommand>());
// std::unique_ptr<CommandOutputTargets> &&target
//   return std::make_shared<CompareAudioCommand>(*this, std::move(target));
}

template<bool Const>
bool CompareAudioCommand::VisitSettings(SettingsVisitorBase<Const>& S)
{
    S.Define(errorThreshold,  wxT("Threshold"),   0.0f,  0.0f,    0.01f,    1.0f);
    return true;
}

bool CompareAudioCommand::VisitSettings(SettingsVisitor& S)
{ return VisitSettings<false>(S); }

bool CompareAudioCommand::VisitSettings(ConstSettingsVisitor& S)
{ return VisitSettings<true>(S); }

void CompareAudioCommand::PopulateOrExchange(ShuttleGui& S)
{
    S.AddSpace(0, 5);

    S.StartMultiColumn(2, wxALIGN_CENTER);
    {
        S.TieTextBox(XXO("Threshold:"), errorThreshold);
    }
    S.EndMultiColumn();
}

// Update member variables with project selection data (and validate)
bool CompareAudioCommand::GetSelection(const CommandContext& context, AudacityProject& proj)
{
    // Get the selected time interval
    auto& selectedRegion = ViewInfo::Get(proj).selectedRegion;
    mT0 = selectedRegion.t0();
    mT1 = selectedRegion.t1();
    if (mT0 >= mT1) {
        context.Error(wxT("There is no selection!"));
        return false;
    }

    // Get the selected tracks and check that there are at least two to
    // compare
    auto trackRange = TrackList::Get(proj).Selected<const WaveTrack>();
    mTrack0 = *trackRange.first;
    if (!mTrack0) {
        context.Error(wxT("No tracks selected! Select two tracks to compare."));
        return false;
    }
    mTrack1 = *++trackRange.first;
    if (!mTrack1) {
        context.Error(wxT("Only one track selected! Select two tracks to compare."));
        return false;
    }
    if (mTrack0->NChannels() != mTrack1->NChannels()) {
        context.Error(wxT("Selected tracks must have the same number of channels!"));
        return false;
    }
    if (*++trackRange.first) {
        context.Status(wxT("More than two tracks selected - only the first two will be compared."));
    }
    return true;
}

double CompareAudioCommand::CompareSample(double value1, double value2)
{
    return fabs(value1 - value2);
}

inline int min(int a, int b)
{
    return (a < b) ? a : b;
}

bool CompareAudioCommand::Apply(const CommandContext& context)
{
    if (!GetSelection(context, context.project)) {
        return false;
    }

    wxString msg = wxT("Comparing tracks '");
    msg += mTrack0->GetName() + wxT("' and '")
           + mTrack1->GetName() + wxT("'.");
    context.Status(msg);

    long errorCount = 0;
    // Initialize buffers for track data to be analyzed
    auto buffSize = std::min(mTrack0->GetMaxBlockSize(), mTrack1->GetMaxBlockSize());

    Floats buff0{ buffSize };
    Floats buff1{ buffSize };

    // Compare tracks block by block
    auto s0 = mTrack0->TimeToLongSamples(mT0);
    auto s1 = mTrack0->TimeToLongSamples(mT1);
    const auto channels0 = mTrack0->Channels();
    auto iter = mTrack1->Channels().begin();
    for (const auto pChannel0 : channels0) {
        const auto pChannel1 = *iter++;
        auto position = s0;
        auto length = s1 - s0;
        while (position < s1) {
            // Get a block of data into the buffers
            auto block = limitSampleBufferSize(
                pChannel0->GetBestBlockSize(position), s1 - position
                );
            pChannel0->GetFloats(buff0.get(), position, block);
            pChannel1->GetFloats(buff1.get(), position, block);

            for (decltype(block) buffPos = 0; buffPos < block; ++buffPos) {
                if (CompareSample(buff0[buffPos], buff1[buffPos]) > errorThreshold) {
                    ++errorCount;
                }
            }

            position += block;
            context.Progress(
                (position - s0).as_double()
                / length.as_double()
                );
        }
    }

    // Output the results
    double errorSeconds = mTrack0->LongSamplesToTime(errorCount);
    context.Status(wxString::Format(wxT("%li"), errorCount));
    context.Status(wxString::Format(wxT("%.4f"), errorSeconds));
    context.Status(wxString::Format(wxT("Finished comparison: %li samples (%.3f seconds) exceeded the error threshold of %f."), errorCount,
                                    errorSeconds, errorThreshold));
    return true;
}

namespace {
using namespace MenuRegistry;

// Register menu items

AttachedItem sAttachment{
    // Note that the PLUGIN_SYMBOL must have a space between words,
    // whereas the short-form used here must not.
    // (So if you did write "Compare Audio" for the PLUGIN_SYMBOL name, then
    // you would have to use "CompareAudio" here.)
    Command(wxT("CompareAudio"), XXO("Compare Audio..."),
            CommandDispatch::OnAudacityCommand, AudioIONotBusyFlag()),
    wxT("Optional/Extra/Part2/Scriptables2")
};
}
