/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2018 Audacity Team
   License: wxwidgets

   James Crook

******************************************************************//**

\file SetClipCommand.cpp
\brief Definitions for SetClipCommand

\class SetClipCommand
\brief Command that sets clip information

*//*******************************************************************/

#include "SetClipCommand.h"

#include "CommandContext.h"
#include "CommandDispatch.h"
#include "MenuRegistry.h"
#include "../CommonCommandFlags.h"
#include "LoadCommands.h"
#include "../tracks/playabletrack/wavetrack/ui/WaveformAppearance.h"
#include "WaveTrack.h"
#include "SettingsVisitor.h"
#include "ShuttleGui.h"

const ComponentInterfaceSymbol SetClipCommand::Symbol
{ XO("Set Clip") };

namespace {
BuiltinCommandsModule::Registration< SetClipCommand > reg;
}

SetClipCommand::SetClipCommand()
{
}

enum kColours
{
    kColour0,
    kColour1,
    kColour2,
    kColour3,
    nColours
};

static const EnumValueSymbol kColourStrings[nColours] =
{
    { wxT("Color0"), XO("Color 0") },
    { wxT("Color1"), XO("Color 1") },
    { wxT("Color2"), XO("Color 2") },
    { wxT("Color3"), XO("Color 3") },
};

template<bool Const>
bool SetClipCommand::VisitSettings(SettingsVisitorBase<Const>& S)
{
    S.OptionalY(bHasContainsTime).Define(mContainsTime,   wxT("At"),         0.0, 0.0, 100000.0);
    S.OptionalN(bHasColour).DefineEnum(mColour,         wxT("Color"),      kColour0, kColourStrings, nColours);
    // Allowing a negative start time is not a mistake.
    // It will be used in demonstrating time before zero.
    S.OptionalN(bHasT0).Define(mT0,             wxT("Start"),      0.0, -5.0, 1000000.0);
    return true;
}

bool SetClipCommand::VisitSettings(SettingsVisitor& S)
{ return VisitSettings<false>(S); }

bool SetClipCommand::VisitSettings(ConstSettingsVisitor& S)
{ return VisitSettings<true>(S); }

void SetClipCommand::PopulateOrExchange(ShuttleGui& S)
{
    S.AddSpace(0, 5);

    S.StartMultiColumn(3, wxALIGN_CENTER);
    {
        S.Optional(bHasContainsTime).TieNumericTextBox(XXO("At:"),            mContainsTime);
        S.Optional(bHasColour).TieChoice(XXO("Color:"),         mColour,
                                         Msgids(kColourStrings, nColours));
        S.Optional(bHasT0).TieNumericTextBox(XXO("Start:"),         mT0);
        S.Optional(bHasName).TieTextBox(XXO("Name:"),          mName);
    }
    S.EndMultiColumn();
}

bool SetClipCommand::Apply(const CommandContext& context)
{
    for (const auto track : TrackList::Get(context.project)) {
        if (!track->GetSelected()) {
            continue;
        }

        // if no 'At' is specified, then any clip in any selected track will be set.
        track->TypeSwitch([&](WaveTrack& waveTrack) {
            for (const auto& interval : waveTrack.Intervals()) {
                if (!bHasContainsTime
                    || (interval->Start() <= mContainsTime
                        && interval->End() >= mContainsTime)) {
                    // Inside this IF is where we actually apply the command
                    if (bHasColour) {
                        for (const auto channel : interval->Channels()) {
                            WaveColorAttachment::Get(*channel).SetColorIndex(mColour);
                        }
                    }
                    // No validation of overlap yet.  We assume the user is sensible!
                    if (bHasT0) {
                        interval->SetPlayStartTime(mT0);
                    }
                    // \todo Use SetClip to move a clip between tracks too.
                    if (bHasName) {
                        interval->SetName(mName);
                    }
                }
            }
        });
    }
    return true;
}

namespace {
using namespace MenuRegistry;

// Register menu items

AttachedItem sAttachment1{
    // Note that the PLUGIN_SYMBOL must have a space between words,
    // whereas the short-form used here must not.
    // (So if you did write "Compare Audio" for the PLUGIN_SYMBOL name, then
    // you would have to use "CompareAudio" here.)
    Command(wxT("SetClip"), XXO("Set Clip..."),
            CommandDispatch::OnAudacityCommand, AudioIONotBusyFlag()),
    wxT("Optional/Extra/Part2/Scriptables1")
};
}
