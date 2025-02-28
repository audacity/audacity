/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2018 Audacity Team
   License: wxwidgets

   James Crook

******************************************************************//**

\file SetEnvelopeCommand.cpp
\brief Definitions for SetEnvelopeCommand

\class SetEnvelopeCommand
\brief Command that sets envelope information

*//*******************************************************************/

#include "SetEnvelopeCommand.h"

#include "CommandContext.h"
#include "CommandDispatch.h"
#include "MenuRegistry.h"
#include "../CommonCommandFlags.h"
#include "LoadCommands.h"
#include "ProjectHistory.h"
#include "UndoManager.h"
#include "WaveClip.h"
#include "WaveTrack.h"
#include "Envelope.h"
#include "SettingsVisitor.h"
#include "ShuttleGui.h"

const ComponentInterfaceSymbol SetEnvelopeCommand::Symbol
{ XO("Set Envelope") };

namespace {
BuiltinCommandsModule::Registration< SetEnvelopeCommand > reg;
}

SetEnvelopeCommand::SetEnvelopeCommand()
{
}

template<bool Const>
bool SetEnvelopeCommand::VisitSettings(SettingsVisitorBase<Const>& S)
{
    S.OptionalY(bHasT).Define(mT,              wxT("Time"),     0.0, 0.0, 100000.0);
    S.OptionalY(bHasV).Define(mV,              wxT("Value"),    1.0, 0.0, 2.0);
    S.OptionalN(bHasDelete).Define(mbDelete,        wxT("Delete"),   false);
    return true;
}

bool SetEnvelopeCommand::VisitSettings(SettingsVisitor& S)
{ return VisitSettings<false>(S); }

bool SetEnvelopeCommand::VisitSettings(ConstSettingsVisitor& S)
{ return VisitSettings<true>(S); }

void SetEnvelopeCommand::PopulateOrExchange(ShuttleGui& S)
{
    S.AddSpace(0, 5);

    S.StartMultiColumn(3, wxALIGN_CENTER);
    {
        S.Optional(bHasT).TieNumericTextBox(XXO("Time:"),          mT);
        S.Optional(bHasV).TieNumericTextBox(XXO("Value:"),         mV);
        S.Optional(bHasDelete).TieCheckBox(XXO("Delete"),         mbDelete);
    }
    S.EndMultiColumn();
}

bool SetEnvelopeCommand::ApplyInner(const CommandContext& context, Track& t)
{
    // if no time is specified, then
    //   - delete deletes any envelope in selected tracks.
    //   - value is not set for any clip
    t.TypeSwitch([&](WaveTrack& waveTrack) {
        for (const auto pClip : waveTrack.SortedIntervalArray()) {
            bool bFound
                =!bHasT || (
                      (pClip->GetPlayStartTime() <= mT)
                      && (pClip->GetPlayEndTime() >= mT)
                      );
            if (bFound) {
                // Inside this IF is where we actually apply the command
                auto& env = pClip->GetEnvelope();
                bool didSomething = false;
                if (bHasDelete && mbDelete) {
                    env.Clear(), didSomething = true;
                }
                if (bHasT && bHasV) {
                    env.InsertOrReplace(mT, env.ClampValue(mV)),
                    didSomething = true;
                }

                if (didSomething) {
                    // Consolidate, because this ApplyInner() function may be
                    // visited multiple times in one command invocation
                    ProjectHistory::Get(context.project).PushState(
                        XO("Edited Envelope"), XO("Envelope"),
                        UndoPush::CONSOLIDATE);
                }
            }
        }
    });

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
    Command(wxT("SetEnvelope"), XXO("Set Envelope..."),
            CommandDispatch::OnAudacityCommand, AudioIONotBusyFlag()),
    wxT("Optional/Extra/Part2/Scriptables1")
};
}
