/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2009 Audacity Team
   License: wxwidgets

   Dan Horgan
   James Crook

******************************************************************//**

\file SelectCommand.cpp
\brief Definitions for SelectCommand classes

\class SelectTimeCommand
\brief Command for changing the time selection

\class SelectFrequenciesCommand
\brief Command for changing the frequency selection

\class SelectTracksCommand
\brief Command for changing the selection of tracks

\class SelectCommand
\brief Command for changing time, frequency and track selection. This
class is a little baroque, as it uses the SelectTimeCommand,
SelectFrequenciesCommand and SelectTracksCommand, when it could just
explicitly code all three.

*//*******************************************************************/

#include "SelectCommand.h"

#include <float.h>

#include "CommandDispatch.h"
#include "MenuRegistry.h"
#include "../CommonCommandFlags.h"
#include "EffectOutputTracks.h"
#include "LoadCommands.h"
#include "ProjectSelectionManager.h"
#include "../TrackPanel.h"
#include "SettingsVisitor.h"
#include "ShuttleGui.h"
#include "Track.h"
#include "Effect.h"
#include "ViewInfo.h"
#include "CommandContext.h"
#include "WaveTrack.h"

const ComponentInterfaceSymbol SelectTimeCommand::Symbol
{ XO("Select Time") };

namespace {
BuiltinCommandsModule::Registration< SelectTimeCommand > reg;
}

// Relative to project and relative to selection cover MOST options, since you can already
// set a selection to a clip.
const int nRelativeTos =6;
static const EnumValueSymbol kRelativeTo[nRelativeTos] =
{
    { wxT("ProjectStart"), XO("Project Start") },
    { XO("Project") },
    { wxT("ProjectEnd"), XO("Project End") },
    { wxT("SelectionStart"), XO("Selection Start") },
    { XO("Selection") },
    { wxT("SelectionEnd"), XO("Selection End") }
};

template<bool Const>
bool SelectTimeCommand::VisitSettings(SettingsVisitorBase<Const>& S)
{
    // Allow selection down to -ve 100seconds.
    // Typically used to expand/contract selections by a small amount.
    S.OptionalY(bHasT0).Define(mT0, wxT("Start"), 0.0, -100.0, (double)FLT_MAX);
    S.OptionalY(bHasT1).Define(mT1, wxT("End"), 0.0, -100.0, (double)FLT_MAX);
    S.OptionalN(bHasRelativeSpec).DefineEnum(mRelativeTo,   wxT("RelativeTo"), 0, kRelativeTo, nRelativeTos);
    return true;
}

bool SelectTimeCommand::VisitSettings(SettingsVisitor& S)
{ return VisitSettings<false>(S); }

bool SelectTimeCommand::VisitSettings(ConstSettingsVisitor& S)
{ return VisitSettings<true>(S); }

void SelectTimeCommand::PopulateOrExchange(ShuttleGui& S)
{
    S.AddSpace(0, 5);

    S.StartMultiColumn(3, wxEXPAND);
    {
        S.SetStretchyCol(2);
        S.Optional(bHasT0).TieTextBox(XXO("Start Time:"), mT0);
        S.Optional(bHasT1).TieTextBox(XXO("End Time:"),   mT1);
        // Chooses what time is relative to.
        S.Optional(bHasRelativeSpec).TieChoice(
            XXO("Relative To:"),
            mRelativeTo, Msgids(kRelativeTo, nRelativeTos));
    }
    S.EndMultiColumn();
}

bool SelectTimeCommand::Apply(const CommandContext& context)
{
    // Many commands need focus on track panel.
    // No harm in setting it with a scripted select.
    TrackPanel::Get(context.project).SetFocus();
    if (!bHasT0 && !bHasT1) {
        return true;
    }

    // Defaults if no value...
    if (!bHasT0) {
        mT0 = 0.0;
    }
    if (!bHasT1) {
        mT1 = 0.0;
    }
    if (!bHasRelativeSpec) {
        mRelativeTo = 0;
    }

    AudacityProject* p = &context.project;
    double end = TrackList::Get(*p).GetEndTime();
    double t0;
    double t1;

    auto& selectedRegion = ViewInfo::Get(*p).selectedRegion;
    switch (bHasRelativeSpec ? mRelativeTo : 0) {
    default:
    case 0: //project start
        t0 = mT0;
        t1 = mT1;
        break;
    case 1: //project
        t0 = mT0;
        t1 = end + mT1;
        break;
    case 2: //project end;
        t0 = end - mT0;
        t1 = end - mT1;
        break;
    case 3: //selection start
        t0 = mT0 + selectedRegion.t0();
        t1 = mT1 + selectedRegion.t0();
        break;
    case 4: //selection
        t0 = mT0 + selectedRegion.t0();
        t1 = mT1 + selectedRegion.t1();
        break;
    case 5: //selection end
        t0 =  selectedRegion.t1() - mT0;
        t1 =  selectedRegion.t1() - mT1;
        break;
    }

    selectedRegion.setTimes(t0, t1);
    return true;
}

const ComponentInterfaceSymbol SelectFrequenciesCommand::Symbol
{ XO("Select Frequencies") };

namespace {
BuiltinCommandsModule::Registration< SelectFrequenciesCommand > reg2;
}

template<bool Const>
bool SelectFrequenciesCommand::VisitSettings(SettingsVisitorBase<Const>& S)
{
    S.OptionalN(bHasTop).Define(mTop,    wxT("High"), 0.0, 0.0, (double)FLT_MAX);
    S.OptionalN(bHasBottom).Define(mBottom, wxT("Low"),  0.0, 0.0, (double)FLT_MAX);
    return true;
}

bool SelectFrequenciesCommand::VisitSettings(SettingsVisitor& S)
{ return VisitSettings<false>(S); }

bool SelectFrequenciesCommand::VisitSettings(ConstSettingsVisitor& S)
{ return VisitSettings<true>(S); }

void SelectFrequenciesCommand::PopulateOrExchange(ShuttleGui& S)
{
    S.AddSpace(0, 5);

    S.StartMultiColumn(3, wxEXPAND);
    {
        S.SetStretchyCol(2);
        S.Optional(bHasTop).TieTextBox(XXO("High:"), mTop);
        S.Optional(bHasBottom).TieTextBox(XXO("Low:"),  mBottom);
    }
    S.EndMultiColumn();
}

bool SelectFrequenciesCommand::Apply(const CommandContext& context)
{
    if (!bHasBottom && !bHasTop) {
        return true;
    }

    // Defaults if no value...
    if (!bHasTop) {
        mTop = 0.0;
    }
    if (!bHasBottom) {
        mBottom = 0.0;
    }

    ProjectSelectionManager::Get(context.project).ModifySpectralSelection(
        WaveTrack::ProjectNyquistFrequency(context.project),
        mBottom, mTop, false);// false for not done.
    return true;
}

const ComponentInterfaceSymbol SelectTracksCommand::Symbol
{ XO("Select Tracks") };

namespace {
BuiltinCommandsModule::Registration< SelectTracksCommand > reg3;
}

const int nModes =3;
static const EnumValueSymbol kModes[nModes] =
{
    // These are acceptable dual purpose internal/visible names

    /* i18n-hint verb, imperative */
    { XO("Set") },
    { XO("Add") },
    { XO("Remove") },
};

template<bool Const>
bool SelectTracksCommand::VisitSettings(SettingsVisitorBase<Const>& S)
{
    S.OptionalN(bHasFirstTrack).Define(mFirstTrack, wxT("Track"), 0.0, 0.0, 100.0);
    S.OptionalN(bHasNumTracks).Define(mNumTracks,  wxT("TrackCount"),  1.0, 0.0, 100.0);
    S.OptionalY(bHasMode).DefineEnum(mMode,   wxT("Mode"), 0, kModes, nModes);

    return true;
}

bool SelectTracksCommand::VisitSettings(SettingsVisitor& S)
{ return VisitSettings<false>(S); }

bool SelectTracksCommand::VisitSettings(ConstSettingsVisitor& S)
{ return VisitSettings<true>(S); }

void SelectTracksCommand::PopulateOrExchange(ShuttleGui& S)
{
    S.AddSpace(0, 5);

    S.StartMultiColumn(3, wxEXPAND);
    {
        S.SetStretchyCol(2);
        S.Optional(bHasFirstTrack).TieTextBox(XXO("First Track:"), mFirstTrack);
        S.Optional(bHasNumTracks).TieTextBox(XXO("Track Count:"), mNumTracks);
    }
    S.EndMultiColumn();
    S.StartMultiColumn(2, wxALIGN_CENTER);
    {
        // Always used, so no check box.
        S.TieChoice(XXO("Mode:"), mMode, Msgids(kModes, nModes));
    }
    S.EndMultiColumn();
}

bool SelectTracksCommand::Apply(const CommandContext& context)
{
    // Count selection as a do-nothing effect.
    // Used to invalidate cached selection and tracks.
    EffectOutputTracks::IncEffectCounter();
    int index = 0;
    auto& tracks = TrackList::Get(context.project);

    // Defaults if no value...
    if (!bHasNumTracks) {
        mNumTracks = 1.0;
    }
    if (!bHasFirstTrack) {
        mFirstTrack = 0.0;
    }

    // Multiple channels count as fractions of a track.
    double last = mFirstTrack + mNumTracks;
    double first = mFirstTrack;

    for (auto t : tracks) {
        const bool sel = first <= index && index < last;
        if (mMode == 0) { // Set
            t->SetSelected(sel);
        } else if (mMode == 1 && sel) { // Add
            t->SetSelected(sel);
        } else if (mMode == 2 && sel) { // Remove
            t->SetSelected(!sel);
        }
        ++index;
    }
    return true;
}

const ComponentInterfaceSymbol SelectCommand::Symbol
{ XO("Select") };

namespace {
BuiltinCommandsModule::Registration< SelectCommand > reg4;
}

template<bool Const>
bool SelectCommand::VisitSettings(SettingsVisitorBase<Const>& S)
{
    return
        mSelTime.VisitSettings(S)
        && mSelFreq.VisitSettings(S)
        && mSelTracks.VisitSettings(S);
}

bool SelectCommand::VisitSettings(SettingsVisitor& S)
{
    return VisitSettings<false>(S);
}

bool SelectCommand::VisitSettings(ConstSettingsVisitor& S)
{
    return VisitSettings<true>(S);
}

namespace {
using namespace MenuRegistry;

// Register menu items

AttachedItem sAttachment1{
    Items(wxT(""),
          // Note that the PLUGIN_SYMBOL must have a space between words,
          // whereas the short-form used here must not.
          // (So if you did write "Compare Audio" for the PLUGIN_SYMBOL name, then
          // you would have to use "CompareAudio" here.)
          Command(wxT("SelectTime"), XXO("Select Time..."),
                  CommandDispatch::OnAudacityCommand, AudioIONotBusyFlag()),
          Command(wxT("SelectFrequencies"), XXO("Select Frequencies..."),
                  CommandDispatch::OnAudacityCommand, AudioIONotBusyFlag()),
          Command(wxT("SelectTracks"), XXO("Select Tracks..."),
                  CommandDispatch::OnAudacityCommand, AudioIONotBusyFlag())
          ),
    wxT("Optional/Extra/Part2/Scriptables1")
};

AttachedItem sAttachment2{
    // Note that the PLUGIN_SYMBOL must have a space between words,
    // whereas the short-form used here must not.
    // (So if you did write "Compare Audio" for the PLUGIN_SYMBOL name, then
    // you would have to use "CompareAudio" here.)
    Command(wxT("Select"), XXO("Select..."),
            CommandDispatch::OnAudacityCommand, AudioIONotBusyFlag()),
    wxT("Optional/Extra/Part2/Scriptables2")
};
}
