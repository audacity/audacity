/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2018 Audacity Team
   License: wxwidgets

   Dan Horgan
   James Crook

******************************************************************//**

\file SetTrackCommand.cpp
\brief Definitions for SetTrackCommand built up from
SetTrackBase, SetTrackStatusCommand, SetTrackAudioCommand and
SetTrackVisualsCommand

\class SetTrackBase
\brief Base class for the various track modifying command classes, that
loops over selected tracks. Subclasses override ApplyInner() to change
one track.

\class SetTrackStatusCommand
\brief A SetTrackBase that sets name, selected and focus.

\class SetTrackAudioCommand
\brief A SetTrackBase that sets pan, volume, mute and solo.

\class SetTrackVisualsCommand
\brief A SetTrackBase that sets appearance of a track.

\class SetTrackCommand
\brief A SetTrackBase that combines SetTrackStatusCommand,
SetTrackAudioConmmand and SetTrackVisualsCommand.

*//*******************************************************************/

#include "SetTrackInfoCommand.h"

#include "CommandDispatch.h"
#include "MenuRegistry.h"
#include "../CommonCommandFlags.h"
#include "LoadCommands.h"
#include "Project.h"
#include "TrackFocus.h"
#include "../TrackPanel.h"
#include "tracks/playabletrack/wavetrack/ui/WaveformAppearance.h"
#include "WaveformSettings.h"
#include "WaveTrack.h"
#include "SpectrogramSettings.h"
#include "SettingsVisitor.h"
#include "ShuttleGui.h"
#include "../tracks/playabletrack/wavetrack/ui/WaveChannelView.h"
#include "WaveChannelViewConstants.h"
#include "../tracks/playabletrack/wavetrack/ui/WaveformView.h"
#include "CommandContext.h"
#include "prefs/WaveformScale.h"

bool SetTrackBase::Apply(const CommandContext& context)
{
    auto& tracks = TrackList::Get(context.project);
    for (auto t : tracks) {
        if (t->GetSelected()) {
            ApplyInner(context, *t);
        }
    }
    return true;
}

const ComponentInterfaceSymbol SetTrackStatusCommand::Symbol
{ XO("Set Track Status") };

namespace {
BuiltinCommandsModule::Registration< SetTrackStatusCommand > reg;
}

template<bool Const>
bool SetTrackStatusCommand::VisitSettings(SettingsVisitorBase<Const>& S)
{
    S.OptionalN(bHasTrackName).Define(mTrackName,      wxT("Name"),       _("Unnamed"));
    // There is also a select command.  This is an alternative.
    S.OptionalN(bHasSelected).Define(bSelected,       wxT("Selected"),   false);
    S.OptionalN(bHasFocused).Define(bFocused,        wxT("Focused"),    false);
    return true;
}

bool SetTrackStatusCommand::VisitSettings(SettingsVisitor& S)
{ return VisitSettings<false>(S); }

bool SetTrackStatusCommand::VisitSettings(ConstSettingsVisitor& S)
{ return VisitSettings<true>(S); }

void SetTrackStatusCommand::PopulateOrExchange(ShuttleGui& S)
{
    S.StartMultiColumn(3, wxEXPAND);
    {
        S.SetStretchyCol(2);
        S.Optional(bHasTrackName).TieTextBox(XXO("Name:"),          mTrackName);
    }
    S.EndMultiColumn();
    S.StartMultiColumn(2, wxEXPAND);
    {
        S.SetStretchyCol(1);
        S.Optional(bHasSelected).TieCheckBox(XXO("Selected"),           bSelected);
        S.Optional(bHasFocused).TieCheckBox(XXO("Focused"),            bFocused);
    }
    S.EndMultiColumn();
}

bool SetTrackStatusCommand::ApplyInner(const CommandContext& context, Track& t)
{
    //auto wt = dynamic_cast<WaveTrack *>(t);
    //auto pt = dynamic_cast<PlayableTrack *>(t);

    if (bHasTrackName) {
        t.SetName(mTrackName);
    }

    if (bHasSelected) {
        t.SetSelected(bSelected);
    }

    if (bHasFocused) {
        auto& trackFocus = TrackFocus::Get(context.project);
        if (bFocused) {
            trackFocus.Set(&t);
        } else if (&t == trackFocus.Get()) {
            trackFocus.Set(nullptr);
        }
    }
    return true;
}

const ComponentInterfaceSymbol SetTrackAudioCommand::Symbol
{ XO("Set Track Audio") };

namespace {
BuiltinCommandsModule::Registration< SetTrackAudioCommand > reg2;
}

template<bool Const>
bool SetTrackAudioCommand::VisitSettings(SettingsVisitorBase<Const>& S)
{
    S.OptionalN(bHasMute).Define(bMute,           wxT("Mute"),       false);
    S.OptionalN(bHasSolo).Define(bSolo,           wxT("Solo"),       false);

    S.OptionalN(bHasVolume).Define(mVolume,         wxT("Volume"),     0.0,  -36.0, 36.0);
    S.OptionalN(bHasPan).Define(mPan,            wxT("Pan"),        0.0, -100.0, 100.0);
    return true;
}

bool SetTrackAudioCommand::VisitSettings(SettingsVisitor& S)
{ return VisitSettings<false>(S); }

bool SetTrackAudioCommand::VisitSettings(ConstSettingsVisitor& S)
{ return VisitSettings<true>(S); }

void SetTrackAudioCommand::PopulateOrExchange(ShuttleGui& S)
{
    S.StartMultiColumn(2, wxEXPAND);
    {
        S.SetStretchyCol(1);
        S.Optional(bHasMute).TieCheckBox(XXO("Mute"),               bMute);
        S.Optional(bHasSolo).TieCheckBox(XXO("Solo"),               bSolo);
    }
    S.EndMultiColumn();
    S.StartMultiColumn(3, wxEXPAND);
    {
        S.SetStretchyCol(2);
        S.Optional(bHasVolume).TieSlider(XXO("Volume:"),        mVolume, 36.0, -36.0);
        S.Optional(bHasPan).TieSlider(XXO("Pan:"),           mPan,  100.0, -100.0);
    }
    S.EndMultiColumn();
}

bool SetTrackAudioCommand::ApplyInner(const CommandContext& context, Track& t)
{
    static_cast<void>(context);
    auto wt = dynamic_cast<WaveTrack*>(&t);
    auto pt = dynamic_cast<PlayableTrack*>(&t);

    if (wt && bHasVolume) {
        wt->SetVolume(DB_TO_LINEAR(mVolume));
    }
    if (wt && bHasPan) {
        wt->SetPan(mPan / 100.0);
    }

    if (pt && bHasSolo) {
        pt->SetSolo(bSolo);
    }
    if (pt && bHasMute) {
        pt->SetMute(bMute);
    }
    return true;
}

const ComponentInterfaceSymbol SetTrackVisualsCommand::Symbol
{ XO("Set Track Visuals") };

namespace {
BuiltinCommandsModule::Registration< SetTrackVisualsCommand > reg3;
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

enum kScaleTypes
{
    kLinearAmp,
    kLogarithmicDb,
    kLinearDb,
    nScaleTypes
};

static const EnumValueSymbol kScaleTypeStrings[nScaleTypes] =
{
    /* i18n-hint: abbreviates amplitude */
    { wxT("Linear"), XO("Linear (amp)") },
    /* i18n-hint: abbreviates decibels */
    { wxT("dB"), XO("Logarithmic (dB)") },
    /* i18n-hint: abbreviates decibels */
    { wxT("LinearDB"), XO("Linear (dB)") }
};

enum kZoomTypes
{
    kReset,
    kTimes2,
    kHalfWave,
    nZoomTypes
};

static const EnumValueSymbol kZoomTypeStrings[nZoomTypes] =
{
    { XO("Reset") },
    { wxT("Times2"), XO("Times 2") },
    { XO("HalfWave") },
};

static EnumValueSymbols DiscoverSubViewTypes()
{
    const auto& types = WaveChannelSubViewType::All();
    auto result = transform_container< EnumValueSymbols >(
        types, std::mem_fn(&WaveChannelSubView::Type::name));
    result.push_back(WaveChannelViewConstants::MultiViewSymbol);
    return result;
}

template<bool Const>
bool SetTrackVisualsCommand::VisitSettings(SettingsVisitorBase<Const>& S)
{
    S.OptionalN(bHasHeight).Define(mHeight,         wxT("Height"),     120, 44, 2000);

    {
        auto symbols = DiscoverSubViewTypes();
        S.OptionalN(bHasDisplayType).DefineEnum(mDisplayType,    wxT("Display"),    0, symbols.data(), symbols.size());
    }

    S.OptionalN(bHasScaleType).DefineEnum(mScaleType,      wxT("Scale"),      kLinearAmp,   kScaleTypeStrings, nScaleTypes);
    S.OptionalN(bHasColour).DefineEnum(mColour,         wxT("Color"),      kColour0,  kColourStrings, nColours);
    S.OptionalN(bHasVZoom).DefineEnum(mVZoom,          wxT("VZoom"),      kReset,    kZoomTypeStrings, nZoomTypes);
    S.OptionalN(bHasVZoomTop).Define(mVZoomTop,       wxT("VZoomHigh"),  1.0,  -2.0,  2.0);
    S.OptionalN(bHasVZoomBottom).Define(mVZoomBottom,    wxT("VZoomLow"),   -1.0, -2.0,  2.0);

    S.OptionalN(bHasUseSpecPrefs).Define(bUseSpecPrefs,   wxT("SpecPrefs"),  false);
    S.OptionalN(bHasSpectralSelect).Define(bSpectralSelect, wxT("SpectralSel"), true);

    auto schemes = SpectrogramSettings::GetColorSchemeNames();
    S.OptionalN(bHasSpecColorScheme).DefineEnum(mSpecColorScheme, wxT("SpecColor"),  SpectrogramSettings::csColorNew,
                                                schemes.data(), schemes.size());

    return true;
}

bool SetTrackVisualsCommand::VisitSettings(SettingsVisitor& S)
{ return VisitSettings<false>(S); }

bool SetTrackVisualsCommand::VisitSettings(ConstSettingsVisitor& S)
{ return VisitSettings<true>(S); }

void SetTrackVisualsCommand::PopulateOrExchange(ShuttleGui& S)
{
    S.StartMultiColumn(3, wxEXPAND);
    {
        S.SetStretchyCol(2);
        S.Optional(bHasHeight).TieNumericTextBox(XXO("Height:"),        mHeight);
        S.Optional(bHasColour).TieChoice(XXO("Color:"),         mColour,
                                         Msgids(kColourStrings, nColours));

        {
            auto symbols = DiscoverSubViewTypes();
            auto typeNames = transform_container<TranslatableStrings>(
                symbols, std::mem_fn(&EnumValueSymbol::Stripped));
            S.Optional(bHasDisplayType).TieChoice(XXO("Display:"),       mDisplayType,
                                                  typeNames);
        }

        S.Optional(bHasScaleType).TieChoice(XXO("Scale:"),         mScaleType,
                                            Msgids(kScaleTypeStrings, nScaleTypes));
        S.Optional(bHasVZoom).TieChoice(XXO("VZoom:"),         mVZoom,
                                        Msgids(kZoomTypeStrings, nZoomTypes));
        S.Optional(bHasVZoomTop).TieTextBox(XXO("VZoom Top:"),     mVZoomTop);
        S.Optional(bHasVZoomBottom).TieTextBox(XXO("VZoom Bottom:"),  mVZoomBottom);
    }
    S.EndMultiColumn();
    S.StartMultiColumn(2, wxEXPAND);
    {
        S.SetStretchyCol(1);
        S.Optional(bHasUseSpecPrefs).TieCheckBox(XXO("Use Spectral Prefs"), bUseSpecPrefs);
        S.Optional(bHasSpectralSelect).TieCheckBox(XXO("Spectral Select"),    bSpectralSelect);
    }
    S.EndMultiColumn();
    S.StartMultiColumn(3, wxEXPAND);
    {
        S.SetStretchyCol(2);
        auto schemes = SpectrogramSettings::GetColorSchemeNames();
        S.Optional(bHasSpecColorScheme).TieChoice(XC("Sche&me:", "spectrum prefs"), mSpecColorScheme,
                                                  Msgids(schemes.data(), schemes.size()));
    }
    S.EndMultiColumn();
}

bool SetTrackVisualsCommand::ApplyInner(
    const CommandContext& context, Track& t)
{
    static_cast<void>(context);
    const auto wt = dynamic_cast<WaveTrack*>(&t);
    if (!wt) {
        return true;
    }
    auto& wc = **wt->Channels().begin();
    //auto pt = dynamic_cast<PlayableTrack *>(t);
    static const double ZOOMLIMIT = 0.001f;

    if (bHasColour) {
        WaveformAppearance::Get(*wt).SetColorIndex(mColour);
    }

    if (bHasHeight) {
        for (auto pChannel : t.Channels<WaveChannel>()) {
            ChannelView::Get(*pChannel).SetExpandedHeight(mHeight);
        }
    }

    if (bHasDisplayType) {
        auto& view = WaveChannelView::Get(wc);
        auto& all = WaveChannelSubViewType::All();
        if (mDisplayType < all.size()) {
            view.SetDisplay(all[ mDisplayType ].id);
        } else {
            view.SetMultiView(true);
            view.SetDisplay(WaveChannelSubViewType::Default(), false);
        }
    }
    if (bHasScaleType) {
        auto& scaleType = WaveformSettings::Get(*wt).scaleType;
        switch (mScaleType) {
        default:
        case kLinearAmp: scaleType = WaveformSettings::stLinearAmp;
        case kLogarithmicDb: scaleType = WaveformSettings::stLogarithmicDb;
        case kLinearDb: scaleType = WaveformSettings::stLinearDb;
        }
    }

    if (bHasVZoom) {
        auto& cache = WaveformScale::Get(*wt);
        switch (mVZoom) {
        default:
        case kReset: cache.SetDisplayBounds(-1, 1);
            break;
        case kTimes2: cache.SetDisplayBounds(-2, 2);
            break;
        case kHalfWave: cache.SetDisplayBounds(0, 1);
            break;
        }
    }

    if ((bHasVZoomTop || bHasVZoomBottom) && !bHasVZoom) {
        float vzmin, vzmax;
        auto& cache = WaveformScale::Get(*wt);
        cache.GetDisplayBounds(vzmin, vzmax);

        if (!bHasVZoomTop) {
            mVZoomTop = vzmax;
        }
        if (!bHasVZoomBottom) {
            mVZoomBottom = vzmin;
        }

        mVZoomTop = std::clamp(mVZoomTop, -2.0, 2.0);
        mVZoomBottom = std::clamp(mVZoomBottom, -2.0, 2.0);

        if (mVZoomBottom > mVZoomTop) {
            std::swap(mVZoomTop, mVZoomBottom);
        }
        if (mVZoomTop - mVZoomBottom < ZOOMLIMIT) {
            double c = (mVZoomBottom + mVZoomTop) / 2;
            mVZoomBottom = c - ZOOMLIMIT / 2.0;
            mVZoomTop = c + ZOOMLIMIT / 2.0;
        }
        cache.SetDisplayBounds(mVZoomBottom, mVZoomTop);
        auto& tp = TrackPanel::Get(context.project);
        tp.UpdateVRulers();
    }

    if (bHasUseSpecPrefs) {
        if (bUseSpecPrefs) {
            // reset it, and next we will be getting the defaults.
            SpectrogramSettings::Reset(wc);
        } else {
            SpectrogramSettings::Own(wc);
        }
    }
    auto& settings = SpectrogramSettings::Get(*wt);
    if (wt && bHasSpectralSelect) {
        settings.spectralSelection = bSpectralSelect;
    }
    if (wt && bHasSpecColorScheme) {
        settings.colorScheme
            =static_cast<SpectrogramSettings::ColorScheme>(mSpecColorScheme);
    }

    return true;
}

const ComponentInterfaceSymbol SetTrackCommand::Symbol
{ XO("Set Track") };

namespace {
BuiltinCommandsModule::Registration< SetTrackCommand > reg4;
}

bool SetTrackCommand::VisitSettings(SettingsVisitor& S)
{ return VisitSettings<false>(S); }
bool SetTrackCommand::VisitSettings(ConstSettingsVisitor& S)
{ return VisitSettings<true>(S); }

namespace {
using namespace MenuRegistry;

// Register menu items

AttachedItem sAttachment1{
    Items(wxT(""),
          // Note that the PLUGIN_SYMBOL must have a space between words,
          // whereas the short-form used here must not.
          // (So if you did write "Compare Audio" for the PLUGIN_SYMBOL name, then
          // you would have to use "CompareAudio" here.)
          Command(wxT("SetTrackStatus"), XXO("Set Track Status..."),
                  CommandDispatch::OnAudacityCommand, AudioIONotBusyFlag()),
          Command(wxT("SetTrackAudio"), XXO("Set Track Audio..."),
                  CommandDispatch::OnAudacityCommand, AudioIONotBusyFlag()),
          Command(wxT("SetTrackVisuals"), XXO("Set Track Visuals..."),
                  CommandDispatch::OnAudacityCommand, AudioIONotBusyFlag())
          ),
    wxT("Optional/Extra/Part2/Scriptables1")
};

AttachedItem sAttachment2{
    // Note that the PLUGIN_SYMBOL must have a space between words,
    // whereas the short-form used here must not.
    // (So if you did write "Compare Audio" for the PLUGIN_SYMBOL name, then
    // you would have to use "CompareAudio" here.)
    Command(wxT("SetTrack"), XXO("Set Track..."),
            CommandDispatch::OnAudacityCommand, AudioIONotBusyFlag()),
    wxT("Optional/Extra/Part2/Scriptables2")
};
}
