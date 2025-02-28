/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2018 Audacity Team
   License: wxwidgets

   Dan Horgan
   James Crook

******************************************************************//**

\file SetTrackCommand.h
\brief Declarations of SetTrackCommand and SetTrackCommandType classes

*//*******************************************************************/

#ifndef __SET_TRACK_COMMAND__
#define __SET_TRACK_COMMAND__

#include "Command.h"
#include "CommandType.h"

class Track;

class SetTrackBase : public AudacityCommand
{
public:
    bool Apply(const CommandContext& context) final;
    virtual bool ApplyInner(const CommandContext& context, Track& t) = 0;
};

class SetTrackStatusCommand : public SetTrackBase
{
public:
    static const ComponentInterfaceSymbol Symbol;

    //SetTrackStatusCommand();
    // ComponentInterface overrides
    ComponentInterfaceSymbol GetSymbol() const override { return Symbol; }
    TranslatableString GetDescription() const override { return XO("Sets various values for a track."); }
    template<bool Const> bool VisitSettings(SettingsVisitorBase<Const>& S);
    bool VisitSettings(SettingsVisitor& S) override;
    bool VisitSettings(ConstSettingsVisitor& S) override;
    void PopulateOrExchange(ShuttleGui& S) override;

    // AudacityCommand overrides
    ManualPageID ManualPage() override { return L"Extra_Menu:_Scriptables_I#set_track_status"; }
    bool ApplyInner(const CommandContext& context, Track& t) override;

public:
    wxString mTrackName;
    bool bSelected;
    bool bFocused;

// For tracking optional parameters.
    bool bHasTrackName;
    bool bHasSelected;
    bool bHasFocused;
};

class SetTrackAudioCommand : public SetTrackBase
{
public:
    static const ComponentInterfaceSymbol Symbol;

    //SetTrackAudioCommand();
    // ComponentInterface overrides
    ComponentInterfaceSymbol GetSymbol() const override { return Symbol; }
    TranslatableString GetDescription() const override { return XO("Sets various values for a track."); }
    template<bool Const> bool VisitSettings(SettingsVisitorBase<Const>& S);
    bool VisitSettings(SettingsVisitor& S) override;
    bool VisitSettings(ConstSettingsVisitor& S) override;
    void PopulateOrExchange(ShuttleGui& S) override;

    // AudacityCommand overrides
    ManualPageID ManualPage() override { return L"Extra_Menu:_Scriptables_I#set_track_audio"; }
    bool ApplyInner(const CommandContext& context, Track& t) override;

public:
    double mPan;
    double mVolume;
    bool bSolo;
    bool bMute;

// For tracking optional parameters.
    bool bHasPan;
    bool bHasVolume;
    bool bHasSolo;
    bool bHasMute;
};

class SetTrackVisualsCommand : public SetTrackBase
{
public:
    static const ComponentInterfaceSymbol Symbol;

    //SetTrackVisualsCommand();
    // ComponentInterface overrides
    ComponentInterfaceSymbol GetSymbol() const override { return Symbol; }
    TranslatableString GetDescription() const override { return XO("Sets various values for a track."); }
    template<bool Const> bool VisitSettings(SettingsVisitorBase<Const>& S);
    bool VisitSettings(SettingsVisitor& S) override;
    bool VisitSettings(ConstSettingsVisitor& S) override;
    void PopulateOrExchange(ShuttleGui& S) override;

    // AudacityCommand overrides
    ManualPageID ManualPage() override { return L"Extra_Menu:_Scriptables_I#set_track_visuals"; }
    bool ApplyInner(const CommandContext& context, Track& t) override;

public:
    int mColour;
    int mHeight;
    int mDisplayType;
    int mScaleType;
    int mVZoom;
    double mVZoomTop;
    double mVZoomBottom;

    bool bUseSpecPrefs;
    bool bSpectralSelect;
    int mSpecColorScheme;

// For tracking optional parameters.
    bool bHasColour;
    bool bHasHeight;
    bool bHasDisplayType;
    bool bHasScaleType;
    bool bHasVZoom;
    bool bHasVZoomTop;
    bool bHasVZoomBottom;

    bool bHasUseSpecPrefs;
    bool bHasSpectralSelect;
    bool bHasSpecColorScheme;
};

class SetTrackCommand : public SetTrackBase
{
public:
    static const ComponentInterfaceSymbol Symbol;

    // ComponentInterface overrides
    ComponentInterfaceSymbol GetSymbol() const override { return Symbol; }
    TranslatableString GetDescription() const override { return XO("Sets various values for a track."); }
    // AudacityCommand overrides
    ManualPageID ManualPage() override { return L"Extra_Menu:_Scriptables_II#set_track"; }

public:

    template<bool Const> bool VisitSettings(SettingsVisitorBase<Const>& S)
    {
        return
            mSetStatus.VisitSettings(S)
            && mSetAudio.VisitSettings(S)
            && mSetVisuals.VisitSettings(S);
    }

    bool VisitSettings(SettingsVisitor& S) override;
    bool VisitSettings(ConstSettingsVisitor& S) override;
    void PopulateOrExchange(ShuttleGui& S) override
    {
        mSetStatus.PopulateOrExchange(S);
        mSetAudio.PopulateOrExchange(S);
        mSetVisuals.PopulateOrExchange(S);
    }

    bool ApplyInner(const CommandContext& context, Track& t) override
    {
        return
            mSetStatus.ApplyInner(context, t)
            && mSetAudio.ApplyInner(context, t)
            && mSetVisuals.ApplyInner(context, t);
    }

private:
    SetTrackStatusCommand mSetStatus;
    SetTrackAudioCommand mSetAudio;
    SetTrackVisualsCommand mSetVisuals;
};

#endif /* End of include guard: __SETTRACKINFOCOMMAND__ */
