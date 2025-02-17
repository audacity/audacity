/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2018 Audacity Team
   License: wxwidgets

   James Crook

******************************************************************//**

\file SetClipCommand.h
\brief Declarations of SetClipCommand and SetClipCommandType classes

*//*******************************************************************/

#ifndef __SET_CLIP_COMMAND__
#define __SET_CLIP_COMMAND__

#include "SetTrackInfoCommand.h"

class SetClipCommand : public AudacityCommand
{
public:
    static const ComponentInterfaceSymbol Symbol;

    SetClipCommand();
    // ComponentInterface overrides
    ComponentInterfaceSymbol GetSymbol() const override { return Symbol; }
    TranslatableString GetDescription() const override { return XO("Sets various values for a clip."); }
    template<bool Const> bool VisitSettings(SettingsVisitorBase<Const>& S);
    bool VisitSettings(SettingsVisitor& S) override;
    bool VisitSettings(ConstSettingsVisitor& S) override;
    void PopulateOrExchange(ShuttleGui& S) override;

    // AudacityCommand overrides
    ManualPageID ManualPage() override { return L"Extra_Menu:_Scriptables_I#set_clip"; }
    bool Apply(const CommandContext& context) override;

public:
    double mContainsTime;
    int mColour;
    double mT0;
    wxString mName;

// For tracking optional parameters.
    bool bHasContainsTime;
    bool bHasColour;
    bool bHasT0;
    bool bHasName;
};

#endif /* End of include guard: __SETTRACKINFOCOMMAND__ */
