/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2009 Audacity Team
   License: wxwidgets

   Dan Horgan
   Marty Goddard
******************************************************************//**

\file GetTrackInfoCommand.h
\brief Declarations of GetTrackInfoCommand and GetTrackInfoCommandType classes

*//*******************************************************************/

#ifndef __GETTRACKINFOCOMMAND__
#define __GETTRACKINFOCOMMAND__

#include "Command.h"
#include "CommandType.h"

class GetTrackInfoCommand final : public AudacityCommand
{
public:
    static const ComponentInterfaceSymbol Symbol;

    GetTrackInfoCommand();
    // ComponentInterface overrides
    ComponentInterfaceSymbol GetSymbol() const override { return Symbol; }
    TranslatableString GetDescription() const override { return XO("Gets track values as JSON."); }
    template<bool Const> bool VisitSettings(SettingsVisitorBase<Const>& S);
    bool VisitSettings(SettingsVisitor& S) override;
    bool VisitSettings(ConstSettingsVisitor& S) override;
    void PopulateOrExchange(ShuttleGui& S) override;

    // AudacityCommand overrides
    ManualPageID ManualPage() override { return L"Extra_Menu:_Tools#get_track_info"; }

    bool Apply(const CommandContext& context) override;
public:
    int mInfoType;
};

#endif /* End of include guard: __GETTRACKINFOCOMMAND__ */
