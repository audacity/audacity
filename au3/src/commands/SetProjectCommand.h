/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2018 Audacity Team
   License: wxwidgets

   Dan Horgan
   James Crook

******************************************************************//**

\file SetProjectCommand.h
\brief Declarations of SetProjectCommand and SetProjectCommandType classes

*//*******************************************************************/

#ifndef __SET_PROJECT_COMMAND__
#define __SET_PROJECT_COMMAND__

#include "Command.h"
#include "CommandType.h"

class SetProjectCommand : public AudacityCommand
{
public:
    static const ComponentInterfaceSymbol Symbol;

    SetProjectCommand();
    // ComponentInterface overrides
    ComponentInterfaceSymbol GetSymbol() const override { return Symbol; }
    TranslatableString GetDescription() const override { return XO("Sets various values for a project."); }
    template<bool Const> bool VisitSettings(SettingsVisitorBase<Const>& S);
    bool VisitSettings(SettingsVisitor& S) override;
    bool VisitSettings(ConstSettingsVisitor& S) override;
    void PopulateOrExchange(ShuttleGui& S) override;

    // AudacityCommand overrides
    ManualPageID ManualPage() override { return L"Extra_Menu:_Scriptables_I#set_project"; }

    bool Apply(const CommandContext& context) override;

public:

    wxString mName;
    int mPosX;
    int mPosY;
    int mWidth;
    int mHeight;
    double mRate;

// For tracking optional parameters.
    bool bHasName;
    bool bHasSizing;
    bool bHasRate;
};

#endif /* End of include guard: __SETTRACKINFOCOMMAND__ */
