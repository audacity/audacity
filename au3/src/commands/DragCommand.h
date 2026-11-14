/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2009 Audacity Team
   License: wxwidgets

   James Crook

******************************************************************//**

\file DragCommand.h
\brief Declarations of DragCommand and DragCommandType classes

*//*******************************************************************/

#ifndef __DRAG_COMMAND__
#define __DRAG_COMMAND__

#include "Command.h"
#include "CommandType.h"

class DragCommand : public AudacityCommand
{
public:
    static const ComponentInterfaceSymbol Symbol;

    DragCommand();
    // ComponentInterface overrides
    ComponentInterfaceSymbol GetSymbol() const override { return Symbol; }
    TranslatableString GetDescription() const override { return XO("Drags mouse from one place to another."); }
    template<bool Const> bool VisitSettings(SettingsVisitorBase<Const>& S);
    bool VisitSettings(SettingsVisitor& S) override;
    bool VisitSettings(ConstSettingsVisitor& S) override;
    void PopulateOrExchange(ShuttleGui& S) override;

    // AudacityCommand overrides
    ManualPageID ManualPage() override { return L"Extra_Menu:_Scriptables_II#move_mouse"; }

    bool Apply(const CommandContext& context) override;

public:
    double mFromX;
    double mFromY;
    double mToX;
    double mToY;
    int mRelativeTo;
    int mId;
    wxString mWinName;

    bool bHasFromX;
    bool bHasFromY;
    bool bHasToX;
    bool bHasToY;
    bool bHasRelativeTo;
    bool bHasId;
    bool bHasWinName;
};

#endif /* End of include guard: __DRAG_COMMAND__ */
