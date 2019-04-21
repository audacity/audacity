/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2018 Audacity Team
   License: wxwidgets

   James Crook

******************************************************************//**

\file SetLabelCommand.h
\brief Declarations of SetLabelCommand and SetLabelCommandType classes

*//*******************************************************************/

#ifndef __SET_LABEL_COMMAND__
#define __SET_LABEL_COMMAND__

#include "Command.h"
#include "CommandType.h"

#define SET_LABEL_PLUGIN_SYMBOL ComponentInterfaceSymbol{ XO("Set Label") }

class SetLabelCommand : public AudacityCommand
{
public:
   SetLabelCommand();
   // ComponentInterface overrides
   ComponentInterfaceSymbol GetSymbol() override {return SET_LABEL_PLUGIN_SYMBOL;};
   wxString GetDescription() override {return _("Sets various values for a label.");};
   bool DefineParams( ShuttleParams & S ) override;
   void PopulateOrExchange(ShuttleGui & S) override;

   // AudacityCommand overrides
   wxString ManualPage() override {return wxT("Extra_Menu:_Scriptables_I#set_label");};

   bool Apply(const CommandContext & context) override;

public:
   int mLabelIndex;
   wxString mText;
   double mT0;
   double mT1;
   bool mbSelected;

// For tracking optional parameters.
   bool bHasText;
   bool bHasT0;
   bool bHasT1;
   bool bHasSelected;
};


#endif /* End of include guard: __SETTRACKINFOCOMMAND__ */
