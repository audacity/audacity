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

class SetLabelCommand : public AudacityCommand
{
public:
   static const ComponentInterfaceSymbol Symbol;

   SetLabelCommand();
   // ComponentInterface overrides
   ComponentInterfaceSymbol GetSymbol() override {return Symbol;};
   TranslatableString GetDescription() override {return XO("Sets various values for a label.");};
   bool DefineParams( ShuttleParams & S ) override;
   void PopulateOrExchange(ShuttleGui & S) override;

   // AudacityCommand overrides
   wxString ManualPage() override {return wxT("Extra_Menu:_Scriptables_I#set_label");};

   bool Apply(const CommandContext & context) override;

public:
   // zero-based index of the desired label, within the concatenation of the
   // arrays of labels of all label tracks
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
