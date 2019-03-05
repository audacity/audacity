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

#define SET_PROJECT_PLUGIN_SYMBOL ComponentInterfaceSymbol{ XO("Set Project") }

class SetProjectCommand : public AudacityCommand
{
public:
   SetProjectCommand();
   // ComponentInterface overrides
   ComponentInterfaceSymbol GetSymbol() override {return SET_PROJECT_PLUGIN_SYMBOL;};
   wxString GetDescription() override {return _("Sets various values for a project.");};
   bool DefineParams( ShuttleParams & S ) override;
   void PopulateOrExchange(ShuttleGui & S) override;

   // AudacityCommand overrides
   wxString ManualPage() override {return wxT("Extra_Menu:_Scriptables_I#set_project");};

   bool Apply(const CommandContext & context) override;

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
