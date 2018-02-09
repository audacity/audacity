/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2009 Audacity Team
   License: wxwidgets

   Dan Horgan
   James Crook

******************************************************************//**

\file SetClipCommand.h
\brief Declarations of SetClipCommand and SetClipCommandType classes

*//*******************************************************************/

#ifndef __SET_CLIP_COMMAND__
#define __SET_CLIP_COMMAND__

#include "Command.h"
#include "CommandType.h"

#define SET_CLIP_PLUGIN_SYMBOL XO("Set Clip")

class SetClipCommand : public AudacityCommand
{
public:
   SetClipCommand();
   // CommandDefinitionInterface overrides
   wxString GetSymbol() override {return SET_CLIP_PLUGIN_SYMBOL;};
   wxString GetDescription() override {return _("Sets various values for a clip.");};
   bool DefineParams( ShuttleParams & S ) override;
   void PopulateOrExchange(ShuttleGui & S) override;

   // AudacityCommand overrides
   wxString ManualPage() override {return wxT("Extra_Menu:_Tools#set_clip");};

   bool Apply(const CommandContext & context) override;

public:
   int mClipIndex;
   int mColour;
   double mT0;

// For tracking optional parameters.
   bool bHasColour;
   bool bHasT0;
};


#endif /* End of include guard: __SETTRACKINFOCOMMAND__ */
