/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2018 Audacity Team
   License: wxwidgets

   James Crook

******************************************************************//**

\file SetEnvelopeCommand.h
\brief Declarations of SetEnvelopeCommand class

*//*******************************************************************/

#ifndef __SET_ENVELOPE_COMMAND__
#define __SET_ENVELOPE_COMMAND__

#include "Command.h"
#include "CommandType.h"

#define SET_ENVELOPE_PLUGIN_SYMBOL XO("Set Envelope")

class SetEnvelopeCommand : public AudacityCommand
{
public:
   SetEnvelopeCommand();
   // CommandDefinitionInterface overrides
   wxString GetSymbol() override {return SET_ENVELOPE_PLUGIN_SYMBOL;};
   wxString GetDescription() override {return _("Sets an envelope point position.");};
   bool DefineParams( ShuttleParams & S ) override;
   void PopulateOrExchange(ShuttleGui & S) override;

   // AudacityCommand overrides
   wxString ManualPage() override {return wxT("Extra_Menu:_Tools#set_label");};

   bool Apply(const CommandContext & context) override;

public:
   int mTrackIndex;
   int mChannelIndex;
   double mContainsTime;
   double mT;
   double mV;
   bool mbDelete;

   bool bHasTrackIndex;
   bool bHasChannelIndex;
   bool bHasContainsTime;
   bool bHasT;
   bool bHasV;
   bool bHasDelete;
};


#endif /* End of include guard: __SETTRACKINFOCOMMAND__ */
