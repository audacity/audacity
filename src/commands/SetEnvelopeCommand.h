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
#include "SetTrackInfoCommand.h"

#define SET_ENVELOPE_PLUGIN_SYMBOL ComponentInterfaceSymbol{ XO("Set Envelope") }

class SetEnvelopeCommand : public SetTrackBase
{
public:
   SetEnvelopeCommand();
   // ComponentInterface overrides
   ComponentInterfaceSymbol GetSymbol() override {return SET_ENVELOPE_PLUGIN_SYMBOL;};
   wxString GetDescription() override {return _("Sets an envelope point position.");};
   bool DefineParams( ShuttleParams & S ) override;
   void PopulateOrExchange(ShuttleGui & S) override;

   // AudacityCommand overrides
   wxString ManualPage() override {return wxT("Extra_Menu:_Scriptables_I#set_envelope");};
   bool ApplyInner( const CommandContext & context, Track * t ) override;

public:
   double mT;
   double mV;
   bool mbDelete;

   bool bHasT;
   bool bHasV;
   bool bHasDelete;
};


#endif /* End of include guard: __SETTRACKINFOCOMMAND__ */
