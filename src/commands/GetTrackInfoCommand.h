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

#define GET_TRACK_INFO_PLUGIN_SYMBOL ComponentInterfaceSymbol{ XO("Get Track Info") }

class GetTrackInfoCommand final : public AudacityCommand
{
public:
   GetTrackInfoCommand();
   // ComponentInterface overrides
   ComponentInterfaceSymbol GetSymbol() override {return GET_TRACK_INFO_PLUGIN_SYMBOL;};
   wxString GetDescription() override {return _("Gets track values as JSON.");};
   bool DefineParams( ShuttleParams & S ) override;
   void PopulateOrExchange(ShuttleGui & S) override;

   // AudacityCommand overrides
   wxString ManualPage() override {return wxT("Extra_Menu:_Tools#get_track_info");};

   bool Apply(const CommandContext &context ) override;
public:
   int mInfoType;
};

#endif /* End of include guard: __GETTRACKINFOCOMMAND__ */
