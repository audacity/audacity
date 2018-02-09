/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2009 Audacity Team
   License: wxWidgets

   Dan Horgan

******************************************************************//**

\file GetInfoCommand.h
\brief Contains declaration of GetInfoCommand class.

\class GetInfoCommand
\brief Command which outputs a list of available menu commands on the status
channel.

*//*******************************************************************/

#ifndef __GET_INFO_COMMAND__
#define __GET_INFO_COMMAND__

#include "Command.h"
#include "CommandType.h"

class wxMenuBar;
class wxPoint;

#define GET_INFO_PLUGIN_SYMBOL XO("Get Info")

class GetInfoCommand : public AudacityCommand
{
public:
   // CommandDefinitionInterface overrides
   wxString GetSymbol() override {return GET_INFO_PLUGIN_SYMBOL;};
   wxString GetDescription() override {return _("Gets information in JSON format.");};
   bool DefineParams( ShuttleParams & S ) override;
   void PopulateOrExchange(ShuttleGui & S) override;

   // AudacityCommand overrides
   wxString ManualPage() override {return wxT("Automation");};
   bool Apply(const CommandContext &context) override;
   bool ApplyInner(const CommandContext &context);

public:
   int mInfoType;
   int mFormat;

private:
   bool SendCommands(const CommandContext & context, int flags);
   bool SendMenus(const CommandContext & context);
   bool SendTracks(const CommandContext & context);
   bool SendLabels(const CommandContext & context);
   bool SendClips(const CommandContext & context);
   bool SendBoxes(const CommandContext & context);

   void ExploreMenu( const CommandContext &context, wxMenu * pMenu, int Id, int depth );
   void ExploreTrackPanel( const CommandContext & context,
      wxPoint P, wxWindow * pWin, int Id, int depth );
   void ExploreAdornments( const CommandContext & context,
      wxPoint P, wxWindow * pWin, int Id, int depth );
   void ExploreWindows( const CommandContext & context,
      wxPoint P, wxWindow * pWin, int Id, int depth );

};

#endif /* End of include guard: __GET_INFO_COMMAND__ */
