/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2009 Audacity Team
   License: GPL v2 - see LICENSE.txt

   Dan Horgan
   James Crook

******************************************************************//**

\file SelectCommand.h
\brief Declarations for SelectCommand and SelectCommandType classes

*//*******************************************************************/

#ifndef __SELECTCOMMAND__
#define __SELECTCOMMAND__



#include "CommandType.h"
#include "Command.h"

//#include "../commands/AudacityCommand.h"


#define SELECT_TIME_PLUGIN_SYMBOL XO("Select Time")
#define SELECT_TRACKS_PLUGIN_SYMBOL XO("Select Tracks")
#define SELECT_PLUGIN_SYMBOL XO("Select")

class SelectTimeCommand : public AudacityCommand
{
public:
   // CommandDefinitionInterface overrides
   wxString GetSymbol() override {return SELECT_TIME_PLUGIN_SYMBOL;};
   wxString GetDescription() override {return _("Selects a time range.");};
   bool DefineParams( ShuttleParams & S ) override;
   void PopulateOrExchange(ShuttleGui & S) override;
   bool Apply(const CommandContext & context) override;

   // AudacityCommand overrides
   wxString ManualPage() override {return wxT("Audio_Selection");};
   double mT0;
   double mT1;
   bool mFromEnd;
};

class SelectTracksCommand : public AudacityCommand
{
public:
   // CommandDefinitionInterface overrides
   wxString GetSymbol() override {return SELECT_TRACKS_PLUGIN_SYMBOL;};
   wxString GetDescription() override {return _("Selects a range of tracks.");};
   bool DefineParams( ShuttleParams & S ) override;
   void PopulateOrExchange(ShuttleGui & S) override;
   bool Apply(const CommandContext & context) override;
   // AudacityCommand overrides
   wxString ManualPage() override {return wxT("Audio_Selection");};

   int mFirstTrack;
   int mLastTrack;
   int mMode;
};


class SelectCommand : public AudacityCommand
{
public:
   // CommandDefinitionInterface overrides
   wxString GetSymbol() override {return SELECT_PLUGIN_SYMBOL;};
   wxString GetDescription() override {return _("Selects Audio.");};
   bool DefineParams( ShuttleParams & S ) override { 
      return mSelTime.DefineParams(S) &&  mSelTracks.DefineParams(S);
   };
   void PopulateOrExchange(ShuttleGui & S) override {
      mSelTime.PopulateOrExchange(S);
      mSelTracks.PopulateOrExchange(S);
   };
   bool Apply(const CommandContext & context) override {
      return mSelTime.Apply(context) &&  mSelTracks.Apply(context);
   }
   // AudacityCommand overrides
   wxString ManualPage() override {return wxT("Audio_Selection");};
private:
   SelectTimeCommand mSelTime;
   SelectTracksCommand mSelTracks;

};

#endif /* End of include guard: __SELECTCOMMAND__ */
