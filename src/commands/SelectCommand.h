/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2018 Audacity Team
   License: GPL v2 - see LICENSE.txt

   Dan Horgan
   James Crook

******************************************************************//**

\file SelectCommand.h
\brief Declarations for SelectCommand and SelectCommandType classes

*//*******************************************************************/

#ifndef __SELECT_COMMAND__
#define __SELECT_COMMAND__



#include "CommandType.h"
#include "Command.h"

//#include "../commands/AudacityCommand.h"


#define SELECT_TIME_PLUGIN_SYMBOL ComponentInterfaceSymbol{ XO("Select Time") }
#define SELECT_FREQUENCIES_PLUGIN_SYMBOL ComponentInterfaceSymbol{ XO("Select Frequencies") }
#define SELECT_TRACKS_PLUGIN_SYMBOL ComponentInterfaceSymbol{ XO("Select Tracks") }
#define SELECT_PLUGIN_SYMBOL ComponentInterfaceSymbol{ XO("Select") }

class SelectTimeCommand : public AudacityCommand
{
public:
   // ComponentInterface overrides
   ComponentInterfaceSymbol GetSymbol() override {return SELECT_TIME_PLUGIN_SYMBOL;};
   wxString GetDescription() override {return _("Selects a time range.");};
   bool DefineParams( ShuttleParams & S ) override;
   void PopulateOrExchange(ShuttleGui & S) override;
   bool Apply(const CommandContext & context) override;

   // AudacityCommand overrides
   wxString ManualPage() override {return wxT("Extra_Menu:_Scriptables_I#select_time");};

   bool bHasT0;
   bool bHasT1;
   bool bHasFromEnd;
   bool bHasRelativeSpec;

   double mT0;
   double mT1;
   int mRelativeTo;
   bool mFromEnd;
};

class SelectFrequenciesCommand : public AudacityCommand
{
public:
   // ComponentInterface overrides
   ComponentInterfaceSymbol GetSymbol() override {return SELECT_FREQUENCIES_PLUGIN_SYMBOL;};
   wxString GetDescription() override {return _("Selects a frequency range.");};
   bool DefineParams( ShuttleParams & S ) override;
   void PopulateOrExchange(ShuttleGui & S) override;
   bool Apply(const CommandContext & context) override;

   // AudacityCommand overrides
   wxString ManualPage() override {return wxT("Extra_Menu:_Scriptables_I#select_frequencies");};

   bool bHasBottom;
   bool bHasTop;

   double mBottom;
   double mTop;
};


class SelectTracksCommand : public AudacityCommand
{
public:
   // ComponentInterface overrides
   ComponentInterfaceSymbol GetSymbol() override {return SELECT_TRACKS_PLUGIN_SYMBOL;};
   wxString GetDescription() override {return _("Selects a range of tracks.");};
   bool DefineParams( ShuttleParams & S ) override;
   void PopulateOrExchange(ShuttleGui & S) override;
   bool Apply(const CommandContext & context) override;
   // AudacityCommand overrides
   wxString ManualPage() override {return wxT("Extra_Menu:_Scriptables_I#select_tracks");};

   bool bHasFirstTrack;
   bool bHasNumTracks;
   bool bHasMode;

   double mFirstTrack;
   double mNumTracks;
   int mMode;
};


class SelectCommand : public AudacityCommand
{
public:
   // ComponentInterface overrides
   ComponentInterfaceSymbol GetSymbol() override {return SELECT_PLUGIN_SYMBOL;};
   wxString GetDescription() override {return _("Selects Audio.");};
   bool DefineParams( ShuttleParams & S ) override { 
      return 
         mSelTime.DefineParams(S) &&  
         mSelFreq.DefineParams(S) &&
         mSelTracks.DefineParams(S);
   };
   void PopulateOrExchange(ShuttleGui & S) override {
      mSelTime.PopulateOrExchange(S);
      mSelFreq.PopulateOrExchange(S);
      mSelTracks.PopulateOrExchange(S);
   };
   bool Apply(const CommandContext & context) override {
      return 
         mSelTime.Apply(context) &&  
         mSelFreq.Apply( context )&&
         mSelTracks.Apply(context);
   }
   // AudacityCommand overrides
   wxString ManualPage() override {return wxT("Extra_Menu:_Scriptables_II#select");};
private:
   SelectTimeCommand mSelTime;
   SelectFrequenciesCommand mSelFreq;
   SelectTracksCommand mSelTracks;


};

#endif /* End of include guard: __SELECT_COMMAND__ */
