/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2009 Audacity Team
   License: wxwidgets

   Dan Horgan
   James Crook

******************************************************************//**

\file SetTrackInfoCommand.h
\brief Declarations of SetTrackInfoCommand and SetTrackInfoCommandType classes

*//*******************************************************************/

#ifndef __SETTRACKINFOCOMMAND__
#define __SETTRACKINFOCOMMAND__

#include "Command.h"
#include "CommandType.h"

#define SET_TRACK_INFO_PLUGIN_SYMBOL XO("Set Track Info")

class SetTrackInfoCommand : public AudacityCommand
{
public:
   SetTrackInfoCommand();
   // CommandDefinitionInterface overrides
   wxString GetSymbol() override {return SET_TRACK_INFO_PLUGIN_SYMBOL;};
   wxString GetDescription() override {return _("Sets various values for a track.");};
   bool DefineParams( ShuttleParams & S ) override;
   void PopulateOrExchange(ShuttleGui & S) override;

   // AudacityCommand overrides
   wxString ManualPage() override {return wxT("Extra_Menu:_Tools#set_track_info");};

   bool Apply(const CommandContext & context) override;

public:
   int mTrackIndex;
   wxString mTrackName;
   double mPan;
   double mGain;
   bool bSelected;
   bool bFocused;
   bool bSolo;
   bool bMute;

// For tracking optional parameters.
   bool bHasTrackName;
   bool bHasPan;
   bool bHasGain;
   bool bHasSelected;
   bool bHasFocused;
   bool bHasSolo;
   bool bHasMute;
};


#endif /* End of include guard: __SETTRACKINFOCOMMAND__ */
