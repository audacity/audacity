/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2009 Audacity Team
   License: wxwidgets

   Dan Horgan
   James Crook

******************************************************************//**

\file CompareAudioCommand.h
\brief Contains declaration of CompareAudioCommand and CompareAudioCommandType
classes

*//*******************************************************************/

#ifndef __COMPAREAUDIOCOMMAND__
#define __COMPAREAUDIOCOMMAND__

#include "Command.h"
#include "CommandType.h"

class WaveTrack;

#define COMPARE_AUDIO_PLUGIN_SYMBOL ComponentInterfaceSymbol{ XO("Compare Audio") }

class CompareAudioCommand final : public AudacityCommand
{
public:
   // ComponentInterface overrides
   ComponentInterfaceSymbol GetSymbol() override {return XO("Compare Audio");}
   wxString GetDescription() override {return _("Compares a range on two tracks.");};
   bool DefineParams( ShuttleParams & S ) override;
   void PopulateOrExchange(ShuttleGui & S) override;
   bool Apply() override;

   // AudacityCommand overrides
   wxString ManualPage() override {return wxT("Extra_Menu:_Scriptables_II#compare_Audio");};
   bool Apply(const CommandContext &context) override;


private:
   double errorThreshold;
   double mT0, mT1;
   const WaveTrack *mTrack0;
   const WaveTrack *mTrack1;

   // Update member variables with project selection data (and validate)
   bool GetSelection(const CommandContext &context, AudacityProject &proj);

protected:
   double CompareSample(double value1, double value2) /* not override */;

};

#endif /* End of include guard: __COMPAREAUDIOCOMMAND__ */
