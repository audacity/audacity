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

class CompareAudioCommand final : public AudacityCommand
{
public:
   static const ComponentInterfaceSymbol Symbol;

   // ComponentInterface overrides
   ComponentInterfaceSymbol GetSymbol() override {return Symbol;}
   TranslatableString  GetDescription() override {return XO("Compares a range on two tracks.");};
   bool DefineParams( ShuttleParams & S ) override;
   void PopulateOrExchange(ShuttleGui & S) override;

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
