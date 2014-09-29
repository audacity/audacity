/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2009 Audacity Team
   License: wxwidgets

   Dan Horgan

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

class CompareAudioCommandType : public CommandType
{
public:
   virtual wxString BuildName();
   virtual void BuildSignature(CommandSignature &signature);
   virtual Command *Create(CommandOutputTarget *target);
};

class CompareAudioCommand : public CommandImplementation
{
private:
   double mT0, mT1;
   WaveTrack *mTrack0;
   WaveTrack *mTrack1;

   // Update member variables with project selection data (and validate)
   bool GetSelection(AudacityProject &proj);

protected:
   virtual double CompareSample(double value1, double value2);

public:
   CompareAudioCommand(CommandType &type, CommandOutputTarget *target)
      : CommandImplementation(type, target)
   { }
   virtual bool Apply(CommandExecutionContext context);
};

#endif /* End of include guard: __COMPAREAUDIOCOMMAND__ */
