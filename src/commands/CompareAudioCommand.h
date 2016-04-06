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

class CompareAudioCommandType final : public CommandType
{
public:
   wxString BuildName() override;
   void BuildSignature(CommandSignature &signature) override;
   CommandHolder Create(std::unique_ptr<CommandOutputTarget> &&target) override;
};

class CompareAudioCommand final : public CommandImplementation
{
private:
   double mT0, mT1;
   WaveTrack *mTrack0;
   WaveTrack *mTrack1;

   // Update member variables with project selection data (and validate)
   bool GetSelection(AudacityProject &proj);

protected:
   double CompareSample(double value1, double value2) /* not override */;

public:
   CompareAudioCommand(CommandType &type, std::unique_ptr<CommandOutputTarget> &&target)
      : CommandImplementation(type, std::move(target))
   { }
   bool Apply(CommandExecutionContext context) override;
};

#endif /* End of include guard: __COMPAREAUDIOCOMMAND__ */
