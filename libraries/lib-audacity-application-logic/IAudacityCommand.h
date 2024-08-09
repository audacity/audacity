/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  IAudacityCommand.h

  Matthieu Hodgkinson

**********************************************************************/
#pragma once

#include "ComponentInterface.h"
#include "Identifier.h"
#include "SettingsVisitor.h"

class AudacityProject;
class CommandContext;

class AUDACITY_APPLICATION_LOGIC_API IAudacityCommand : public ComponentInterface
{
public:
   virtual ~IAudacityCommand() = default;

   // Called once each time an effect is called.  Perform any initialization;
   // make sure that the command can be performed and
   // return false otherwise
   virtual bool Init() = 0;
   //! Visit settings, if defined.  false means no defined settings.
   //! Default implementation returns false
   virtual bool VisitSettings(SettingsVisitor&) = 0;
   //! Visit settings, if defined.  false means no defined settings.
   //! Default implementation returns false
   virtual bool VisitSettings(ConstSettingsVisitor&) = 0;
   virtual bool SaveSettingsAsString(wxString& parms) = 0;
   virtual bool LoadSettingsFromString(const wxString& parms) = 0;
   // If necessary, open a dialog to get parameters from the user.
   // This method will not always be called (for example if a user
   // repeats a command using 'repeat last command') but if it is called,
   // it will be called after Init.
   virtual bool PromptUser(AudacityProject& project) = 0;
   virtual void SetBatchProcessing(bool start) = 0;
   virtual bool DoAudacityCommand(
      const CommandContext& context, bool shouldPrompt = true) = 0;
   // Name of page in the Audacity alpha manual
   virtual ManualPageID ManualPage() = 0;
};
