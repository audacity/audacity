/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2009 Audacity Team
   License: wxwidgets

   Dan Horgan

******************************************************************//**

\file CommandType.h
\brief Contains declarations for CommandType class

*//*******************************************************************/

#ifndef __COMMANDTYPE__
#define __COMMANDTYPE__

#include "CommandSignature.h"
#include "../commands/AudacityCommand.h"

class OldStyleCommand;

/**************************************************************//**

\class OldStyleCommand 
\brief OldStyleCommand is the key class that allows us to carry
a converted (not textual) command from a non-GUI place to the GUI
thread.  It contains the command AND the context that will be used 
for its output.

\class OldStyleCommandPointer
\brief OldStyleCommandPointer is a shared_ptr to a OldStyleCommandPointer.

*******************************************************************/

using OldStyleCommandPointer = std::shared_ptr<OldStyleCommand>;
class CommandOutputTargets;
class CommandSignature;
class wxString;

class AUDACITY_DLL_API OldStyleCommandType : public AudacityCommand
{
private:
   ComponentInterfaceSymbol mSymbol;
   Optional<CommandSignature> mSignature;

public:
   OldStyleCommandType();
   virtual ~OldStyleCommandType();
   ComponentInterfaceSymbol GetSymbol() override;
   CommandSignature &GetSignature();
   wxString Describe(); // for debugging only ?

   // Subclasses should override the following:
   // =========================================

   // Return the name of the command type
   virtual ComponentInterfaceSymbol BuildName() = 0;

   /// Postcondition: signature is a 'signature' map containing parameter
   // names, validators and default values.
   virtual void BuildSignature(CommandSignature &signature) = 0;

   // Create a command instance with the specified output target
   virtual OldStyleCommandPointer Create(
      AudacityProject &project, std::unique_ptr<CommandOutputTargets> &&target) = 0;
};

#endif /* End of include guard: __COMMANDTYPE__ */
