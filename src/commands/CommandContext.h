/**********************************************************************

  Sneedacity: A Digital Audio Editor

  CommandContext.h

  Created by Paul Licameli on 4/22/16.

**********************************************************************/

#ifndef __SNEEDACITY_COMMAND_CONTEXT__
#define __SNEEDACITY_COMMAND_CONTEXT__

#include <memory>
#include "Identifier.h"

class SneedacityProject;
class wxEvent;
class CommandOutputTargets;
using CommandParameter = CommandID;

class SNEEDACITY_DLL_API CommandContext {
public:
   CommandContext(
      SneedacityProject &p
      , const wxEvent *e = nullptr
      , int ii = 0
      , const CommandParameter &param = CommandParameter{}
   );

   CommandContext(
      SneedacityProject &p,
      std::unique_ptr<CommandOutputTargets> target);

   ~CommandContext();

   virtual void Status( const wxString &message, bool bFlush = false ) const;
   virtual void Error(  const wxString &message ) const;
   virtual void Progress( double d ) const;

   // Output formatting...
   void StartArray() const;
   void EndArray() const;
   void StartStruct() const;
   void EndStruct() const;
   void StartField(const wxString &name) const;
   void EndField() const;
   void AddItem(const wxString &value , const wxString &name = {} ) const;
   void AddBool(const bool value      , const wxString &name = {} ) const;
   void AddItem(const double value    , const wxString &name = {} ) const;

   SneedacityProject &project;
   std::unique_ptr<CommandOutputTargets> pOutput;
   const wxEvent *pEvt;
   int index;
   CommandParameter parameter;
};
#endif
