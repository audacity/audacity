/**********************************************************************

  Audacity: A Digital Audio Editor

  CommandContext.h

  Created by Paul Licameli on 4/22/16.

**********************************************************************/

#ifndef __AUDACITY_COMMAND_CONTEXT__
#define __AUDACITY_COMMAND_CONTEXT__

#include <wx/string.h>
#include <wx/event.h>
#include "../MemoryX.h"
#include "Command.h"

class AudacityProject;
class AudacityApp;
class wxEvent;
class CommandOutputTarget;
using CommandParameter = wxString;

class CommandContext {
public:
   CommandContext(
      AudacityProject &p
      , const wxEvent *e = nullptr
      , int ii = 0
      , const CommandParameter &param = CommandParameter{}
   );

   CommandContext(
      AudacityProject &p,
      std::unique_ptr<CommandOutputTarget> target);

   virtual void Status( const wxString & WXUNUSED(message) ) const;
   virtual void Error(  const wxString & WXUNUSED(message) ) const;
   virtual void Progress( double WXUNUSED(d) ) const;

   // Output formatting...
   void StartArray() const;
   void EndArray() const;
   void StartStruct() const;
   void EndStruct() const;
   void AddItem(const wxString &value , const wxString &name="" ) const;
   void AddBool(const bool value      , const wxString &name="" ) const;
   void AddItem(const double value    , const wxString &name="" ) const;

   AudacityProject &project;
   std::unique_ptr<CommandOutputTarget> pOutput;
   const wxEvent *pEvt;
   int index;
   CommandParameter parameter;
   AudacityApp *GetApp() const;
   AudacityProject *GetProject() const;
};
#endif
