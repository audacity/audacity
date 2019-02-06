/**********************************************************************

  Audacity: A Digital Audio Editor

  Demo.cpp

  James Crook

*******************************************************************//**

\class DemoCommand
\brief An AudacityCommand that does nothing but provide 
parameters.  It is for development purposes.

*//****************************************************************//**

\class DemoDialog
\brief DemoDialog used with DemoCommand

*//*******************************************************************/

#include "../Audacity.h"
#include "Demo.h"

#include <float.h>

#include <wx/intl.h>

#include "../Shuttle.h"
#include "../ShuttleGui.h"
#include "../widgets/ErrorDialog.h"
#include "../widgets/valnum.h"
#include "../SampleFormat.h"
#include "../commands/Command.h"
#include "../commands/CommandContext.h"

bool DemoCommand::DefineParams( ShuttleParams & S ){
   S.Define( delay, wxT("Delay"), 1.0f, 0.001f,  FLT_MAX, 1.0f );
   S.Define( decay, wxT("Decay"), 0.5f, 0.0f,    FLT_MAX, 1.0f  );
   return true;
}

bool DemoCommand::Apply(const CommandContext & context){
   context.Status( "A Message");
   return true;
}

void DemoCommand::PopulateOrExchange(ShuttleGui & S)
{
   S.AddSpace(0, 5);

   S.StartMultiColumn(2, wxALIGN_CENTER);
   {
      S.TieTextBox(_("Delay time (seconds):"),delay);
      S.TieTextBox(_("Decay factor:"),decay);
   }
   S.EndMultiColumn();
}



