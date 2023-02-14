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


#include "Demo.h"
#include "LoadCommands.h"

#include <float.h>

#include "SettingsVisitor.h"
#include "ShuttleGui.h"
#include "AudacityMessageBox.h"
#include "valnum.h"
#include "../commands/CommandContext.h"

const ComponentInterfaceSymbol DemoCommand::Symbol
{ XO("Demo") };

//Don't register the demo command.  
//namespace{ BuiltinCommandsModule::Registration< DemoCommand > reg; }

template<bool Const>
bool DemoCommand::VisitSettings( SettingsVisitorBase<Const> & S ){
   S.Define( delay, wxT("Delay"), 1.0f, 0.001f,  FLT_MAX, 1.0f );
   S.Define( decay, wxT("Decay"), 0.5f, 0.0f,    FLT_MAX, 1.0f  );
   return true;
}

bool DemoCommand::VisitSettings( SettingsVisitor & S )
   { return VisitSettings<false>(S); }

bool DemoCommand::VisitSettings( ConstSettingsVisitor & S )
   { return VisitSettings<true>(S); }

bool DemoCommand::Apply(const CommandContext & context){
   context.Status( "A Message");
   return true;
}

void DemoCommand::PopulateOrExchange(ShuttleGui & S)
{
   S.AddSpace(0, 5);

   S.StartMultiColumn(2, wxALIGN_CENTER);
   {
      S.TieTextBox(XXO("Delay time (seconds):"),delay);
      S.TieTextBox(XXO("Decay factor:"),decay);
   }
   S.EndMultiColumn();
}



