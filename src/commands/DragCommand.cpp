/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2018 Audacity Team
   License: wxwidgets

   James Crook

******************************************************************//**

\file DragCommand.cpp
\brief Definitions for DragCommand

\class DragCommand
\brief Command that sets clip information

*//*******************************************************************/


#include "DragCommand.h"

#include "LoadCommands.h"
#include "Project.h"
#include "../ProjectWindows.h"
#include "../WaveTrack.h"
#include "../Shuttle.h"
#include "../ShuttleGui.h"
#include "CommandContext.h"

#include <wx/frame.h>

const ComponentInterfaceSymbol DragCommand::Symbol
{ XO("Drag") };

namespace{ BuiltinCommandsModule::Registration< DragCommand > reg; }

DragCommand::DragCommand()
{
}

enum kCoordTypes
{
   kPanel,
   kApp,
   kTrack0,
   kTrack1,
   nCoordTypes
};

static const EnumValueSymbol kCoordTypeStrings[nCoordTypes] =
{
   { XO("Panel") },
   { wxT("App"),    XO("Application") },
   { wxT("Track0"), XO("Track 0") },
   { wxT("Track1"), XO("Track 1") },
};


bool DragCommand::DefineParams( ShuttleParams & S ){ 
   S.OptionalN( bHasId         ).Define(     mId,          wxT("Id"),         11000.0, -100000.0, 1000000.0);
   S.OptionalY( bHasWinName    ).Define(     mWinName,     wxT("Window"),     "Timeline");
   S.OptionalY( bHasFromX      ).Define(     mFromX,       wxT("FromX"),      200.0, 0.0, 1000000.0);
   S.OptionalY( bHasFromY      ).Define(     mFromY,       wxT("FromY"),      10.0,  0.0, 1000000.0);
   S.OptionalN( bHasToX        ).Define(     mToX,         wxT("ToX"),        400.0, 0.0, 1000000.0);
   S.OptionalN( bHasToY        ).Define(     mToY,         wxT("ToY"),        10.0,  0.0, 1000000.0);
   S.OptionalN( bHasRelativeTo ).DefineEnum( mRelativeTo,  wxT("RelativeTo"), kPanel, kCoordTypeStrings, nCoordTypes );
   return true;
};

void DragCommand::PopulateOrExchange(ShuttleGui & S)
{
   S.AddSpace(0, 5);

   S.StartMultiColumn(3, wxALIGN_CENTER);
   {
      /* i18n-hint abbreviates "Identity" or "Identifier" */
      S.Optional( bHasId         ).TieNumericTextBox(  XXO("Id:"),          mId );
      S.Optional( bHasWinName    ).TieTextBox(         XXO("Window Name:"), mWinName );
      S.Optional( bHasFromX      ).TieNumericTextBox(  XXO("From X:"),      mFromX );
      S.Optional( bHasFromY      ).TieNumericTextBox(  XXO("From Y:"),      mFromY );
      S.Optional( bHasToX        ).TieNumericTextBox(  XXO("To X:"),        mToX );
      S.Optional( bHasToY        ).TieNumericTextBox(  XXO("To Y:"),        mToY );
      S.Optional( bHasRelativeTo ).TieChoice(          XXO("Relative To:"), mRelativeTo,
         Msgids( kCoordTypeStrings, nCoordTypes ) );
   }
   S.EndMultiColumn();
}

bool DragCommand::Apply(const CommandContext & context)
{
   // Defaults if no value...
   if( !bHasFromX )
      mFromX = 200.0;
   if( !bHasFromY )
      mFromY = 10;
   if( !bHasToX )
      mToX = 400;
   if( !bHasToY )
      mToY = 10;

   wxWindow * pWin = &GetProjectFrame( context.project );
   wxWindow * pWin1 = nullptr;
   wxMouseEvent Evt( wxEVT_MOTION );
   Evt.m_x = mFromX;
   Evt.m_y = mFromY;
   if( bHasId )
      pWin1 = pWin->FindWindowById( mId );
   if( bHasWinName )
      pWin1 = pWin->FindWindowByName( mWinName );
   if( pWin1 )
      pWin = pWin1;
   // Process twice - possible bug in Audacity being worked around
   // where we need an event to enter AND an event to move.
   // AdornedRuler Quick-Play bug.
   pWin->GetEventHandler()->ProcessEvent( Evt );
   pWin->GetEventHandler()->ProcessEvent( Evt );
   if( bHasToX ){
      wxMouseEvent Evt2( wxEVT_LEFT_DOWN );
      Evt2.m_leftDown = true;
      Evt2.m_x = mFromX;
      Evt2.m_y = mFromY;
      Evt2.m_aux2Down = true;
      pWin->GetEventHandler()->ProcessEvent( Evt2 );
      wxMouseEvent Evt3( wxEVT_MOTION );
      Evt3.m_leftDown = true;
      Evt2.m_aux2Down = true;
      Evt3.m_x = mToX;
      Evt3.m_y = mToY;
      // AdornedRuler Quick-Play bug again.
      pWin->GetEventHandler()->ProcessEvent( Evt3 );
      pWin->GetEventHandler()->ProcessEvent( Evt3 );
      wxMouseEvent Evt4( wxEVT_LEFT_UP );
      Evt2.m_aux2Down = true;
      Evt4.m_x = mToX;
      Evt4.m_y = mToY;
      pWin->GetEventHandler()->ProcessEvent( Evt4 );
   }
   return true;
}
