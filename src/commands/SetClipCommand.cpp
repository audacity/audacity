/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2018 Audacity Team
   License: wxwidgets

   James Crook

******************************************************************//**

\file SetClipCommand.cpp
\brief Definitions for SetClipCommand

\class SetClipCommand
\brief Command that sets clip information

*//*******************************************************************/


#include "SetClipCommand.h"

#include "LoadCommands.h"
#include "../WaveClip.h"
#include "../WaveTrack.h"
#include "../Shuttle.h"
#include "../ShuttleGui.h"

const ComponentInterfaceSymbol SetClipCommand::Symbol
{ XO("Set Clip") };

namespace{ BuiltinCommandsModule::Registration< SetClipCommand > reg; }

SetClipCommand::SetClipCommand()
{
}

enum kColours
{
   kColour0,
   kColour1,
   kColour2,
   kColour3,
   nColours
};

static const EnumValueSymbol kColourStrings[nColours] =
{
   { wxT("Color0"), XO("Color 0") },
   { wxT("Color1"), XO("Color 1") },
   { wxT("Color2"), XO("Color 2") },
   { wxT("Color3"), XO("Color 3") },
};


bool SetClipCommand::DefineParams( ShuttleParams & S ){ 
   S.OptionalY( bHasContainsTime   ).Define(     mContainsTime,   wxT("At"),         0.0, 0.0, 100000.0 );
   S.OptionalN( bHasColour         ).DefineEnum( mColour,         wxT("Color"),      kColour0, kColourStrings, nColours );
   // Allowing a negative start time is not a mistake.
   // It will be used in demonstrating time before zero.
   S.OptionalN( bHasT0             ).Define(     mT0,             wxT("Start"),      0.0, -5.0, 1000000.0);
   return true;
};

void SetClipCommand::PopulateOrExchange(ShuttleGui & S)
{
   S.AddSpace(0, 5);

   S.StartMultiColumn(3, wxALIGN_CENTER);
   {
      S.Optional( bHasContainsTime).TieNumericTextBox(  XXO("At:"),            mContainsTime );
      S.Optional( bHasColour      ).TieChoice(          XXO("Color:"),         mColour,
         Msgids( kColourStrings, nColours ) );
      S.Optional( bHasT0          ).TieNumericTextBox(  XXO("Start:"),         mT0 );
      S.Optional( bHasName        ).TieTextBox(         XXO("Name:"),          mName );
   }
   S.EndMultiColumn();
}

bool SetClipCommand::ApplyInner( const CommandContext &, Track * t )
{
   // if no 'At' is specified, then any clip in any selected track will be set.
   t->TypeSwitch([&](WaveTrack *waveTrack) {
      WaveClipPointers ptrs( waveTrack->SortedClipArray());
      for(auto it = ptrs.begin(); (it != ptrs.end()); it++ ){
         WaveClip * pClip = *it;
         bool bFound =
            !bHasContainsTime || (
               ( pClip->GetPlayStartTime() <= mContainsTime ) &&
               ( pClip->GetPlayEndTime() >= mContainsTime )
            );
         if( bFound )
         {
            // Inside this IF is where we actually apply the command

            if( bHasColour )
               pClip->SetColourIndex(mColour);
            // No validation of overlap yet.  We assume the user is sensible!
            if( bHasT0 )
               pClip->SetPlayStartTime(mT0);
            // \todo Use SetClip to move a clip between tracks too.
            if( bHasName )
               pClip->SetName(mName);

         }
      }
   } );
   return true;
}
