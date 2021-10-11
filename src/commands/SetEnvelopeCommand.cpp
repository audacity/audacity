/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2018 Audacity Team
   License: wxwidgets

   James Crook

******************************************************************//**

\file SetEnvelopeCommand.cpp
\brief Definitions for SetEnvelopeCommand

\class SetEnvelopeCommand
\brief Command that sets envelope information

*//*******************************************************************/


#include "SetEnvelopeCommand.h"

#include "CommandContext.h"
#include "LoadCommands.h"
#include "../ProjectHistory.h"
#include "../UndoManager.h"
#include "../WaveClip.h"
#include "../WaveTrack.h"
#include "../Envelope.h"
#include "../Shuttle.h"
#include "../ShuttleGui.h"

const ComponentInterfaceSymbol SetEnvelopeCommand::Symbol
{ XO("Set Envelope") };

namespace{ BuiltinCommandsModule::Registration< SetEnvelopeCommand > reg; }


SetEnvelopeCommand::SetEnvelopeCommand()
{
}


bool SetEnvelopeCommand::DefineParams( ShuttleParams & S ){ 
   S.OptionalY( bHasT              ).Define(  mT,              wxT("Time"),     0.0, 0.0, 100000.0);
   S.OptionalY( bHasV              ).Define(  mV,              wxT("Value"),    1.0, 0.0, 2.0);
   S.OptionalN( bHasDelete         ).Define(  mbDelete,        wxT("Delete"),   false );
   return true;
};

void SetEnvelopeCommand::PopulateOrExchange(ShuttleGui & S)
{
   S.AddSpace(0, 5);

   S.StartMultiColumn(3, wxALIGN_CENTER);
   {
      S.Optional( bHasT           ).TieNumericTextBox(  XXO("Time:"),          mT );
      S.Optional( bHasV           ).TieNumericTextBox(  XXO("Value:"),         mV );
      S.Optional( bHasDelete      ).TieCheckBox(        XXO("Delete"),         mbDelete );
   }
   S.EndMultiColumn();
}

bool SetEnvelopeCommand::ApplyInner( const CommandContext &context, Track * t )
{
   // if no time is specified, then
   //   - delete deletes any envelope in selected tracks.
   //   - value is not set for any clip
   t->TypeSwitch([&](WaveTrack *waveTrack) {
      WaveClipPointers ptrs( waveTrack->SortedClipArray());
      for(auto it = ptrs.begin(); (it != ptrs.end()); it++ ){
         WaveClip * pClip = *it;
         bool bFound =
            !bHasT || (
               ( pClip->GetPlayStartTime() <= mT) &&
               ( pClip->GetPlayEndTime() >= mT )
            );
         if( bFound )
         {
            // Inside this IF is where we actually apply the command
            Envelope* pEnv = pClip->GetEnvelope();
            bool didSomething = false;
            if( bHasDelete && mbDelete )
               pEnv->Clear(), didSomething = true;
            if( bHasT && bHasV )
               pEnv->InsertOrReplace( mT, pEnv->ClampValue( mV ) ),
               didSomething = true;

            if (didSomething)
               // Consolidate, because this ApplyInner() function may be
               // visited multiple times in one command invocation
               ProjectHistory::Get(context.project).PushState(
                  XO("Edited Envelope"), XO("Envelope"),
                  UndoPush::CONSOLIDATE);
         }
      }
   } );

   return true;
}
