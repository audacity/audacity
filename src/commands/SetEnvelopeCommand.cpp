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
#include "../Project.h"
#include "../Track.h"
#include "../TrackPanel.h"
#include "../WaveTrack.h"
#include "../Envelope.h"
#include "../ShuttleGui.h"
#include "CommandContext.h"

SetEnvelopeCommand::SetEnvelopeCommand()
{
}


bool SetEnvelopeCommand::DefineParams( ShuttleParams & S ){ 
   S.OptionalY( bHasTrackIndex     ).Define(  mTrackIndex,     wxT("Track"),      0, 0, 100 );
   S.OptionalN( bHasChannelIndex   ).Define(  mChannelIndex,   wxT("Channel"),    0, 0, 100 );
   S.OptionalY( bHasT              ).Define(  mT,              wxT("Time"),     0.0, 0.0, 100000.0);
   S.OptionalY( bHasV              ).Define(  mV,              wxT("Value"),    0.0, 0.0, 2.0);
   S.OptionalN( bHasDelete         ).Define(  mbDelete,        wxT("Delete"),   false );
   return true;
};

void SetEnvelopeCommand::PopulateOrExchange(ShuttleGui & S)
{
   S.AddSpace(0, 5);

   S.StartMultiColumn(3, wxALIGN_CENTER);
   {
      S.Optional( bHasTrackIndex  ).TieNumericTextBox(  _("Track Index:"),   mTrackIndex );
      S.Optional( bHasChannelIndex).TieNumericTextBox(  _("Channel Index:"), mChannelIndex );
      S.Optional( bHasT           ).TieNumericTextBox(  _("Time:"),          mT );
      S.Optional( bHasV           ).TieNumericTextBox(  _("Value:"),         mV );
      S.Optional( bHasDelete      ).TieCheckBox(        _("Delete:"),        mbDelete );
   }
   S.EndMultiColumn();
}

bool SetEnvelopeCommand::Apply(const CommandContext & context)
{
   // \todo we have similar code for finding the nth Label, Clip, Track etc.
   // this code could be put in subroutines/reduced.

   TrackList *tracks = context.GetProject()->GetTracks();
   TrackListIterator iter(tracks);
   Track *t = iter.First();
   WaveClip * pClip = NULL;
   int i=0;
   int j=0;

   bool bIsSecondChannel = false;

   while (t )
   {
      bool bThisTrack = 
         (bHasTrackIndex && (i==mTrackIndex)) ||
         (bHasChannelIndex && (j==mChannelIndex ) ) ||
         (!bHasTrackIndex && !bHasChannelIndex) ;

      if( bThisTrack && (t->GetKind() == Track::Wave)) {
         bool bFound = false;
         WaveTrack *waveTrack = static_cast<WaveTrack*>(t);
         WaveClipPointers ptrs( waveTrack->SortedClipArray());
         for(auto it = ptrs.begin(); (it != ptrs.end()); it++ ){
            pClip = *it;
            bFound = 
               !bHasT || (
                  ( pClip->GetStartTime() <= mT) &&
                  ( pClip->GetEndTime() >= mT )
               );
            if( bFound )
            {
               // Inside this IF is where we actually apply the command
               Envelope* pEnv = pClip->GetEnvelope();
               if( mbDelete )
                  pEnv->mEnv.clear();
               else
                  pEnv->InsertOrReplace( mT, mV );
            }
         }
      }
      bIsSecondChannel = t->GetLinked();
      if( !bIsSecondChannel )
         ++i;
      j++;
      t = iter.Next();
   }

   return true;
}
