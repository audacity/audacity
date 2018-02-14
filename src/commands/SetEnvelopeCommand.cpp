/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2009 Audacity Team
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
   S.Define(  mTrackIndex,  wxT("Track"),    0, 0, 100 );
   S.Define(  mT,           wxT("Time"),     0.0, 0.0, 100000.0);
   S.Define(  mV,           wxT("Value"),    0.0, 0.0, 2.0);
   S.Define(  mbDelete,     wxT("Delete"),   false );
   return true;
};

void SetEnvelopeCommand::PopulateOrExchange(ShuttleGui & S)
{
   S.AddSpace(0, 5);

   S.StartMultiColumn(2, wxALIGN_CENTER);
   {
      S.TieNumericTextBox( _("Track Index:"), mTrackIndex );
      S.TieNumericTextBox( _("Time:"),        mT );
      S.TieNumericTextBox( _("Value:"),       mV );
      S.TieCheckBox(       _("Delete:"),      mbDelete );
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

   bool bFound = false;

   while (t && !bFound) {
      if (t->GetKind() == Track::Wave) {
         WaveTrack *waveTrack = static_cast<WaveTrack*>(t);
         WaveClipPointers ptrs( waveTrack->SortedClipArray());
         for(auto it = ptrs.begin(); (it != ptrs.end()) && !bFound; it++,i++ ){
            pClip = *it;
            bFound = 
               ( pClip->GetStartTime() <= mT ) &&
               ( pClip->GetEndTime() >= mT );
         }
      }
      t = iter.Next();
   }
   if( !bFound )
      return false;
   Envelope* pEnv = pClip->GetEnvelope();
   if( mbDelete ){
      pEnv->mEnv.clear();
      return true;
   }

   pEnv->InsertOrReplace( mT, mV );
   /*
   double tFind = mT - 0.000001 - pEnv->mOffset; // 100,000th of a second before.

   bFound = false;
   for( i=0;i<pEnv->mEnv.size() && !bFound;i++ ){
      bFound = tFind > pEnv->mEnv[i].GetT();
   }
   i -= bFound ? 1 :0;

   pEnv->Insert( i, EnvPoint( mT, mV ) ); 
   */

   return true;
}
