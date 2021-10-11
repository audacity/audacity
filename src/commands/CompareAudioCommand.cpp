/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2018 Audacity Team
   License: wxwidgets

   Dan Horgan
   James Crook

******************************************************************//**

\file CompareAudioCommand.cpp
\brief Contains definitions for CompareAudioCommand class

\class CompareAudioCommand
\brief Returns information about the amount of audio that is about a certain
threshold of difference in two selected tracks

*//*******************************************************************/


#include "CompareAudioCommand.h"

#include "LoadCommands.h"
#include "ViewInfo.h"
#include "../WaveTrack.h"


#include <float.h>
#include <wx/intl.h>

#include "../Shuttle.h"
#include "../ShuttleGui.h"
#include "../widgets/AudacityMessageBox.h"
#include "../widgets/valnum.h"
#include "CommandContext.h"

const ComponentInterfaceSymbol CompareAudioCommand::Symbol
{ XO("Compare Audio") };

namespace{ BuiltinCommandsModule::Registration< CompareAudioCommand > reg; }

extern void RegisterCompareAudio( Registrar & R){
   R.AddCommand( std::make_unique<CompareAudioCommand>() );
// std::unique_ptr<CommandOutputTargets> &&target
//   return std::make_shared<CompareAudioCommand>(*this, std::move(target));

}

bool CompareAudioCommand::DefineParams( ShuttleParams & S ){
   S.Define( errorThreshold,  wxT("Threshold"),   0.0f,  0.0f,    0.01f,    1.0f );
   return true;
}

void CompareAudioCommand::PopulateOrExchange(ShuttleGui & S)
{
   S.AddSpace(0, 5);

   S.StartMultiColumn(2, wxALIGN_CENTER);
   {
      S.TieTextBox(XXO("Threshold:"),errorThreshold);
   }
   S.EndMultiColumn();
}

// Update member variables with project selection data (and validate)
bool CompareAudioCommand::GetSelection(const CommandContext &context, AudacityProject &proj)
{
   // Get the selected time interval
   auto &selectedRegion = ViewInfo::Get( proj ).selectedRegion;
   mT0 = selectedRegion.t0();
   mT1 = selectedRegion.t1();
   if (mT0 >= mT1)
   {
      context.Error(wxT("There is no selection!"));
      return false;
   }

   // Get the selected tracks and check that there are at least two to
   // compare
   auto trackRange = TrackList::Get( proj ).Selected< const WaveTrack >();
   mTrack0 = *trackRange.first;
   if (mTrack0 == NULL)
   {
      context.Error(wxT("No tracks selected! Select two tracks to compare."));
      return false;
   }
   mTrack1 = * ++ trackRange.first;
   if (mTrack1 == NULL)
   {
      context.Error(wxT("Only one track selected! Select two tracks to compare."));
      return false;
   }
   if ( * ++ trackRange.first )
   {
      context.Status(wxT("More than two tracks selected - only the first two will be compared."));
   }
   return true;
}

double CompareAudioCommand::CompareSample(double value1, double value2)
{
   return fabs(value1 - value2);
}

inline int min(int a, int b)
{
   return (a < b) ? a : b;
}

bool CompareAudioCommand::Apply(const CommandContext & context)
{
   if (!GetSelection(context, context.project))
   {
      return false;
   }

   wxString msg = wxT("Comparing tracks '");
   msg += mTrack0->GetName() + wxT("' and '")
      + mTrack1->GetName() + wxT("'.");
   context.Status(msg);

   long errorCount = 0;
   // Initialize buffers for track data to be analyzed
   auto buffSize = std::min(mTrack0->GetMaxBlockSize(), mTrack1->GetMaxBlockSize());

   Floats buff0{ buffSize };
   Floats buff1{ buffSize };

   // Compare tracks block by block
   auto s0 = mTrack0->TimeToLongSamples(mT0);
   auto s1 = mTrack0->TimeToLongSamples(mT1);
   auto position = s0;
   auto length = s1 - s0;
   while (position < s1)
   {
      // Get a block of data into the buffers
      auto block = limitSampleBufferSize(
         mTrack0->GetBestBlockSize(position), s1 - position
      );
      mTrack0->GetFloats(buff0.get(), position, block);
      mTrack1->GetFloats(buff1.get(), position, block);

      for (decltype(block) buffPos = 0; buffPos < block; ++buffPos)
      {
         if (CompareSample(buff0[buffPos], buff1[buffPos]) > errorThreshold)
         {
            ++errorCount;
         }
      }

      position += block;
      context.Progress(
         (position - s0).as_double() /
         length.as_double()
      );
   }

   // Output the results
   double errorSeconds = mTrack0->LongSamplesToTime(errorCount);
   context.Status(wxString::Format(wxT("%li"), errorCount));
   context.Status(wxString::Format(wxT("%.4f"), errorSeconds));
   context.Status(wxString::Format(wxT("Finished comparison: %li samples (%.3f seconds) exceeded the error threshold of %f."), errorCount, errorSeconds, errorThreshold));
   return true;
}

