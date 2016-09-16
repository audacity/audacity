/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2009 Audacity Team
   License: wxwidgets

   Dan Horgan

******************************************************************//**

\file CompareAudioCommand.cpp
\brief Contains definitions for CompareAudioCommand class

\class CompareAudioCommand
\brief Returns information about the amount of audio that is about a certain
threshold of difference in two selected tracks

*//*******************************************************************/

#include "CompareAudioCommand.h"
#include "../Project.h"
#include "Command.h"
#include "../WaveTrack.h"

wxString CompareAudioCommandType::BuildName()
{
   return wxT("CompareAudio");
}

void CompareAudioCommandType::BuildSignature(CommandSignature &signature)
{
   auto thresholdValidator = make_movable<DoubleValidator>();
   signature.AddParameter(wxT("Threshold"), 0.0, std::move(thresholdValidator));
}

CommandHolder CompareAudioCommandType::Create(std::unique_ptr<CommandOutputTarget> &&target)
{
   return std::make_shared<CompareAudioCommand>(*this, std::move(target));
}

// Update member variables with project selection data (and validate)
bool CompareAudioCommand::GetSelection(AudacityProject &proj)
{
   // Get the selected time interval
   mT0 = proj.mViewInfo.selectedRegion.t0();
   mT1 = proj.mViewInfo.selectedRegion.t1();
   if (mT0 >= mT1)
   {
      Error(wxT("There is no selection!"));
      return false;
   }

   // Get the selected tracks and check that there are at least two to
   // compare
   SelectedTrackListOfKindIterator iter(Track::Wave, proj.GetTracks());
   mTrack0 = (WaveTrack*)(iter.First());
   if (mTrack0 == NULL)
   {
      Error(wxT("No tracks selected! Select two tracks to compare."));
      return false;
   }
   mTrack1 = (WaveTrack*)(iter.Next());
   if (mTrack1 == NULL)
   {
      Error(wxT("Only one track selected! Select two tracks to compare."));
      return false;
   }
   if (iter.Next() != NULL)
   {
      Status(wxT("More than two tracks selected - only the first two will be compared."));
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

bool CompareAudioCommand::Apply(CommandExecutionContext context)
{
   if (!GetSelection(*context.GetProject()))
   {
      return false;
   }

   wxString msg = wxT("Comparing tracks '");
   msg += mTrack0->GetName() + wxT("' and '")
      + mTrack1->GetName() + wxT("'.");
   Status(msg);

   long errorCount = 0;
   double errorThreshold = GetDouble(wxT("Threshold"));

   // Initialize buffers for track data to be analyzed
   auto buffSize = std::min(mTrack0->GetMaxBlockSize(), mTrack1->GetMaxBlockSize());
   float *buff0 = new float[buffSize];
   float *buff1 = new float[buffSize];

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
      mTrack0->Get((samplePtr)buff0, floatSample, position, block);
      mTrack1->Get((samplePtr)buff1, floatSample, position, block);

      for (decltype(block) buffPos = 0; buffPos < block; ++buffPos)
      {
         if (CompareSample(buff0[buffPos], buff1[buffPos]) > errorThreshold)
         {
            ++errorCount;
         }
      }

      position += block;
      Progress(
         (position - s0).as_double() /
         length.as_double()
      );
   }

   delete [] buff0;
   delete [] buff1;

   // Output the results
   double errorSeconds = mTrack0->LongSamplesToTime(errorCount);
   Status(wxString::Format(wxT("%li"), errorCount));
   Status(wxString::Format(wxT("%.4f"), errorSeconds));
   Status(wxString::Format(wxT("Finished comparison: %li samples (%.3f seconds) exceeded the error threshold of %f."), errorCount, errorSeconds, errorThreshold));
   return true;
}
