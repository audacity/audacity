/**********************************************************************
   Audacity: A Digital Audio Editor
   Audacity(R) is copyright (c) 1999-2021 Audacity Team.

   DeepLearningAnalyzer.cpp
   Aldo Aguilar
   Hugo Flores Garcia
******************************************************************/
/*******************************************************************/

#include "DeepLearningAnalyzer.h"

#include <cstddef>
#include <string>
#include <vector>
#include <wx/stattext.h>

#include "FileNames.h"
#include "Shuttle.h"
#include "ShuttleGui.h"
#include "WaveTrack.h"
#include "LabelTrack.h"

#include "effects/LoadEffects.h"

#include <torch/script.h>

// DeepLearningEffectBase implementation

std::string DeepLearningAnalyzer::GetDeepEffectID()
{ return "waveform-to-labels";}

const ComponentInterfaceSymbol DeepLearningAnalyzer::Symbol
{ XO("Deep Learning Analyzer") };

// register audio Labeler
namespace { BuiltinEffectsModule::Registration<DeepLearningAnalyzer> reg; }

DeepLearningAnalyzer::DeepLearningAnalyzer() 
{ 
   SetLinearEffectFlag(false); 
}

DeepLearningAnalyzer::~DeepLearningAnalyzer() 
{
}


// ComponentInterface implementation

ComponentInterfaceSymbol DeepLearningAnalyzer::GetSymbol() { return Symbol; }

TranslatableString DeepLearningAnalyzer::GetDescription() 
{
  return XO("The auto labeler uses deep learning models to "
         "annotate audio tracks based on their contents automatically."); 
}

ManualPageID DeepLearningAnalyzer::ManualPage() 
{
  return L"Deep_Learning_Analyzer"; 
}

// EffectDefinitionInterface implementation

EffectType DeepLearningAnalyzer::GetType() { return EffectTypeAnalyze; }

// Effect implementation

// ProcessOne() takes a track, transforms it to bunch of buffer-blocks,
// performs a forward pass through the deep model, and writes
// the output to new tracks.
bool DeepLearningAnalyzer::ProcessOne(WaveTrack *leader, double tStart, double tEnd) 
{
   const auto &pModel = mActiveModel->GetModel();
   // get current models labels
   const std::vector<std::string> &classList = pModel->GetCard()->labels();

   if (leader == NULL)
      return false;

   // i8n-hint: This will be the name of the new label track, which will contain annotations for the input audio track. 
   wxString labelTrackName(XO("%s - labels").Format(leader->GetName()).Translation());
   std::shared_ptr<AddedAnalysisTrack> labelTrack =
      AddAnalysisTrack(labelTrackName);

   sampleFormat origFmt = leader->GetSampleFormat();
   int origRate = leader->GetRate();

   // Initiate processing buffer, most likely shorter than
   // the length of the selection being processed.
   Floats buffer{leader->GetMaxBlockSize()};

   // get each of the blocks we will process
   for (BlockIndex block : GetBlockIndices(leader, tStart, tEnd)) 
   {
      // Get a blockSize of samples (smaller than the size of the buffer)
      sampleCount samplePos = block.first;
      size_t blockSize = block.second;

      // get a torch tensor from the leader track
      torch::Tensor input =
         BuildMultichannelTensor(leader, buffer.get(), samplePos, blockSize);

      // if we're not doing a multichannel forward pass, downmix
      if (!pModel->GetCard()->multichannel())
         input = input.sum(0, true, torch::kFloat);

      // resample!
      input = pModel->Resample(input, origRate, pModel->GetCard()->sample_rate());

      // forward pass!
      torch::jit::IValue output = ForwardPassInThread(input);

      // split forward pass output into output tensor and timestamps
      auto [modelOutput, timestamps] = pModel->ToTimestamps(output);

      // write the block's label to the label track
      double blockStart = leader->LongSamplesToTime(samplePos);
      sampleCount blockEndSamples = samplePos + blockSize;
      double blockEnd = leader->LongSamplesToTime(blockEndSamples);

      TensorToLabelTrack(modelOutput, *labelTrack, blockStart, blockEnd, timestamps, classList);

      // Update the Progress meter
      double tPos = leader->LongSamplesToTime(samplePos);
      if (TrackProgress(mCurrentTrackNum, (tPos - tStart) / (tEnd - tStart)))
      return false;
   }
   labelTrack->Commit();
   return true;
}

std::vector<Stamp> Coalesce(std::vector<Stamp> &timestamps)
{
   if (timestamps.size() == 0)
      return timestamps;

   // sort the stamps by start time
   std::function< bool(Stamp &a, Stamp &b)> compareByStartTime (
      [](Stamp &a, Stamp &b)
      {
         return a.first < b.first;
      }
   );

   std::sort(timestamps.begin(), timestamps.end(), compareByStartTime);
   std::vector<Stamp> coalescedStamps; 

   Stamp prev = timestamps[0];
   for (const auto &stamp : timestamps)
   {
      if (stamp.first <= prev.second)
      {
         if (stamp.second > prev.second)
         {
            prev.second = stamp.second;
         }
      }
      else
      {
         coalescedStamps.emplace_back(prev);
         prev = Stamp(stamp);
      }
   }

   // check if last is not equal to end of last stamp
   // append the last stamp
   if (!coalescedStamps.empty())
   {
      if (coalescedStamps.back().second != prev.second)
      {
         coalescedStamps.emplace_back(prev);
      }
   }
   else if (coalescedStamps.empty() && !timestamps.empty())
   {
      coalescedStamps.emplace_back(prev);
   }

   return coalescedStamps;
}

void DeepLearningAnalyzer::TensorToLabelTrack
(torch::Tensor output, AddedAnalysisTrack &labelTrack,
   double tStart, double tEnd, torch::Tensor timestamps, 
   const std::vector<std::string> &classList) 
{
   // add the start offset to the timestamps
   timestamps += tStart;

   // check that the output is one dimensional
   wxASSERT(output.dim() == 1);

   // create a map of labels to timestamps
   std::map<wxString, std::vector<Stamp> > timestampsMap;

   for (int i = 0 ; i < output.size(0); i++)
   {
      torch::Tensor classIndex = output[i];
      wxString label = classList[classIndex.item().to<int>()];

      double t1 = timestamps[i][0].item().to<double>();
      double t2 = timestamps[i][1].item().to<double>();

      // discard any bad stamps (where t1 > t2)
      if (t1 > t2)
         continue;

      timestampsMap[label].emplace_back(t1, t2);
   }
   
   for (const auto &label : classList)
   {
      // coalesce all labels for the class
      timestampsMap[label] = Coalesce(timestampsMap[label]);

      for (const auto &stamp : timestampsMap[label])
      {
         // write the timestamp to the label track
         SelectedRegion region(stamp.first, stamp.second);

         labelTrack.get()->AddLabel(region, label);
      }
   }
} 
