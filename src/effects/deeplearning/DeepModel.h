/**********************************************************************

   Audacity: A Digital Audio Editor
   Audacity(R) is copyright (c) 1999-2021 Audacity Team.
   License: GPL v2.  See License.txt.

   DeepModel.h
   Hugo Flores Garcia

******************************************************************/
/**

\class DeepModel
\brief base class for handling torchscript models

\class SeparationModel
\brief source separation deep models

TODO: add a more thorough description

*/
/*******************************************************************/

#pragma once

#include <torch/script.h>
#include <torch/torch.h>

#include <rapidjson/document.h>
#include <rapidjson/schema.h>
#include "rapidjson/prettywriter.h" 

using TensorWithTimestamps = std::tuple<torch::Tensor, torch::Tensor>;

class DeepModel
{
private:
   torch::jit::script::Module mModel;
   torch::jit::script::Module mResampler;
   bool mLoaded;

   rapidjson::Document mMetadata;

   int mSampleRate;

   FilePath ModelsDir(); 

public:
   DeepModel();
   bool Load(const std::string &modelPath);
   bool IsLoaded(){ return mLoaded; };

   rapidjson::Document GetMetadata();
   
   // \brief queries the metadata dictionary, 
   // will convert any JSON type to a non-prettified string
   // if the key does not exist, returns "None"
   // useful for when we want to display certain 
   // model metadata to the user
   std::string QueryMetadata(const char *key);
   std::vector<std::string> GetLabels();

   int GetSampleRate(){return mSampleRate;}

   torch::Tensor Resample(const torch::Tensor &waveform, int sampleRateIn, int sampleRateOut);
   torch::jit::IValue Forward(const torch::Tensor &waveform);
   torch::Tensor ToTensor(const torch::jit::IValue &output);
   TensorWithTimestamps ToTimestamps(const torch::jit::IValue &output);
};

