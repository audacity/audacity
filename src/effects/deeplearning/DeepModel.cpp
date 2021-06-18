#include "DeepModel.h"

#include <torch/script.h>
#include <torch/torch.h>

#include <rapidjson/document.h>
#include "../../WaveTrack.h"
#include "../../WaveClip.h"

// DeepModel Implementation

DeepModel::DeepModel() : mLoaded(false) {}

bool DeepModel::Load(const std::string &modelPath)
{
   try
   {
      // create a placeholder for our metadata string
      torch::jit::ExtraFilesMap extraFilesMap_;
      std::pair<std::string, std::string> metadata("metadata.json", "");
      extraFilesMap_.insert(metadata);

      // load the model to CPU, as well as the metadata
      mModel = torch::jit::load(modelPath, torch::kCPU,  extraFilesMap_);
      mModel.eval();

      // load the model metadata
      std::string data = extraFilesMap_["metadata.json"];

      // parse the data
      mMetadata.Parse(data.c_str());
      if (mMetadata.Parse(data.c_str()).HasParseError()) 
         throw std::exception(); // TODO: throw a better exception

      assert(mMetadata.IsObject());

      // set the sample rate
      assert(mMetadata.HasMember("sample_rate"));
      assert(mMetadata["sample_rate"].IsInt());
      mSampleRate = mMetadata["sample_rate"].GetInt();

      mLoaded = true;
   }
   catch (const std::exception &e)
   {
      // TODO: how to make this an audacity exception
      std::cerr << e.what() << '\n';
      mLoaded = false;
   }

   return mLoaded;
}

rapidjson::Document DeepModel::GetMetadata()
{
   rapidjson::Document::AllocatorType& allocator = mMetadata.GetAllocator();

   rapidjson::Document copy;
   copy.CopyFrom(mMetadata, allocator);

   if (!copy.IsObject()) copy.SetObject();

   return copy;
}

std::string DeepModel::QueryMetadata(const char *key)
{
   std::string output;

   if (!mMetadata.IsObject())
   {
      output = "None";
   }
   // get the value as a string type
   else if (mMetadata.HasMember(key))
   {
      rapidjson::StringBuffer sBuffer;
      rapidjson::Writer<rapidjson::StringBuffer> writer(sBuffer);

      mMetadata[key].Accept(writer);
      output = sBuffer.GetString();
   }
   else
   {
      output = "None";
   }
   return std::string(output);
}

std::vector<std::string> DeepModel::GetLabels()
{
   assert(mMetadata.HasMember("n_src"));
   assert(mMetadata["n_src"].GetInt() == mMetadata["labels"].Size());

   // iterate through the labels and collect
   std::vector<std::string> labels;
   for (rapidjson::Value::ConstValueIterator itr = mMetadata["labels"].Begin(); 
                                             itr != mMetadata["labels"].End();
                                             ++itr)
   {
      labels.emplace_back(itr->GetString());
   }

   return labels;
}

// forward pass through the model!
torch::Tensor DeepModel::Forward(const torch::Tensor &tensorInput)
{
   torch::NoGradGuard no_grad;
   if (mLoaded)
   {
      // TODO: check input sizes here and throw and exception
      // if the audio is not the correct dimensions

      // set up for jit model
      std::vector<torch::jit::IValue> inputs = {tensorInput};

      // forward pass!
      auto tensorOutput = mModel.forward(inputs).toTensor();

      // move tensor output to CPU
      tensorOutput = tensorOutput;

      // make tensor contiguous to return to track
      tensorOutput = tensorOutput.contiguous();

      return tensorOutput;
   }
   else 
   {
      // TODO: maybe this should return a bool indicating success?
      throw std::exception();
   }
}