#include "DeepModel.h"

#include <torch/script.h>
#include <torch/torch.h>

#include <rapidjson/document.h>

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
      // TODO: load model metadata from a json string.
      std::string data = extraFilesMap_["metadata.json"];

      // parse the data
      rapidjson::Document document;
      document.Parse(data.c_str());
      if (document.Parse(data.c_str()).HasParseError()) 
         throw std::exception(); // TODO: throw a better exception

      assert(document.IsObject());
      // // document.SetObject();

      // //tmp: print the doc
      // auto a = document.MemberBegin();
      // auto b = document.MemberEnd();

      // for (rapidjson::Value::MemberIterator M = document.MemberBegin(); 
      //       M!=document.MemberEnd(); M++)
      // {
      //    const char* key   = M->name.GetString();
      //    const char* value = M->value.GetString();

      //    if (key!=NULL && value!=NULL)
      //    {
      //       printf("%s = %s", key,value);
      //    }
      // }

      // set the sample rate
      printf("has sample_rate = %d\n", document.HasMember("sample_rate"));
      assert(document.HasMember("sample_rate"));
      assert(document["sample_rate"].IsInt());
      mSampleRate = document["sample_rate"].GetInt();

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

torch::Tensor DeepModel::Forward(const torch::Tensor tensorInput)
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
      return tensorInput;
   }
}