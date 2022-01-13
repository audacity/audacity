/**********************************************************************

   Audacity: A Digital Audio Editor
   Audacity(R) is copyright (c) 1999-2021 Audacity Team.

   DeepModel.cpp
   Hugo Flores Garcia

******************************************************************/

#include "DeepModel.h"

#include <torch/script.h>
#include <torch/torch.h>

#include "FileNames.h"
#include "WaveTrack.h"
#include "WaveClip.h"

#include "CodeConversions.h"

// DeepModel Implementation

FilePath DeepModel::DLModelsDir()
{
   return FileNames::MkDir( wxFileName( FileNames::DataDir(), wxT("deeplearning-models") ).GetFullPath() );
}

FilePath DeepModel::BuiltInModulesDir()
{
   return FileNames::MkDir( wxFileName( FileNames::BaseDir(), wxT("deeplearning-models") ).GetFullPath() );
}

void DeepModel::LoadResampler()
{
   std::unique_ptr<torch::jit::script::Module> tmp;

   // load the resampler module
   std::string resamplerPath = audacity::ToUTF8(wxFileName(BuiltInModulesDir(), wxT("resampler.pt"))
                                       .GetFullPath());
   try
   {
      tmp = std::make_unique<torch::jit::script::Module>
                     (torch::jit::load(resamplerPath, torch::kCPU));
      tmp->eval();
   }
   catch (const std::exception &e)
   {
      throw ModelException(XO("An error occurred while loading the resampler"), e.what());
   }
   
   mResampler = std::move(tmp);
}

void DeepModel::Load(const std::string &modelPath)
{
   std::unique_ptr<torch::jit::script::Module> tmp;
   try
   { 
      // set mResampler
      LoadResampler();

      // place in a temporary model, 
      // to preserve object state in case either of these throw. 
      tmp = std::make_unique<torch::jit::script::Module>
                  (torch::jit::load(modelPath, torch::kCPU));
      tmp->eval();

   }
   catch (const std::exception &e)
   {
      throw ModelException(XO("An error occurred while loading model"), e.what());
   }

   // now, move to mModel
   mModel = std::move(tmp);
   
   // finally, mark as loaded
   mLoaded = true;
}

void DeepModel::Load(std::istream &bytes)
{
   std::unique_ptr<torch::jit::script::Module> tmp;
   try
   {
      // load the resampler module
      LoadResampler();

      // place in a temporary model, 
      // to preserve object state in case either of these throw. 
      tmp = std::make_unique<torch::jit::script::Module>
                  (torch::jit::load(bytes, torch::kCPU));
      tmp->eval();
   }
   catch (const std::exception &e)
   {
      throw ModelException(XO("Error while loading model"), e.what());
   }

   // now, move to mModel
   mModel = std::move(tmp);
   
   // finally, mark as loaded
   mLoaded = true;
}

void DeepModel::Offload()
{
   mModel.reset();
   mLoaded = false;
}

bool DeepModel::IsLoaded() const
{
   return mLoaded; 
}

void DeepModel::Save(const std::string &modelPath) const
{
   if (!mLoaded)
      throw ModelException(XO("attempted save when no module was loaded."), "");

   mModel->save(modelPath);
}

void DeepModel::SetCard(ModelCardHolder card)
{
   // set the card
   mCard = std::move(card);
}

torch::Tensor DeepModel::Resample(const torch::Tensor &waveform, int sampleRateIn, 
                                  int sampleRateOut) const
{
   if (!mLoaded) 
      throw ModelException(XO("Attempted resample while is not loaded."
                                       " Please call Load() first."), ""); 

   // early exit if the sample rates are the same
   if (sampleRateIn == sampleRateOut)
      return waveform;

   // set up inputs
   // torchaudio likes that sample rates are cast to float, for some reason.
   std::vector<torch::jit::IValue> inputs = {waveform, 
                                             static_cast<float>(sampleRateIn), 
                                             static_cast<float>(sampleRateOut)};

   try
   {
      return mResampler->forward(inputs).toTensor();
   }
   catch (const std::exception &e)
   {
      throw ModelException(XO("A libtorch error occured while resampling."), e.what());
   }
}

// forward pass through the model!
torch::jit::IValue DeepModel::Forward(const torch::Tensor &waveform) const
{
   // NoGradGuard prevents the model from storing gradients, which should
   // make computation faster and memory usage lower. 
   torch::NoGradGuard NoGrad;

   if (!mLoaded) 
      throw ModelException(XO("Attempted forward pass while model is not loaded."
                                       " Please call Load() first."), ""); 

   // set up for jit model
   std::vector<torch::jit::IValue> inputs = {waveform};

   // forward pass!
   try
   {
      return  mModel->forward(inputs);
   }
   catch (const std::exception &e)
   {
      throw ModelException(XO("A libtorch error occurred during the forward pass"), e.what());
   }
}

torch::Tensor DeepModel::ToTensor(const torch::jit::IValue &output) const
{
   return output.toTensor().contiguous();
}

TensorWithTimestamps DeepModel::ToTimestamps(const torch::jit::IValue &output) const
{
   try
   {
      auto tupleOutput = output.toTuple();

      torch::Tensor modelOutput = tupleOutput->elements()[0].toTensor();
      torch::Tensor timestamps = tupleOutput->elements()[1].toTensor();

      return TensorWithTimestamps(modelOutput, timestamps);
   }
   catch (const std::exception &e)
   {
      throw ModelException(XO("A libtorch error occurred while converting the model "
                              "output to a tensor with timestamps."), e.what());
   }
}
