#include "DeepModel.h"
#include <iostream>
#include <vector>
#include <stdlib.h>

#include <torch/script.h>
#include <torch/torch.h>

DeepModel::DeepModel() : mLoaded(false) {}

DeepModel::DeepModel(const std::string &modelPath)
{
   try
   {
      // load the model
      mModel = torch::jit::load(modelPath);
      mModel.eval();

      // load the model metadata
      // TODO: load model metadata from a yaml file.

      mLoaded = true;
   }
   catch (const std::exception &e)
   {
      // TODO: how to make this an audacity exception
      std::cerr << e.what() << '\n';
      mLoaded = false;
   }
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