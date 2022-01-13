/**********************************************************************

   Audacity: A Digital Audio Editor
   Audacity(R) is copyright (c) 1999-2021 Audacity Team.

   DeepModel.h
   Hugo Flores Garcia

******************************************************************/
/**

\file DeepModel.h
\brief Implements a class for handling torchscript deep learning models.

*/
/*******************************************************************/

#pragma once

#include "ModelCard.h"
#include "wx/log.h"

#include <tuple>

#include <torch/script.h>
#include <torch/torch.h>
#include "AudacityException.h"
#include "Identifier.h"

class DeepModel;

using ModulePtr = std::unique_ptr<torch::jit::script::Module>;
using DeepModelHolder = std::shared_ptr<DeepModel>;
using TensorWithTimestamps = std::tuple<torch::Tensor, torch::Tensor>;

class ModelException final : public MessageBoxException
{
public:
   ModelException(const TranslatableString msg, std::string trace) :
                  m_msg(std::move(msg)),
                  m_trace(std::move(trace)),
                  MessageBoxException{
                     ExceptionType::Internal,
                     XO("Deep Model Error")
                  }
   { 
      if (!m_trace.empty()) 
         wxLogError(wxString(m_trace)); 
   }

   //! internal message
   virtual const char* what() const noexcept
      { return m_msg.Translation().c_str(); }

   //! user facing message
   virtual TranslatableString ErrorMessage() const
      { return XO("Deep Model Error: %s").Format(m_msg);}
   
   const TranslatableString m_msg;
   const std::string m_trace;
};

class DeepModel final
{
public:
   // TODO: maybe we want to support a couple of search paths?
   //! returns the base directory for deep learning models
   static FilePath DLModelsDir();

   //! returns the base directory for built in deep learning models
   static FilePath BuiltInModulesDir();

   DeepModel() = default;
   DeepModel(ModelCard &card);

   //! loads a torchscript model from file. 
   //! @excsafety: strong
   void Load(const std::string &modelPath);

   //! loads a torchscript model from a stream of bytes in memory.
   //! @excsafety: strong
   void Load(std::istream &bytes);

   //! checks if a model is loaded onto memory. 
   bool IsLoaded() const;

   //! releases the model file, but keeps the card
   void Offload();

   //! saves the currently loaded model to disk. 
   void Save(const std::string &modelPath) const;

   //! Sets the model's associated model card (for metadata). 
   void SetCard(ModelCardHolder card);

   //! Get the model's metadata card. Use the ModelCard to access metadata attributes in the model's metadata json file. 
   ModelCardHolder GetCard() const { return mCard; };

   //! resample a waveform tensor. Waveform should be shape (channels, samples)
   torch::Tensor Resample(const torch::Tensor &waveform, int sampleRateIn, int sampleRateOut) const;

   //! forward pass throught the model. Waveform should be shape (channels, samples)
   torch::jit::IValue Forward(const torch::Tensor &waveform) const;
   
   //! converts a torch::jit::IValue to a torch::Tensor
   torch::Tensor ToTensor(const torch::jit::IValue &output) const;

   //! converts a torch::jit::IValue to a TensorWithTimestamps, for labeler models. 
   TensorWithTimestamps ToTimestamps(const torch::jit::IValue &output) const;

private:
   ModulePtr mModel;
   ModulePtr mResampler;

   ModelCardHolder mCard;

   bool mLoaded {false};

private:
   void LoadResampler();
};

