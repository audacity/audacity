#ifndef __AUDACITY_EFFECT_DEEPMODEL__
#define __AUDACITY_EFFECT_DEEPMODEL__

#include <torch/script.h>
#include <torch/torch.h>

#define kDEVICE torch::kCPU
#define kCPU torch::kCPU

class DeepModel
{
private:
   torch::jit::script::Module mModel;
   bool mLoaded;

   int mSampleRate;

public:
   DeepModel();
   bool Load(const std::string &modelPath);

   torch::Tensor Downmix(const torch::Tensor audio);
   torch::Tensor Preprocess(const torch::Tensor audio);

   torch::Tensor Forward(const torch::Tensor input);

   static torch::Tensor Track2Tensor();
};

#endif
