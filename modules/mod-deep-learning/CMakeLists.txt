#[[
Deep-learning extension module for Audacity

Provides an API (DeepModel.h) for using deep learning models written in PyTorch 
(and compiled into libtorch), as well as an API for retrieving these models from
HuggingFace repositories. 


Implements two effects for processing with deep learning models: Deep Learning Effect and Deep Learning Analyzer.

Deep Learning Effect performs waveform to waveform processing, 
and is useful for audio-in-audio-out tasks (such as source separation, 
voice conversion, style transfer, amplifier emulation, etc.), 
while Deep Learning Analyzer performs waveform to labels processing,
and is useful for annotation tasks (such as sound event detection, 
musical instrument recognition, automatic speech recognition, etc.).
]]

set( SOURCES
   ActiveModel.cpp
   ActiveModel.h

   DeepModel.cpp
   DeepModel.h
   ModelCard.cpp
   ModelCard.h

   DeepModelManager.cpp
   DeepModelManager.h

   DeepLearningEffectBase.cpp
   DeepLearningEffectBase.h
   ModelManagerPanel.cpp
   ModelManagerPanel.h
   ModelCardPanel.cpp
   ModelCardPanel.h
   ExploreHuggingFaceDialog.cpp
   ExploreHuggingFaceDialog.h
      
   DeepLearningEffect.cpp 
   DeepLearningEffect.h
   DeepLearningAnalyzer.cpp
   DeepLearningAnalyzer.h
)
set( LIBRARIES
   Audacity
   ThreadPool::ThreadPool
)

audacity_module( mod-deep-learning "${SOURCES}" "${LIBRARIES}"
   "" "" )
