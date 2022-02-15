/**********************************************************************

  Audacity: A Digital Audio Editor

  @file ParameterValueQueue.h

  @author Vitaly Sverchinsky

  @brief Part of Audacity VST3 module

**********************************************************************/

#pragma once

#include <vector>
#include <pluginterfaces/vst/ivstparameterchanges.h>

namespace internal {

   //! IParamValueQueue implementation. Holds automation points
   //! sorted in time. Can be initialized with any parameter id,
   //! and disposed later to avoid reallocations
   class ParameterValueQueue final : public Steinberg::Vst::IParamValueQueue
   {
      struct Point
      {
         Steinberg::int32 sampleOffset;
         Steinberg::Vst::ParamValue paramValue;

         bool operator <(const Point& other) const;
      };

      bool mInitialized { false };
      Steinberg::Vst::ParamID mId;
      std::vector<Point> mPoints;

   public:

      ParameterValueQueue();
      virtual ~ParameterValueQueue();

      void initialize(Steinberg::Vst::ParamID id);

      bool isInitialized() const noexcept;

      void dispose() noexcept;

      Steinberg::Vst::ParamID PLUGIN_API getParameterId() override;

      Steinberg::int32 PLUGIN_API getPointCount() override;

      Steinberg::tresult PLUGIN_API getPoint(Steinberg::int32 index, Steinberg::int32& sampleOffset,
         Steinberg::Vst::ParamValue& value) override;

      Steinberg::tresult PLUGIN_API addPoint(Steinberg::int32 sampleOffset, Steinberg::Vst::ParamValue value,
         Steinberg::int32& index) override;

      DECLARE_FUNKNOWN_METHODS
   };

}
