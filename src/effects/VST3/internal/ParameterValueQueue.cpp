/**********************************************************************

  Audacity: A Digital Audio Editor

  @file ParameterValueQueue.cpp

  @author Vitaly Sverchinsky

  @brief Part of Audacity VST3 module

**********************************************************************/

#include "ParameterValueQueue.h"

#include <wx/debug.h>

bool internal::ParameterValueQueue::Point::operator<(const Point& other) const
{
   return sampleOffset < other.sampleOffset;
}

internal::ParameterValueQueue::ParameterValueQueue()
{
   FUNKNOWN_CTOR
}

internal::ParameterValueQueue::~ParameterValueQueue()
{
   wxASSERT(!mInitialized);
   FUNKNOWN_DTOR;
}

void internal::ParameterValueQueue::initialize(Steinberg::Vst::ParamID id)
{
   wxASSERT(!mInitialized);//misuse

   mInitialized = true;
   mId = id;
}

bool internal::ParameterValueQueue::isInitialized() const noexcept
{
   return mInitialized;
}

void internal::ParameterValueQueue::dispose() noexcept
{
   mInitialized = false;
   mPoints.clear();
}

Steinberg::Vst::ParamID internal::ParameterValueQueue::getParameterId()
{
   wxASSERT(mInitialized);
   return mId;
}

Steinberg::int32 internal::ParameterValueQueue::getPointCount()
{
   wxASSERT(mInitialized);
   return static_cast<int>(mPoints.size());
}

Steinberg::tresult internal::ParameterValueQueue::getPoint(Steinberg::int32 index, Steinberg::int32& sampleOffset,
   Steinberg::Vst::ParamValue& value)
{
   wxASSERT(mInitialized);
   if(index >= 0 && index < mPoints.size())
   {
      sampleOffset = mPoints[index].sampleOffset;
      value = mPoints[index].paramValue;
      return Steinberg::kResultOk;
   }
   return Steinberg::kInvalidArgument;
}

Steinberg::tresult internal::ParameterValueQueue::addPoint(Steinberg::int32 sampleOffset,
   Steinberg::Vst::ParamValue value, Steinberg::int32& index)
{
   //store points sorted by time value

   wxASSERT(mInitialized);
   
   auto newPoint = Point { sampleOffset, value };
   const auto it =
      std::lower_bound(mPoints.begin(), mPoints.end(), newPoint);
   
   if(it == mPoints.end())
   {
      index = mPoints.size();
      mPoints.push_back(std::move(newPoint));
   }
   else
   {
      if (it->sampleOffset == sampleOffset)
         it->paramValue = value;
      else
         mPoints.insert(it, std::move(newPoint));
      index = std::distance(mPoints.begin(), it);
   }  

   return Steinberg::kResultOk;
}

IMPLEMENT_FUNKNOWN_METHODS(internal::ParameterValueQueue, Steinberg::Vst::IParamValueQueue, Steinberg::Vst::IParamValueQueue::iid)
