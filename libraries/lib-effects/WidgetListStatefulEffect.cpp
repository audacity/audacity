/**********************************************************************

   Audacity: A Digital Audio Editor

   WidgetListStatefulEffect.cpp

   Matthieu Hodgkinson

**********************************************************************/
#include "WidgetListStatefulEffect.h"

WidgetListStatefulEffect::Instance::Instance(
   WidgetListStatefulEffect& processor)
    : StatefulEffect::Instance { processor }
    , mWidgetList { processor }
{
}

EffectWidgetList* WidgetListStatefulEffect::Instance::GetWidgetList()
{
   return &mWidgetList;
}

std::shared_ptr<EffectInstance> WidgetListStatefulEffect::MakeInstance() const
{
   return std::make_shared<Instance>(
      const_cast<WidgetListStatefulEffect&>(*this));
}
