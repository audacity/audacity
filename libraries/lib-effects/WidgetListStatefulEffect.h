/**********************************************************************

   Audacity: A Digital Audio Editor

   WidgetListStatefulEffect.h

   Matthieu Hodgkinson

**********************************************************************/
#pragma once

#include "EffectWidgetList.h"
#include "StatefulEffect.h"

/*!
 * A convenience class to be inherited by effect classes that implement
 * `EffectWidgetList` and `StatefulEffect`.
 */
class EFFECTS_API WidgetListStatefulEffect :
    public StatefulEffect,
    public EffectWidgetList
{
public:
   class EFFECTS_API Instance : public StatefulEffect::Instance
   {
   public:
      Instance(WidgetListStatefulEffect& processor);

   private:
      EffectWidgetList* GetWidgetList() override;
      EffectWidgetList& mWidgetList;
   };

   std::shared_ptr<EffectInstance> MakeInstance() const override;
};
