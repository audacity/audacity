/**********************************************************************

  Audacity: A Digital Audio Editor

  EffectWidgetList.h

  Matthieu Hodgkinson

  Generic interface to describe an effect, list its parameters and
  give access to them.

**********************************************************************/
#pragma once

#include "EffectWidget.h"
#include <optional>

/*!
 * Widgets whose index is in the range [start, end) should be grouped together
 * in the UI. The UI should display the widgets in the order of the spans.
 * For an example of where this might be used, see the BassTreble effect
 * wxWidget UI.
 */
struct EffectWidgetGroup
{
   std::string name;
   int start = 0;
   int end = 0;
};

class EFFECTS_API EffectWidgetList
{
public:
   virtual ~EffectWidgetList();

   virtual int GetWidgetCount() const = 0;

   virtual std::optional<EffectWidget> GetWidget(int index) const = 0;

   /*!
    * Returned spans are sorted by start, and obey the rules that
    * - `start` >= 0
    * - `end` <= GetWidgetCount()
    * - spans[i].end <= spans[i+1].start
    */
   virtual std::vector<EffectWidgetGroup> GetWidgetGroups() const = 0;

   /*!
    * Updating this widget may cause other widgets to be updated. Client should
    * therefore update representation of all widgets after this call.
    */
   virtual void SetWidget(int index, const EffectWidget& widget) = 0;
};
