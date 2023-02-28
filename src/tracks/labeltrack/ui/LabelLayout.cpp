/**********************************************************************
 
 Audacity: A Digital Audio Editor
 
 @file LabelLayout.cpp
 
 Paul Licameli split from LabelTrack.h
 
 **********************************************************************/
#include "LabelLayout.h"

static LabelStructAttachments::RegisteredFactory key{
   [](LabelStruct&){ return std::make_unique<LabelLayout>(); }
};

LabelLayout &LabelLayout::Get(const LabelStruct &labelStruct)
{
   auto &mut = const_cast<LabelStruct&>(labelStruct);
   return static_cast<LabelLayout&>(mut.Attachments::Get<LabelLayout>(key));
}

LabelLayout::~LabelLayout() = default;

std::unique_ptr<LabelStructAttachment> LabelLayout::Clone() const
{
   return std::make_unique<LabelLayout>(*this);
}

// Adjust label's left or right boundary, depending which is requested.
// Return true iff the label flipped.
bool LabelLayout::AdjustEdge(LabelStruct &label, int iEdge, double fNewTime)
{
   auto &layout = Get(label);
   layout.updated = true;
   if (iEdge < 0)
      return label.selectedRegion.setT0(fNewTime);
   else
      return label.selectedRegion.setT1(fNewTime);
}

// We're moving the label.  Adjust both left and right edge.
void LabelLayout::MoveLabel(LabelStruct &label, int iEdge, double fNewTime)
{
   auto &layout = Get(label);
   double fTimeSpan = label.getDuration();

   if (iEdge < 0)
      label.selectedRegion.setTimes(fNewTime, fNewTime + fTimeSpan);
   else
      label.selectedRegion.setTimes(fNewTime - fTimeSpan, fNewTime);

   layout.updated = true;
}
