/**********************************************************************
 
 Audacity: A Digital Audio Editor
 
 @file LabelLayout.h
 
 Paul Licameli split from LabelTrack.h
 
 **********************************************************************/
#ifndef __AUDACITY_LABEL_LAYOUT__
#define __AUDACITY_LABEL_LAYOUT__

#include "LabelTrack.h"

struct LabelLayout
   : public LabelStructAttachment
{
   //! Allow mutative access to attached data of a const label
   static LabelLayout &Get(const LabelStruct &labelStruct);

   ~LabelLayout() override;
   std::unique_ptr<LabelStructAttachment> Clone() const override;

   // Returns true iff the label got inverted:
   static bool AdjustEdge(LabelStruct &label, int iEdge, double fNewTime);
   static void MoveLabel(LabelStruct &label, int iEdge, double fNewTime);

   // Working storage for on-screen layout.
   int width{}; /// width of the text in pixels.
   int x{};     /// Pixel position of left hand glyph
   int x1{};    /// Pixel position of right hand glyph
   int xText{}; /// Pixel position of left hand side of text box
   int y{};     /// Pixel position of label.

   bool updated{ false }; /// flag to tell if the label times were updated
};

#endif
