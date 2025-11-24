/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

 Audacity: A Digital Audio Editor

 @file RoundedStaticBitmap.h

 Dmitry Makarenko

 **********************************************************************/

#include "RoundedStaticBitmap.h"
#include <wx/image.h>


 wxImage RoundedImage(const wxImage& source, int radius) {
   wxImage image = source;

   if (!image.HasAlpha()) {
      image.InitAlpha();
   }

   const int w = image.GetWidth();
   const int h = image.GetHeight();
   const float feather = 1.5f;

   for (int y = 0; y < h; y++) {
      for (int x = 0; x < w; x++) {

         bool inCorner = false;
         float dx = 0, dy = 0;
         float alpha = 1.0f;

         // top left
         if (x < radius && y < radius) {
            dx = radius - x - 1;
            dy = radius - y - 1;
            inCorner = true;
         }
         // top right
         else if (x >= w - radius && y < radius) {
            dx = x - (w - radius);
            dy = radius - y - 1;
            inCorner = true;
         }
         // bottom left
         else if (x < radius && y >= h - radius) {
            dx = radius - x - 1;
            dy = y - (h - radius);
            inCorner = true;
         }
         // bottom right
         else if (x >= w - radius && y >= h - radius) {
            dx = x - (w - radius);
            dy = y - (h - radius);
            inCorner = true;
         }

         if (inCorner) {
            const float distance = std::hypot(dx, dy);
            if (distance > radius) {
               alpha = 0.0f;
            }
            else if (distance > radius - feather) {
               const float t = (radius - distance) / feather;
               alpha = t * t * (3.0f - 2.0f * t);
            }
         }

         const unsigned char currentAlpha = image.GetAlpha(x, y);
         const unsigned char newAlpha = static_cast<unsigned char>(wxIMAGE_ALPHA_OPAQUE * alpha);

         // do not increase the opacity
         if (currentAlpha > newAlpha) {
            image.SetAlpha(x, y, newAlpha);
         }
      }
   }
   return image;
}

RoundedStaticBitmap::RoundedStaticBitmap(wxWindow *parent,
                                         wxWindowID id,
                                         const wxBitmap& bitmap,
                                         int radius,
                                         const wxPoint& pos,
                                         const wxSize& size,
                                         long style,
                                         const wxString& name)
   : mRadius(radius),
     wxStaticBitmap(parent, id, bitmap, pos, size, style, name)
{
}

#if wxCHECK_VERSION(3, 1, 6)

void RoundedStaticBitmap::SetBitmap(const wxBitmapBundle& bitmap)
{
   mImage = bitmap.GetBitmapFor(this).ConvertToImage();
   auto roundedImage = RoundedImage(mImage, mRadius);
   wxStaticBitmap::SetBitmap(wxBitmapBundle::FromImage(roundedImage));
}

#else

void RoundedStaticBitmap::SetBitmap(const wxBitmap& bitmap)
{
   mImage = bitmap.ConvertToImage();
   auto roundedImage = RoundedImage(mImage, mRadius);
   wxStaticBitmap::SetBitmap(wxBitmap(roundedImage));
}

#endif

void RoundedStaticBitmap::SetImage(const wxImage& image)
{
   mImage = image;
   auto roundedImage = RoundedImage(mImage, mRadius);
   wxStaticBitmap::SetBitmap(wxBitmap(roundedImage));
}

void RoundedStaticBitmap::SetRadius(int radius)
{
   if (mRadius == radius)
      return;

   mRadius = radius;
   auto roundedImage = RoundedImage(mImage, mRadius);
   wxStaticBitmap::SetBitmap(wxBitmap(roundedImage));
}

int RoundedStaticBitmap::GetRadius() const
{
   return mRadius;
}

