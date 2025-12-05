/**********************************************************************

  Audacity: A Digital Audio Editor

  ImageManipulation.cpp

  Dominic Mazzoni
  James Crook

  wxWidgets license (Dominic to confirm).

********************************************************************//*!

\file ImageManipulation.cpp

Provides Image Manipulation functions.

wxWidgets misses some important functions involving cutting and
pasting bitmaps, and (in version 2.6.1) is patchy in support of alpha
channel.  This collection of functions fills that gap.

*//*********************************************************************/

#include "ImageManipulation.h"

#include <wx/colour.h>
#include <wx/image.h>

#include "AllThemeResources.h"
#include "Theme.h"

/// This looks at the first pixel in the image, and shifts
/// the entire image by the vector difference between that
/// pixel and the dstColour.  For better control, use
/// ChangeImageColour(wxImage, wxColour*, wxColour*) below
std::unique_ptr<wxImage> ChangeImageColour(wxImage* srcImage, wxColour& dstColour)
{
    unsigned char* src = srcImage->GetData();
    wxColour c;
    c.Set(src[0], src[1], src[2]);
    return ChangeImageColour(srcImage, c, dstColour);
}

///This will explicitly shift the image color from
///srcColour to dstColour.
std::unique_ptr<wxImage> ChangeImageColour(wxImage* srcImage,
                                           wxColour& srcColour,
                                           wxColour& dstColour)
{
    // This function takes a source image, which it assumes to
    // be grayscale, and smoothly changes the overall color
    // to the specified color, and returns the result as a
    // NEW image.  This works well for grayscale 3D images.
    // Audacity uses this routines to make the buttons
    // (skip-start, play, stop, record, skip-end) adapt to
    // the color scheme of the user.

    unsigned char* src = srcImage->GetData();
    int width = srcImage->GetWidth();
    int height = srcImage->GetHeight();

    auto dstImage = std::make_unique<wxImage>(width, height);
    unsigned char* dst = dstImage->GetData();

    //Get the source color
    int srcVal[3], srcOpp[3];
    srcVal[0] = srcColour.Red();
    srcVal[1] = srcColour.Green();
    srcVal[2] = srcColour.Blue();

    int dstVal[3], dstOpp[3];
    dstVal[0] = dstColour.Red();
    dstVal[1] = dstColour.Green();
    dstVal[2] = dstColour.Blue();

    int i;
    for (i = 0; i < 3; i++) {
        srcOpp[i] = 256 - srcVal[i]; // avoid zero!
        dstOpp[i] = 255 - dstVal[i];
    }

    int c = 0;
    for (i = 0; i < width * height * 3; i++) {
        int s = (int)*src;

        if (s >= srcVal[c]) {
            *dst++ = dstVal[c] + dstOpp[c] * (s - srcVal[c]) / srcOpp[c];
        } else {
            *dst++ = dstVal[c] * s / srcVal[c];
        }
        src++;
        c = (c + 1) % 3;
    }

    if (srcImage->HasAlpha()) {
        // Preserve transparencies
        dstImage->InitAlpha();
        memcpy(dstImage->GetAlpha(), srcImage->GetAlpha(), width * height);
    }

    return dstImage;
}

/// Takes a background image, foreground image, and mask
/// (i.e. the alpha channel for the foreground), and
/// returns a NEW image where the foreground has been
/// overlaid onto the background using alpha-blending,
/// at location (xoff, yoff).
std::unique_ptr<wxImage> OverlayImage(wxImage* background, wxImage* foreground,
                                      wxImage* mask, int xoff, int yoff)
{
    unsigned char* bg = background->GetData();
    unsigned char* fg = foreground->GetData();
    unsigned char* mk = mask->GetData();

    int bgWidth = background->GetWidth();
    int bgHeight = background->GetHeight();
    int fgWidth = foreground->GetWidth();
    int fgHeight = foreground->GetHeight();
    int mkWidth = mask->GetWidth();
    int mkHeight = mask->GetHeight();

    //Now, determine the dimensions of the images to be masked together
    //on top of the background.  This should be equal to the area of the
    //smaller of the foreground and the mask, as long as it is
    //within the area of the background, given the offset.

    //Make sure the foreground size is no bigger than the mask
    int wCutoff = (fgWidth < mkWidth) ? fgWidth : mkWidth;
    int hCutoff = (fgHeight < mkHeight) ? fgHeight : mkHeight;

    // If the masked foreground + offset is bigger than the background, masking
    // should only occur within these bounds of the foreground image
    wCutoff = (bgWidth - xoff > wCutoff) ? wCutoff : bgWidth - xoff;
    hCutoff = (bgHeight - yoff > hCutoff) ? hCutoff : bgHeight - yoff;

    //Make a NEW image the size of the background
    auto dstImage = std::make_unique<wxImage>(bgWidth, bgHeight);
    unsigned char* dst = dstImage->GetData();
    memcpy(dst, bg, bgWidth * bgHeight * 3);

    // Go through the foreground image bit by bit and mask it on to the
    // background, at an offset of xoff,yoff.
    // BUT...Don't go beyond the size of the background image,
    // the foreground image, or the mask
    int x, y;
    for (y = 0; y < hCutoff; y++) {
        unsigned char* bkp = bg + 3 * ((y + yoff) * bgWidth + xoff);
        unsigned char* dstp = dst + 3 * ((y + yoff) * bgWidth + xoff);

        for (x = 0; x < wCutoff; x++) {
            int value = mk[3 * (y * mkWidth + x)];
            int opp = 255 - value;

            for (int c = 0; c < 3; c++) {
                dstp[x * 3 + c]
                    =((bkp[x * 3 + c] * opp)
                      + (fg[3 * (y * fgWidth + x) + c] * value)) / 255;
            }
        }
    }
    return dstImage;
}

/// Takes a background image, foreground image, and mask
/// (i.e. the alpha channel for the foreground), and
/// returns a NEW image where the foreground has been
/// overlaid onto the background using alpha-blending,
/// at location (xoff, yoff).
std::unique_ptr<wxImage> OverlayImage(teBmps eBack, teBmps eForeground,
                                      int xoff, int yoff)
{
    wxImage imgBack(theTheme.Image(eBack));
    wxImage imgFore(theTheme.Image(eForeground));

    // TMP: dmazzoni - just so the code runs even though not all of
    // our images have transparency...
    if (!imgFore.HasAlpha()) {
        return std::make_unique<wxImage>(imgBack);
    }

    wxASSERT(imgFore.HasAlpha());

    unsigned char* bg = imgBack.GetData();
    unsigned char* fg = imgFore.GetData();
    unsigned char* mk = imgFore.GetAlpha();

    int bgWidth = imgBack.GetWidth();
    int bgHeight = imgBack.GetHeight();
    int fgWidth = imgFore.GetWidth();
    int fgHeight = imgFore.GetHeight();

    //Now, determine the dimensions of the images to be masked together
    //on top of the background.  This should be equal to the area of the
    //smaller of the foreground and the mask, as long as it is
    //within the area of the background, given the offset.

    //Make sure the foreground size is no bigger than the mask
    int wCutoff = fgWidth;
    int hCutoff = fgHeight;

    // If the masked foreground + offset is bigger than the background, masking
    // should only occur within these bounds of the foreground image
    wCutoff = (bgWidth - xoff > wCutoff) ? wCutoff : bgWidth - xoff;
    hCutoff = (bgHeight - yoff > hCutoff) ? hCutoff : bgHeight - yoff;

    //Make a NEW image the size of the background
    auto dstImage = std::make_unique<wxImage>(bgWidth, bgHeight);
    unsigned char* dst = dstImage->GetData();
    memcpy(dst, bg, bgWidth * bgHeight * 3);

    // If background image has tranparency, then we want to blend with the
    // current background colour.
    if (imgBack.HasAlpha()) {
        unsigned char* pAlpha = imgBack.GetAlpha();
        wxColour c = theTheme.Colour(clrMedium);
        unsigned char onePixImage[3];
        // GetData() guarantees RGB order [wxWidgets does the ocnversion]
        onePixImage[ 0 ] = c.Red();
        onePixImage[ 1 ] = c.Green();
        onePixImage[ 2 ] = c.Blue();
        for ( int i=0; i < bgWidth * bgHeight; i++) {
            unsigned char* pPix = &dst[ 3 * i];
            float alpha = 1.0 - (pAlpha[i] / 255.0);
            pPix[0] = pPix[0] + alpha * ((int)onePixImage[0] - (int)pPix[0]);
            pPix[1] = pPix[1] + alpha * ((int)onePixImage[1] - (int)pPix[1]);
            pPix[2] = pPix[2] + alpha * ((int)onePixImage[2] - (int)pPix[2]);
        }
    }

    // Go through the foreground image bit by bit and mask it on to the
    // background, at an offset of xoff,yoff.
    // BUT...Don't go beyond the size of the background image,
    // the foreground image, or the mask
    int x, y;
    for (y = 0; y < hCutoff; y++) {
        unsigned char* bkp = bg + 3 * ((y + yoff) * bgWidth + xoff);
        unsigned char* dstp = dst + 3 * ((y + yoff) * bgWidth + xoff);

        for (x = 0; x < wCutoff; x++) {
            int value = mk[(y * fgWidth + x)];// Don't multiply by 3...
            int opp = 255 - value;

            for (int c = 0; c < 3; c++) {
                dstp[x * 3 + c]
                    =((bkp[x * 3 + c] * opp)
                      + (fg[3 * (y * fgWidth + x) + c] * value)) / 255;
            }
        }
    }
    return dstImage;
}

// Creates an image with a solid background color
std::unique_ptr<wxImage> CreateBackground(int width, int height, wxColour colour)
{
    auto i = std::make_unique<wxImage>(width, height);
    unsigned char* ip;
    int srcVal[3];
    int x;

    srcVal[0] = colour.Red();
    srcVal[1] = colour.Green();
    srcVal[2] = colour.Blue();

    ip = i->GetData();
    for (x=0; x < width * height; x++) {
        *ip++ = srcVal[0];
        *ip++ = srcVal[1];
        *ip++ = srcVal[2];
    }

    return i;
}

std::unique_ptr<wxImage> CreateSysBackground
    (int width, int height, int WXUNUSED(offset), wxColour colour)
{
    return CreateBackground(width, height, colour);
}

/// Pastes one image into another including the alpha channel.
/// Differs from OverlayImage in that:
///   Happens in place to existing background image.
///   Pastes image on top; no blending with existing background is done.
void PasteSubImage(wxImage* background, wxImage* foreground, int xoff, int yoff)
{
    unsigned char* bg = background->GetData();
    unsigned char* fg = foreground->GetData();
    unsigned char* bgAlpha = background->HasAlpha() ? background->GetAlpha() : NULL;
    unsigned char* fgAlpha = foreground->HasAlpha() ? foreground->GetAlpha() : NULL;
    // For testing... Set as if no alpha in foreground....
    // fgAlpha = NULL;

    int bgWidth = background->GetWidth();
    int bgHeight = background->GetHeight();
    int fgWidth = foreground->GetWidth();
    int fgHeight = foreground->GetHeight();

    int wCutoff = fgWidth;
    int hCutoff = fgHeight;

    // If the masked foreground + offset is bigger than the background, masking
    // should only occur within these bounds of the foreground image
    wCutoff = (bgWidth - xoff > wCutoff) ? wCutoff : bgWidth - xoff;
    hCutoff = (bgHeight - yoff > hCutoff) ? hCutoff : bgHeight - yoff;

    // Go through the foreground image bit by bit and place it on to the
    // background, at an offset of xoff,yoff.
    // Don't go beyond the size of the background image,
    // or the foreground image.
    int y;
    unsigned char* bkp;
    unsigned char* fgp;
    unsigned char* bgAlphap;
    unsigned char* fgAlphap;
    for (y = 0; y < hCutoff; y++) {
        // RGB bytes
        bkp = bg + 3 * ((y + yoff) * bgWidth + xoff);
        fgp = fg + 3 * (y * fgWidth);
        memcpy(bkp, fgp, 3 * wCutoff);

        // Alpha bytes.
        if (bgAlpha) {
            bgAlphap = bgAlpha + ((y + yoff) * bgWidth + xoff);
            if (fgAlpha) {
                fgAlphap = fgAlpha + (y * fgWidth);
                memcpy(bgAlphap, fgAlphap, wCutoff);
            } else {
                memset(bgAlphap, 255, wCutoff);
            }
        }
    }
}

/// Gets a rectangle from within another image, INCLUDING the alpha channel
/// \bug in wxWidgets, wxImage::GetSubImage should do this itself.
wxImage GetSubImageWithAlpha(const wxImage& Src,  const wxRect& rect)
{
    //First part of this code is lifted from wxImage::GetSubImage() source code.
    wxImage image;

    wxCHECK_MSG(Src.Ok(), image, wxT("invalid image"));

    wxCHECK_MSG((rect.GetLeft() >= 0) && (rect.GetTop() >= 0) && (
                    rect.GetRight() <= Src.GetWidth()) && (rect.GetBottom() <= Src.GetHeight()),
                image, wxT("invalid subimage size"));

    int subwidth=rect.GetWidth();
    const int subheight=rect.GetHeight();

    image.Create(subwidth, subheight, false);

    unsigned char* subdata = image.GetData(), * data=Src.GetData();

    wxCHECK_MSG(subdata, image, wxT("unable to create image"));

    // JKC: Quick hack - don't deal with masks - need to understand macro M_IMGDATA first.
//   if (Src.M_IMGDATA->m_hasMask)
//      image.SetMaskColour( Src.M_IMGDATA->m_maskRed, Src.M_IMGDATA->m_maskGreen, Src.M_IMGDATA->m_maskBlue );

    int subleft=3 * rect.GetLeft();
    int width=3 * Src.GetWidth();
    subwidth*=3;

    data+=rect.GetTop() * width + subleft;

    for (long j = 0; j < subheight; ++j) {
        memcpy(subdata, data, subwidth);
        subdata+=subwidth;
        data+=width;
    }

    image.InitAlpha();
    if (!Src.HasAlpha()) {
        return image;
    }
    // OK, so we've copied the RGB data.
    // Now do the Alpha channel.
    //wxASSERT( Src.HasAlpha() );

    subleft/=3;
    width/=3;
    subwidth/=3;

    data =Src.GetAlpha();
    subdata =image.GetAlpha();

    data+=rect.GetTop() * width + subleft;

    for (long j = 0; j < subheight; ++j) {
        memcpy(subdata, data, subwidth);
        subdata+=subwidth;
        data+=width;
    }
    return image;
}
