/**********************************************************************

  Audacity: A Digital Audio Editor

  ImageRoll.cpp

  Dominic Mazzoni

*******************************************************************//**

\class ImageRoll
\brief
  An ImageRoll is an image that can be expanded to an arbitrary size;
  it is made up of both fixed pieces and repeating pieces.


  A typical
  ImageRoll might be made up of two fixed ends and a repeating
  middle part:

\verbatim
  /-----\                       /-----\
  |LEFT |--REPEATING--REPEATING-|RIGHT|
  \-----/                       \-----/
\endverbatim

  As you resize the image, it could look like this:

\verbatim
  /-----\ /-----\
  |LEFT |-|RIGHT|
  \-----/ \-----/
\endverbatim

  Or like this:

\verbatim
  /-----\                                  /-----\
  |LEFT |--REPEATING--REPEATING--REPEATING-|RIGHT|
  \-----/                                  \-----/
\endverbatim

  Note that an ImageRoll can have a center piece; in fact, its pieces
  always alternate fixed, repeating, fixed, repeating, etc. - although
  one of these pieces is allowed to be of size zero, making it skipped.
  Here's an example with a center piece:

\verbatim
  /-----\                /------\                /-----\
  |LEFT |-REPEAT--REPEAT-|CENTER|-repeat--repeat-|RIGHT|
  \-----/                \------/                \-----/
\endverbatim

  Note that the left and right repeating sections can be different.
  Of course, an ImageRoll can be oriented vertically as well.
  In the future, support for an ImageRoll that expands both horizontally
  and vertically at the same time will be supported.

  An ImageRoll is initialized with a _single_ wxImage that defines
  all of its pieces.  This is done by way of a "magic color" which
  separates each piece in the image.  If the magic colored pixel is
  denoted by "X", the above ImageRoll could be encoded like this:

\verbatim
  /-----\X        X/------\X        X/-----\
  |LEFT |X-REPEAT-X|CENTER|X-repeat-X|RIGHT|
  \-----/X        X\------/X        X\-----/
\endverbatim

  Putting two lines of magic color in a row will create a blank
  piece.  For example, for an ImageRoll with a center piece but no
  left and right pieces:

\verbatim
  X        X/------\X        X
  X-REPEAT-X|CENTER|X-repeat-X
  X        X\------/X        X
\endverbatim

  Once again, the pieces are always assumed to alternate: fixed,
  repeating, fixed, repeating, etc.  The magic color is specified
  when you construct the ImageRoll from a wxImage.

  In the constructor, you also choose whether it is a horizontal or
  vertical ImageRoll (and later a "Frame" as well).  You can also
  choose a "fixed" ImageRoll, which behaves just like a wxImage -
  this is handy so that you can use ImageRolls everywhere you were
  previously using wxImages.

*//****************************************************************//**

\class ImageRollPanel
\brief A wxPanel which displays an ImageRoll.

*//*******************************************************************/

#include "ImageRoll.h"

#include <wx/bitmap.h>
#include <wx/dcmemory.h>
#include <wx/dcclient.h>

ImageRoll::ImageRoll(const ImageRoll&) = default;
ImageRoll& ImageRoll::operator =(const ImageRoll&) = default;
ImageRoll::~ImageRoll() = default;

// static
ImageArray ImageRoll::SplitH(const wxImage& src, wxColour magicColor)
{
    ImageArray result;

    int width = src.GetWidth();
    int height = src.GetHeight();
    unsigned char* data = src.GetData();
    unsigned char* ptr = data;
    unsigned char magicRed = magicColor.Red();
    unsigned char magicGreen = magicColor.Green();
    unsigned char magicBlue = magicColor.Blue();
    bool cur, prev;
    int i, j, start;

    // Sanity check...
    if (width <= 0 || height <= 0 || data == NULL) {
        return result;
    }

    prev = false;
    start = 0;
    for (i=0; i < width + 1; i++) {
        if (i < width) {
            unsigned char* ptr2 = ptr;
            cur = true;
            for (j=0; j < height && cur; j++) {
                if (!(ptr2[0] == magicRed
                      && ptr2[1] == magicGreen
                      && ptr2[2] == magicBlue)) {
                    cur = false;
                }
                ptr2 += 3 * width;
            }
        } else {
            cur = !prev;
        }

        if ((cur && !prev)) {
            wxRect subRect(start, 0, i - start, height);
            wxImage subImage;
            if (subRect.width > 0) {
                subImage = src.GetSubImage(subRect);
            } else {
                subImage = wxImage(subRect.width, subRect.height);
            }
            result.push_back(subImage);
        } else if (!cur && prev) {
            start = i;
        }

        prev = cur;
        ptr += 3;
    }

    return result;
}

// static
ImageArray ImageRoll::SplitV(const wxImage& src, wxColour magicColor)
{
    ImageArray result;
    int width = src.GetWidth();
    int height = src.GetHeight();
    unsigned char* data = src.GetData();
    unsigned char* ptr = data;
    unsigned char magicRed = magicColor.Red();
    unsigned char magicGreen = magicColor.Green();
    unsigned char magicBlue = magicColor.Blue();
    bool cur, prev;
    int i, j, start;

    // Sanity check...
    if (width <= 0 || height <= 0 || data == NULL) {
        return result;
    }

    prev = false;
    start = 0;
    for (i=0; i < height + 1; i++) {
        if (i < height) {
            unsigned char* ptr2 = ptr;
            cur = true;
            for (j=0; j < width && cur; j++) {
                if (!(ptr2[0] == magicRed
                      && ptr2[1] == magicGreen
                      && ptr2[2] == magicBlue)) {
                    cur = false;
                }
                ptr2 += 3;
            }
        } else {
            cur = !prev;
        }

        if ((cur && !prev)) {
            wxRect subRect(0, start, width, i - start);
            wxImage subImage;
            if (subRect.width > 0) {
                subImage = src.GetSubImage(subRect);
            } else {
                subImage = wxImage(subRect.width, subRect.height);
            }
            result.push_back(subImage);
        } else if (!cur && prev) {
            start = i;
        }

        prev = cur;
        ptr += 3 * width;
    }

    return result;
}

void ImageRoll::Init(RollType type, const wxImage& src, wxColour magicColor)
{
    ImageArray images;
    int i;

    mType = type;

    switch (mType) {
    case HorizontalRoll:
        images = SplitH(src, magicColor);

        mMinSize.x = 0;
        mMinSize.y = src.GetHeight();
        mMaxSize.x = 9999;
        mMaxSize.y = src.GetHeight();

        for (i = 0; i < (int)images.size(); i++) {
            if (images[i].Ok()) {
                mPieces.push_back(wxBitmap(images[i]));
                mMinSize.x += mPieces[i].GetWidth();
            } else {
                mPieces.push_back(wxBitmap());
            }
        }
        break;

    case VerticalRoll:
        images = SplitV(src, magicColor);

        mMinSize.x = src.GetWidth();
        mMinSize.y = 0;
        mMaxSize.x = src.GetWidth();
        mMaxSize.y = 9999;

        for (i = 0; i < (int)images.size(); i++) {
            if (images[i].Ok()) {
                mPieces.push_back(wxBitmap(images[i]));
                mMinSize.y += mPieces[i].GetHeight();
            } else {
                mPieces.push_back(wxBitmap());
            }
        }
        break;

    case FixedImage:
        mPieces.push_back(wxBitmap(src));
        mMinSize.x = src.GetWidth();
        mMinSize.y = src.GetHeight();
        mMaxSize.x = src.GetWidth();
        mMaxSize.y = src.GetHeight();
        break;

    /* Adding these shuts up some GCC warnings. It is functionally what was
     * implicit here before - Richard */
    case Uninitialized:
        break;

    case Frame:
        break;
    } // switch
}

ImageRoll::ImageRoll()
{
    mType = Uninitialized;
}

ImageRoll::ImageRoll(RollType type, const wxImage& src, wxColour magicColor)
{
    Init(type, src, magicColor);
}

ImageRoll::ImageRoll(const wxImage& src)
{
    Init(FixedImage, src, *wxWHITE);
}

bool ImageRoll::Ok() const
{
    return mType != Uninitialized;
}

void ImageRoll::DrawBitmap(wxDC& dc, wxBitmap& bitmap,
                           int x, int y, int logicalFunc)
{
    auto func = static_cast< wxRasterOperationMode >(logicalFunc);
    if (func == wxCOPY) {
        dc.DrawBitmap(bitmap, x, y);
    } else {
        wxMemoryDC memDC;
        memDC.SelectObject(bitmap);
        dc.Blit(x, y, bitmap.GetWidth(), bitmap.GetHeight(),
                &memDC, 0, 0, func);
    }
}

void ImageRoll::Draw(wxDC& dc, wxRect rect)
{
    Draw(dc, rect, wxCOPY);
}

void ImageRoll::Draw(wxDC& dc, wxRect rect, int WXUNUSED(logicalFunc))
{
    auto func = wxCOPY;
    int width = rect.width;
    int height = rect.height;
    int num = (int)mPieces.size();
    int i, j;

    switch (mType) {
    case HorizontalRoll: {
        // The pieces alternate fixed, rolling, fixed, rolling...

        int fixedWidth = 0;
        for (i=0; i < num; i+=2) {
            fixedWidth += (mPieces[i].Ok() ? mPieces[i].GetWidth() : 0);
        }

        int rollingSpace = width - fixedWidth;
        int numRolling = num / 2;
        int x = 0;

        for (i=0; i < num; i++) {
            int w = (mPieces[i].Ok() ? mPieces[i].GetWidth() : 0);

            if (i % 2 == 0) {
                // fixed

                if (mPieces[i].Ok()) {
                    DrawBitmap(dc, mPieces[i], rect.x + x, rect.y, func);
                }
                x += w;
            } else {
                // rolling

                int space
                    =((1 + (i / 2)) * rollingSpace / numRolling)
                      - ((i / 2) * rollingSpace / numRolling);

                j = 0;
                while (j < space) {
                    if (mPieces[i].Ok()) {
                        DrawBitmap(dc, mPieces[i], rect.x + x + j, rect.y, func);
                    }
                    j += w;
                }

                x += space;
            }
        }
    } break; // case HorizontalRoll

    case VerticalRoll: {
        // The pieces alternate fixed, rolling, fixed, rolling...

        int fixedHeight = 0;
        for (i=0; i < num; i+=2) {
            fixedHeight += (mPieces[i].Ok() ? mPieces[i].GetHeight() : 0);
        }

        int rollingSpace = height - fixedHeight;
        int numRolling = num / 2;
        int y = 0;

        for (i=0; i < num; i++) {
            int h = (mPieces[i].Ok() ? mPieces[i].GetHeight() : 0);

            if (i % 2 == 0) {
                // fixed

                if (mPieces[i].Ok()) {
                    DrawBitmap(dc, mPieces[i], rect.x, rect.y + y, func);
                }
                y += h;
            } else {
                // rolling

                int space
                    =((1 + (i / 2)) * rollingSpace / numRolling)
                      - ((i / 2) * rollingSpace / numRolling);

                j = 0;
                while (j < space) {
                    if (mPieces[i].Ok()) {
                        DrawBitmap(dc, mPieces[i], rect.x, rect.y + y + j, func);
                    }
                    j += h;
                }

                y += space;
            }
        }
    } break; // case VerticalRoll

    case FixedImage:
        DrawBitmap(dc, mPieces[0], rect.x, rect.y, func);
        break;
    /* the other possible cases don't really make sense, but not having them
     * listed gives a GCC warning */
    case Uninitialized:
        break;

    case Frame:
        break;
    } // switch
}
