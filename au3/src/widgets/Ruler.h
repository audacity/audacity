/**********************************************************************

  Audacity: A Digital Audio Editor

  Ruler.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_RULER__
#define __AUDACITY_RULER__

#include "RulerUpdater.h" // member variable
#include "NumberScale.h" // member variable

#include "Observer.h"

#include <wx/colour.h> // member variable
#include <wx/font.h> // member variable
#include <wx/pen.h> // member variable

class wxDC;

class Envelope;
class RulerFormat;

#include "TranslatableString.h"

struct RulerInvalidatedMessage {};

class AUDACITY_DLL_API Ruler : public Observer::Publisher<RulerInvalidatedMessage>
{
public:

    struct TickLengths {
        int majorLength, minorLength, minorMinorLength;
    };

    //
    // Constructor / Destructor
    //

    Ruler(const RulerUpdater& updater, const RulerFormat& format);
    ~Ruler();

    //
    // Required Ruler Parameters
    //

    void SetBounds(int left, int top, int right, int bottom);

    // wxHORIZONTAL || wxVERTICAL
    void SetOrientation(int orient);

    // min is the value at (x, y)
    // max is the value at (x+width, y+height)
    // (at the center of the pixel, in both cases)
    void SetRange(double min, double max);

    // An overload needed for the special case of fisheye
    // min is the value at (x, y)
    // max is the value at (x+width, y+height)
    // hiddenMin, hiddenMax are the values that would be shown without the fisheye.
    // (at the center of the pixel, in both cases)
    void SetRange(double min, double max, double hiddenMin, double hiddenMax);

    // Set the kind of updater the ruler will use (Linear, Logarithmic, Custom, etc.)
    // Invalidates the ruler.  However ruler doesn't invalidate automatically
    // if the updater's state is mutated elsewhere.
    void SetUpdater(const RulerUpdater* pUpdater);

    //
    // Optional Ruler Parameters
    //

    // If twoTone is true, cause zero and positive numbers to appear black, negative in another color.
    void SetTwoTone(bool twoTone);

    // Set the format to be used for generating tick labels and tick spacing for some updaters
    // Invalidates the ruler.  However ruler doesn't invalidate automatically
    // if the formatter's state is mutated elsewhere.
    void SetFormat(const RulerFormat* pFormat);

    // Specify the name of the units (like "dB") if you
    // want numbers like "1.6" formatted as "1.6 dB".
    void SetUnits(const TranslatableString& units);
    void SetDbMirrorValue(const double d);

    // If this is true, the edges of the ruler will always
    // receive a label.  If not, the nearest round number is
    // labeled (which may or may not be the edge).
    void SetLabelEdges(bool labelEdges);

    // Makes a vertical ruler hug the left side (instead of right)
    // and a horizontal ruler hug the top (instead of bottom)
    void SetFlip(bool flip);

    // Set it to false if you don't want minor labels.
    void SetMinor(bool value);

    // Good defaults are provided, but you can override here
    void SetFonts(const wxFont& minorFont, const wxFont& majorFont, const wxFont& minorMinorFont);
    RulerStruct::Fonts GetFonts() const;

    void SetNumberScale(const NumberScale& scale);

    void SetTickLengths(const TickLengths& tLengths);

    // The ruler will not draw text within this (pixel) range.
    // Use this if you have another graphic object obscuring part
    // of the ruler's area.  The values start and end are interpreted
    // relative to the Ruler's local coordinates.
    void OfflimitsPixels(int start, int end);

    //
    // Calculates and returns the maximum size required by the ruler
    //
    void GetMaxSize(wxCoord* width, wxCoord* height);

    //
    // Drawing
    //

    // Note that it will not erase for you...
    void Draw(wxDC& dc) const;
    void Draw(wxDC& dc, const Envelope* envelope) const;
    // If length <> 0, draws lines perpendiculars to ruler corresponding
    // to selected ticks (major, minor, or both), in an adjacent window.
    // You may need to use the offsets if you are using part of the dc for rulers, borders etc.
    void DrawGrid(wxDC& dc, int length, bool minor = true, bool major = true, int xOffset = 0, int yOffset = 0) const;

    // So we can have white ticks on black...
    void SetTickColour(const wxColour& colour)
    { mTickColour = colour; mPen.SetColour(colour); }

    // Force regeneration of labels at next draw time
    void Invalidate();

private:

    void ChooseFonts(wxDC& dc) const;

    void UpdateCache(wxDC& dc, const Envelope* envelope) const;

public:
    bool mbTicksOnly; // true => no line the length of the ruler
    bool mbTicksAtExtremes;

private:

    RulerStruct mRulerStruct;
    TickLengths mTickLengths;

    wxColour mTickColour;

    wxPen mPen;

    std::unique_ptr<RulerStruct::Fonts> mpUserFonts;

    const RulerUpdater* mpUpdater{};

    RulerUpdater::Bits mUserBits;

    struct Cache;
    mutable std::unique_ptr<Cache> mpCache;

    // Returns 'zero' label coordinate (for grid drawing)
    int FindZero(const RulerUpdater::Labels& labels) const;

    int GetZeroPosition() const;

    bool mbMinor;
    bool mTwoTone;
};

#endif //define __AUDACITY_RULER__
