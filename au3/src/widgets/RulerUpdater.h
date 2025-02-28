/**********************************************************************

  Audacity: A Digital Audio Editor

  RulerUpdater.h

  Dominic Mazzoni
  Michael Papadopoulos split from Ruler.h

**********************************************************************/

#ifndef __AUDACITY_UPDATER__
#define __AUDACITY_UPDATER__

#include "ViewInfo.h" // for children
#include "Envelope.h"
#include "NumberScale.h" // member variable
#include "RulerFormat.h" // member variable

#include <wx/font.h>
#include <optional>

class wxDC;
class wxColor;

struct RulerStruct {
    struct Fonts {
        wxFont major, minor, minorMinor;
        int lead;
    };

    double mMin{ 0.0 };
    double mHiddenMin{ 0.0 };
    double mMax{ 100.0 };
    double mHiddenMax{ 100.0 };

    int mOrientation{ wxHORIZONTAL };
    bool mFlip{ false };
    bool mLabelEdges{ false };

    const RulerFormat* mpRulerFormat{};

    int mLeft{ -1 };
    int mTop{ -1 };
    int mRight{ -1 };
    int mBottom{ -1 };
    int mLength{ 0 };

    double mDbMirrorValue{ 0.0 };

    // Found on demand
    mutable std::unique_ptr<Fonts> mpFonts;
    TranslatableString mUnits;

    NumberScale mNumberScale;
};

class RulerUpdater
{
public:
    struct Label {
        double value;
        int pos;
        int lx, ly;

        // Minorminor tick draws only when optional is nonempty
        // Major or minor tick draws regardless
        std::optional<TranslatableString> text;

        TranslatableString units;

        void Draw(wxDC& dc, bool twoTone, wxColour c, std::unique_ptr<RulerStruct::Fonts>& fonts) const;
    };
    using Labels = std::vector<Label>;

    using Bits = std::vector< bool >;

    struct UpdateOutputs {
        Labels& majorLabels, & minorLabels, & minorMinorLabels;
        Bits& bits;
        wxRect& box;
    };

    RulerUpdater() {}
    virtual ~RulerUpdater() = 0;

    virtual void Update(
        wxDC& dc, const Envelope* envelope, UpdateOutputs& allOutputs, const RulerStruct& context)// Envelope *speedEnv, long minSpeed, long maxSpeed )
    const = 0;

protected:
    struct TickOutputs {
        Labels& labels;
        Bits& bits;
        wxRect& box;
    };

    struct TickSizes
    {
        RulerFormat:: TickType tickType = RulerFormat::t_major;

        double mMajor = 0;
        double mMinor = 0;
        double mMinorMinor = 0;
        double mUnits = 0;

        int mDigits;

        TickSizes(
            double UPP, int orientation, const RulerFormat* format, bool log);

        TranslatableString LabelString(double d, const RulerFormat* format) const;
    };

    static std::pair< wxRect, Label > MakeTick(
        RulerUpdater::Label lab, wxDC& dc, wxFont font, std::vector<bool>& bits, int left, int top, int spacing, int lead, bool flip,
        int orientation);

    void BoxAdjust(
        UpdateOutputs& allOutputs, const RulerStruct& context) const;
};

#endif //define __AUDACITY_UPDATER__
