/**********************************************************************

  Audacity: A Digital Audio Editor

  EqualizationCurves.h

  Mitch Golden
  Vaughan Johnson (Preview)

  Paul Licameli split from Equalization.h

***********************************************************************/
#ifndef __AUDACITY_EFFECT_EQUALIZATION_CURVES__
#define __AUDACITY_EFFECT_EQUALIZATION_CURVES__

#include "XMLTagHandler.h"

#include <vector>
class TranslatableString;

//! One point in a curve
class BUILTIN_EFFECTS_API EQPoint
{
public:
    EQPoint(const double f, const double d) { Freq = f; dB = d; }

    bool operator <(const EQPoint& p1) const
    {
        return Freq < p1.Freq;
    }

    double Freq;
    double dB;
};

//! One curve in a list
class BUILTIN_EFFECTS_API EQCurve
{
public:
    EQCurve(const wxString& name = {}) { Name = name; }
    EQCurve(const wxChar* name) { Name = name; }

    bool operator <(const EQCurve& that) const
    {
        return Name.CmpNoCase(that.Name) < 0;
    }

    wxString Name;
    std::vector<EQPoint> points;
};

using EQCurveArray = std::vector<EQCurve>;

//! Serializer of curves into XML files
class BUILTIN_EFFECTS_API EQCurveWriter
{
public:
    explicit EQCurveWriter(const EQCurveArray& curves)
        : mCurves{curves} {}
    void SaveCurves(const wxString& fileName = {});

private:
    void WriteXML(XMLWriter& xmlFile) const;
    const EQCurveArray& mCurves;
};

//! Deserializer of curves from XML files
class BUILTIN_EFFECTS_API EQCurveReader : public XMLTagHandler
{
public:
    EQCurveReader(
        EQCurveArray& curves, const TranslatableString& name, int options)
        : mCurves{curves}, mName{name}, mOptions{options} {}

    // XMLTagHandler callback methods for loading and saving
    bool HandleXMLTag(const std::string_view& tag, const AttributesList& attrs) override;
    XMLTagHandler* HandleXMLChild(const std::string_view& tag) override;

    void LoadCurves(const wxString& fileName = {}, bool append = false);

private:
    bool GetDefaultFileName(wxFileName& fileName);
    wxString GetPrefsPrefix();
    // Merge NEW curves only or update all factory presets.
    // Uses EQCurveWriter
    void UpdateDefaultCurves(bool updateAll = false);
    EQCurveArray& mCurves;
    const TranslatableString mName;
    const int mOptions;
};

#endif
