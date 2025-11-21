/**********************************************************************

Audacity: A Digital Audio Editor

SelectedRegion.cpp

Paul Licameli

*******************************************************************/

#include "SelectedRegion.h"

#include "XMLWriter.h"
#include "XMLAttributeValueView.h"

const char* SelectedRegion::sDefaultT0Name = "selStart";
const char* SelectedRegion::sDefaultT1Name = "selEnd";

namespace {
const char* sDefaultF0Name = "selLow";
const char* sDefaultF1Name = "selHigh";
}

void SelectedRegion::WriteXMLAttributes
    (XMLWriter& xmlFile,
    const char* legacyT0Name, const char* legacyT1Name) const
// may throw
{
    xmlFile.WriteAttr(legacyT0Name, t0(), 10);
    xmlFile.WriteAttr(legacyT1Name, t1(), 10);
    if (f0() >= 0) {
        xmlFile.WriteAttr(sDefaultF0Name, f0(), 10);
    }
    if (f1() >= 0) {
        xmlFile.WriteAttr(sDefaultF1Name, f1(), 10);
    }
}

bool SelectedRegion::HandleXMLAttribute
    (const std::string_view& attr, const XMLAttributeValueView& value,
    const char* legacyT0Name, const char* legacyT1Name)
{
    // Keep this function consistent with the table in the next!
    typedef bool (SelectedRegion::* Setter)(double, bool);
    Setter setter = 0;
    if (attr == legacyT0Name) {
        setter = &SelectedRegion::setT0;
    } else if (attr == legacyT1Name) {
        setter = &SelectedRegion::setT1;
    } else if (attr == sDefaultF0Name) {
        setter = &SelectedRegion::setF0;
    } else if (attr == sDefaultF1Name) {
        setter = &SelectedRegion::setF1;
    } else {
        return false;
    }

    double dblValue;

    if (!value.TryGet(dblValue)) {
        return false;
    }

    // False means don't flip time or frequency boundaries
    (void)(this->*setter)(dblValue, false);
    return true;
}

XMLMethodRegistryBase::Mutators<SelectedRegion>
SelectedRegion::Mutators(
    const char* legacyT0Name, const char* legacyT1Name)
{
    // Keep this table consistent with the previous function!
    return {
        { legacyT0Name, [=](auto& selectedRegion, auto value){
                selectedRegion
                .HandleXMLAttribute(legacyT0Name, value,
                                    legacyT0Name, legacyT1Name);
            } },
        { legacyT1Name, [=](auto& selectedRegion, auto value){
                selectedRegion
                .HandleXMLAttribute(legacyT1Name, value,
                                    legacyT0Name, legacyT1Name);
            } },
        { sDefaultF0Name, [=](auto& selectedRegion, auto value){
                selectedRegion
                .HandleXMLAttribute(sDefaultF0Name, value, "", "");
            } },
        { sDefaultF1Name, [=](auto& selectedRegion, auto value){
                selectedRegion
                .HandleXMLAttribute(sDefaultF1Name, value, "", "");
            } },
    };
}
