/**********************************************************************

Audacity: A Digital Audio Editor

SelectedRegion.cpp

Paul Licameli

*******************************************************************/

#include "SelectedRegion.h"



#include "XMLWriter.h"

const wxChar *SelectedRegion::sDefaultT0Name = wxT("selStart");
const wxChar *SelectedRegion::sDefaultT1Name = wxT("selEnd");

namespace {
const wxChar *sDefaultF0Name = wxT("selLow");
const wxChar *sDefaultF1Name = wxT("selHigh");
}

void SelectedRegion::WriteXMLAttributes
(XMLWriter &xmlFile,
 const wxChar *legacyT0Name, const wxChar *legacyT1Name) const
// may throw
{
   xmlFile.WriteAttr(legacyT0Name, t0(), 10);
   xmlFile.WriteAttr(legacyT1Name, t1(), 10);
#ifdef EXPERIMENTAL_SPECTRAL_EDITING
   if (f0() >= 0)
      xmlFile.WriteAttr(sDefaultF0Name, f0(), 10);
   if (f1() >= 0)
      xmlFile.WriteAttr(sDefaultF1Name, f1(), 10);
#endif
}

bool SelectedRegion::HandleXMLAttribute
(const wxChar *attr, const wxChar *value,
 const wxChar *legacyT0Name, const wxChar *legacyT1Name)
{
   typedef bool (SelectedRegion::*Setter)(double, bool);
   Setter setter = 0;
   if (!wxStrcmp(attr, legacyT0Name))
      setter = &SelectedRegion::setT0;
   else if (!wxStrcmp(attr, legacyT1Name))
      setter = &SelectedRegion::setT1;
#ifdef EXPERIMENTAL_SPECTRAL_EDITING
   else if (!wxStrcmp(attr, sDefaultF0Name))
      setter = &SelectedRegion::setF0;
   else if (!wxStrcmp(attr, sDefaultF1Name))
      setter = &SelectedRegion::setF1;
#endif
   else
      return false;

   double dblValue;
   if (!Internat::CompatibleToDouble(value, &dblValue))
      return false;

   // False means don't flip time or frequency boundaries
   (void)(this->*setter)(dblValue, false);
   return true;
}
