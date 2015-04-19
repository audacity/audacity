/**********************************************************************

Audacity: A Digital Audio Editor

ViewInfo.cpp

Paul Licameli

**********************************************************************/

#include "ViewInfo.h"

#include "Internat.h"
#include "xml/XMLWriter.h"

ViewInfo::ViewInfo(double start, double screenDuration, double pixelsPerSecond)
   : selectedRegion()
   , track(0)
   , vpos(0)
   , h(start)
   , screen(screenDuration)
   , total(screen)
   , zoom(pixelsPerSecond)

   , sbarH(0)
   , sbarScreen(1)
   , sbarTotal(1)
   , sbarScale(1.0)
   , scrollStep(16)

   , bUpdateTrackIndicator(true)
{
}

void ViewInfo::WriteXMLAttributes(XMLWriter &xmlFile)
{
   selectedRegion.WriteXMLAttributes(xmlFile, wxT("sel0"), wxT("sel1"));
   xmlFile.WriteAttr(wxT("vpos"), vpos);
   xmlFile.WriteAttr(wxT("h"), h, 10);
   xmlFile.WriteAttr(wxT("zoom"), zoom, 10);
}

bool ViewInfo::ReadXMLAttribute(const wxChar *attr, const wxChar *value)
{
   if (selectedRegion.HandleXMLAttribute(attr, value, wxT("sel0"), wxT("sel1")))
      return true;

   if (!wxStrcmp(attr, wxT("vpos"))) {
      long longVpos;
      wxString(value).ToLong(&longVpos);
      vpos = int(longVpos);
      return true;
   }

   if (!wxStrcmp(attr, wxT("h"))) {
      Internat::CompatibleToDouble(value, &h);
      return true;
   }

   if (!wxStrcmp(attr, wxT("zoom"))) {
      Internat::CompatibleToDouble(value, &zoom);
      return true;
   }

   return false;
}
