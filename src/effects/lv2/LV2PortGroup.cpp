/**********************************************************************

  Audacity: A Digital Audio Editor

  LV2PortGroup.cpp

  Audacity(R) is copyright (c) 1999-2008 Audacity Team.
  License: GPL v2.  See License.txt.

*********************************************************************/

#include "LV2PortGroup.h"

#if defined(USE_SLV2)

LV2PortGroup::LV2PortGroup(const wxString& name)
  : mName(name) {
  
}
   
void LV2PortGroup::AddSubGroup(const LV2PortGroup& subgroup) {
  mSubgroups.push_back(subgroup);
}
   
const std::vector<LV2PortGroup>& LV2PortGroup::GetSubGroups() const {
  return mSubgroups;
}

void LV2PortGroup::AddParameter(uint32_t parameter) {
  mParameters.push_back(parameter);
}
   
const std::vector<uint32_t>& LV2PortGroup::GetParameters() const {
  return mParameters;
}

const wxString& LV2PortGroup::GetName() const {
  return mName;
}

#endif

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: d8622b59-5c08-4e7f-a170-2502ff8af8e5

