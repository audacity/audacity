/**********************************************************************

  Audacity: A Digital Audio Editor

  LV2PortGroup.cpp

  Audacity(R) is copyright (c) 1999-2008 Audacity Team.
  License: GPL v2.  See License.txt.

*********************************************************************/

#include "../../Audacity.h"

#if defined(USE_LV2)

#include <wx/dynarray.h>

#include "LV2PortGroup.h"

LV2PortGroup::LV2PortGroup(const wxString & name)
:  mName(name)
{
}

void LV2PortGroup::AddSubGroup(const LV2PortGroup & subgroup)
{
   wxString name = subgroup.GetName();

   LV2PortGroupArray::iterator i;
   for (i = mSubGroups.begin(); i != mSubGroups.end(); i++)
   {
      if ((*i)->GetName() == name)
      {
         return;
      }
   }

   mSubGroups.push_back(&subgroup);
}

const LV2PortGroupArray & LV2PortGroup::GetSubGroups() const
{
   return mSubGroups;
}

void LV2PortGroup::AddParameter(int parameter)
{
   mParameters.Add(parameter);
}

const wxArrayInt & LV2PortGroup::GetParameters() const
{
   return mParameters;
}

const wxString & LV2PortGroup::GetName() const
{
   return mName;
}

#endif
