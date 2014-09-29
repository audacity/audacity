/**********************************************************************

  Audacity: A Digital Audio Editor

  LV2PortGroup.h

  Audacity(R) is copyright (c) 1999-2008 Audacity Team.
  License: GPL v2.  See License.txt.

*********************************************************************/

#ifndef LV2PORTGROUP_H
#define LV2PORTGROUP_H


#include <vector>

#include <wx/string.h>

class LV2PortGroup;
typedef std::vector<const LV2PortGroup *> LV2PortGroupArray;

/** A class that contains information about a single LV2 plugin port group,
    such as its children and its name. */
class LV2PortGroup
{
public:
   LV2PortGroup(const wxString & name = wxEmptyString);

   /** Add a subgroup of this group. */
   void AddSubGroup(const LV2PortGroup & subgroup);

   /** Return a list of all subgroups. */
   const LV2PortGroupArray & GetSubGroups() const;

   /** Add a parameter number (not port number). */
   void AddParameter(int parameter);

   /** Return a list of all parameters in this group. */
   const wxArrayInt & GetParameters() const;

   const wxString & GetName() const;

 private:

   wxString mName;
   LV2PortGroupArray mSubGroups;
   wxArrayInt mParameters;
};

#endif
