/**********************************************************************

  Audacity: A Digital Audio Editor

  EffectCategory.cpp

  Audacity(R) is copyright (c) 1999-2008 Audacity Team.
  License: GPL v2.  See License.txt.

**********************************************************************/


#include "Effect.h"
#include "EffectCategory.h"


EffectCategory::EffectCategory(const wxString& uri, const wxString& name)
   : mUri(uri),
     mName(name),
     mParentsFrozen(false) {

}


const wxString& EffectCategory::GetUri() const {
   return mUri;
}

const wxString& EffectCategory::GetName() const {
   return mName;
}

const CategorySet& EffectCategory::GetParents() const {
   return mParents;
}

const CategorySet& EffectCategory::GetSubCategories() const {
   return mSubCategories;
}

EffectSet EffectCategory::GetEffects(int type) const {
   EffectSet result;
   EffectSet::const_iterator iter;
   for (iter = mEffects.begin(); iter != mEffects.end(); ++iter) {
      int g = (*iter)->GetEffectFlags();
      if ((g & type) == g)
         result.insert(*iter);
   }
   return result;
}

   // Return all the effects that belong to this immediate category or any
   // of its subcategories), filtered by effect type.
EffectSet EffectCategory::GetAllEffects(int type) const {
   EffectSet result = GetEffects(type);
   CategorySet::const_iterator iter;
   for (iter = mSubCategories.begin(); iter != mSubCategories.end(); ++iter) {
      EffectSet tmp = (*iter)->GetAllEffects(type);
      EffectSet::const_iterator itr2;
      for (itr2 = tmp.begin(); itr2 != tmp.end(); ++itr2)
         result.insert(*itr2);
   }
   return result;
}


bool EffectCategory::AddParent(EffectCategory* parent) {
   if (mParentsFrozen)
      return false;
   if (parent->IsDescendantOf(this))
      return false;
   mParents.insert(parent);
   parent->mSubCategories.insert(this);
   return true;
}

bool EffectCategory::AddEffect(Effect* effect) {
   mEffects.insert(effect);
   return true;
}

void EffectCategory::FreezeParents() {
   mParentsFrozen = true;
}

void EffectCategory::UnfreezeParents() {
   mParentsFrozen = false;
}

bool EffectCategory::IsDescendantOf(EffectCategory* category) {
   if (category == this)
      return true;
   CategorySet::const_iterator iter;
   for (iter = mParents.begin(); iter != mParents.end(); ++iter) {
      if ((*iter)->IsDescendantOf(category))
         return true;
   }
   return false;
}
