/**********************************************************************

  Audacity: A Digital Audio Editor

  EffectManager.cpp

  Audacity(R) is copyright (c) 1999-2008 Audacity Team.
  License: GPL v2.  See License.txt.

**********************************************************************/

#include <iostream>

#include "EffectManager.h"


//
// Initialisations for static class members
//

EffectManager& EffectManager::Get() {
   static EffectManager em;
   return em;
}


EffectManager::EffectManager()
:  mNumEffects(0)
{
#ifdef EFFECT_CATEGORIES
   mCategories = new CategoryMap();
   mRootCategories = new CategorySet();
   mUnsorted = new EffectSet();
#endif
}

EffectManager::~EffectManager()
{
#ifdef EFFECT_CATEGORIES
   CategoryMap::iterator i;
   for (i = mCategories->begin(); i != mCategories->end(); ++i)
      delete i->second;

   delete mUnsorted;
   delete mRootCategories;
   delete mCategories;
#endif
}

void EffectManager::RegisterEffect(Effect *f, int NewFlags)
{
   f->mID = mNumEffects;
   mNumEffects++;
   if( NewFlags != 0)
      f->SetEffectFlags( NewFlags );

   // Insert the effect into the list in alphabetical order
   // A linear search is good enough as long as there are
   // only a few dozen or even a few hundred effects.
   wxString name = Effect::StripAmpersand(f->GetEffectName());
   int len = mEffects.GetCount();
   int i;
   for(i=0; i<len; i++)
      if (name.CmpNoCase(Effect::StripAmpersand(mEffects[i]->GetEffectName())) < 0) {
         mEffects.Insert(f, i);
         break;
      }
   if (i==len)
      mEffects.Add(f);

#ifdef EFFECT_CATEGORIES
   // Add the effect in the right categories
   std::set<wxString> catUris = f->GetEffectCategories();
   bool oneValid = false;
   std::set<wxString>::const_iterator iter;
   for (iter = catUris.begin(); iter != catUris.end(); ++iter) {
      EffectCategory* cat = LookupCategory(*iter);
      if (cat != 0) {
         cat->AddEffect(f);
         oneValid = true;
      }
   }
   if (!oneValid)
      mUnsorted->insert(f);

#endif
}

void EffectManager::UnregisterEffects()
{
   for(int i=0; i<mNumEffects; i++)
      delete mEffects[i];

   mEffects.clear();

#ifdef EFFECT_CATEGORIES
   mUnsorted->clear();

   CategoryMap::iterator iter;
   for (iter = mCategories->begin(); iter != mCategories->end(); ++iter)
      iter->second->mEffects.clear();
#endif
}

int EffectManager::GetNumEffects()
{
   return mNumEffects;
}

Effect *EffectManager::GetEffect(int ID)
{
   for(int i=0; i<mNumEffects; i++)
      if (mEffects[i]->mID == ID)
         return mEffects[i];

   return NULL;
}

Effect* EffectManager::GetEffectByIdentifier(const wxString strTarget, const int kFlags /*= ALL_EFFECTS*/)
{
   if( strTarget == wxT("") ) // set GetEffectIdentifier to wxT("") to not show an effect in Batch mode
      return NULL;
   for (unsigned int i = 0; i < mEffects.GetCount(); i++)
   {
      int nFlags = mEffects[i]->GetEffectFlags();
      if (((nFlags & kFlags) == nFlags) && strTarget.IsSameAs(mEffects[i]->GetEffectIdentifier()))
         return mEffects[i];
   }
   return NULL;
}

EffectArray *EffectManager::GetEffects(int flags /* = ALL_EFFECTS */)
{
   EffectArray *results = new EffectArray();

   int len = mEffects.GetCount();
   for(int i=0; i<len; i++) {
      int g = mEffects[i]->GetEffectFlags();
      if ((flags & g) == g)
         results->Add(mEffects[i]);
   }

   return results;
}


#ifdef EFFECT_CATEGORIES

EffectCategory* EffectManager::AddCategory(const wxString& URI,
                                           const wxString& name) {

   CategoryMap::const_iterator iter = mCategories->find(URI);
   if (iter != mCategories->end())
      return iter->second;
   EffectCategory* cat = new EffectCategory(URI, name);
   mCategories->insert(std::make_pair(URI, cat));
   mRootCategories->insert(cat);
   return cat;
}

EffectCategory* EffectManager::LookupCategory(const wxString& URI) {
   CategoryMap::const_iterator iter = mCategories->find(URI);
   if (iter != mCategories->end())
      return iter->second;
   return 0;
}

bool EffectManager::AddCategoryParent(EffectCategory* child,
                                      EffectCategory* parent) {
   bool result = child->AddParent(parent);
   if (!result)
      return false;
   CategorySet::iterator iter = mRootCategories->find(child);
   if (iter != mRootCategories->end())
      mRootCategories->erase(iter);
   return true;
}

void EffectManager::FreezeCategories() {
   CategoryMap::iterator iter;
   for (iter = mCategories->begin(); iter != mCategories->end(); ++iter)
      iter->second->FreezeParents();
}

const CategorySet& EffectManager::GetRootCategories() const {
   return *mRootCategories;
}

EffectSet EffectManager::GetUnsortedEffects(int flags) const {

   if (flags == ALL_EFFECTS)
      return *mUnsorted;

   EffectSet result;
   EffectSet::const_iterator iter;
   for (iter = mUnsorted->begin(); iter != mUnsorted->end(); ++iter) {
      int g = (*iter)->GetEffectFlags();
      if ((flags & g) == g)
         result.insert(*iter);
   }

   return result;
}

#endif
