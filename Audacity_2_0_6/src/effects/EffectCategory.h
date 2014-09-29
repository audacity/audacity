/**********************************************************************

  Audacity: A Digital Audio Editor

  EffectCategory.h

  Audacity(R) is copyright (c) 1999-2008 Audacity Team.
  License: GPL v2.  See License.txt.

******************************************************************//**

\class EffectCategory
\brief EffectManager contains all the information about an effect category.

That includes links to parent categories and subcategories, name and
URI, and of course the effects that belong to this category.

*//*******************************************************************/


#ifndef __AUDACITY_EFFECTCATEGORY__
#define __AUDACITY_EFFECTCATEGORY__

#include <set>

#include <wx/string.h>

#include "Effect.h"


class EffectCategory {

 public:

   /** This is used in the EffectSet typedef. It compares effect pointers,
       first by their names and then by pointer values. This makes EffectSets
       automatically sorted by name while allowing for different effects with
       the same name. */
   struct CompareEffects {
      bool operator()(Effect* a, Effect* b) const {
         return (a->GetEffectName() < b->GetEffectName()) ||
            ((a->GetEffectName() == b->GetEffectName()) && (a < b));
      }
   };

   typedef std::set<Effect*, CompareEffects> EffectSet;

   /** This is used in the CategorySet typedef. It compares category pointers,
       first by their names and then by pointer values. This makes CategorySets
       automatically sorted by name while allowing for different categories
       with the same name. */
   struct CompareCategories {
      bool operator()(const EffectCategory* a, const EffectCategory* b) const {
         return (a->GetName() < b->GetName()) ||
            ((a->GetName() == b->GetName()) && (a < b));
      }
   };

   typedef std::set<EffectCategory*, CompareCategories> CategorySet;


   /** Return the URI for this category, which is used as a global, persistent
       string identifier for the category. */
   const wxString& GetUri() const;

   /** Return the (possibly i18n'd) name for the category. */
   const wxString& GetName() const;

   /** Return all the parent categories of this category. A category may
       have 0, 1 or more parents - the structure of all categories forms
       a DAG, not necessarily a tree, since LV2 allows for categories to
       have multiple parents (e.g. Reverb is a subcategory of both Simulator
       and Delay). */
   const CategorySet& GetParents() const;

   /** Return all the subcategories of this category. A category may have 0, 1
       or more subcategories. */
   const CategorySet& GetSubCategories() const;

   /** Return all the effects that belong to this immediate category (not any
       of its subcategories), filtered by effect type. */
   EffectSet GetEffects(int type = ALL_EFFECTS) const;

   /** Return all the effects that belong to this immediate category or any
       of its subcategories), filtered by effect type. */
   EffectSet GetAllEffects(int type = ALL_EFFECTS) const;

 protected:

   /** All constructors and destructors are non-public, allowing only
       EffectManager (which is our friend) to create new categories. */
   EffectCategory(const wxString& uri, const wxString& name);

   /** Add a new parent category to this category, making this a subcategory of
       the parent. Returns true if the parent was added (or already had been
       added), false if the parent could not be added because FreezeParents()
       has been called. This is protected so only our friend EffectManager
       can change the category graph (it needs to keep track of root nodes). */
   bool AddParent(EffectCategory* parent);

   /** Add an effect to this category. This is protected so only our friend
       EffectManager can call it. */
   bool AddEffect(Effect* effect);

   /** This function sets a flag that makes AddParent() fail when called for
       this category object, preventing anyone from adding new parents to
       this category. This is useful if you are mapping a different category
       tree onto the already existing one (e.g. LRDF onto LV2) and don't
       want to add more internal parent/subcategory relations between already
       existing categories, but still want to allow adding of completely
       new, unmapped subcategories. */
   void FreezeParents();

   /** Reset the flag set by FreezeParents(), allowing to add parent categories
       again. */
   void UnfreezeParents();

   /** Returns true if category is an ancestor of this category, false
       if not. */
   bool IsDescendantOf(EffectCategory* category);

   friend class EffectManager;

   wxString mUri;
   wxString mName;
   CategorySet mParents;
   CategorySet mSubCategories;
   EffectSet mEffects;
   bool mParentsFrozen;

};


// Add these in the global namespace to reduce typing
typedef EffectCategory::EffectSet EffectSet;
typedef EffectCategory::CategorySet CategorySet;


#endif
