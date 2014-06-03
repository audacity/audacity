/**********************************************************************

  Audacity: A Digital Audio Editor

  EffectManager.h

  Audacity(R) is copyright (c) 1999-2008 Audacity Team.
  License: GPL v2.  See License.txt.

******************************************************************//**

\class EffectManager
\brief EffectManager is the class that handles effects and effect categories.

It maintains a graph of effect categories and subcategories,
registers and unregisters effects and can return filtered lists of
effects.

*//*******************************************************************/


#ifndef __AUDACITY_EFFECTMANAGER__
#define __AUDACITY_EFFECTMANAGER__

#include <map>

#include "Effect.h"

#ifdef EFFECT_CATEGORIES
#include "EffectCategory.h"
#endif

WX_DEFINE_USER_EXPORTED_ARRAY(Effect *, EffectArray, class AUDACITY_DLL_API);


class AUDACITY_DLL_API EffectManager {

 public:

   /** Get the singleton instance of the EffectManager. Probably not safe
       for multi-thread use. */
   static EffectManager& Get();

 //
 // public methods
 //
 // Used by the outside program to register the list of effects and retrieve
 // them by index number, usually when the user selects one from a menu.
 //
 public:

   EffectManager();

   /** A destructor is needed so we can delete all categories. */
   ~EffectManager();

   /** Register an effect so it will appear in the menu. */
   void RegisterEffect(Effect *f, int AdditionalFlags=0);

   /** Unregister all effects. */
   void UnregisterEffects();

   /** Return an effect by its numerical ID. */
   Effect *GetEffect(int ID);

   Effect* GetEffectByIdentifier(const wxString strTarget, const int kFlags = ALL_EFFECTS);

   /** Return the number of registered effects. */
   int GetNumEffects();

   /** Returns a sorted array of effects, which may be filtered
       using the flags parameter.  The caller should dispose
       of the array when done. */
   EffectArray *GetEffects(int flags = ALL_EFFECTS);

#ifdef EFFECT_CATEGORIES

   /** Add a new effect category with the given URI and name and
       return a pointer to it. If a category with this URI already
       exists, return that instead. */
   EffectCategory* AddCategory(const wxString& URI,
                               const wxString& name);

   /** Return a pointer to the effect category with the given URI
       or 0 if no such category has been added. */
   EffectCategory* LookupCategory(const wxString& URI);

   /** Make one category the parent of another category. Both categories
       must have been returned from AddCategory() or LookupCategory().
       If the new parent-child relationship would create any loops
       in the graph of categories false will be returned and the graph
       will not be modified, otherwise the function will return true. */
   bool AddCategoryParent(EffectCategory* child, EffectCategory* parent);

   /** Freeze the subcategory relations between all categories added so far. */
   void FreezeCategories();

   /** Return the set of all root categories, i.e. the ones without parents. */
   const CategorySet& GetRootCategories() const;

   /** Return the set of all uncategorised effects. */
   EffectSet GetUnsortedEffects(int flags = ALL_EFFECTS) const;

#endif

 private:

   EffectArray mEffects;
   int mNumEffects;

#ifdef EFFECT_CATEGORIES
   // This maps URIs to EffectCategory pointers for all added categories.
   // It is needed for fast lookup and easy deletion.
   typedef std::map<wxString, EffectCategory*> CategoryMap;
   CategoryMap *mCategories;

   // These are the root categories, i.e. the ones without parents.
   CategorySet *mRootCategories;

   // Special category that all effects with unknown category URIs
   // are placed in.
   EffectSet *mUnsorted;
#endif

};


#endif
