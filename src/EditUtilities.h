/**********************************************************************

  Audacity: A Digital Audio Editor

  @file EditUtilities.h
  @brief Declare functions used in EditMenus but also needed elsewhere

  Paul Licameli split from EditMenus.cpp

**********************************************************************/

#ifndef __AUDACITY_EDIT_UTILITIES__
#define __AUDACITY_EDIT_UTILITIES__

#include <memory>
#include <vector>

class AudacityProject;
class TranslatableString;

//! A set of callbacks for special cut, copy, and paste behavior for tracks
struct AUDACITY_DLL_API CopyPasteMethods {
   virtual ~CopyPasteMethods();

   //! Called before doing the default paste procedure, which is skipped if return is true
   virtual bool DoPaste(AudacityProject &) = 0;

   //! Called before doing the default cut procedure, which is skipped if return is true
   virtual bool DoCut(AudacityProject &) = 0;

   //! Called before doing the default copy procedure, which is skipped if return is true
   virtual bool DoCopy(AudacityProject &) = 0;

   //! When true, cut/copy menu items will be enabled.  Else, they may yet be enabled for other reasons.
   virtual bool Enable(const AudacityProject &) = 0;
};

using CopyPasteMethodsTable =
   std::vector< std::unique_ptr< CopyPasteMethods > >;

CopyPasteMethodsTable &GetCopyPasteMethods();

//! A statically constructed object that installs callbacks
/*!
 Multiple callback objects may be installed and will be tried in the order they were installed until
 one returns true from the relevant virtual function
*/
struct AUDACITY_DLL_API RegisterCopyPasteMethods {
   RegisterCopyPasteMethods(
      std::unique_ptr<CopyPasteMethods> pMethods);
   ~RegisterCopyPasteMethods();

   struct AUDACITY_DLL_API Init{ Init(); };
};

//! Guarantee existence of the registry before use
static RegisterCopyPasteMethods::Init sInitializeCopyPasteMethods;

#endif
