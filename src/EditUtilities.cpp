/**********************************************************************

  Audacity: A Digital Audio Editor

  @file EditUtilities.cpp
  @brief Implement functions declared in EditUtilities.h

  Paul Licameli split from EditMenus.cpp

**********************************************************************/

#include "EditUtilities.h"

CopyPasteMethods::~CopyPasteMethods() = default;

CopyPasteMethodsTable &GetCopyPasteMethods()
{
   static CopyPasteMethodsTable methods;
   return methods;
}

RegisterCopyPasteMethods::RegisterCopyPasteMethods(
   std::unique_ptr<CopyPasteMethods> pMethods)
{
   GetCopyPasteMethods().emplace_back(move(pMethods));
}

RegisterCopyPasteMethods::~RegisterCopyPasteMethods()
{
   GetCopyPasteMethods().pop_back();
}

RegisterCopyPasteMethods::Init::Init() { GetCopyPasteMethods(); }
