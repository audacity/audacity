/**********************************************************************

  Audacity: A Digital Audio Editor

  ProjectFormatVersion.h

  Dmitry Vedenko

**********************************************************************/

#pragma once

#include <cstdint>

/*!
   @brief A structure that holds the project version.

   aup3 file format stores version as a 32 bit integer. This class is used for easier interop
   with the user_version field of the project file.
*/
struct PROJECT_API ProjectFormatVersion final
{
   uint8_t Major { 0 };
   uint8_t Minor { 0 };
   uint8_t Revision { 0 };
   uint8_t ModLevel { 0 };

   // Create ProjectFormatVersion from the uint32_t value 
   static ProjectFormatVersion FromPacked(uint32_t) noexcept;
   //! Returns a version packed to 32-bit integer.
   uint32_t GetPacked() const noexcept;

   //! Returns true if version is valid, i.e. Major != 0
   bool IsValid() const noexcept;
};

PROJECT_API bool operator==(ProjectFormatVersion lhs, ProjectFormatVersion rhs) noexcept;
PROJECT_API bool operator!=(ProjectFormatVersion lhs, ProjectFormatVersion rhs) noexcept;
PROJECT_API bool operator<(ProjectFormatVersion lhs, ProjectFormatVersion rhs) noexcept;

//! This constant represents the current version of Audacity
PROJECT_API extern const ProjectFormatVersion SupportedProjectFormatVersion;
//! This is a helper constant for the "most compatible" project version with the value (3, 0, 0, 0). 
PROJECT_API extern const ProjectFormatVersion BaseProjectFormatVersion;
