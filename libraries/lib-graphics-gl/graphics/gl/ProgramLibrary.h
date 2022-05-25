/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  ProgramLibrary.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include <cstddef>
#include <map>
#include <memory>

namespace graphics::gl
{
class GLRenderer;
class Program;

enum class ProgramID
{
   Default,

   Count
};

class ProgramLibrary final
{
public:
   explicit ProgramLibrary(GLRenderer& renderer);
   ~ProgramLibrary();

   ProgramLibrary(const ProgramLibrary&) = delete;
   ProgramLibrary& operator=(const ProgramLibrary&) = delete;
   ProgramLibrary(ProgramLibrary&&) = delete;
   ProgramLibrary& operator=(ProgramLibrary&&) = delete;

   std::shared_ptr<Program> GetProgram(ProgramID programID) const;
   std::shared_ptr<Program> GetLinearGradientProgram(size_t stops) const;

private:
   void CreateProgram(ProgramID programID);
   
   GLRenderer& mRenderer;

   std::shared_ptr<Program> mLibrary[size_t(ProgramID::Count)];
   
   std::map<size_t, std::shared_ptr<Program>> mLinearGradientPrograms;
};

} // namespace graphics::gl
