/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  Program.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include <memory>
#include <string_view>
#include <map>

#include "GLFunctions.h"

namespace graphics::gl
{
class GLRenderer;
class Context;

class Program final
{
public:
   ~Program();

   void SetUniform(std::string_view name, GLint value);
   void SetUniform(std::string_view name, GLfloat value);
   void SetUniform(std::string_view name, GLfloat value0, GLfloat value1);
   void SetUniform(std::string_view name, GLfloat value0, GLfloat value1, GLfloat value2);
   void SetUniform(std::string_view name, GLfloat value0, GLfloat value1, GLfloat value2, GLfloat value3);

private:
   void Bind(Context& context);
   
   Program(GLRenderer& renderer, GLuint program);
   GLint GetUniformLocation(std::string_view name);

   GLRenderer& mRenderer;
   GLuint mProgram;

   std::map<std::string, GLint, std::less<>> mUniformLocations;

   friend class ProgramBuilder;
   friend class Context;
};

class ProgramBuilder final
{
public:
   explicit ProgramBuilder(GLRenderer& renderer);
   ~ProgramBuilder();

   ProgramBuilder& AddVertexShader(std::string_view shader);
   ProgramBuilder& AddFragmentShader(std::string_view shader);

   ProgramBuilder& BindAttributeLocation(std::string_view name, uint32_t index);

   std::shared_ptr<Program> Build();

   std::string_view GetCompilationLog() const;

private:
   const char* GetNullTerminatedString(std::string_view name);
   
   ProgramBuilder& AddShader(GLenum shaderType, std::string_view shader);
   
   GLRenderer& mRenderer;
   const GLFunctions& mFunctions;

   GLuint mProgram;

   std::string mCacheString;
   std::string mLogString;
};

} // namespace graphics::gl
