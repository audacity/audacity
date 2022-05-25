/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  Program.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include <memory>
#include <string_view>
#include <string>
#include <map>
#include <variant>
#include <vector>

#include "GLFunctions.h"
#include "graphics/Color.h"

namespace graphics::gl
{
class GLRenderer;
class Context;

class ProgramConstants final
{
public:
   enum class UniformSize
   {
      Scalar,
      Vec2,
      Vec3,
      Vec4,
      Mat2,
      Mat3,
      Mat4,
      Mat2x3,
      Mat3x2,
      Mat2x4,
      Mat4x2,
      Mat3x4,
      Mat4x3,

      Count
   };

   void SetUniform(std::string_view name, UniformSize size, GLsizei count, const GLint* data);

   void SetUniform(std::string_view name, UniformSize size, GLsizei count, const GLfloat* data);

   void SetUniform(std::string_view name, GLsizei count, const Color* data);
   
   void SetUniform(std::string_view name, GLint value);
   void SetUniform(std::string_view name, GLfloat value);
   void SetUniform(std::string_view name, GLfloat value0, GLfloat value1);
   void SetUniform(std::string_view name, GLfloat value0, GLfloat value1, GLfloat value2);
   void SetUniform(std::string_view name, GLfloat value0, GLfloat value1, GLfloat value2, GLfloat value3);
   void SetUniform(std::string_view name, Color color);

   size_t GetVersion() const noexcept;

private:
   using UniformData = std::variant<std::vector<GLint>, std::vector<GLfloat>>;
   
   struct Uniform final
   {
      std::string name;
      UniformData data;
      UniformSize size { UniformSize::Scalar };
      GLsizei count { 1 };
   };

   Uniform& GetUniformForUpdate(std::string_view name);

   std::vector<Uniform> mUniforms;

   size_t mVersion { 0 };

   friend class Program;
};

class Program final
{
public:
   ~Program();

private:
   void Bind(Context& context, const ProgramConstants* constants);

   void UpdateProgramConstants(Context& context, const ProgramConstants& constants);
   
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
