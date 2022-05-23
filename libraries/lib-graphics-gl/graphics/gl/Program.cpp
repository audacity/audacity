/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  Program.cpp

  Dmitry Vedenko

**********************************************************************/
#include "Program.h"

#include "GLRenderer.h"
#include "Context.h"

namespace graphics::gl
{

Program::~Program()
{
   mRenderer.GetResourceContext().GetFunctions().DeleteProgram(mProgram);
}   

void Program::SetUniform(std::string_view name, GLint value)
{
   mRenderer.GetResourceContext().GetFunctions().Uniform1i(
      GetUniformLocation(name), value);
}

void Program::SetUniform(std::string_view name, GLfloat value)
{
   mRenderer.GetResourceContext().GetFunctions().Uniform1f(
      GetUniformLocation(name), value);
}

void Program::SetUniform(std::string_view name, GLfloat value0, GLfloat value1)
{
   mRenderer.GetResourceContext().GetFunctions().Uniform2f(
      GetUniformLocation(name), value0, value1);
}

void Program::SetUniform(
   std::string_view name, GLfloat value0, GLfloat value1, GLfloat value2)
{
   mRenderer.GetResourceContext().GetFunctions().Uniform3f(
      GetUniformLocation(name), value0, value1, value2);
}

void Program::SetUniform(
   std::string_view name, GLfloat value0, GLfloat value1, GLfloat value2,
   GLfloat value3)
{
   mRenderer.GetResourceContext().GetFunctions().Uniform4f(
      GetUniformLocation(name), value0, value1, value2, value3);
}

void Program::Bind(Context& context)
{
   context.GetFunctions().UseProgram(mProgram);
}

Program::Program(GLRenderer& renderer, GLuint program)
    : mRenderer(renderer)
    , mProgram(program)
{
}

GLint Program::GetUniformLocation(std::string_view name)
{
   auto it = mUniformLocations.find(name);

   if (it != mUniformLocations.end())
      return it->second;

   std::string nameStr(name);

   const GLint location =
      mRenderer.GetResourceContext().GetFunctions().GetUniformLocation(
         mProgram, nameStr.c_str());

   mUniformLocations.emplace(std::move(nameStr), location);

   return location;
}

ProgramBuilder::ProgramBuilder(GLRenderer& renderer)
    : mRenderer(renderer)
    , mFunctions(renderer.GetResourceContext().GetFunctions())
{
   mProgram = mFunctions.CreateProgram();
}

ProgramBuilder::~ProgramBuilder()
{
   if (mProgram != 0)
      mRenderer.GetResourceContext().GetFunctions().DeleteProgram(mProgram);
}

ProgramBuilder& ProgramBuilder::AddVertexShader(std::string_view shader)
{
   return AddShader(GLenum::VERTEX_SHADER, shader);
}

ProgramBuilder& ProgramBuilder::AddFragmentShader(std::string_view shader)
{
   return AddShader(GLenum::FRAGMENT_SHADER, shader);
}

ProgramBuilder&
ProgramBuilder::BindAttributeLocation(std::string_view name, uint32_t index)
{
   mFunctions.BindAttribLocation(
      mProgram, index, GetNullTerminatedString(name));
   return *this;
}

std::shared_ptr<Program> ProgramBuilder::Build()
{
   mFunctions.LinkProgram(mProgram);
   
   GLint linked;
   mFunctions.GetProgramiv(mProgram, GLenum::LINK_STATUS, &linked);

   if (!linked)
   {
      GLsizei logLength = 0;

      mFunctions.GetProgramiv(mProgram, GLenum::INFO_LOG_LENGTH, &logLength);
      std::string log(logLength + 1, '\0');

      mFunctions.GetProgramInfoLog(mProgram, logLength, &logLength, log.data());

      mLogString += log + "\n";

      return {};
   }

   GLuint program = 0;
   std::swap(program, mProgram);
   
   return std::shared_ptr<Program>(new Program(mRenderer, program));
}

std::string_view ProgramBuilder::GetCompilationLog() const
{
   return mLogString;
}

const char* ProgramBuilder::GetNullTerminatedString(std::string_view name)
{
   if (name.empty())
      return "";

   if (name.data()[name.length()] == '\0')
      return name.data();

   mCacheString = std::string(name);
   
   return mCacheString.c_str();
}

ProgramBuilder&
ProgramBuilder::AddShader(GLenum shaderType, std::string_view shaderSource)
{
   const GLuint shader = mFunctions.CreateShader(shaderType);

   const char* source = shaderSource.data();
   const GLsizei length(shaderSource.length());
   
   mFunctions.ShaderSource(shader, 1, &source, &length);
   mFunctions.CompileShader(shader);

   GLint compiled;
   mFunctions.GetShaderiv(shader, GLenum::COMPILE_STATUS, &compiled);
   
   if (!compiled)
   {
      GLsizei logLength = 0;

      mFunctions.GetShaderiv(shader, GLenum::INFO_LOG_LENGTH, &logLength);
      std::string log(logLength + 1, '\0');

      mFunctions.GetShaderInfoLog(shader, logLength, &logLength, log.data());

      mLogString += log + "\n";
   }
   else
   {
      mFunctions.AttachShader(mProgram, shader);
   }

   mFunctions.DeleteShader(shader);
   
   return *this;
}

} // namespace graphics::gl
