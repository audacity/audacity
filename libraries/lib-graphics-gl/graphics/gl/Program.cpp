/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  Program.cpp

  Dmitry Vedenko

**********************************************************************/
#include "Program.h"

#include <array>

#include "GLRenderer.h"
#include "Context.h"

#include "MemoryX.h"

namespace graphics::gl
{

namespace
{
using IntFunction    = void(GLAPIENTRY*)(GLint location, GLsizei count, const GLint* v);
using FloatFunction  = void(GLAPIENTRY*)(GLint location, GLsizei count, const GLfloat* v);
using MatrixFunction = void(GLAPIENTRY*)(GLint location, GLsizei count, GLboolean transpose, const GLfloat* value);

struct UniformFunction final
{
   GLsizei components;

   IntFunction intFunction;
   FloatFunction floatFunction;
   MatrixFunction matrixFunction;
};

class UniformFunctions final
{
public:
   explicit UniformFunctions(const GLFunctions& functions)
       : mFunctions {
          UniformFunction { 1, functions.Uniform1iv, functions.Uniform1fv,
                            nullptr }, // Scalar
          UniformFunction { 2, functions.Uniform2iv, functions.Uniform2fv,
                            nullptr }, // Vec2
          UniformFunction { 3, functions.Uniform3iv, functions.Uniform3fv,
                            nullptr }, // Vec3
          UniformFunction { 4, functions.Uniform4iv, functions.Uniform4fv,
                            nullptr }, // Vec4
          UniformFunction { 4, nullptr, nullptr,
                            functions.UniformMatrix2fv }, // Mat2
          UniformFunction { 9, nullptr, nullptr,
                            functions.UniformMatrix3fv }, // Mat3
          UniformFunction { 16, nullptr, nullptr,
                            functions.UniformMatrix4fv }, // Mat4
          UniformFunction { 6, nullptr, nullptr,
                            functions.UniformMatrix2x3fv }, // Mat2x3
          UniformFunction { 6, nullptr, nullptr,
                            functions.UniformMatrix3x2fv }, // Mat3x2
          UniformFunction { 8, nullptr, nullptr,
                            functions.UniformMatrix2x4fv }, // Mat2x4
          UniformFunction { 8, nullptr, nullptr,
                            functions.UniformMatrix4x2fv }, // Mat4x2
          UniformFunction { 12, nullptr, nullptr,
                            functions.UniformMatrix3x4fv }, // Mat3x4
          UniformFunction { 12, nullptr, nullptr,
                            functions.UniformMatrix4x3fv }, // Mat4x3
       }
   {
   }

   void SetUniform(GLint location, ProgramConstants::UniformSize size, const GLsizei count, const std::vector<GLint>& data) const
   {
      auto& fns = mFunctions[size_t(size)];

      assert(data.size() == fns.components * count);
      assert(fns.intFunction != nullptr);

      if (fns.intFunction != nullptr && data.size() == fns.components * count)
         fns.intFunction(location, count, data.data());
   }

   void SetUniform(GLint location, ProgramConstants::UniformSize size, const GLsizei count, const std::vector<GLfloat>& data) const
   {
      auto& fns = mFunctions[size_t(size)];

      assert(data.size() == fns.components * count);

      if (data.size() == fns.components * count)
      {
         if (fns.floatFunction != nullptr)
            fns.floatFunction(location, count, data.data());
         else
            fns.matrixFunction(location, count, false, data.data());
      }
   }

private:
   using Functions = std::array<UniformFunction, size_t(ProgramConstants::UniformSize::Count)>;
   Functions mFunctions;
};

size_t GetComponetsCount(ProgramConstants::UniformSize size) noexcept
{
   const size_t lookup[] = {
      1, 2, 3, 4, 4, 9, 16, 6, 6, 8, 8, 12, 12,
   };

   return lookup[size_t(size)];
}
}

void ProgramConstants::SetUniform(
   std::string_view name, UniformSize size, GLsizei count, const GLint* data)
{
   if (size >= UniformSize::Count)
      return;

   auto& uniform = GetUniformForUpdate(name);

   uniform.size = size;
   uniform.count = count;

   uniform.data = std::vector<GLint> (data, data + count * GetComponetsCount(size));
}

void ProgramConstants::SetUniform(
   std::string_view name, UniformSize size, GLsizei count, const GLfloat* data)
{
   if (size >= UniformSize::Count)
      return;

   auto& uniform = GetUniformForUpdate(name);

   uniform.size = size;
   uniform.count = count;

   uniform.data =
      std::vector<GLfloat>(data, data + count * GetComponetsCount(size));
}

void ProgramConstants::SetUniform(
   std::string_view name, GLsizei count, const Color* data)
{
   auto& uniform = GetUniformForUpdate(name);

   uniform.size = UniformSize::Vec4;
   uniform.count = count;

   std::vector<GLfloat> colors;
   colors.reserve(4 * count);

   for (GLsizei i = 0; i < count; ++i)
   {
      const auto color = data[i];

      colors.push_back(color.GetRed() / 255.0f);
      colors.push_back(color.GetGreen() / 255.0f);
      colors.push_back(color.GetBlue() / 255.0f);
      colors.push_back(color.GetAlpha() / 255.0f);
   }

   uniform.data = std::move(colors);
}

void ProgramConstants::SetUniform(std::string_view name, GLint value)
{
   SetUniform(name, UniformSize::Scalar, 1, &value);
}

void ProgramConstants::SetUniform(std::string_view name, GLfloat value)
{
   SetUniform(name, UniformSize::Scalar, 1, &value);
}

void ProgramConstants::SetUniform(
   std::string_view name, GLfloat value0, GLfloat value1)
{
   const GLfloat data[] = { value0, value1 };

   SetUniform(name, UniformSize::Vec2, 1, data);
}

void ProgramConstants::SetUniform(
   std::string_view name, GLfloat value0, GLfloat value1, GLfloat value2)
{
   const GLfloat data[] = { value0, value1, value2 };

   SetUniform(name, UniformSize::Vec3, 1, data);
}

void ProgramConstants::SetUniform(
   std::string_view name, GLfloat value0, GLfloat value1, GLfloat value2,
   GLfloat value3)
{
   const GLfloat data[] = {
      value0, value1, value2, value3
   };

   SetUniform(name, UniformSize::Vec4, 1, data);
}

void ProgramConstants::SetUniform(std::string_view name, Color color)
{
   SetUniform(name, 1, &color);
}

size_t ProgramConstants::GetVersion() const noexcept
{
   return mVersion;
}

ProgramConstants::Uniform& ProgramConstants::GetUniformForUpdate(std::string_view name)
{
   ++mVersion;

   auto it = std::find_if(
      mUniforms.begin(), mUniforms.end(),
      [name](const auto& uniform) { return uniform.name == name; });

   if (it != mUniforms.end())
      return *it;

   mUniforms.emplace_back();
   mUniforms.back().name = std::string(name);

   return mUniforms.back();
}

Program::~Program()
{
   mRenderer.GetResourceContext().GetFunctions().DeleteProgram(mProgram);
}

void Program::Bind(Context& context, const ProgramConstants* constants)
{
   context.GetFunctions().UseProgram(mProgram);

   if (constants != nullptr)
      UpdateProgramConstants(context, *constants);
}

void Program::UpdateProgramConstants(
   Context& context, const ProgramConstants& constants)
{
   UniformFunctions functions(context.GetFunctions());

   for (const auto& uniform : constants.mUniforms)
   {
      Visit ([&functions, &uniform, location = GetUniformLocation(uniform.name)](const auto& type) {
         if (location != -1)
            functions.SetUniform(location, uniform.size, uniform.count, type);
      }, uniform.data);
   }
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
   if (mProgram == 0)
      return {};

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
   if (mProgram == 0)
      return *this;

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

      mFunctions.DeleteProgram(mProgram);
      mProgram = 0;
   }
   else
   {
      mFunctions.AttachShader(mProgram, shader);
   }

   mFunctions.DeleteShader(shader);

   return *this;
}

} // namespace graphics::gl
