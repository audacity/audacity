/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  ProgramLibrary.cpp

  Dmitry Vedenko

**********************************************************************/
#include "ProgramLibrary.h"
#include "Program.h"

namespace graphics::gl
{
namespace
{
const char* defaultVertexShader = R"(#version 140

in vec2 in_Position;
in vec2 in_UV;
in vec4 in_MulColor;
in vec4 in_AddColor;

out vec2 var_UV;
out vec4 var_MulColor;
out vec4 var_AddColor;

void main()
{
   gl_Position = vec4(in_Position, 0.0, 1.0);

   var_UV = in_UV;

   var_MulColor = in_MulColor;
   var_AddColor = in_AddColor;
}
)";

const char* defaultFragmentShader = R"(#version 140

in vec2 var_UV;
in vec4 var_MulColor;
in vec4 var_AddColor;

uniform sampler2D s_Texture;

out vec4 fragColor;

void main()
{
	vec4 tex = texture(s_Texture, var_UV);
	fragColor = tex * var_MulColor + var_AddColor;
}
)";

using ProgramFactory = std::shared_ptr<Program>(*)(GLRenderer& renderer);

ProgramFactory factories[] = { [](GLRenderer& renderer) {
                                  return ProgramBuilder(renderer)
                                     .AddVertexShader(defaultVertexShader)
                                     .AddFragmentShader(defaultFragmentShader)
                                     .BindAttributeLocation("in_Position", 0)
                                     .BindAttributeLocation("in_UV", 1)
                                     .BindAttributeLocation("in_MulColor", 2)
                                     .BindAttributeLocation("in_AddColor", 3)
                                     .Build();
                               } };
}

ProgramLibrary::ProgramLibrary(GLRenderer& renderer)
    : mRenderer(renderer)
{
}

ProgramLibrary::~ProgramLibrary()
{
}

std::shared_ptr<Program> ProgramLibrary::GetProgram(ProgramID programID) const
{
   if (programID >= ProgramID::Count)
      return nullptr;

   if (mLibrary[size_t(programID)] == nullptr)
      const_cast<ProgramLibrary*>(this)->CreateProgram(programID);

   return mLibrary[size_t(programID)];
}

void ProgramLibrary::CreateProgram(ProgramID programID)
{
   mLibrary[size_t(programID)] = factories[size_t(programID)](mRenderer);
}

} // namespace graphics::gl
