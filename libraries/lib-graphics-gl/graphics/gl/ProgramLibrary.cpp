/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  ProgramLibrary.cpp

  Dmitry Vedenko

**********************************************************************/
#include "ProgramLibrary.h"

#include <string>

#include "Program.h"

namespace graphics::gl
{
namespace
{
const char* defaultVertexShader = R"(#version 150

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

const char* defaultFragmentShader = R"(#version 150

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

const char* linearGradientFragmentShader = R"(
in vec2 var_UV;
in vec4 var_MulColor;
in vec4 var_AddColor;

layout(origin_upper_left) in vec4 gl_FragCoord;

uniform vec2 c_GradientStart;
uniform vec2 c_GradientEnd;

uniform vec4 c_Colors[COLORS_COUNT];
uniform float c_Stops[COLORS_COUNT];

out vec4 fragColor;

void main()
{
   vec2 direction = c_GradientEnd - c_GradientStart;
   float directionLength = length(direction);
   vec2 normalizedDirection = normalize(direction);

   float distance = dot(normalizedDirection, gl_FragCoord.xy - c_GradientStart) / directionLength;

   vec4 outColor = c_Colors[0];

   for (int i = 0; i < COLORS_COUNT - 1; ++i)
       outColor = mix(outColor, c_Colors[i + 1], smoothstep(c_Stops[i], c_Stops[i + 1], distance));

   fragColor = outColor;
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
                                     .BindSampler("s_Texture", 0)
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

std::shared_ptr<Program>
ProgramLibrary::GetLinearGradientProgram(size_t stops) const
{
   if (stops < 2)
      return {};

   auto it = mLinearGradientPrograms.find(stops);

   if (it != mLinearGradientPrograms.end())
      return it->second;

   const std::string framentShader = "#version 150\n\n#define COLORS_COUNT " +
                                     std::to_string(stops) + "\n\n" +
                                     linearGradientFragmentShader;

   auto program = ProgramBuilder(mRenderer)
                     .AddVertexShader(defaultVertexShader)
                     .AddFragmentShader(framentShader)
                     .BindAttributeLocation("in_Position", 0)
                     .BindAttributeLocation("in_UV", 1)
                     .BindAttributeLocation("in_MulColor", 2)
                     .BindAttributeLocation("in_AddColor", 3)
                     .Build();

   if (program != nullptr)
   {
      const_cast<ProgramLibrary*>(this)->mLinearGradientPrograms.emplace(
         stops, program);
   }

   return program;
}

void ProgramLibrary::CreateProgram(ProgramID programID)
{
   mLibrary[size_t(programID)] = factories[size_t(programID)](mRenderer);
}

} // namespace graphics::gl
