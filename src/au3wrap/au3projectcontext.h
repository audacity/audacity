/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "framework/global/modularity/ioc.h"

class AudacityProject;

namespace au::au3 {
void setProjectIocContext(AudacityProject& project, const muse::modularity::ContextPtr& context);

muse::modularity::ContextPtr projectIocContext(AudacityProject& project);
} // namespace au::au3
