/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "types/val.h"

namespace au::importexport::utils {
muse::Val matrixToVal(const std::vector<std::vector<bool> >& matrix);
std::vector<std::vector<bool> > valToMatrix(const muse::Val& val);
}
