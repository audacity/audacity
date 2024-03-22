/*
  This file is part of KDDockWidgets.

  SPDX-FileCopyrightText: 2019-2021 Klarälvdalens Datakonsult AB, a KDAB Group company <info@kdab.com>
  Author: Sérgio Martins <sergio.martins@kdab.com>

  SPDX-License-Identifier: GPL-2.0-only OR GPL-3.0-only

  Contact KDAB at <info@kdab.com> for commercial licensing options.
*/

#include "NullIndicators_p.h"

namespace KDDockWidgets {

NullIndicators::NullIndicators(DropArea *dropArea)
    : DropIndicatorOverlayInterface(dropArea)
{
}

NullIndicators::~NullIndicators() = default;

}
