# This file is part of KDDockWidgets.
#
# SPDX-FileCopyrightText: 2020-2021 Klarälvdalens Datakonsult AB, a KDAB Group company <info@kdab.com>
# Author: Sergio Martins <sergio.martins@kdab.com>
#
# SPDX-License-Identifier: GPL-2.0-only OR GPL-3.0-only
#
# Contact KDAB at <info@kdab.com> for commercial licensing options.
#

add_subdirectory(customtitlebar)
add_subdirectory(dockwidgets)
add_subdirectory(mdi)

set_compiler_flags(kddockwidgets_example_quick)
set_compiler_flags(kddockwidgets_example_mdi_quick)
set_compiler_flags(kddockwidgets_customtitlebar_quick)
