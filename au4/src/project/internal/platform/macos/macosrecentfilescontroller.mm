/*
 * SPDX-License-Identifier: GPL-3.0-only
 * Audacity-CLA-applies
 *
 * Audacity
 * Music Composition & Notation
 *
 * Copyright (C) 2024 Audacity Limited
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License version 3 as
 * published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */
#include <Cocoa/Cocoa.h>

#include "macosrecentfilescontroller.h"

using namespace au::project;

void MacOSRecentFilesController::prependPlatformRecentFile(const muse::io::path_t& path)
{
    NSString* string = path.toQString().toNSString();
    NSURL* url = [NSURL fileURLWithPath:string];
    [[NSDocumentController sharedDocumentController] noteNewRecentDocumentURL:url];
}

void MacOSRecentFilesController::clearPlatformRecentFiles()
{
    [[NSDocumentController sharedDocumentController] clearRecentDocuments:nil];
}
