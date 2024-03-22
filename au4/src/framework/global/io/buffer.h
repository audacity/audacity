/*
 * SPDX-License-Identifier: GPL-3.0-only
 * MuseScore-CLA-applies
 *
 * MuseScore
 * Music Composition & Notation
 *
 * Copyright (C) 2021 MuseScore BVBA and others
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
#ifndef MU_IO_BUFFER_H
#define MU_IO_BUFFER_H

#include "../types/bytearray.h"
#include "iodevice.h"

namespace mu::io {
class Buffer : public IODevice
{
public:
    Buffer();
    Buffer(size_t size);
    Buffer(const uint8_t* data, size_t size);
    Buffer(ByteArray* ba);
    Buffer(ByteArray&& ba);

    const ByteArray& data() const;

protected:

    bool doOpen(OpenMode m) override;
    size_t dataSize() const override;
    const uint8_t* rawData() const override;
    bool resizeData(size_t size) override;
    size_t writeData(const uint8_t* data, size_t len) override;

private:

    ByteArray* m_ref = nullptr;
    ByteArray m_ba;
};
}

#endif // MU_IO_BUFFER_H
