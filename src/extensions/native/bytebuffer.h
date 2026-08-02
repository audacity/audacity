/*
 * SPDX-License-Identifier: GPL-3.0-only
 * MuseScore-CLA-applies
 */
#pragma once

#include <memory>

#include <QByteArray>
#include <QObject>

namespace muse::extensions {
class ByteBuffer final : public QObject
{
    Q_OBJECT
    Q_PROPERTY(qulonglong byteLength READ byteLength CONSTANT)

public:
    explicit ByteBuffer(size_t byteLength, QObject* parent = nullptr);

    qulonglong byteLength() const;
    Q_INVOKABLE QByteArray copyToArrayBuffer() const;
    Q_INVOKABLE void copyFromArrayBuffer(const QByteArray& buffer);
    void* data();
    const void* data() const;
    std::shared_ptr<QByteArray> storage() const;
    bool isExclusive() const;

private:
    std::shared_ptr<QByteArray> m_data;
};
} // namespace muse::extensions
