#include "wave.h"

#include <cstring>

using namespace au::trackedit;
Wave::Wave(const int16_t* data, size_t size)
{
    m_data = std::make_shared<Data>();
    m_data->resize(size + 1);
    m_data->operator [](size) = 0;
    std::memcpy(m_data->data(), data, size);
}

void Wave::detach()
{
    if (!m_data) {
        return;
    }

    if (m_data.use_count() == 1) {
        return;
    }

    m_data = std::make_shared<Data>(*m_data);
}

size_t Wave::size() const
{
    return m_data->size();
}

const int16_t* Wave::constData() const
{
    return m_data->data();
}

int16_t Wave::operator[](size_t pos) const
{
    if (pos < size()) {
        return m_data->operator [](pos);
    }
    return 0;
}

int16_t& Wave::operator[](size_t pos)
{
    detach();

    if (pos < size()) {
        return m_data->operator [](pos);
    }

    static int16_t _dummy;
    return _dummy;
}
