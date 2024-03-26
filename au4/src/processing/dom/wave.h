#ifndef AU_PROСESSING_WAVE_H
#define AU_PROСESSING_WAVE_H

#include <memory>
#include <vector>

namespace au::processing {
class Wave
{
public:
    Wave() = default;
    Wave(const int16_t* data, size_t size);

    size_t size() const;
    const int16_t* constData() const;

    int16_t operator[](size_t pos) const;
    int16_t& operator[](size_t pos);

private:

    void detach();

    using Data = std::vector<int16_t>;
    std::shared_ptr<Data> m_data;
};
}

#endif // AU_PROСESSING_WAVE_H
