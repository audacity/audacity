#pragma once

namespace au::projectscene {
struct SampleData {
    std::vector<int> y {};
    std::vector<int> x {};

    SampleData() = default;

    SampleData(std::vector<int> pY, std::vector<int> pX)
        : y(std::move(pY)), x(std::move(pX)) {}

    size_t size() const { return x.size(); }
};
}
