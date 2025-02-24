#pragma once

namespace au::projectscene {
struct SampleData {
    std::vector<double> y {};
    std::vector<double> x {};

    SampleData() = default;

    SampleData(std::vector<double> pY, std::vector<double> pX)
        : y(std::move(pY)), x(std::move(pX)) {}

    size_t size() const { return x.size(); }
};
}
