#pragma once

namespace au::projectscene {
struct SampleData {
    std::vector<double> y {};
    std::vector<double> x {};
    std::vector<int> clippedX {};

    SampleData() = default;

    SampleData(std::vector<double> pY, std::vector<double> pX, std::vector<int> pClippedX)
        : y(std::move(pY)), x(std::move(pX)), clippedX(std::move(pClippedX)) {}

    size_t size() const { return x.size(); }
};
}
