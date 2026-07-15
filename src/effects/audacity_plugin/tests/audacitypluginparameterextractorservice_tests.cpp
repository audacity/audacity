/*
 * Audacity: A Digital Audio Editor
 */
#include <gtest/gtest.h>

#include <cmath>
#include <limits>
#include <memory>
#include <optional>
#include <string>
#include <utility>
#include <vector>

#include "internal/audacityplugineffect.h"
#include "internal/audacitypluginparameterextractorservice.h"

namespace au::effects {
namespace {
au::audacityplugin::Value doubleValue(double value)
{
    return value;
}

au::audacityplugin::ParameterDescriptor doubleParameter(
    std::string key, double defaultValue,
    std::optional<double> minimum = std::nullopt,
    std::optional<double> maximum = std::nullopt,
    std::string unit = {})
{
    au::audacityplugin::ParameterDescriptor result;
    result.key = std::move(key);
    result.name = result.key;
    result.type = au::audacityplugin::ParameterType::Double;
    result.unit = std::move(unit);
    result.defaultValue = doubleValue(defaultValue);
    if (minimum) {
        result.minimum = doubleValue(*minimum);
    }
    if (maximum) {
        result.maximum = doubleValue(*maximum);
    }
    return result;
}

class ParameterEffect final : public au::audacityplugin::IEffectInstance
{
public:
    ParameterEffect()
        : m_parameters{
                       doubleParameter("duration", 1.0, 0.0, std::nullopt, "s"),
                       doubleParameter("ceiling", 1.0, std::nullopt, 10.0, "dB"),
                       doubleParameter("unbounded", 1.0),
                       }
    {
        for (const auto& parameter : m_parameters) {
            m_values.push_back(parameter.defaultValue);
        }
    }

    const std::vector<au::audacityplugin::ParameterDescriptor>& parameters() const override
    {
        return m_parameters;
    }

    au::audacityplugin::Value value(uint64_t index) const override
    {
        return m_values.at(static_cast<size_t>(index));
    }

    au::audacityplugin::Status setValue(
        uint64_t index, const au::audacityplugin::Value& value) override
    {
        if (index >= m_parameters.size()
            || !std::holds_alternative<double>(value)
            || !std::isfinite(std::get<double>(value))) {
            return au::audacityplugin::Status::InvalidArgument;
        }
        m_values[static_cast<size_t>(index)] = value;
        m_lastSetIndex = index;
        m_lastSetValue = std::get<double>(value);
        return au::audacityplugin::Status::Ok;
    }

    au::audacityplugin::Status validate() override
    {
        return std::get<double>(m_values[0]) > 0.0
               ? au::audacityplugin::Status::Ok
               : au::audacityplugin::Status::ValidationFailed;
    }

    au::audacityplugin::Status apply(
        const au::audacityplugin::OfflineArgs&,
        au::audacityplugin::IOfflineHost&) override
    {
        return au::audacityplugin::Status::Ok;
    }

    uint64_t lastSetIndex() const { return m_lastSetIndex; }
    double lastSetValue() const { return m_lastSetValue; }

private:
    std::vector<au::audacityplugin::ParameterDescriptor> m_parameters;
    std::vector<au::audacityplugin::Value> m_values;
    uint64_t m_lastSetIndex = std::numeric_limits<uint64_t>::max();
    double m_lastSetValue = 0.0;
};

TEST(AudacityPluginParameterExtractorServiceTests,
     OpenDoubleBoundsProduceUsableNumericRanges)
{
    au::audacityplugin::EffectDescriptor descriptor;
    descriptor.pluginId = "org.audacityteam.test";
    descriptor.effectId = "numeric-parameters";
    AudacityPluginEffect effect { std::move(descriptor) };
    auto native = std::make_shared<ParameterEffect>();
    AudacityPluginEffectInstance instance { effect, native };
    AudacityPluginParameterExtractorService extractor;

    const auto parameters = extractor.extractParameters(&instance);
    ASSERT_EQ(parameters.size(), 3U);
    const double largest = std::numeric_limits<double>::max();

    EXPECT_EQ(parameters[0].type, ParameterType::Numeric);
    EXPECT_EQ(parameters[0].units, u"s");
    EXPECT_DOUBLE_EQ(parameters[0].minValue, 0.0);
    EXPECT_DOUBLE_EQ(parameters[0].maxValue, largest);

    EXPECT_EQ(parameters[1].type, ParameterType::Numeric);
    EXPECT_EQ(parameters[1].units, u"dB");
    EXPECT_DOUBLE_EQ(parameters[1].minValue, -largest);
    EXPECT_DOUBLE_EQ(parameters[1].maxValue, 10.0);

    EXPECT_EQ(parameters[2].type, ParameterType::Numeric);
    EXPECT_TRUE(parameters[2].units.empty());
    EXPECT_DOUBLE_EQ(parameters[2].minValue, -largest);
    EXPECT_DOUBLE_EQ(parameters[2].maxValue, largest);

    EXPECT_TRUE(extractor.setParameterValue(&instance, u"duration", 2.5));
    EXPECT_EQ(native->lastSetIndex(), 0U);
    EXPECT_DOUBLE_EQ(native->lastSetValue(), 2.5);
}

TEST(AudacityPluginParameterExtractorServiceTests,
     StandardDurationIsHostOwnedAndStoredInEffectSettingsExtra)
{
    au::audacityplugin::EffectDescriptor descriptor;
    descriptor.pluginId = "org.audacityteam.test";
    descriptor.effectId = "generator";
    descriptor.group = au::audacityplugin::PresentationGroup::Generate;
    AudacityPluginEffect effect { std::move(descriptor) };
    auto native = std::make_shared<ParameterEffect>();
    AudacityPluginEffectInstance instance { effect, native };
    AudacityPluginParameterExtractorService extractor;

    EffectSettings original = effect.MakeSettings();
    original.extra.SetDuration(7.0);
    EffectSettings working = original;
    working.extra.SetDuration(2.75);
    auto access = std::make_shared<SimpleEffectSettingsAccess>(working);

    const auto parameters = extractor.extractParameters(&instance, access);
    ASSERT_EQ(parameters.size(), 4U);
    const auto& duration = parameters.back();
    EXPECT_EQ(duration.id, u"audacity.generator-duration");
    EXPECT_EQ(duration.type, ParameterType::Time);
    EXPECT_DOUBLE_EQ(duration.currentValue, 2.75);
    EXPECT_DOUBLE_EQ(original.extra.GetDuration(), 7.0);
    const auto lastNativeParameterSet = native->lastSetIndex();

    EXPECT_TRUE(extractor.setParameterValue(
                    &instance, duration.id, 3.5, access));
    EXPECT_DOUBLE_EQ(access->Get().extra.GetDuration(), 3.5);
    EXPECT_EQ(native->lastSetIndex(), lastNativeParameterSet);

    EXPECT_TRUE(extractor.setParameterValue(
                    &instance, duration.id, 0.0, access));
    EXPECT_DOUBLE_EQ(access->Get().extra.GetDuration(), 0.0);
}
} // namespace
} // namespace au::effects
