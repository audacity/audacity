/*
 * Audacity: A Digital Audio Editor
 */
#include "projectdocument.h"

#include <cmath>

using namespace au::projectrecovery;

namespace {
double realAttribute(const XmlNode& node, std::string_view name, double fallback = 0)
{
    const XmlValue* value = node.attribute(name);
    const auto real = value ? value->toDouble() : std::nullopt;
    return real.value_or(fallback);
}

int64_t integerAttribute(const XmlNode& node, std::string_view name, int64_t fallback = 0)
{
    const XmlValue* value = node.attribute(name);
    const auto integer = value ? value->toInteger() : std::nullopt;
    return integer.value_or(fallback);
}

std::string textAttribute(const XmlNode& node, std::string_view name)
{
    const XmlValue* value = node.attribute(name);
    return value && value->type == FieldType::String ? value->text : std::string();
}

Sequence readSequence(const XmlNode& node)
{
    Sequence sequence;
    sequence.maxSamples = integerAttribute(node, "maxsamples");
    sequence.sampleFormat = static_cast<uint32_t>(integerAttribute(node, "sampleformat"));
    sequence.numSamples = integerAttribute(node, "numsamples");
    for (const XmlNode& child : node.children) {
        if (child.tag == "waveblock") {
            const XmlValue* blockId = child.attribute("blockid");
            sequence.blocks.push_back({ integerAttribute(child, "start"), integerAttribute(child, "blockid"), 0,
                                        blockId ? blockId->offset : 0 });
        }
    }
    for (size_t i = 0; i < sequence.blocks.size(); ++i) {
        const int64_t end = i + 1 < sequence.blocks.size() ? sequence.blocks[i + 1].start : sequence.numSamples;
        sequence.blocks[i].length = end - sequence.blocks[i].start;
    }
    return sequence;
}

WaveClip readClip(const XmlNode& node)
{
    WaveClip clip;
    clip.name = textAttribute(node, "name");
    clip.offset = realAttribute(node, "offset");
    clip.trimLeft = realAttribute(node, "trimLeft");
    clip.trimRight = realAttribute(node, "trimRight");
    clip.stretchRatio = realAttribute(node, "clipStretchRatio", 1);
    if (const XmlValue* value = node.attribute("rawAudioTempo")) {
        const auto tempo = value->toDouble();
        if (tempo && *tempo != 0) {
            clip.rawAudioTempo = tempo;
        }
    }
    if (const XmlValue* value = node.attribute("clipTempo")) {
        clip.clipTempo = value->toDouble();
    }
    if (const XmlValue* value = node.attribute("clipStretchToMatchTempo")) {
        clip.stretchToMatchTempo = value->toBool().value_or(true);
    }
    for (const XmlNode& child : node.children) {
        if (child.tag == "sequence") {
            clip.sequences.push_back(readSequence(child));
        } else if (child.tag == "waveclip") {
            clip.cutLines.push_back(readClip(child));
        }
    }
    return clip;
}
}

std::optional<ProjectDocument> au::projectrecovery::readProjectDocument(const XmlDocument& document)
{
    const XmlNode* root = nullptr;
    for (const XmlNode& element : document.elements) {
        if (element.tag == "project") {
            root = &element;
        }
    }
    if (!root) {
        return std::nullopt;
    }

    ProjectDocument project;
    project.version = textAttribute(*root, "version");
    project.audacityVersion = textAttribute(*root, "audacityversion");
    project.rate = realAttribute(*root, "rate");
    project.tempo = realAttribute(*root, "time_signature_tempo", 120);
    for (const XmlNode& child : root->children) {
        if (child.tag != "wavetrack") {
            continue;
        }
        WaveTrack track;
        track.name = textAttribute(child, "name");
        track.channel = static_cast<int>(integerAttribute(child, "channel"));
        track.linked = integerAttribute(child, "linked");
        track.rate = realAttribute(child, "rate", project.rate);
        for (const XmlNode& clipNode : child.children) {
            if (clipNode.tag == "waveclip") {
                track.clips.push_back(readClip(clipNode));
            }
        }
        project.waveTracks.push_back(std::move(track));
    }
    return project;
}

double au::projectrecovery::effectiveStretchRatio(const WaveClip& clip, const ProjectDocument& project)
{
    double ratio = clip.stretchRatio;
    if (!clip.rawAudioTempo) {
        return ratio;
    }
    std::optional<double> tempo = clip.clipTempo;
    if (!tempo && clip.stretchToMatchTempo) {
        tempo = project.tempo;
    }
    const double rawAudioTempo = *clip.rawAudioTempo;
    if (tempo && std::isfinite(*tempo) && *tempo > 0 && std::isfinite(rawAudioTempo) && rawAudioTempo > 0) {
        ratio *= rawAudioTempo / *tempo;
    }
    return ratio;
}
