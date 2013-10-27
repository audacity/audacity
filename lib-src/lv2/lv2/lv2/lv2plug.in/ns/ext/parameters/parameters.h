/*
  Copyright 2012 David Robillard <http://drobilla.net>

  Permission to use, copy, modify, and/or distribute this software for any
  purpose with or without fee is hereby granted, provided that the above
  copyright notice and this permission notice appear in all copies.

  THIS SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
  WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
  MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
  ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
  ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
  OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
*/

#ifndef LV2_PARAMETERS_H
#define LV2_PARAMETERS_H

#define LV2_PARAMETERS_URI    "http://lv2plug.in/ns/ext/parameters"
#define LV2_PARAMETERS_PREFIX LV2_PARAMETERS_URI "#"

#define LV2_PARAMETERS__CompressorControls LV2_PARAMETERS_PREFIX "CompressorControls"
#define LV2_PARAMETERS__ControlGroup       LV2_PARAMETERS_PREFIX "ControlGroup"
#define LV2_PARAMETERS__EnvelopeControls   LV2_PARAMETERS_PREFIX "EnvelopeControls"
#define LV2_PARAMETERS__FilterControls     LV2_PARAMETERS_PREFIX "FilterControls"
#define LV2_PARAMETERS__OscillatorControls LV2_PARAMETERS_PREFIX "OscillatorControls"
#define LV2_PARAMETERS__amplitude          LV2_PARAMETERS_PREFIX "amplitude"
#define LV2_PARAMETERS__attack             LV2_PARAMETERS_PREFIX "attack"
#define LV2_PARAMETERS__bypass             LV2_PARAMETERS_PREFIX "bypass"
#define LV2_PARAMETERS__cutoffFrequency    LV2_PARAMETERS_PREFIX "cutoffFrequency"
#define LV2_PARAMETERS__decay              LV2_PARAMETERS_PREFIX "decay"
#define LV2_PARAMETERS__delay              LV2_PARAMETERS_PREFIX "delay"
#define LV2_PARAMETERS__dryLevel           LV2_PARAMETERS_PREFIX "dryLevel"
#define LV2_PARAMETERS__frequency          LV2_PARAMETERS_PREFIX "frequency"
#define LV2_PARAMETERS__gain               LV2_PARAMETERS_PREFIX "gain"
#define LV2_PARAMETERS__hold               LV2_PARAMETERS_PREFIX "hold"
#define LV2_PARAMETERS__pulseWidth         LV2_PARAMETERS_PREFIX "pulseWidth"
#define LV2_PARAMETERS__ratio              LV2_PARAMETERS_PREFIX "ratio"
#define LV2_PARAMETERS__release            LV2_PARAMETERS_PREFIX "release"
#define LV2_PARAMETERS__resonance          LV2_PARAMETERS_PREFIX "resonance"
#define LV2_PARAMETERS__sampleRate         LV2_PARAMETERS_PREFIX "sampleRate"
#define LV2_PARAMETERS__sustain            LV2_PARAMETERS_PREFIX "sustain"
#define LV2_PARAMETERS__threshold          LV2_PARAMETERS_PREFIX "threshold"
#define LV2_PARAMETERS__waveform           LV2_PARAMETERS_PREFIX "waveform"
#define LV2_PARAMETERS__wetDryRatio        LV2_PARAMETERS_PREFIX "wetDryRatio"
#define LV2_PARAMETERS__wetLevel           LV2_PARAMETERS_PREFIX "wetLevel"

#endif  /* LV2_PARAMETERS_H */
