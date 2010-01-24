<?xml version='1.0' encoding='ISO-8859-1' standalone='yes'?>
<tagfile>
  <compound kind="page">
    <filename>index</filename>
    <title></title>
    <name>index</name>
    <docanchor>cpp_api</docanchor>
    <docanchor>intro</docanchor>
    <docanchor>getting_started</docanchor>
    <docanchor>c_api</docanchor>
    <docanchor>embedded_developers</docanchor>
  </compound>
  <compound kind="file">
    <name>decoder.h</name>
    <path>/home/jcoalson/flac/build/include/FLAC++/</path>
    <filename>FLAC++_2decoder_8h.html</filename>
  </compound>
  <compound kind="file">
    <name>decoder.h</name>
    <path>/home/jcoalson/flac/build/include/OggFLAC++/</path>
    <filename>OggFLAC++_2decoder_8h.html</filename>
  </compound>
  <compound kind="file">
    <name>encoder.h</name>
    <path>/home/jcoalson/flac/build/include/FLAC++/</path>
    <filename>FLAC++_2encoder_8h.html</filename>
  </compound>
  <compound kind="file">
    <name>encoder.h</name>
    <path>/home/jcoalson/flac/build/include/OggFLAC++/</path>
    <filename>OggFLAC++_2encoder_8h.html</filename>
  </compound>
  <compound kind="file">
    <name>file_decoder.h</name>
    <path>/home/jcoalson/flac/build/include/FLAC/</path>
    <filename>file__decoder_8h.html</filename>
    <class kind="struct">FLAC__FileDecoder</class>
    <member kind="typedef">
      <type>FLAC__StreamDecoderWriteStatus(*</type>
      <name>FLAC__FileDecoderWriteCallback</name>
      <anchor>a1</anchor>
      <arglist>)(const FLAC__FileDecoder *decoder, const FLAC__Frame *frame, const FLAC__int32 *const buffer[], void *client_data)</arglist>
    </member>
    <member kind="typedef">
      <type>void(*</type>
      <name>FLAC__FileDecoderMetadataCallback</name>
      <anchor>a2</anchor>
      <arglist>)(const FLAC__FileDecoder *decoder, const FLAC__StreamMetadata *metadata, void *client_data)</arglist>
    </member>
    <member kind="typedef">
      <type>void(*</type>
      <name>FLAC__FileDecoderErrorCallback</name>
      <anchor>a3</anchor>
      <arglist>)(const FLAC__FileDecoder *decoder, FLAC__StreamDecoderErrorStatus status, void *client_data)</arglist>
    </member>
    <member kind="enumeration">
      <name>FLAC__FileDecoderState</name>
      <anchor>a34</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__FILE_DECODER_OK</name>
      <anchor>a34a4</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__FILE_DECODER_END_OF_FILE</name>
      <anchor>a34a5</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__FILE_DECODER_ERROR_OPENING_FILE</name>
      <anchor>a34a6</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__FILE_DECODER_MEMORY_ALLOCATION_ERROR</name>
      <anchor>a34a7</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__FILE_DECODER_SEEK_ERROR</name>
      <anchor>a34a8</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__FILE_DECODER_SEEKABLE_STREAM_DECODER_ERROR</name>
      <anchor>a34a9</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__FILE_DECODER_ALREADY_INITIALIZED</name>
      <anchor>a34a10</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__FILE_DECODER_INVALID_CALLBACK</name>
      <anchor>a34a11</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__FILE_DECODER_UNINITIALIZED</name>
      <anchor>a34a12</anchor>
      <arglist></arglist>
    </member>
    <member kind="function">
      <type>FLAC__FileDecoder *</type>
      <name>FLAC__file_decoder_new</name>
      <anchor>a4</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>FLAC__file_decoder_delete</name>
      <anchor>a5</anchor>
      <arglist>(FLAC__FileDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__file_decoder_set_md5_checking</name>
      <anchor>a6</anchor>
      <arglist>(FLAC__FileDecoder *decoder, FLAC__bool value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__file_decoder_set_filename</name>
      <anchor>a7</anchor>
      <arglist>(FLAC__FileDecoder *decoder, const char *value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__file_decoder_set_write_callback</name>
      <anchor>a8</anchor>
      <arglist>(FLAC__FileDecoder *decoder, FLAC__FileDecoderWriteCallback value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__file_decoder_set_metadata_callback</name>
      <anchor>a9</anchor>
      <arglist>(FLAC__FileDecoder *decoder, FLAC__FileDecoderMetadataCallback value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__file_decoder_set_error_callback</name>
      <anchor>a10</anchor>
      <arglist>(FLAC__FileDecoder *decoder, FLAC__FileDecoderErrorCallback value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__file_decoder_set_client_data</name>
      <anchor>a11</anchor>
      <arglist>(FLAC__FileDecoder *decoder, void *value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__file_decoder_set_metadata_respond</name>
      <anchor>a12</anchor>
      <arglist>(FLAC__FileDecoder *decoder, FLAC__MetadataType type)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__file_decoder_set_metadata_respond_application</name>
      <anchor>a13</anchor>
      <arglist>(FLAC__FileDecoder *decoder, const FLAC__byte id[4])</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__file_decoder_set_metadata_respond_all</name>
      <anchor>a14</anchor>
      <arglist>(FLAC__FileDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__file_decoder_set_metadata_ignore</name>
      <anchor>a15</anchor>
      <arglist>(FLAC__FileDecoder *decoder, FLAC__MetadataType type)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__file_decoder_set_metadata_ignore_application</name>
      <anchor>a16</anchor>
      <arglist>(FLAC__FileDecoder *decoder, const FLAC__byte id[4])</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__file_decoder_set_metadata_ignore_all</name>
      <anchor>a17</anchor>
      <arglist>(FLAC__FileDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__FileDecoderState</type>
      <name>FLAC__file_decoder_get_state</name>
      <anchor>a18</anchor>
      <arglist>(const FLAC__FileDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__SeekableStreamDecoderState</type>
      <name>FLAC__file_decoder_get_seekable_stream_decoder_state</name>
      <anchor>a19</anchor>
      <arglist>(const FLAC__FileDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__StreamDecoderState</type>
      <name>FLAC__file_decoder_get_stream_decoder_state</name>
      <anchor>a20</anchor>
      <arglist>(const FLAC__FileDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__file_decoder_get_md5_checking</name>
      <anchor>a21</anchor>
      <arglist>(const FLAC__FileDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>FLAC__file_decoder_get_channels</name>
      <anchor>a22</anchor>
      <arglist>(const FLAC__FileDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__ChannelAssignment</type>
      <name>FLAC__file_decoder_get_channel_assignment</name>
      <anchor>a23</anchor>
      <arglist>(const FLAC__FileDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>FLAC__file_decoder_get_bits_per_sample</name>
      <anchor>a24</anchor>
      <arglist>(const FLAC__FileDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>FLAC__file_decoder_get_sample_rate</name>
      <anchor>a25</anchor>
      <arglist>(const FLAC__FileDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>FLAC__file_decoder_get_blocksize</name>
      <anchor>a26</anchor>
      <arglist>(const FLAC__FileDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__FileDecoderState</type>
      <name>FLAC__file_decoder_init</name>
      <anchor>a27</anchor>
      <arglist>(FLAC__FileDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__file_decoder_finish</name>
      <anchor>a28</anchor>
      <arglist>(FLAC__FileDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__file_decoder_process_single</name>
      <anchor>a29</anchor>
      <arglist>(FLAC__FileDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__file_decoder_process_until_end_of_metadata</name>
      <anchor>a30</anchor>
      <arglist>(FLAC__FileDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__file_decoder_process_until_end_of_file</name>
      <anchor>a31</anchor>
      <arglist>(FLAC__FileDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__file_decoder_process_remaining_frames</name>
      <anchor>a32</anchor>
      <arglist>(FLAC__FileDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__file_decoder_seek_absolute</name>
      <anchor>a33</anchor>
      <arglist>(FLAC__FileDecoder *decoder, FLAC__uint64 sample)</arglist>
    </member>
    <member kind="variable">
      <type>const char *const</type>
      <name>FLAC__FileDecoderStateString</name>
      <anchor>a0</anchor>
      <arglist>[]</arglist>
    </member>
  </compound>
  <compound kind="file">
    <name>file_encoder.h</name>
    <path>/home/jcoalson/flac/build/include/FLAC/</path>
    <filename>file__encoder_8h.html</filename>
    <class kind="struct">FLAC__FileEncoder</class>
    <member kind="typedef">
      <type>void(*</type>
      <name>FLAC__FileEncoderProgressCallback</name>
      <anchor>a1</anchor>
      <arglist>)(const FLAC__FileEncoder *encoder, FLAC__uint64 bytes_written, FLAC__uint64 samples_written, unsigned frames_written, unsigned total_frames_estimate, void *client_data)</arglist>
    </member>
    <member kind="enumeration">
      <name>FLAC__FileEncoderState</name>
      <anchor>a51</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__FILE_ENCODER_OK</name>
      <anchor>a51a2</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__FILE_ENCODER_NO_FILENAME</name>
      <anchor>a51a3</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__FILE_ENCODER_SEEKABLE_STREAM_ENCODER_ERROR</name>
      <anchor>a51a4</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__FILE_ENCODER_FATAL_ERROR_WHILE_WRITING</name>
      <anchor>a51a5</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__FILE_ENCODER_ERROR_OPENING_FILE</name>
      <anchor>a51a6</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__FILE_ENCODER_MEMORY_ALLOCATION_ERROR</name>
      <anchor>a51a7</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__FILE_ENCODER_ALREADY_INITIALIZED</name>
      <anchor>a51a8</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__FILE_ENCODER_UNINITIALIZED</name>
      <anchor>a51a9</anchor>
      <arglist></arglist>
    </member>
    <member kind="function">
      <type>FLAC__FileEncoder *</type>
      <name>FLAC__file_encoder_new</name>
      <anchor>a2</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>FLAC__file_encoder_delete</name>
      <anchor>a3</anchor>
      <arglist>(FLAC__FileEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__file_encoder_set_verify</name>
      <anchor>a4</anchor>
      <arglist>(FLAC__FileEncoder *encoder, FLAC__bool value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__file_encoder_set_streamable_subset</name>
      <anchor>a5</anchor>
      <arglist>(FLAC__FileEncoder *encoder, FLAC__bool value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__file_encoder_set_do_mid_side_stereo</name>
      <anchor>a6</anchor>
      <arglist>(FLAC__FileEncoder *encoder, FLAC__bool value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__file_encoder_set_loose_mid_side_stereo</name>
      <anchor>a7</anchor>
      <arglist>(FLAC__FileEncoder *encoder, FLAC__bool value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__file_encoder_set_channels</name>
      <anchor>a8</anchor>
      <arglist>(FLAC__FileEncoder *encoder, unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__file_encoder_set_bits_per_sample</name>
      <anchor>a9</anchor>
      <arglist>(FLAC__FileEncoder *encoder, unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__file_encoder_set_sample_rate</name>
      <anchor>a10</anchor>
      <arglist>(FLAC__FileEncoder *encoder, unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__file_encoder_set_blocksize</name>
      <anchor>a11</anchor>
      <arglist>(FLAC__FileEncoder *encoder, unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__file_encoder_set_max_lpc_order</name>
      <anchor>a12</anchor>
      <arglist>(FLAC__FileEncoder *encoder, unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__file_encoder_set_qlp_coeff_precision</name>
      <anchor>a13</anchor>
      <arglist>(FLAC__FileEncoder *encoder, unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__file_encoder_set_do_qlp_coeff_prec_search</name>
      <anchor>a14</anchor>
      <arglist>(FLAC__FileEncoder *encoder, FLAC__bool value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__file_encoder_set_do_escape_coding</name>
      <anchor>a15</anchor>
      <arglist>(FLAC__FileEncoder *encoder, FLAC__bool value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__file_encoder_set_do_exhaustive_model_search</name>
      <anchor>a16</anchor>
      <arglist>(FLAC__FileEncoder *encoder, FLAC__bool value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__file_encoder_set_min_residual_partition_order</name>
      <anchor>a17</anchor>
      <arglist>(FLAC__FileEncoder *encoder, unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__file_encoder_set_max_residual_partition_order</name>
      <anchor>a18</anchor>
      <arglist>(FLAC__FileEncoder *encoder, unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__file_encoder_set_rice_parameter_search_dist</name>
      <anchor>a19</anchor>
      <arglist>(FLAC__FileEncoder *encoder, unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__file_encoder_set_total_samples_estimate</name>
      <anchor>a20</anchor>
      <arglist>(FLAC__FileEncoder *encoder, FLAC__uint64 value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__file_encoder_set_metadata</name>
      <anchor>a21</anchor>
      <arglist>(FLAC__FileEncoder *encoder, FLAC__StreamMetadata **metadata, unsigned num_blocks)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__file_encoder_set_filename</name>
      <anchor>a22</anchor>
      <arglist>(FLAC__FileEncoder *encoder, const char *value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__file_encoder_set_progress_callback</name>
      <anchor>a23</anchor>
      <arglist>(FLAC__FileEncoder *encoder, FLAC__FileEncoderProgressCallback value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__file_encoder_set_client_data</name>
      <anchor>a24</anchor>
      <arglist>(FLAC__FileEncoder *encoder, void *value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__FileEncoderState</type>
      <name>FLAC__file_encoder_get_state</name>
      <anchor>a25</anchor>
      <arglist>(const FLAC__FileEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__SeekableStreamEncoderState</type>
      <name>FLAC__file_encoder_get_seekable_stream_encoder_state</name>
      <anchor>a26</anchor>
      <arglist>(const FLAC__FileEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__StreamEncoderState</type>
      <name>FLAC__file_encoder_get_stream_encoder_state</name>
      <anchor>a27</anchor>
      <arglist>(const FLAC__FileEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__StreamDecoderState</type>
      <name>FLAC__file_encoder_get_verify_decoder_state</name>
      <anchor>a28</anchor>
      <arglist>(const FLAC__FileEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>FLAC__file_encoder_get_verify_decoder_error_stats</name>
      <anchor>a29</anchor>
      <arglist>(const FLAC__FileEncoder *encoder, FLAC__uint64 *absolute_sample, unsigned *frame_number, unsigned *channel, unsigned *sample, FLAC__int32 *expected, FLAC__int32 *got)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__file_encoder_get_verify</name>
      <anchor>a30</anchor>
      <arglist>(const FLAC__FileEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__file_encoder_get_streamable_subset</name>
      <anchor>a31</anchor>
      <arglist>(const FLAC__FileEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__file_encoder_get_do_mid_side_stereo</name>
      <anchor>a32</anchor>
      <arglist>(const FLAC__FileEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__file_encoder_get_loose_mid_side_stereo</name>
      <anchor>a33</anchor>
      <arglist>(const FLAC__FileEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>FLAC__file_encoder_get_channels</name>
      <anchor>a34</anchor>
      <arglist>(const FLAC__FileEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>FLAC__file_encoder_get_bits_per_sample</name>
      <anchor>a35</anchor>
      <arglist>(const FLAC__FileEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>FLAC__file_encoder_get_sample_rate</name>
      <anchor>a36</anchor>
      <arglist>(const FLAC__FileEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>FLAC__file_encoder_get_blocksize</name>
      <anchor>a37</anchor>
      <arglist>(const FLAC__FileEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>FLAC__file_encoder_get_max_lpc_order</name>
      <anchor>a38</anchor>
      <arglist>(const FLAC__FileEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>FLAC__file_encoder_get_qlp_coeff_precision</name>
      <anchor>a39</anchor>
      <arglist>(const FLAC__FileEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__file_encoder_get_do_qlp_coeff_prec_search</name>
      <anchor>a40</anchor>
      <arglist>(const FLAC__FileEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__file_encoder_get_do_escape_coding</name>
      <anchor>a41</anchor>
      <arglist>(const FLAC__FileEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__file_encoder_get_do_exhaustive_model_search</name>
      <anchor>a42</anchor>
      <arglist>(const FLAC__FileEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>FLAC__file_encoder_get_min_residual_partition_order</name>
      <anchor>a43</anchor>
      <arglist>(const FLAC__FileEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>FLAC__file_encoder_get_max_residual_partition_order</name>
      <anchor>a44</anchor>
      <arglist>(const FLAC__FileEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>FLAC__file_encoder_get_rice_parameter_search_dist</name>
      <anchor>a45</anchor>
      <arglist>(const FLAC__FileEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__uint64</type>
      <name>FLAC__file_encoder_get_total_samples_estimate</name>
      <anchor>a46</anchor>
      <arglist>(const FLAC__FileEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__FileEncoderState</type>
      <name>FLAC__file_encoder_init</name>
      <anchor>a47</anchor>
      <arglist>(FLAC__FileEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>FLAC__file_encoder_finish</name>
      <anchor>a48</anchor>
      <arglist>(FLAC__FileEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__file_encoder_process</name>
      <anchor>a49</anchor>
      <arglist>(FLAC__FileEncoder *encoder, const FLAC__int32 *const buffer[], unsigned samples)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__file_encoder_process_interleaved</name>
      <anchor>a50</anchor>
      <arglist>(FLAC__FileEncoder *encoder, const FLAC__int32 buffer[], unsigned samples)</arglist>
    </member>
    <member kind="variable">
      <type>const char *const</type>
      <name>FLAC__FileEncoderStateString</name>
      <anchor>a0</anchor>
      <arglist>[]</arglist>
    </member>
  </compound>
  <compound kind="file">
    <name>format.h</name>
    <path>/home/jcoalson/flac/build/include/FLAC/</path>
    <filename>format_8h.html</filename>
    <class kind="struct">FLAC__EntropyCodingMethod_PartitionedRiceContents</class>
    <class kind="struct">FLAC__EntropyCodingMethod_PartitionedRice</class>
    <class kind="struct">FLAC__EntropyCodingMethod</class>
    <class kind="struct">FLAC__Subframe_Constant</class>
    <class kind="struct">FLAC__Subframe_Verbatim</class>
    <class kind="struct">FLAC__Subframe_Fixed</class>
    <class kind="struct">FLAC__Subframe_LPC</class>
    <class kind="struct">FLAC__Subframe</class>
    <class kind="struct">FLAC__FrameHeader</class>
    <class kind="struct">FLAC__FrameFooter</class>
    <class kind="struct">FLAC__Frame</class>
    <class kind="struct">FLAC__StreamMetadata_StreamInfo</class>
    <class kind="struct">FLAC__StreamMetadata_Padding</class>
    <class kind="struct">FLAC__StreamMetadata_Application</class>
    <class kind="struct">FLAC__StreamMetadata_SeekPoint</class>
    <class kind="struct">FLAC__StreamMetadata_SeekTable</class>
    <class kind="struct">FLAC__StreamMetadata_VorbisComment_Entry</class>
    <class kind="struct">FLAC__StreamMetadata_VorbisComment</class>
    <class kind="struct">FLAC__StreamMetadata</class>
    <member kind="define">
      <type>#define</type>
      <name>FLAC__MIN_BLOCK_SIZE</name>
      <anchor>a56</anchor>
      <arglist></arglist>
    </member>
    <member kind="define">
      <type>#define</type>
      <name>FLAC__MAX_BLOCK_SIZE</name>
      <anchor>a57</anchor>
      <arglist></arglist>
    </member>
    <member kind="define">
      <type>#define</type>
      <name>FLAC__MAX_CHANNELS</name>
      <anchor>a58</anchor>
      <arglist></arglist>
    </member>
    <member kind="define">
      <type>#define</type>
      <name>FLAC__MIN_BITS_PER_SAMPLE</name>
      <anchor>a59</anchor>
      <arglist></arglist>
    </member>
    <member kind="define">
      <type>#define</type>
      <name>FLAC__MAX_BITS_PER_SAMPLE</name>
      <anchor>a60</anchor>
      <arglist></arglist>
    </member>
    <member kind="define">
      <type>#define</type>
      <name>FLAC__REFERENCE_CODEC_MAX_BITS_PER_SAMPLE</name>
      <anchor>a61</anchor>
      <arglist></arglist>
    </member>
    <member kind="define">
      <type>#define</type>
      <name>FLAC__MAX_SAMPLE_RATE</name>
      <anchor>a62</anchor>
      <arglist></arglist>
    </member>
    <member kind="define">
      <type>#define</type>
      <name>FLAC__MAX_LPC_ORDER</name>
      <anchor>a63</anchor>
      <arglist></arglist>
    </member>
    <member kind="define">
      <type>#define</type>
      <name>FLAC__MIN_QLP_COEFF_PRECISION</name>
      <anchor>a64</anchor>
      <arglist></arglist>
    </member>
    <member kind="define">
      <type>#define</type>
      <name>FLAC__MAX_FIXED_ORDER</name>
      <anchor>a65</anchor>
      <arglist></arglist>
    </member>
    <member kind="define">
      <type>#define</type>
      <name>FLAC__MAX_RICE_PARTITION_ORDER</name>
      <anchor>a66</anchor>
      <arglist></arglist>
    </member>
    <member kind="define">
      <type>#define</type>
      <name>FLAC__STREAM_SYNC_LENGTH</name>
      <anchor>a67</anchor>
      <arglist></arglist>
    </member>
    <member kind="define">
      <type>#define</type>
      <name>FLAC__STREAM_METADATA_STREAMINFO_LENGTH</name>
      <anchor>a68</anchor>
      <arglist></arglist>
    </member>
    <member kind="define">
      <type>#define</type>
      <name>FLAC__STREAM_METADATA_SEEKPOINT_LENGTH</name>
      <anchor>a69</anchor>
      <arglist></arglist>
    </member>
    <member kind="define">
      <type>#define</type>
      <name>FLAC__STREAM_METADATA_HEADER_LENGTH</name>
      <anchor>a70</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumeration">
      <name>FLAC__EntropyCodingMethodType</name>
      <anchor>a71</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__ENTROPY_CODING_METHOD_PARTITIONED_RICE</name>
      <anchor>a71a68</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumeration">
      <name>FLAC__SubframeType</name>
      <anchor>a72</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__SUBFRAME_TYPE_CONSTANT</name>
      <anchor>a72a69</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__SUBFRAME_TYPE_VERBATIM</name>
      <anchor>a72a70</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__SUBFRAME_TYPE_FIXED</name>
      <anchor>a72a71</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__SUBFRAME_TYPE_LPC</name>
      <anchor>a72a72</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumeration">
      <name>FLAC__ChannelAssignment</name>
      <anchor>a73</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__CHANNEL_ASSIGNMENT_INDEPENDENT</name>
      <anchor>a73a73</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__CHANNEL_ASSIGNMENT_LEFT_SIDE</name>
      <anchor>a73a74</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__CHANNEL_ASSIGNMENT_RIGHT_SIDE</name>
      <anchor>a73a75</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__CHANNEL_ASSIGNMENT_MID_SIDE</name>
      <anchor>a73a76</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumeration">
      <name>FLAC__FrameNumberType</name>
      <anchor>a74</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__FRAME_NUMBER_TYPE_FRAME_NUMBER</name>
      <anchor>a74a77</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__FRAME_NUMBER_TYPE_SAMPLE_NUMBER</name>
      <anchor>a74a78</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumeration">
      <name>FLAC__MetadataType</name>
      <anchor>a75</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__METADATA_TYPE_STREAMINFO</name>
      <anchor>a75a79</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__METADATA_TYPE_PADDING</name>
      <anchor>a75a80</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__METADATA_TYPE_APPLICATION</name>
      <anchor>a75a81</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__METADATA_TYPE_SEEKTABLE</name>
      <anchor>a75a82</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__METADATA_TYPE_VORBIS_COMMENT</name>
      <anchor>a75a83</anchor>
      <arglist></arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__format_sample_rate_is_valid</name>
      <anchor>a53</anchor>
      <arglist>(unsigned sample_rate)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__format_seektable_is_legal</name>
      <anchor>a54</anchor>
      <arglist>(const FLAC__StreamMetadata_SeekTable *seek_table)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>FLAC__format_seektable_sort</name>
      <anchor>a55</anchor>
      <arglist>(FLAC__StreamMetadata_SeekTable *seek_table)</arglist>
    </member>
    <member kind="variable">
      <type>const char *</type>
      <name>FLAC__VERSION_STRING</name>
      <anchor>a0</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const char *</type>
      <name>FLAC__VENDOR_STRING</name>
      <anchor>a1</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const FLAC__byte</type>
      <name>FLAC__STREAM_SYNC_STRING</name>
      <anchor>a2</anchor>
      <arglist>[4]</arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_SYNC</name>
      <anchor>a3</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_SYNC_LEN</name>
      <anchor>a4</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const char *const</type>
      <name>FLAC__EntropyCodingMethodTypeString</name>
      <anchor>a5</anchor>
      <arglist>[]</arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__ENTROPY_CODING_METHOD_PARTITIONED_RICE_ORDER_LEN</name>
      <anchor>a6</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__ENTROPY_CODING_METHOD_PARTITIONED_RICE_PARAMETER_LEN</name>
      <anchor>a7</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__ENTROPY_CODING_METHOD_PARTITIONED_RICE_RAW_LEN</name>
      <anchor>a8</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__ENTROPY_CODING_METHOD_PARTITIONED_RICE_ESCAPE_PARAMETER</name>
      <anchor>a9</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__ENTROPY_CODING_METHOD_TYPE_LEN</name>
      <anchor>a10</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const char *const</type>
      <name>FLAC__SubframeTypeString</name>
      <anchor>a11</anchor>
      <arglist>[]</arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__SUBFRAME_LPC_QLP_COEFF_PRECISION_LEN</name>
      <anchor>a12</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__SUBFRAME_LPC_QLP_SHIFT_LEN</name>
      <anchor>a13</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__SUBFRAME_ZERO_PAD_LEN</name>
      <anchor>a14</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__SUBFRAME_TYPE_LEN</name>
      <anchor>a15</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__SUBFRAME_WASTED_BITS_FLAG_LEN</name>
      <anchor>a16</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__SUBFRAME_TYPE_CONSTANT_BYTE_ALIGNED_MASK</name>
      <anchor>a17</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__SUBFRAME_TYPE_VERBATIM_BYTE_ALIGNED_MASK</name>
      <anchor>a18</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__SUBFRAME_TYPE_FIXED_BYTE_ALIGNED_MASK</name>
      <anchor>a19</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__SUBFRAME_TYPE_LPC_BYTE_ALIGNED_MASK</name>
      <anchor>a20</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const char *const</type>
      <name>FLAC__ChannelAssignmentString</name>
      <anchor>a21</anchor>
      <arglist>[]</arglist>
    </member>
    <member kind="variable">
      <type>const char *const</type>
      <name>FLAC__FrameNumberTypeString</name>
      <anchor>a22</anchor>
      <arglist>[]</arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__FRAME_HEADER_SYNC</name>
      <anchor>a23</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__FRAME_HEADER_SYNC_LEN</name>
      <anchor>a24</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__FRAME_HEADER_RESERVED_LEN</name>
      <anchor>a25</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__FRAME_HEADER_BLOCK_SIZE_LEN</name>
      <anchor>a26</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__FRAME_HEADER_SAMPLE_RATE_LEN</name>
      <anchor>a27</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__FRAME_HEADER_CHANNEL_ASSIGNMENT_LEN</name>
      <anchor>a28</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__FRAME_HEADER_BITS_PER_SAMPLE_LEN</name>
      <anchor>a29</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__FRAME_HEADER_ZERO_PAD_LEN</name>
      <anchor>a30</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__FRAME_HEADER_CRC_LEN</name>
      <anchor>a31</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__FRAME_FOOTER_CRC_LEN</name>
      <anchor>a32</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const char *const</type>
      <name>FLAC__MetadataTypeString</name>
      <anchor>a33</anchor>
      <arglist>[]</arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_METADATA_STREAMINFO_MIN_BLOCK_SIZE_LEN</name>
      <anchor>a34</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_METADATA_STREAMINFO_MAX_BLOCK_SIZE_LEN</name>
      <anchor>a35</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_METADATA_STREAMINFO_MIN_FRAME_SIZE_LEN</name>
      <anchor>a36</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_METADATA_STREAMINFO_MAX_FRAME_SIZE_LEN</name>
      <anchor>a37</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_METADATA_STREAMINFO_SAMPLE_RATE_LEN</name>
      <anchor>a38</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_METADATA_STREAMINFO_CHANNELS_LEN</name>
      <anchor>a39</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_METADATA_STREAMINFO_BITS_PER_SAMPLE_LEN</name>
      <anchor>a40</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_METADATA_STREAMINFO_TOTAL_SAMPLES_LEN</name>
      <anchor>a41</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_METADATA_STREAMINFO_MD5SUM_LEN</name>
      <anchor>a42</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_METADATA_APPLICATION_ID_LEN</name>
      <anchor>a43</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_METADATA_SEEKPOINT_SAMPLE_NUMBER_LEN</name>
      <anchor>a44</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_METADATA_SEEKPOINT_STREAM_OFFSET_LEN</name>
      <anchor>a45</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_METADATA_SEEKPOINT_FRAME_SAMPLES_LEN</name>
      <anchor>a46</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const FLAC__uint64</type>
      <name>FLAC__STREAM_METADATA_SEEKPOINT_PLACEHOLDER</name>
      <anchor>a47</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_METADATA_VORBIS_COMMENT_ENTRY_LENGTH_LEN</name>
      <anchor>a48</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_METADATA_VORBIS_COMMENT_NUM_COMMENTS_LEN</name>
      <anchor>a49</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_METADATA_IS_LAST_LEN</name>
      <anchor>a50</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_METADATA_TYPE_LEN</name>
      <anchor>a51</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_METADATA_LENGTH_LEN</name>
      <anchor>a52</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="file">
    <name>metadata.h</name>
    <path>/home/jcoalson/flac/build/include/FLAC/</path>
    <filename>metadata_8h.html</filename>
    <member kind="typedef">
      <type>FLAC__Metadata_SimpleIterator</type>
      <name>FLAC__Metadata_SimpleIterator</name>
      <anchor>a0</anchor>
      <arglist></arglist>
    </member>
    <member kind="typedef">
      <type>FLAC__Metadata_Chain</type>
      <name>FLAC__Metadata_Chain</name>
      <anchor>a0</anchor>
      <arglist></arglist>
    </member>
    <member kind="typedef">
      <type>FLAC__Metadata_Iterator</type>
      <name>FLAC__Metadata_Iterator</name>
      <anchor>a1</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumeration">
      <name>FLAC__Metadata_SimpleIteratorStatus</name>
      <anchor>a14</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__METADATA_SIMPLE_ITERATOR_STATUS_OK</name>
      <anchor>a14a5</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__METADATA_SIMPLE_ITERATOR_STATUS_ILLEGAL_INPUT</name>
      <anchor>a14a6</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__METADATA_SIMPLE_ITERATOR_STATUS_ERROR_OPENING_FILE</name>
      <anchor>a14a7</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__METADATA_SIMPLE_ITERATOR_STATUS_NOT_A_FLAC_FILE</name>
      <anchor>a14a8</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__METADATA_SIMPLE_ITERATOR_STATUS_NOT_WRITABLE</name>
      <anchor>a14a9</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__METADATA_SIMPLE_ITERATOR_STATUS_BAD_METADATA</name>
      <anchor>a14a10</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__METADATA_SIMPLE_ITERATOR_STATUS_READ_ERROR</name>
      <anchor>a14a11</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__METADATA_SIMPLE_ITERATOR_STATUS_SEEK_ERROR</name>
      <anchor>a14a12</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__METADATA_SIMPLE_ITERATOR_STATUS_WRITE_ERROR</name>
      <anchor>a14a13</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__METADATA_SIMPLE_ITERATOR_STATUS_RENAME_ERROR</name>
      <anchor>a14a14</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__METADATA_SIMPLE_ITERATOR_STATUS_UNLINK_ERROR</name>
      <anchor>a14a15</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__METADATA_SIMPLE_ITERATOR_STATUS_MEMORY_ALLOCATION_ERROR</name>
      <anchor>a14a16</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__METADATA_SIMPLE_ITERATOR_STATUS_INTERNAL_ERROR</name>
      <anchor>a14a17</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumeration">
      <name>FLAC__Metadata_ChainStatus</name>
      <anchor>a21</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__METADATA_CHAIN_STATUS_OK</name>
      <anchor>a21a18</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__METADATA_CHAIN_STATUS_ILLEGAL_INPUT</name>
      <anchor>a21a19</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__METADATA_CHAIN_STATUS_ERROR_OPENING_FILE</name>
      <anchor>a21a20</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__METADATA_CHAIN_STATUS_NOT_A_FLAC_FILE</name>
      <anchor>a21a21</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__METADATA_CHAIN_STATUS_NOT_WRITABLE</name>
      <anchor>a21a22</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__METADATA_CHAIN_STATUS_BAD_METADATA</name>
      <anchor>a21a23</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__METADATA_CHAIN_STATUS_READ_ERROR</name>
      <anchor>a21a24</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__METADATA_CHAIN_STATUS_SEEK_ERROR</name>
      <anchor>a21a25</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__METADATA_CHAIN_STATUS_WRITE_ERROR</name>
      <anchor>a21a26</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__METADATA_CHAIN_STATUS_RENAME_ERROR</name>
      <anchor>a21a27</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__METADATA_CHAIN_STATUS_UNLINK_ERROR</name>
      <anchor>a21a28</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__METADATA_CHAIN_STATUS_MEMORY_ALLOCATION_ERROR</name>
      <anchor>a21a29</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__METADATA_CHAIN_STATUS_INTERNAL_ERROR</name>
      <anchor>a21a30</anchor>
      <arglist></arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_get_streaminfo</name>
      <anchor>a0</anchor>
      <arglist>(const char *filename, FLAC__StreamMetadata *streaminfo)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__Metadata_SimpleIterator *</type>
      <name>FLAC__metadata_simple_iterator_new</name>
      <anchor>a2</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>FLAC__metadata_simple_iterator_delete</name>
      <anchor>a3</anchor>
      <arglist>(FLAC__Metadata_SimpleIterator *iterator)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__Metadata_SimpleIteratorStatus</type>
      <name>FLAC__metadata_simple_iterator_status</name>
      <anchor>a4</anchor>
      <arglist>(FLAC__Metadata_SimpleIterator *iterator)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_simple_iterator_init</name>
      <anchor>a5</anchor>
      <arglist>(FLAC__Metadata_SimpleIterator *iterator, const char *filename, FLAC__bool read_only, FLAC__bool preserve_file_stats)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_simple_iterator_is_writable</name>
      <anchor>a6</anchor>
      <arglist>(const FLAC__Metadata_SimpleIterator *iterator)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_simple_iterator_next</name>
      <anchor>a7</anchor>
      <arglist>(FLAC__Metadata_SimpleIterator *iterator)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_simple_iterator_prev</name>
      <anchor>a8</anchor>
      <arglist>(FLAC__Metadata_SimpleIterator *iterator)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__MetadataType</type>
      <name>FLAC__metadata_simple_iterator_get_block_type</name>
      <anchor>a9</anchor>
      <arglist>(const FLAC__Metadata_SimpleIterator *iterator)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__StreamMetadata *</type>
      <name>FLAC__metadata_simple_iterator_get_block</name>
      <anchor>a10</anchor>
      <arglist>(FLAC__Metadata_SimpleIterator *iterator)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_simple_iterator_set_block</name>
      <anchor>a11</anchor>
      <arglist>(FLAC__Metadata_SimpleIterator *iterator, FLAC__StreamMetadata *block, FLAC__bool use_padding)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_simple_iterator_insert_block_after</name>
      <anchor>a12</anchor>
      <arglist>(FLAC__Metadata_SimpleIterator *iterator, FLAC__StreamMetadata *block, FLAC__bool use_padding)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_simple_iterator_delete_block</name>
      <anchor>a13</anchor>
      <arglist>(FLAC__Metadata_SimpleIterator *iterator, FLAC__bool use_padding)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__Metadata_Chain *</type>
      <name>FLAC__metadata_chain_new</name>
      <anchor>a3</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>FLAC__metadata_chain_delete</name>
      <anchor>a4</anchor>
      <arglist>(FLAC__Metadata_Chain *chain)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__Metadata_ChainStatus</type>
      <name>FLAC__metadata_chain_status</name>
      <anchor>a5</anchor>
      <arglist>(FLAC__Metadata_Chain *chain)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_chain_read</name>
      <anchor>a6</anchor>
      <arglist>(FLAC__Metadata_Chain *chain, const char *filename)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_chain_write</name>
      <anchor>a7</anchor>
      <arglist>(FLAC__Metadata_Chain *chain, FLAC__bool use_padding, FLAC__bool preserve_file_stats)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>FLAC__metadata_chain_merge_padding</name>
      <anchor>a8</anchor>
      <arglist>(FLAC__Metadata_Chain *chain)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>FLAC__metadata_chain_sort_padding</name>
      <anchor>a9</anchor>
      <arglist>(FLAC__Metadata_Chain *chain)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__Metadata_Iterator *</type>
      <name>FLAC__metadata_iterator_new</name>
      <anchor>a10</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>FLAC__metadata_iterator_delete</name>
      <anchor>a11</anchor>
      <arglist>(FLAC__Metadata_Iterator *iterator)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>FLAC__metadata_iterator_init</name>
      <anchor>a12</anchor>
      <arglist>(FLAC__Metadata_Iterator *iterator, FLAC__Metadata_Chain *chain)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_iterator_next</name>
      <anchor>a13</anchor>
      <arglist>(FLAC__Metadata_Iterator *iterator)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_iterator_prev</name>
      <anchor>a14</anchor>
      <arglist>(FLAC__Metadata_Iterator *iterator)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__MetadataType</type>
      <name>FLAC__metadata_iterator_get_block_type</name>
      <anchor>a15</anchor>
      <arglist>(const FLAC__Metadata_Iterator *iterator)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__StreamMetadata *</type>
      <name>FLAC__metadata_iterator_get_block</name>
      <anchor>a16</anchor>
      <arglist>(FLAC__Metadata_Iterator *iterator)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_iterator_set_block</name>
      <anchor>a17</anchor>
      <arglist>(FLAC__Metadata_Iterator *iterator, FLAC__StreamMetadata *block)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_iterator_delete_block</name>
      <anchor>a18</anchor>
      <arglist>(FLAC__Metadata_Iterator *iterator, FLAC__bool replace_with_padding)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_iterator_insert_block_before</name>
      <anchor>a19</anchor>
      <arglist>(FLAC__Metadata_Iterator *iterator, FLAC__StreamMetadata *block)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_iterator_insert_block_after</name>
      <anchor>a20</anchor>
      <arglist>(FLAC__Metadata_Iterator *iterator, FLAC__StreamMetadata *block)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__StreamMetadata *</type>
      <name>FLAC__metadata_object_new</name>
      <anchor>a0</anchor>
      <arglist>(FLAC__MetadataType type)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__StreamMetadata *</type>
      <name>FLAC__metadata_object_clone</name>
      <anchor>a1</anchor>
      <arglist>(const FLAC__StreamMetadata *object)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>FLAC__metadata_object_delete</name>
      <anchor>a2</anchor>
      <arglist>(FLAC__StreamMetadata *object)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_object_is_equal</name>
      <anchor>a3</anchor>
      <arglist>(const FLAC__StreamMetadata *block1, const FLAC__StreamMetadata *block2)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_object_application_set_data</name>
      <anchor>a4</anchor>
      <arglist>(FLAC__StreamMetadata *object, FLAC__byte *data, unsigned length, FLAC__bool copy)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_object_seektable_resize_points</name>
      <anchor>a5</anchor>
      <arglist>(FLAC__StreamMetadata *object, unsigned new_num_points)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>FLAC__metadata_object_seektable_set_point</name>
      <anchor>a6</anchor>
      <arglist>(FLAC__StreamMetadata *object, unsigned point_num, FLAC__StreamMetadata_SeekPoint point)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_object_seektable_insert_point</name>
      <anchor>a7</anchor>
      <arglist>(FLAC__StreamMetadata *object, unsigned point_num, FLAC__StreamMetadata_SeekPoint point)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_object_seektable_delete_point</name>
      <anchor>a8</anchor>
      <arglist>(FLAC__StreamMetadata *object, unsigned point_num)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_object_seektable_is_legal</name>
      <anchor>a9</anchor>
      <arglist>(const FLAC__StreamMetadata *object)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_object_seektable_template_append_placeholders</name>
      <anchor>a10</anchor>
      <arglist>(FLAC__StreamMetadata *object, unsigned num)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_object_seektable_template_append_point</name>
      <anchor>a11</anchor>
      <arglist>(FLAC__StreamMetadata *object, FLAC__uint64 sample_number)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_object_seektable_template_append_points</name>
      <anchor>a12</anchor>
      <arglist>(FLAC__StreamMetadata *object, FLAC__uint64 sample_numbers[], unsigned num)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_object_seektable_template_append_spaced_points</name>
      <anchor>a13</anchor>
      <arglist>(FLAC__StreamMetadata *object, unsigned num, FLAC__uint64 total_samples)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_object_seektable_template_sort</name>
      <anchor>a14</anchor>
      <arglist>(FLAC__StreamMetadata *object, FLAC__bool compact)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_object_vorbiscomment_set_vendor_string</name>
      <anchor>a15</anchor>
      <arglist>(FLAC__StreamMetadata *object, FLAC__StreamMetadata_VorbisComment_Entry entry, FLAC__bool copy)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_object_vorbiscomment_resize_comments</name>
      <anchor>a16</anchor>
      <arglist>(FLAC__StreamMetadata *object, unsigned new_num_comments)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_object_vorbiscomment_set_comment</name>
      <anchor>a17</anchor>
      <arglist>(FLAC__StreamMetadata *object, unsigned comment_num, FLAC__StreamMetadata_VorbisComment_Entry entry, FLAC__bool copy)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_object_vorbiscomment_insert_comment</name>
      <anchor>a18</anchor>
      <arglist>(FLAC__StreamMetadata *object, unsigned comment_num, FLAC__StreamMetadata_VorbisComment_Entry entry, FLAC__bool copy)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_object_vorbiscomment_delete_comment</name>
      <anchor>a19</anchor>
      <arglist>(FLAC__StreamMetadata *object, unsigned comment_num)</arglist>
    </member>
    <member kind="variable">
      <type>const char *const</type>
      <name>FLAC__Metadata_SimpleIteratorStatusString</name>
      <anchor>a1</anchor>
      <arglist>[]</arglist>
    </member>
    <member kind="variable">
      <type>const char *const</type>
      <name>FLAC__Metadata_ChainStatusString</name>
      <anchor>a2</anchor>
      <arglist>[]</arglist>
    </member>
  </compound>
  <compound kind="file">
    <name>metadata.h</name>
    <path>/home/jcoalson/flac/build/include/FLAC++/</path>
    <filename>+_2metadata_8h.html</filename>
  </compound>
  <compound kind="file">
    <name>seekable_stream_decoder.h</name>
    <path>/home/jcoalson/flac/build/include/FLAC/</path>
    <filename>seekable__stream__decoder_8h.html</filename>
    <class kind="struct">FLAC__SeekableStreamDecoder</class>
    <member kind="typedef">
      <type>FLAC__SeekableStreamDecoderReadStatus(*</type>
      <name>FLAC__SeekableStreamDecoderReadCallback</name>
      <anchor>a5</anchor>
      <arglist>)(const FLAC__SeekableStreamDecoder *decoder, FLAC__byte buffer[], unsigned *bytes, void *client_data)</arglist>
    </member>
    <member kind="typedef">
      <type>FLAC__SeekableStreamDecoderSeekStatus(*</type>
      <name>FLAC__SeekableStreamDecoderSeekCallback</name>
      <anchor>a6</anchor>
      <arglist>)(const FLAC__SeekableStreamDecoder *decoder, FLAC__uint64 absolute_byte_offset, void *client_data)</arglist>
    </member>
    <member kind="typedef">
      <type>FLAC__SeekableStreamDecoderTellStatus(*</type>
      <name>FLAC__SeekableStreamDecoderTellCallback</name>
      <anchor>a7</anchor>
      <arglist>)(const FLAC__SeekableStreamDecoder *decoder, FLAC__uint64 *absolute_byte_offset, void *client_data)</arglist>
    </member>
    <member kind="typedef">
      <type>FLAC__SeekableStreamDecoderLengthStatus(*</type>
      <name>FLAC__SeekableStreamDecoderLengthCallback</name>
      <anchor>a8</anchor>
      <arglist>)(const FLAC__SeekableStreamDecoder *decoder, FLAC__uint64 *stream_length, void *client_data)</arglist>
    </member>
    <member kind="typedef">
      <type>FLAC__bool(*</type>
      <name>FLAC__SeekableStreamDecoderEofCallback</name>
      <anchor>a9</anchor>
      <arglist>)(const FLAC__SeekableStreamDecoder *decoder, void *client_data)</arglist>
    </member>
    <member kind="typedef">
      <type>FLAC__StreamDecoderWriteStatus(*</type>
      <name>FLAC__SeekableStreamDecoderWriteCallback</name>
      <anchor>a10</anchor>
      <arglist>)(const FLAC__SeekableStreamDecoder *decoder, const FLAC__Frame *frame, const FLAC__int32 *const buffer[], void *client_data)</arglist>
    </member>
    <member kind="typedef">
      <type>void(*</type>
      <name>FLAC__SeekableStreamDecoderMetadataCallback</name>
      <anchor>a11</anchor>
      <arglist>)(const FLAC__SeekableStreamDecoder *decoder, const FLAC__StreamMetadata *metadata, void *client_data)</arglist>
    </member>
    <member kind="typedef">
      <type>void(*</type>
      <name>FLAC__SeekableStreamDecoderErrorCallback</name>
      <anchor>a12</anchor>
      <arglist>)(const FLAC__SeekableStreamDecoder *decoder, FLAC__StreamDecoderErrorStatus status, void *client_data)</arglist>
    </member>
    <member kind="enumeration">
      <name>FLAC__SeekableStreamDecoderState</name>
      <anchor>a47</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__SEEKABLE_STREAM_DECODER_OK</name>
      <anchor>a47a13</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__SEEKABLE_STREAM_DECODER_SEEKING</name>
      <anchor>a47a14</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__SEEKABLE_STREAM_DECODER_END_OF_STREAM</name>
      <anchor>a47a15</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__SEEKABLE_STREAM_DECODER_MEMORY_ALLOCATION_ERROR</name>
      <anchor>a47a16</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__SEEKABLE_STREAM_DECODER_STREAM_DECODER_ERROR</name>
      <anchor>a47a17</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__SEEKABLE_STREAM_DECODER_READ_ERROR</name>
      <anchor>a47a18</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__SEEKABLE_STREAM_DECODER_SEEK_ERROR</name>
      <anchor>a47a19</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__SEEKABLE_STREAM_DECODER_ALREADY_INITIALIZED</name>
      <anchor>a47a20</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__SEEKABLE_STREAM_DECODER_INVALID_CALLBACK</name>
      <anchor>a47a21</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__SEEKABLE_STREAM_DECODER_UNINITIALIZED</name>
      <anchor>a47a22</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumeration">
      <name>FLAC__SeekableStreamDecoderReadStatus</name>
      <anchor>a48</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__SEEKABLE_STREAM_DECODER_READ_STATUS_OK</name>
      <anchor>a48a23</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__SEEKABLE_STREAM_DECODER_READ_STATUS_ERROR</name>
      <anchor>a48a24</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumeration">
      <name>FLAC__SeekableStreamDecoderSeekStatus</name>
      <anchor>a49</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__SEEKABLE_STREAM_DECODER_SEEK_STATUS_OK</name>
      <anchor>a49a25</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__SEEKABLE_STREAM_DECODER_SEEK_STATUS_ERROR</name>
      <anchor>a49a26</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumeration">
      <name>FLAC__SeekableStreamDecoderTellStatus</name>
      <anchor>a50</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__SEEKABLE_STREAM_DECODER_TELL_STATUS_OK</name>
      <anchor>a50a27</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__SEEKABLE_STREAM_DECODER_TELL_STATUS_ERROR</name>
      <anchor>a50a28</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumeration">
      <name>FLAC__SeekableStreamDecoderLengthStatus</name>
      <anchor>a51</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__SEEKABLE_STREAM_DECODER_LENGTH_STATUS_OK</name>
      <anchor>a51a29</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__SEEKABLE_STREAM_DECODER_LENGTH_STATUS_ERROR</name>
      <anchor>a51a30</anchor>
      <arglist></arglist>
    </member>
    <member kind="function">
      <type>FLAC__SeekableStreamDecoder *</type>
      <name>FLAC__seekable_stream_decoder_new</name>
      <anchor>a13</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>FLAC__seekable_stream_decoder_delete</name>
      <anchor>a14</anchor>
      <arglist>(FLAC__SeekableStreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__seekable_stream_decoder_set_md5_checking</name>
      <anchor>a15</anchor>
      <arglist>(FLAC__SeekableStreamDecoder *decoder, FLAC__bool value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__seekable_stream_decoder_set_read_callback</name>
      <anchor>a16</anchor>
      <arglist>(FLAC__SeekableStreamDecoder *decoder, FLAC__SeekableStreamDecoderReadCallback value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__seekable_stream_decoder_set_seek_callback</name>
      <anchor>a17</anchor>
      <arglist>(FLAC__SeekableStreamDecoder *decoder, FLAC__SeekableStreamDecoderSeekCallback value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__seekable_stream_decoder_set_tell_callback</name>
      <anchor>a18</anchor>
      <arglist>(FLAC__SeekableStreamDecoder *decoder, FLAC__SeekableStreamDecoderTellCallback value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__seekable_stream_decoder_set_length_callback</name>
      <anchor>a19</anchor>
      <arglist>(FLAC__SeekableStreamDecoder *decoder, FLAC__SeekableStreamDecoderLengthCallback value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__seekable_stream_decoder_set_eof_callback</name>
      <anchor>a20</anchor>
      <arglist>(FLAC__SeekableStreamDecoder *decoder, FLAC__SeekableStreamDecoderEofCallback value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__seekable_stream_decoder_set_write_callback</name>
      <anchor>a21</anchor>
      <arglist>(FLAC__SeekableStreamDecoder *decoder, FLAC__SeekableStreamDecoderWriteCallback value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__seekable_stream_decoder_set_metadata_callback</name>
      <anchor>a22</anchor>
      <arglist>(FLAC__SeekableStreamDecoder *decoder, FLAC__SeekableStreamDecoderMetadataCallback value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__seekable_stream_decoder_set_error_callback</name>
      <anchor>a23</anchor>
      <arglist>(FLAC__SeekableStreamDecoder *decoder, FLAC__SeekableStreamDecoderErrorCallback value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__seekable_stream_decoder_set_client_data</name>
      <anchor>a24</anchor>
      <arglist>(FLAC__SeekableStreamDecoder *decoder, void *value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__seekable_stream_decoder_set_metadata_respond</name>
      <anchor>a25</anchor>
      <arglist>(FLAC__SeekableStreamDecoder *decoder, FLAC__MetadataType type)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__seekable_stream_decoder_set_metadata_respond_application</name>
      <anchor>a26</anchor>
      <arglist>(FLAC__SeekableStreamDecoder *decoder, const FLAC__byte id[4])</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__seekable_stream_decoder_set_metadata_respond_all</name>
      <anchor>a27</anchor>
      <arglist>(FLAC__SeekableStreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__seekable_stream_decoder_set_metadata_ignore</name>
      <anchor>a28</anchor>
      <arglist>(FLAC__SeekableStreamDecoder *decoder, FLAC__MetadataType type)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__seekable_stream_decoder_set_metadata_ignore_application</name>
      <anchor>a29</anchor>
      <arglist>(FLAC__SeekableStreamDecoder *decoder, const FLAC__byte id[4])</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__seekable_stream_decoder_set_metadata_ignore_all</name>
      <anchor>a30</anchor>
      <arglist>(FLAC__SeekableStreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__SeekableStreamDecoderState</type>
      <name>FLAC__seekable_stream_decoder_get_state</name>
      <anchor>a31</anchor>
      <arglist>(const FLAC__SeekableStreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__StreamDecoderState</type>
      <name>FLAC__seekable_stream_decoder_get_stream_decoder_state</name>
      <anchor>a32</anchor>
      <arglist>(const FLAC__SeekableStreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__seekable_stream_decoder_get_md5_checking</name>
      <anchor>a33</anchor>
      <arglist>(const FLAC__SeekableStreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>FLAC__seekable_stream_decoder_get_channels</name>
      <anchor>a34</anchor>
      <arglist>(const FLAC__SeekableStreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__ChannelAssignment</type>
      <name>FLAC__seekable_stream_decoder_get_channel_assignment</name>
      <anchor>a35</anchor>
      <arglist>(const FLAC__SeekableStreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>FLAC__seekable_stream_decoder_get_bits_per_sample</name>
      <anchor>a36</anchor>
      <arglist>(const FLAC__SeekableStreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>FLAC__seekable_stream_decoder_get_sample_rate</name>
      <anchor>a37</anchor>
      <arglist>(const FLAC__SeekableStreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>FLAC__seekable_stream_decoder_get_blocksize</name>
      <anchor>a38</anchor>
      <arglist>(const FLAC__SeekableStreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__SeekableStreamDecoderState</type>
      <name>FLAC__seekable_stream_decoder_init</name>
      <anchor>a39</anchor>
      <arglist>(FLAC__SeekableStreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__seekable_stream_decoder_finish</name>
      <anchor>a40</anchor>
      <arglist>(FLAC__SeekableStreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__seekable_stream_decoder_flush</name>
      <anchor>a41</anchor>
      <arglist>(FLAC__SeekableStreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__seekable_stream_decoder_reset</name>
      <anchor>a42</anchor>
      <arglist>(FLAC__SeekableStreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__seekable_stream_decoder_process_single</name>
      <anchor>a43</anchor>
      <arglist>(FLAC__SeekableStreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__seekable_stream_decoder_process_until_end_of_metadata</name>
      <anchor>a44</anchor>
      <arglist>(FLAC__SeekableStreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__seekable_stream_decoder_process_until_end_of_stream</name>
      <anchor>a45</anchor>
      <arglist>(FLAC__SeekableStreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__seekable_stream_decoder_seek_absolute</name>
      <anchor>a46</anchor>
      <arglist>(FLAC__SeekableStreamDecoder *decoder, FLAC__uint64 sample)</arglist>
    </member>
    <member kind="variable">
      <type>const char *const</type>
      <name>FLAC__SeekableStreamDecoderStateString</name>
      <anchor>a0</anchor>
      <arglist>[]</arglist>
    </member>
    <member kind="variable">
      <type>const char *const</type>
      <name>FLAC__SeekableStreamDecoderReadStatusString</name>
      <anchor>a1</anchor>
      <arglist>[]</arglist>
    </member>
    <member kind="variable">
      <type>const char *const</type>
      <name>FLAC__SeekableStreamDecoderSeekStatusString</name>
      <anchor>a2</anchor>
      <arglist>[]</arglist>
    </member>
    <member kind="variable">
      <type>const char *const</type>
      <name>FLAC__SeekableStreamDecoderTellStatusString</name>
      <anchor>a3</anchor>
      <arglist>[]</arglist>
    </member>
    <member kind="variable">
      <type>const char *const</type>
      <name>FLAC__SeekableStreamDecoderLengthStatusString</name>
      <anchor>a4</anchor>
      <arglist>[]</arglist>
    </member>
  </compound>
  <compound kind="file">
    <name>seekable_stream_encoder.h</name>
    <path>/home/jcoalson/flac/build/include/FLAC/</path>
    <filename>seekable__stream__encoder_8h.html</filename>
    <class kind="struct">FLAC__SeekableStreamEncoder</class>
    <member kind="typedef">
      <type>FLAC__SeekableStreamEncoderSeekStatus(*</type>
      <name>FLAC__SeekableStreamEncoderSeekCallback</name>
      <anchor>a2</anchor>
      <arglist>)(const FLAC__SeekableStreamEncoder *encoder, FLAC__uint64 absolute_byte_offset, void *client_data)</arglist>
    </member>
    <member kind="typedef">
      <type>FLAC__StreamEncoderWriteStatus(*</type>
      <name>FLAC__SeekableStreamEncoderWriteCallback</name>
      <anchor>a3</anchor>
      <arglist>)(const FLAC__SeekableStreamEncoder *encoder, const FLAC__byte buffer[], unsigned bytes, unsigned samples, unsigned current_frame, void *client_data)</arglist>
    </member>
    <member kind="enumeration">
      <name>FLAC__SeekableStreamEncoderState</name>
      <anchor>a52</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__SEEKABLE_STREAM_ENCODER_OK</name>
      <anchor>a52a4</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__SEEKABLE_STREAM_ENCODER_STREAM_ENCODER_ERROR</name>
      <anchor>a52a5</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__SEEKABLE_STREAM_ENCODER_MEMORY_ALLOCATION_ERROR</name>
      <anchor>a52a6</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__SEEKABLE_STREAM_ENCODER_WRITE_ERROR</name>
      <anchor>a52a7</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__SEEKABLE_STREAM_ENCODER_READ_ERROR</name>
      <anchor>a52a8</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__SEEKABLE_STREAM_ENCODER_SEEK_ERROR</name>
      <anchor>a52a9</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__SEEKABLE_STREAM_ENCODER_ALREADY_INITIALIZED</name>
      <anchor>a52a10</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__SEEKABLE_STREAM_ENCODER_INVALID_CALLBACK</name>
      <anchor>a52a11</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__SEEKABLE_STREAM_ENCODER_INVALID_SEEKTABLE</name>
      <anchor>a52a12</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__SEEKABLE_STREAM_ENCODER_UNINITIALIZED</name>
      <anchor>a52a13</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumeration">
      <name>FLAC__SeekableStreamEncoderSeekStatus</name>
      <anchor>a53</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__SEEKABLE_STREAM_ENCODER_SEEK_STATUS_OK</name>
      <anchor>a53a14</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__SEEKABLE_STREAM_ENCODER_SEEK_STATUS_ERROR</name>
      <anchor>a53a15</anchor>
      <arglist></arglist>
    </member>
    <member kind="function">
      <type>FLAC__SeekableStreamEncoder *</type>
      <name>FLAC__seekable_stream_encoder_new</name>
      <anchor>a4</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>FLAC__seekable_stream_encoder_delete</name>
      <anchor>a5</anchor>
      <arglist>(FLAC__SeekableStreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__seekable_stream_encoder_set_verify</name>
      <anchor>a6</anchor>
      <arglist>(FLAC__SeekableStreamEncoder *encoder, FLAC__bool value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__seekable_stream_encoder_set_streamable_subset</name>
      <anchor>a7</anchor>
      <arglist>(FLAC__SeekableStreamEncoder *encoder, FLAC__bool value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__seekable_stream_encoder_set_do_mid_side_stereo</name>
      <anchor>a8</anchor>
      <arglist>(FLAC__SeekableStreamEncoder *encoder, FLAC__bool value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__seekable_stream_encoder_set_loose_mid_side_stereo</name>
      <anchor>a9</anchor>
      <arglist>(FLAC__SeekableStreamEncoder *encoder, FLAC__bool value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__seekable_stream_encoder_set_channels</name>
      <anchor>a10</anchor>
      <arglist>(FLAC__SeekableStreamEncoder *encoder, unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__seekable_stream_encoder_set_bits_per_sample</name>
      <anchor>a11</anchor>
      <arglist>(FLAC__SeekableStreamEncoder *encoder, unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__seekable_stream_encoder_set_sample_rate</name>
      <anchor>a12</anchor>
      <arglist>(FLAC__SeekableStreamEncoder *encoder, unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__seekable_stream_encoder_set_blocksize</name>
      <anchor>a13</anchor>
      <arglist>(FLAC__SeekableStreamEncoder *encoder, unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__seekable_stream_encoder_set_max_lpc_order</name>
      <anchor>a14</anchor>
      <arglist>(FLAC__SeekableStreamEncoder *encoder, unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__seekable_stream_encoder_set_qlp_coeff_precision</name>
      <anchor>a15</anchor>
      <arglist>(FLAC__SeekableStreamEncoder *encoder, unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__seekable_stream_encoder_set_do_qlp_coeff_prec_search</name>
      <anchor>a16</anchor>
      <arglist>(FLAC__SeekableStreamEncoder *encoder, FLAC__bool value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__seekable_stream_encoder_set_do_escape_coding</name>
      <anchor>a17</anchor>
      <arglist>(FLAC__SeekableStreamEncoder *encoder, FLAC__bool value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__seekable_stream_encoder_set_do_exhaustive_model_search</name>
      <anchor>a18</anchor>
      <arglist>(FLAC__SeekableStreamEncoder *encoder, FLAC__bool value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__seekable_stream_encoder_set_min_residual_partition_order</name>
      <anchor>a19</anchor>
      <arglist>(FLAC__SeekableStreamEncoder *encoder, unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__seekable_stream_encoder_set_max_residual_partition_order</name>
      <anchor>a20</anchor>
      <arglist>(FLAC__SeekableStreamEncoder *encoder, unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__seekable_stream_encoder_set_rice_parameter_search_dist</name>
      <anchor>a21</anchor>
      <arglist>(FLAC__SeekableStreamEncoder *encoder, unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__seekable_stream_encoder_set_total_samples_estimate</name>
      <anchor>a22</anchor>
      <arglist>(FLAC__SeekableStreamEncoder *encoder, FLAC__uint64 value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__seekable_stream_encoder_set_metadata</name>
      <anchor>a23</anchor>
      <arglist>(FLAC__SeekableStreamEncoder *encoder, FLAC__StreamMetadata **metadata, unsigned num_blocks)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__seekable_stream_encoder_set_seek_callback</name>
      <anchor>a24</anchor>
      <arglist>(FLAC__SeekableStreamEncoder *encoder, FLAC__SeekableStreamEncoderSeekCallback value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__seekable_stream_encoder_set_write_callback</name>
      <anchor>a25</anchor>
      <arglist>(FLAC__SeekableStreamEncoder *encoder, FLAC__SeekableStreamEncoderWriteCallback value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__seekable_stream_encoder_set_client_data</name>
      <anchor>a26</anchor>
      <arglist>(FLAC__SeekableStreamEncoder *encoder, void *value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__SeekableStreamEncoderState</type>
      <name>FLAC__seekable_stream_encoder_get_state</name>
      <anchor>a27</anchor>
      <arglist>(const FLAC__SeekableStreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__StreamEncoderState</type>
      <name>FLAC__seekable_stream_encoder_get_stream_encoder_state</name>
      <anchor>a28</anchor>
      <arglist>(const FLAC__SeekableStreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__StreamDecoderState</type>
      <name>FLAC__seekable_stream_encoder_get_verify_decoder_state</name>
      <anchor>a29</anchor>
      <arglist>(const FLAC__SeekableStreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>FLAC__seekable_stream_encoder_get_verify_decoder_error_stats</name>
      <anchor>a30</anchor>
      <arglist>(const FLAC__SeekableStreamEncoder *encoder, FLAC__uint64 *absolute_sample, unsigned *frame_number, unsigned *channel, unsigned *sample, FLAC__int32 *expected, FLAC__int32 *got)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__seekable_stream_encoder_get_verify</name>
      <anchor>a31</anchor>
      <arglist>(const FLAC__SeekableStreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__seekable_stream_encoder_get_streamable_subset</name>
      <anchor>a32</anchor>
      <arglist>(const FLAC__SeekableStreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__seekable_stream_encoder_get_do_mid_side_stereo</name>
      <anchor>a33</anchor>
      <arglist>(const FLAC__SeekableStreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__seekable_stream_encoder_get_loose_mid_side_stereo</name>
      <anchor>a34</anchor>
      <arglist>(const FLAC__SeekableStreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>FLAC__seekable_stream_encoder_get_channels</name>
      <anchor>a35</anchor>
      <arglist>(const FLAC__SeekableStreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>FLAC__seekable_stream_encoder_get_bits_per_sample</name>
      <anchor>a36</anchor>
      <arglist>(const FLAC__SeekableStreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>FLAC__seekable_stream_encoder_get_sample_rate</name>
      <anchor>a37</anchor>
      <arglist>(const FLAC__SeekableStreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>FLAC__seekable_stream_encoder_get_blocksize</name>
      <anchor>a38</anchor>
      <arglist>(const FLAC__SeekableStreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>FLAC__seekable_stream_encoder_get_max_lpc_order</name>
      <anchor>a39</anchor>
      <arglist>(const FLAC__SeekableStreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>FLAC__seekable_stream_encoder_get_qlp_coeff_precision</name>
      <anchor>a40</anchor>
      <arglist>(const FLAC__SeekableStreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__seekable_stream_encoder_get_do_qlp_coeff_prec_search</name>
      <anchor>a41</anchor>
      <arglist>(const FLAC__SeekableStreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__seekable_stream_encoder_get_do_escape_coding</name>
      <anchor>a42</anchor>
      <arglist>(const FLAC__SeekableStreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__seekable_stream_encoder_get_do_exhaustive_model_search</name>
      <anchor>a43</anchor>
      <arglist>(const FLAC__SeekableStreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>FLAC__seekable_stream_encoder_get_min_residual_partition_order</name>
      <anchor>a44</anchor>
      <arglist>(const FLAC__SeekableStreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>FLAC__seekable_stream_encoder_get_max_residual_partition_order</name>
      <anchor>a45</anchor>
      <arglist>(const FLAC__SeekableStreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>FLAC__seekable_stream_encoder_get_rice_parameter_search_dist</name>
      <anchor>a46</anchor>
      <arglist>(const FLAC__SeekableStreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__uint64</type>
      <name>FLAC__seekable_stream_encoder_get_total_samples_estimate</name>
      <anchor>a47</anchor>
      <arglist>(const FLAC__SeekableStreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__SeekableStreamEncoderState</type>
      <name>FLAC__seekable_stream_encoder_init</name>
      <anchor>a48</anchor>
      <arglist>(FLAC__SeekableStreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>FLAC__seekable_stream_encoder_finish</name>
      <anchor>a49</anchor>
      <arglist>(FLAC__SeekableStreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__seekable_stream_encoder_process</name>
      <anchor>a50</anchor>
      <arglist>(FLAC__SeekableStreamEncoder *encoder, const FLAC__int32 *const buffer[], unsigned samples)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__seekable_stream_encoder_process_interleaved</name>
      <anchor>a51</anchor>
      <arglist>(FLAC__SeekableStreamEncoder *encoder, const FLAC__int32 buffer[], unsigned samples)</arglist>
    </member>
    <member kind="variable">
      <type>const char *const</type>
      <name>FLAC__SeekableStreamEncoderStateString</name>
      <anchor>a0</anchor>
      <arglist>[]</arglist>
    </member>
    <member kind="variable">
      <type>const char *const</type>
      <name>FLAC__SeekableStreamEncoderSeekStatusString</name>
      <anchor>a1</anchor>
      <arglist>[]</arglist>
    </member>
  </compound>
  <compound kind="file">
    <name>stream_decoder.h</name>
    <path>/home/jcoalson/flac/build/include/FLAC/</path>
    <filename>FLAC_2stream__decoder_8h.html</filename>
    <class kind="struct">FLAC__StreamDecoder</class>
    <member kind="typedef">
      <type>FLAC__StreamDecoderReadStatus(*</type>
      <name>FLAC__StreamDecoderReadCallback</name>
      <anchor>a4</anchor>
      <arglist>)(const FLAC__StreamDecoder *decoder, FLAC__byte buffer[], unsigned *bytes, void *client_data)</arglist>
    </member>
    <member kind="typedef">
      <type>FLAC__StreamDecoderWriteStatus(*</type>
      <name>FLAC__StreamDecoderWriteCallback</name>
      <anchor>a5</anchor>
      <arglist>)(const FLAC__StreamDecoder *decoder, const FLAC__Frame *frame, const FLAC__int32 *const buffer[], void *client_data)</arglist>
    </member>
    <member kind="typedef">
      <type>void(*</type>
      <name>FLAC__StreamDecoderMetadataCallback</name>
      <anchor>a6</anchor>
      <arglist>)(const FLAC__StreamDecoder *decoder, const FLAC__StreamMetadata *metadata, void *client_data)</arglist>
    </member>
    <member kind="typedef">
      <type>void(*</type>
      <name>FLAC__StreamDecoderErrorCallback</name>
      <anchor>a7</anchor>
      <arglist>)(const FLAC__StreamDecoder *decoder, FLAC__StreamDecoderErrorStatus status, void *client_data)</arglist>
    </member>
    <member kind="enumeration">
      <name>FLAC__StreamDecoderState</name>
      <anchor>a34</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__STREAM_DECODER_SEARCH_FOR_METADATA</name>
      <anchor>a34a8</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__STREAM_DECODER_READ_METADATA</name>
      <anchor>a34a9</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__STREAM_DECODER_SEARCH_FOR_FRAME_SYNC</name>
      <anchor>a34a10</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__STREAM_DECODER_READ_FRAME</name>
      <anchor>a34a11</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__STREAM_DECODER_END_OF_STREAM</name>
      <anchor>a34a12</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__STREAM_DECODER_ABORTED</name>
      <anchor>a34a13</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__STREAM_DECODER_UNPARSEABLE_STREAM</name>
      <anchor>a34a14</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__STREAM_DECODER_MEMORY_ALLOCATION_ERROR</name>
      <anchor>a34a15</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__STREAM_DECODER_ALREADY_INITIALIZED</name>
      <anchor>a34a16</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__STREAM_DECODER_INVALID_CALLBACK</name>
      <anchor>a34a17</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__STREAM_DECODER_UNINITIALIZED</name>
      <anchor>a34a18</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumeration">
      <name>FLAC__StreamDecoderReadStatus</name>
      <anchor>a35</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__STREAM_DECODER_READ_STATUS_CONTINUE</name>
      <anchor>a35a19</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__STREAM_DECODER_READ_STATUS_END_OF_STREAM</name>
      <anchor>a35a20</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__STREAM_DECODER_READ_STATUS_ABORT</name>
      <anchor>a35a21</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumeration">
      <name>FLAC__StreamDecoderWriteStatus</name>
      <anchor>a36</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__STREAM_DECODER_WRITE_STATUS_CONTINUE</name>
      <anchor>a36a22</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__STREAM_DECODER_WRITE_STATUS_ABORT</name>
      <anchor>a36a23</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumeration">
      <name>FLAC__StreamDecoderErrorStatus</name>
      <anchor>a37</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__STREAM_DECODER_ERROR_STATUS_LOST_SYNC</name>
      <anchor>a37a24</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__STREAM_DECODER_ERROR_STATUS_BAD_HEADER</name>
      <anchor>a37a25</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__STREAM_DECODER_ERROR_STATUS_FRAME_CRC_MISMATCH</name>
      <anchor>a37a26</anchor>
      <arglist></arglist>
    </member>
    <member kind="function">
      <type>FLAC__StreamDecoder *</type>
      <name>FLAC__stream_decoder_new</name>
      <anchor>a8</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>FLAC__stream_decoder_delete</name>
      <anchor>a9</anchor>
      <arglist>(FLAC__StreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_decoder_set_read_callback</name>
      <anchor>a10</anchor>
      <arglist>(FLAC__StreamDecoder *decoder, FLAC__StreamDecoderReadCallback value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_decoder_set_write_callback</name>
      <anchor>a11</anchor>
      <arglist>(FLAC__StreamDecoder *decoder, FLAC__StreamDecoderWriteCallback value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_decoder_set_metadata_callback</name>
      <anchor>a12</anchor>
      <arglist>(FLAC__StreamDecoder *decoder, FLAC__StreamDecoderMetadataCallback value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_decoder_set_error_callback</name>
      <anchor>a13</anchor>
      <arglist>(FLAC__StreamDecoder *decoder, FLAC__StreamDecoderErrorCallback value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_decoder_set_client_data</name>
      <anchor>a14</anchor>
      <arglist>(FLAC__StreamDecoder *decoder, void *value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_decoder_set_metadata_respond</name>
      <anchor>a15</anchor>
      <arglist>(FLAC__StreamDecoder *decoder, FLAC__MetadataType type)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_decoder_set_metadata_respond_application</name>
      <anchor>a16</anchor>
      <arglist>(FLAC__StreamDecoder *decoder, const FLAC__byte id[4])</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_decoder_set_metadata_respond_all</name>
      <anchor>a17</anchor>
      <arglist>(FLAC__StreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_decoder_set_metadata_ignore</name>
      <anchor>a18</anchor>
      <arglist>(FLAC__StreamDecoder *decoder, FLAC__MetadataType type)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_decoder_set_metadata_ignore_application</name>
      <anchor>a19</anchor>
      <arglist>(FLAC__StreamDecoder *decoder, const FLAC__byte id[4])</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_decoder_set_metadata_ignore_all</name>
      <anchor>a20</anchor>
      <arglist>(FLAC__StreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__StreamDecoderState</type>
      <name>FLAC__stream_decoder_get_state</name>
      <anchor>a21</anchor>
      <arglist>(const FLAC__StreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>FLAC__stream_decoder_get_channels</name>
      <anchor>a22</anchor>
      <arglist>(const FLAC__StreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__ChannelAssignment</type>
      <name>FLAC__stream_decoder_get_channel_assignment</name>
      <anchor>a23</anchor>
      <arglist>(const FLAC__StreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>FLAC__stream_decoder_get_bits_per_sample</name>
      <anchor>a24</anchor>
      <arglist>(const FLAC__StreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>FLAC__stream_decoder_get_sample_rate</name>
      <anchor>a25</anchor>
      <arglist>(const FLAC__StreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>FLAC__stream_decoder_get_blocksize</name>
      <anchor>a26</anchor>
      <arglist>(const FLAC__StreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__StreamDecoderState</type>
      <name>FLAC__stream_decoder_init</name>
      <anchor>a27</anchor>
      <arglist>(FLAC__StreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>FLAC__stream_decoder_finish</name>
      <anchor>a28</anchor>
      <arglist>(FLAC__StreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_decoder_flush</name>
      <anchor>a29</anchor>
      <arglist>(FLAC__StreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_decoder_reset</name>
      <anchor>a30</anchor>
      <arglist>(FLAC__StreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_decoder_process_single</name>
      <anchor>a31</anchor>
      <arglist>(FLAC__StreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_decoder_process_until_end_of_metadata</name>
      <anchor>a32</anchor>
      <arglist>(FLAC__StreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_decoder_process_until_end_of_stream</name>
      <anchor>a33</anchor>
      <arglist>(FLAC__StreamDecoder *decoder)</arglist>
    </member>
    <member kind="variable">
      <type>const char *const</type>
      <name>FLAC__StreamDecoderStateString</name>
      <anchor>a0</anchor>
      <arglist>[]</arglist>
    </member>
    <member kind="variable">
      <type>const char *const</type>
      <name>FLAC__StreamDecoderReadStatusString</name>
      <anchor>a1</anchor>
      <arglist>[]</arglist>
    </member>
    <member kind="variable">
      <type>const char *const</type>
      <name>FLAC__StreamDecoderWriteStatusString</name>
      <anchor>a2</anchor>
      <arglist>[]</arglist>
    </member>
    <member kind="variable">
      <type>const char *const</type>
      <name>FLAC__StreamDecoderErrorStatusString</name>
      <anchor>a3</anchor>
      <arglist>[]</arglist>
    </member>
  </compound>
  <compound kind="file">
    <name>stream_decoder.h</name>
    <path>/home/jcoalson/flac/build/include/OggFLAC/</path>
    <filename>OggFLAC_2stream__decoder_8h.html</filename>
    <class kind="struct">OggFLAC__StreamDecoder</class>
    <member kind="typedef">
      <type>FLAC__StreamDecoderReadStatus(*</type>
      <name>OggFLAC__StreamDecoderReadCallback</name>
      <anchor>a1</anchor>
      <arglist>)(const OggFLAC__StreamDecoder *decoder, FLAC__byte buffer[], unsigned *bytes, void *client_data)</arglist>
    </member>
    <member kind="typedef">
      <type>FLAC__StreamDecoderWriteStatus(*</type>
      <name>OggFLAC__StreamDecoderWriteCallback</name>
      <anchor>a2</anchor>
      <arglist>)(const OggFLAC__StreamDecoder *decoder, const FLAC__Frame *frame, const FLAC__int32 *const buffer[], void *client_data)</arglist>
    </member>
    <member kind="typedef">
      <type>void(*</type>
      <name>OggFLAC__StreamDecoderMetadataCallback</name>
      <anchor>a3</anchor>
      <arglist>)(const OggFLAC__StreamDecoder *decoder, const FLAC__StreamMetadata *metadata, void *client_data)</arglist>
    </member>
    <member kind="typedef">
      <type>void(*</type>
      <name>OggFLAC__StreamDecoderErrorCallback</name>
      <anchor>a4</anchor>
      <arglist>)(const OggFLAC__StreamDecoder *decoder, FLAC__StreamDecoderErrorStatus status, void *client_data)</arglist>
    </member>
    <member kind="enumeration">
      <name>OggFLAC__StreamDecoderState</name>
      <anchor>a33</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>OggFLAC__STREAM_DECODER_OK</name>
      <anchor>a33a5</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>OggFLAC__STREAM_DECODER_OGG_ERROR</name>
      <anchor>a33a6</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>OggFLAC__STREAM_DECODER_READ_ERROR</name>
      <anchor>a33a7</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>OggFLAC__STREAM_DECODER_FLAC_STREAM_DECODER_ERROR</name>
      <anchor>a33a8</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>OggFLAC__STREAM_DECODER_INVALID_CALLBACK</name>
      <anchor>a33a9</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>OggFLAC__STREAM_DECODER_MEMORY_ALLOCATION_ERROR</name>
      <anchor>a33a10</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>OggFLAC__STREAM_DECODER_ALREADY_INITIALIZED</name>
      <anchor>a33a11</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>OggFLAC__STREAM_DECODER_UNINITIALIZED</name>
      <anchor>a33a12</anchor>
      <arglist></arglist>
    </member>
    <member kind="function">
      <type>OggFLAC__StreamDecoder *</type>
      <name>OggFLAC__stream_decoder_new</name>
      <anchor>a5</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>OggFLAC__stream_decoder_delete</name>
      <anchor>a6</anchor>
      <arglist>(OggFLAC__StreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>OggFLAC__stream_decoder_set_read_callback</name>
      <anchor>a7</anchor>
      <arglist>(OggFLAC__StreamDecoder *decoder, OggFLAC__StreamDecoderReadCallback value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>OggFLAC__stream_decoder_set_write_callback</name>
      <anchor>a8</anchor>
      <arglist>(OggFLAC__StreamDecoder *decoder, OggFLAC__StreamDecoderWriteCallback value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>OggFLAC__stream_decoder_set_metadata_callback</name>
      <anchor>a9</anchor>
      <arglist>(OggFLAC__StreamDecoder *decoder, OggFLAC__StreamDecoderMetadataCallback value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>OggFLAC__stream_decoder_set_error_callback</name>
      <anchor>a10</anchor>
      <arglist>(OggFLAC__StreamDecoder *decoder, OggFLAC__StreamDecoderErrorCallback value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>OggFLAC__stream_decoder_set_client_data</name>
      <anchor>a11</anchor>
      <arglist>(OggFLAC__StreamDecoder *decoder, void *value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>OggFLAC__stream_decoder_set_serial_number</name>
      <anchor>a12</anchor>
      <arglist>(OggFLAC__StreamDecoder *decoder, long serial_number)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>OggFLAC__stream_decoder_set_metadata_respond</name>
      <anchor>a13</anchor>
      <arglist>(OggFLAC__StreamDecoder *decoder, FLAC__MetadataType type)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>OggFLAC__stream_decoder_set_metadata_respond_application</name>
      <anchor>a14</anchor>
      <arglist>(OggFLAC__StreamDecoder *decoder, const FLAC__byte id[4])</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>OggFLAC__stream_decoder_set_metadata_respond_all</name>
      <anchor>a15</anchor>
      <arglist>(OggFLAC__StreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>OggFLAC__stream_decoder_set_metadata_ignore</name>
      <anchor>a16</anchor>
      <arglist>(OggFLAC__StreamDecoder *decoder, FLAC__MetadataType type)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>OggFLAC__stream_decoder_set_metadata_ignore_application</name>
      <anchor>a17</anchor>
      <arglist>(OggFLAC__StreamDecoder *decoder, const FLAC__byte id[4])</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>OggFLAC__stream_decoder_set_metadata_ignore_all</name>
      <anchor>a18</anchor>
      <arglist>(OggFLAC__StreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>OggFLAC__StreamDecoderState</type>
      <name>OggFLAC__stream_decoder_get_state</name>
      <anchor>a19</anchor>
      <arglist>(const OggFLAC__StreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__StreamDecoderState</type>
      <name>OggFLAC__stream_decoder_get_FLAC_stream_decoder_state</name>
      <anchor>a20</anchor>
      <arglist>(const OggFLAC__StreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>OggFLAC__stream_decoder_get_channels</name>
      <anchor>a21</anchor>
      <arglist>(const OggFLAC__StreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__ChannelAssignment</type>
      <name>OggFLAC__stream_decoder_get_channel_assignment</name>
      <anchor>a22</anchor>
      <arglist>(const OggFLAC__StreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>OggFLAC__stream_decoder_get_bits_per_sample</name>
      <anchor>a23</anchor>
      <arglist>(const OggFLAC__StreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>OggFLAC__stream_decoder_get_sample_rate</name>
      <anchor>a24</anchor>
      <arglist>(const OggFLAC__StreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>OggFLAC__stream_decoder_get_blocksize</name>
      <anchor>a25</anchor>
      <arglist>(const OggFLAC__StreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>OggFLAC__StreamDecoderState</type>
      <name>OggFLAC__stream_decoder_init</name>
      <anchor>a26</anchor>
      <arglist>(OggFLAC__StreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>OggFLAC__stream_decoder_finish</name>
      <anchor>a27</anchor>
      <arglist>(OggFLAC__StreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>OggFLAC__stream_decoder_flush</name>
      <anchor>a28</anchor>
      <arglist>(OggFLAC__StreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>OggFLAC__stream_decoder_reset</name>
      <anchor>a29</anchor>
      <arglist>(OggFLAC__StreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>OggFLAC__stream_decoder_process_single</name>
      <anchor>a30</anchor>
      <arglist>(OggFLAC__StreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>OggFLAC__stream_decoder_process_until_end_of_metadata</name>
      <anchor>a31</anchor>
      <arglist>(OggFLAC__StreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>OggFLAC__stream_decoder_process_until_end_of_stream</name>
      <anchor>a32</anchor>
      <arglist>(OggFLAC__StreamDecoder *decoder)</arglist>
    </member>
    <member kind="variable">
      <type>const char *const</type>
      <name>OggFLAC__StreamDecoderStateString</name>
      <anchor>a0</anchor>
      <arglist>[]</arglist>
    </member>
  </compound>
  <compound kind="file">
    <name>stream_encoder.h</name>
    <path>/home/jcoalson/flac/build/include/FLAC/</path>
    <filename>FLAC_2stream__encoder_8h.html</filename>
    <class kind="struct">FLAC__StreamEncoder</class>
    <member kind="typedef">
      <type>FLAC__StreamEncoderWriteStatus(*</type>
      <name>FLAC__StreamEncoderWriteCallback</name>
      <anchor>a2</anchor>
      <arglist>)(const FLAC__StreamEncoder *encoder, const FLAC__byte buffer[], unsigned bytes, unsigned samples, unsigned current_frame, void *client_data)</arglist>
    </member>
    <member kind="typedef">
      <type>void(*</type>
      <name>FLAC__StreamEncoderMetadataCallback</name>
      <anchor>a3</anchor>
      <arglist>)(const FLAC__StreamEncoder *encoder, const FLAC__StreamMetadata *metadata, void *client_data)</arglist>
    </member>
    <member kind="enumeration">
      <name>FLAC__StreamEncoderState</name>
      <anchor>a51</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__STREAM_ENCODER_OK</name>
      <anchor>a51a4</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__STREAM_ENCODER_VERIFY_DECODER_ERROR</name>
      <anchor>a51a5</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__STREAM_ENCODER_VERIFY_MISMATCH_IN_AUDIO_DATA</name>
      <anchor>a51a6</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__STREAM_ENCODER_INVALID_CALLBACK</name>
      <anchor>a51a7</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__STREAM_ENCODER_INVALID_NUMBER_OF_CHANNELS</name>
      <anchor>a51a8</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__STREAM_ENCODER_INVALID_BITS_PER_SAMPLE</name>
      <anchor>a51a9</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__STREAM_ENCODER_INVALID_SAMPLE_RATE</name>
      <anchor>a51a10</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__STREAM_ENCODER_INVALID_BLOCK_SIZE</name>
      <anchor>a51a11</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__STREAM_ENCODER_INVALID_MAX_LPC_ORDER</name>
      <anchor>a51a12</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__STREAM_ENCODER_INVALID_QLP_COEFF_PRECISION</name>
      <anchor>a51a13</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__STREAM_ENCODER_MID_SIDE_CHANNELS_MISMATCH</name>
      <anchor>a51a14</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__STREAM_ENCODER_MID_SIDE_SAMPLE_SIZE_MISMATCH</name>
      <anchor>a51a15</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__STREAM_ENCODER_ILLEGAL_MID_SIDE_FORCE</name>
      <anchor>a51a16</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__STREAM_ENCODER_BLOCK_SIZE_TOO_SMALL_FOR_LPC_ORDER</name>
      <anchor>a51a17</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__STREAM_ENCODER_NOT_STREAMABLE</name>
      <anchor>a51a18</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__STREAM_ENCODER_FRAMING_ERROR</name>
      <anchor>a51a19</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__STREAM_ENCODER_INVALID_METADATA</name>
      <anchor>a51a20</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__STREAM_ENCODER_FATAL_ERROR_WHILE_ENCODING</name>
      <anchor>a51a21</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__STREAM_ENCODER_FATAL_ERROR_WHILE_WRITING</name>
      <anchor>a51a22</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__STREAM_ENCODER_MEMORY_ALLOCATION_ERROR</name>
      <anchor>a51a23</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__STREAM_ENCODER_ALREADY_INITIALIZED</name>
      <anchor>a51a24</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__STREAM_ENCODER_UNINITIALIZED</name>
      <anchor>a51a25</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumeration">
      <name>FLAC__StreamEncoderWriteStatus</name>
      <anchor>a52</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__STREAM_ENCODER_WRITE_STATUS_OK</name>
      <anchor>a52a26</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__STREAM_ENCODER_WRITE_STATUS_FATAL_ERROR</name>
      <anchor>a52a27</anchor>
      <arglist></arglist>
    </member>
    <member kind="function">
      <type>FLAC__StreamEncoder *</type>
      <name>FLAC__stream_encoder_new</name>
      <anchor>a4</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>FLAC__stream_encoder_delete</name>
      <anchor>a5</anchor>
      <arglist>(FLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_set_verify</name>
      <anchor>a6</anchor>
      <arglist>(FLAC__StreamEncoder *encoder, FLAC__bool value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_set_streamable_subset</name>
      <anchor>a7</anchor>
      <arglist>(FLAC__StreamEncoder *encoder, FLAC__bool value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_set_do_mid_side_stereo</name>
      <anchor>a8</anchor>
      <arglist>(FLAC__StreamEncoder *encoder, FLAC__bool value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_set_loose_mid_side_stereo</name>
      <anchor>a9</anchor>
      <arglist>(FLAC__StreamEncoder *encoder, FLAC__bool value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_set_channels</name>
      <anchor>a10</anchor>
      <arglist>(FLAC__StreamEncoder *encoder, unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_set_bits_per_sample</name>
      <anchor>a11</anchor>
      <arglist>(FLAC__StreamEncoder *encoder, unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_set_sample_rate</name>
      <anchor>a12</anchor>
      <arglist>(FLAC__StreamEncoder *encoder, unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_set_blocksize</name>
      <anchor>a13</anchor>
      <arglist>(FLAC__StreamEncoder *encoder, unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_set_max_lpc_order</name>
      <anchor>a14</anchor>
      <arglist>(FLAC__StreamEncoder *encoder, unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_set_qlp_coeff_precision</name>
      <anchor>a15</anchor>
      <arglist>(FLAC__StreamEncoder *encoder, unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_set_do_qlp_coeff_prec_search</name>
      <anchor>a16</anchor>
      <arglist>(FLAC__StreamEncoder *encoder, FLAC__bool value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_set_do_escape_coding</name>
      <anchor>a17</anchor>
      <arglist>(FLAC__StreamEncoder *encoder, FLAC__bool value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_set_do_exhaustive_model_search</name>
      <anchor>a18</anchor>
      <arglist>(FLAC__StreamEncoder *encoder, FLAC__bool value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_set_min_residual_partition_order</name>
      <anchor>a19</anchor>
      <arglist>(FLAC__StreamEncoder *encoder, unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_set_max_residual_partition_order</name>
      <anchor>a20</anchor>
      <arglist>(FLAC__StreamEncoder *encoder, unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_set_rice_parameter_search_dist</name>
      <anchor>a21</anchor>
      <arglist>(FLAC__StreamEncoder *encoder, unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_set_total_samples_estimate</name>
      <anchor>a22</anchor>
      <arglist>(FLAC__StreamEncoder *encoder, FLAC__uint64 value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_set_metadata</name>
      <anchor>a23</anchor>
      <arglist>(FLAC__StreamEncoder *encoder, FLAC__StreamMetadata **metadata, unsigned num_blocks)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_set_write_callback</name>
      <anchor>a24</anchor>
      <arglist>(FLAC__StreamEncoder *encoder, FLAC__StreamEncoderWriteCallback value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_set_metadata_callback</name>
      <anchor>a25</anchor>
      <arglist>(FLAC__StreamEncoder *encoder, FLAC__StreamEncoderMetadataCallback value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_set_client_data</name>
      <anchor>a26</anchor>
      <arglist>(FLAC__StreamEncoder *encoder, void *value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__StreamEncoderState</type>
      <name>FLAC__stream_encoder_get_state</name>
      <anchor>a27</anchor>
      <arglist>(const FLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__StreamDecoderState</type>
      <name>FLAC__stream_encoder_get_verify_decoder_state</name>
      <anchor>a28</anchor>
      <arglist>(const FLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>FLAC__stream_encoder_get_verify_decoder_error_stats</name>
      <anchor>a29</anchor>
      <arglist>(const FLAC__StreamEncoder *encoder, FLAC__uint64 *absolute_sample, unsigned *frame_number, unsigned *channel, unsigned *sample, FLAC__int32 *expected, FLAC__int32 *got)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_get_verify</name>
      <anchor>a30</anchor>
      <arglist>(const FLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_get_streamable_subset</name>
      <anchor>a31</anchor>
      <arglist>(const FLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_get_do_mid_side_stereo</name>
      <anchor>a32</anchor>
      <arglist>(const FLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_get_loose_mid_side_stereo</name>
      <anchor>a33</anchor>
      <arglist>(const FLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>FLAC__stream_encoder_get_channels</name>
      <anchor>a34</anchor>
      <arglist>(const FLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>FLAC__stream_encoder_get_bits_per_sample</name>
      <anchor>a35</anchor>
      <arglist>(const FLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>FLAC__stream_encoder_get_sample_rate</name>
      <anchor>a36</anchor>
      <arglist>(const FLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>FLAC__stream_encoder_get_blocksize</name>
      <anchor>a37</anchor>
      <arglist>(const FLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>FLAC__stream_encoder_get_max_lpc_order</name>
      <anchor>a38</anchor>
      <arglist>(const FLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>FLAC__stream_encoder_get_qlp_coeff_precision</name>
      <anchor>a39</anchor>
      <arglist>(const FLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_get_do_qlp_coeff_prec_search</name>
      <anchor>a40</anchor>
      <arglist>(const FLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_get_do_escape_coding</name>
      <anchor>a41</anchor>
      <arglist>(const FLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_get_do_exhaustive_model_search</name>
      <anchor>a42</anchor>
      <arglist>(const FLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>FLAC__stream_encoder_get_min_residual_partition_order</name>
      <anchor>a43</anchor>
      <arglist>(const FLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>FLAC__stream_encoder_get_max_residual_partition_order</name>
      <anchor>a44</anchor>
      <arglist>(const FLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>FLAC__stream_encoder_get_rice_parameter_search_dist</name>
      <anchor>a45</anchor>
      <arglist>(const FLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__uint64</type>
      <name>FLAC__stream_encoder_get_total_samples_estimate</name>
      <anchor>a46</anchor>
      <arglist>(const FLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__StreamEncoderState</type>
      <name>FLAC__stream_encoder_init</name>
      <anchor>a47</anchor>
      <arglist>(FLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>FLAC__stream_encoder_finish</name>
      <anchor>a48</anchor>
      <arglist>(FLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_process</name>
      <anchor>a49</anchor>
      <arglist>(FLAC__StreamEncoder *encoder, const FLAC__int32 *const buffer[], unsigned samples)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_process_interleaved</name>
      <anchor>a50</anchor>
      <arglist>(FLAC__StreamEncoder *encoder, const FLAC__int32 buffer[], unsigned samples)</arglist>
    </member>
    <member kind="variable">
      <type>const char *const</type>
      <name>FLAC__StreamEncoderStateString</name>
      <anchor>a0</anchor>
      <arglist>[]</arglist>
    </member>
    <member kind="variable">
      <type>const char *const</type>
      <name>FLAC__StreamEncoderWriteStatusString</name>
      <anchor>a1</anchor>
      <arglist>[]</arglist>
    </member>
  </compound>
  <compound kind="file">
    <name>stream_encoder.h</name>
    <path>/home/jcoalson/flac/build/include/OggFLAC/</path>
    <filename>OggFLAC_2stream__encoder_8h.html</filename>
    <class kind="struct">OggFLAC__StreamEncoder</class>
    <member kind="typedef">
      <type>FLAC__StreamEncoderWriteStatus(*</type>
      <name>OggFLAC__StreamEncoderWriteCallback</name>
      <anchor>a1</anchor>
      <arglist>)(const OggFLAC__StreamEncoder *encoder, const FLAC__byte buffer[], unsigned bytes, unsigned samples, unsigned current_frame, void *client_data)</arglist>
    </member>
    <member kind="enumeration">
      <name>OggFLAC__StreamEncoderState</name>
      <anchor>a50</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>OggFLAC__STREAM_ENCODER_OK</name>
      <anchor>a50a2</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>OggFLAC__STREAM_ENCODER_OGG_ERROR</name>
      <anchor>a50a3</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>OggFLAC__STREAM_ENCODER_FLAC_STREAM_ENCODER_ERROR</name>
      <anchor>a50a4</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>OggFLAC__STREAM_ENCODER_INVALID_CALLBACK</name>
      <anchor>a50a5</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>OggFLAC__STREAM_ENCODER_MEMORY_ALLOCATION_ERROR</name>
      <anchor>a50a6</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>OggFLAC__STREAM_ENCODER_ALREADY_INITIALIZED</name>
      <anchor>a50a7</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>OggFLAC__STREAM_ENCODER_UNINITIALIZED</name>
      <anchor>a50a8</anchor>
      <arglist></arglist>
    </member>
    <member kind="function">
      <type>OggFLAC__StreamEncoder *</type>
      <name>OggFLAC__stream_encoder_new</name>
      <anchor>a2</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>OggFLAC__stream_encoder_delete</name>
      <anchor>a3</anchor>
      <arglist>(OggFLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>OggFLAC__stream_encoder_set_serial_number</name>
      <anchor>a4</anchor>
      <arglist>(OggFLAC__StreamEncoder *encoder, long serial_number)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>OggFLAC__stream_encoder_set_verify</name>
      <anchor>a5</anchor>
      <arglist>(OggFLAC__StreamEncoder *encoder, FLAC__bool value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>OggFLAC__stream_encoder_set_streamable_subset</name>
      <anchor>a6</anchor>
      <arglist>(OggFLAC__StreamEncoder *encoder, FLAC__bool value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>OggFLAC__stream_encoder_set_do_mid_side_stereo</name>
      <anchor>a7</anchor>
      <arglist>(OggFLAC__StreamEncoder *encoder, FLAC__bool value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>OggFLAC__stream_encoder_set_loose_mid_side_stereo</name>
      <anchor>a8</anchor>
      <arglist>(OggFLAC__StreamEncoder *encoder, FLAC__bool value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>OggFLAC__stream_encoder_set_channels</name>
      <anchor>a9</anchor>
      <arglist>(OggFLAC__StreamEncoder *encoder, unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>OggFLAC__stream_encoder_set_bits_per_sample</name>
      <anchor>a10</anchor>
      <arglist>(OggFLAC__StreamEncoder *encoder, unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>OggFLAC__stream_encoder_set_sample_rate</name>
      <anchor>a11</anchor>
      <arglist>(OggFLAC__StreamEncoder *encoder, unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>OggFLAC__stream_encoder_set_blocksize</name>
      <anchor>a12</anchor>
      <arglist>(OggFLAC__StreamEncoder *encoder, unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>OggFLAC__stream_encoder_set_max_lpc_order</name>
      <anchor>a13</anchor>
      <arglist>(OggFLAC__StreamEncoder *encoder, unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>OggFLAC__stream_encoder_set_qlp_coeff_precision</name>
      <anchor>a14</anchor>
      <arglist>(OggFLAC__StreamEncoder *encoder, unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>OggFLAC__stream_encoder_set_do_qlp_coeff_prec_search</name>
      <anchor>a15</anchor>
      <arglist>(OggFLAC__StreamEncoder *encoder, FLAC__bool value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>OggFLAC__stream_encoder_set_do_escape_coding</name>
      <anchor>a16</anchor>
      <arglist>(OggFLAC__StreamEncoder *encoder, FLAC__bool value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>OggFLAC__stream_encoder_set_do_exhaustive_model_search</name>
      <anchor>a17</anchor>
      <arglist>(OggFLAC__StreamEncoder *encoder, FLAC__bool value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>OggFLAC__stream_encoder_set_min_residual_partition_order</name>
      <anchor>a18</anchor>
      <arglist>(OggFLAC__StreamEncoder *encoder, unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>OggFLAC__stream_encoder_set_max_residual_partition_order</name>
      <anchor>a19</anchor>
      <arglist>(OggFLAC__StreamEncoder *encoder, unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>OggFLAC__stream_encoder_set_rice_parameter_search_dist</name>
      <anchor>a20</anchor>
      <arglist>(OggFLAC__StreamEncoder *encoder, unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>OggFLAC__stream_encoder_set_total_samples_estimate</name>
      <anchor>a21</anchor>
      <arglist>(OggFLAC__StreamEncoder *encoder, FLAC__uint64 value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>OggFLAC__stream_encoder_set_metadata</name>
      <anchor>a22</anchor>
      <arglist>(OggFLAC__StreamEncoder *encoder, FLAC__StreamMetadata **metadata, unsigned num_blocks)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>OggFLAC__stream_encoder_set_write_callback</name>
      <anchor>a23</anchor>
      <arglist>(OggFLAC__StreamEncoder *encoder, OggFLAC__StreamEncoderWriteCallback value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>OggFLAC__stream_encoder_set_client_data</name>
      <anchor>a24</anchor>
      <arglist>(OggFLAC__StreamEncoder *encoder, void *value)</arglist>
    </member>
    <member kind="function">
      <type>OggFLAC__StreamEncoderState</type>
      <name>OggFLAC__stream_encoder_get_state</name>
      <anchor>a25</anchor>
      <arglist>(const OggFLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__StreamEncoderState</type>
      <name>OggFLAC__stream_encoder_get_FLAC_stream_encoder_state</name>
      <anchor>a26</anchor>
      <arglist>(const OggFLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__StreamDecoderState</type>
      <name>OggFLAC__stream_encoder_get_verify_decoder_state</name>
      <anchor>a27</anchor>
      <arglist>(const OggFLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>OggFLAC__stream_encoder_get_verify_decoder_error_stats</name>
      <anchor>a28</anchor>
      <arglist>(const OggFLAC__StreamEncoder *encoder, FLAC__uint64 *absolute_sample, unsigned *frame_number, unsigned *channel, unsigned *sample, FLAC__int32 *expected, FLAC__int32 *got)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>OggFLAC__stream_encoder_get_verify</name>
      <anchor>a29</anchor>
      <arglist>(const OggFLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>OggFLAC__stream_encoder_get_streamable_subset</name>
      <anchor>a30</anchor>
      <arglist>(const OggFLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>OggFLAC__stream_encoder_get_do_mid_side_stereo</name>
      <anchor>a31</anchor>
      <arglist>(const OggFLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>OggFLAC__stream_encoder_get_loose_mid_side_stereo</name>
      <anchor>a32</anchor>
      <arglist>(const OggFLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>OggFLAC__stream_encoder_get_channels</name>
      <anchor>a33</anchor>
      <arglist>(const OggFLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>OggFLAC__stream_encoder_get_bits_per_sample</name>
      <anchor>a34</anchor>
      <arglist>(const OggFLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>OggFLAC__stream_encoder_get_sample_rate</name>
      <anchor>a35</anchor>
      <arglist>(const OggFLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>OggFLAC__stream_encoder_get_blocksize</name>
      <anchor>a36</anchor>
      <arglist>(const OggFLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>OggFLAC__stream_encoder_get_max_lpc_order</name>
      <anchor>a37</anchor>
      <arglist>(const OggFLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>OggFLAC__stream_encoder_get_qlp_coeff_precision</name>
      <anchor>a38</anchor>
      <arglist>(const OggFLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>OggFLAC__stream_encoder_get_do_qlp_coeff_prec_search</name>
      <anchor>a39</anchor>
      <arglist>(const OggFLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>OggFLAC__stream_encoder_get_do_escape_coding</name>
      <anchor>a40</anchor>
      <arglist>(const OggFLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>OggFLAC__stream_encoder_get_do_exhaustive_model_search</name>
      <anchor>a41</anchor>
      <arglist>(const OggFLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>OggFLAC__stream_encoder_get_min_residual_partition_order</name>
      <anchor>a42</anchor>
      <arglist>(const OggFLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>OggFLAC__stream_encoder_get_max_residual_partition_order</name>
      <anchor>a43</anchor>
      <arglist>(const OggFLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>OggFLAC__stream_encoder_get_rice_parameter_search_dist</name>
      <anchor>a44</anchor>
      <arglist>(const OggFLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__uint64</type>
      <name>OggFLAC__stream_encoder_get_total_samples_estimate</name>
      <anchor>a45</anchor>
      <arglist>(const OggFLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>OggFLAC__StreamEncoderState</type>
      <name>OggFLAC__stream_encoder_init</name>
      <anchor>a46</anchor>
      <arglist>(OggFLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>OggFLAC__stream_encoder_finish</name>
      <anchor>a47</anchor>
      <arglist>(OggFLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>OggFLAC__stream_encoder_process</name>
      <anchor>a48</anchor>
      <arglist>(OggFLAC__StreamEncoder *encoder, const FLAC__int32 *const buffer[], unsigned samples)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>OggFLAC__stream_encoder_process_interleaved</name>
      <anchor>a49</anchor>
      <arglist>(OggFLAC__StreamEncoder *encoder, const FLAC__int32 buffer[], unsigned samples)</arglist>
    </member>
    <member kind="variable">
      <type>const char *const</type>
      <name>OggFLAC__StreamEncoderStateString</name>
      <anchor>a0</anchor>
      <arglist>[]</arglist>
    </member>
  </compound>
  <compound kind="struct">
    <name>FLAC__EntropyCodingMethod</name>
    <filename>structFLAC____EntropyCodingMethod.html</filename>
    <member kind="variable">
      <type>FLAC__EntropyCodingMethodType</type>
      <name>type</name>
      <anchor>m0</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>FLAC__EntropyCodingMethod::@0</type>
      <name>data</name>
      <anchor>m2</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>FLAC__EntropyCodingMethod_PartitionedRice</type>
      <name>partitioned_rice</name>
      <anchor>m0</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="struct">
    <name>FLAC__EntropyCodingMethod_PartitionedRice</name>
    <filename>structFLAC____EntropyCodingMethod__PartitionedRice.html</filename>
    <member kind="variable">
      <type>unsigned</type>
      <name>order</name>
      <anchor>m0</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const FLAC__EntropyCodingMethod_PartitionedRiceContents *</type>
      <name>contents</name>
      <anchor>m1</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="struct">
    <name>FLAC__EntropyCodingMethod_PartitionedRiceContents</name>
    <filename>structFLAC____EntropyCodingMethod__PartitionedRiceContents.html</filename>
    <member kind="variable">
      <type>unsigned *</type>
      <name>parameters</name>
      <anchor>m0</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>unsigned *</type>
      <name>raw_bits</name>
      <anchor>m1</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>unsigned</type>
      <name>capacity_by_order</name>
      <anchor>m2</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="struct">
    <name>FLAC__FileDecoder</name>
    <filename>structFLAC____FileDecoder.html</filename>
    <member kind="variable">
      <type>FLAC__FileDecoderProtected *</type>
      <name>protected_</name>
      <anchor>m0</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>FLAC__FileDecoderPrivate *</type>
      <name>private_</name>
      <anchor>m1</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="struct">
    <name>FLAC__FileEncoder</name>
    <filename>structFLAC____FileEncoder.html</filename>
    <member kind="variable">
      <type>FLAC__FileEncoderProtected *</type>
      <name>protected_</name>
      <anchor>m0</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>FLAC__FileEncoderPrivate *</type>
      <name>private_</name>
      <anchor>m1</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="struct">
    <name>FLAC__Frame</name>
    <filename>structFLAC____Frame.html</filename>
    <member kind="variable">
      <type>FLAC__FrameHeader</type>
      <name>header</name>
      <anchor>m0</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>FLAC__Subframe</type>
      <name>subframes</name>
      <anchor>m1</anchor>
      <arglist>[FLAC__MAX_CHANNELS]</arglist>
    </member>
    <member kind="variable">
      <type>FLAC__FrameFooter</type>
      <name>footer</name>
      <anchor>m2</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="struct">
    <name>FLAC__FrameFooter</name>
    <filename>structFLAC____FrameFooter.html</filename>
    <member kind="variable">
      <type>FLAC__uint16</type>
      <name>crc</name>
      <anchor>m0</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="struct">
    <name>FLAC__FrameHeader</name>
    <filename>structFLAC____FrameHeader.html</filename>
    <member kind="variable">
      <type>unsigned</type>
      <name>blocksize</name>
      <anchor>m0</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>unsigned</type>
      <name>sample_rate</name>
      <anchor>m1</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>unsigned</type>
      <name>channels</name>
      <anchor>m2</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>FLAC__ChannelAssignment</type>
      <name>channel_assignment</name>
      <anchor>m3</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>unsigned</type>
      <name>bits_per_sample</name>
      <anchor>m4</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>FLAC__FrameNumberType</type>
      <name>number_type</name>
      <anchor>m5</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>FLAC__FrameHeader::@2</type>
      <name>number</name>
      <anchor>m8</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>FLAC__uint32</type>
      <name>frame_number</name>
      <anchor>m0</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>FLAC__uint64</type>
      <name>sample_number</name>
      <anchor>m1</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>FLAC__uint8</type>
      <name>crc</name>
      <anchor>m9</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="struct">
    <name>FLAC__SeekableStreamDecoder</name>
    <filename>structFLAC____SeekableStreamDecoder.html</filename>
    <member kind="variable">
      <type>FLAC__SeekableStreamDecoderProtected *</type>
      <name>protected_</name>
      <anchor>m0</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>FLAC__SeekableStreamDecoderPrivate *</type>
      <name>private_</name>
      <anchor>m1</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="struct">
    <name>FLAC__SeekableStreamEncoder</name>
    <filename>structFLAC____SeekableStreamEncoder.html</filename>
    <member kind="variable">
      <type>FLAC__SeekableStreamEncoderProtected *</type>
      <name>protected_</name>
      <anchor>m0</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>FLAC__SeekableStreamEncoderPrivate *</type>
      <name>private_</name>
      <anchor>m1</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="struct">
    <name>FLAC__StreamDecoder</name>
    <filename>structFLAC____StreamDecoder.html</filename>
    <member kind="variable">
      <type>FLAC__StreamDecoderProtected *</type>
      <name>protected_</name>
      <anchor>m0</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>FLAC__StreamDecoderPrivate *</type>
      <name>private_</name>
      <anchor>m1</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="struct">
    <name>FLAC__StreamEncoder</name>
    <filename>structFLAC____StreamEncoder.html</filename>
    <member kind="variable">
      <type>FLAC__StreamEncoderProtected *</type>
      <name>protected_</name>
      <anchor>m0</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>FLAC__StreamEncoderPrivate *</type>
      <name>private_</name>
      <anchor>m1</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="struct">
    <name>FLAC__StreamMetadata</name>
    <filename>structFLAC____StreamMetadata.html</filename>
    <member kind="variable">
      <type>FLAC__MetadataType</type>
      <name>type</name>
      <anchor>m0</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>FLAC__bool</type>
      <name>is_last</name>
      <anchor>m1</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>unsigned</type>
      <name>length</name>
      <anchor>m2</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>FLAC__StreamMetadata::@3</type>
      <name>data</name>
      <anchor>m8</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>FLAC__StreamMetadata_StreamInfo</type>
      <name>stream_info</name>
      <anchor>m0</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>FLAC__StreamMetadata_Padding</type>
      <name>padding</name>
      <anchor>m1</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>FLAC__StreamMetadata_Application</type>
      <name>application</name>
      <anchor>m2</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>FLAC__StreamMetadata_SeekTable</type>
      <name>seek_table</name>
      <anchor>m3</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>FLAC__StreamMetadata_VorbisComment</type>
      <name>vorbis_comment</name>
      <anchor>m4</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="struct">
    <name>FLAC__StreamMetadata_Application</name>
    <filename>structFLAC____StreamMetadata__Application.html</filename>
    <member kind="variable">
      <type>FLAC__byte</type>
      <name>id</name>
      <anchor>m0</anchor>
      <arglist>[4]</arglist>
    </member>
    <member kind="variable">
      <type>FLAC__byte *</type>
      <name>data</name>
      <anchor>m1</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="struct">
    <name>FLAC__StreamMetadata_Padding</name>
    <filename>structFLAC____StreamMetadata__Padding.html</filename>
    <member kind="variable">
      <type>int</type>
      <name>dummy</name>
      <anchor>m0</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="struct">
    <name>FLAC__StreamMetadata_SeekPoint</name>
    <filename>structFLAC____StreamMetadata__SeekPoint.html</filename>
    <member kind="variable">
      <type>FLAC__uint64</type>
      <name>sample_number</name>
      <anchor>m0</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>FLAC__uint64</type>
      <name>stream_offset</name>
      <anchor>m1</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>unsigned</type>
      <name>frame_samples</name>
      <anchor>m2</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="struct">
    <name>FLAC__StreamMetadata_SeekTable</name>
    <filename>structFLAC____StreamMetadata__SeekTable.html</filename>
    <member kind="variable">
      <type>unsigned</type>
      <name>num_points</name>
      <anchor>m0</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>FLAC__StreamMetadata_SeekPoint *</type>
      <name>points</name>
      <anchor>m1</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="struct">
    <name>FLAC__StreamMetadata_StreamInfo</name>
    <filename>structFLAC____StreamMetadata__StreamInfo.html</filename>
    <member kind="variable">
      <type>unsigned</type>
      <name>min_blocksize</name>
      <anchor>m0</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>unsigned</type>
      <name>max_blocksize</name>
      <anchor>m1</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>unsigned</type>
      <name>min_framesize</name>
      <anchor>m2</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>unsigned</type>
      <name>max_framesize</name>
      <anchor>m3</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>unsigned</type>
      <name>sample_rate</name>
      <anchor>m4</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>unsigned</type>
      <name>channels</name>
      <anchor>m5</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>unsigned</type>
      <name>bits_per_sample</name>
      <anchor>m6</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>FLAC__uint64</type>
      <name>total_samples</name>
      <anchor>m7</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>FLAC__byte</type>
      <name>md5sum</name>
      <anchor>m8</anchor>
      <arglist>[16]</arglist>
    </member>
  </compound>
  <compound kind="struct">
    <name>FLAC__StreamMetadata_VorbisComment</name>
    <filename>structFLAC____StreamMetadata__VorbisComment.html</filename>
    <member kind="variable">
      <type>FLAC__StreamMetadata_VorbisComment_Entry</type>
      <name>vendor_string</name>
      <anchor>m0</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>FLAC__uint32</type>
      <name>num_comments</name>
      <anchor>m1</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>FLAC__StreamMetadata_VorbisComment_Entry *</type>
      <name>comments</name>
      <anchor>m2</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="struct">
    <name>FLAC__StreamMetadata_VorbisComment_Entry</name>
    <filename>structFLAC____StreamMetadata__VorbisComment__Entry.html</filename>
    <member kind="variable">
      <type>FLAC__uint32</type>
      <name>length</name>
      <anchor>m0</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>FLAC__byte *</type>
      <name>entry</name>
      <anchor>m1</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="struct">
    <name>FLAC__Subframe</name>
    <filename>structFLAC____Subframe.html</filename>
    <member kind="variable">
      <type>FLAC__SubframeType</type>
      <name>type</name>
      <anchor>m0</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>FLAC__Subframe::@1</type>
      <name>data</name>
      <anchor>m5</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>FLAC__Subframe_Constant</type>
      <name>constant</name>
      <anchor>m0</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>FLAC__Subframe_Fixed</type>
      <name>fixed</name>
      <anchor>m1</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>FLAC__Subframe_LPC</type>
      <name>lpc</name>
      <anchor>m2</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>FLAC__Subframe_Verbatim</type>
      <name>verbatim</name>
      <anchor>m3</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>unsigned</type>
      <name>wasted_bits</name>
      <anchor>m6</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="struct">
    <name>FLAC__Subframe_Constant</name>
    <filename>structFLAC____Subframe__Constant.html</filename>
    <member kind="variable">
      <type>FLAC__int32</type>
      <name>value</name>
      <anchor>m0</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="struct">
    <name>FLAC__Subframe_Fixed</name>
    <filename>structFLAC____Subframe__Fixed.html</filename>
    <member kind="variable">
      <type>FLAC__EntropyCodingMethod</type>
      <name>entropy_coding_method</name>
      <anchor>m0</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>unsigned</type>
      <name>order</name>
      <anchor>m1</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>FLAC__int32</type>
      <name>warmup</name>
      <anchor>m2</anchor>
      <arglist>[FLAC__MAX_FIXED_ORDER]</arglist>
    </member>
    <member kind="variable">
      <type>const FLAC__int32 *</type>
      <name>residual</name>
      <anchor>m3</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="struct">
    <name>FLAC__Subframe_LPC</name>
    <filename>structFLAC____Subframe__LPC.html</filename>
    <member kind="variable">
      <type>FLAC__EntropyCodingMethod</type>
      <name>entropy_coding_method</name>
      <anchor>m0</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>unsigned</type>
      <name>order</name>
      <anchor>m1</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>unsigned</type>
      <name>qlp_coeff_precision</name>
      <anchor>m2</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>int</type>
      <name>quantization_level</name>
      <anchor>m3</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>FLAC__int32</type>
      <name>qlp_coeff</name>
      <anchor>m4</anchor>
      <arglist>[FLAC__MAX_LPC_ORDER]</arglist>
    </member>
    <member kind="variable">
      <type>FLAC__int32</type>
      <name>warmup</name>
      <anchor>m5</anchor>
      <arglist>[FLAC__MAX_LPC_ORDER]</arglist>
    </member>
    <member kind="variable">
      <type>const FLAC__int32 *</type>
      <name>residual</name>
      <anchor>m6</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="struct">
    <name>FLAC__Subframe_Verbatim</name>
    <filename>structFLAC____Subframe__Verbatim.html</filename>
    <member kind="variable">
      <type>const FLAC__int32 *</type>
      <name>data</name>
      <anchor>m0</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="struct">
    <name>OggFLAC__StreamDecoder</name>
    <filename>structOggFLAC____StreamDecoder.html</filename>
    <member kind="variable">
      <type>OggFLAC__StreamDecoderProtected *</type>
      <name>protected_</name>
      <anchor>m0</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>OggFLAC__StreamDecoderPrivate *</type>
      <name>private_</name>
      <anchor>m1</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="struct">
    <name>OggFLAC__StreamEncoder</name>
    <filename>structOggFLAC____StreamEncoder.html</filename>
    <member kind="variable">
      <type>OggFLAC__StreamEncoderProtected *</type>
      <name>protected_</name>
      <anchor>m0</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>OggFLAC__StreamEncoderPrivate *</type>
      <name>private_</name>
      <anchor>m1</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="group">
    <name>flac</name>
    <title>FLAC C API</title>
    <filename>group__flac.html</filename>
    <subgroup>flac_format</subgroup>
    <subgroup>flac_metadata</subgroup>
    <subgroup>flac_decoder</subgroup>
    <subgroup>flac_encoder</subgroup>
  </compound>
  <compound kind="group">
    <name>flac_file_decoder</name>
    <title>FLAC/file_decoder.h: file decoder interface</title>
    <filename>group__flac__file__decoder.html</filename>
    <class kind="struct">FLAC__FileDecoder</class>
    <member kind="typedef">
      <type>FLAC__StreamDecoderWriteStatus(*</type>
      <name>FLAC__FileDecoderWriteCallback</name>
      <anchor>a1</anchor>
      <arglist>)(const FLAC__FileDecoder *decoder, const FLAC__Frame *frame, const FLAC__int32 *const buffer[], void *client_data)</arglist>
    </member>
    <member kind="typedef">
      <type>void(*</type>
      <name>FLAC__FileDecoderMetadataCallback</name>
      <anchor>a2</anchor>
      <arglist>)(const FLAC__FileDecoder *decoder, const FLAC__StreamMetadata *metadata, void *client_data)</arglist>
    </member>
    <member kind="typedef">
      <type>void(*</type>
      <name>FLAC__FileDecoderErrorCallback</name>
      <anchor>a3</anchor>
      <arglist>)(const FLAC__FileDecoder *decoder, FLAC__StreamDecoderErrorStatus status, void *client_data)</arglist>
    </member>
    <member kind="enumeration">
      <name>FLAC__FileDecoderState</name>
      <anchor>a34</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__FILE_DECODER_OK</name>
      <anchor>a34a4</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__FILE_DECODER_END_OF_FILE</name>
      <anchor>a34a5</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__FILE_DECODER_ERROR_OPENING_FILE</name>
      <anchor>a34a6</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__FILE_DECODER_MEMORY_ALLOCATION_ERROR</name>
      <anchor>a34a7</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__FILE_DECODER_SEEK_ERROR</name>
      <anchor>a34a8</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__FILE_DECODER_SEEKABLE_STREAM_DECODER_ERROR</name>
      <anchor>a34a9</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__FILE_DECODER_ALREADY_INITIALIZED</name>
      <anchor>a34a10</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__FILE_DECODER_INVALID_CALLBACK</name>
      <anchor>a34a11</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__FILE_DECODER_UNINITIALIZED</name>
      <anchor>a34a12</anchor>
      <arglist></arglist>
    </member>
    <member kind="function">
      <type>FLAC__FileDecoder *</type>
      <name>FLAC__file_decoder_new</name>
      <anchor>a4</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>FLAC__file_decoder_delete</name>
      <anchor>a5</anchor>
      <arglist>(FLAC__FileDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__file_decoder_set_md5_checking</name>
      <anchor>a6</anchor>
      <arglist>(FLAC__FileDecoder *decoder, FLAC__bool value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__file_decoder_set_filename</name>
      <anchor>a7</anchor>
      <arglist>(FLAC__FileDecoder *decoder, const char *value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__file_decoder_set_write_callback</name>
      <anchor>a8</anchor>
      <arglist>(FLAC__FileDecoder *decoder, FLAC__FileDecoderWriteCallback value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__file_decoder_set_metadata_callback</name>
      <anchor>a9</anchor>
      <arglist>(FLAC__FileDecoder *decoder, FLAC__FileDecoderMetadataCallback value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__file_decoder_set_error_callback</name>
      <anchor>a10</anchor>
      <arglist>(FLAC__FileDecoder *decoder, FLAC__FileDecoderErrorCallback value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__file_decoder_set_client_data</name>
      <anchor>a11</anchor>
      <arglist>(FLAC__FileDecoder *decoder, void *value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__file_decoder_set_metadata_respond</name>
      <anchor>a12</anchor>
      <arglist>(FLAC__FileDecoder *decoder, FLAC__MetadataType type)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__file_decoder_set_metadata_respond_application</name>
      <anchor>a13</anchor>
      <arglist>(FLAC__FileDecoder *decoder, const FLAC__byte id[4])</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__file_decoder_set_metadata_respond_all</name>
      <anchor>a14</anchor>
      <arglist>(FLAC__FileDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__file_decoder_set_metadata_ignore</name>
      <anchor>a15</anchor>
      <arglist>(FLAC__FileDecoder *decoder, FLAC__MetadataType type)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__file_decoder_set_metadata_ignore_application</name>
      <anchor>a16</anchor>
      <arglist>(FLAC__FileDecoder *decoder, const FLAC__byte id[4])</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__file_decoder_set_metadata_ignore_all</name>
      <anchor>a17</anchor>
      <arglist>(FLAC__FileDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__FileDecoderState</type>
      <name>FLAC__file_decoder_get_state</name>
      <anchor>a18</anchor>
      <arglist>(const FLAC__FileDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__SeekableStreamDecoderState</type>
      <name>FLAC__file_decoder_get_seekable_stream_decoder_state</name>
      <anchor>a19</anchor>
      <arglist>(const FLAC__FileDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__StreamDecoderState</type>
      <name>FLAC__file_decoder_get_stream_decoder_state</name>
      <anchor>a20</anchor>
      <arglist>(const FLAC__FileDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__file_decoder_get_md5_checking</name>
      <anchor>a21</anchor>
      <arglist>(const FLAC__FileDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>FLAC__file_decoder_get_channels</name>
      <anchor>a22</anchor>
      <arglist>(const FLAC__FileDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__ChannelAssignment</type>
      <name>FLAC__file_decoder_get_channel_assignment</name>
      <anchor>a23</anchor>
      <arglist>(const FLAC__FileDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>FLAC__file_decoder_get_bits_per_sample</name>
      <anchor>a24</anchor>
      <arglist>(const FLAC__FileDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>FLAC__file_decoder_get_sample_rate</name>
      <anchor>a25</anchor>
      <arglist>(const FLAC__FileDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>FLAC__file_decoder_get_blocksize</name>
      <anchor>a26</anchor>
      <arglist>(const FLAC__FileDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__FileDecoderState</type>
      <name>FLAC__file_decoder_init</name>
      <anchor>a27</anchor>
      <arglist>(FLAC__FileDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__file_decoder_finish</name>
      <anchor>a28</anchor>
      <arglist>(FLAC__FileDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__file_decoder_process_single</name>
      <anchor>a29</anchor>
      <arglist>(FLAC__FileDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__file_decoder_process_until_end_of_metadata</name>
      <anchor>a30</anchor>
      <arglist>(FLAC__FileDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__file_decoder_process_until_end_of_file</name>
      <anchor>a31</anchor>
      <arglist>(FLAC__FileDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__file_decoder_process_remaining_frames</name>
      <anchor>a32</anchor>
      <arglist>(FLAC__FileDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__file_decoder_seek_absolute</name>
      <anchor>a33</anchor>
      <arglist>(FLAC__FileDecoder *decoder, FLAC__uint64 sample)</arglist>
    </member>
    <member kind="variable">
      <type>const char *const</type>
      <name>FLAC__FileDecoderStateString</name>
      <anchor>a0</anchor>
      <arglist>[]</arglist>
    </member>
  </compound>
  <compound kind="group">
    <name>flac_file_encoder</name>
    <title>FLAC/file_encoder.h: file encoder interface</title>
    <filename>group__flac__file__encoder.html</filename>
    <class kind="struct">FLAC__FileEncoder</class>
    <member kind="typedef">
      <type>void(*</type>
      <name>FLAC__FileEncoderProgressCallback</name>
      <anchor>a1</anchor>
      <arglist>)(const FLAC__FileEncoder *encoder, FLAC__uint64 bytes_written, FLAC__uint64 samples_written, unsigned frames_written, unsigned total_frames_estimate, void *client_data)</arglist>
    </member>
    <member kind="enumeration">
      <name>FLAC__FileEncoderState</name>
      <anchor>a51</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__FILE_ENCODER_OK</name>
      <anchor>a51a2</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__FILE_ENCODER_NO_FILENAME</name>
      <anchor>a51a3</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__FILE_ENCODER_SEEKABLE_STREAM_ENCODER_ERROR</name>
      <anchor>a51a4</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__FILE_ENCODER_FATAL_ERROR_WHILE_WRITING</name>
      <anchor>a51a5</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__FILE_ENCODER_ERROR_OPENING_FILE</name>
      <anchor>a51a6</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__FILE_ENCODER_MEMORY_ALLOCATION_ERROR</name>
      <anchor>a51a7</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__FILE_ENCODER_ALREADY_INITIALIZED</name>
      <anchor>a51a8</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__FILE_ENCODER_UNINITIALIZED</name>
      <anchor>a51a9</anchor>
      <arglist></arglist>
    </member>
    <member kind="function">
      <type>FLAC__FileEncoder *</type>
      <name>FLAC__file_encoder_new</name>
      <anchor>a2</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>FLAC__file_encoder_delete</name>
      <anchor>a3</anchor>
      <arglist>(FLAC__FileEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__file_encoder_set_verify</name>
      <anchor>a4</anchor>
      <arglist>(FLAC__FileEncoder *encoder, FLAC__bool value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__file_encoder_set_streamable_subset</name>
      <anchor>a5</anchor>
      <arglist>(FLAC__FileEncoder *encoder, FLAC__bool value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__file_encoder_set_do_mid_side_stereo</name>
      <anchor>a6</anchor>
      <arglist>(FLAC__FileEncoder *encoder, FLAC__bool value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__file_encoder_set_loose_mid_side_stereo</name>
      <anchor>a7</anchor>
      <arglist>(FLAC__FileEncoder *encoder, FLAC__bool value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__file_encoder_set_channels</name>
      <anchor>a8</anchor>
      <arglist>(FLAC__FileEncoder *encoder, unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__file_encoder_set_bits_per_sample</name>
      <anchor>a9</anchor>
      <arglist>(FLAC__FileEncoder *encoder, unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__file_encoder_set_sample_rate</name>
      <anchor>a10</anchor>
      <arglist>(FLAC__FileEncoder *encoder, unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__file_encoder_set_blocksize</name>
      <anchor>a11</anchor>
      <arglist>(FLAC__FileEncoder *encoder, unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__file_encoder_set_max_lpc_order</name>
      <anchor>a12</anchor>
      <arglist>(FLAC__FileEncoder *encoder, unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__file_encoder_set_qlp_coeff_precision</name>
      <anchor>a13</anchor>
      <arglist>(FLAC__FileEncoder *encoder, unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__file_encoder_set_do_qlp_coeff_prec_search</name>
      <anchor>a14</anchor>
      <arglist>(FLAC__FileEncoder *encoder, FLAC__bool value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__file_encoder_set_do_escape_coding</name>
      <anchor>a15</anchor>
      <arglist>(FLAC__FileEncoder *encoder, FLAC__bool value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__file_encoder_set_do_exhaustive_model_search</name>
      <anchor>a16</anchor>
      <arglist>(FLAC__FileEncoder *encoder, FLAC__bool value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__file_encoder_set_min_residual_partition_order</name>
      <anchor>a17</anchor>
      <arglist>(FLAC__FileEncoder *encoder, unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__file_encoder_set_max_residual_partition_order</name>
      <anchor>a18</anchor>
      <arglist>(FLAC__FileEncoder *encoder, unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__file_encoder_set_rice_parameter_search_dist</name>
      <anchor>a19</anchor>
      <arglist>(FLAC__FileEncoder *encoder, unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__file_encoder_set_total_samples_estimate</name>
      <anchor>a20</anchor>
      <arglist>(FLAC__FileEncoder *encoder, FLAC__uint64 value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__file_encoder_set_metadata</name>
      <anchor>a21</anchor>
      <arglist>(FLAC__FileEncoder *encoder, FLAC__StreamMetadata **metadata, unsigned num_blocks)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__file_encoder_set_filename</name>
      <anchor>a22</anchor>
      <arglist>(FLAC__FileEncoder *encoder, const char *value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__file_encoder_set_progress_callback</name>
      <anchor>a23</anchor>
      <arglist>(FLAC__FileEncoder *encoder, FLAC__FileEncoderProgressCallback value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__file_encoder_set_client_data</name>
      <anchor>a24</anchor>
      <arglist>(FLAC__FileEncoder *encoder, void *value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__FileEncoderState</type>
      <name>FLAC__file_encoder_get_state</name>
      <anchor>a25</anchor>
      <arglist>(const FLAC__FileEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__SeekableStreamEncoderState</type>
      <name>FLAC__file_encoder_get_seekable_stream_encoder_state</name>
      <anchor>a26</anchor>
      <arglist>(const FLAC__FileEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__StreamEncoderState</type>
      <name>FLAC__file_encoder_get_stream_encoder_state</name>
      <anchor>a27</anchor>
      <arglist>(const FLAC__FileEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__StreamDecoderState</type>
      <name>FLAC__file_encoder_get_verify_decoder_state</name>
      <anchor>a28</anchor>
      <arglist>(const FLAC__FileEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>FLAC__file_encoder_get_verify_decoder_error_stats</name>
      <anchor>a29</anchor>
      <arglist>(const FLAC__FileEncoder *encoder, FLAC__uint64 *absolute_sample, unsigned *frame_number, unsigned *channel, unsigned *sample, FLAC__int32 *expected, FLAC__int32 *got)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__file_encoder_get_verify</name>
      <anchor>a30</anchor>
      <arglist>(const FLAC__FileEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__file_encoder_get_streamable_subset</name>
      <anchor>a31</anchor>
      <arglist>(const FLAC__FileEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__file_encoder_get_do_mid_side_stereo</name>
      <anchor>a32</anchor>
      <arglist>(const FLAC__FileEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__file_encoder_get_loose_mid_side_stereo</name>
      <anchor>a33</anchor>
      <arglist>(const FLAC__FileEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>FLAC__file_encoder_get_channels</name>
      <anchor>a34</anchor>
      <arglist>(const FLAC__FileEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>FLAC__file_encoder_get_bits_per_sample</name>
      <anchor>a35</anchor>
      <arglist>(const FLAC__FileEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>FLAC__file_encoder_get_sample_rate</name>
      <anchor>a36</anchor>
      <arglist>(const FLAC__FileEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>FLAC__file_encoder_get_blocksize</name>
      <anchor>a37</anchor>
      <arglist>(const FLAC__FileEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>FLAC__file_encoder_get_max_lpc_order</name>
      <anchor>a38</anchor>
      <arglist>(const FLAC__FileEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>FLAC__file_encoder_get_qlp_coeff_precision</name>
      <anchor>a39</anchor>
      <arglist>(const FLAC__FileEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__file_encoder_get_do_qlp_coeff_prec_search</name>
      <anchor>a40</anchor>
      <arglist>(const FLAC__FileEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__file_encoder_get_do_escape_coding</name>
      <anchor>a41</anchor>
      <arglist>(const FLAC__FileEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__file_encoder_get_do_exhaustive_model_search</name>
      <anchor>a42</anchor>
      <arglist>(const FLAC__FileEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>FLAC__file_encoder_get_min_residual_partition_order</name>
      <anchor>a43</anchor>
      <arglist>(const FLAC__FileEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>FLAC__file_encoder_get_max_residual_partition_order</name>
      <anchor>a44</anchor>
      <arglist>(const FLAC__FileEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>FLAC__file_encoder_get_rice_parameter_search_dist</name>
      <anchor>a45</anchor>
      <arglist>(const FLAC__FileEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__uint64</type>
      <name>FLAC__file_encoder_get_total_samples_estimate</name>
      <anchor>a46</anchor>
      <arglist>(const FLAC__FileEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__FileEncoderState</type>
      <name>FLAC__file_encoder_init</name>
      <anchor>a47</anchor>
      <arglist>(FLAC__FileEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>FLAC__file_encoder_finish</name>
      <anchor>a48</anchor>
      <arglist>(FLAC__FileEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__file_encoder_process</name>
      <anchor>a49</anchor>
      <arglist>(FLAC__FileEncoder *encoder, const FLAC__int32 *const buffer[], unsigned samples)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__file_encoder_process_interleaved</name>
      <anchor>a50</anchor>
      <arglist>(FLAC__FileEncoder *encoder, const FLAC__int32 buffer[], unsigned samples)</arglist>
    </member>
    <member kind="variable">
      <type>const char *const</type>
      <name>FLAC__FileEncoderStateString</name>
      <anchor>a0</anchor>
      <arglist>[]</arglist>
    </member>
  </compound>
  <compound kind="group">
    <name>flac_format</name>
    <title>FLAC/format.h: format components</title>
    <filename>group__flac__format.html</filename>
    <class kind="struct">FLAC__EntropyCodingMethod_PartitionedRiceContents</class>
    <class kind="struct">FLAC__EntropyCodingMethod_PartitionedRice</class>
    <class kind="struct">FLAC__EntropyCodingMethod</class>
    <class kind="struct">FLAC__Subframe_Constant</class>
    <class kind="struct">FLAC__Subframe_Verbatim</class>
    <class kind="struct">FLAC__Subframe_Fixed</class>
    <class kind="struct">FLAC__Subframe_LPC</class>
    <class kind="struct">FLAC__Subframe</class>
    <class kind="struct">FLAC__FrameHeader</class>
    <class kind="struct">FLAC__FrameFooter</class>
    <class kind="struct">FLAC__Frame</class>
    <class kind="struct">FLAC__StreamMetadata_StreamInfo</class>
    <class kind="struct">FLAC__StreamMetadata_Padding</class>
    <class kind="struct">FLAC__StreamMetadata_Application</class>
    <class kind="struct">FLAC__StreamMetadata_SeekPoint</class>
    <class kind="struct">FLAC__StreamMetadata_SeekTable</class>
    <class kind="struct">FLAC__StreamMetadata_VorbisComment_Entry</class>
    <class kind="struct">FLAC__StreamMetadata_VorbisComment</class>
    <class kind="struct">FLAC__StreamMetadata</class>
    <member kind="define">
      <type>#define</type>
      <name>FLAC__MIN_BLOCK_SIZE</name>
      <anchor>a56</anchor>
      <arglist></arglist>
    </member>
    <member kind="define">
      <type>#define</type>
      <name>FLAC__MAX_BLOCK_SIZE</name>
      <anchor>a57</anchor>
      <arglist></arglist>
    </member>
    <member kind="define">
      <type>#define</type>
      <name>FLAC__MAX_CHANNELS</name>
      <anchor>a58</anchor>
      <arglist></arglist>
    </member>
    <member kind="define">
      <type>#define</type>
      <name>FLAC__MIN_BITS_PER_SAMPLE</name>
      <anchor>a59</anchor>
      <arglist></arglist>
    </member>
    <member kind="define">
      <type>#define</type>
      <name>FLAC__MAX_BITS_PER_SAMPLE</name>
      <anchor>a60</anchor>
      <arglist></arglist>
    </member>
    <member kind="define">
      <type>#define</type>
      <name>FLAC__REFERENCE_CODEC_MAX_BITS_PER_SAMPLE</name>
      <anchor>a61</anchor>
      <arglist></arglist>
    </member>
    <member kind="define">
      <type>#define</type>
      <name>FLAC__MAX_SAMPLE_RATE</name>
      <anchor>a62</anchor>
      <arglist></arglist>
    </member>
    <member kind="define">
      <type>#define</type>
      <name>FLAC__MAX_LPC_ORDER</name>
      <anchor>a63</anchor>
      <arglist></arglist>
    </member>
    <member kind="define">
      <type>#define</type>
      <name>FLAC__MIN_QLP_COEFF_PRECISION</name>
      <anchor>a64</anchor>
      <arglist></arglist>
    </member>
    <member kind="define">
      <type>#define</type>
      <name>FLAC__MAX_FIXED_ORDER</name>
      <anchor>a65</anchor>
      <arglist></arglist>
    </member>
    <member kind="define">
      <type>#define</type>
      <name>FLAC__MAX_RICE_PARTITION_ORDER</name>
      <anchor>a66</anchor>
      <arglist></arglist>
    </member>
    <member kind="define">
      <type>#define</type>
      <name>FLAC__STREAM_SYNC_LENGTH</name>
      <anchor>a67</anchor>
      <arglist></arglist>
    </member>
    <member kind="define">
      <type>#define</type>
      <name>FLAC__STREAM_METADATA_STREAMINFO_LENGTH</name>
      <anchor>a68</anchor>
      <arglist></arglist>
    </member>
    <member kind="define">
      <type>#define</type>
      <name>FLAC__STREAM_METADATA_SEEKPOINT_LENGTH</name>
      <anchor>a69</anchor>
      <arglist></arglist>
    </member>
    <member kind="define">
      <type>#define</type>
      <name>FLAC__STREAM_METADATA_HEADER_LENGTH</name>
      <anchor>a70</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumeration">
      <name>FLAC__EntropyCodingMethodType</name>
      <anchor>a71</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__ENTROPY_CODING_METHOD_PARTITIONED_RICE</name>
      <anchor>a71a68</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumeration">
      <name>FLAC__SubframeType</name>
      <anchor>a72</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__SUBFRAME_TYPE_CONSTANT</name>
      <anchor>a72a69</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__SUBFRAME_TYPE_VERBATIM</name>
      <anchor>a72a70</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__SUBFRAME_TYPE_FIXED</name>
      <anchor>a72a71</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__SUBFRAME_TYPE_LPC</name>
      <anchor>a72a72</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumeration">
      <name>FLAC__ChannelAssignment</name>
      <anchor>a73</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__CHANNEL_ASSIGNMENT_INDEPENDENT</name>
      <anchor>a73a73</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__CHANNEL_ASSIGNMENT_LEFT_SIDE</name>
      <anchor>a73a74</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__CHANNEL_ASSIGNMENT_RIGHT_SIDE</name>
      <anchor>a73a75</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__CHANNEL_ASSIGNMENT_MID_SIDE</name>
      <anchor>a73a76</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumeration">
      <name>FLAC__FrameNumberType</name>
      <anchor>a74</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__FRAME_NUMBER_TYPE_FRAME_NUMBER</name>
      <anchor>a74a77</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__FRAME_NUMBER_TYPE_SAMPLE_NUMBER</name>
      <anchor>a74a78</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumeration">
      <name>FLAC__MetadataType</name>
      <anchor>a75</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__METADATA_TYPE_STREAMINFO</name>
      <anchor>a75a79</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__METADATA_TYPE_PADDING</name>
      <anchor>a75a80</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__METADATA_TYPE_APPLICATION</name>
      <anchor>a75a81</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__METADATA_TYPE_SEEKTABLE</name>
      <anchor>a75a82</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__METADATA_TYPE_VORBIS_COMMENT</name>
      <anchor>a75a83</anchor>
      <arglist></arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__format_sample_rate_is_valid</name>
      <anchor>a53</anchor>
      <arglist>(unsigned sample_rate)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__format_seektable_is_legal</name>
      <anchor>a54</anchor>
      <arglist>(const FLAC__StreamMetadata_SeekTable *seek_table)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>FLAC__format_seektable_sort</name>
      <anchor>a55</anchor>
      <arglist>(FLAC__StreamMetadata_SeekTable *seek_table)</arglist>
    </member>
    <member kind="variable">
      <type>const char *</type>
      <name>FLAC__VERSION_STRING</name>
      <anchor>a0</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const char *</type>
      <name>FLAC__VENDOR_STRING</name>
      <anchor>a1</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const FLAC__byte</type>
      <name>FLAC__STREAM_SYNC_STRING</name>
      <anchor>a2</anchor>
      <arglist>[4]</arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_SYNC</name>
      <anchor>a3</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_SYNC_LEN</name>
      <anchor>a4</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const char *const</type>
      <name>FLAC__EntropyCodingMethodTypeString</name>
      <anchor>a5</anchor>
      <arglist>[]</arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__ENTROPY_CODING_METHOD_PARTITIONED_RICE_ORDER_LEN</name>
      <anchor>a6</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__ENTROPY_CODING_METHOD_PARTITIONED_RICE_PARAMETER_LEN</name>
      <anchor>a7</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__ENTROPY_CODING_METHOD_PARTITIONED_RICE_RAW_LEN</name>
      <anchor>a8</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__ENTROPY_CODING_METHOD_PARTITIONED_RICE_ESCAPE_PARAMETER</name>
      <anchor>a9</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__ENTROPY_CODING_METHOD_TYPE_LEN</name>
      <anchor>a10</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const char *const</type>
      <name>FLAC__SubframeTypeString</name>
      <anchor>a11</anchor>
      <arglist>[]</arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__SUBFRAME_LPC_QLP_COEFF_PRECISION_LEN</name>
      <anchor>a12</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__SUBFRAME_LPC_QLP_SHIFT_LEN</name>
      <anchor>a13</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__SUBFRAME_ZERO_PAD_LEN</name>
      <anchor>a14</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__SUBFRAME_TYPE_LEN</name>
      <anchor>a15</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__SUBFRAME_WASTED_BITS_FLAG_LEN</name>
      <anchor>a16</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__SUBFRAME_TYPE_CONSTANT_BYTE_ALIGNED_MASK</name>
      <anchor>a17</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__SUBFRAME_TYPE_VERBATIM_BYTE_ALIGNED_MASK</name>
      <anchor>a18</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__SUBFRAME_TYPE_FIXED_BYTE_ALIGNED_MASK</name>
      <anchor>a19</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__SUBFRAME_TYPE_LPC_BYTE_ALIGNED_MASK</name>
      <anchor>a20</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const char *const</type>
      <name>FLAC__ChannelAssignmentString</name>
      <anchor>a21</anchor>
      <arglist>[]</arglist>
    </member>
    <member kind="variable">
      <type>const char *const</type>
      <name>FLAC__FrameNumberTypeString</name>
      <anchor>a22</anchor>
      <arglist>[]</arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__FRAME_HEADER_SYNC</name>
      <anchor>a23</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__FRAME_HEADER_SYNC_LEN</name>
      <anchor>a24</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__FRAME_HEADER_RESERVED_LEN</name>
      <anchor>a25</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__FRAME_HEADER_BLOCK_SIZE_LEN</name>
      <anchor>a26</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__FRAME_HEADER_SAMPLE_RATE_LEN</name>
      <anchor>a27</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__FRAME_HEADER_CHANNEL_ASSIGNMENT_LEN</name>
      <anchor>a28</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__FRAME_HEADER_BITS_PER_SAMPLE_LEN</name>
      <anchor>a29</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__FRAME_HEADER_ZERO_PAD_LEN</name>
      <anchor>a30</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__FRAME_HEADER_CRC_LEN</name>
      <anchor>a31</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__FRAME_FOOTER_CRC_LEN</name>
      <anchor>a32</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const char *const</type>
      <name>FLAC__MetadataTypeString</name>
      <anchor>a33</anchor>
      <arglist>[]</arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_METADATA_STREAMINFO_MIN_BLOCK_SIZE_LEN</name>
      <anchor>a34</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_METADATA_STREAMINFO_MAX_BLOCK_SIZE_LEN</name>
      <anchor>a35</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_METADATA_STREAMINFO_MIN_FRAME_SIZE_LEN</name>
      <anchor>a36</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_METADATA_STREAMINFO_MAX_FRAME_SIZE_LEN</name>
      <anchor>a37</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_METADATA_STREAMINFO_SAMPLE_RATE_LEN</name>
      <anchor>a38</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_METADATA_STREAMINFO_CHANNELS_LEN</name>
      <anchor>a39</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_METADATA_STREAMINFO_BITS_PER_SAMPLE_LEN</name>
      <anchor>a40</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_METADATA_STREAMINFO_TOTAL_SAMPLES_LEN</name>
      <anchor>a41</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_METADATA_STREAMINFO_MD5SUM_LEN</name>
      <anchor>a42</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_METADATA_APPLICATION_ID_LEN</name>
      <anchor>a43</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_METADATA_SEEKPOINT_SAMPLE_NUMBER_LEN</name>
      <anchor>a44</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_METADATA_SEEKPOINT_STREAM_OFFSET_LEN</name>
      <anchor>a45</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_METADATA_SEEKPOINT_FRAME_SAMPLES_LEN</name>
      <anchor>a46</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const FLAC__uint64</type>
      <name>FLAC__STREAM_METADATA_SEEKPOINT_PLACEHOLDER</name>
      <anchor>a47</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_METADATA_VORBIS_COMMENT_ENTRY_LENGTH_LEN</name>
      <anchor>a48</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_METADATA_VORBIS_COMMENT_NUM_COMMENTS_LEN</name>
      <anchor>a49</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_METADATA_IS_LAST_LEN</name>
      <anchor>a50</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_METADATA_TYPE_LEN</name>
      <anchor>a51</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_METADATA_LENGTH_LEN</name>
      <anchor>a52</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="group">
    <name>flac_metadata</name>
    <title>FLAC/metadata.h: metadata interfaces</title>
    <filename>group__flac__metadata.html</filename>
    <subgroup>flac_metadata_level0</subgroup>
    <subgroup>flac_metadata_level1</subgroup>
    <subgroup>flac_metadata_level2</subgroup>
    <subgroup>flac_metadata_object</subgroup>
  </compound>
  <compound kind="group">
    <name>flac_metadata_level0</name>
    <title>FLAC/metadata.h: metadata level 0 interface</title>
    <filename>group__flac__metadata__level0.html</filename>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_get_streaminfo</name>
      <anchor>a0</anchor>
      <arglist>(const char *filename, FLAC__StreamMetadata *streaminfo)</arglist>
    </member>
  </compound>
  <compound kind="group">
    <name>flac_metadata_level1</name>
    <title>FLAC/metadata.h: metadata level 1 interface</title>
    <filename>group__flac__metadata__level1.html</filename>
    <member kind="typedef">
      <type>FLAC__Metadata_SimpleIterator</type>
      <name>FLAC__Metadata_SimpleIterator</name>
      <anchor>a0</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumeration">
      <name>FLAC__Metadata_SimpleIteratorStatus</name>
      <anchor>a14</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__METADATA_SIMPLE_ITERATOR_STATUS_OK</name>
      <anchor>a14a5</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__METADATA_SIMPLE_ITERATOR_STATUS_ILLEGAL_INPUT</name>
      <anchor>a14a6</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__METADATA_SIMPLE_ITERATOR_STATUS_ERROR_OPENING_FILE</name>
      <anchor>a14a7</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__METADATA_SIMPLE_ITERATOR_STATUS_NOT_A_FLAC_FILE</name>
      <anchor>a14a8</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__METADATA_SIMPLE_ITERATOR_STATUS_NOT_WRITABLE</name>
      <anchor>a14a9</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__METADATA_SIMPLE_ITERATOR_STATUS_BAD_METADATA</name>
      <anchor>a14a10</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__METADATA_SIMPLE_ITERATOR_STATUS_READ_ERROR</name>
      <anchor>a14a11</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__METADATA_SIMPLE_ITERATOR_STATUS_SEEK_ERROR</name>
      <anchor>a14a12</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__METADATA_SIMPLE_ITERATOR_STATUS_WRITE_ERROR</name>
      <anchor>a14a13</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__METADATA_SIMPLE_ITERATOR_STATUS_RENAME_ERROR</name>
      <anchor>a14a14</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__METADATA_SIMPLE_ITERATOR_STATUS_UNLINK_ERROR</name>
      <anchor>a14a15</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__METADATA_SIMPLE_ITERATOR_STATUS_MEMORY_ALLOCATION_ERROR</name>
      <anchor>a14a16</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__METADATA_SIMPLE_ITERATOR_STATUS_INTERNAL_ERROR</name>
      <anchor>a14a17</anchor>
      <arglist></arglist>
    </member>
    <member kind="function">
      <type>FLAC__Metadata_SimpleIterator *</type>
      <name>FLAC__metadata_simple_iterator_new</name>
      <anchor>a2</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>FLAC__metadata_simple_iterator_delete</name>
      <anchor>a3</anchor>
      <arglist>(FLAC__Metadata_SimpleIterator *iterator)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__Metadata_SimpleIteratorStatus</type>
      <name>FLAC__metadata_simple_iterator_status</name>
      <anchor>a4</anchor>
      <arglist>(FLAC__Metadata_SimpleIterator *iterator)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_simple_iterator_init</name>
      <anchor>a5</anchor>
      <arglist>(FLAC__Metadata_SimpleIterator *iterator, const char *filename, FLAC__bool read_only, FLAC__bool preserve_file_stats)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_simple_iterator_is_writable</name>
      <anchor>a6</anchor>
      <arglist>(const FLAC__Metadata_SimpleIterator *iterator)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_simple_iterator_next</name>
      <anchor>a7</anchor>
      <arglist>(FLAC__Metadata_SimpleIterator *iterator)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_simple_iterator_prev</name>
      <anchor>a8</anchor>
      <arglist>(FLAC__Metadata_SimpleIterator *iterator)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__MetadataType</type>
      <name>FLAC__metadata_simple_iterator_get_block_type</name>
      <anchor>a9</anchor>
      <arglist>(const FLAC__Metadata_SimpleIterator *iterator)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__StreamMetadata *</type>
      <name>FLAC__metadata_simple_iterator_get_block</name>
      <anchor>a10</anchor>
      <arglist>(FLAC__Metadata_SimpleIterator *iterator)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_simple_iterator_set_block</name>
      <anchor>a11</anchor>
      <arglist>(FLAC__Metadata_SimpleIterator *iterator, FLAC__StreamMetadata *block, FLAC__bool use_padding)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_simple_iterator_insert_block_after</name>
      <anchor>a12</anchor>
      <arglist>(FLAC__Metadata_SimpleIterator *iterator, FLAC__StreamMetadata *block, FLAC__bool use_padding)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_simple_iterator_delete_block</name>
      <anchor>a13</anchor>
      <arglist>(FLAC__Metadata_SimpleIterator *iterator, FLAC__bool use_padding)</arglist>
    </member>
    <member kind="variable">
      <type>const char *const</type>
      <name>FLAC__Metadata_SimpleIteratorStatusString</name>
      <anchor>a1</anchor>
      <arglist>[]</arglist>
    </member>
  </compound>
  <compound kind="group">
    <name>flac_metadata_level2</name>
    <title>FLAC/metadata.h: metadata level 2 interface</title>
    <filename>group__flac__metadata__level2.html</filename>
    <member kind="typedef">
      <type>FLAC__Metadata_Chain</type>
      <name>FLAC__Metadata_Chain</name>
      <anchor>a0</anchor>
      <arglist></arglist>
    </member>
    <member kind="typedef">
      <type>FLAC__Metadata_Iterator</type>
      <name>FLAC__Metadata_Iterator</name>
      <anchor>a1</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumeration">
      <name>FLAC__Metadata_ChainStatus</name>
      <anchor>a21</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__METADATA_CHAIN_STATUS_OK</name>
      <anchor>a21a18</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__METADATA_CHAIN_STATUS_ILLEGAL_INPUT</name>
      <anchor>a21a19</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__METADATA_CHAIN_STATUS_ERROR_OPENING_FILE</name>
      <anchor>a21a20</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__METADATA_CHAIN_STATUS_NOT_A_FLAC_FILE</name>
      <anchor>a21a21</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__METADATA_CHAIN_STATUS_NOT_WRITABLE</name>
      <anchor>a21a22</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__METADATA_CHAIN_STATUS_BAD_METADATA</name>
      <anchor>a21a23</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__METADATA_CHAIN_STATUS_READ_ERROR</name>
      <anchor>a21a24</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__METADATA_CHAIN_STATUS_SEEK_ERROR</name>
      <anchor>a21a25</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__METADATA_CHAIN_STATUS_WRITE_ERROR</name>
      <anchor>a21a26</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__METADATA_CHAIN_STATUS_RENAME_ERROR</name>
      <anchor>a21a27</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__METADATA_CHAIN_STATUS_UNLINK_ERROR</name>
      <anchor>a21a28</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__METADATA_CHAIN_STATUS_MEMORY_ALLOCATION_ERROR</name>
      <anchor>a21a29</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__METADATA_CHAIN_STATUS_INTERNAL_ERROR</name>
      <anchor>a21a30</anchor>
      <arglist></arglist>
    </member>
    <member kind="function">
      <type>FLAC__Metadata_Chain *</type>
      <name>FLAC__metadata_chain_new</name>
      <anchor>a3</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>FLAC__metadata_chain_delete</name>
      <anchor>a4</anchor>
      <arglist>(FLAC__Metadata_Chain *chain)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__Metadata_ChainStatus</type>
      <name>FLAC__metadata_chain_status</name>
      <anchor>a5</anchor>
      <arglist>(FLAC__Metadata_Chain *chain)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_chain_read</name>
      <anchor>a6</anchor>
      <arglist>(FLAC__Metadata_Chain *chain, const char *filename)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_chain_write</name>
      <anchor>a7</anchor>
      <arglist>(FLAC__Metadata_Chain *chain, FLAC__bool use_padding, FLAC__bool preserve_file_stats)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>FLAC__metadata_chain_merge_padding</name>
      <anchor>a8</anchor>
      <arglist>(FLAC__Metadata_Chain *chain)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>FLAC__metadata_chain_sort_padding</name>
      <anchor>a9</anchor>
      <arglist>(FLAC__Metadata_Chain *chain)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__Metadata_Iterator *</type>
      <name>FLAC__metadata_iterator_new</name>
      <anchor>a10</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>FLAC__metadata_iterator_delete</name>
      <anchor>a11</anchor>
      <arglist>(FLAC__Metadata_Iterator *iterator)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>FLAC__metadata_iterator_init</name>
      <anchor>a12</anchor>
      <arglist>(FLAC__Metadata_Iterator *iterator, FLAC__Metadata_Chain *chain)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_iterator_next</name>
      <anchor>a13</anchor>
      <arglist>(FLAC__Metadata_Iterator *iterator)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_iterator_prev</name>
      <anchor>a14</anchor>
      <arglist>(FLAC__Metadata_Iterator *iterator)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__MetadataType</type>
      <name>FLAC__metadata_iterator_get_block_type</name>
      <anchor>a15</anchor>
      <arglist>(const FLAC__Metadata_Iterator *iterator)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__StreamMetadata *</type>
      <name>FLAC__metadata_iterator_get_block</name>
      <anchor>a16</anchor>
      <arglist>(FLAC__Metadata_Iterator *iterator)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_iterator_set_block</name>
      <anchor>a17</anchor>
      <arglist>(FLAC__Metadata_Iterator *iterator, FLAC__StreamMetadata *block)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_iterator_delete_block</name>
      <anchor>a18</anchor>
      <arglist>(FLAC__Metadata_Iterator *iterator, FLAC__bool replace_with_padding)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_iterator_insert_block_before</name>
      <anchor>a19</anchor>
      <arglist>(FLAC__Metadata_Iterator *iterator, FLAC__StreamMetadata *block)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_iterator_insert_block_after</name>
      <anchor>a20</anchor>
      <arglist>(FLAC__Metadata_Iterator *iterator, FLAC__StreamMetadata *block)</arglist>
    </member>
    <member kind="variable">
      <type>const char *const</type>
      <name>FLAC__Metadata_ChainStatusString</name>
      <anchor>a2</anchor>
      <arglist>[]</arglist>
    </member>
  </compound>
  <compound kind="group">
    <name>flac_metadata_object</name>
    <title>FLAC/metadata.h: metadata object methods</title>
    <filename>group__flac__metadata__object.html</filename>
    <member kind="function">
      <type>FLAC__StreamMetadata *</type>
      <name>FLAC__metadata_object_new</name>
      <anchor>a0</anchor>
      <arglist>(FLAC__MetadataType type)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__StreamMetadata *</type>
      <name>FLAC__metadata_object_clone</name>
      <anchor>a1</anchor>
      <arglist>(const FLAC__StreamMetadata *object)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>FLAC__metadata_object_delete</name>
      <anchor>a2</anchor>
      <arglist>(FLAC__StreamMetadata *object)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_object_is_equal</name>
      <anchor>a3</anchor>
      <arglist>(const FLAC__StreamMetadata *block1, const FLAC__StreamMetadata *block2)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_object_application_set_data</name>
      <anchor>a4</anchor>
      <arglist>(FLAC__StreamMetadata *object, FLAC__byte *data, unsigned length, FLAC__bool copy)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_object_seektable_resize_points</name>
      <anchor>a5</anchor>
      <arglist>(FLAC__StreamMetadata *object, unsigned new_num_points)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>FLAC__metadata_object_seektable_set_point</name>
      <anchor>a6</anchor>
      <arglist>(FLAC__StreamMetadata *object, unsigned point_num, FLAC__StreamMetadata_SeekPoint point)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_object_seektable_insert_point</name>
      <anchor>a7</anchor>
      <arglist>(FLAC__StreamMetadata *object, unsigned point_num, FLAC__StreamMetadata_SeekPoint point)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_object_seektable_delete_point</name>
      <anchor>a8</anchor>
      <arglist>(FLAC__StreamMetadata *object, unsigned point_num)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_object_seektable_is_legal</name>
      <anchor>a9</anchor>
      <arglist>(const FLAC__StreamMetadata *object)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_object_seektable_template_append_placeholders</name>
      <anchor>a10</anchor>
      <arglist>(FLAC__StreamMetadata *object, unsigned num)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_object_seektable_template_append_point</name>
      <anchor>a11</anchor>
      <arglist>(FLAC__StreamMetadata *object, FLAC__uint64 sample_number)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_object_seektable_template_append_points</name>
      <anchor>a12</anchor>
      <arglist>(FLAC__StreamMetadata *object, FLAC__uint64 sample_numbers[], unsigned num)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_object_seektable_template_append_spaced_points</name>
      <anchor>a13</anchor>
      <arglist>(FLAC__StreamMetadata *object, unsigned num, FLAC__uint64 total_samples)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_object_seektable_template_sort</name>
      <anchor>a14</anchor>
      <arglist>(FLAC__StreamMetadata *object, FLAC__bool compact)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_object_vorbiscomment_set_vendor_string</name>
      <anchor>a15</anchor>
      <arglist>(FLAC__StreamMetadata *object, FLAC__StreamMetadata_VorbisComment_Entry entry, FLAC__bool copy)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_object_vorbiscomment_resize_comments</name>
      <anchor>a16</anchor>
      <arglist>(FLAC__StreamMetadata *object, unsigned new_num_comments)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_object_vorbiscomment_set_comment</name>
      <anchor>a17</anchor>
      <arglist>(FLAC__StreamMetadata *object, unsigned comment_num, FLAC__StreamMetadata_VorbisComment_Entry entry, FLAC__bool copy)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_object_vorbiscomment_insert_comment</name>
      <anchor>a18</anchor>
      <arglist>(FLAC__StreamMetadata *object, unsigned comment_num, FLAC__StreamMetadata_VorbisComment_Entry entry, FLAC__bool copy)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_object_vorbiscomment_delete_comment</name>
      <anchor>a19</anchor>
      <arglist>(FLAC__StreamMetadata *object, unsigned comment_num)</arglist>
    </member>
  </compound>
  <compound kind="group">
    <name>flac_seekable_stream_decoder</name>
    <title>FLAC/seekable_stream_decoder.h: seekable stream decoder interface</title>
    <filename>group__flac__seekable__stream__decoder.html</filename>
    <class kind="struct">FLAC__SeekableStreamDecoder</class>
    <member kind="typedef">
      <type>FLAC__SeekableStreamDecoderReadStatus(*</type>
      <name>FLAC__SeekableStreamDecoderReadCallback</name>
      <anchor>a5</anchor>
      <arglist>)(const FLAC__SeekableStreamDecoder *decoder, FLAC__byte buffer[], unsigned *bytes, void *client_data)</arglist>
    </member>
    <member kind="typedef">
      <type>FLAC__SeekableStreamDecoderSeekStatus(*</type>
      <name>FLAC__SeekableStreamDecoderSeekCallback</name>
      <anchor>a6</anchor>
      <arglist>)(const FLAC__SeekableStreamDecoder *decoder, FLAC__uint64 absolute_byte_offset, void *client_data)</arglist>
    </member>
    <member kind="typedef">
      <type>FLAC__SeekableStreamDecoderTellStatus(*</type>
      <name>FLAC__SeekableStreamDecoderTellCallback</name>
      <anchor>a7</anchor>
      <arglist>)(const FLAC__SeekableStreamDecoder *decoder, FLAC__uint64 *absolute_byte_offset, void *client_data)</arglist>
    </member>
    <member kind="typedef">
      <type>FLAC__SeekableStreamDecoderLengthStatus(*</type>
      <name>FLAC__SeekableStreamDecoderLengthCallback</name>
      <anchor>a8</anchor>
      <arglist>)(const FLAC__SeekableStreamDecoder *decoder, FLAC__uint64 *stream_length, void *client_data)</arglist>
    </member>
    <member kind="typedef">
      <type>FLAC__bool(*</type>
      <name>FLAC__SeekableStreamDecoderEofCallback</name>
      <anchor>a9</anchor>
      <arglist>)(const FLAC__SeekableStreamDecoder *decoder, void *client_data)</arglist>
    </member>
    <member kind="typedef">
      <type>FLAC__StreamDecoderWriteStatus(*</type>
      <name>FLAC__SeekableStreamDecoderWriteCallback</name>
      <anchor>a10</anchor>
      <arglist>)(const FLAC__SeekableStreamDecoder *decoder, const FLAC__Frame *frame, const FLAC__int32 *const buffer[], void *client_data)</arglist>
    </member>
    <member kind="typedef">
      <type>void(*</type>
      <name>FLAC__SeekableStreamDecoderMetadataCallback</name>
      <anchor>a11</anchor>
      <arglist>)(const FLAC__SeekableStreamDecoder *decoder, const FLAC__StreamMetadata *metadata, void *client_data)</arglist>
    </member>
    <member kind="typedef">
      <type>void(*</type>
      <name>FLAC__SeekableStreamDecoderErrorCallback</name>
      <anchor>a12</anchor>
      <arglist>)(const FLAC__SeekableStreamDecoder *decoder, FLAC__StreamDecoderErrorStatus status, void *client_data)</arglist>
    </member>
    <member kind="enumeration">
      <name>FLAC__SeekableStreamDecoderState</name>
      <anchor>a47</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__SEEKABLE_STREAM_DECODER_OK</name>
      <anchor>a47a13</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__SEEKABLE_STREAM_DECODER_SEEKING</name>
      <anchor>a47a14</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__SEEKABLE_STREAM_DECODER_END_OF_STREAM</name>
      <anchor>a47a15</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__SEEKABLE_STREAM_DECODER_MEMORY_ALLOCATION_ERROR</name>
      <anchor>a47a16</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__SEEKABLE_STREAM_DECODER_STREAM_DECODER_ERROR</name>
      <anchor>a47a17</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__SEEKABLE_STREAM_DECODER_READ_ERROR</name>
      <anchor>a47a18</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__SEEKABLE_STREAM_DECODER_SEEK_ERROR</name>
      <anchor>a47a19</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__SEEKABLE_STREAM_DECODER_ALREADY_INITIALIZED</name>
      <anchor>a47a20</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__SEEKABLE_STREAM_DECODER_INVALID_CALLBACK</name>
      <anchor>a47a21</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__SEEKABLE_STREAM_DECODER_UNINITIALIZED</name>
      <anchor>a47a22</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumeration">
      <name>FLAC__SeekableStreamDecoderReadStatus</name>
      <anchor>a48</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__SEEKABLE_STREAM_DECODER_READ_STATUS_OK</name>
      <anchor>a48a23</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__SEEKABLE_STREAM_DECODER_READ_STATUS_ERROR</name>
      <anchor>a48a24</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumeration">
      <name>FLAC__SeekableStreamDecoderSeekStatus</name>
      <anchor>a49</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__SEEKABLE_STREAM_DECODER_SEEK_STATUS_OK</name>
      <anchor>a49a25</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__SEEKABLE_STREAM_DECODER_SEEK_STATUS_ERROR</name>
      <anchor>a49a26</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumeration">
      <name>FLAC__SeekableStreamDecoderTellStatus</name>
      <anchor>a50</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__SEEKABLE_STREAM_DECODER_TELL_STATUS_OK</name>
      <anchor>a50a27</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__SEEKABLE_STREAM_DECODER_TELL_STATUS_ERROR</name>
      <anchor>a50a28</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumeration">
      <name>FLAC__SeekableStreamDecoderLengthStatus</name>
      <anchor>a51</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__SEEKABLE_STREAM_DECODER_LENGTH_STATUS_OK</name>
      <anchor>a51a29</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__SEEKABLE_STREAM_DECODER_LENGTH_STATUS_ERROR</name>
      <anchor>a51a30</anchor>
      <arglist></arglist>
    </member>
    <member kind="function">
      <type>FLAC__SeekableStreamDecoder *</type>
      <name>FLAC__seekable_stream_decoder_new</name>
      <anchor>a13</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>FLAC__seekable_stream_decoder_delete</name>
      <anchor>a14</anchor>
      <arglist>(FLAC__SeekableStreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__seekable_stream_decoder_set_md5_checking</name>
      <anchor>a15</anchor>
      <arglist>(FLAC__SeekableStreamDecoder *decoder, FLAC__bool value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__seekable_stream_decoder_set_read_callback</name>
      <anchor>a16</anchor>
      <arglist>(FLAC__SeekableStreamDecoder *decoder, FLAC__SeekableStreamDecoderReadCallback value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__seekable_stream_decoder_set_seek_callback</name>
      <anchor>a17</anchor>
      <arglist>(FLAC__SeekableStreamDecoder *decoder, FLAC__SeekableStreamDecoderSeekCallback value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__seekable_stream_decoder_set_tell_callback</name>
      <anchor>a18</anchor>
      <arglist>(FLAC__SeekableStreamDecoder *decoder, FLAC__SeekableStreamDecoderTellCallback value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__seekable_stream_decoder_set_length_callback</name>
      <anchor>a19</anchor>
      <arglist>(FLAC__SeekableStreamDecoder *decoder, FLAC__SeekableStreamDecoderLengthCallback value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__seekable_stream_decoder_set_eof_callback</name>
      <anchor>a20</anchor>
      <arglist>(FLAC__SeekableStreamDecoder *decoder, FLAC__SeekableStreamDecoderEofCallback value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__seekable_stream_decoder_set_write_callback</name>
      <anchor>a21</anchor>
      <arglist>(FLAC__SeekableStreamDecoder *decoder, FLAC__SeekableStreamDecoderWriteCallback value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__seekable_stream_decoder_set_metadata_callback</name>
      <anchor>a22</anchor>
      <arglist>(FLAC__SeekableStreamDecoder *decoder, FLAC__SeekableStreamDecoderMetadataCallback value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__seekable_stream_decoder_set_error_callback</name>
      <anchor>a23</anchor>
      <arglist>(FLAC__SeekableStreamDecoder *decoder, FLAC__SeekableStreamDecoderErrorCallback value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__seekable_stream_decoder_set_client_data</name>
      <anchor>a24</anchor>
      <arglist>(FLAC__SeekableStreamDecoder *decoder, void *value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__seekable_stream_decoder_set_metadata_respond</name>
      <anchor>a25</anchor>
      <arglist>(FLAC__SeekableStreamDecoder *decoder, FLAC__MetadataType type)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__seekable_stream_decoder_set_metadata_respond_application</name>
      <anchor>a26</anchor>
      <arglist>(FLAC__SeekableStreamDecoder *decoder, const FLAC__byte id[4])</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__seekable_stream_decoder_set_metadata_respond_all</name>
      <anchor>a27</anchor>
      <arglist>(FLAC__SeekableStreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__seekable_stream_decoder_set_metadata_ignore</name>
      <anchor>a28</anchor>
      <arglist>(FLAC__SeekableStreamDecoder *decoder, FLAC__MetadataType type)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__seekable_stream_decoder_set_metadata_ignore_application</name>
      <anchor>a29</anchor>
      <arglist>(FLAC__SeekableStreamDecoder *decoder, const FLAC__byte id[4])</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__seekable_stream_decoder_set_metadata_ignore_all</name>
      <anchor>a30</anchor>
      <arglist>(FLAC__SeekableStreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__SeekableStreamDecoderState</type>
      <name>FLAC__seekable_stream_decoder_get_state</name>
      <anchor>a31</anchor>
      <arglist>(const FLAC__SeekableStreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__StreamDecoderState</type>
      <name>FLAC__seekable_stream_decoder_get_stream_decoder_state</name>
      <anchor>a32</anchor>
      <arglist>(const FLAC__SeekableStreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__seekable_stream_decoder_get_md5_checking</name>
      <anchor>a33</anchor>
      <arglist>(const FLAC__SeekableStreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>FLAC__seekable_stream_decoder_get_channels</name>
      <anchor>a34</anchor>
      <arglist>(const FLAC__SeekableStreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__ChannelAssignment</type>
      <name>FLAC__seekable_stream_decoder_get_channel_assignment</name>
      <anchor>a35</anchor>
      <arglist>(const FLAC__SeekableStreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>FLAC__seekable_stream_decoder_get_bits_per_sample</name>
      <anchor>a36</anchor>
      <arglist>(const FLAC__SeekableStreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>FLAC__seekable_stream_decoder_get_sample_rate</name>
      <anchor>a37</anchor>
      <arglist>(const FLAC__SeekableStreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>FLAC__seekable_stream_decoder_get_blocksize</name>
      <anchor>a38</anchor>
      <arglist>(const FLAC__SeekableStreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__SeekableStreamDecoderState</type>
      <name>FLAC__seekable_stream_decoder_init</name>
      <anchor>a39</anchor>
      <arglist>(FLAC__SeekableStreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__seekable_stream_decoder_finish</name>
      <anchor>a40</anchor>
      <arglist>(FLAC__SeekableStreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__seekable_stream_decoder_flush</name>
      <anchor>a41</anchor>
      <arglist>(FLAC__SeekableStreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__seekable_stream_decoder_reset</name>
      <anchor>a42</anchor>
      <arglist>(FLAC__SeekableStreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__seekable_stream_decoder_process_single</name>
      <anchor>a43</anchor>
      <arglist>(FLAC__SeekableStreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__seekable_stream_decoder_process_until_end_of_metadata</name>
      <anchor>a44</anchor>
      <arglist>(FLAC__SeekableStreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__seekable_stream_decoder_process_until_end_of_stream</name>
      <anchor>a45</anchor>
      <arglist>(FLAC__SeekableStreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__seekable_stream_decoder_seek_absolute</name>
      <anchor>a46</anchor>
      <arglist>(FLAC__SeekableStreamDecoder *decoder, FLAC__uint64 sample)</arglist>
    </member>
    <member kind="variable">
      <type>const char *const</type>
      <name>FLAC__SeekableStreamDecoderStateString</name>
      <anchor>a0</anchor>
      <arglist>[]</arglist>
    </member>
    <member kind="variable">
      <type>const char *const</type>
      <name>FLAC__SeekableStreamDecoderReadStatusString</name>
      <anchor>a1</anchor>
      <arglist>[]</arglist>
    </member>
    <member kind="variable">
      <type>const char *const</type>
      <name>FLAC__SeekableStreamDecoderSeekStatusString</name>
      <anchor>a2</anchor>
      <arglist>[]</arglist>
    </member>
    <member kind="variable">
      <type>const char *const</type>
      <name>FLAC__SeekableStreamDecoderTellStatusString</name>
      <anchor>a3</anchor>
      <arglist>[]</arglist>
    </member>
    <member kind="variable">
      <type>const char *const</type>
      <name>FLAC__SeekableStreamDecoderLengthStatusString</name>
      <anchor>a4</anchor>
      <arglist>[]</arglist>
    </member>
  </compound>
  <compound kind="group">
    <name>flac_seekable_stream_encoder</name>
    <title>FLAC/seekable_stream_encoder.h: seekable stream encoder interface</title>
    <filename>group__flac__seekable__stream__encoder.html</filename>
    <class kind="struct">FLAC__SeekableStreamEncoder</class>
    <member kind="typedef">
      <type>FLAC__SeekableStreamEncoderSeekStatus(*</type>
      <name>FLAC__SeekableStreamEncoderSeekCallback</name>
      <anchor>a2</anchor>
      <arglist>)(const FLAC__SeekableStreamEncoder *encoder, FLAC__uint64 absolute_byte_offset, void *client_data)</arglist>
    </member>
    <member kind="typedef">
      <type>FLAC__StreamEncoderWriteStatus(*</type>
      <name>FLAC__SeekableStreamEncoderWriteCallback</name>
      <anchor>a3</anchor>
      <arglist>)(const FLAC__SeekableStreamEncoder *encoder, const FLAC__byte buffer[], unsigned bytes, unsigned samples, unsigned current_frame, void *client_data)</arglist>
    </member>
    <member kind="enumeration">
      <name>FLAC__SeekableStreamEncoderState</name>
      <anchor>a52</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__SEEKABLE_STREAM_ENCODER_OK</name>
      <anchor>a52a4</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__SEEKABLE_STREAM_ENCODER_STREAM_ENCODER_ERROR</name>
      <anchor>a52a5</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__SEEKABLE_STREAM_ENCODER_MEMORY_ALLOCATION_ERROR</name>
      <anchor>a52a6</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__SEEKABLE_STREAM_ENCODER_WRITE_ERROR</name>
      <anchor>a52a7</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__SEEKABLE_STREAM_ENCODER_READ_ERROR</name>
      <anchor>a52a8</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__SEEKABLE_STREAM_ENCODER_SEEK_ERROR</name>
      <anchor>a52a9</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__SEEKABLE_STREAM_ENCODER_ALREADY_INITIALIZED</name>
      <anchor>a52a10</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__SEEKABLE_STREAM_ENCODER_INVALID_CALLBACK</name>
      <anchor>a52a11</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__SEEKABLE_STREAM_ENCODER_INVALID_SEEKTABLE</name>
      <anchor>a52a12</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__SEEKABLE_STREAM_ENCODER_UNINITIALIZED</name>
      <anchor>a52a13</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumeration">
      <name>FLAC__SeekableStreamEncoderSeekStatus</name>
      <anchor>a53</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__SEEKABLE_STREAM_ENCODER_SEEK_STATUS_OK</name>
      <anchor>a53a14</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__SEEKABLE_STREAM_ENCODER_SEEK_STATUS_ERROR</name>
      <anchor>a53a15</anchor>
      <arglist></arglist>
    </member>
    <member kind="function">
      <type>FLAC__SeekableStreamEncoder *</type>
      <name>FLAC__seekable_stream_encoder_new</name>
      <anchor>a4</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>FLAC__seekable_stream_encoder_delete</name>
      <anchor>a5</anchor>
      <arglist>(FLAC__SeekableStreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__seekable_stream_encoder_set_verify</name>
      <anchor>a6</anchor>
      <arglist>(FLAC__SeekableStreamEncoder *encoder, FLAC__bool value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__seekable_stream_encoder_set_streamable_subset</name>
      <anchor>a7</anchor>
      <arglist>(FLAC__SeekableStreamEncoder *encoder, FLAC__bool value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__seekable_stream_encoder_set_do_mid_side_stereo</name>
      <anchor>a8</anchor>
      <arglist>(FLAC__SeekableStreamEncoder *encoder, FLAC__bool value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__seekable_stream_encoder_set_loose_mid_side_stereo</name>
      <anchor>a9</anchor>
      <arglist>(FLAC__SeekableStreamEncoder *encoder, FLAC__bool value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__seekable_stream_encoder_set_channels</name>
      <anchor>a10</anchor>
      <arglist>(FLAC__SeekableStreamEncoder *encoder, unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__seekable_stream_encoder_set_bits_per_sample</name>
      <anchor>a11</anchor>
      <arglist>(FLAC__SeekableStreamEncoder *encoder, unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__seekable_stream_encoder_set_sample_rate</name>
      <anchor>a12</anchor>
      <arglist>(FLAC__SeekableStreamEncoder *encoder, unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__seekable_stream_encoder_set_blocksize</name>
      <anchor>a13</anchor>
      <arglist>(FLAC__SeekableStreamEncoder *encoder, unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__seekable_stream_encoder_set_max_lpc_order</name>
      <anchor>a14</anchor>
      <arglist>(FLAC__SeekableStreamEncoder *encoder, unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__seekable_stream_encoder_set_qlp_coeff_precision</name>
      <anchor>a15</anchor>
      <arglist>(FLAC__SeekableStreamEncoder *encoder, unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__seekable_stream_encoder_set_do_qlp_coeff_prec_search</name>
      <anchor>a16</anchor>
      <arglist>(FLAC__SeekableStreamEncoder *encoder, FLAC__bool value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__seekable_stream_encoder_set_do_escape_coding</name>
      <anchor>a17</anchor>
      <arglist>(FLAC__SeekableStreamEncoder *encoder, FLAC__bool value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__seekable_stream_encoder_set_do_exhaustive_model_search</name>
      <anchor>a18</anchor>
      <arglist>(FLAC__SeekableStreamEncoder *encoder, FLAC__bool value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__seekable_stream_encoder_set_min_residual_partition_order</name>
      <anchor>a19</anchor>
      <arglist>(FLAC__SeekableStreamEncoder *encoder, unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__seekable_stream_encoder_set_max_residual_partition_order</name>
      <anchor>a20</anchor>
      <arglist>(FLAC__SeekableStreamEncoder *encoder, unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__seekable_stream_encoder_set_rice_parameter_search_dist</name>
      <anchor>a21</anchor>
      <arglist>(FLAC__SeekableStreamEncoder *encoder, unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__seekable_stream_encoder_set_total_samples_estimate</name>
      <anchor>a22</anchor>
      <arglist>(FLAC__SeekableStreamEncoder *encoder, FLAC__uint64 value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__seekable_stream_encoder_set_metadata</name>
      <anchor>a23</anchor>
      <arglist>(FLAC__SeekableStreamEncoder *encoder, FLAC__StreamMetadata **metadata, unsigned num_blocks)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__seekable_stream_encoder_set_seek_callback</name>
      <anchor>a24</anchor>
      <arglist>(FLAC__SeekableStreamEncoder *encoder, FLAC__SeekableStreamEncoderSeekCallback value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__seekable_stream_encoder_set_write_callback</name>
      <anchor>a25</anchor>
      <arglist>(FLAC__SeekableStreamEncoder *encoder, FLAC__SeekableStreamEncoderWriteCallback value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__seekable_stream_encoder_set_client_data</name>
      <anchor>a26</anchor>
      <arglist>(FLAC__SeekableStreamEncoder *encoder, void *value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__SeekableStreamEncoderState</type>
      <name>FLAC__seekable_stream_encoder_get_state</name>
      <anchor>a27</anchor>
      <arglist>(const FLAC__SeekableStreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__StreamEncoderState</type>
      <name>FLAC__seekable_stream_encoder_get_stream_encoder_state</name>
      <anchor>a28</anchor>
      <arglist>(const FLAC__SeekableStreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__StreamDecoderState</type>
      <name>FLAC__seekable_stream_encoder_get_verify_decoder_state</name>
      <anchor>a29</anchor>
      <arglist>(const FLAC__SeekableStreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>FLAC__seekable_stream_encoder_get_verify_decoder_error_stats</name>
      <anchor>a30</anchor>
      <arglist>(const FLAC__SeekableStreamEncoder *encoder, FLAC__uint64 *absolute_sample, unsigned *frame_number, unsigned *channel, unsigned *sample, FLAC__int32 *expected, FLAC__int32 *got)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__seekable_stream_encoder_get_verify</name>
      <anchor>a31</anchor>
      <arglist>(const FLAC__SeekableStreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__seekable_stream_encoder_get_streamable_subset</name>
      <anchor>a32</anchor>
      <arglist>(const FLAC__SeekableStreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__seekable_stream_encoder_get_do_mid_side_stereo</name>
      <anchor>a33</anchor>
      <arglist>(const FLAC__SeekableStreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__seekable_stream_encoder_get_loose_mid_side_stereo</name>
      <anchor>a34</anchor>
      <arglist>(const FLAC__SeekableStreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>FLAC__seekable_stream_encoder_get_channels</name>
      <anchor>a35</anchor>
      <arglist>(const FLAC__SeekableStreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>FLAC__seekable_stream_encoder_get_bits_per_sample</name>
      <anchor>a36</anchor>
      <arglist>(const FLAC__SeekableStreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>FLAC__seekable_stream_encoder_get_sample_rate</name>
      <anchor>a37</anchor>
      <arglist>(const FLAC__SeekableStreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>FLAC__seekable_stream_encoder_get_blocksize</name>
      <anchor>a38</anchor>
      <arglist>(const FLAC__SeekableStreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>FLAC__seekable_stream_encoder_get_max_lpc_order</name>
      <anchor>a39</anchor>
      <arglist>(const FLAC__SeekableStreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>FLAC__seekable_stream_encoder_get_qlp_coeff_precision</name>
      <anchor>a40</anchor>
      <arglist>(const FLAC__SeekableStreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__seekable_stream_encoder_get_do_qlp_coeff_prec_search</name>
      <anchor>a41</anchor>
      <arglist>(const FLAC__SeekableStreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__seekable_stream_encoder_get_do_escape_coding</name>
      <anchor>a42</anchor>
      <arglist>(const FLAC__SeekableStreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__seekable_stream_encoder_get_do_exhaustive_model_search</name>
      <anchor>a43</anchor>
      <arglist>(const FLAC__SeekableStreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>FLAC__seekable_stream_encoder_get_min_residual_partition_order</name>
      <anchor>a44</anchor>
      <arglist>(const FLAC__SeekableStreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>FLAC__seekable_stream_encoder_get_max_residual_partition_order</name>
      <anchor>a45</anchor>
      <arglist>(const FLAC__SeekableStreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>FLAC__seekable_stream_encoder_get_rice_parameter_search_dist</name>
      <anchor>a46</anchor>
      <arglist>(const FLAC__SeekableStreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__uint64</type>
      <name>FLAC__seekable_stream_encoder_get_total_samples_estimate</name>
      <anchor>a47</anchor>
      <arglist>(const FLAC__SeekableStreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__SeekableStreamEncoderState</type>
      <name>FLAC__seekable_stream_encoder_init</name>
      <anchor>a48</anchor>
      <arglist>(FLAC__SeekableStreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>FLAC__seekable_stream_encoder_finish</name>
      <anchor>a49</anchor>
      <arglist>(FLAC__SeekableStreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__seekable_stream_encoder_process</name>
      <anchor>a50</anchor>
      <arglist>(FLAC__SeekableStreamEncoder *encoder, const FLAC__int32 *const buffer[], unsigned samples)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__seekable_stream_encoder_process_interleaved</name>
      <anchor>a51</anchor>
      <arglist>(FLAC__SeekableStreamEncoder *encoder, const FLAC__int32 buffer[], unsigned samples)</arglist>
    </member>
    <member kind="variable">
      <type>const char *const</type>
      <name>FLAC__SeekableStreamEncoderStateString</name>
      <anchor>a0</anchor>
      <arglist>[]</arglist>
    </member>
    <member kind="variable">
      <type>const char *const</type>
      <name>FLAC__SeekableStreamEncoderSeekStatusString</name>
      <anchor>a1</anchor>
      <arglist>[]</arglist>
    </member>
  </compound>
  <compound kind="group">
    <name>flac_decoder</name>
    <title>FLAC/_decoder.h: decoder interfaces</title>
    <filename>group__flac__decoder.html</filename>
    <subgroup>flac_file_decoder</subgroup>
    <subgroup>flac_seekable_stream_decoder</subgroup>
    <subgroup>flac_stream_decoder</subgroup>
  </compound>
  <compound kind="group">
    <name>flac_stream_decoder</name>
    <title>FLAC/stream_decoder.h: stream decoder interface</title>
    <filename>group__flac__stream__decoder.html</filename>
    <class kind="struct">FLAC__StreamDecoder</class>
    <member kind="typedef">
      <type>FLAC__StreamDecoderReadStatus(*</type>
      <name>FLAC__StreamDecoderReadCallback</name>
      <anchor>a4</anchor>
      <arglist>)(const FLAC__StreamDecoder *decoder, FLAC__byte buffer[], unsigned *bytes, void *client_data)</arglist>
    </member>
    <member kind="typedef">
      <type>FLAC__StreamDecoderWriteStatus(*</type>
      <name>FLAC__StreamDecoderWriteCallback</name>
      <anchor>a5</anchor>
      <arglist>)(const FLAC__StreamDecoder *decoder, const FLAC__Frame *frame, const FLAC__int32 *const buffer[], void *client_data)</arglist>
    </member>
    <member kind="typedef">
      <type>void(*</type>
      <name>FLAC__StreamDecoderMetadataCallback</name>
      <anchor>a6</anchor>
      <arglist>)(const FLAC__StreamDecoder *decoder, const FLAC__StreamMetadata *metadata, void *client_data)</arglist>
    </member>
    <member kind="typedef">
      <type>void(*</type>
      <name>FLAC__StreamDecoderErrorCallback</name>
      <anchor>a7</anchor>
      <arglist>)(const FLAC__StreamDecoder *decoder, FLAC__StreamDecoderErrorStatus status, void *client_data)</arglist>
    </member>
    <member kind="enumeration">
      <name>FLAC__StreamDecoderState</name>
      <anchor>a34</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__STREAM_DECODER_SEARCH_FOR_METADATA</name>
      <anchor>a34a8</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__STREAM_DECODER_READ_METADATA</name>
      <anchor>a34a9</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__STREAM_DECODER_SEARCH_FOR_FRAME_SYNC</name>
      <anchor>a34a10</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__STREAM_DECODER_READ_FRAME</name>
      <anchor>a34a11</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__STREAM_DECODER_END_OF_STREAM</name>
      <anchor>a34a12</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__STREAM_DECODER_ABORTED</name>
      <anchor>a34a13</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__STREAM_DECODER_UNPARSEABLE_STREAM</name>
      <anchor>a34a14</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__STREAM_DECODER_MEMORY_ALLOCATION_ERROR</name>
      <anchor>a34a15</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__STREAM_DECODER_ALREADY_INITIALIZED</name>
      <anchor>a34a16</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__STREAM_DECODER_INVALID_CALLBACK</name>
      <anchor>a34a17</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__STREAM_DECODER_UNINITIALIZED</name>
      <anchor>a34a18</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumeration">
      <name>FLAC__StreamDecoderReadStatus</name>
      <anchor>a35</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__STREAM_DECODER_READ_STATUS_CONTINUE</name>
      <anchor>a35a19</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__STREAM_DECODER_READ_STATUS_END_OF_STREAM</name>
      <anchor>a35a20</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__STREAM_DECODER_READ_STATUS_ABORT</name>
      <anchor>a35a21</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumeration">
      <name>FLAC__StreamDecoderWriteStatus</name>
      <anchor>a36</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__STREAM_DECODER_WRITE_STATUS_CONTINUE</name>
      <anchor>a36a22</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__STREAM_DECODER_WRITE_STATUS_ABORT</name>
      <anchor>a36a23</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumeration">
      <name>FLAC__StreamDecoderErrorStatus</name>
      <anchor>a37</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__STREAM_DECODER_ERROR_STATUS_LOST_SYNC</name>
      <anchor>a37a24</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__STREAM_DECODER_ERROR_STATUS_BAD_HEADER</name>
      <anchor>a37a25</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__STREAM_DECODER_ERROR_STATUS_FRAME_CRC_MISMATCH</name>
      <anchor>a37a26</anchor>
      <arglist></arglist>
    </member>
    <member kind="function">
      <type>FLAC__StreamDecoder *</type>
      <name>FLAC__stream_decoder_new</name>
      <anchor>a8</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>FLAC__stream_decoder_delete</name>
      <anchor>a9</anchor>
      <arglist>(FLAC__StreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_decoder_set_read_callback</name>
      <anchor>a10</anchor>
      <arglist>(FLAC__StreamDecoder *decoder, FLAC__StreamDecoderReadCallback value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_decoder_set_write_callback</name>
      <anchor>a11</anchor>
      <arglist>(FLAC__StreamDecoder *decoder, FLAC__StreamDecoderWriteCallback value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_decoder_set_metadata_callback</name>
      <anchor>a12</anchor>
      <arglist>(FLAC__StreamDecoder *decoder, FLAC__StreamDecoderMetadataCallback value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_decoder_set_error_callback</name>
      <anchor>a13</anchor>
      <arglist>(FLAC__StreamDecoder *decoder, FLAC__StreamDecoderErrorCallback value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_decoder_set_client_data</name>
      <anchor>a14</anchor>
      <arglist>(FLAC__StreamDecoder *decoder, void *value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_decoder_set_metadata_respond</name>
      <anchor>a15</anchor>
      <arglist>(FLAC__StreamDecoder *decoder, FLAC__MetadataType type)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_decoder_set_metadata_respond_application</name>
      <anchor>a16</anchor>
      <arglist>(FLAC__StreamDecoder *decoder, const FLAC__byte id[4])</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_decoder_set_metadata_respond_all</name>
      <anchor>a17</anchor>
      <arglist>(FLAC__StreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_decoder_set_metadata_ignore</name>
      <anchor>a18</anchor>
      <arglist>(FLAC__StreamDecoder *decoder, FLAC__MetadataType type)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_decoder_set_metadata_ignore_application</name>
      <anchor>a19</anchor>
      <arglist>(FLAC__StreamDecoder *decoder, const FLAC__byte id[4])</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_decoder_set_metadata_ignore_all</name>
      <anchor>a20</anchor>
      <arglist>(FLAC__StreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__StreamDecoderState</type>
      <name>FLAC__stream_decoder_get_state</name>
      <anchor>a21</anchor>
      <arglist>(const FLAC__StreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>FLAC__stream_decoder_get_channels</name>
      <anchor>a22</anchor>
      <arglist>(const FLAC__StreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__ChannelAssignment</type>
      <name>FLAC__stream_decoder_get_channel_assignment</name>
      <anchor>a23</anchor>
      <arglist>(const FLAC__StreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>FLAC__stream_decoder_get_bits_per_sample</name>
      <anchor>a24</anchor>
      <arglist>(const FLAC__StreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>FLAC__stream_decoder_get_sample_rate</name>
      <anchor>a25</anchor>
      <arglist>(const FLAC__StreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>FLAC__stream_decoder_get_blocksize</name>
      <anchor>a26</anchor>
      <arglist>(const FLAC__StreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__StreamDecoderState</type>
      <name>FLAC__stream_decoder_init</name>
      <anchor>a27</anchor>
      <arglist>(FLAC__StreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>FLAC__stream_decoder_finish</name>
      <anchor>a28</anchor>
      <arglist>(FLAC__StreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_decoder_flush</name>
      <anchor>a29</anchor>
      <arglist>(FLAC__StreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_decoder_reset</name>
      <anchor>a30</anchor>
      <arglist>(FLAC__StreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_decoder_process_single</name>
      <anchor>a31</anchor>
      <arglist>(FLAC__StreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_decoder_process_until_end_of_metadata</name>
      <anchor>a32</anchor>
      <arglist>(FLAC__StreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_decoder_process_until_end_of_stream</name>
      <anchor>a33</anchor>
      <arglist>(FLAC__StreamDecoder *decoder)</arglist>
    </member>
    <member kind="variable">
      <type>const char *const</type>
      <name>FLAC__StreamDecoderStateString</name>
      <anchor>a0</anchor>
      <arglist>[]</arglist>
    </member>
    <member kind="variable">
      <type>const char *const</type>
      <name>FLAC__StreamDecoderReadStatusString</name>
      <anchor>a1</anchor>
      <arglist>[]</arglist>
    </member>
    <member kind="variable">
      <type>const char *const</type>
      <name>FLAC__StreamDecoderWriteStatusString</name>
      <anchor>a2</anchor>
      <arglist>[]</arglist>
    </member>
    <member kind="variable">
      <type>const char *const</type>
      <name>FLAC__StreamDecoderErrorStatusString</name>
      <anchor>a3</anchor>
      <arglist>[]</arglist>
    </member>
  </compound>
  <compound kind="group">
    <name>flac_encoder</name>
    <title>FLAC/_encoder.h: encoder interfaces</title>
    <filename>group__flac__encoder.html</filename>
    <subgroup>flac_file_encoder</subgroup>
    <subgroup>flac_seekable_stream_encoder</subgroup>
    <subgroup>flac_stream_encoder</subgroup>
  </compound>
  <compound kind="group">
    <name>flac_stream_encoder</name>
    <title>FLAC/stream_encoder.h: stream encoder interface</title>
    <filename>group__flac__stream__encoder.html</filename>
    <class kind="struct">FLAC__StreamEncoder</class>
    <member kind="typedef">
      <type>FLAC__StreamEncoderWriteStatus(*</type>
      <name>FLAC__StreamEncoderWriteCallback</name>
      <anchor>a2</anchor>
      <arglist>)(const FLAC__StreamEncoder *encoder, const FLAC__byte buffer[], unsigned bytes, unsigned samples, unsigned current_frame, void *client_data)</arglist>
    </member>
    <member kind="typedef">
      <type>void(*</type>
      <name>FLAC__StreamEncoderMetadataCallback</name>
      <anchor>a3</anchor>
      <arglist>)(const FLAC__StreamEncoder *encoder, const FLAC__StreamMetadata *metadata, void *client_data)</arglist>
    </member>
    <member kind="enumeration">
      <name>FLAC__StreamEncoderState</name>
      <anchor>a51</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__STREAM_ENCODER_OK</name>
      <anchor>a51a4</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__STREAM_ENCODER_VERIFY_DECODER_ERROR</name>
      <anchor>a51a5</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__STREAM_ENCODER_VERIFY_MISMATCH_IN_AUDIO_DATA</name>
      <anchor>a51a6</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__STREAM_ENCODER_INVALID_CALLBACK</name>
      <anchor>a51a7</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__STREAM_ENCODER_INVALID_NUMBER_OF_CHANNELS</name>
      <anchor>a51a8</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__STREAM_ENCODER_INVALID_BITS_PER_SAMPLE</name>
      <anchor>a51a9</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__STREAM_ENCODER_INVALID_SAMPLE_RATE</name>
      <anchor>a51a10</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__STREAM_ENCODER_INVALID_BLOCK_SIZE</name>
      <anchor>a51a11</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__STREAM_ENCODER_INVALID_MAX_LPC_ORDER</name>
      <anchor>a51a12</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__STREAM_ENCODER_INVALID_QLP_COEFF_PRECISION</name>
      <anchor>a51a13</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__STREAM_ENCODER_MID_SIDE_CHANNELS_MISMATCH</name>
      <anchor>a51a14</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__STREAM_ENCODER_MID_SIDE_SAMPLE_SIZE_MISMATCH</name>
      <anchor>a51a15</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__STREAM_ENCODER_ILLEGAL_MID_SIDE_FORCE</name>
      <anchor>a51a16</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__STREAM_ENCODER_BLOCK_SIZE_TOO_SMALL_FOR_LPC_ORDER</name>
      <anchor>a51a17</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__STREAM_ENCODER_NOT_STREAMABLE</name>
      <anchor>a51a18</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__STREAM_ENCODER_FRAMING_ERROR</name>
      <anchor>a51a19</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__STREAM_ENCODER_INVALID_METADATA</name>
      <anchor>a51a20</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__STREAM_ENCODER_FATAL_ERROR_WHILE_ENCODING</name>
      <anchor>a51a21</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__STREAM_ENCODER_FATAL_ERROR_WHILE_WRITING</name>
      <anchor>a51a22</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__STREAM_ENCODER_MEMORY_ALLOCATION_ERROR</name>
      <anchor>a51a23</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__STREAM_ENCODER_ALREADY_INITIALIZED</name>
      <anchor>a51a24</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__STREAM_ENCODER_UNINITIALIZED</name>
      <anchor>a51a25</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumeration">
      <name>FLAC__StreamEncoderWriteStatus</name>
      <anchor>a52</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__STREAM_ENCODER_WRITE_STATUS_OK</name>
      <anchor>a52a26</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>FLAC__STREAM_ENCODER_WRITE_STATUS_FATAL_ERROR</name>
      <anchor>a52a27</anchor>
      <arglist></arglist>
    </member>
    <member kind="function">
      <type>FLAC__StreamEncoder *</type>
      <name>FLAC__stream_encoder_new</name>
      <anchor>a4</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>FLAC__stream_encoder_delete</name>
      <anchor>a5</anchor>
      <arglist>(FLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_set_verify</name>
      <anchor>a6</anchor>
      <arglist>(FLAC__StreamEncoder *encoder, FLAC__bool value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_set_streamable_subset</name>
      <anchor>a7</anchor>
      <arglist>(FLAC__StreamEncoder *encoder, FLAC__bool value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_set_do_mid_side_stereo</name>
      <anchor>a8</anchor>
      <arglist>(FLAC__StreamEncoder *encoder, FLAC__bool value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_set_loose_mid_side_stereo</name>
      <anchor>a9</anchor>
      <arglist>(FLAC__StreamEncoder *encoder, FLAC__bool value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_set_channels</name>
      <anchor>a10</anchor>
      <arglist>(FLAC__StreamEncoder *encoder, unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_set_bits_per_sample</name>
      <anchor>a11</anchor>
      <arglist>(FLAC__StreamEncoder *encoder, unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_set_sample_rate</name>
      <anchor>a12</anchor>
      <arglist>(FLAC__StreamEncoder *encoder, unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_set_blocksize</name>
      <anchor>a13</anchor>
      <arglist>(FLAC__StreamEncoder *encoder, unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_set_max_lpc_order</name>
      <anchor>a14</anchor>
      <arglist>(FLAC__StreamEncoder *encoder, unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_set_qlp_coeff_precision</name>
      <anchor>a15</anchor>
      <arglist>(FLAC__StreamEncoder *encoder, unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_set_do_qlp_coeff_prec_search</name>
      <anchor>a16</anchor>
      <arglist>(FLAC__StreamEncoder *encoder, FLAC__bool value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_set_do_escape_coding</name>
      <anchor>a17</anchor>
      <arglist>(FLAC__StreamEncoder *encoder, FLAC__bool value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_set_do_exhaustive_model_search</name>
      <anchor>a18</anchor>
      <arglist>(FLAC__StreamEncoder *encoder, FLAC__bool value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_set_min_residual_partition_order</name>
      <anchor>a19</anchor>
      <arglist>(FLAC__StreamEncoder *encoder, unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_set_max_residual_partition_order</name>
      <anchor>a20</anchor>
      <arglist>(FLAC__StreamEncoder *encoder, unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_set_rice_parameter_search_dist</name>
      <anchor>a21</anchor>
      <arglist>(FLAC__StreamEncoder *encoder, unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_set_total_samples_estimate</name>
      <anchor>a22</anchor>
      <arglist>(FLAC__StreamEncoder *encoder, FLAC__uint64 value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_set_metadata</name>
      <anchor>a23</anchor>
      <arglist>(FLAC__StreamEncoder *encoder, FLAC__StreamMetadata **metadata, unsigned num_blocks)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_set_write_callback</name>
      <anchor>a24</anchor>
      <arglist>(FLAC__StreamEncoder *encoder, FLAC__StreamEncoderWriteCallback value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_set_metadata_callback</name>
      <anchor>a25</anchor>
      <arglist>(FLAC__StreamEncoder *encoder, FLAC__StreamEncoderMetadataCallback value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_set_client_data</name>
      <anchor>a26</anchor>
      <arglist>(FLAC__StreamEncoder *encoder, void *value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__StreamEncoderState</type>
      <name>FLAC__stream_encoder_get_state</name>
      <anchor>a27</anchor>
      <arglist>(const FLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__StreamDecoderState</type>
      <name>FLAC__stream_encoder_get_verify_decoder_state</name>
      <anchor>a28</anchor>
      <arglist>(const FLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>FLAC__stream_encoder_get_verify_decoder_error_stats</name>
      <anchor>a29</anchor>
      <arglist>(const FLAC__StreamEncoder *encoder, FLAC__uint64 *absolute_sample, unsigned *frame_number, unsigned *channel, unsigned *sample, FLAC__int32 *expected, FLAC__int32 *got)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_get_verify</name>
      <anchor>a30</anchor>
      <arglist>(const FLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_get_streamable_subset</name>
      <anchor>a31</anchor>
      <arglist>(const FLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_get_do_mid_side_stereo</name>
      <anchor>a32</anchor>
      <arglist>(const FLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_get_loose_mid_side_stereo</name>
      <anchor>a33</anchor>
      <arglist>(const FLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>FLAC__stream_encoder_get_channels</name>
      <anchor>a34</anchor>
      <arglist>(const FLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>FLAC__stream_encoder_get_bits_per_sample</name>
      <anchor>a35</anchor>
      <arglist>(const FLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>FLAC__stream_encoder_get_sample_rate</name>
      <anchor>a36</anchor>
      <arglist>(const FLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>FLAC__stream_encoder_get_blocksize</name>
      <anchor>a37</anchor>
      <arglist>(const FLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>FLAC__stream_encoder_get_max_lpc_order</name>
      <anchor>a38</anchor>
      <arglist>(const FLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>FLAC__stream_encoder_get_qlp_coeff_precision</name>
      <anchor>a39</anchor>
      <arglist>(const FLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_get_do_qlp_coeff_prec_search</name>
      <anchor>a40</anchor>
      <arglist>(const FLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_get_do_escape_coding</name>
      <anchor>a41</anchor>
      <arglist>(const FLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_get_do_exhaustive_model_search</name>
      <anchor>a42</anchor>
      <arglist>(const FLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>FLAC__stream_encoder_get_min_residual_partition_order</name>
      <anchor>a43</anchor>
      <arglist>(const FLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>FLAC__stream_encoder_get_max_residual_partition_order</name>
      <anchor>a44</anchor>
      <arglist>(const FLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>FLAC__stream_encoder_get_rice_parameter_search_dist</name>
      <anchor>a45</anchor>
      <arglist>(const FLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__uint64</type>
      <name>FLAC__stream_encoder_get_total_samples_estimate</name>
      <anchor>a46</anchor>
      <arglist>(const FLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__StreamEncoderState</type>
      <name>FLAC__stream_encoder_init</name>
      <anchor>a47</anchor>
      <arglist>(FLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>FLAC__stream_encoder_finish</name>
      <anchor>a48</anchor>
      <arglist>(FLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_process</name>
      <anchor>a49</anchor>
      <arglist>(FLAC__StreamEncoder *encoder, const FLAC__int32 *const buffer[], unsigned samples)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_process_interleaved</name>
      <anchor>a50</anchor>
      <arglist>(FLAC__StreamEncoder *encoder, const FLAC__int32 buffer[], unsigned samples)</arglist>
    </member>
    <member kind="variable">
      <type>const char *const</type>
      <name>FLAC__StreamEncoderStateString</name>
      <anchor>a0</anchor>
      <arglist>[]</arglist>
    </member>
    <member kind="variable">
      <type>const char *const</type>
      <name>FLAC__StreamEncoderWriteStatusString</name>
      <anchor>a1</anchor>
      <arglist>[]</arglist>
    </member>
  </compound>
  <compound kind="group">
    <name>flacpp</name>
    <title>FLAC C++ API</title>
    <filename>group__flacpp.html</filename>
    <subgroup>flacpp_decoder</subgroup>
    <subgroup>flacpp_encoder</subgroup>
    <subgroup>flacpp_metadata</subgroup>
  </compound>
  <compound kind="group">
    <name>flacpp_decoder</name>
    <title>FLAC++/decoder.h: decoder classes</title>
    <filename>group__flacpp__decoder.html</filename>
    <subgroup>flacpp_stream_decoder</subgroup>
    <subgroup>flacpp_seekable_stream_decoder</subgroup>
    <subgroup>flacpp_file_decoder</subgroup>
  </compound>
  <compound kind="group">
    <name>flacpp_stream_decoder</name>
    <title>FLAC++/decoder.h: stream decoder class</title>
    <filename>group__flacpp__stream__decoder.html</filename>
    <class kind="class">FLAC::Decoder::Stream</class>
  </compound>
  <compound kind="group">
    <name>flacpp_seekable_stream_decoder</name>
    <title>FLAC++/decoder.h: seekable stream decoder class</title>
    <filename>group__flacpp__seekable__stream__decoder.html</filename>
    <class kind="class">FLAC::Decoder::SeekableStream</class>
  </compound>
  <compound kind="group">
    <name>flacpp_file_decoder</name>
    <title>FLAC++/decoder.h: file decoder class</title>
    <filename>group__flacpp__file__decoder.html</filename>
    <class kind="class">FLAC::Decoder::File</class>
  </compound>
  <compound kind="group">
    <name>flacpp_encoder</name>
    <title>FLAC++/encoder.h: encoder classes</title>
    <filename>group__flacpp__encoder.html</filename>
    <subgroup>flacpp_stream_encoder</subgroup>
    <subgroup>flacpp_seekable_stream_encoder</subgroup>
    <subgroup>flacpp_file_encoder</subgroup>
  </compound>
  <compound kind="group">
    <name>flacpp_stream_encoder</name>
    <title>FLAC++/encoder.h: stream encoder class</title>
    <filename>group__flacpp__stream__encoder.html</filename>
    <class kind="class">FLAC::Encoder::Stream</class>
  </compound>
  <compound kind="group">
    <name>flacpp_seekable_stream_encoder</name>
    <title>FLAC++/encoder.h: seekable stream encoder class</title>
    <filename>group__flacpp__seekable__stream__encoder.html</filename>
    <class kind="class">FLAC::Encoder::SeekableStream</class>
  </compound>
  <compound kind="group">
    <name>flacpp_file_encoder</name>
    <title>FLAC++/encoder.h: file encoder class</title>
    <filename>group__flacpp__file__encoder.html</filename>
    <class kind="class">FLAC::Encoder::File</class>
  </compound>
  <compound kind="group">
    <name>flacpp_metadata</name>
    <title>FLAC++/metadata.h: metadata interfaces</title>
    <filename>group__flacpp__metadata.html</filename>
    <subgroup>flacpp_metadata_object</subgroup>
    <subgroup>flacpp_metadata_level0</subgroup>
    <subgroup>flacpp_metadata_level1</subgroup>
    <subgroup>flacpp_metadata_level2</subgroup>
  </compound>
  <compound kind="group">
    <name>flacpp_metadata_object</name>
    <title>FLAC++/metadata.h: metadata object classes</title>
    <filename>group__flacpp__metadata__object.html</filename>
    <class kind="class">FLAC::Metadata::Prototype</class>
    <class kind="class">FLAC::Metadata::StreamInfo</class>
    <class kind="class">FLAC::Metadata::Padding</class>
    <class kind="class">FLAC::Metadata::Application</class>
    <class kind="class">FLAC::Metadata::SeekTable</class>
    <class kind="class">FLAC::Metadata::VorbisComment</class>
    <member kind="function">
      <type>bool</type>
      <name>operator==</name>
      <anchor>a1</anchor>
      <arglist>(const Prototype &amp;) const</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>operator==</name>
      <anchor>a2</anchor>
      <arglist>(const::FLAC__StreamMetadata &amp;) const</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>operator==</name>
      <anchor>a3</anchor>
      <arglist>(const::FLAC__StreamMetadata *) const</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>operator!=</name>
      <anchor>a4</anchor>
      <arglist>(const Prototype &amp;) const</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>operator!=</name>
      <anchor>a5</anchor>
      <arglist>(const::FLAC__StreamMetadata &amp;) const</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>operator!=</name>
      <anchor>a6</anchor>
      <arglist>(const::FLAC__StreamMetadata *) const</arglist>
    </member>
    <member kind="function">
      <type>Prototype *</type>
      <name>clone</name>
      <anchor>a0</anchor>
      <arglist>(const Prototype *)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>is_valid</name>
      <anchor>a7</anchor>
      <arglist>() const</arglist>
    </member>
  </compound>
  <compound kind="group">
    <name>flacpp_metadata_level0</name>
    <title>FLAC++/metadata.h: metadata level 0 interface</title>
    <filename>group__flacpp__metadata__level0.html</filename>
    <member kind="function">
      <type>bool</type>
      <name>get_streaminfo</name>
      <anchor>a0</anchor>
      <arglist>(const char *filename, StreamInfo &amp;streaminfo)</arglist>
    </member>
  </compound>
  <compound kind="group">
    <name>flacpp_metadata_level1</name>
    <title>FLAC++/metadata.h: metadata level 1 interface</title>
    <filename>group__flacpp__metadata__level1.html</filename>
    <class kind="class">FLAC::Metadata::SimpleIterator</class>
  </compound>
  <compound kind="group">
    <name>flacpp_metadata_level2</name>
    <title>FLAC++/metadata.h: metadata level 2 interface</title>
    <filename>group__flacpp__metadata__level2.html</filename>
    <class kind="class">FLAC::Metadata::Chain</class>
    <class kind="class">FLAC::Metadata::Iterator</class>
  </compound>
  <compound kind="group">
    <name>oggflac</name>
    <title>OggFLAC C API</title>
    <filename>group__oggflac.html</filename>
    <subgroup>oggflac_decoder</subgroup>
    <subgroup>oggflac_encoder</subgroup>
  </compound>
  <compound kind="group">
    <name>oggflac_decoder</name>
    <title>OggFLAC/_decoder.h: decoder interfaces</title>
    <filename>group__oggflac__decoder.html</filename>
    <subgroup>oggflac_stream_decoder</subgroup>
  </compound>
  <compound kind="group">
    <name>oggflac_stream_decoder</name>
    <title>OggFLAC/stream_decoder.h: stream decoder interface</title>
    <filename>group__oggflac__stream__decoder.html</filename>
    <class kind="struct">OggFLAC__StreamDecoder</class>
    <member kind="typedef">
      <type>FLAC__StreamDecoderReadStatus(*</type>
      <name>OggFLAC__StreamDecoderReadCallback</name>
      <anchor>a1</anchor>
      <arglist>)(const OggFLAC__StreamDecoder *decoder, FLAC__byte buffer[], unsigned *bytes, void *client_data)</arglist>
    </member>
    <member kind="typedef">
      <type>FLAC__StreamDecoderWriteStatus(*</type>
      <name>OggFLAC__StreamDecoderWriteCallback</name>
      <anchor>a2</anchor>
      <arglist>)(const OggFLAC__StreamDecoder *decoder, const FLAC__Frame *frame, const FLAC__int32 *const buffer[], void *client_data)</arglist>
    </member>
    <member kind="typedef">
      <type>void(*</type>
      <name>OggFLAC__StreamDecoderMetadataCallback</name>
      <anchor>a3</anchor>
      <arglist>)(const OggFLAC__StreamDecoder *decoder, const FLAC__StreamMetadata *metadata, void *client_data)</arglist>
    </member>
    <member kind="typedef">
      <type>void(*</type>
      <name>OggFLAC__StreamDecoderErrorCallback</name>
      <anchor>a4</anchor>
      <arglist>)(const OggFLAC__StreamDecoder *decoder, FLAC__StreamDecoderErrorStatus status, void *client_data)</arglist>
    </member>
    <member kind="enumeration">
      <name>OggFLAC__StreamDecoderState</name>
      <anchor>a33</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>OggFLAC__STREAM_DECODER_OK</name>
      <anchor>a33a5</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>OggFLAC__STREAM_DECODER_OGG_ERROR</name>
      <anchor>a33a6</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>OggFLAC__STREAM_DECODER_READ_ERROR</name>
      <anchor>a33a7</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>OggFLAC__STREAM_DECODER_FLAC_STREAM_DECODER_ERROR</name>
      <anchor>a33a8</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>OggFLAC__STREAM_DECODER_INVALID_CALLBACK</name>
      <anchor>a33a9</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>OggFLAC__STREAM_DECODER_MEMORY_ALLOCATION_ERROR</name>
      <anchor>a33a10</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>OggFLAC__STREAM_DECODER_ALREADY_INITIALIZED</name>
      <anchor>a33a11</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>OggFLAC__STREAM_DECODER_UNINITIALIZED</name>
      <anchor>a33a12</anchor>
      <arglist></arglist>
    </member>
    <member kind="function">
      <type>OggFLAC__StreamDecoder *</type>
      <name>OggFLAC__stream_decoder_new</name>
      <anchor>a5</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>OggFLAC__stream_decoder_delete</name>
      <anchor>a6</anchor>
      <arglist>(OggFLAC__StreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>OggFLAC__stream_decoder_set_read_callback</name>
      <anchor>a7</anchor>
      <arglist>(OggFLAC__StreamDecoder *decoder, OggFLAC__StreamDecoderReadCallback value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>OggFLAC__stream_decoder_set_write_callback</name>
      <anchor>a8</anchor>
      <arglist>(OggFLAC__StreamDecoder *decoder, OggFLAC__StreamDecoderWriteCallback value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>OggFLAC__stream_decoder_set_metadata_callback</name>
      <anchor>a9</anchor>
      <arglist>(OggFLAC__StreamDecoder *decoder, OggFLAC__StreamDecoderMetadataCallback value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>OggFLAC__stream_decoder_set_error_callback</name>
      <anchor>a10</anchor>
      <arglist>(OggFLAC__StreamDecoder *decoder, OggFLAC__StreamDecoderErrorCallback value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>OggFLAC__stream_decoder_set_client_data</name>
      <anchor>a11</anchor>
      <arglist>(OggFLAC__StreamDecoder *decoder, void *value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>OggFLAC__stream_decoder_set_serial_number</name>
      <anchor>a12</anchor>
      <arglist>(OggFLAC__StreamDecoder *decoder, long serial_number)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>OggFLAC__stream_decoder_set_metadata_respond</name>
      <anchor>a13</anchor>
      <arglist>(OggFLAC__StreamDecoder *decoder, FLAC__MetadataType type)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>OggFLAC__stream_decoder_set_metadata_respond_application</name>
      <anchor>a14</anchor>
      <arglist>(OggFLAC__StreamDecoder *decoder, const FLAC__byte id[4])</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>OggFLAC__stream_decoder_set_metadata_respond_all</name>
      <anchor>a15</anchor>
      <arglist>(OggFLAC__StreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>OggFLAC__stream_decoder_set_metadata_ignore</name>
      <anchor>a16</anchor>
      <arglist>(OggFLAC__StreamDecoder *decoder, FLAC__MetadataType type)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>OggFLAC__stream_decoder_set_metadata_ignore_application</name>
      <anchor>a17</anchor>
      <arglist>(OggFLAC__StreamDecoder *decoder, const FLAC__byte id[4])</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>OggFLAC__stream_decoder_set_metadata_ignore_all</name>
      <anchor>a18</anchor>
      <arglist>(OggFLAC__StreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>OggFLAC__StreamDecoderState</type>
      <name>OggFLAC__stream_decoder_get_state</name>
      <anchor>a19</anchor>
      <arglist>(const OggFLAC__StreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__StreamDecoderState</type>
      <name>OggFLAC__stream_decoder_get_FLAC_stream_decoder_state</name>
      <anchor>a20</anchor>
      <arglist>(const OggFLAC__StreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>OggFLAC__stream_decoder_get_channels</name>
      <anchor>a21</anchor>
      <arglist>(const OggFLAC__StreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__ChannelAssignment</type>
      <name>OggFLAC__stream_decoder_get_channel_assignment</name>
      <anchor>a22</anchor>
      <arglist>(const OggFLAC__StreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>OggFLAC__stream_decoder_get_bits_per_sample</name>
      <anchor>a23</anchor>
      <arglist>(const OggFLAC__StreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>OggFLAC__stream_decoder_get_sample_rate</name>
      <anchor>a24</anchor>
      <arglist>(const OggFLAC__StreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>OggFLAC__stream_decoder_get_blocksize</name>
      <anchor>a25</anchor>
      <arglist>(const OggFLAC__StreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>OggFLAC__StreamDecoderState</type>
      <name>OggFLAC__stream_decoder_init</name>
      <anchor>a26</anchor>
      <arglist>(OggFLAC__StreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>OggFLAC__stream_decoder_finish</name>
      <anchor>a27</anchor>
      <arglist>(OggFLAC__StreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>OggFLAC__stream_decoder_flush</name>
      <anchor>a28</anchor>
      <arglist>(OggFLAC__StreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>OggFLAC__stream_decoder_reset</name>
      <anchor>a29</anchor>
      <arglist>(OggFLAC__StreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>OggFLAC__stream_decoder_process_single</name>
      <anchor>a30</anchor>
      <arglist>(OggFLAC__StreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>OggFLAC__stream_decoder_process_until_end_of_metadata</name>
      <anchor>a31</anchor>
      <arglist>(OggFLAC__StreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>OggFLAC__stream_decoder_process_until_end_of_stream</name>
      <anchor>a32</anchor>
      <arglist>(OggFLAC__StreamDecoder *decoder)</arglist>
    </member>
    <member kind="variable">
      <type>const char *const</type>
      <name>OggFLAC__StreamDecoderStateString</name>
      <anchor>a0</anchor>
      <arglist>[]</arglist>
    </member>
  </compound>
  <compound kind="group">
    <name>oggflac_encoder</name>
    <title>OggFLAC/_encoder.h: encoder interfaces</title>
    <filename>group__oggflac__encoder.html</filename>
    <subgroup>oggflac_stream_encoder</subgroup>
  </compound>
  <compound kind="group">
    <name>oggflac_stream_encoder</name>
    <title>OggFLAC/stream_encoder.h: stream encoder interface</title>
    <filename>group__oggflac__stream__encoder.html</filename>
    <class kind="struct">OggFLAC__StreamEncoder</class>
    <member kind="typedef">
      <type>FLAC__StreamEncoderWriteStatus(*</type>
      <name>OggFLAC__StreamEncoderWriteCallback</name>
      <anchor>a1</anchor>
      <arglist>)(const OggFLAC__StreamEncoder *encoder, const FLAC__byte buffer[], unsigned bytes, unsigned samples, unsigned current_frame, void *client_data)</arglist>
    </member>
    <member kind="enumeration">
      <name>OggFLAC__StreamEncoderState</name>
      <anchor>a50</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>OggFLAC__STREAM_ENCODER_OK</name>
      <anchor>a50a2</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>OggFLAC__STREAM_ENCODER_OGG_ERROR</name>
      <anchor>a50a3</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>OggFLAC__STREAM_ENCODER_FLAC_STREAM_ENCODER_ERROR</name>
      <anchor>a50a4</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>OggFLAC__STREAM_ENCODER_INVALID_CALLBACK</name>
      <anchor>a50a5</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>OggFLAC__STREAM_ENCODER_MEMORY_ALLOCATION_ERROR</name>
      <anchor>a50a6</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>OggFLAC__STREAM_ENCODER_ALREADY_INITIALIZED</name>
      <anchor>a50a7</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <name>OggFLAC__STREAM_ENCODER_UNINITIALIZED</name>
      <anchor>a50a8</anchor>
      <arglist></arglist>
    </member>
    <member kind="function">
      <type>OggFLAC__StreamEncoder *</type>
      <name>OggFLAC__stream_encoder_new</name>
      <anchor>a2</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>OggFLAC__stream_encoder_delete</name>
      <anchor>a3</anchor>
      <arglist>(OggFLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>OggFLAC__stream_encoder_set_serial_number</name>
      <anchor>a4</anchor>
      <arglist>(OggFLAC__StreamEncoder *encoder, long serial_number)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>OggFLAC__stream_encoder_set_verify</name>
      <anchor>a5</anchor>
      <arglist>(OggFLAC__StreamEncoder *encoder, FLAC__bool value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>OggFLAC__stream_encoder_set_streamable_subset</name>
      <anchor>a6</anchor>
      <arglist>(OggFLAC__StreamEncoder *encoder, FLAC__bool value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>OggFLAC__stream_encoder_set_do_mid_side_stereo</name>
      <anchor>a7</anchor>
      <arglist>(OggFLAC__StreamEncoder *encoder, FLAC__bool value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>OggFLAC__stream_encoder_set_loose_mid_side_stereo</name>
      <anchor>a8</anchor>
      <arglist>(OggFLAC__StreamEncoder *encoder, FLAC__bool value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>OggFLAC__stream_encoder_set_channels</name>
      <anchor>a9</anchor>
      <arglist>(OggFLAC__StreamEncoder *encoder, unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>OggFLAC__stream_encoder_set_bits_per_sample</name>
      <anchor>a10</anchor>
      <arglist>(OggFLAC__StreamEncoder *encoder, unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>OggFLAC__stream_encoder_set_sample_rate</name>
      <anchor>a11</anchor>
      <arglist>(OggFLAC__StreamEncoder *encoder, unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>OggFLAC__stream_encoder_set_blocksize</name>
      <anchor>a12</anchor>
      <arglist>(OggFLAC__StreamEncoder *encoder, unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>OggFLAC__stream_encoder_set_max_lpc_order</name>
      <anchor>a13</anchor>
      <arglist>(OggFLAC__StreamEncoder *encoder, unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>OggFLAC__stream_encoder_set_qlp_coeff_precision</name>
      <anchor>a14</anchor>
      <arglist>(OggFLAC__StreamEncoder *encoder, unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>OggFLAC__stream_encoder_set_do_qlp_coeff_prec_search</name>
      <anchor>a15</anchor>
      <arglist>(OggFLAC__StreamEncoder *encoder, FLAC__bool value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>OggFLAC__stream_encoder_set_do_escape_coding</name>
      <anchor>a16</anchor>
      <arglist>(OggFLAC__StreamEncoder *encoder, FLAC__bool value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>OggFLAC__stream_encoder_set_do_exhaustive_model_search</name>
      <anchor>a17</anchor>
      <arglist>(OggFLAC__StreamEncoder *encoder, FLAC__bool value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>OggFLAC__stream_encoder_set_min_residual_partition_order</name>
      <anchor>a18</anchor>
      <arglist>(OggFLAC__StreamEncoder *encoder, unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>OggFLAC__stream_encoder_set_max_residual_partition_order</name>
      <anchor>a19</anchor>
      <arglist>(OggFLAC__StreamEncoder *encoder, unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>OggFLAC__stream_encoder_set_rice_parameter_search_dist</name>
      <anchor>a20</anchor>
      <arglist>(OggFLAC__StreamEncoder *encoder, unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>OggFLAC__stream_encoder_set_total_samples_estimate</name>
      <anchor>a21</anchor>
      <arglist>(OggFLAC__StreamEncoder *encoder, FLAC__uint64 value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>OggFLAC__stream_encoder_set_metadata</name>
      <anchor>a22</anchor>
      <arglist>(OggFLAC__StreamEncoder *encoder, FLAC__StreamMetadata **metadata, unsigned num_blocks)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>OggFLAC__stream_encoder_set_write_callback</name>
      <anchor>a23</anchor>
      <arglist>(OggFLAC__StreamEncoder *encoder, OggFLAC__StreamEncoderWriteCallback value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>OggFLAC__stream_encoder_set_client_data</name>
      <anchor>a24</anchor>
      <arglist>(OggFLAC__StreamEncoder *encoder, void *value)</arglist>
    </member>
    <member kind="function">
      <type>OggFLAC__StreamEncoderState</type>
      <name>OggFLAC__stream_encoder_get_state</name>
      <anchor>a25</anchor>
      <arglist>(const OggFLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__StreamEncoderState</type>
      <name>OggFLAC__stream_encoder_get_FLAC_stream_encoder_state</name>
      <anchor>a26</anchor>
      <arglist>(const OggFLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__StreamDecoderState</type>
      <name>OggFLAC__stream_encoder_get_verify_decoder_state</name>
      <anchor>a27</anchor>
      <arglist>(const OggFLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>OggFLAC__stream_encoder_get_verify_decoder_error_stats</name>
      <anchor>a28</anchor>
      <arglist>(const OggFLAC__StreamEncoder *encoder, FLAC__uint64 *absolute_sample, unsigned *frame_number, unsigned *channel, unsigned *sample, FLAC__int32 *expected, FLAC__int32 *got)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>OggFLAC__stream_encoder_get_verify</name>
      <anchor>a29</anchor>
      <arglist>(const OggFLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>OggFLAC__stream_encoder_get_streamable_subset</name>
      <anchor>a30</anchor>
      <arglist>(const OggFLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>OggFLAC__stream_encoder_get_do_mid_side_stereo</name>
      <anchor>a31</anchor>
      <arglist>(const OggFLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>OggFLAC__stream_encoder_get_loose_mid_side_stereo</name>
      <anchor>a32</anchor>
      <arglist>(const OggFLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>OggFLAC__stream_encoder_get_channels</name>
      <anchor>a33</anchor>
      <arglist>(const OggFLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>OggFLAC__stream_encoder_get_bits_per_sample</name>
      <anchor>a34</anchor>
      <arglist>(const OggFLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>OggFLAC__stream_encoder_get_sample_rate</name>
      <anchor>a35</anchor>
      <arglist>(const OggFLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>OggFLAC__stream_encoder_get_blocksize</name>
      <anchor>a36</anchor>
      <arglist>(const OggFLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>OggFLAC__stream_encoder_get_max_lpc_order</name>
      <anchor>a37</anchor>
      <arglist>(const OggFLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>OggFLAC__stream_encoder_get_qlp_coeff_precision</name>
      <anchor>a38</anchor>
      <arglist>(const OggFLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>OggFLAC__stream_encoder_get_do_qlp_coeff_prec_search</name>
      <anchor>a39</anchor>
      <arglist>(const OggFLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>OggFLAC__stream_encoder_get_do_escape_coding</name>
      <anchor>a40</anchor>
      <arglist>(const OggFLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>OggFLAC__stream_encoder_get_do_exhaustive_model_search</name>
      <anchor>a41</anchor>
      <arglist>(const OggFLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>OggFLAC__stream_encoder_get_min_residual_partition_order</name>
      <anchor>a42</anchor>
      <arglist>(const OggFLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>OggFLAC__stream_encoder_get_max_residual_partition_order</name>
      <anchor>a43</anchor>
      <arglist>(const OggFLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>OggFLAC__stream_encoder_get_rice_parameter_search_dist</name>
      <anchor>a44</anchor>
      <arglist>(const OggFLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__uint64</type>
      <name>OggFLAC__stream_encoder_get_total_samples_estimate</name>
      <anchor>a45</anchor>
      <arglist>(const OggFLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>OggFLAC__StreamEncoderState</type>
      <name>OggFLAC__stream_encoder_init</name>
      <anchor>a46</anchor>
      <arglist>(OggFLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>OggFLAC__stream_encoder_finish</name>
      <anchor>a47</anchor>
      <arglist>(OggFLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>OggFLAC__stream_encoder_process</name>
      <anchor>a48</anchor>
      <arglist>(OggFLAC__StreamEncoder *encoder, const FLAC__int32 *const buffer[], unsigned samples)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>OggFLAC__stream_encoder_process_interleaved</name>
      <anchor>a49</anchor>
      <arglist>(OggFLAC__StreamEncoder *encoder, const FLAC__int32 buffer[], unsigned samples)</arglist>
    </member>
    <member kind="variable">
      <type>const char *const</type>
      <name>OggFLAC__StreamEncoderStateString</name>
      <anchor>a0</anchor>
      <arglist>[]</arglist>
    </member>
  </compound>
  <compound kind="group">
    <name>oggflacpp</name>
    <title>OggFLAC C++ API</title>
    <filename>group__oggflacpp.html</filename>
    <subgroup>oggflacpp_decoder</subgroup>
    <subgroup>oggflacpp_encoder</subgroup>
  </compound>
  <compound kind="group">
    <name>oggflacpp_decoder</name>
    <title>OggFLAC++/decoder.h: decoder classes</title>
    <filename>group__oggflacpp__decoder.html</filename>
    <subgroup>oggflacpp_stream_decoder</subgroup>
  </compound>
  <compound kind="group">
    <name>oggflacpp_stream_decoder</name>
    <title>OggFLAC++/decoder.h: stream decoder class</title>
    <filename>group__oggflacpp__stream__decoder.html</filename>
    <class kind="class">OggFLAC::Decoder::Stream</class>
  </compound>
  <compound kind="group">
    <name>oggflacpp_encoder</name>
    <title>OggFLAC++/encoder.h: encoder classes</title>
    <filename>group__oggflacpp__encoder.html</filename>
    <subgroup>oggflacpp_stream_encoder</subgroup>
  </compound>
  <compound kind="group">
    <name>oggflacpp_stream_encoder</name>
    <title>OggFLAC++/encoder.h: stream encoder class</title>
    <filename>group__oggflacpp__stream__encoder.html</filename>
    <class kind="class">OggFLAC::Encoder::Stream</class>
  </compound>
  <compound kind="class">
    <name>FLAC::Decoder::Stream</name>
    <filename>classFLAC_1_1Decoder_1_1Stream.html</filename>
    <member kind="function">
      <type></type>
      <name>Stream</name>
      <anchor>a0</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual</type>
      <name>~Stream</name>
      <anchor>a1</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>is_valid</name>
      <anchor>a2</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>operator bool</name>
      <anchor>a3</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_metadata_respond</name>
      <anchor>a4</anchor>
      <arglist>(::FLAC__MetadataType type)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_metadata_respond_application</name>
      <anchor>a5</anchor>
      <arglist>(const FLAC__byte id[4])</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_metadata_respond_all</name>
      <anchor>a6</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_metadata_ignore</name>
      <anchor>a7</anchor>
      <arglist>(::FLAC__MetadataType type)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_metadata_ignore_application</name>
      <anchor>a8</anchor>
      <arglist>(const FLAC__byte id[4])</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_metadata_ignore_all</name>
      <anchor>a9</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>State</type>
      <name>get_state</name>
      <anchor>a10</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>get_channels</name>
      <anchor>a11</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>::FLAC__ChannelAssignment</type>
      <name>get_channel_assignment</name>
      <anchor>a12</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>get_bits_per_sample</name>
      <anchor>a13</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>get_sample_rate</name>
      <anchor>a14</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>get_blocksize</name>
      <anchor>a15</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>State</type>
      <name>init</name>
      <anchor>a16</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>finish</name>
      <anchor>a17</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>flush</name>
      <anchor>a18</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>reset</name>
      <anchor>a19</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>process_single</name>
      <anchor>a20</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>process_until_end_of_metadata</name>
      <anchor>a21</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>process_until_end_of_stream</name>
      <anchor>a22</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function" protection="public" virtualness="pure">
      <type>virtual::FLAC__StreamDecoderReadStatus</type>
      <name>read_callback</name>
      <anchor>b0</anchor>
      <arglist>(FLAC__byte buffer[], unsigned *bytes)=0</arglist>
    </member>
    <member kind="function" protection="public" virtualness="pure">
      <type>virtual::FLAC__StreamDecoderWriteStatus</type>
      <name>write_callback</name>
      <anchor>b1</anchor>
      <arglist>(const::FLAC__Frame *frame, const FLAC__int32 *const buffer[])=0</arglist>
    </member>
    <member kind="function" protection="public" virtualness="pure">
      <type>virtual void</type>
      <name>metadata_callback</name>
      <anchor>b2</anchor>
      <arglist>(const::FLAC__StreamMetadata *metadata)=0</arglist>
    </member>
    <member kind="function" protection="public" virtualness="pure">
      <type>virtual void</type>
      <name>error_callback</name>
      <anchor>b3</anchor>
      <arglist>(::FLAC__StreamDecoderErrorStatus status)=0</arglist>
    </member>
    <member kind="variable" protection="public">
      <type>::FLAC__StreamDecoder *</type>
      <name>decoder_</name>
      <anchor>n0</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="class">
    <name>FLAC::Decoder::SeekableStream</name>
    <filename>classFLAC_1_1Decoder_1_1SeekableStream.html</filename>
    <member kind="function">
      <type></type>
      <name>SeekableStream</name>
      <anchor>a0</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual</type>
      <name>~SeekableStream</name>
      <anchor>a1</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>is_valid</name>
      <anchor>a2</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>operator bool</name>
      <anchor>a3</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_md5_checking</name>
      <anchor>a4</anchor>
      <arglist>(bool value)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_metadata_respond</name>
      <anchor>a5</anchor>
      <arglist>(::FLAC__MetadataType type)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_metadata_respond_application</name>
      <anchor>a6</anchor>
      <arglist>(const FLAC__byte id[4])</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_metadata_respond_all</name>
      <anchor>a7</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_metadata_ignore</name>
      <anchor>a8</anchor>
      <arglist>(::FLAC__MetadataType type)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_metadata_ignore_application</name>
      <anchor>a9</anchor>
      <arglist>(const FLAC__byte id[4])</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_metadata_ignore_all</name>
      <anchor>a10</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>State</type>
      <name>get_state</name>
      <anchor>a11</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>Stream::State</type>
      <name>get_stream_decoder_state</name>
      <anchor>a12</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>get_md5_checking</name>
      <anchor>a13</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>get_channels</name>
      <anchor>a14</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>::FLAC__ChannelAssignment</type>
      <name>get_channel_assignment</name>
      <anchor>a15</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>get_bits_per_sample</name>
      <anchor>a16</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>get_sample_rate</name>
      <anchor>a17</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>get_blocksize</name>
      <anchor>a18</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>State</type>
      <name>init</name>
      <anchor>a19</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>finish</name>
      <anchor>a20</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>flush</name>
      <anchor>a21</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>reset</name>
      <anchor>a22</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>process_single</name>
      <anchor>a23</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>process_until_end_of_metadata</name>
      <anchor>a24</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>process_until_end_of_stream</name>
      <anchor>a25</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>seek_absolute</name>
      <anchor>a26</anchor>
      <arglist>(FLAC__uint64 sample)</arglist>
    </member>
    <member kind="function" protection="public" virtualness="pure">
      <type>virtual::FLAC__SeekableStreamDecoderReadStatus</type>
      <name>read_callback</name>
      <anchor>b0</anchor>
      <arglist>(FLAC__byte buffer[], unsigned *bytes)=0</arglist>
    </member>
    <member kind="function" protection="public" virtualness="pure">
      <type>virtual::FLAC__SeekableStreamDecoderSeekStatus</type>
      <name>seek_callback</name>
      <anchor>b1</anchor>
      <arglist>(FLAC__uint64 absolute_byte_offset)=0</arglist>
    </member>
    <member kind="function" protection="public" virtualness="pure">
      <type>virtual::FLAC__SeekableStreamDecoderTellStatus</type>
      <name>tell_callback</name>
      <anchor>b2</anchor>
      <arglist>(FLAC__uint64 *absolute_byte_offset)=0</arglist>
    </member>
    <member kind="function" protection="public" virtualness="pure">
      <type>virtual::FLAC__SeekableStreamDecoderLengthStatus</type>
      <name>length_callback</name>
      <anchor>b3</anchor>
      <arglist>(FLAC__uint64 *stream_length)=0</arglist>
    </member>
    <member kind="function" protection="public" virtualness="pure">
      <type>virtual bool</type>
      <name>eof_callback</name>
      <anchor>b4</anchor>
      <arglist>()=0</arglist>
    </member>
    <member kind="function" protection="public" virtualness="pure">
      <type>virtual::FLAC__StreamDecoderWriteStatus</type>
      <name>write_callback</name>
      <anchor>b5</anchor>
      <arglist>(const::FLAC__Frame *frame, const FLAC__int32 *const buffer[])=0</arglist>
    </member>
    <member kind="function" protection="public" virtualness="pure">
      <type>virtual void</type>
      <name>metadata_callback</name>
      <anchor>b6</anchor>
      <arglist>(const::FLAC__StreamMetadata *metadata)=0</arglist>
    </member>
    <member kind="function" protection="public" virtualness="pure">
      <type>virtual void</type>
      <name>error_callback</name>
      <anchor>b7</anchor>
      <arglist>(::FLAC__StreamDecoderErrorStatus status)=0</arglist>
    </member>
    <member kind="variable" protection="public">
      <type>::FLAC__SeekableStreamDecoder *</type>
      <name>decoder_</name>
      <anchor>n0</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="class">
    <name>FLAC::Decoder::File</name>
    <filename>classFLAC_1_1Decoder_1_1File.html</filename>
    <member kind="function">
      <type></type>
      <name>File</name>
      <anchor>a0</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual</type>
      <name>~File</name>
      <anchor>a1</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>is_valid</name>
      <anchor>a2</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>operator bool</name>
      <anchor>a3</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_md5_checking</name>
      <anchor>a4</anchor>
      <arglist>(bool value)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_filename</name>
      <anchor>a5</anchor>
      <arglist>(const char *value)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_metadata_respond</name>
      <anchor>a6</anchor>
      <arglist>(::FLAC__MetadataType type)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_metadata_respond_application</name>
      <anchor>a7</anchor>
      <arglist>(const FLAC__byte id[4])</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_metadata_respond_all</name>
      <anchor>a8</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_metadata_ignore</name>
      <anchor>a9</anchor>
      <arglist>(::FLAC__MetadataType type)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_metadata_ignore_application</name>
      <anchor>a10</anchor>
      <arglist>(const FLAC__byte id[4])</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_metadata_ignore_all</name>
      <anchor>a11</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>State</type>
      <name>get_state</name>
      <anchor>a12</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>SeekableStream::State</type>
      <name>get_seekable_stream_decoder_state</name>
      <anchor>a13</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>Stream::State</type>
      <name>get_stream_decoder_state</name>
      <anchor>a14</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>get_md5_checking</name>
      <anchor>a15</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>get_channels</name>
      <anchor>a16</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>::FLAC__ChannelAssignment</type>
      <name>get_channel_assignment</name>
      <anchor>a17</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>get_bits_per_sample</name>
      <anchor>a18</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>get_sample_rate</name>
      <anchor>a19</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>get_blocksize</name>
      <anchor>a20</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>State</type>
      <name>init</name>
      <anchor>a21</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>finish</name>
      <anchor>a22</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>process_single</name>
      <anchor>a23</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>process_until_end_of_metadata</name>
      <anchor>a24</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>process_until_end_of_file</name>
      <anchor>a25</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>seek_absolute</name>
      <anchor>a26</anchor>
      <arglist>(FLAC__uint64 sample)</arglist>
    </member>
    <member kind="function" protection="public" virtualness="pure">
      <type>virtual::FLAC__StreamDecoderWriteStatus</type>
      <name>write_callback</name>
      <anchor>b0</anchor>
      <arglist>(const::FLAC__Frame *frame, const FLAC__int32 *const buffer[])=0</arglist>
    </member>
    <member kind="function" protection="public" virtualness="pure">
      <type>virtual void</type>
      <name>metadata_callback</name>
      <anchor>b1</anchor>
      <arglist>(const::FLAC__StreamMetadata *metadata)=0</arglist>
    </member>
    <member kind="function" protection="public" virtualness="pure">
      <type>virtual void</type>
      <name>error_callback</name>
      <anchor>b2</anchor>
      <arglist>(::FLAC__StreamDecoderErrorStatus status)=0</arglist>
    </member>
    <member kind="variable" protection="public">
      <type>::FLAC__FileDecoder *</type>
      <name>decoder_</name>
      <anchor>n0</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="class">
    <name>FLAC::Encoder::Stream</name>
    <filename>classFLAC_1_1Encoder_1_1Stream.html</filename>
    <member kind="function">
      <type></type>
      <name>Stream</name>
      <anchor>a0</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual</type>
      <name>~Stream</name>
      <anchor>a1</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>is_valid</name>
      <anchor>a2</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>operator bool</name>
      <anchor>a3</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_verify</name>
      <anchor>a4</anchor>
      <arglist>(bool value)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_streamable_subset</name>
      <anchor>a5</anchor>
      <arglist>(bool value)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_do_mid_side_stereo</name>
      <anchor>a6</anchor>
      <arglist>(bool value)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_loose_mid_side_stereo</name>
      <anchor>a7</anchor>
      <arglist>(bool value)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_channels</name>
      <anchor>a8</anchor>
      <arglist>(unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_bits_per_sample</name>
      <anchor>a9</anchor>
      <arglist>(unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_sample_rate</name>
      <anchor>a10</anchor>
      <arglist>(unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_blocksize</name>
      <anchor>a11</anchor>
      <arglist>(unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_max_lpc_order</name>
      <anchor>a12</anchor>
      <arglist>(unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_qlp_coeff_precision</name>
      <anchor>a13</anchor>
      <arglist>(unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_do_qlp_coeff_prec_search</name>
      <anchor>a14</anchor>
      <arglist>(bool value)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_do_escape_coding</name>
      <anchor>a15</anchor>
      <arglist>(bool value)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_do_exhaustive_model_search</name>
      <anchor>a16</anchor>
      <arglist>(bool value)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_min_residual_partition_order</name>
      <anchor>a17</anchor>
      <arglist>(unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_max_residual_partition_order</name>
      <anchor>a18</anchor>
      <arglist>(unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_rice_parameter_search_dist</name>
      <anchor>a19</anchor>
      <arglist>(unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_total_samples_estimate</name>
      <anchor>a20</anchor>
      <arglist>(FLAC__uint64 value)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_metadata</name>
      <anchor>a21</anchor>
      <arglist>(::FLAC__StreamMetadata **metadata, unsigned num_blocks)</arglist>
    </member>
    <member kind="function">
      <type>State</type>
      <name>get_state</name>
      <anchor>a22</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>Decoder::Stream::State</type>
      <name>get_verify_decoder_state</name>
      <anchor>a23</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>get_verify_decoder_error_stats</name>
      <anchor>a24</anchor>
      <arglist>(FLAC__uint64 *absolute_sample, unsigned *frame_number, unsigned *channel, unsigned *sample, FLAC__int32 *expected, FLAC__int32 *got)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>get_verify</name>
      <anchor>a25</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>get_streamable_subset</name>
      <anchor>a26</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>get_do_mid_side_stereo</name>
      <anchor>a27</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>get_loose_mid_side_stereo</name>
      <anchor>a28</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>get_channels</name>
      <anchor>a29</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>get_bits_per_sample</name>
      <anchor>a30</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>get_sample_rate</name>
      <anchor>a31</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>get_blocksize</name>
      <anchor>a32</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>get_max_lpc_order</name>
      <anchor>a33</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>get_qlp_coeff_precision</name>
      <anchor>a34</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>get_do_qlp_coeff_prec_search</name>
      <anchor>a35</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>get_do_escape_coding</name>
      <anchor>a36</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>get_do_exhaustive_model_search</name>
      <anchor>a37</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>get_min_residual_partition_order</name>
      <anchor>a38</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>get_max_residual_partition_order</name>
      <anchor>a39</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>get_rice_parameter_search_dist</name>
      <anchor>a40</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>FLAC__uint64</type>
      <name>get_total_samples_estimate</name>
      <anchor>a41</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>State</type>
      <name>init</name>
      <anchor>a42</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>finish</name>
      <anchor>a43</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>process</name>
      <anchor>a44</anchor>
      <arglist>(const FLAC__int32 *const buffer[], unsigned samples)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>process_interleaved</name>
      <anchor>a45</anchor>
      <arglist>(const FLAC__int32 buffer[], unsigned samples)</arglist>
    </member>
    <member kind="function" protection="public" virtualness="pure">
      <type>virtual::FLAC__StreamEncoderWriteStatus</type>
      <name>write_callback</name>
      <anchor>b0</anchor>
      <arglist>(const FLAC__byte buffer[], unsigned bytes, unsigned samples, unsigned current_frame)=0</arglist>
    </member>
    <member kind="function" protection="public" virtualness="pure">
      <type>virtual void</type>
      <name>metadata_callback</name>
      <anchor>b1</anchor>
      <arglist>(const::FLAC__StreamMetadata *metadata)=0</arglist>
    </member>
    <member kind="variable" protection="public">
      <type>::FLAC__StreamEncoder *</type>
      <name>encoder_</name>
      <anchor>n0</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="class">
    <name>FLAC::Encoder::SeekableStream</name>
    <filename>classFLAC_1_1Encoder_1_1SeekableStream.html</filename>
    <member kind="function">
      <type></type>
      <name>SeekableStream</name>
      <anchor>a0</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual</type>
      <name>~SeekableStream</name>
      <anchor>a1</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>is_valid</name>
      <anchor>a2</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>operator bool</name>
      <anchor>a3</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_verify</name>
      <anchor>a4</anchor>
      <arglist>(bool value)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_streamable_subset</name>
      <anchor>a5</anchor>
      <arglist>(bool value)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_do_mid_side_stereo</name>
      <anchor>a6</anchor>
      <arglist>(bool value)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_loose_mid_side_stereo</name>
      <anchor>a7</anchor>
      <arglist>(bool value)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_channels</name>
      <anchor>a8</anchor>
      <arglist>(unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_bits_per_sample</name>
      <anchor>a9</anchor>
      <arglist>(unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_sample_rate</name>
      <anchor>a10</anchor>
      <arglist>(unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_blocksize</name>
      <anchor>a11</anchor>
      <arglist>(unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_max_lpc_order</name>
      <anchor>a12</anchor>
      <arglist>(unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_qlp_coeff_precision</name>
      <anchor>a13</anchor>
      <arglist>(unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_do_qlp_coeff_prec_search</name>
      <anchor>a14</anchor>
      <arglist>(bool value)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_do_escape_coding</name>
      <anchor>a15</anchor>
      <arglist>(bool value)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_do_exhaustive_model_search</name>
      <anchor>a16</anchor>
      <arglist>(bool value)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_min_residual_partition_order</name>
      <anchor>a17</anchor>
      <arglist>(unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_max_residual_partition_order</name>
      <anchor>a18</anchor>
      <arglist>(unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_rice_parameter_search_dist</name>
      <anchor>a19</anchor>
      <arglist>(unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_total_samples_estimate</name>
      <anchor>a20</anchor>
      <arglist>(FLAC__uint64 value)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_metadata</name>
      <anchor>a21</anchor>
      <arglist>(::FLAC__StreamMetadata **metadata, unsigned num_blocks)</arglist>
    </member>
    <member kind="function">
      <type>State</type>
      <name>get_state</name>
      <anchor>a22</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>Stream::State</type>
      <name>get_stream_encoder_state</name>
      <anchor>a23</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>Decoder::Stream::State</type>
      <name>get_verify_decoder_state</name>
      <anchor>a24</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>get_verify_decoder_error_stats</name>
      <anchor>a25</anchor>
      <arglist>(FLAC__uint64 *absolute_sample, unsigned *frame_number, unsigned *channel, unsigned *sample, FLAC__int32 *expected, FLAC__int32 *got)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>get_verify</name>
      <anchor>a26</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>get_streamable_subset</name>
      <anchor>a27</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>get_do_mid_side_stereo</name>
      <anchor>a28</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>get_loose_mid_side_stereo</name>
      <anchor>a29</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>get_channels</name>
      <anchor>a30</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>get_bits_per_sample</name>
      <anchor>a31</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>get_sample_rate</name>
      <anchor>a32</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>get_blocksize</name>
      <anchor>a33</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>get_max_lpc_order</name>
      <anchor>a34</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>get_qlp_coeff_precision</name>
      <anchor>a35</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>get_do_qlp_coeff_prec_search</name>
      <anchor>a36</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>get_do_escape_coding</name>
      <anchor>a37</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>get_do_exhaustive_model_search</name>
      <anchor>a38</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>get_min_residual_partition_order</name>
      <anchor>a39</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>get_max_residual_partition_order</name>
      <anchor>a40</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>get_rice_parameter_search_dist</name>
      <anchor>a41</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>FLAC__uint64</type>
      <name>get_total_samples_estimate</name>
      <anchor>a42</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>State</type>
      <name>init</name>
      <anchor>a43</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>finish</name>
      <anchor>a44</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>process</name>
      <anchor>a45</anchor>
      <arglist>(const FLAC__int32 *const buffer[], unsigned samples)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>process_interleaved</name>
      <anchor>a46</anchor>
      <arglist>(const FLAC__int32 buffer[], unsigned samples)</arglist>
    </member>
    <member kind="function" protection="public" virtualness="pure">
      <type>virtual::FLAC__SeekableStreamEncoderSeekStatus</type>
      <name>seek_callback</name>
      <anchor>b0</anchor>
      <arglist>(FLAC__uint64 absolute_byte_offset)=0</arglist>
    </member>
    <member kind="function" protection="public" virtualness="pure">
      <type>virtual::FLAC__StreamEncoderWriteStatus</type>
      <name>write_callback</name>
      <anchor>b1</anchor>
      <arglist>(const FLAC__byte buffer[], unsigned bytes, unsigned samples, unsigned current_frame)=0</arglist>
    </member>
    <member kind="variable" protection="public">
      <type>::FLAC__SeekableStreamEncoder *</type>
      <name>encoder_</name>
      <anchor>n0</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="class">
    <name>FLAC::Encoder::File</name>
    <filename>classFLAC_1_1Encoder_1_1File.html</filename>
    <member kind="function">
      <type></type>
      <name>File</name>
      <anchor>a0</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual</type>
      <name>~File</name>
      <anchor>a1</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>is_valid</name>
      <anchor>a2</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>operator bool</name>
      <anchor>a3</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_verify</name>
      <anchor>a4</anchor>
      <arglist>(bool value)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_streamable_subset</name>
      <anchor>a5</anchor>
      <arglist>(bool value)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_do_mid_side_stereo</name>
      <anchor>a6</anchor>
      <arglist>(bool value)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_loose_mid_side_stereo</name>
      <anchor>a7</anchor>
      <arglist>(bool value)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_channels</name>
      <anchor>a8</anchor>
      <arglist>(unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_bits_per_sample</name>
      <anchor>a9</anchor>
      <arglist>(unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_sample_rate</name>
      <anchor>a10</anchor>
      <arglist>(unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_blocksize</name>
      <anchor>a11</anchor>
      <arglist>(unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_max_lpc_order</name>
      <anchor>a12</anchor>
      <arglist>(unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_qlp_coeff_precision</name>
      <anchor>a13</anchor>
      <arglist>(unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_do_qlp_coeff_prec_search</name>
      <anchor>a14</anchor>
      <arglist>(bool value)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_do_escape_coding</name>
      <anchor>a15</anchor>
      <arglist>(bool value)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_do_exhaustive_model_search</name>
      <anchor>a16</anchor>
      <arglist>(bool value)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_min_residual_partition_order</name>
      <anchor>a17</anchor>
      <arglist>(unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_max_residual_partition_order</name>
      <anchor>a18</anchor>
      <arglist>(unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_rice_parameter_search_dist</name>
      <anchor>a19</anchor>
      <arglist>(unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_total_samples_estimate</name>
      <anchor>a20</anchor>
      <arglist>(FLAC__uint64 value)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_metadata</name>
      <anchor>a21</anchor>
      <arglist>(::FLAC__StreamMetadata **metadata, unsigned num_blocks)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_filename</name>
      <anchor>a22</anchor>
      <arglist>(const char *value)</arglist>
    </member>
    <member kind="function">
      <type>State</type>
      <name>get_state</name>
      <anchor>a23</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>SeekableStream::State</type>
      <name>get_seekable_stream_encoder_state</name>
      <anchor>a24</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>Stream::State</type>
      <name>get_stream_encoder_state</name>
      <anchor>a25</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>Decoder::Stream::State</type>
      <name>get_verify_decoder_state</name>
      <anchor>a26</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>get_verify_decoder_error_stats</name>
      <anchor>a27</anchor>
      <arglist>(FLAC__uint64 *absolute_sample, unsigned *frame_number, unsigned *channel, unsigned *sample, FLAC__int32 *expected, FLAC__int32 *got)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>get_verify</name>
      <anchor>a28</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>get_streamable_subset</name>
      <anchor>a29</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>get_do_mid_side_stereo</name>
      <anchor>a30</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>get_loose_mid_side_stereo</name>
      <anchor>a31</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>get_channels</name>
      <anchor>a32</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>get_bits_per_sample</name>
      <anchor>a33</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>get_sample_rate</name>
      <anchor>a34</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>get_blocksize</name>
      <anchor>a35</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>get_max_lpc_order</name>
      <anchor>a36</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>get_qlp_coeff_precision</name>
      <anchor>a37</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>get_do_qlp_coeff_prec_search</name>
      <anchor>a38</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>get_do_escape_coding</name>
      <anchor>a39</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>get_do_exhaustive_model_search</name>
      <anchor>a40</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>get_min_residual_partition_order</name>
      <anchor>a41</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>get_max_residual_partition_order</name>
      <anchor>a42</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>get_rice_parameter_search_dist</name>
      <anchor>a43</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>FLAC__uint64</type>
      <name>get_total_samples_estimate</name>
      <anchor>a44</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>State</type>
      <name>init</name>
      <anchor>a45</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>finish</name>
      <anchor>a46</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>process</name>
      <anchor>a47</anchor>
      <arglist>(const FLAC__int32 *const buffer[], unsigned samples)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>process_interleaved</name>
      <anchor>a48</anchor>
      <arglist>(const FLAC__int32 buffer[], unsigned samples)</arglist>
    </member>
    <member kind="function" protection="public" virtualness="virtual">
      <type>virtual void</type>
      <name>progress_callback</name>
      <anchor>b0</anchor>
      <arglist>(FLAC__uint64 bytes_written, FLAC__uint64 samples_written, unsigned frames_written, unsigned total_frames_estimate)</arglist>
    </member>
    <member kind="variable" protection="public">
      <type>::FLAC__FileEncoder *</type>
      <name>encoder_</name>
      <anchor>n0</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="class">
    <name>FLAC::Metadata::Prototype</name>
    <filename>classFLAC_1_1Metadata_1_1Prototype.html</filename>
    <member kind="function" virtualness="virtual">
      <type>virtual</type>
      <name>~Prototype</name>
      <anchor>a0</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>is_valid</name>
      <anchor>a7</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>get_is_last</name>
      <anchor>a2</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>::FLAC__MetadataType</type>
      <name>get_type</name>
      <anchor>a3</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>get_length</name>
      <anchor>a4</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>set_is_last</name>
      <anchor>a5</anchor>
      <arglist>(bool)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>operator==</name>
      <anchor>a1</anchor>
      <arglist>(const Prototype &amp;) const</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>operator==</name>
      <anchor>a2</anchor>
      <arglist>(const::FLAC__StreamMetadata &amp;) const</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>operator==</name>
      <anchor>a3</anchor>
      <arglist>(const::FLAC__StreamMetadata *) const</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>operator!=</name>
      <anchor>a4</anchor>
      <arglist>(const Prototype &amp;) const</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>operator!=</name>
      <anchor>a5</anchor>
      <arglist>(const::FLAC__StreamMetadata &amp;) const</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>operator!=</name>
      <anchor>a6</anchor>
      <arglist>(const::FLAC__StreamMetadata *) const</arglist>
    </member>
    <member kind="function" protection="public">
      <type></type>
      <name>Prototype</name>
      <anchor>b0</anchor>
      <arglist>(::FLAC__StreamMetadata *object, bool copy)</arglist>
    </member>
    <member kind="function" protection="public" virtualness="virtual">
      <type>virtual void</type>
      <name>clear</name>
      <anchor>b1</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function" protection="public">
      <type></type>
      <name>Prototype</name>
      <anchor>z0_0</anchor>
      <arglist>(const Prototype &amp;)</arglist>
    </member>
    <member kind="function" protection="public">
      <type></type>
      <name>Prototype</name>
      <anchor>z0_1</anchor>
      <arglist>(const::FLAC__StreamMetadata &amp;)</arglist>
    </member>
    <member kind="function" protection="public">
      <type></type>
      <name>Prototype</name>
      <anchor>z0_2</anchor>
      <arglist>(const::FLAC__StreamMetadata *)</arglist>
    </member>
    <member kind="function" protection="public">
      <type>void</type>
      <name>operator=</name>
      <anchor>z1_0</anchor>
      <arglist>(const Prototype &amp;)</arglist>
    </member>
    <member kind="function" protection="public">
      <type>void</type>
      <name>operator=</name>
      <anchor>z1_1</anchor>
      <arglist>(const::FLAC__StreamMetadata &amp;)</arglist>
    </member>
    <member kind="function" protection="public">
      <type>void</type>
      <name>operator=</name>
      <anchor>z1_2</anchor>
      <arglist>(const::FLAC__StreamMetadata *)</arglist>
    </member>
    <member kind="variable" protection="public">
      <type>::FLAC__StreamMetadata *</type>
      <name>object_</name>
      <anchor>n0</anchor>
      <arglist></arglist>
    </member>
    <member kind="friend">
      <type>friend class</type>
      <name>SimpleIterator</name>
      <anchor>l0</anchor>
      <arglist></arglist>
    </member>
    <member kind="friend">
      <type>friend class</type>
      <name>Iterator</name>
      <anchor>l1</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="class">
    <name>FLAC::Metadata::StreamInfo</name>
    <filename>classFLAC_1_1Metadata_1_1StreamInfo.html</filename>
    <base>FLAC::Metadata::Prototype</base>
    <member kind="function">
      <type></type>
      <name>StreamInfo</name>
      <anchor>a0</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>StreamInfo</name>
      <anchor>a1</anchor>
      <arglist>(::FLAC__StreamMetadata *object, bool copy)</arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>~StreamInfo</name>
      <anchor>a2</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>StreamInfo</name>
      <anchor>z4_0</anchor>
      <arglist>(const StreamInfo &amp;object)</arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>StreamInfo</name>
      <anchor>z4_1</anchor>
      <arglist>(const::FLAC__StreamMetadata &amp;object)</arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>StreamInfo</name>
      <anchor>z4_2</anchor>
      <arglist>(const::FLAC__StreamMetadata *object)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>operator=</name>
      <anchor>z5_0</anchor>
      <arglist>(const StreamInfo &amp;object)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>operator=</name>
      <anchor>z5_1</anchor>
      <arglist>(const::FLAC__StreamMetadata &amp;object)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>operator=</name>
      <anchor>z5_2</anchor>
      <arglist>(const::FLAC__StreamMetadata *object)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>operator==</name>
      <anchor>z6_0</anchor>
      <arglist>(const StreamInfo &amp;object) const</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>operator==</name>
      <anchor>z6_1</anchor>
      <arglist>(const::FLAC__StreamMetadata &amp;object) const</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>operator==</name>
      <anchor>z6_2</anchor>
      <arglist>(const::FLAC__StreamMetadata *object) const</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>operator!=</name>
      <anchor>z7_0</anchor>
      <arglist>(const StreamInfo &amp;object) const</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>operator!=</name>
      <anchor>z7_1</anchor>
      <arglist>(const::FLAC__StreamMetadata &amp;object) const</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>operator!=</name>
      <anchor>z7_2</anchor>
      <arglist>(const::FLAC__StreamMetadata *object) const</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>get_min_blocksize</name>
      <anchor>z8_0</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>get_max_blocksize</name>
      <anchor>z8_1</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>get_min_framesize</name>
      <anchor>z8_2</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>get_max_framesize</name>
      <anchor>z8_3</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>get_sample_rate</name>
      <anchor>z8_4</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>get_channels</name>
      <anchor>z8_5</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>get_bits_per_sample</name>
      <anchor>z8_6</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>FLAC__uint64</type>
      <name>get_total_samples</name>
      <anchor>z8_7</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>const FLAC__byte *</type>
      <name>get_md5sum</name>
      <anchor>z8_8</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>set_min_blocksize</name>
      <anchor>z8_9</anchor>
      <arglist>(unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>set_max_blocksize</name>
      <anchor>z8_10</anchor>
      <arglist>(unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>set_min_framesize</name>
      <anchor>z8_11</anchor>
      <arglist>(unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>set_max_framesize</name>
      <anchor>z8_12</anchor>
      <arglist>(unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>set_sample_rate</name>
      <anchor>z8_13</anchor>
      <arglist>(unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>set_channels</name>
      <anchor>z8_14</anchor>
      <arglist>(unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>set_bits_per_sample</name>
      <anchor>z8_15</anchor>
      <arglist>(unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>set_total_samples</name>
      <anchor>z8_16</anchor>
      <arglist>(FLAC__uint64 value)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>set_md5sum</name>
      <anchor>z8_17</anchor>
      <arglist>(const FLAC__byte value[16])</arglist>
    </member>
  </compound>
  <compound kind="class">
    <name>FLAC::Metadata::Padding</name>
    <filename>classFLAC_1_1Metadata_1_1Padding.html</filename>
    <base>FLAC::Metadata::Prototype</base>
    <member kind="function">
      <type></type>
      <name>Padding</name>
      <anchor>a0</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>Padding</name>
      <anchor>a1</anchor>
      <arglist>(::FLAC__StreamMetadata *object, bool copy)</arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>~Padding</name>
      <anchor>a2</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>set_length</name>
      <anchor>a3</anchor>
      <arglist>(unsigned length)</arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>Padding</name>
      <anchor>z9_0</anchor>
      <arglist>(const Padding &amp;object)</arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>Padding</name>
      <anchor>z9_1</anchor>
      <arglist>(const::FLAC__StreamMetadata &amp;object)</arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>Padding</name>
      <anchor>z9_2</anchor>
      <arglist>(const::FLAC__StreamMetadata *object)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>operator=</name>
      <anchor>z10_0</anchor>
      <arglist>(const Padding &amp;object)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>operator=</name>
      <anchor>z10_1</anchor>
      <arglist>(const::FLAC__StreamMetadata &amp;object)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>operator=</name>
      <anchor>z10_2</anchor>
      <arglist>(const::FLAC__StreamMetadata *object)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>operator==</name>
      <anchor>z11_0</anchor>
      <arglist>(const Padding &amp;object) const</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>operator==</name>
      <anchor>z11_1</anchor>
      <arglist>(const::FLAC__StreamMetadata &amp;object) const</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>operator==</name>
      <anchor>z11_2</anchor>
      <arglist>(const::FLAC__StreamMetadata *object) const</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>operator!=</name>
      <anchor>z12_0</anchor>
      <arglist>(const Padding &amp;object) const</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>operator!=</name>
      <anchor>z12_1</anchor>
      <arglist>(const::FLAC__StreamMetadata &amp;object) const</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>operator!=</name>
      <anchor>z12_2</anchor>
      <arglist>(const::FLAC__StreamMetadata *object) const</arglist>
    </member>
  </compound>
  <compound kind="class">
    <name>FLAC::Metadata::Application</name>
    <filename>classFLAC_1_1Metadata_1_1Application.html</filename>
    <base>FLAC::Metadata::Prototype</base>
    <member kind="function">
      <type></type>
      <name>Application</name>
      <anchor>a0</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>Application</name>
      <anchor>a1</anchor>
      <arglist>(::FLAC__StreamMetadata *object, bool copy)</arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>~Application</name>
      <anchor>a2</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>const FLAC__byte *</type>
      <name>get_id</name>
      <anchor>a3</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>const FLAC__byte *</type>
      <name>get_data</name>
      <anchor>a4</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>set_id</name>
      <anchor>a5</anchor>
      <arglist>(const FLAC__byte value[4])</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_data</name>
      <anchor>a6</anchor>
      <arglist>(const FLAC__byte *data, unsigned length)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_data</name>
      <anchor>a7</anchor>
      <arglist>(FLAC__byte *data, unsigned length, bool copy)</arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>Application</name>
      <anchor>z13_0</anchor>
      <arglist>(const Application &amp;object)</arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>Application</name>
      <anchor>z13_1</anchor>
      <arglist>(const::FLAC__StreamMetadata &amp;object)</arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>Application</name>
      <anchor>z13_2</anchor>
      <arglist>(const::FLAC__StreamMetadata *object)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>operator=</name>
      <anchor>z14_0</anchor>
      <arglist>(const Application &amp;object)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>operator=</name>
      <anchor>z14_1</anchor>
      <arglist>(const::FLAC__StreamMetadata &amp;object)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>operator=</name>
      <anchor>z14_2</anchor>
      <arglist>(const::FLAC__StreamMetadata *object)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>operator==</name>
      <anchor>z15_0</anchor>
      <arglist>(const Application &amp;object) const</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>operator==</name>
      <anchor>z15_1</anchor>
      <arglist>(const::FLAC__StreamMetadata &amp;object) const</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>operator==</name>
      <anchor>z15_2</anchor>
      <arglist>(const::FLAC__StreamMetadata *object) const</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>operator!=</name>
      <anchor>z16_0</anchor>
      <arglist>(const Application &amp;object) const</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>operator!=</name>
      <anchor>z16_1</anchor>
      <arglist>(const::FLAC__StreamMetadata &amp;object) const</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>operator!=</name>
      <anchor>z16_2</anchor>
      <arglist>(const::FLAC__StreamMetadata *object) const</arglist>
    </member>
  </compound>
  <compound kind="class">
    <name>FLAC::Metadata::SeekTable</name>
    <filename>classFLAC_1_1Metadata_1_1SeekTable.html</filename>
    <base>FLAC::Metadata::Prototype</base>
    <member kind="function">
      <type></type>
      <name>SeekTable</name>
      <anchor>a0</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>SeekTable</name>
      <anchor>a1</anchor>
      <arglist>(::FLAC__StreamMetadata *object, bool copy)</arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>~SeekTable</name>
      <anchor>a2</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>get_num_points</name>
      <anchor>a3</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>::FLAC__StreamMetadata_SeekPoint</type>
      <name>get_point</name>
      <anchor>a4</anchor>
      <arglist>(unsigned index) const</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>set_point</name>
      <anchor>a5</anchor>
      <arglist>(unsigned index, const::FLAC__StreamMetadata_SeekPoint &amp;point)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>insert_point</name>
      <anchor>a6</anchor>
      <arglist>(unsigned index, const::FLAC__StreamMetadata_SeekPoint &amp;point)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>delete_point</name>
      <anchor>a7</anchor>
      <arglist>(unsigned index)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>is_legal</name>
      <anchor>a8</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>SeekTable</name>
      <anchor>z17_0</anchor>
      <arglist>(const SeekTable &amp;object)</arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>SeekTable</name>
      <anchor>z17_1</anchor>
      <arglist>(const::FLAC__StreamMetadata &amp;object)</arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>SeekTable</name>
      <anchor>z17_2</anchor>
      <arglist>(const::FLAC__StreamMetadata *object)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>operator=</name>
      <anchor>z18_0</anchor>
      <arglist>(const SeekTable &amp;object)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>operator=</name>
      <anchor>z18_1</anchor>
      <arglist>(const::FLAC__StreamMetadata &amp;object)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>operator=</name>
      <anchor>z18_2</anchor>
      <arglist>(const::FLAC__StreamMetadata *object)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>operator==</name>
      <anchor>z19_0</anchor>
      <arglist>(const SeekTable &amp;object) const</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>operator==</name>
      <anchor>z19_1</anchor>
      <arglist>(const::FLAC__StreamMetadata &amp;object) const</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>operator==</name>
      <anchor>z19_2</anchor>
      <arglist>(const::FLAC__StreamMetadata *object) const</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>operator!=</name>
      <anchor>z20_0</anchor>
      <arglist>(const SeekTable &amp;object) const</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>operator!=</name>
      <anchor>z20_1</anchor>
      <arglist>(const::FLAC__StreamMetadata &amp;object) const</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>operator!=</name>
      <anchor>z20_2</anchor>
      <arglist>(const::FLAC__StreamMetadata *object) const</arglist>
    </member>
  </compound>
  <compound kind="class">
    <name>FLAC::Metadata::VorbisComment</name>
    <filename>classFLAC_1_1Metadata_1_1VorbisComment.html</filename>
    <base>FLAC::Metadata::Prototype</base>
    <member kind="function">
      <type></type>
      <name>VorbisComment</name>
      <anchor>a0</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>VorbisComment</name>
      <anchor>a1</anchor>
      <arglist>(::FLAC__StreamMetadata *object, bool copy)</arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>~VorbisComment</name>
      <anchor>a2</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>get_num_comments</name>
      <anchor>a3</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>Entry</type>
      <name>get_vendor_string</name>
      <anchor>a4</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>Entry</type>
      <name>get_comment</name>
      <anchor>a5</anchor>
      <arglist>(unsigned index) const</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_vendor_string</name>
      <anchor>a6</anchor>
      <arglist>(const Entry &amp;entry)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_comment</name>
      <anchor>a7</anchor>
      <arglist>(unsigned index, const Entry &amp;entry)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>insert_comment</name>
      <anchor>a8</anchor>
      <arglist>(unsigned index, const Entry &amp;entry)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>delete_comment</name>
      <anchor>a9</anchor>
      <arglist>(unsigned index)</arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>VorbisComment</name>
      <anchor>z21_0</anchor>
      <arglist>(const VorbisComment &amp;object)</arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>VorbisComment</name>
      <anchor>z21_1</anchor>
      <arglist>(const::FLAC__StreamMetadata &amp;object)</arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>VorbisComment</name>
      <anchor>z21_2</anchor>
      <arglist>(const::FLAC__StreamMetadata *object)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>operator=</name>
      <anchor>z22_0</anchor>
      <arglist>(const VorbisComment &amp;object)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>operator=</name>
      <anchor>z22_1</anchor>
      <arglist>(const::FLAC__StreamMetadata &amp;object)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>operator=</name>
      <anchor>z22_2</anchor>
      <arglist>(const::FLAC__StreamMetadata *object)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>operator==</name>
      <anchor>z23_0</anchor>
      <arglist>(const VorbisComment &amp;object) const</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>operator==</name>
      <anchor>z23_1</anchor>
      <arglist>(const::FLAC__StreamMetadata &amp;object) const</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>operator==</name>
      <anchor>z23_2</anchor>
      <arglist>(const::FLAC__StreamMetadata *object) const</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>operator!=</name>
      <anchor>z24_0</anchor>
      <arglist>(const VorbisComment &amp;object) const</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>operator!=</name>
      <anchor>z24_1</anchor>
      <arglist>(const::FLAC__StreamMetadata &amp;object) const</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>operator!=</name>
      <anchor>z24_2</anchor>
      <arglist>(const::FLAC__StreamMetadata *object) const</arglist>
    </member>
  </compound>
  <compound kind="class">
    <name>FLAC::Metadata::VorbisComment::Entry</name>
    <filename>classFLAC_1_1Metadata_1_1VorbisComment_1_1Entry.html</filename>
    <member kind="function">
      <type></type>
      <name>Entry</name>
      <anchor>a0</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>Entry</name>
      <anchor>a1</anchor>
      <arglist>(const char *field, unsigned field_length)</arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>Entry</name>
      <anchor>a2</anchor>
      <arglist>(const char *field_name, const char *field_value, unsigned field_value_length)</arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>Entry</name>
      <anchor>a3</anchor>
      <arglist>(const Entry &amp;entry)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>operator=</name>
      <anchor>a4</anchor>
      <arglist>(const Entry &amp;entry)</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual</type>
      <name>~Entry</name>
      <anchor>a5</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual bool</type>
      <name>is_valid</name>
      <anchor>a6</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>get_field_length</name>
      <anchor>a7</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>get_field_name_length</name>
      <anchor>a8</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>get_field_value_length</name>
      <anchor>a9</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>::FLAC__StreamMetadata_VorbisComment_Entry</type>
      <name>get_entry</name>
      <anchor>a10</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>const char *</type>
      <name>get_field</name>
      <anchor>a11</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>const char *</type>
      <name>get_field_name</name>
      <anchor>a12</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>const char *</type>
      <name>get_field_value</name>
      <anchor>a13</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_field</name>
      <anchor>a14</anchor>
      <arglist>(const char *field, unsigned field_length)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_field_name</name>
      <anchor>a15</anchor>
      <arglist>(const char *field_name)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_field_value</name>
      <anchor>a16</anchor>
      <arglist>(const char *field_value, unsigned field_value_length)</arglist>
    </member>
    <member kind="variable" protection="public">
      <type>bool</type>
      <name>is_valid_</name>
      <anchor>n0</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable" protection="public">
      <type>::FLAC__StreamMetadata_VorbisComment_Entry</type>
      <name>entry_</name>
      <anchor>n1</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable" protection="public">
      <type>char *</type>
      <name>field_name_</name>
      <anchor>n2</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable" protection="public">
      <type>unsigned</type>
      <name>field_name_length_</name>
      <anchor>n3</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable" protection="public">
      <type>char *</type>
      <name>field_value_</name>
      <anchor>n4</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable" protection="public">
      <type>unsigned</type>
      <name>field_value_length_</name>
      <anchor>n5</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="class">
    <name>FLAC::Metadata::VorbisComment::Entry</name>
    <filename>classFLAC_1_1Metadata_1_1VorbisComment_1_1Entry.html</filename>
    <member kind="function">
      <type></type>
      <name>Entry</name>
      <anchor>a0</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>Entry</name>
      <anchor>a1</anchor>
      <arglist>(const char *field, unsigned field_length)</arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>Entry</name>
      <anchor>a2</anchor>
      <arglist>(const char *field_name, const char *field_value, unsigned field_value_length)</arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>Entry</name>
      <anchor>a3</anchor>
      <arglist>(const Entry &amp;entry)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>operator=</name>
      <anchor>a4</anchor>
      <arglist>(const Entry &amp;entry)</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual</type>
      <name>~Entry</name>
      <anchor>a5</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual bool</type>
      <name>is_valid</name>
      <anchor>a6</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>get_field_length</name>
      <anchor>a7</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>get_field_name_length</name>
      <anchor>a8</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>get_field_value_length</name>
      <anchor>a9</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>::FLAC__StreamMetadata_VorbisComment_Entry</type>
      <name>get_entry</name>
      <anchor>a10</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>const char *</type>
      <name>get_field</name>
      <anchor>a11</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>const char *</type>
      <name>get_field_name</name>
      <anchor>a12</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>const char *</type>
      <name>get_field_value</name>
      <anchor>a13</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_field</name>
      <anchor>a14</anchor>
      <arglist>(const char *field, unsigned field_length)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_field_name</name>
      <anchor>a15</anchor>
      <arglist>(const char *field_name)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_field_value</name>
      <anchor>a16</anchor>
      <arglist>(const char *field_value, unsigned field_value_length)</arglist>
    </member>
    <member kind="variable" protection="public">
      <type>bool</type>
      <name>is_valid_</name>
      <anchor>n0</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable" protection="public">
      <type>::FLAC__StreamMetadata_VorbisComment_Entry</type>
      <name>entry_</name>
      <anchor>n1</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable" protection="public">
      <type>char *</type>
      <name>field_name_</name>
      <anchor>n2</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable" protection="public">
      <type>unsigned</type>
      <name>field_name_length_</name>
      <anchor>n3</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable" protection="public">
      <type>char *</type>
      <name>field_value_</name>
      <anchor>n4</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable" protection="public">
      <type>unsigned</type>
      <name>field_value_length_</name>
      <anchor>n5</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="class">
    <name>FLAC::Metadata::SimpleIterator</name>
    <filename>classFLAC_1_1Metadata_1_1SimpleIterator.html</filename>
    <member kind="function">
      <type></type>
      <name>SimpleIterator</name>
      <anchor>a0</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual</type>
      <name>~SimpleIterator</name>
      <anchor>a1</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>init</name>
      <anchor>a2</anchor>
      <arglist>(const char *filename, bool read_only, bool preserve_file_stats)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>is_valid</name>
      <anchor>a3</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>Status</type>
      <name>status</name>
      <anchor>a4</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>is_writable</name>
      <anchor>a5</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>next</name>
      <anchor>a6</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>prev</name>
      <anchor>a7</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>::FLAC__MetadataType</type>
      <name>get_block_type</name>
      <anchor>a8</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>Prototype *</type>
      <name>get_block</name>
      <anchor>a9</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_block</name>
      <anchor>a10</anchor>
      <arglist>(Prototype *block, bool use_padding=true)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>insert_block_after</name>
      <anchor>a11</anchor>
      <arglist>(Prototype *block, bool use_padding=true)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>delete_block</name>
      <anchor>a12</anchor>
      <arglist>(bool use_padding=true)</arglist>
    </member>
    <member kind="function" protection="public">
      <type>void</type>
      <name>clear</name>
      <anchor>b0</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="variable" protection="public">
      <type>::FLAC__Metadata_SimpleIterator *</type>
      <name>iterator_</name>
      <anchor>n0</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="class">
    <name>FLAC::Metadata::Chain</name>
    <filename>classFLAC_1_1Metadata_1_1Chain.html</filename>
    <member kind="function">
      <type></type>
      <name>Chain</name>
      <anchor>a0</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual</type>
      <name>~Chain</name>
      <anchor>a1</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>is_valid</name>
      <anchor>a2</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>Status</type>
      <name>status</name>
      <anchor>a3</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>read</name>
      <anchor>a4</anchor>
      <arglist>(const char *filename)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>write</name>
      <anchor>a5</anchor>
      <arglist>(bool use_padding=true, bool preserve_file_stats=false)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>merge_padding</name>
      <anchor>a6</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>sort_padding</name>
      <anchor>a7</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function" protection="public" virtualness="virtual">
      <type>virtual void</type>
      <name>clear</name>
      <anchor>b0</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="variable" protection="public">
      <type>::FLAC__Metadata_Chain *</type>
      <name>chain_</name>
      <anchor>n0</anchor>
      <arglist></arglist>
    </member>
    <member kind="friend">
      <type>friend class</type>
      <name>Iterator</name>
      <anchor>l0</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="class">
    <name>FLAC::Metadata::Iterator</name>
    <filename>classFLAC_1_1Metadata_1_1Iterator.html</filename>
    <member kind="function">
      <type></type>
      <name>Iterator</name>
      <anchor>a0</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual</type>
      <name>~Iterator</name>
      <anchor>a1</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>is_valid</name>
      <anchor>a2</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>init</name>
      <anchor>a3</anchor>
      <arglist>(Chain &amp;chain)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>next</name>
      <anchor>a4</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>prev</name>
      <anchor>a5</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>::FLAC__MetadataType</type>
      <name>get_block_type</name>
      <anchor>a6</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>Prototype *</type>
      <name>get_block</name>
      <anchor>a7</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_block</name>
      <anchor>a8</anchor>
      <arglist>(Prototype *block)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>delete_block</name>
      <anchor>a9</anchor>
      <arglist>(bool replace_with_padding)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>insert_block_before</name>
      <anchor>a10</anchor>
      <arglist>(Prototype *block)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>insert_block_after</name>
      <anchor>a11</anchor>
      <arglist>(Prototype *block)</arglist>
    </member>
    <member kind="function" protection="public" virtualness="virtual">
      <type>virtual void</type>
      <name>clear</name>
      <anchor>b0</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="variable" protection="public">
      <type>::FLAC__Metadata_Iterator *</type>
      <name>iterator_</name>
      <anchor>n0</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="class">
    <name>OggFLAC::Decoder::Stream</name>
    <filename>classOggFLAC_1_1Decoder_1_1Stream.html</filename>
    <member kind="function">
      <type></type>
      <name>Stream</name>
      <anchor>a0</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual</type>
      <name>~Stream</name>
      <anchor>a1</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>is_valid</name>
      <anchor>a2</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>operator bool</name>
      <anchor>a3</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_serial_number</name>
      <anchor>a4</anchor>
      <arglist>(long value)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_metadata_respond</name>
      <anchor>a5</anchor>
      <arglist>(::FLAC__MetadataType type)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_metadata_respond_application</name>
      <anchor>a6</anchor>
      <arglist>(const FLAC__byte id[4])</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_metadata_respond_all</name>
      <anchor>a7</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_metadata_ignore</name>
      <anchor>a8</anchor>
      <arglist>(::FLAC__MetadataType type)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_metadata_ignore_application</name>
      <anchor>a9</anchor>
      <arglist>(const FLAC__byte id[4])</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_metadata_ignore_all</name>
      <anchor>a10</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>State</type>
      <name>get_state</name>
      <anchor>a11</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>FLAC::Decoder::Stream::State</type>
      <name>get_FLAC_stream_decoder_state</name>
      <anchor>a12</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>get_channels</name>
      <anchor>a13</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>::FLAC__ChannelAssignment</type>
      <name>get_channel_assignment</name>
      <anchor>a14</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>get_bits_per_sample</name>
      <anchor>a15</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>get_sample_rate</name>
      <anchor>a16</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>get_blocksize</name>
      <anchor>a17</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>State</type>
      <name>init</name>
      <anchor>a18</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>finish</name>
      <anchor>a19</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>flush</name>
      <anchor>a20</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>reset</name>
      <anchor>a21</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>process_single</name>
      <anchor>a22</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>process_until_end_of_metadata</name>
      <anchor>a23</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>process_until_end_of_stream</name>
      <anchor>a24</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function" protection="public" virtualness="pure">
      <type>virtual::FLAC__StreamDecoderReadStatus</type>
      <name>read_callback</name>
      <anchor>b0</anchor>
      <arglist>(FLAC__byte buffer[], unsigned *bytes)=0</arglist>
    </member>
    <member kind="function" protection="public" virtualness="pure">
      <type>virtual::FLAC__StreamDecoderWriteStatus</type>
      <name>write_callback</name>
      <anchor>b1</anchor>
      <arglist>(const::FLAC__Frame *frame, const FLAC__int32 *const buffer[])=0</arglist>
    </member>
    <member kind="function" protection="public" virtualness="pure">
      <type>virtual void</type>
      <name>metadata_callback</name>
      <anchor>b2</anchor>
      <arglist>(const::FLAC__StreamMetadata *metadata)=0</arglist>
    </member>
    <member kind="function" protection="public" virtualness="pure">
      <type>virtual void</type>
      <name>error_callback</name>
      <anchor>b3</anchor>
      <arglist>(::FLAC__StreamDecoderErrorStatus status)=0</arglist>
    </member>
    <member kind="variable" protection="public">
      <type>::OggFLAC__StreamDecoder *</type>
      <name>decoder_</name>
      <anchor>n0</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="class">
    <name>OggFLAC::Encoder::Stream</name>
    <filename>classOggFLAC_1_1Encoder_1_1Stream.html</filename>
    <member kind="function">
      <type></type>
      <name>Stream</name>
      <anchor>a0</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual</type>
      <name>~Stream</name>
      <anchor>a1</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>is_valid</name>
      <anchor>a2</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>operator bool</name>
      <anchor>a3</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_serial_number</name>
      <anchor>a4</anchor>
      <arglist>(long value)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_verify</name>
      <anchor>a5</anchor>
      <arglist>(bool value)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_streamable_subset</name>
      <anchor>a6</anchor>
      <arglist>(bool value)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_do_mid_side_stereo</name>
      <anchor>a7</anchor>
      <arglist>(bool value)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_loose_mid_side_stereo</name>
      <anchor>a8</anchor>
      <arglist>(bool value)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_channels</name>
      <anchor>a9</anchor>
      <arglist>(unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_bits_per_sample</name>
      <anchor>a10</anchor>
      <arglist>(unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_sample_rate</name>
      <anchor>a11</anchor>
      <arglist>(unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_blocksize</name>
      <anchor>a12</anchor>
      <arglist>(unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_max_lpc_order</name>
      <anchor>a13</anchor>
      <arglist>(unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_qlp_coeff_precision</name>
      <anchor>a14</anchor>
      <arglist>(unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_do_qlp_coeff_prec_search</name>
      <anchor>a15</anchor>
      <arglist>(bool value)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_do_escape_coding</name>
      <anchor>a16</anchor>
      <arglist>(bool value)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_do_exhaustive_model_search</name>
      <anchor>a17</anchor>
      <arglist>(bool value)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_min_residual_partition_order</name>
      <anchor>a18</anchor>
      <arglist>(unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_max_residual_partition_order</name>
      <anchor>a19</anchor>
      <arglist>(unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_rice_parameter_search_dist</name>
      <anchor>a20</anchor>
      <arglist>(unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_total_samples_estimate</name>
      <anchor>a21</anchor>
      <arglist>(FLAC__uint64 value)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_metadata</name>
      <anchor>a22</anchor>
      <arglist>(::FLAC__StreamMetadata **metadata, unsigned num_blocks)</arglist>
    </member>
    <member kind="function">
      <type>State</type>
      <name>get_state</name>
      <anchor>a23</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>FLAC::Encoder::Stream::State</type>
      <name>get_FLAC_stream_encoder_state</name>
      <anchor>a24</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>FLAC::Decoder::Stream::State</type>
      <name>get_verify_decoder_state</name>
      <anchor>a25</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>get_verify_decoder_error_stats</name>
      <anchor>a26</anchor>
      <arglist>(FLAC__uint64 *absolute_sample, unsigned *frame_number, unsigned *channel, unsigned *sample, FLAC__int32 *expected, FLAC__int32 *got)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>get_verify</name>
      <anchor>a27</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>get_streamable_subset</name>
      <anchor>a28</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>get_do_mid_side_stereo</name>
      <anchor>a29</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>get_loose_mid_side_stereo</name>
      <anchor>a30</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>get_channels</name>
      <anchor>a31</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>get_bits_per_sample</name>
      <anchor>a32</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>get_sample_rate</name>
      <anchor>a33</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>get_blocksize</name>
      <anchor>a34</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>get_max_lpc_order</name>
      <anchor>a35</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>get_qlp_coeff_precision</name>
      <anchor>a36</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>get_do_qlp_coeff_prec_search</name>
      <anchor>a37</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>get_do_escape_coding</name>
      <anchor>a38</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>get_do_exhaustive_model_search</name>
      <anchor>a39</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>get_min_residual_partition_order</name>
      <anchor>a40</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>get_max_residual_partition_order</name>
      <anchor>a41</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>get_rice_parameter_search_dist</name>
      <anchor>a42</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>FLAC__uint64</type>
      <name>get_total_samples_estimate</name>
      <anchor>a43</anchor>
      <arglist>() const</arglist>
    </member>
    <member kind="function">
      <type>State</type>
      <name>init</name>
      <anchor>a44</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>finish</name>
      <anchor>a45</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>process</name>
      <anchor>a46</anchor>
      <arglist>(const FLAC__int32 *const buffer[], unsigned samples)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>process_interleaved</name>
      <anchor>a47</anchor>
      <arglist>(const FLAC__int32 buffer[], unsigned samples)</arglist>
    </member>
    <member kind="function" protection="public" virtualness="pure">
      <type>virtual::FLAC__StreamEncoderWriteStatus</type>
      <name>write_callback</name>
      <anchor>b0</anchor>
      <arglist>(const FLAC__byte buffer[], unsigned bytes, unsigned samples, unsigned current_frame)=0</arglist>
    </member>
    <member kind="variable" protection="public">
      <type>::OggFLAC__StreamEncoder *</type>
      <name>encoder_</name>
      <anchor>n0</anchor>
      <arglist></arglist>
    </member>
  </compound>
</tagfile>
