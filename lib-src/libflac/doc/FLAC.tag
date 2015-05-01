<?xml version='1.0' encoding='UTF-8' standalone='yes' ?>
<tagfile>
  <compound kind="file">
    <name>decoder.h</name>
    <path>/home/erikd/Git/flac/include/FLAC++/</path>
    <filename>decoder_8h</filename>
    <includes id="_09_2export_8h" name="export.h" local="yes" imported="no">export.h</includes>
    <includes id="stream__decoder_8h" name="stream_decoder.h" local="yes" imported="no">FLAC/stream_decoder.h</includes>
    <class kind="class">FLAC::Decoder::Stream</class>
    <class kind="class">FLAC::Decoder::Stream::State</class>
    <class kind="class">FLAC::Decoder::File</class>
  </compound>
  <compound kind="file">
    <name>encoder.h</name>
    <path>/home/erikd/Git/flac/include/FLAC++/</path>
    <filename>encoder_8h</filename>
    <includes id="_09_2export_8h" name="export.h" local="yes" imported="no">export.h</includes>
    <includes id="stream__encoder_8h" name="stream_encoder.h" local="yes" imported="no">FLAC/stream_encoder.h</includes>
    <includes id="decoder_8h" name="decoder.h" local="yes" imported="no">decoder.h</includes>
    <includes id="_09_2metadata_8h" name="metadata.h" local="yes" imported="no">metadata.h</includes>
    <class kind="class">FLAC::Encoder::Stream</class>
    <class kind="class">FLAC::Encoder::Stream::State</class>
    <class kind="class">FLAC::Encoder::File</class>
  </compound>
  <compound kind="file">
    <name>callback.h</name>
    <path>/home/erikd/Git/flac/include/FLAC/</path>
    <filename>callback_8h</filename>
    <class kind="struct">FLAC__IOCallbacks</class>
    <member kind="typedef">
      <type>void *</type>
      <name>FLAC__IOHandle</name>
      <anchorfile>group__flac__callbacks.html</anchorfile>
      <anchor>ga4c329c3168dee6e352384c5e9306260d</anchor>
      <arglist></arglist>
    </member>
    <member kind="typedef">
      <type>size_t(*</type>
      <name>FLAC__IOCallback_Read</name>
      <anchorfile>group__flac__callbacks.html</anchorfile>
      <anchor>ga5d75bd553bd041db0d30d869455bab38</anchor>
      <arglist>)(void *ptr, size_t size, size_t nmemb, FLAC__IOHandle handle)</arglist>
    </member>
    <member kind="typedef">
      <type>size_t(*</type>
      <name>FLAC__IOCallback_Write</name>
      <anchorfile>group__flac__callbacks.html</anchorfile>
      <anchor>ga473cbe45055fe15c967de13181d01e87</anchor>
      <arglist>)(const void *ptr, size_t size, size_t nmemb, FLAC__IOHandle handle)</arglist>
    </member>
    <member kind="typedef">
      <type>int(*</type>
      <name>FLAC__IOCallback_Seek</name>
      <anchorfile>group__flac__callbacks.html</anchorfile>
      <anchor>ga5090a35604829d8178f4f2d1a40a38d0</anchor>
      <arglist>)(FLAC__IOHandle handle, FLAC__int64 offset, int whence)</arglist>
    </member>
    <member kind="typedef">
      <type>FLAC__int64(*</type>
      <name>FLAC__IOCallback_Tell</name>
      <anchorfile>group__flac__callbacks.html</anchorfile>
      <anchor>ga016e3dd2d006975ea1031970dac232fb</anchor>
      <arglist>)(FLAC__IOHandle handle)</arglist>
    </member>
    <member kind="typedef">
      <type>int(*</type>
      <name>FLAC__IOCallback_Eof</name>
      <anchorfile>group__flac__callbacks.html</anchorfile>
      <anchor>gac9fe1afad0426a8f1756d136b3442990</anchor>
      <arglist>)(FLAC__IOHandle handle)</arglist>
    </member>
    <member kind="typedef">
      <type>int(*</type>
      <name>FLAC__IOCallback_Close</name>
      <anchorfile>group__flac__callbacks.html</anchorfile>
      <anchor>ga3749f8621f43e937eb69a9536db3b19a</anchor>
      <arglist>)(FLAC__IOHandle handle)</arglist>
    </member>
  </compound>
  <compound kind="file">
    <name>export.h</name>
    <path>/home/erikd/Git/flac/include/FLAC/</path>
    <filename>export_8h</filename>
    <member kind="define">
      <type>#define</type>
      <name>FLAC_API_VERSION_CURRENT</name>
      <anchorfile>group__flac__export.html</anchorfile>
      <anchor>ga31180fe15eea416cd8957cfca1a4c4f8</anchor>
      <arglist></arglist>
    </member>
    <member kind="define">
      <type>#define</type>
      <name>FLAC_API_VERSION_REVISION</name>
      <anchorfile>group__flac__export.html</anchorfile>
      <anchor>ga811641dd9f8c542d9260240e7fbe8e93</anchor>
      <arglist></arglist>
    </member>
    <member kind="define">
      <type>#define</type>
      <name>FLAC_API_VERSION_AGE</name>
      <anchorfile>group__flac__export.html</anchorfile>
      <anchor>ga1add3e09c8dfd57e8c921f299f0bbec1</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>int</type>
      <name>FLAC_API_SUPPORTS_OGG_FLAC</name>
      <anchorfile>group__flac__export.html</anchorfile>
      <anchor>ga84ffcb0af1038c60eb3e21fd002093cf</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="file">
    <name>export.h</name>
    <path>/home/erikd/Git/flac/include/FLAC++/</path>
    <filename>_09_2export_8h</filename>
  </compound>
  <compound kind="file">
    <name>format.h</name>
    <path>/home/erikd/Git/flac/include/FLAC/</path>
    <filename>format_8h</filename>
    <includes id="export_8h" name="export.h" local="yes" imported="no">export.h</includes>
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
    <class kind="struct">FLAC__StreamMetadata_CueSheet_Index</class>
    <class kind="struct">FLAC__StreamMetadata_CueSheet_Track</class>
    <class kind="struct">FLAC__StreamMetadata_CueSheet</class>
    <class kind="struct">FLAC__StreamMetadata_Picture</class>
    <class kind="struct">FLAC__StreamMetadata_Unknown</class>
    <class kind="struct">FLAC__StreamMetadata</class>
    <member kind="define">
      <type>#define</type>
      <name>FLAC__MAX_METADATA_TYPE_CODE</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga626a412545818c2271fa2202c02ff1d6</anchor>
      <arglist></arglist>
    </member>
    <member kind="define">
      <type>#define</type>
      <name>FLAC__MIN_BLOCK_SIZE</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gaa5a85c2ea434221ce684be3469517003</anchor>
      <arglist></arglist>
    </member>
    <member kind="define">
      <type>#define</type>
      <name>FLAC__MAX_BLOCK_SIZE</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gaef78bc1b04f721e7b4563381f5514e8d</anchor>
      <arglist></arglist>
    </member>
    <member kind="define">
      <type>#define</type>
      <name>FLAC__SUBSET_MAX_BLOCK_SIZE_48000HZ</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga8f6ba2c28fbfcf52326d115c95b0a751</anchor>
      <arglist></arglist>
    </member>
    <member kind="define">
      <type>#define</type>
      <name>FLAC__MAX_CHANNELS</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga488aa5678a58d08f984f5d39185b763d</anchor>
      <arglist></arglist>
    </member>
    <member kind="define">
      <type>#define</type>
      <name>FLAC__MIN_BITS_PER_SAMPLE</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga30b0f21abbb2cdfd461fe04b425b5438</anchor>
      <arglist></arglist>
    </member>
    <member kind="define">
      <type>#define</type>
      <name>FLAC__MAX_BITS_PER_SAMPLE</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gad0156d56751e80241fa349d1e25064a6</anchor>
      <arglist></arglist>
    </member>
    <member kind="define">
      <type>#define</type>
      <name>FLAC__REFERENCE_CODEC_MAX_BITS_PER_SAMPLE</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga0fc418d96053d385fd2f56dce8007fbc</anchor>
      <arglist></arglist>
    </member>
    <member kind="define">
      <type>#define</type>
      <name>FLAC__MAX_SAMPLE_RATE</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga99abeef0c05c6bc76eacfa865abbfa70</anchor>
      <arglist></arglist>
    </member>
    <member kind="define">
      <type>#define</type>
      <name>FLAC__MAX_LPC_ORDER</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga16108d413f524329f338cff6e05f3aff</anchor>
      <arglist></arglist>
    </member>
    <member kind="define">
      <type>#define</type>
      <name>FLAC__SUBSET_MAX_LPC_ORDER_48000HZ</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga9791efa78147196820c86a6041d7774d</anchor>
      <arglist></arglist>
    </member>
    <member kind="define">
      <type>#define</type>
      <name>FLAC__MIN_QLP_COEFF_PRECISION</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gaf52033b2950b9396dd92b167b3bbe4db</anchor>
      <arglist></arglist>
    </member>
    <member kind="define">
      <type>#define</type>
      <name>FLAC__MAX_QLP_COEFF_PRECISION</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga6aa38a4bc5b9d96a78253ccb8b08bd1f</anchor>
      <arglist></arglist>
    </member>
    <member kind="define">
      <type>#define</type>
      <name>FLAC__MAX_FIXED_ORDER</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gabd0d5d6fe71b337244712b244ae7cb0f</anchor>
      <arglist></arglist>
    </member>
    <member kind="define">
      <type>#define</type>
      <name>FLAC__MAX_RICE_PARTITION_ORDER</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga78a2e97e230b2aa7f99edc94a466f5bb</anchor>
      <arglist></arglist>
    </member>
    <member kind="define">
      <type>#define</type>
      <name>FLAC__SUBSET_MAX_RICE_PARTITION_ORDER</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gab19dec1b56de482ccfeb5f9843f60a14</anchor>
      <arglist></arglist>
    </member>
    <member kind="define">
      <type>#define</type>
      <name>FLAC__STREAM_SYNC_LENGTH</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gae7ddaf298d3ceb83aae6301908675c1d</anchor>
      <arglist></arglist>
    </member>
    <member kind="define">
      <type>#define</type>
      <name>FLAC__STREAM_METADATA_STREAMINFO_LENGTH</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga06dfae7260da40e4c5f8fc4d531b326c</anchor>
      <arglist></arglist>
    </member>
    <member kind="define">
      <type>#define</type>
      <name>FLAC__STREAM_METADATA_SEEKPOINT_LENGTH</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gabdf85aa2c9a483378dfe850b85ab93ef</anchor>
      <arglist></arglist>
    </member>
    <member kind="define">
      <type>#define</type>
      <name>FLAC__STREAM_METADATA_HEADER_LENGTH</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga706a29b8a14902c457783bfd4fd7bab2</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumeration">
      <type></type>
      <name>FLAC__EntropyCodingMethodType</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga951733d2ea01943514290012cd622d3a</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__ENTROPY_CODING_METHOD_PARTITIONED_RICE</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gga951733d2ea01943514290012cd622d3aa5253f8b8edc61220739f229a299775dd</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__ENTROPY_CODING_METHOD_PARTITIONED_RICE2</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gga951733d2ea01943514290012cd622d3aa202960a608ee91f9f11c2575b9ecc5aa</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumeration">
      <type></type>
      <name>FLAC__SubframeType</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga1f431eaf213e74d7747589932d263348</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__SUBFRAME_TYPE_CONSTANT</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gga1f431eaf213e74d7747589932d263348a9bf56d836aeffb11d614e29ea1cdf2a9</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__SUBFRAME_TYPE_VERBATIM</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gga1f431eaf213e74d7747589932d263348a8520596ef07d6c8577f07025f137657b</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__SUBFRAME_TYPE_FIXED</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gga1f431eaf213e74d7747589932d263348a6b3cce73039a513f9afefdc8e4f664a5</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__SUBFRAME_TYPE_LPC</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gga1f431eaf213e74d7747589932d263348a31437462c3e4c3a5a214a91eff8cc3af</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumeration">
      <type></type>
      <name>FLAC__ChannelAssignment</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga79855f8525672e37f299bbe02952ef9c</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__CHANNEL_ASSIGNMENT_INDEPENDENT</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gga79855f8525672e37f299bbe02952ef9ca3c554e4c8512c2de31dfd3305f8b31b3</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__CHANNEL_ASSIGNMENT_LEFT_SIDE</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gga79855f8525672e37f299bbe02952ef9ca28d41295b20593561dc9934cc977d5cb</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__CHANNEL_ASSIGNMENT_RIGHT_SIDE</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gga79855f8525672e37f299bbe02952ef9cad155b61582140b2b90362005f1a93e2e</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__CHANNEL_ASSIGNMENT_MID_SIDE</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gga79855f8525672e37f299bbe02952ef9ca85c1512c0473b5ede364a9943759a80c</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumeration">
      <type></type>
      <name>FLAC__FrameNumberType</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga8fe9ebc78386cd2a3d23b7b8e3818e1c</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__FRAME_NUMBER_TYPE_FRAME_NUMBER</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gga8fe9ebc78386cd2a3d23b7b8e3818e1ca0b9cbf3853f0ae105cf9b5360164f794</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__FRAME_NUMBER_TYPE_SAMPLE_NUMBER</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gga8fe9ebc78386cd2a3d23b7b8e3818e1ca9220ce93dcc151e5edd5db7e7155b35a</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumeration">
      <type></type>
      <name>FLAC__MetadataType</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gac71714ba8ddbbd66d26bb78a427fac01</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__METADATA_TYPE_STREAMINFO</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ggac71714ba8ddbbd66d26bb78a427fac01acffa517e969ba6a868dcf10e5da75c28</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__METADATA_TYPE_PADDING</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ggac71714ba8ddbbd66d26bb78a427fac01a6dcb741fc0aef389580f110e88beb896</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__METADATA_TYPE_APPLICATION</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ggac71714ba8ddbbd66d26bb78a427fac01a2b287a22a1ac9440b309127884c8d41b</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__METADATA_TYPE_SEEKTABLE</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ggac71714ba8ddbbd66d26bb78a427fac01a5f6323e489be1318f0e3747960ebdd91</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__METADATA_TYPE_VORBIS_COMMENT</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ggac71714ba8ddbbd66d26bb78a427fac01ad013576bc5196b907547739518605520</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__METADATA_TYPE_CUESHEET</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ggac71714ba8ddbbd66d26bb78a427fac01a0b3f07ae60609126562cd0233ce00a65</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__METADATA_TYPE_PICTURE</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ggac71714ba8ddbbd66d26bb78a427fac01acf28ae2788366617c1aeab81d5961c6e</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__METADATA_TYPE_UNDEFINED</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ggac71714ba8ddbbd66d26bb78a427fac01acf6ac61fcc866608f5583c275dc34d47</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__MAX_METADATA_TYPE</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ggac71714ba8ddbbd66d26bb78a427fac01a1a2f283a3dd9e7b46181d7a114ec5805</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumeration">
      <type></type>
      <name>FLAC__StreamMetadata_Picture_Type</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gaf6d3e836cee023e0b8d897f1fdc9825d</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_METADATA_PICTURE_TYPE_OTHER</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ggaf6d3e836cee023e0b8d897f1fdc9825dadd6d6af32499b1973e48c9e8f13357ce</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_METADATA_PICTURE_TYPE_FILE_ICON_STANDARD</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ggaf6d3e836cee023e0b8d897f1fdc9825da5eca52e5cfcb718f33f5fce9b1021a49</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_METADATA_PICTURE_TYPE_FILE_ICON</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ggaf6d3e836cee023e0b8d897f1fdc9825daaf44b9d5fb75dde6941463e5029aa351</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_METADATA_PICTURE_TYPE_FRONT_COVER</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ggaf6d3e836cee023e0b8d897f1fdc9825da3e20b405fd4e835ff3a4465b8bcb7c36</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_METADATA_PICTURE_TYPE_BACK_COVER</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ggaf6d3e836cee023e0b8d897f1fdc9825da9ae132f2ee7d3baf35f94a9dc9640f62</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_METADATA_PICTURE_TYPE_LEAFLET_PAGE</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ggaf6d3e836cee023e0b8d897f1fdc9825dad3cb471b7925ae5034d9fd9ecfafb87a</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_METADATA_PICTURE_TYPE_MEDIA</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ggaf6d3e836cee023e0b8d897f1fdc9825dac994edc4166107ab5790e49f0b57ffd9</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_METADATA_PICTURE_TYPE_LEAD_ARTIST</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ggaf6d3e836cee023e0b8d897f1fdc9825da1282e252e20553c39907074052960f42</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_METADATA_PICTURE_TYPE_ARTIST</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ggaf6d3e836cee023e0b8d897f1fdc9825da4cead70f8720f180fc220e6df8d55cce</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_METADATA_PICTURE_TYPE_CONDUCTOR</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ggaf6d3e836cee023e0b8d897f1fdc9825dae01a47af0b0c4d89500b755ebca866ce</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_METADATA_PICTURE_TYPE_BAND</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ggaf6d3e836cee023e0b8d897f1fdc9825da8515523b4c9ab65ffef7db98bc09ceb1</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_METADATA_PICTURE_TYPE_COMPOSER</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ggaf6d3e836cee023e0b8d897f1fdc9825da5ea1554bc96deb45731bc5897600d1c2</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_METADATA_PICTURE_TYPE_LYRICIST</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ggaf6d3e836cee023e0b8d897f1fdc9825da86159eda8969514f5992b3e341103f22</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_METADATA_PICTURE_TYPE_RECORDING_LOCATION</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ggaf6d3e836cee023e0b8d897f1fdc9825dac96e810cdd81465709b4a3a03289e89c</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_METADATA_PICTURE_TYPE_DURING_RECORDING</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ggaf6d3e836cee023e0b8d897f1fdc9825da8cee3bb376ed1044b3a7e20b9c971ff1</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_METADATA_PICTURE_TYPE_DURING_PERFORMANCE</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ggaf6d3e836cee023e0b8d897f1fdc9825da4d4dc6904984370501865988d948de3f</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_METADATA_PICTURE_TYPE_VIDEO_SCREEN_CAPTURE</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ggaf6d3e836cee023e0b8d897f1fdc9825da7adc2b194968b51768721de7bda39df9</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_METADATA_PICTURE_TYPE_FISH</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ggaf6d3e836cee023e0b8d897f1fdc9825dabbf0d7c519ae8ba8cec7d1f165f67b0f</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_METADATA_PICTURE_TYPE_ILLUSTRATION</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ggaf6d3e836cee023e0b8d897f1fdc9825da89ba412c9d89c937c28afdab508d047a</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_METADATA_PICTURE_TYPE_BAND_LOGOTYPE</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ggaf6d3e836cee023e0b8d897f1fdc9825da751716a4528a78a8d53f435c816c4917</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_METADATA_PICTURE_TYPE_PUBLISHER_LOGOTYPE</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ggaf6d3e836cee023e0b8d897f1fdc9825da31d75150a4079482fe122e703eff9141</anchor>
      <arglist></arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__format_sample_rate_is_valid</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga985a32bf66e3a69a48e8f9ccd7c5e2e9</anchor>
      <arglist>(unsigned sample_rate)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__format_blocksize_is_subset</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga5370258a7aae32ad18b4c69fbd5e4a36</anchor>
      <arglist>(unsigned blocksize, unsigned sample_rate)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__format_sample_rate_is_subset</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gae305f200f9f4fca80f8ee3d004cf1164</anchor>
      <arglist>(unsigned sample_rate)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__format_vorbiscomment_entry_name_is_legal</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gae5fb55cd5977ebf178c5b38da831c057</anchor>
      <arglist>(const char *name)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__format_vorbiscomment_entry_value_is_legal</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gad4509984c8a8a0b926a4fb1ba25ec449</anchor>
      <arglist>(const FLAC__byte *value, unsigned length)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__format_vorbiscomment_entry_is_legal</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gab98da8754f99fdf7ba0583275b200de3</anchor>
      <arglist>(const FLAC__byte *entry, unsigned length)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__format_seektable_is_legal</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga02ed0843553fb8f718fe8e7c54d12244</anchor>
      <arglist>(const FLAC__StreamMetadata_SeekTable *seek_table)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>FLAC__format_seektable_sort</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga64dede2811616c7aa41caaed9c855cd4</anchor>
      <arglist>(FLAC__StreamMetadata_SeekTable *seek_table)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__format_cuesheet_is_legal</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gaa9ed0fa4ed04dbfdaa163d0f5308c080</anchor>
      <arglist>(const FLAC__StreamMetadata_CueSheet *cue_sheet, FLAC__bool check_cd_da_subset, const char **violation)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__format_picture_is_legal</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga82ca3ffc97c106c61882134f1a7fb1be</anchor>
      <arglist>(const FLAC__StreamMetadata_Picture *picture, const char **violation)</arglist>
    </member>
    <member kind="variable">
      <type>const char *</type>
      <name>FLAC__VERSION_STRING</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga52e2616f9a0b94881cd7711c18d62a35</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const char *</type>
      <name>FLAC__VENDOR_STRING</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gad5cccab0de3adda58914edf3c31fd64f</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const FLAC__byte</type>
      <name>FLAC__STREAM_SYNC_STRING</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga3f275a3a6056e0d53df3b72b03adde4b</anchor>
      <arglist>[4]</arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_SYNC</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga21e4646e61486382c6d91234474fce66</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_SYNC_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga5f4f9e89655ecc48a7a539f92da1b7e7</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const char *const</type>
      <name>FLAC__EntropyCodingMethodTypeString</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga41603ac35eed8c77c2f2e0b12067d88a</anchor>
      <arglist>[]</arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__ENTROPY_CODING_METHOD_PARTITIONED_RICE_ORDER_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga5056169ee48d8ece011d05f0fb9b3d43</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__ENTROPY_CODING_METHOD_PARTITIONED_RICE_PARAMETER_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gace7a2a1c6b75dc4b02d34933dae21cde</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__ENTROPY_CODING_METHOD_PARTITIONED_RICE2_PARAMETER_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga0b08df429809f9a78710c5251c9615ea</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__ENTROPY_CODING_METHOD_PARTITIONED_RICE_RAW_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga2299e09372ce2d652ad215ad5d57f6f7</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__ENTROPY_CODING_METHOD_PARTITIONED_RICE_ESCAPE_PARAMETER</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga8c0af33a0ef538cd8e9e04e8a25913af</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__ENTROPY_CODING_METHOD_PARTITIONED_RICE2_ESCAPE_PARAMETER</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gaad1bb90d3f58a38aab4509e43e2be1fd</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__ENTROPY_CODING_METHOD_TYPE_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga224222cd46f6613456c5efd75d713946</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const char *const</type>
      <name>FLAC__SubframeTypeString</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga78d78f45f123cfbb50cebd61b96097df</anchor>
      <arglist>[]</arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__SUBFRAME_LPC_QLP_COEFF_PRECISION_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga5fe1e86b574141cd1e11d9be6b1b8202</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__SUBFRAME_LPC_QLP_SHIFT_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga1d957b042a1e673e39a815427c6aa494</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__SUBFRAME_ZERO_PAD_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga9aace0e3dfa3bd2a3195e134852082c2</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__SUBFRAME_TYPE_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gab4b4a36a869e404834e01e288f9105a6</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__SUBFRAME_WASTED_BITS_FLAG_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gacf0f85d0592281227a5a1400087c466e</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__SUBFRAME_TYPE_CONSTANT_BYTE_ALIGNED_MASK</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gad783daf32785798dcc3a23e1a4cecefc</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__SUBFRAME_TYPE_VERBATIM_BYTE_ALIGNED_MASK</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gadf86648f3eb3ffecd547019577a2ab0f</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__SUBFRAME_TYPE_FIXED_BYTE_ALIGNED_MASK</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga3d8dbb2da910837bc5811f8b12bb60f7</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__SUBFRAME_TYPE_LPC_BYTE_ALIGNED_MASK</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gaf4edf4982798602f2793f907ee7d7695</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const char *const</type>
      <name>FLAC__ChannelAssignmentString</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gab1a1d3929a4e5a5aff2c15010742aa21</anchor>
      <arglist>[]</arglist>
    </member>
    <member kind="variable">
      <type>const char *const</type>
      <name>FLAC__FrameNumberTypeString</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga931a0e63c0f2b31fab801e1dd693fa4e</anchor>
      <arglist>[]</arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__FRAME_HEADER_SYNC</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga100c4e86ebb9b85b2a987d1ad383596b</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__FRAME_HEADER_SYNC_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga6be66feb8358e5a39869ce3e39f7b47a</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__FRAME_HEADER_RESERVED_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gad9a46acc93058fb6aba6e0cf8f9c2713</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__FRAME_HEADER_BLOCKING_STRATEGY_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gaa1f482e9172cd95795b32724784d8be9</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__FRAME_HEADER_BLOCK_SIZE_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga2c5c93ba19375583ca27c3d288e90a03</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__FRAME_HEADER_SAMPLE_RATE_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga1ec5825f1a07d1136204840d5d89feca</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__FRAME_HEADER_CHANNEL_ASSIGNMENT_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gafa9050a64f02d18ea7426e4c382bb6a6</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__FRAME_HEADER_BITS_PER_SAMPLE_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga9d6055c79974497dc99912b0e9ebbe41</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__FRAME_HEADER_ZERO_PAD_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gaf761899e8a95a7bce3f429e6648ca14e</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__FRAME_HEADER_CRC_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga82e91fc3b93cd8573c814dd282b5b6ed</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__FRAME_FOOTER_CRC_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga238b33bf853ea3ab0fc0dbaad5e41f4e</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const char *const</type>
      <name>FLAC__MetadataTypeString</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gaa9ad23f06a579d1110d61d54c8c999f0</anchor>
      <arglist>[]</arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_METADATA_STREAMINFO_MIN_BLOCK_SIZE_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gac10f1ca2318884d9ed142350744eca1a</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_METADATA_STREAMINFO_MAX_BLOCK_SIZE_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga9049314bc422ba321414afdafad76d04</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_METADATA_STREAMINFO_MIN_FRAME_SIZE_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga3abb82a701c670c7a9f8f47fc4df5edc</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_METADATA_STREAMINFO_MAX_FRAME_SIZE_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga4e7c5315f21eaa0e3f0dfb6517eb4545</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_METADATA_STREAMINFO_SAMPLE_RATE_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga7c5529d77e0cf806e709dc147ff83452</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_METADATA_STREAMINFO_CHANNELS_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga719aee1e62d71e1e03d6299aade7a7c4</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_METADATA_STREAMINFO_BITS_PER_SAMPLE_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga4d8a3f1a75a24e8d0a966f8ad01f15ed</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_METADATA_STREAMINFO_TOTAL_SAMPLES_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gaeb69211400ad50dd6c3503e8092c6afb</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_METADATA_STREAMINFO_MD5SUM_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga492d4321cbd601ed24489ad59d9ddaf8</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_METADATA_APPLICATION_ID_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gac374cf68c046406a062daf0b811e9020</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_METADATA_SEEKPOINT_SAMPLE_NUMBER_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga2afc4c1f5b9c522274e4fb7236d645f5</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_METADATA_SEEKPOINT_STREAM_OFFSET_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gafa894a5e0e9f5fa40a76c5a9f19800cf</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_METADATA_SEEKPOINT_FRAME_SAMPLES_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga330813fcf2c4ebb133843511134b6c11</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const FLAC__uint64</type>
      <name>FLAC__STREAM_METADATA_SEEKPOINT_PLACEHOLDER</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gad5d58774aea926635e6841c411d60566</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_METADATA_VORBIS_COMMENT_ENTRY_LENGTH_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gad73e13be54a583adfa4d2e43c1a4bdff</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_METADATA_VORBIS_COMMENT_NUM_COMMENTS_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gabdd8ad7a3de9abd96358fe10f4b0e6a0</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_METADATA_CUESHEET_INDEX_OFFSET_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gaec8a23f851fb225d16b996282413704b</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_METADATA_CUESHEET_INDEX_NUMBER_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gad7e0ce539714d0cc47053a6b11711227</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_METADATA_CUESHEET_INDEX_RESERVED_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga8cafc79f8b98b5ced139b37c46b4f4bf</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_METADATA_CUESHEET_TRACK_OFFSET_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gaccf9e6a290a44763f2616ff65dd4e422</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_METADATA_CUESHEET_TRACK_NUMBER_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gad0a4822167882a1321467b0dc67f145b</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_METADATA_CUESHEET_TRACK_ISRC_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gac63e5eb63aa52b1fe780bc89c55dec92</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_METADATA_CUESHEET_TRACK_TYPE_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gac32d5288f0f80fbd9101bb0352850a4f</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_METADATA_CUESHEET_TRACK_PRE_EMPHASIS_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gafb26465f020a51491ecad3c2fe8839be</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_METADATA_CUESHEET_TRACK_RESERVED_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gad825e57e995f161d945a51bb363e4650</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_METADATA_CUESHEET_TRACK_NUM_INDICES_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga443798929acc94f4b7b6c19ab92c5b25</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_METADATA_CUESHEET_MEDIA_CATALOG_NUMBER_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga380732fda7b866b075b138e9153717b9</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_METADATA_CUESHEET_LEAD_IN_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga9e47028762ec6709a14d8b81e7712285</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_METADATA_CUESHEET_IS_CD_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gaf1817667af48cd8c122488664972fd0c</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_METADATA_CUESHEET_RESERVED_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga1ceaba7fd5e900d423b6d9537d1979fd</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_METADATA_CUESHEET_NUM_TRACKS_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gaa0046d3f2bc430feb97a4b04053db01e</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const char *const</type>
      <name>FLAC__StreamMetadata_Picture_TypeString</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga2d27672452696cb97fd39db1cf43486b</anchor>
      <arglist>[]</arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_METADATA_PICTURE_TYPE_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gaa06ecd0960798a4e16e6b6a218008da7</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_METADATA_PICTURE_MIME_TYPE_LENGTH_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gafe9e4d8a4942eef747612e0cab836c4e</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_METADATA_PICTURE_DESCRIPTION_LENGTH_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga96910250e54bb335a3c11940372c6585</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_METADATA_PICTURE_WIDTH_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga729da59256030685bf9f29cdbf571e4d</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_METADATA_PICTURE_HEIGHT_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga173238fb8a7499b67b1658bb3ac6a81a</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_METADATA_PICTURE_DEPTH_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga6b9cf7c11b6fb7f96dfbc197db280128</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_METADATA_PICTURE_COLORS_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga6ed3603f4d9092d99e4b4d57bb431cc5</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_METADATA_PICTURE_DATA_LENGTH_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga808843e986308268c5dc6c841c67a74a</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_METADATA_IS_LAST_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga2a2e0d9428b90662b6790404dd393830</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_METADATA_TYPE_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gaf88b0f6621a4b22b37a8fe2ef82a7204</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_METADATA_LENGTH_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gaa16818a9f8de9e3bd75e832cc6149eb8</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="file">
    <name>metadata.h</name>
    <path>/home/erikd/Git/flac/include/FLAC/</path>
    <filename>metadata_8h</filename>
    <includes id="export_8h" name="export.h" local="yes" imported="no">export.h</includes>
    <includes id="callback_8h" name="callback.h" local="yes" imported="no">callback.h</includes>
    <includes id="format_8h" name="format.h" local="yes" imported="no">format.h</includes>
    <member kind="typedef">
      <type>struct FLAC__Metadata_SimpleIterator</type>
      <name>FLAC__Metadata_SimpleIterator</name>
      <anchorfile>group__flac__metadata__level1.html</anchorfile>
      <anchor>ga6accccddbb867dfc2eece9ee3ffecb3a</anchor>
      <arglist></arglist>
    </member>
    <member kind="typedef">
      <type>struct FLAC__Metadata_Chain</type>
      <name>FLAC__Metadata_Chain</name>
      <anchorfile>group__flac__metadata__level2.html</anchorfile>
      <anchor>gaec6993c60b88f222a52af86f8f47bfdf</anchor>
      <arglist></arglist>
    </member>
    <member kind="typedef">
      <type>struct FLAC__Metadata_Iterator</type>
      <name>FLAC__Metadata_Iterator</name>
      <anchorfile>group__flac__metadata__level2.html</anchorfile>
      <anchor>ga9f3e135a07cdef7e51597646aa7b89b2</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumeration">
      <type></type>
      <name>FLAC__Metadata_SimpleIteratorStatus</name>
      <anchorfile>group__flac__metadata__level1.html</anchorfile>
      <anchor>gac926e7d2773a05066115cac9048bbec9</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__METADATA_SIMPLE_ITERATOR_STATUS_OK</name>
      <anchorfile>group__flac__metadata__level1.html</anchorfile>
      <anchor>ggac926e7d2773a05066115cac9048bbec9a33aadd73194c0d7e307d643237e0ddcd</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__METADATA_SIMPLE_ITERATOR_STATUS_ILLEGAL_INPUT</name>
      <anchorfile>group__flac__metadata__level1.html</anchorfile>
      <anchor>ggac926e7d2773a05066115cac9048bbec9a0a3933cb38c8957a8d5c3d1afb4766f9</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__METADATA_SIMPLE_ITERATOR_STATUS_ERROR_OPENING_FILE</name>
      <anchorfile>group__flac__metadata__level1.html</anchorfile>
      <anchor>ggac926e7d2773a05066115cac9048bbec9a20e835bbb74b4d039e598617f68d2af6</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__METADATA_SIMPLE_ITERATOR_STATUS_NOT_A_FLAC_FILE</name>
      <anchorfile>group__flac__metadata__level1.html</anchorfile>
      <anchor>ggac926e7d2773a05066115cac9048bbec9a7785f77a612be8956fbe7cab73497220</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__METADATA_SIMPLE_ITERATOR_STATUS_NOT_WRITABLE</name>
      <anchorfile>group__flac__metadata__level1.html</anchorfile>
      <anchor>ggac926e7d2773a05066115cac9048bbec9af055d8c0c663e72134fe2db8037b6880</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__METADATA_SIMPLE_ITERATOR_STATUS_BAD_METADATA</name>
      <anchorfile>group__flac__metadata__level1.html</anchorfile>
      <anchor>ggac926e7d2773a05066115cac9048bbec9a14c897124887858109200723826f85b7</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__METADATA_SIMPLE_ITERATOR_STATUS_READ_ERROR</name>
      <anchorfile>group__flac__metadata__level1.html</anchorfile>
      <anchor>ggac926e7d2773a05066115cac9048bbec9a088df964f0852dd7e19304e920c3ee8e</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__METADATA_SIMPLE_ITERATOR_STATUS_SEEK_ERROR</name>
      <anchorfile>group__flac__metadata__level1.html</anchorfile>
      <anchor>ggac926e7d2773a05066115cac9048bbec9a2ad85a32e291d1e918692d68cc22fd40</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__METADATA_SIMPLE_ITERATOR_STATUS_WRITE_ERROR</name>
      <anchorfile>group__flac__metadata__level1.html</anchorfile>
      <anchor>ggac926e7d2773a05066115cac9048bbec9ac2337299c2347ca311caeaa7d71d857c</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__METADATA_SIMPLE_ITERATOR_STATUS_RENAME_ERROR</name>
      <anchorfile>group__flac__metadata__level1.html</anchorfile>
      <anchor>ggac926e7d2773a05066115cac9048bbec9a2e073843fa99419d76a0b210da96ceb6</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__METADATA_SIMPLE_ITERATOR_STATUS_UNLINK_ERROR</name>
      <anchorfile>group__flac__metadata__level1.html</anchorfile>
      <anchor>ggac926e7d2773a05066115cac9048bbec9a4f855433038c576da127fc1de9d18f9b</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__METADATA_SIMPLE_ITERATOR_STATUS_MEMORY_ALLOCATION_ERROR</name>
      <anchorfile>group__flac__metadata__level1.html</anchorfile>
      <anchor>ggac926e7d2773a05066115cac9048bbec9aa8386ed0a20d7e91b0022d203ec3cdec</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__METADATA_SIMPLE_ITERATOR_STATUS_INTERNAL_ERROR</name>
      <anchorfile>group__flac__metadata__level1.html</anchorfile>
      <anchor>ggac926e7d2773a05066115cac9048bbec9a9d821ae65a1c5de619daa88c850906df</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumeration">
      <type></type>
      <name>FLAC__Metadata_ChainStatus</name>
      <anchorfile>group__flac__metadata__level2.html</anchorfile>
      <anchor>gafe2a924893b0800b020bea8160fd4531</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__METADATA_CHAIN_STATUS_OK</name>
      <anchorfile>group__flac__metadata__level2.html</anchorfile>
      <anchor>ggafe2a924893b0800b020bea8160fd4531a293be942ec54576f2b3c73613af968e9</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__METADATA_CHAIN_STATUS_ILLEGAL_INPUT</name>
      <anchorfile>group__flac__metadata__level2.html</anchorfile>
      <anchor>ggafe2a924893b0800b020bea8160fd4531a1be9400982f411173af46bf0c3acbdc7</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__METADATA_CHAIN_STATUS_ERROR_OPENING_FILE</name>
      <anchorfile>group__flac__metadata__level2.html</anchorfile>
      <anchor>ggafe2a924893b0800b020bea8160fd4531a43d2741a650576052fa3615d8cd64d86</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__METADATA_CHAIN_STATUS_NOT_A_FLAC_FILE</name>
      <anchorfile>group__flac__metadata__level2.html</anchorfile>
      <anchor>ggafe2a924893b0800b020bea8160fd4531a99748a4b12ed10f9368375cc8deeb143</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__METADATA_CHAIN_STATUS_NOT_WRITABLE</name>
      <anchorfile>group__flac__metadata__level2.html</anchorfile>
      <anchor>ggafe2a924893b0800b020bea8160fd4531ac469c6543ebb117e99064572c16672d4</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__METADATA_CHAIN_STATUS_BAD_METADATA</name>
      <anchorfile>group__flac__metadata__level2.html</anchorfile>
      <anchor>ggafe2a924893b0800b020bea8160fd4531a8efd2c76dc06308eb6eba59e1bc6300b</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__METADATA_CHAIN_STATUS_READ_ERROR</name>
      <anchorfile>group__flac__metadata__level2.html</anchorfile>
      <anchor>ggafe2a924893b0800b020bea8160fd4531a0525de5fb5d8aeeb4e848e33a8d503c6</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__METADATA_CHAIN_STATUS_SEEK_ERROR</name>
      <anchorfile>group__flac__metadata__level2.html</anchorfile>
      <anchor>ggafe2a924893b0800b020bea8160fd4531a5814bc26bcf92143198b8e7f028f43a2</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__METADATA_CHAIN_STATUS_WRITE_ERROR</name>
      <anchorfile>group__flac__metadata__level2.html</anchorfile>
      <anchor>ggafe2a924893b0800b020bea8160fd4531a66460c735e4745788b40889329e8489f</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__METADATA_CHAIN_STATUS_RENAME_ERROR</name>
      <anchorfile>group__flac__metadata__level2.html</anchorfile>
      <anchor>ggafe2a924893b0800b020bea8160fd4531af4ecf22bc3e5adf78a9c765f856efb0d</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__METADATA_CHAIN_STATUS_UNLINK_ERROR</name>
      <anchorfile>group__flac__metadata__level2.html</anchorfile>
      <anchor>ggafe2a924893b0800b020bea8160fd4531a1cd3138ed493f6a0f5b95fb8481edd1e</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__METADATA_CHAIN_STATUS_MEMORY_ALLOCATION_ERROR</name>
      <anchorfile>group__flac__metadata__level2.html</anchorfile>
      <anchor>ggafe2a924893b0800b020bea8160fd4531ab12ec938f7556a163c609194ee0aede0</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__METADATA_CHAIN_STATUS_INTERNAL_ERROR</name>
      <anchorfile>group__flac__metadata__level2.html</anchorfile>
      <anchor>ggafe2a924893b0800b020bea8160fd4531a36b9bcf93da8e0f111738a65eab36e9d</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__METADATA_CHAIN_STATUS_INVALID_CALLBACKS</name>
      <anchorfile>group__flac__metadata__level2.html</anchorfile>
      <anchor>ggafe2a924893b0800b020bea8160fd4531ab8a6aa5f115db3f07ad2ed4adbcbe060</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__METADATA_CHAIN_STATUS_READ_WRITE_MISMATCH</name>
      <anchorfile>group__flac__metadata__level2.html</anchorfile>
      <anchor>ggafe2a924893b0800b020bea8160fd4531a0d9e64ad6514c88b8ea9e9171c42ec9a</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__METADATA_CHAIN_STATUS_WRONG_WRITE_CALL</name>
      <anchorfile>group__flac__metadata__level2.html</anchorfile>
      <anchor>ggafe2a924893b0800b020bea8160fd4531af86670707345e2d02cc84aec059459d0</anchor>
      <arglist></arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_get_streaminfo</name>
      <anchorfile>group__flac__metadata__level0.html</anchorfile>
      <anchor>ga804b42d9da714199b4b383ce51078d51</anchor>
      <arglist>(const char *filename, FLAC__StreamMetadata *streaminfo)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_get_tags</name>
      <anchorfile>group__flac__metadata__level0.html</anchorfile>
      <anchor>ga1626af09cd39d4fa37d5b46ebe3790fd</anchor>
      <arglist>(const char *filename, FLAC__StreamMetadata **tags)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_get_cuesheet</name>
      <anchorfile>group__flac__metadata__level0.html</anchorfile>
      <anchor>ga0f47949dca514506718276205a4fae0b</anchor>
      <arglist>(const char *filename, FLAC__StreamMetadata **cuesheet)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_get_picture</name>
      <anchorfile>group__flac__metadata__level0.html</anchorfile>
      <anchor>ga0c9cd22296400c8ce16ee1db011342cb</anchor>
      <arglist>(const char *filename, FLAC__StreamMetadata **picture, FLAC__StreamMetadata_Picture_Type type, const char *mime_type, const FLAC__byte *description, unsigned max_width, unsigned max_height, unsigned max_depth, unsigned max_colors)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__Metadata_SimpleIterator *</type>
      <name>FLAC__metadata_simple_iterator_new</name>
      <anchorfile>group__flac__metadata__level1.html</anchorfile>
      <anchor>ga017ae86f3351888f50feb47026ed2482</anchor>
      <arglist>(void)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>FLAC__metadata_simple_iterator_delete</name>
      <anchorfile>group__flac__metadata__level1.html</anchorfile>
      <anchor>ga4619be06f51429fea71e5b98900cec3e</anchor>
      <arglist>(FLAC__Metadata_SimpleIterator *iterator)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__Metadata_SimpleIteratorStatus</type>
      <name>FLAC__metadata_simple_iterator_status</name>
      <anchorfile>group__flac__metadata__level1.html</anchorfile>
      <anchor>gae8fd236fe6049c61f7f3b4a6ecbcd240</anchor>
      <arglist>(FLAC__Metadata_SimpleIterator *iterator)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_simple_iterator_init</name>
      <anchorfile>group__flac__metadata__level1.html</anchorfile>
      <anchor>gaba8daf276fd7da863a2522ac050125fd</anchor>
      <arglist>(FLAC__Metadata_SimpleIterator *iterator, const char *filename, FLAC__bool read_only, FLAC__bool preserve_file_stats)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_simple_iterator_is_writable</name>
      <anchorfile>group__flac__metadata__level1.html</anchorfile>
      <anchor>ga5150ecd8668c610f79192a2838667790</anchor>
      <arglist>(const FLAC__Metadata_SimpleIterator *iterator)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_simple_iterator_next</name>
      <anchorfile>group__flac__metadata__level1.html</anchorfile>
      <anchor>gabb7de0a1067efae353e0792dc6e51905</anchor>
      <arglist>(FLAC__Metadata_SimpleIterator *iterator)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_simple_iterator_prev</name>
      <anchorfile>group__flac__metadata__level1.html</anchorfile>
      <anchor>ga6db5313b31120b28e210ae721d6525a8</anchor>
      <arglist>(FLAC__Metadata_SimpleIterator *iterator)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_simple_iterator_is_last</name>
      <anchorfile>group__flac__metadata__level1.html</anchorfile>
      <anchor>ga9eb215059840960de69aa84469ba954f</anchor>
      <arglist>(const FLAC__Metadata_SimpleIterator *iterator)</arglist>
    </member>
    <member kind="function">
      <type>off_t</type>
      <name>FLAC__metadata_simple_iterator_get_block_offset</name>
      <anchorfile>group__flac__metadata__level1.html</anchorfile>
      <anchor>gade0a61723420daeb4bc226713671c6f0</anchor>
      <arglist>(const FLAC__Metadata_SimpleIterator *iterator)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__MetadataType</type>
      <name>FLAC__metadata_simple_iterator_get_block_type</name>
      <anchorfile>group__flac__metadata__level1.html</anchorfile>
      <anchor>ga17b61d17e83432913abf4334d6e0c073</anchor>
      <arglist>(const FLAC__Metadata_SimpleIterator *iterator)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>FLAC__metadata_simple_iterator_get_block_length</name>
      <anchorfile>group__flac__metadata__level1.html</anchorfile>
      <anchor>ga93ec66e9cfb99f04ce4125b8be906cef</anchor>
      <arglist>(const FLAC__Metadata_SimpleIterator *iterator)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_simple_iterator_get_application_id</name>
      <anchorfile>group__flac__metadata__level1.html</anchorfile>
      <anchor>gad4fea2d7d98d16e75e6d8260f690a5dc</anchor>
      <arglist>(FLAC__Metadata_SimpleIterator *iterator, FLAC__byte *id)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__StreamMetadata *</type>
      <name>FLAC__metadata_simple_iterator_get_block</name>
      <anchorfile>group__flac__metadata__level1.html</anchorfile>
      <anchor>ga1b7374cafd886ceb880b050dfa1e387a</anchor>
      <arglist>(FLAC__Metadata_SimpleIterator *iterator)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_simple_iterator_set_block</name>
      <anchorfile>group__flac__metadata__level1.html</anchorfile>
      <anchor>gae1dd863561606658f88c492682de7b80</anchor>
      <arglist>(FLAC__Metadata_SimpleIterator *iterator, FLAC__StreamMetadata *block, FLAC__bool use_padding)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_simple_iterator_insert_block_after</name>
      <anchorfile>group__flac__metadata__level1.html</anchorfile>
      <anchor>ga7a0c00e93bb37324a20926e92e604102</anchor>
      <arglist>(FLAC__Metadata_SimpleIterator *iterator, FLAC__StreamMetadata *block, FLAC__bool use_padding)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_simple_iterator_delete_block</name>
      <anchorfile>group__flac__metadata__level1.html</anchorfile>
      <anchor>gac3116c8e6e7f59914ae22c0c4c6b0a23</anchor>
      <arglist>(FLAC__Metadata_SimpleIterator *iterator, FLAC__bool use_padding)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__Metadata_Chain *</type>
      <name>FLAC__metadata_chain_new</name>
      <anchorfile>group__flac__metadata__level2.html</anchorfile>
      <anchor>ga381a1b6efff8d4e9d793f1dda515bd73</anchor>
      <arglist>(void)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>FLAC__metadata_chain_delete</name>
      <anchorfile>group__flac__metadata__level2.html</anchorfile>
      <anchor>ga46b6c67f30db2955798dfb5556f63aa3</anchor>
      <arglist>(FLAC__Metadata_Chain *chain)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__Metadata_ChainStatus</type>
      <name>FLAC__metadata_chain_status</name>
      <anchorfile>group__flac__metadata__level2.html</anchorfile>
      <anchor>ga8e74773f8ca2bb2bc0b56a65ca0299f4</anchor>
      <arglist>(FLAC__Metadata_Chain *chain)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_chain_read</name>
      <anchorfile>group__flac__metadata__level2.html</anchorfile>
      <anchor>ga5a4f2056c30f78af5a79f6b64d5bfdcd</anchor>
      <arglist>(FLAC__Metadata_Chain *chain, const char *filename)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_chain_read_ogg</name>
      <anchorfile>group__flac__metadata__level2.html</anchorfile>
      <anchor>ga3995010aab28a483ad9905669e5c4954</anchor>
      <arglist>(FLAC__Metadata_Chain *chain, const char *filename)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_chain_read_with_callbacks</name>
      <anchorfile>group__flac__metadata__level2.html</anchorfile>
      <anchor>ga595f55b611ed588d4d55a9b2eb9d2add</anchor>
      <arglist>(FLAC__Metadata_Chain *chain, FLAC__IOHandle handle, FLAC__IOCallbacks callbacks)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_chain_read_ogg_with_callbacks</name>
      <anchorfile>group__flac__metadata__level2.html</anchorfile>
      <anchor>gaccc2f991722682d3c31d36f51985066c</anchor>
      <arglist>(FLAC__Metadata_Chain *chain, FLAC__IOHandle handle, FLAC__IOCallbacks callbacks)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_chain_check_if_tempfile_needed</name>
      <anchorfile>group__flac__metadata__level2.html</anchorfile>
      <anchor>ga46602f64d423cfe5d5f8a4155f8a97e2</anchor>
      <arglist>(FLAC__Metadata_Chain *chain, FLAC__bool use_padding)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_chain_write</name>
      <anchorfile>group__flac__metadata__level2.html</anchorfile>
      <anchor>ga46bf9cf7d426078101b9297ba80bb835</anchor>
      <arglist>(FLAC__Metadata_Chain *chain, FLAC__bool use_padding, FLAC__bool preserve_file_stats)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_chain_write_with_callbacks</name>
      <anchorfile>group__flac__metadata__level2.html</anchorfile>
      <anchor>ga70532b3705294dc891d8db649a4d4843</anchor>
      <arglist>(FLAC__Metadata_Chain *chain, FLAC__bool use_padding, FLAC__IOHandle handle, FLAC__IOCallbacks callbacks)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_chain_write_with_callbacks_and_tempfile</name>
      <anchorfile>group__flac__metadata__level2.html</anchorfile>
      <anchor>ga72facaa621e8d798036a4a7da3643e41</anchor>
      <arglist>(FLAC__Metadata_Chain *chain, FLAC__bool use_padding, FLAC__IOHandle handle, FLAC__IOCallbacks callbacks, FLAC__IOHandle temp_handle, FLAC__IOCallbacks temp_callbacks)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>FLAC__metadata_chain_merge_padding</name>
      <anchorfile>group__flac__metadata__level2.html</anchorfile>
      <anchor>ga0a43897914edb751cb87f7e281aff3dc</anchor>
      <arglist>(FLAC__Metadata_Chain *chain)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>FLAC__metadata_chain_sort_padding</name>
      <anchorfile>group__flac__metadata__level2.html</anchorfile>
      <anchor>ga82b66fe71c727adb9cf80a1da9834ce5</anchor>
      <arglist>(FLAC__Metadata_Chain *chain)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__Metadata_Iterator *</type>
      <name>FLAC__metadata_iterator_new</name>
      <anchorfile>group__flac__metadata__level2.html</anchorfile>
      <anchor>ga1941ca04671813fc039ea7fd35ae6461</anchor>
      <arglist>(void)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>FLAC__metadata_iterator_delete</name>
      <anchorfile>group__flac__metadata__level2.html</anchorfile>
      <anchor>ga374c246e1aeafd803d29a6e99b226241</anchor>
      <arglist>(FLAC__Metadata_Iterator *iterator)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>FLAC__metadata_iterator_init</name>
      <anchorfile>group__flac__metadata__level2.html</anchorfile>
      <anchor>ga2e93196b17a1c73e949e661e33d7311a</anchor>
      <arglist>(FLAC__Metadata_Iterator *iterator, FLAC__Metadata_Chain *chain)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_iterator_next</name>
      <anchorfile>group__flac__metadata__level2.html</anchorfile>
      <anchor>ga60449d0c1d76a73978159e3aa5e79459</anchor>
      <arglist>(FLAC__Metadata_Iterator *iterator)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_iterator_prev</name>
      <anchorfile>group__flac__metadata__level2.html</anchorfile>
      <anchor>gaa28df1c5aa56726f573f90e4bae2fe50</anchor>
      <arglist>(FLAC__Metadata_Iterator *iterator)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__MetadataType</type>
      <name>FLAC__metadata_iterator_get_block_type</name>
      <anchorfile>group__flac__metadata__level2.html</anchorfile>
      <anchor>ga83ecb59ffa16bfbb1e286e64f9270de1</anchor>
      <arglist>(const FLAC__Metadata_Iterator *iterator)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__StreamMetadata *</type>
      <name>FLAC__metadata_iterator_get_block</name>
      <anchorfile>group__flac__metadata__level2.html</anchorfile>
      <anchor>gad3e7fbc3b3d9c192a3ac425c7b263641</anchor>
      <arglist>(FLAC__Metadata_Iterator *iterator)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_iterator_set_block</name>
      <anchorfile>group__flac__metadata__level2.html</anchorfile>
      <anchor>gaf61795b21300a2b0c9940c11974aab53</anchor>
      <arglist>(FLAC__Metadata_Iterator *iterator, FLAC__StreamMetadata *block)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_iterator_delete_block</name>
      <anchorfile>group__flac__metadata__level2.html</anchorfile>
      <anchor>gadf860af967d2ee483be01fc0ed8767a9</anchor>
      <arglist>(FLAC__Metadata_Iterator *iterator, FLAC__bool replace_with_padding)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_iterator_insert_block_before</name>
      <anchorfile>group__flac__metadata__level2.html</anchorfile>
      <anchor>ga8ac45e2df8b6fd6f5db345c4293aa435</anchor>
      <arglist>(FLAC__Metadata_Iterator *iterator, FLAC__StreamMetadata *block)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_iterator_insert_block_after</name>
      <anchorfile>group__flac__metadata__level2.html</anchorfile>
      <anchor>ga55e53757f91696e2578196a2799fc632</anchor>
      <arglist>(FLAC__Metadata_Iterator *iterator, FLAC__StreamMetadata *block)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__StreamMetadata *</type>
      <name>FLAC__metadata_object_new</name>
      <anchorfile>group__flac__metadata__object.html</anchorfile>
      <anchor>ga5df7bc8c72cafed1391bdc5ffc876e0f</anchor>
      <arglist>(FLAC__MetadataType type)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__StreamMetadata *</type>
      <name>FLAC__metadata_object_clone</name>
      <anchorfile>group__flac__metadata__object.html</anchorfile>
      <anchor>ga29af0ecc2a015ef22289f206bc308d80</anchor>
      <arglist>(const FLAC__StreamMetadata *object)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>FLAC__metadata_object_delete</name>
      <anchorfile>group__flac__metadata__object.html</anchorfile>
      <anchor>ga6b3159744a1e5c4ce9d349fd0ebae800</anchor>
      <arglist>(FLAC__StreamMetadata *object)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_object_is_equal</name>
      <anchorfile>group__flac__metadata__object.html</anchorfile>
      <anchor>ga6853bcafe731b1db37105d49f3085349</anchor>
      <arglist>(const FLAC__StreamMetadata *block1, const FLAC__StreamMetadata *block2)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_object_application_set_data</name>
      <anchorfile>group__flac__metadata__object.html</anchorfile>
      <anchor>ga07cd39bbe12f4f4e144e38c5265b997d</anchor>
      <arglist>(FLAC__StreamMetadata *object, FLAC__byte *data, unsigned length, FLAC__bool copy)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_object_seektable_resize_points</name>
      <anchorfile>group__flac__metadata__object.html</anchorfile>
      <anchor>gaffa16bae5e3683c983dc137fd56f0c26</anchor>
      <arglist>(FLAC__StreamMetadata *object, unsigned new_num_points)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>FLAC__metadata_object_seektable_set_point</name>
      <anchorfile>group__flac__metadata__object.html</anchorfile>
      <anchor>gaa1a69eb95a3c17aa973466589e85f3c1</anchor>
      <arglist>(FLAC__StreamMetadata *object, unsigned point_num, FLAC__StreamMetadata_SeekPoint point)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_object_seektable_insert_point</name>
      <anchorfile>group__flac__metadata__object.html</anchorfile>
      <anchor>ga794109d40ff0065659c005f1cf86d3c9</anchor>
      <arglist>(FLAC__StreamMetadata *object, unsigned point_num, FLAC__StreamMetadata_SeekPoint point)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_object_seektable_delete_point</name>
      <anchorfile>group__flac__metadata__object.html</anchorfile>
      <anchor>ga8e42ac803f857eaa7814838c49a15c5f</anchor>
      <arglist>(FLAC__StreamMetadata *object, unsigned point_num)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_object_seektable_is_legal</name>
      <anchorfile>group__flac__metadata__object.html</anchorfile>
      <anchor>gacd3e1b83fabc1dabccb725b2876c8f53</anchor>
      <arglist>(const FLAC__StreamMetadata *object)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_object_seektable_template_append_placeholders</name>
      <anchorfile>group__flac__metadata__object.html</anchorfile>
      <anchor>ga6280327ed000ee85846a5533fd40a33b</anchor>
      <arglist>(FLAC__StreamMetadata *object, unsigned num)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_object_seektable_template_append_point</name>
      <anchorfile>group__flac__metadata__object.html</anchorfile>
      <anchor>ga0b3aca4fbebc206cd79f13ac36f653f0</anchor>
      <arglist>(FLAC__StreamMetadata *object, FLAC__uint64 sample_number)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_object_seektable_template_append_points</name>
      <anchorfile>group__flac__metadata__object.html</anchorfile>
      <anchor>gac838116fa0e48242651944ab94bab508</anchor>
      <arglist>(FLAC__StreamMetadata *object, FLAC__uint64 sample_numbers[], unsigned num)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_object_seektable_template_append_spaced_points</name>
      <anchorfile>group__flac__metadata__object.html</anchorfile>
      <anchor>ga7dcbd38a3a71a8aa26e93a6992a8f83e</anchor>
      <arglist>(FLAC__StreamMetadata *object, unsigned num, FLAC__uint64 total_samples)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_object_seektable_template_append_spaced_points_by_samples</name>
      <anchorfile>group__flac__metadata__object.html</anchorfile>
      <anchor>ga6bce5ee9332ea070d65482a2c1ce1c2d</anchor>
      <arglist>(FLAC__StreamMetadata *object, unsigned samples, FLAC__uint64 total_samples)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_object_seektable_template_sort</name>
      <anchorfile>group__flac__metadata__object.html</anchorfile>
      <anchor>gafb0449b639ba5c618826d893c2961260</anchor>
      <arglist>(FLAC__StreamMetadata *object, FLAC__bool compact)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_object_vorbiscomment_set_vendor_string</name>
      <anchorfile>group__flac__metadata__object.html</anchorfile>
      <anchor>ga5cf1a57afab200b4b67730a77d3ee162</anchor>
      <arglist>(FLAC__StreamMetadata *object, FLAC__StreamMetadata_VorbisComment_Entry entry, FLAC__bool copy)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_object_vorbiscomment_resize_comments</name>
      <anchorfile>group__flac__metadata__object.html</anchorfile>
      <anchor>ga829152404c9d160c7bc67699dd7f857e</anchor>
      <arglist>(FLAC__StreamMetadata *object, unsigned new_num_comments)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_object_vorbiscomment_set_comment</name>
      <anchorfile>group__flac__metadata__object.html</anchorfile>
      <anchor>gaefadb16fe0fff9600beab0edbac8d226</anchor>
      <arglist>(FLAC__StreamMetadata *object, unsigned comment_num, FLAC__StreamMetadata_VorbisComment_Entry entry, FLAC__bool copy)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_object_vorbiscomment_insert_comment</name>
      <anchorfile>group__flac__metadata__object.html</anchorfile>
      <anchor>ga44ec196a99c8cd7d1d50817c8532ddb3</anchor>
      <arglist>(FLAC__StreamMetadata *object, unsigned comment_num, FLAC__StreamMetadata_VorbisComment_Entry entry, FLAC__bool copy)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_object_vorbiscomment_append_comment</name>
      <anchorfile>group__flac__metadata__object.html</anchorfile>
      <anchor>ga889b8b9c5bbd1070a1214c3da8b72863</anchor>
      <arglist>(FLAC__StreamMetadata *object, FLAC__StreamMetadata_VorbisComment_Entry entry, FLAC__bool copy)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_object_vorbiscomment_replace_comment</name>
      <anchorfile>group__flac__metadata__object.html</anchorfile>
      <anchor>ga0608308e8c4c09aa610747d8dff90a34</anchor>
      <arglist>(FLAC__StreamMetadata *object, FLAC__StreamMetadata_VorbisComment_Entry entry, FLAC__bool all, FLAC__bool copy)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_object_vorbiscomment_delete_comment</name>
      <anchorfile>group__flac__metadata__object.html</anchorfile>
      <anchor>gafe14896322f7d638f5de0c61addd1dc7</anchor>
      <arglist>(FLAC__StreamMetadata *object, unsigned comment_num)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_object_vorbiscomment_entry_from_name_value_pair</name>
      <anchorfile>group__flac__metadata__object.html</anchorfile>
      <anchor>gab644c34515c04630c62a7645fab2947e</anchor>
      <arglist>(FLAC__StreamMetadata_VorbisComment_Entry *entry, const char *field_name, const char *field_value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_object_vorbiscomment_entry_to_name_value_pair</name>
      <anchorfile>group__flac__metadata__object.html</anchorfile>
      <anchor>ga29079764fabda53cb3e890e6d05c8345</anchor>
      <arglist>(const FLAC__StreamMetadata_VorbisComment_Entry entry, char **field_name, char **field_value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_object_vorbiscomment_entry_matches</name>
      <anchorfile>group__flac__metadata__object.html</anchorfile>
      <anchor>gac4c32b1055642b11cd77628ac9508ded</anchor>
      <arglist>(const FLAC__StreamMetadata_VorbisComment_Entry entry, const char *field_name, unsigned field_name_length)</arglist>
    </member>
    <member kind="function">
      <type>int</type>
      <name>FLAC__metadata_object_vorbiscomment_find_entry_from</name>
      <anchorfile>group__flac__metadata__object.html</anchorfile>
      <anchor>ga23d79d11e427e1590f406a7137c8bff2</anchor>
      <arglist>(const FLAC__StreamMetadata *object, unsigned offset, const char *field_name)</arglist>
    </member>
    <member kind="function">
      <type>int</type>
      <name>FLAC__metadata_object_vorbiscomment_remove_entry_matching</name>
      <anchorfile>group__flac__metadata__object.html</anchorfile>
      <anchor>ga017d743b3200a27b8567ef33592224b8</anchor>
      <arglist>(FLAC__StreamMetadata *object, const char *field_name)</arglist>
    </member>
    <member kind="function">
      <type>int</type>
      <name>FLAC__metadata_object_vorbiscomment_remove_entries_matching</name>
      <anchorfile>group__flac__metadata__object.html</anchorfile>
      <anchor>ga5a3ff5856098c449622ba850684aec75</anchor>
      <arglist>(FLAC__StreamMetadata *object, const char *field_name)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__StreamMetadata_CueSheet_Track *</type>
      <name>FLAC__metadata_object_cuesheet_track_new</name>
      <anchorfile>group__flac__metadata__object.html</anchorfile>
      <anchor>gafe2983a9c09685e34626cab39b3fb52c</anchor>
      <arglist>(void)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__StreamMetadata_CueSheet_Track *</type>
      <name>FLAC__metadata_object_cuesheet_track_clone</name>
      <anchorfile>group__flac__metadata__object.html</anchorfile>
      <anchor>ga1293d6df6daf2d65143d8bb40eed9261</anchor>
      <arglist>(const FLAC__StreamMetadata_CueSheet_Track *object)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>FLAC__metadata_object_cuesheet_track_delete</name>
      <anchorfile>group__flac__metadata__object.html</anchorfile>
      <anchor>gaa533fd7b72fa079e783de4b155b241ce</anchor>
      <arglist>(FLAC__StreamMetadata_CueSheet_Track *object)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_object_cuesheet_track_resize_indices</name>
      <anchorfile>group__flac__metadata__object.html</anchorfile>
      <anchor>ga484f21de7d533e4825cf807d29ef0204</anchor>
      <arglist>(FLAC__StreamMetadata *object, unsigned track_num, unsigned new_num_indices)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_object_cuesheet_track_insert_index</name>
      <anchorfile>group__flac__metadata__object.html</anchorfile>
      <anchor>gae8be6ad7b27a18c91eb0b91dc305e433</anchor>
      <arglist>(FLAC__StreamMetadata *object, unsigned track_num, unsigned index_num, FLAC__StreamMetadata_CueSheet_Index index)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_object_cuesheet_track_insert_blank_index</name>
      <anchorfile>group__flac__metadata__object.html</anchorfile>
      <anchor>gafd40ef6dcc277f99934deee5367cc627</anchor>
      <arglist>(FLAC__StreamMetadata *object, unsigned track_num, unsigned index_num)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_object_cuesheet_track_delete_index</name>
      <anchorfile>group__flac__metadata__object.html</anchorfile>
      <anchor>ga84584f2244b7d8597b8fec1d81ea5fb8</anchor>
      <arglist>(FLAC__StreamMetadata *object, unsigned track_num, unsigned index_num)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_object_cuesheet_resize_tracks</name>
      <anchorfile>group__flac__metadata__object.html</anchorfile>
      <anchor>gac99e38ed08f342665c63913bd0cc33fc</anchor>
      <arglist>(FLAC__StreamMetadata *object, unsigned new_num_tracks)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_object_cuesheet_set_track</name>
      <anchorfile>group__flac__metadata__object.html</anchorfile>
      <anchor>gaa4708c652be442b19227695621b62088</anchor>
      <arglist>(FLAC__StreamMetadata *object, unsigned track_num, FLAC__StreamMetadata_CueSheet_Track *track, FLAC__bool copy)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_object_cuesheet_insert_track</name>
      <anchorfile>group__flac__metadata__object.html</anchorfile>
      <anchor>gaa974947ae8ec1c86cbb155e0af7593e9</anchor>
      <arglist>(FLAC__StreamMetadata *object, unsigned track_num, FLAC__StreamMetadata_CueSheet_Track *track, FLAC__bool copy)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_object_cuesheet_insert_blank_track</name>
      <anchorfile>group__flac__metadata__object.html</anchorfile>
      <anchor>gab4a8c0855971e650df3331daf84d3fd1</anchor>
      <arglist>(FLAC__StreamMetadata *object, unsigned track_num)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_object_cuesheet_delete_track</name>
      <anchorfile>group__flac__metadata__object.html</anchorfile>
      <anchor>gae1912dfbc599c79732025fd5a5f279cc</anchor>
      <arglist>(FLAC__StreamMetadata *object, unsigned track_num)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_object_cuesheet_is_legal</name>
      <anchorfile>group__flac__metadata__object.html</anchorfile>
      <anchor>ga1a443d9299ce69694ad59bec4519d7b2</anchor>
      <arglist>(const FLAC__StreamMetadata *object, FLAC__bool check_cd_da_subset, const char **violation)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__uint32</type>
      <name>FLAC__metadata_object_cuesheet_calculate_cddb_id</name>
      <anchorfile>group__flac__metadata__object.html</anchorfile>
      <anchor>gaff2f825950b3e4dda4c8ddbf8e2f7ecd</anchor>
      <arglist>(const FLAC__StreamMetadata *object)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_object_picture_set_mime_type</name>
      <anchorfile>group__flac__metadata__object.html</anchorfile>
      <anchor>ga4511ae9ca994c9f4ab035a3c1aa98f45</anchor>
      <arglist>(FLAC__StreamMetadata *object, char *mime_type, FLAC__bool copy)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_object_picture_set_description</name>
      <anchorfile>group__flac__metadata__object.html</anchorfile>
      <anchor>ga293fe7d8b8b9e49d2414db0925b0f442</anchor>
      <arglist>(FLAC__StreamMetadata *object, FLAC__byte *description, FLAC__bool copy)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_object_picture_set_data</name>
      <anchorfile>group__flac__metadata__object.html</anchorfile>
      <anchor>ga00c330534ef8336ed92b30f9e676bb5f</anchor>
      <arglist>(FLAC__StreamMetadata *object, FLAC__byte *data, FLAC__uint32 length, FLAC__bool copy)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_object_picture_is_legal</name>
      <anchorfile>group__flac__metadata__object.html</anchorfile>
      <anchor>ga88268a5186e37d4b98b4df7870561128</anchor>
      <arglist>(const FLAC__StreamMetadata *object, const char **violation)</arglist>
    </member>
    <member kind="variable">
      <type>const char *const</type>
      <name>FLAC__Metadata_SimpleIteratorStatusString</name>
      <anchorfile>group__flac__metadata__level1.html</anchorfile>
      <anchor>gaa2a8b972800c34f9f5807cadf6ecdb57</anchor>
      <arglist>[]</arglist>
    </member>
    <member kind="variable">
      <type>const char *const</type>
      <name>FLAC__Metadata_ChainStatusString</name>
      <anchorfile>group__flac__metadata__level2.html</anchorfile>
      <anchor>ga6498d1976b0d9fa3f8f6295c02e622dd</anchor>
      <arglist>[]</arglist>
    </member>
  </compound>
  <compound kind="file">
    <name>metadata.h</name>
    <path>/home/erikd/Git/flac/include/FLAC++/</path>
    <filename>_09_2metadata_8h</filename>
    <includes id="_09_2export_8h" name="export.h" local="yes" imported="no">export.h</includes>
    <includes id="metadata_8h" name="metadata.h" local="yes" imported="no">FLAC/metadata.h</includes>
    <class kind="class">FLAC::Metadata::Prototype</class>
    <class kind="class">FLAC::Metadata::StreamInfo</class>
    <class kind="class">FLAC::Metadata::Padding</class>
    <class kind="class">FLAC::Metadata::Application</class>
    <class kind="class">FLAC::Metadata::SeekTable</class>
    <class kind="class">FLAC::Metadata::VorbisComment</class>
    <class kind="class">FLAC::Metadata::VorbisComment::Entry</class>
    <class kind="class">FLAC::Metadata::CueSheet</class>
    <class kind="class">FLAC::Metadata::CueSheet::Track</class>
    <class kind="class">FLAC::Metadata::Picture</class>
    <class kind="class">FLAC::Metadata::Unknown</class>
    <class kind="class">FLAC::Metadata::SimpleIterator</class>
    <class kind="class">FLAC::Metadata::SimpleIterator::Status</class>
    <class kind="class">FLAC::Metadata::Chain</class>
    <class kind="class">FLAC::Metadata::Chain::Status</class>
    <class kind="class">FLAC::Metadata::Iterator</class>
    <member kind="function">
      <type>Prototype *</type>
      <name>clone</name>
      <anchorfile>group__flacpp__metadata__object.html</anchorfile>
      <anchor>gae18d91726a320349b2c3fb45e79d21fc</anchor>
      <arglist>(const Prototype *)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>get_streaminfo</name>
      <anchorfile>group__flacpp__metadata__level0.html</anchorfile>
      <anchor>ga8fa8da652f33edeb4dabb4ce39fda04b</anchor>
      <arglist>(const char *filename, StreamInfo &amp;streaminfo)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>get_tags</name>
      <anchorfile>group__flacpp__metadata__level0.html</anchorfile>
      <anchor>ga533a71ba745ca03068523a4a45fb0329</anchor>
      <arglist>(const char *filename, VorbisComment *&amp;tags)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>get_tags</name>
      <anchorfile>group__flacpp__metadata__level0.html</anchorfile>
      <anchor>ga85166e6206f3d5635684de4257f2b00e</anchor>
      <arglist>(const char *filename, VorbisComment &amp;tags)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>get_cuesheet</name>
      <anchorfile>group__flacpp__metadata__level0.html</anchorfile>
      <anchor>ga4fad03d91f22d78acf35dd2f35df9ac7</anchor>
      <arglist>(const char *filename, CueSheet *&amp;cuesheet)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>get_cuesheet</name>
      <anchorfile>group__flacpp__metadata__level0.html</anchorfile>
      <anchor>gaea8f05f89e36af143d73b4280f05cc0e</anchor>
      <arglist>(const char *filename, CueSheet &amp;cuesheet)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>get_picture</name>
      <anchorfile>group__flacpp__metadata__level0.html</anchorfile>
      <anchor>ga2ca454c644cb6548b05545c129e4d6ef</anchor>
      <arglist>(const char *filename, Picture *&amp;picture,::FLAC__StreamMetadata_Picture_Type type, const char *mime_type, const FLAC__byte *description, unsigned max_width, unsigned max_height, unsigned max_depth, unsigned max_colors)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>get_picture</name>
      <anchorfile>group__flacpp__metadata__level0.html</anchorfile>
      <anchor>ga82705f1c0ac6d36c0a508dc33e5e7181</anchor>
      <arglist>(const char *filename, Picture &amp;picture,::FLAC__StreamMetadata_Picture_Type type, const char *mime_type, const FLAC__byte *description, unsigned max_width, unsigned max_height, unsigned max_depth, unsigned max_colors)</arglist>
    </member>
  </compound>
  <compound kind="file">
    <name>stream_decoder.h</name>
    <path>/home/erikd/Git/flac/include/FLAC/</path>
    <filename>stream__decoder_8h</filename>
    <includes id="export_8h" name="export.h" local="yes" imported="no">export.h</includes>
    <includes id="format_8h" name="format.h" local="yes" imported="no">format.h</includes>
    <class kind="struct">FLAC__StreamDecoder</class>
    <member kind="typedef">
      <type>FLAC__StreamDecoderReadStatus(*</type>
      <name>FLAC__StreamDecoderReadCallback</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>ga7a5f593b9bc2d163884348b48c4285fd</anchor>
      <arglist>)(const FLAC__StreamDecoder *decoder, FLAC__byte buffer[], size_t *bytes, void *client_data)</arglist>
    </member>
    <member kind="typedef">
      <type>FLAC__StreamDecoderSeekStatus(*</type>
      <name>FLAC__StreamDecoderSeekCallback</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>ga375614289a1b868f1ead7fa70a581171</anchor>
      <arglist>)(const FLAC__StreamDecoder *decoder, FLAC__uint64 absolute_byte_offset, void *client_data)</arglist>
    </member>
    <member kind="typedef">
      <type>FLAC__StreamDecoderTellStatus(*</type>
      <name>FLAC__StreamDecoderTellCallback</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>ga02990309a9d30acc43ba01fe48021e39</anchor>
      <arglist>)(const FLAC__StreamDecoder *decoder, FLAC__uint64 *absolute_byte_offset, void *client_data)</arglist>
    </member>
    <member kind="typedef">
      <type>FLAC__StreamDecoderLengthStatus(*</type>
      <name>FLAC__StreamDecoderLengthCallback</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>ga453ffb5215a522fb74dc61d694e1453c</anchor>
      <arglist>)(const FLAC__StreamDecoder *decoder, FLAC__uint64 *stream_length, void *client_data)</arglist>
    </member>
    <member kind="typedef">
      <type>FLAC__bool(*</type>
      <name>FLAC__StreamDecoderEofCallback</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>gacc214f6b3cdae1c0f98577267ce19bdd</anchor>
      <arglist>)(const FLAC__StreamDecoder *decoder, void *client_data)</arglist>
    </member>
    <member kind="typedef">
      <type>FLAC__StreamDecoderWriteStatus(*</type>
      <name>FLAC__StreamDecoderWriteCallback</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>gaf98a4f9e2cac5747da6018c3dfc8dde1</anchor>
      <arglist>)(const FLAC__StreamDecoder *decoder, const FLAC__Frame *frame, const FLAC__int32 *const buffer[], void *client_data)</arglist>
    </member>
    <member kind="typedef">
      <type>void(*</type>
      <name>FLAC__StreamDecoderMetadataCallback</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>ga43e2329c15731c002ac4182a47990f85</anchor>
      <arglist>)(const FLAC__StreamDecoder *decoder, const FLAC__StreamMetadata *metadata, void *client_data)</arglist>
    </member>
    <member kind="typedef">
      <type>void(*</type>
      <name>FLAC__StreamDecoderErrorCallback</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>ga4fab6730ff0b22bf45ca4cd04d706569</anchor>
      <arglist>)(const FLAC__StreamDecoder *decoder, FLAC__StreamDecoderErrorStatus status, void *client_data)</arglist>
    </member>
    <member kind="enumeration">
      <type></type>
      <name>FLAC__StreamDecoderState</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>ga3adb6891c5871a87cd5bbae6c770ba2d</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_DECODER_SEARCH_FOR_METADATA</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>gga3adb6891c5871a87cd5bbae6c770ba2dacf4455f4f681a6737a553e10f614704a</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_DECODER_READ_METADATA</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>gga3adb6891c5871a87cd5bbae6c770ba2da4c1853ed1babdcede9a908e12cf7ccf7</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_DECODER_SEARCH_FOR_FRAME_SYNC</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>gga3adb6891c5871a87cd5bbae6c770ba2daccff915757978117720ba1613d088ddf</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_DECODER_READ_FRAME</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>gga3adb6891c5871a87cd5bbae6c770ba2da06dc6158a51a8eb9537b65f2fbb6dc49</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_DECODER_END_OF_STREAM</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>gga3adb6891c5871a87cd5bbae6c770ba2da28ce845052d9d1a780f4107e97f4c853</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_DECODER_OGG_ERROR</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>gga3adb6891c5871a87cd5bbae6c770ba2da3bc0343f47153c5779baf7f37f6e95cf</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_DECODER_SEEK_ERROR</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>gga3adb6891c5871a87cd5bbae6c770ba2daf2c6efcabdfe889081c2260e6681db49</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_DECODER_ABORTED</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>gga3adb6891c5871a87cd5bbae6c770ba2dadb52ab4785bd2eb84a95e8aa82311cd5</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_DECODER_MEMORY_ALLOCATION_ERROR</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>gga3adb6891c5871a87cd5bbae6c770ba2da0d08c527252420813e6a6d6d3e19324a</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_DECODER_UNINITIALIZED</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>gga3adb6891c5871a87cd5bbae6c770ba2da565eaf4d5e68b440ecec771cb22d3427</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumeration">
      <type></type>
      <name>FLAC__StreamDecoderInitStatus</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>gaaed54a24ac6310d29c5cafba79759c44</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_DECODER_INIT_STATUS_OK</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>ggaaed54a24ac6310d29c5cafba79759c44ac94c7e9396f30642f34805e5d626e011</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_DECODER_INIT_STATUS_UNSUPPORTED_CONTAINER</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>ggaaed54a24ac6310d29c5cafba79759c44a8f2188c616c9bc09638eece3ae55f152</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_DECODER_INIT_STATUS_INVALID_CALLBACKS</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>ggaaed54a24ac6310d29c5cafba79759c44a798ad4b6c4e556fd4cb1afbc29562eca</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_DECODER_INIT_STATUS_MEMORY_ALLOCATION_ERROR</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>ggaaed54a24ac6310d29c5cafba79759c44a0110567f0715c6f87357388bc7fa98f9</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_DECODER_INIT_STATUS_ERROR_OPENING_FILE</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>ggaaed54a24ac6310d29c5cafba79759c44a8184c306e0cd2565a8c5adc1381cb469</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_DECODER_INIT_STATUS_ALREADY_INITIALIZED</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>ggaaed54a24ac6310d29c5cafba79759c44a98bc501c9b2fb5d92d8bb0b3321d504f</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumeration">
      <type></type>
      <name>FLAC__StreamDecoderReadStatus</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>gad793ead451206c64a91dc0b851027b93</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_DECODER_READ_STATUS_CONTINUE</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>ggad793ead451206c64a91dc0b851027b93a9a5be0fcf0279b98b2fd462bc4871d06</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_DECODER_READ_STATUS_END_OF_STREAM</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>ggad793ead451206c64a91dc0b851027b93a0a0687d25dc9f7163e6e5e294672170f</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_DECODER_READ_STATUS_ABORT</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>ggad793ead451206c64a91dc0b851027b93a923123aebb349e35662e35a7621b7535</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumeration">
      <type></type>
      <name>FLAC__StreamDecoderSeekStatus</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>gac8d269e3c7af1a5889d3bd38409ed67d</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_DECODER_SEEK_STATUS_OK</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>ggac8d269e3c7af1a5889d3bd38409ed67daca58132d896ad7755827d3f2b72488cc</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_DECODER_SEEK_STATUS_ERROR</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>ggac8d269e3c7af1a5889d3bd38409ed67da969ce92a42a2a95609452e9cf01fcc09</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_DECODER_SEEK_STATUS_UNSUPPORTED</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>ggac8d269e3c7af1a5889d3bd38409ed67da4a01f1e48baf015e78535cc20683ec53</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumeration">
      <type></type>
      <name>FLAC__StreamDecoderTellStatus</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>ga83708207969383bd7b5c1e9148528845</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_DECODER_TELL_STATUS_OK</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>gga83708207969383bd7b5c1e9148528845a516a202ebf4bb61d4a1fb5b029a104dd</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_DECODER_TELL_STATUS_ERROR</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>gga83708207969383bd7b5c1e9148528845aceefd3feb853d5e68a149f2bdd1a9db1</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_DECODER_TELL_STATUS_UNSUPPORTED</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>gga83708207969383bd7b5c1e9148528845add75538234493c9f7a20a846a223ca91</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumeration">
      <type></type>
      <name>FLAC__StreamDecoderLengthStatus</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>gad5860157c2bb34501b8b9370472d727a</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_DECODER_LENGTH_STATUS_OK</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>ggad5860157c2bb34501b8b9370472d727aaef01bfcdc3099686e106d8f88397653d</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_DECODER_LENGTH_STATUS_ERROR</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>ggad5860157c2bb34501b8b9370472d727aab000e31c0c20c0d19df4f2203b01ea23</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_DECODER_LENGTH_STATUS_UNSUPPORTED</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>ggad5860157c2bb34501b8b9370472d727aae35949f46f887e6d826fe0fe4b2a32c1</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumeration">
      <type></type>
      <name>FLAC__StreamDecoderWriteStatus</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>ga73f67eb9e0ab57945afe038751bc62c8</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_DECODER_WRITE_STATUS_CONTINUE</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>gga73f67eb9e0ab57945afe038751bc62c8acea48326e0ab8370d2814f4126fcb84e</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_DECODER_WRITE_STATUS_ABORT</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>gga73f67eb9e0ab57945afe038751bc62c8a23bd6bfec34af704e0d5ea273f14d95d</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumeration">
      <type></type>
      <name>FLAC__StreamDecoderErrorStatus</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>ga130e70bd9a73d3c2416247a3e5132ecf</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_DECODER_ERROR_STATUS_LOST_SYNC</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>gga130e70bd9a73d3c2416247a3e5132ecfa3ceec2a553dc142ad487ae88eb6f7222</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_DECODER_ERROR_STATUS_BAD_HEADER</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>gga130e70bd9a73d3c2416247a3e5132ecfae393a9b91a6b2f23398675b5b57e1e86</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_DECODER_ERROR_STATUS_FRAME_CRC_MISMATCH</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>gga130e70bd9a73d3c2416247a3e5132ecfa208fe77a04e6ff684e50f0eae1214e26</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_DECODER_ERROR_STATUS_UNPARSEABLE_STREAM</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>gga130e70bd9a73d3c2416247a3e5132ecfa8b6864ad65edd8fea039838b6d3e5575</anchor>
      <arglist></arglist>
    </member>
    <member kind="function">
      <type>FLAC__StreamDecoder *</type>
      <name>FLAC__stream_decoder_new</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>ga529c3c1e46417570767fb8e4c76f5477</anchor>
      <arglist>(void)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>FLAC__stream_decoder_delete</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>gad9cf299956da091111d13e83517d8c44</anchor>
      <arglist>(FLAC__StreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_decoder_set_ogg_serial_number</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>ga7fd232e7a2b5070bd26450487edbc2a1</anchor>
      <arglist>(FLAC__StreamDecoder *decoder, long serial_number)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_decoder_set_md5_checking</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>ga8f402243eed54f400ddd2f296ff54497</anchor>
      <arglist>(FLAC__StreamDecoder *decoder, FLAC__bool value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_decoder_set_metadata_respond</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>gad4e685f3d055f70fbaed9ffa4f70f74b</anchor>
      <arglist>(FLAC__StreamDecoder *decoder, FLAC__MetadataType type)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_decoder_set_metadata_respond_application</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>gaee1196ff5fa97df9810f708dc2bc8326</anchor>
      <arglist>(FLAC__StreamDecoder *decoder, const FLAC__byte id[4])</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_decoder_set_metadata_respond_all</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>ga1ce03d8f305a818ff9a573473af99dc4</anchor>
      <arglist>(FLAC__StreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_decoder_set_metadata_ignore</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>gad75f067720da89c4e9d96dedc45f73e6</anchor>
      <arglist>(FLAC__StreamDecoder *decoder, FLAC__MetadataType type)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_decoder_set_metadata_ignore_application</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>gaab41e8bc505b24df4912de53de06b085</anchor>
      <arglist>(FLAC__StreamDecoder *decoder, const FLAC__byte id[4])</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_decoder_set_metadata_ignore_all</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>gaa1307f07fae5d7a4a0c18beeae7ec5e6</anchor>
      <arglist>(FLAC__StreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__StreamDecoderState</type>
      <name>FLAC__stream_decoder_get_state</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>gaf99dac2d9255f7db4df8a6d9974a9a9a</anchor>
      <arglist>(const FLAC__StreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>const char *</type>
      <name>FLAC__stream_decoder_get_resolved_state_string</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>gad28257412951ca266751a19e2cf54be2</anchor>
      <arglist>(const FLAC__StreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_decoder_get_md5_checking</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>gae27a6b30b55beda03559c12a5df21537</anchor>
      <arglist>(const FLAC__StreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__uint64</type>
      <name>FLAC__stream_decoder_get_total_samples</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>ga930d9b591fcfaea74359c722cdfb980c</anchor>
      <arglist>(const FLAC__StreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>FLAC__stream_decoder_get_channels</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>gadefff696d65e72afeb2f058e45066be1</anchor>
      <arglist>(const FLAC__StreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__ChannelAssignment</type>
      <name>FLAC__stream_decoder_get_channel_assignment</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>gae62fdf93c1fedd5fea9258ecdc78bb53</anchor>
      <arglist>(const FLAC__StreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>FLAC__stream_decoder_get_bits_per_sample</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>ga51dc941b24bfe01f8311c9aba86e77f8</anchor>
      <arglist>(const FLAC__StreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>FLAC__stream_decoder_get_sample_rate</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>ga1b0170d020b47c1b841ce8437b564429</anchor>
      <arglist>(const FLAC__StreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>FLAC__stream_decoder_get_blocksize</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>ga568a710a87dbd051b0edba6b53ae52ee</anchor>
      <arglist>(const FLAC__StreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_decoder_get_decode_position</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>gaffd9b0d0832ed01e6d75930b5391def5</anchor>
      <arglist>(const FLAC__StreamDecoder *decoder, FLAC__uint64 *position)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__StreamDecoderInitStatus</type>
      <name>FLAC__stream_decoder_init_stream</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>ga150d381abc5249168e439bc076544b29</anchor>
      <arglist>(FLAC__StreamDecoder *decoder, FLAC__StreamDecoderReadCallback read_callback, FLAC__StreamDecoderSeekCallback seek_callback, FLAC__StreamDecoderTellCallback tell_callback, FLAC__StreamDecoderLengthCallback length_callback, FLAC__StreamDecoderEofCallback eof_callback, FLAC__StreamDecoderWriteCallback write_callback, FLAC__StreamDecoderMetadataCallback metadata_callback, FLAC__StreamDecoderErrorCallback error_callback, void *client_data)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__StreamDecoderInitStatus</type>
      <name>FLAC__stream_decoder_init_ogg_stream</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>ga1b043adeb805c779c1e97cb68959d1ab</anchor>
      <arglist>(FLAC__StreamDecoder *decoder, FLAC__StreamDecoderReadCallback read_callback, FLAC__StreamDecoderSeekCallback seek_callback, FLAC__StreamDecoderTellCallback tell_callback, FLAC__StreamDecoderLengthCallback length_callback, FLAC__StreamDecoderEofCallback eof_callback, FLAC__StreamDecoderWriteCallback write_callback, FLAC__StreamDecoderMetadataCallback metadata_callback, FLAC__StreamDecoderErrorCallback error_callback, void *client_data)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__StreamDecoderInitStatus</type>
      <name>FLAC__stream_decoder_init_FILE</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>ga80aa83631460a53263c84e654586dff0</anchor>
      <arglist>(FLAC__StreamDecoder *decoder, FILE *file, FLAC__StreamDecoderWriteCallback write_callback, FLAC__StreamDecoderMetadataCallback metadata_callback, FLAC__StreamDecoderErrorCallback error_callback, void *client_data)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__StreamDecoderInitStatus</type>
      <name>FLAC__stream_decoder_init_ogg_FILE</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>ga4cc7fbaf905c24d6db48b53b7942fe72</anchor>
      <arglist>(FLAC__StreamDecoder *decoder, FILE *file, FLAC__StreamDecoderWriteCallback write_callback, FLAC__StreamDecoderMetadataCallback metadata_callback, FLAC__StreamDecoderErrorCallback error_callback, void *client_data)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__StreamDecoderInitStatus</type>
      <name>FLAC__stream_decoder_init_file</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>ga4021ead5cff29fd589c915756f902f1a</anchor>
      <arglist>(FLAC__StreamDecoder *decoder, const char *filename, FLAC__StreamDecoderWriteCallback write_callback, FLAC__StreamDecoderMetadataCallback metadata_callback, FLAC__StreamDecoderErrorCallback error_callback, void *client_data)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__StreamDecoderInitStatus</type>
      <name>FLAC__stream_decoder_init_ogg_file</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>ga548f15d7724f3bff7f2608abe8b12f6c</anchor>
      <arglist>(FLAC__StreamDecoder *decoder, const char *filename, FLAC__StreamDecoderWriteCallback write_callback, FLAC__StreamDecoderMetadataCallback metadata_callback, FLAC__StreamDecoderErrorCallback error_callback, void *client_data)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_decoder_finish</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>ga96c47c96920f363cd0972b54067818a9</anchor>
      <arglist>(FLAC__StreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_decoder_flush</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>ga95570a455e582b2ab46ab9bb529f26ac</anchor>
      <arglist>(FLAC__StreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_decoder_reset</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>gaa4183c2d925d5a5edddde9d1ca145725</anchor>
      <arglist>(FLAC__StreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_decoder_process_single</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>ga9d6df4a39892c05955122cf7f987f856</anchor>
      <arglist>(FLAC__StreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_decoder_process_until_end_of_metadata</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>ga027ffb5b75dc39b3d26f55c5e6b42682</anchor>
      <arglist>(FLAC__StreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_decoder_process_until_end_of_stream</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>ga89a0723812fa6ef7cdb173715f1bc81f</anchor>
      <arglist>(FLAC__StreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_decoder_skip_single_frame</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>ga85b666aba976f29e8dd9d7956fce4301</anchor>
      <arglist>(FLAC__StreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_decoder_seek_absolute</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>ga6a2eb6072b9fafefc3f80f1959805ccb</anchor>
      <arglist>(FLAC__StreamDecoder *decoder, FLAC__uint64 sample)</arglist>
    </member>
    <member kind="variable">
      <type>const char *const</type>
      <name>FLAC__StreamDecoderStateString</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>gac192360ac435614394bf43235cb7981e</anchor>
      <arglist>[]</arglist>
    </member>
    <member kind="variable">
      <type>const char *const</type>
      <name>FLAC__StreamDecoderInitStatusString</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>ga0effa1d3031c3206a1719faf984a4f21</anchor>
      <arglist>[]</arglist>
    </member>
    <member kind="variable">
      <type>const char *const</type>
      <name>FLAC__StreamDecoderReadStatusString</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>gab1ee941839b05045ae1d73ee0fdcb8c9</anchor>
      <arglist>[]</arglist>
    </member>
    <member kind="variable">
      <type>const char *const</type>
      <name>FLAC__StreamDecoderSeekStatusString</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>gac49aff0593584b7ed5fd0b2508f824fc</anchor>
      <arglist>[]</arglist>
    </member>
    <member kind="variable">
      <type>const char *const</type>
      <name>FLAC__StreamDecoderTellStatusString</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>ga3c1b7d5a174d6c2e6bcf1b9a87b5a5cb</anchor>
      <arglist>[]</arglist>
    </member>
    <member kind="variable">
      <type>const char *const</type>
      <name>FLAC__StreamDecoderLengthStatusString</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>ga792933fa9e8b65bfcac62d82e52415f5</anchor>
      <arglist>[]</arglist>
    </member>
    <member kind="variable">
      <type>const char *const</type>
      <name>FLAC__StreamDecoderWriteStatusString</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>ga9df7f0fd8cf9888f97a52b5f3f33cdb0</anchor>
      <arglist>[]</arglist>
    </member>
    <member kind="variable">
      <type>const char *const</type>
      <name>FLAC__StreamDecoderErrorStatusString</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>gac428c69b084529322df05ee793440b88</anchor>
      <arglist>[]</arglist>
    </member>
  </compound>
  <compound kind="file">
    <name>stream_encoder.h</name>
    <path>/home/erikd/Git/flac/include/FLAC/</path>
    <filename>stream__encoder_8h</filename>
    <includes id="export_8h" name="export.h" local="yes" imported="no">export.h</includes>
    <includes id="format_8h" name="format.h" local="yes" imported="no">format.h</includes>
    <includes id="stream__decoder_8h" name="stream_decoder.h" local="yes" imported="no">stream_decoder.h</includes>
    <class kind="struct">FLAC__StreamEncoder</class>
    <member kind="typedef">
      <type>FLAC__StreamEncoderReadStatus(*</type>
      <name>FLAC__StreamEncoderReadCallback</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ga97d25c75f49897422d93a9d8405043cd</anchor>
      <arglist>)(const FLAC__StreamEncoder *encoder, FLAC__byte buffer[], size_t *bytes, void *client_data)</arglist>
    </member>
    <member kind="typedef">
      <type>FLAC__StreamEncoderWriteStatus(*</type>
      <name>FLAC__StreamEncoderWriteCallback</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>gabf2f9bb39c806111c83dd16936ff6d09</anchor>
      <arglist>)(const FLAC__StreamEncoder *encoder, const FLAC__byte buffer[], size_t bytes, unsigned samples, unsigned current_frame, void *client_data)</arglist>
    </member>
    <member kind="typedef">
      <type>FLAC__StreamEncoderSeekStatus(*</type>
      <name>FLAC__StreamEncoderSeekCallback</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ga3005a69a7883da53262ec8a124d48c9e</anchor>
      <arglist>)(const FLAC__StreamEncoder *encoder, FLAC__uint64 absolute_byte_offset, void *client_data)</arglist>
    </member>
    <member kind="typedef">
      <type>FLAC__StreamEncoderTellStatus(*</type>
      <name>FLAC__StreamEncoderTellCallback</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ga4cab0b7556d8509a9f74693804c8c86e</anchor>
      <arglist>)(const FLAC__StreamEncoder *encoder, FLAC__uint64 *absolute_byte_offset, void *client_data)</arglist>
    </member>
    <member kind="typedef">
      <type>void(*</type>
      <name>FLAC__StreamEncoderMetadataCallback</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ga87778e16cdd0834a301ee8d8258cf946</anchor>
      <arglist>)(const FLAC__StreamEncoder *encoder, const FLAC__StreamMetadata *metadata, void *client_data)</arglist>
    </member>
    <member kind="typedef">
      <type>void(*</type>
      <name>FLAC__StreamEncoderProgressCallback</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>gac65f8ae0583b665933744b60fd5ba0d9</anchor>
      <arglist>)(const FLAC__StreamEncoder *encoder, FLAC__uint64 bytes_written, FLAC__uint64 samples_written, unsigned frames_written, unsigned total_frames_estimate, void *client_data)</arglist>
    </member>
    <member kind="enumeration">
      <type></type>
      <name>FLAC__StreamEncoderState</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>gac5e9db4fc32ca2fa74abd9c8a87c02a5</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_ENCODER_OK</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ggac5e9db4fc32ca2fa74abd9c8a87c02a5a3a6666ae61a64d955341cec285695bf6</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_ENCODER_UNINITIALIZED</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ggac5e9db4fc32ca2fa74abd9c8a87c02a5a04912e04a3c57d3c53de34742f96d635</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_ENCODER_OGG_ERROR</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ggac5e9db4fc32ca2fa74abd9c8a87c02a5abb312cc8318c7a541cadacd23ceb3bbb</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_ENCODER_VERIFY_DECODER_ERROR</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ggac5e9db4fc32ca2fa74abd9c8a87c02a5a4cb80be4f83eb71f04e74968af1d259e</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_ENCODER_VERIFY_MISMATCH_IN_AUDIO_DATA</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ggac5e9db4fc32ca2fa74abd9c8a87c02a5a011e3d8b2d02a940bfd0e59c05cf5ae0</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_ENCODER_CLIENT_ERROR</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ggac5e9db4fc32ca2fa74abd9c8a87c02a5a8c2b2e9efb43a4f9b25b1d2bd9af5f23</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_ENCODER_IO_ERROR</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ggac5e9db4fc32ca2fa74abd9c8a87c02a5af0e4738522e05a7248435c7148f58f91</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_ENCODER_FRAMING_ERROR</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ggac5e9db4fc32ca2fa74abd9c8a87c02a5a2c2937b7f1600a4ac7c84fc70ab34cf1</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_ENCODER_MEMORY_ALLOCATION_ERROR</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ggac5e9db4fc32ca2fa74abd9c8a87c02a5a35db99d9958bd6c2301a04715fbc44fd</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumeration">
      <type></type>
      <name>FLAC__StreamEncoderInitStatus</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ga3bb869620af2b188d77982a5c30b047d</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_ENCODER_INIT_STATUS_OK</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>gga3bb869620af2b188d77982a5c30b047da20501dce552da74c5df935eeaa0c9ee3</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_ENCODER_INIT_STATUS_ENCODER_ERROR</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>gga3bb869620af2b188d77982a5c30b047da9c64e5f9020d8799e1cd9d39d50e6955</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_ENCODER_INIT_STATUS_UNSUPPORTED_CONTAINER</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>gga3bb869620af2b188d77982a5c30b047da8a822b011de88b67c114505ffef39327</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_ENCODER_INIT_STATUS_INVALID_CALLBACKS</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>gga3bb869620af2b188d77982a5c30b047dac2cf461f02e20513003b8cadeae03f9f</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_ENCODER_INIT_STATUS_INVALID_NUMBER_OF_CHANNELS</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>gga3bb869620af2b188d77982a5c30b047da0541c4f827f081b9f1c54c9441e4aa65</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_ENCODER_INIT_STATUS_INVALID_BITS_PER_SAMPLE</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>gga3bb869620af2b188d77982a5c30b047dad6d2631f464183c0c165155200882e6b</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_ENCODER_INIT_STATUS_INVALID_SAMPLE_RATE</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>gga3bb869620af2b188d77982a5c30b047da6fdcde9e18c37450c79e8f12b9d9c134</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_ENCODER_INIT_STATUS_INVALID_BLOCK_SIZE</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>gga3bb869620af2b188d77982a5c30b047da652c445f1bd8b6cfb963a30bf416c95a</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_ENCODER_INIT_STATUS_INVALID_MAX_LPC_ORDER</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>gga3bb869620af2b188d77982a5c30b047da38a69e94b3333e4ba779d2ff8f43f64e</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_ENCODER_INIT_STATUS_INVALID_QLP_COEFF_PRECISION</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>gga3bb869620af2b188d77982a5c30b047da5be80403bd7a43450139442e0f34ad7e</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_ENCODER_INIT_STATUS_BLOCK_SIZE_TOO_SMALL_FOR_LPC_ORDER</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>gga3bb869620af2b188d77982a5c30b047da62a17a3ed3c05ddf8ea7f6fecbd4e4a1</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_ENCODER_INIT_STATUS_NOT_STREAMABLE</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>gga3bb869620af2b188d77982a5c30b047daa793405c858c7606539082750080a47e</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_ENCODER_INIT_STATUS_INVALID_METADATA</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>gga3bb869620af2b188d77982a5c30b047daa85afdd1849c75a19594416cef63e3e9</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_ENCODER_INIT_STATUS_ALREADY_INITIALIZED</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>gga3bb869620af2b188d77982a5c30b047dab4e7b50d176a127575df90383cb15e1d</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumeration">
      <type></type>
      <name>FLAC__StreamEncoderReadStatus</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ga2e81f007fb0a7414c0bbb453f37ea37f</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_ENCODER_READ_STATUS_CONTINUE</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>gga2e81f007fb0a7414c0bbb453f37ea37fa4bdd691d3666f19ec96ff99402347a2e</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_ENCODER_READ_STATUS_END_OF_STREAM</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>gga2e81f007fb0a7414c0bbb453f37ea37fa562fef84bf86a9a39682e23066d9cfee</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_ENCODER_READ_STATUS_ABORT</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>gga2e81f007fb0a7414c0bbb453f37ea37fa69b94eeab60e07d5fd33f2b3c8b85759</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_ENCODER_READ_STATUS_UNSUPPORTED</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>gga2e81f007fb0a7414c0bbb453f37ea37fa9bb730b8f6354cc1e810017a2f700316</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumeration">
      <type></type>
      <name>FLAC__StreamEncoderWriteStatus</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ga3737471fd49730bb8cf9b182bdeda05e</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_ENCODER_WRITE_STATUS_OK</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>gga3737471fd49730bb8cf9b182bdeda05ea5622e0199f0203c402fcb7b4ca76f808</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_ENCODER_WRITE_STATUS_FATAL_ERROR</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>gga3737471fd49730bb8cf9b182bdeda05ea18e7cd6a443fb8bd303c3ba89946bc85</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumeration">
      <type></type>
      <name>FLAC__StreamEncoderSeekStatus</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ga6d5be3489f45fcf0c252022c65d87aca</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_ENCODER_SEEK_STATUS_OK</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>gga6d5be3489f45fcf0c252022c65d87acaa99853066610d798627888ec2e5afa667</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_ENCODER_SEEK_STATUS_ERROR</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>gga6d5be3489f45fcf0c252022c65d87acaabf93227938b4e1bf3656fe4ba4159c60</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_ENCODER_SEEK_STATUS_UNSUPPORTED</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>gga6d5be3489f45fcf0c252022c65d87acaa8930179a426134caf30a70147448f037</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumeration">
      <type></type>
      <name>FLAC__StreamEncoderTellStatus</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>gab628f63181250eb977a28bf12b7dd9ff</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_ENCODER_TELL_STATUS_OK</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ggab628f63181250eb977a28bf12b7dd9ffa48e071d89494ac8f5471e7c0d7a6f43b</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_ENCODER_TELL_STATUS_ERROR</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ggab628f63181250eb977a28bf12b7dd9ffaf638882e04d7c58e6c29dcc7f410864b</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_ENCODER_TELL_STATUS_UNSUPPORTED</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ggab628f63181250eb977a28bf12b7dd9ffa9d6bbd317f85fd2d6fc72f64e3cb56e7</anchor>
      <arglist></arglist>
    </member>
    <member kind="function">
      <type>FLAC__StreamEncoder *</type>
      <name>FLAC__stream_encoder_new</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>gab09f7620a0ba9c30020c189ce112a52f</anchor>
      <arglist>(void)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>FLAC__stream_encoder_delete</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ga7212e6846f543618b6289666de216b29</anchor>
      <arglist>(FLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_set_ogg_serial_number</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>gaf4f75f7689b6b3fff16b03028aa38326</anchor>
      <arglist>(FLAC__StreamEncoder *encoder, long serial_number)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_set_verify</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ga795be6527a9eb1219331afef2f182a41</anchor>
      <arglist>(FLAC__StreamEncoder *encoder, FLAC__bool value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_set_streamable_subset</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ga35a18815a58141b88db02317892d059b</anchor>
      <arglist>(FLAC__StreamEncoder *encoder, FLAC__bool value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_set_channels</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>gab151c81577dc385196b10727ec24459d</anchor>
      <arglist>(FLAC__StreamEncoder *encoder, unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_set_bits_per_sample</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>gabfa3c989377785cda7c496b69dcb98cb</anchor>
      <arglist>(FLAC__StreamEncoder *encoder, unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_set_sample_rate</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ga07fc8c7806381a055a1eef26387e509f</anchor>
      <arglist>(FLAC__StreamEncoder *encoder, unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_set_compression_level</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>gae49cf32f5256cb47eecd33779493ac85</anchor>
      <arglist>(FLAC__StreamEncoder *encoder, unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_set_blocksize</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ga3f2d344e090d0f6df5dfe8825c28bd61</anchor>
      <arglist>(FLAC__StreamEncoder *encoder, unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_set_do_mid_side_stereo</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ga3bff001a1efc2e4eb520c954066330f4</anchor>
      <arglist>(FLAC__StreamEncoder *encoder, FLAC__bool value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_set_loose_mid_side_stereo</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ga7965d51b93f14cbd6ad5bb9d34f10536</anchor>
      <arglist>(FLAC__StreamEncoder *encoder, FLAC__bool value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_set_apodization</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ga6598f09ac782a1f2a5743ddf247c81c8</anchor>
      <arglist>(FLAC__StreamEncoder *encoder, const char *specification)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_set_max_lpc_order</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>gac256b4ef560fcf099c02eb52bb6c30e9</anchor>
      <arglist>(FLAC__StreamEncoder *encoder, unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_set_qlp_coeff_precision</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>gaa325049ebb02d6fbe820d2268850c6de</anchor>
      <arglist>(FLAC__StreamEncoder *encoder, unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_set_do_qlp_coeff_prec_search</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ga495890067203958e5d67a641f8757b1c</anchor>
      <arglist>(FLAC__StreamEncoder *encoder, FLAC__bool value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_set_do_escape_coding</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>gaed594c373d829f77808a935c54a25fa4</anchor>
      <arglist>(FLAC__StreamEncoder *encoder, FLAC__bool value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_set_do_exhaustive_model_search</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ga054313e7f6eaf5c6122d82c6a8b3b808</anchor>
      <arglist>(FLAC__StreamEncoder *encoder, FLAC__bool value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_set_min_residual_partition_order</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ga31867a9cf8bc7276942381e4a8145969</anchor>
      <arglist>(FLAC__StreamEncoder *encoder, unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_set_max_residual_partition_order</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>gac2e2147be6e4edf68e02d011349fa08c</anchor>
      <arglist>(FLAC__StreamEncoder *encoder, unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_set_rice_parameter_search_dist</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ga11f0c589113b17507c0a620b7872036c</anchor>
      <arglist>(FLAC__StreamEncoder *encoder, unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_set_total_samples_estimate</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>gab943094585d1c0a4bec497e73567cf85</anchor>
      <arglist>(FLAC__StreamEncoder *encoder, FLAC__uint64 value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_set_metadata</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ga9c1098e664d7997947493901ed869b64</anchor>
      <arglist>(FLAC__StreamEncoder *encoder, FLAC__StreamMetadata **metadata, unsigned num_blocks)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__StreamEncoderState</type>
      <name>FLAC__stream_encoder_get_state</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ga0803321b37189dc5eea4fe1cea25c29a</anchor>
      <arglist>(const FLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__StreamDecoderState</type>
      <name>FLAC__stream_encoder_get_verify_decoder_state</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ga820704b95a711e77d55363e8753f9f9f</anchor>
      <arglist>(const FLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>const char *</type>
      <name>FLAC__stream_encoder_get_resolved_state_string</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ga0916f813358eb6f1e44148353acd4d42</anchor>
      <arglist>(const FLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>FLAC__stream_encoder_get_verify_decoder_error_stats</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ga48197fd6507314858222a8e6903292ba</anchor>
      <arglist>(const FLAC__StreamEncoder *encoder, FLAC__uint64 *absolute_sample, unsigned *frame_number, unsigned *channel, unsigned *sample, FLAC__int32 *expected, FLAC__int32 *got)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_get_verify</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ga9efc4964992e001bcec0a8eaedee8d60</anchor>
      <arglist>(const FLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_get_streamable_subset</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ga201e64032ea4298b2379c93652b28245</anchor>
      <arglist>(const FLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>FLAC__stream_encoder_get_channels</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ga535991ba463573d7ad2271ea943051e8</anchor>
      <arglist>(const FLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>FLAC__stream_encoder_get_bits_per_sample</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ga44afb2b8e2623b1dff02e4116c6c58cb</anchor>
      <arglist>(const FLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>FLAC__stream_encoder_get_sample_rate</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ga830bc5489b60fbe814a683a8c4ebc81d</anchor>
      <arglist>(const FLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>FLAC__stream_encoder_get_blocksize</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ga21da057fd9c30bc2475b9bd18b9a1504</anchor>
      <arglist>(const FLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_get_do_mid_side_stereo</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ga32da1f89997ab94ce5d677fcd7e24d56</anchor>
      <arglist>(const FLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_get_loose_mid_side_stereo</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ga1455859cf3d233bd4dfff86af010f4fa</anchor>
      <arglist>(const FLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>FLAC__stream_encoder_get_max_lpc_order</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>gadce7c7d6af89c3bbac0bb0c76d6e257b</anchor>
      <arglist>(const FLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>FLAC__stream_encoder_get_qlp_coeff_precision</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ga5b311992ac47554ae90c8f5a8ed745a7</anchor>
      <arglist>(const FLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_get_do_qlp_coeff_prec_search</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ga65bee5a769d4c5fdc95b81c2fb95061c</anchor>
      <arglist>(const FLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_get_do_escape_coding</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ga0c944049800991422c1bfb3b1c0567a5</anchor>
      <arglist>(const FLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_get_do_exhaustive_model_search</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ga7bc8b32f58df5564db4b6114cb11042d</anchor>
      <arglist>(const FLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>FLAC__stream_encoder_get_min_residual_partition_order</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ga1f29fb94e5c54f1d46ad31c2af0dc5ac</anchor>
      <arglist>(const FLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>FLAC__stream_encoder_get_max_residual_partition_order</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ga55773d82163302c52936299068510e9d</anchor>
      <arglist>(const FLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>FLAC__stream_encoder_get_rice_parameter_search_dist</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>gafd70690deb118136ca4baed350d99cb8</anchor>
      <arglist>(const FLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__uint64</type>
      <name>FLAC__stream_encoder_get_total_samples_estimate</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>gaa22d8935bd985b9cccf6592160ffc6f2</anchor>
      <arglist>(const FLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__StreamEncoderInitStatus</type>
      <name>FLAC__stream_encoder_init_stream</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ga7d801879812b48fcbc40f409800c453c</anchor>
      <arglist>(FLAC__StreamEncoder *encoder, FLAC__StreamEncoderWriteCallback write_callback, FLAC__StreamEncoderSeekCallback seek_callback, FLAC__StreamEncoderTellCallback tell_callback, FLAC__StreamEncoderMetadataCallback metadata_callback, void *client_data)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__StreamEncoderInitStatus</type>
      <name>FLAC__stream_encoder_init_ogg_stream</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ga9d1981bcd30b8db4d73b5466be5570f5</anchor>
      <arglist>(FLAC__StreamEncoder *encoder, FLAC__StreamEncoderReadCallback read_callback, FLAC__StreamEncoderWriteCallback write_callback, FLAC__StreamEncoderSeekCallback seek_callback, FLAC__StreamEncoderTellCallback tell_callback, FLAC__StreamEncoderMetadataCallback metadata_callback, void *client_data)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__StreamEncoderInitStatus</type>
      <name>FLAC__stream_encoder_init_FILE</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ga12789a1c4a4e31cd2e7187259fe127f8</anchor>
      <arglist>(FLAC__StreamEncoder *encoder, FILE *file, FLAC__StreamEncoderProgressCallback progress_callback, void *client_data)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__StreamEncoderInitStatus</type>
      <name>FLAC__stream_encoder_init_ogg_FILE</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ga57fc668f50ffd99a93df326bfab5e2b1</anchor>
      <arglist>(FLAC__StreamEncoder *encoder, FILE *file, FLAC__StreamEncoderProgressCallback progress_callback, void *client_data)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__StreamEncoderInitStatus</type>
      <name>FLAC__stream_encoder_init_file</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ga9d5117c2ac0eeb572784116bf2eb541b</anchor>
      <arglist>(FLAC__StreamEncoder *encoder, const char *filename, FLAC__StreamEncoderProgressCallback progress_callback, void *client_data)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__StreamEncoderInitStatus</type>
      <name>FLAC__stream_encoder_init_ogg_file</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ga4891de2f56045941ae222b61b0fd83a4</anchor>
      <arglist>(FLAC__StreamEncoder *encoder, const char *filename, FLAC__StreamEncoderProgressCallback progress_callback, void *client_data)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_finish</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ga3522f9de5af29807df1b9780a418b7f3</anchor>
      <arglist>(FLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_process</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>gad462406ff4d5a985eac6b4aa5472df57</anchor>
      <arglist>(FLAC__StreamEncoder *encoder, const FLAC__int32 *const buffer[], unsigned samples)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_process_interleaved</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ga4c56bc287efb1a0f69b117a84401baad</anchor>
      <arglist>(FLAC__StreamEncoder *encoder, const FLAC__int32 buffer[], unsigned samples)</arglist>
    </member>
    <member kind="variable">
      <type>const char *const</type>
      <name>FLAC__StreamEncoderStateString</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ga1410b7a076b0c8401682f9f812b66df5</anchor>
      <arglist>[]</arglist>
    </member>
    <member kind="variable">
      <type>const char *const</type>
      <name>FLAC__StreamEncoderInitStatusString</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ga0ec1fa7b3f55b4f07a2727846c285776</anchor>
      <arglist>[]</arglist>
    </member>
    <member kind="variable">
      <type>const char *const</type>
      <name>FLAC__StreamEncoderReadStatusString</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ga1654422c81846b9b399ac5fb98df61dd</anchor>
      <arglist>[]</arglist>
    </member>
    <member kind="variable">
      <type>const char *const</type>
      <name>FLAC__StreamEncoderWriteStatusString</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ga9f64480accd01525cbfa25c11e6bb74e</anchor>
      <arglist>[]</arglist>
    </member>
    <member kind="variable">
      <type>const char *const</type>
      <name>FLAC__StreamEncoderSeekStatusString</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>gabb137b2d787756bf97398f0b60e54c20</anchor>
      <arglist>[]</arglist>
    </member>
    <member kind="variable">
      <type>const char *const</type>
      <name>FLAC__StreamEncoderTellStatusString</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>gaf8ab921ae968be2be255be1f136e1eec</anchor>
      <arglist>[]</arglist>
    </member>
  </compound>
  <compound kind="group">
    <name>porting</name>
    <title>Porting Guide for New Versions</title>
    <filename>group__porting.html</filename>
    <subgroup>porting_1_1_2_to_1_1_3</subgroup>
    <subgroup>porting_1_1_3_to_1_1_4</subgroup>
    <subgroup>porting_1_1_4_to_1_2_0</subgroup>
  </compound>
  <compound kind="group">
    <name>porting_1_1_2_to_1_1_3</name>
    <title>Porting from FLAC 1.1.2 to 1.1.3</title>
    <filename>group__porting__1__1__2__to__1__1__3.html</filename>
  </compound>
  <compound kind="group">
    <name>porting_1_1_3_to_1_1_4</name>
    <title>Porting from FLAC 1.1.3 to 1.1.4</title>
    <filename>group__porting__1__1__3__to__1__1__4.html</filename>
  </compound>
  <compound kind="group">
    <name>porting_1_1_4_to_1_2_0</name>
    <title>Porting from FLAC 1.1.4 to 1.2.0</title>
    <filename>group__porting__1__1__4__to__1__2__0.html</filename>
  </compound>
  <compound kind="group">
    <name>flac</name>
    <title>FLAC C API</title>
    <filename>group__flac.html</filename>
    <subgroup>flac_callbacks</subgroup>
    <subgroup>flac_export</subgroup>
    <subgroup>flac_format</subgroup>
    <subgroup>flac_metadata</subgroup>
    <subgroup>flac_decoder</subgroup>
    <subgroup>flac_encoder</subgroup>
  </compound>
  <compound kind="group">
    <name>flac_callbacks</name>
    <title>FLAC/callback.h: I/O callback structures</title>
    <filename>group__flac__callbacks.html</filename>
    <class kind="struct">FLAC__IOCallbacks</class>
    <member kind="typedef">
      <type>void *</type>
      <name>FLAC__IOHandle</name>
      <anchorfile>group__flac__callbacks.html</anchorfile>
      <anchor>ga4c329c3168dee6e352384c5e9306260d</anchor>
      <arglist></arglist>
    </member>
    <member kind="typedef">
      <type>size_t(*</type>
      <name>FLAC__IOCallback_Read</name>
      <anchorfile>group__flac__callbacks.html</anchorfile>
      <anchor>ga5d75bd553bd041db0d30d869455bab38</anchor>
      <arglist>)(void *ptr, size_t size, size_t nmemb, FLAC__IOHandle handle)</arglist>
    </member>
    <member kind="typedef">
      <type>size_t(*</type>
      <name>FLAC__IOCallback_Write</name>
      <anchorfile>group__flac__callbacks.html</anchorfile>
      <anchor>ga473cbe45055fe15c967de13181d01e87</anchor>
      <arglist>)(const void *ptr, size_t size, size_t nmemb, FLAC__IOHandle handle)</arglist>
    </member>
    <member kind="typedef">
      <type>int(*</type>
      <name>FLAC__IOCallback_Seek</name>
      <anchorfile>group__flac__callbacks.html</anchorfile>
      <anchor>ga5090a35604829d8178f4f2d1a40a38d0</anchor>
      <arglist>)(FLAC__IOHandle handle, FLAC__int64 offset, int whence)</arglist>
    </member>
    <member kind="typedef">
      <type>FLAC__int64(*</type>
      <name>FLAC__IOCallback_Tell</name>
      <anchorfile>group__flac__callbacks.html</anchorfile>
      <anchor>ga016e3dd2d006975ea1031970dac232fb</anchor>
      <arglist>)(FLAC__IOHandle handle)</arglist>
    </member>
    <member kind="typedef">
      <type>int(*</type>
      <name>FLAC__IOCallback_Eof</name>
      <anchorfile>group__flac__callbacks.html</anchorfile>
      <anchor>gac9fe1afad0426a8f1756d136b3442990</anchor>
      <arglist>)(FLAC__IOHandle handle)</arglist>
    </member>
    <member kind="typedef">
      <type>int(*</type>
      <name>FLAC__IOCallback_Close</name>
      <anchorfile>group__flac__callbacks.html</anchorfile>
      <anchor>ga3749f8621f43e937eb69a9536db3b19a</anchor>
      <arglist>)(FLAC__IOHandle handle)</arglist>
    </member>
  </compound>
  <compound kind="group">
    <name>flac_export</name>
    <title>FLAC/export.h: export symbols</title>
    <filename>group__flac__export.html</filename>
    <member kind="define">
      <type>#define</type>
      <name>FLAC_API_VERSION_CURRENT</name>
      <anchorfile>group__flac__export.html</anchorfile>
      <anchor>ga31180fe15eea416cd8957cfca1a4c4f8</anchor>
      <arglist></arglist>
    </member>
    <member kind="define">
      <type>#define</type>
      <name>FLAC_API_VERSION_REVISION</name>
      <anchorfile>group__flac__export.html</anchorfile>
      <anchor>ga811641dd9f8c542d9260240e7fbe8e93</anchor>
      <arglist></arglist>
    </member>
    <member kind="define">
      <type>#define</type>
      <name>FLAC_API_VERSION_AGE</name>
      <anchorfile>group__flac__export.html</anchorfile>
      <anchor>ga1add3e09c8dfd57e8c921f299f0bbec1</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>int</type>
      <name>FLAC_API_SUPPORTS_OGG_FLAC</name>
      <anchorfile>group__flac__export.html</anchorfile>
      <anchor>ga84ffcb0af1038c60eb3e21fd002093cf</anchor>
      <arglist></arglist>
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
    <class kind="struct">FLAC__StreamMetadata_CueSheet_Index</class>
    <class kind="struct">FLAC__StreamMetadata_CueSheet_Track</class>
    <class kind="struct">FLAC__StreamMetadata_CueSheet</class>
    <class kind="struct">FLAC__StreamMetadata_Picture</class>
    <class kind="struct">FLAC__StreamMetadata_Unknown</class>
    <class kind="struct">FLAC__StreamMetadata</class>
    <member kind="define">
      <type>#define</type>
      <name>FLAC__MAX_METADATA_TYPE_CODE</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga626a412545818c2271fa2202c02ff1d6</anchor>
      <arglist></arglist>
    </member>
    <member kind="define">
      <type>#define</type>
      <name>FLAC__MIN_BLOCK_SIZE</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gaa5a85c2ea434221ce684be3469517003</anchor>
      <arglist></arglist>
    </member>
    <member kind="define">
      <type>#define</type>
      <name>FLAC__MAX_BLOCK_SIZE</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gaef78bc1b04f721e7b4563381f5514e8d</anchor>
      <arglist></arglist>
    </member>
    <member kind="define">
      <type>#define</type>
      <name>FLAC__SUBSET_MAX_BLOCK_SIZE_48000HZ</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga8f6ba2c28fbfcf52326d115c95b0a751</anchor>
      <arglist></arglist>
    </member>
    <member kind="define">
      <type>#define</type>
      <name>FLAC__MAX_CHANNELS</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga488aa5678a58d08f984f5d39185b763d</anchor>
      <arglist></arglist>
    </member>
    <member kind="define">
      <type>#define</type>
      <name>FLAC__MIN_BITS_PER_SAMPLE</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga30b0f21abbb2cdfd461fe04b425b5438</anchor>
      <arglist></arglist>
    </member>
    <member kind="define">
      <type>#define</type>
      <name>FLAC__MAX_BITS_PER_SAMPLE</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gad0156d56751e80241fa349d1e25064a6</anchor>
      <arglist></arglist>
    </member>
    <member kind="define">
      <type>#define</type>
      <name>FLAC__REFERENCE_CODEC_MAX_BITS_PER_SAMPLE</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga0fc418d96053d385fd2f56dce8007fbc</anchor>
      <arglist></arglist>
    </member>
    <member kind="define">
      <type>#define</type>
      <name>FLAC__MAX_SAMPLE_RATE</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga99abeef0c05c6bc76eacfa865abbfa70</anchor>
      <arglist></arglist>
    </member>
    <member kind="define">
      <type>#define</type>
      <name>FLAC__MAX_LPC_ORDER</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga16108d413f524329f338cff6e05f3aff</anchor>
      <arglist></arglist>
    </member>
    <member kind="define">
      <type>#define</type>
      <name>FLAC__SUBSET_MAX_LPC_ORDER_48000HZ</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga9791efa78147196820c86a6041d7774d</anchor>
      <arglist></arglist>
    </member>
    <member kind="define">
      <type>#define</type>
      <name>FLAC__MIN_QLP_COEFF_PRECISION</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gaf52033b2950b9396dd92b167b3bbe4db</anchor>
      <arglist></arglist>
    </member>
    <member kind="define">
      <type>#define</type>
      <name>FLAC__MAX_QLP_COEFF_PRECISION</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga6aa38a4bc5b9d96a78253ccb8b08bd1f</anchor>
      <arglist></arglist>
    </member>
    <member kind="define">
      <type>#define</type>
      <name>FLAC__MAX_FIXED_ORDER</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gabd0d5d6fe71b337244712b244ae7cb0f</anchor>
      <arglist></arglist>
    </member>
    <member kind="define">
      <type>#define</type>
      <name>FLAC__MAX_RICE_PARTITION_ORDER</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga78a2e97e230b2aa7f99edc94a466f5bb</anchor>
      <arglist></arglist>
    </member>
    <member kind="define">
      <type>#define</type>
      <name>FLAC__SUBSET_MAX_RICE_PARTITION_ORDER</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gab19dec1b56de482ccfeb5f9843f60a14</anchor>
      <arglist></arglist>
    </member>
    <member kind="define">
      <type>#define</type>
      <name>FLAC__STREAM_SYNC_LENGTH</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gae7ddaf298d3ceb83aae6301908675c1d</anchor>
      <arglist></arglist>
    </member>
    <member kind="define">
      <type>#define</type>
      <name>FLAC__STREAM_METADATA_STREAMINFO_LENGTH</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga06dfae7260da40e4c5f8fc4d531b326c</anchor>
      <arglist></arglist>
    </member>
    <member kind="define">
      <type>#define</type>
      <name>FLAC__STREAM_METADATA_SEEKPOINT_LENGTH</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gabdf85aa2c9a483378dfe850b85ab93ef</anchor>
      <arglist></arglist>
    </member>
    <member kind="define">
      <type>#define</type>
      <name>FLAC__STREAM_METADATA_HEADER_LENGTH</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga706a29b8a14902c457783bfd4fd7bab2</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumeration">
      <type></type>
      <name>FLAC__EntropyCodingMethodType</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga951733d2ea01943514290012cd622d3a</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__ENTROPY_CODING_METHOD_PARTITIONED_RICE</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gga951733d2ea01943514290012cd622d3aa5253f8b8edc61220739f229a299775dd</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__ENTROPY_CODING_METHOD_PARTITIONED_RICE2</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gga951733d2ea01943514290012cd622d3aa202960a608ee91f9f11c2575b9ecc5aa</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumeration">
      <type></type>
      <name>FLAC__SubframeType</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga1f431eaf213e74d7747589932d263348</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__SUBFRAME_TYPE_CONSTANT</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gga1f431eaf213e74d7747589932d263348a9bf56d836aeffb11d614e29ea1cdf2a9</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__SUBFRAME_TYPE_VERBATIM</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gga1f431eaf213e74d7747589932d263348a8520596ef07d6c8577f07025f137657b</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__SUBFRAME_TYPE_FIXED</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gga1f431eaf213e74d7747589932d263348a6b3cce73039a513f9afefdc8e4f664a5</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__SUBFRAME_TYPE_LPC</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gga1f431eaf213e74d7747589932d263348a31437462c3e4c3a5a214a91eff8cc3af</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumeration">
      <type></type>
      <name>FLAC__ChannelAssignment</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga79855f8525672e37f299bbe02952ef9c</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__CHANNEL_ASSIGNMENT_INDEPENDENT</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gga79855f8525672e37f299bbe02952ef9ca3c554e4c8512c2de31dfd3305f8b31b3</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__CHANNEL_ASSIGNMENT_LEFT_SIDE</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gga79855f8525672e37f299bbe02952ef9ca28d41295b20593561dc9934cc977d5cb</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__CHANNEL_ASSIGNMENT_RIGHT_SIDE</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gga79855f8525672e37f299bbe02952ef9cad155b61582140b2b90362005f1a93e2e</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__CHANNEL_ASSIGNMENT_MID_SIDE</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gga79855f8525672e37f299bbe02952ef9ca85c1512c0473b5ede364a9943759a80c</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumeration">
      <type></type>
      <name>FLAC__FrameNumberType</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga8fe9ebc78386cd2a3d23b7b8e3818e1c</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__FRAME_NUMBER_TYPE_FRAME_NUMBER</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gga8fe9ebc78386cd2a3d23b7b8e3818e1ca0b9cbf3853f0ae105cf9b5360164f794</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__FRAME_NUMBER_TYPE_SAMPLE_NUMBER</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gga8fe9ebc78386cd2a3d23b7b8e3818e1ca9220ce93dcc151e5edd5db7e7155b35a</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumeration">
      <type></type>
      <name>FLAC__MetadataType</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gac71714ba8ddbbd66d26bb78a427fac01</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__METADATA_TYPE_STREAMINFO</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ggac71714ba8ddbbd66d26bb78a427fac01acffa517e969ba6a868dcf10e5da75c28</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__METADATA_TYPE_PADDING</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ggac71714ba8ddbbd66d26bb78a427fac01a6dcb741fc0aef389580f110e88beb896</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__METADATA_TYPE_APPLICATION</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ggac71714ba8ddbbd66d26bb78a427fac01a2b287a22a1ac9440b309127884c8d41b</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__METADATA_TYPE_SEEKTABLE</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ggac71714ba8ddbbd66d26bb78a427fac01a5f6323e489be1318f0e3747960ebdd91</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__METADATA_TYPE_VORBIS_COMMENT</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ggac71714ba8ddbbd66d26bb78a427fac01ad013576bc5196b907547739518605520</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__METADATA_TYPE_CUESHEET</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ggac71714ba8ddbbd66d26bb78a427fac01a0b3f07ae60609126562cd0233ce00a65</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__METADATA_TYPE_PICTURE</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ggac71714ba8ddbbd66d26bb78a427fac01acf28ae2788366617c1aeab81d5961c6e</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__METADATA_TYPE_UNDEFINED</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ggac71714ba8ddbbd66d26bb78a427fac01acf6ac61fcc866608f5583c275dc34d47</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__MAX_METADATA_TYPE</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ggac71714ba8ddbbd66d26bb78a427fac01a1a2f283a3dd9e7b46181d7a114ec5805</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumeration">
      <type></type>
      <name>FLAC__StreamMetadata_Picture_Type</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gaf6d3e836cee023e0b8d897f1fdc9825d</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_METADATA_PICTURE_TYPE_OTHER</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ggaf6d3e836cee023e0b8d897f1fdc9825dadd6d6af32499b1973e48c9e8f13357ce</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_METADATA_PICTURE_TYPE_FILE_ICON_STANDARD</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ggaf6d3e836cee023e0b8d897f1fdc9825da5eca52e5cfcb718f33f5fce9b1021a49</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_METADATA_PICTURE_TYPE_FILE_ICON</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ggaf6d3e836cee023e0b8d897f1fdc9825daaf44b9d5fb75dde6941463e5029aa351</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_METADATA_PICTURE_TYPE_FRONT_COVER</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ggaf6d3e836cee023e0b8d897f1fdc9825da3e20b405fd4e835ff3a4465b8bcb7c36</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_METADATA_PICTURE_TYPE_BACK_COVER</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ggaf6d3e836cee023e0b8d897f1fdc9825da9ae132f2ee7d3baf35f94a9dc9640f62</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_METADATA_PICTURE_TYPE_LEAFLET_PAGE</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ggaf6d3e836cee023e0b8d897f1fdc9825dad3cb471b7925ae5034d9fd9ecfafb87a</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_METADATA_PICTURE_TYPE_MEDIA</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ggaf6d3e836cee023e0b8d897f1fdc9825dac994edc4166107ab5790e49f0b57ffd9</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_METADATA_PICTURE_TYPE_LEAD_ARTIST</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ggaf6d3e836cee023e0b8d897f1fdc9825da1282e252e20553c39907074052960f42</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_METADATA_PICTURE_TYPE_ARTIST</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ggaf6d3e836cee023e0b8d897f1fdc9825da4cead70f8720f180fc220e6df8d55cce</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_METADATA_PICTURE_TYPE_CONDUCTOR</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ggaf6d3e836cee023e0b8d897f1fdc9825dae01a47af0b0c4d89500b755ebca866ce</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_METADATA_PICTURE_TYPE_BAND</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ggaf6d3e836cee023e0b8d897f1fdc9825da8515523b4c9ab65ffef7db98bc09ceb1</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_METADATA_PICTURE_TYPE_COMPOSER</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ggaf6d3e836cee023e0b8d897f1fdc9825da5ea1554bc96deb45731bc5897600d1c2</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_METADATA_PICTURE_TYPE_LYRICIST</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ggaf6d3e836cee023e0b8d897f1fdc9825da86159eda8969514f5992b3e341103f22</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_METADATA_PICTURE_TYPE_RECORDING_LOCATION</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ggaf6d3e836cee023e0b8d897f1fdc9825dac96e810cdd81465709b4a3a03289e89c</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_METADATA_PICTURE_TYPE_DURING_RECORDING</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ggaf6d3e836cee023e0b8d897f1fdc9825da8cee3bb376ed1044b3a7e20b9c971ff1</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_METADATA_PICTURE_TYPE_DURING_PERFORMANCE</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ggaf6d3e836cee023e0b8d897f1fdc9825da4d4dc6904984370501865988d948de3f</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_METADATA_PICTURE_TYPE_VIDEO_SCREEN_CAPTURE</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ggaf6d3e836cee023e0b8d897f1fdc9825da7adc2b194968b51768721de7bda39df9</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_METADATA_PICTURE_TYPE_FISH</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ggaf6d3e836cee023e0b8d897f1fdc9825dabbf0d7c519ae8ba8cec7d1f165f67b0f</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_METADATA_PICTURE_TYPE_ILLUSTRATION</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ggaf6d3e836cee023e0b8d897f1fdc9825da89ba412c9d89c937c28afdab508d047a</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_METADATA_PICTURE_TYPE_BAND_LOGOTYPE</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ggaf6d3e836cee023e0b8d897f1fdc9825da751716a4528a78a8d53f435c816c4917</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_METADATA_PICTURE_TYPE_PUBLISHER_LOGOTYPE</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ggaf6d3e836cee023e0b8d897f1fdc9825da31d75150a4079482fe122e703eff9141</anchor>
      <arglist></arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__format_sample_rate_is_valid</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga985a32bf66e3a69a48e8f9ccd7c5e2e9</anchor>
      <arglist>(unsigned sample_rate)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__format_blocksize_is_subset</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga5370258a7aae32ad18b4c69fbd5e4a36</anchor>
      <arglist>(unsigned blocksize, unsigned sample_rate)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__format_sample_rate_is_subset</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gae305f200f9f4fca80f8ee3d004cf1164</anchor>
      <arglist>(unsigned sample_rate)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__format_vorbiscomment_entry_name_is_legal</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gae5fb55cd5977ebf178c5b38da831c057</anchor>
      <arglist>(const char *name)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__format_vorbiscomment_entry_value_is_legal</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gad4509984c8a8a0b926a4fb1ba25ec449</anchor>
      <arglist>(const FLAC__byte *value, unsigned length)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__format_vorbiscomment_entry_is_legal</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gab98da8754f99fdf7ba0583275b200de3</anchor>
      <arglist>(const FLAC__byte *entry, unsigned length)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__format_seektable_is_legal</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga02ed0843553fb8f718fe8e7c54d12244</anchor>
      <arglist>(const FLAC__StreamMetadata_SeekTable *seek_table)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>FLAC__format_seektable_sort</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga64dede2811616c7aa41caaed9c855cd4</anchor>
      <arglist>(FLAC__StreamMetadata_SeekTable *seek_table)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__format_cuesheet_is_legal</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gaa9ed0fa4ed04dbfdaa163d0f5308c080</anchor>
      <arglist>(const FLAC__StreamMetadata_CueSheet *cue_sheet, FLAC__bool check_cd_da_subset, const char **violation)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__format_picture_is_legal</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga82ca3ffc97c106c61882134f1a7fb1be</anchor>
      <arglist>(const FLAC__StreamMetadata_Picture *picture, const char **violation)</arglist>
    </member>
    <member kind="variable">
      <type>const char *</type>
      <name>FLAC__VERSION_STRING</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga52e2616f9a0b94881cd7711c18d62a35</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const char *</type>
      <name>FLAC__VENDOR_STRING</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gad5cccab0de3adda58914edf3c31fd64f</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const FLAC__byte</type>
      <name>FLAC__STREAM_SYNC_STRING</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga3f275a3a6056e0d53df3b72b03adde4b</anchor>
      <arglist>[4]</arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_SYNC</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga21e4646e61486382c6d91234474fce66</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_SYNC_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga5f4f9e89655ecc48a7a539f92da1b7e7</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const char *const</type>
      <name>FLAC__EntropyCodingMethodTypeString</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga41603ac35eed8c77c2f2e0b12067d88a</anchor>
      <arglist>[]</arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__ENTROPY_CODING_METHOD_PARTITIONED_RICE_ORDER_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga5056169ee48d8ece011d05f0fb9b3d43</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__ENTROPY_CODING_METHOD_PARTITIONED_RICE_PARAMETER_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gace7a2a1c6b75dc4b02d34933dae21cde</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__ENTROPY_CODING_METHOD_PARTITIONED_RICE2_PARAMETER_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga0b08df429809f9a78710c5251c9615ea</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__ENTROPY_CODING_METHOD_PARTITIONED_RICE_RAW_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga2299e09372ce2d652ad215ad5d57f6f7</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__ENTROPY_CODING_METHOD_PARTITIONED_RICE_ESCAPE_PARAMETER</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga8c0af33a0ef538cd8e9e04e8a25913af</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__ENTROPY_CODING_METHOD_PARTITIONED_RICE2_ESCAPE_PARAMETER</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gaad1bb90d3f58a38aab4509e43e2be1fd</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__ENTROPY_CODING_METHOD_TYPE_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga224222cd46f6613456c5efd75d713946</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const char *const</type>
      <name>FLAC__SubframeTypeString</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga78d78f45f123cfbb50cebd61b96097df</anchor>
      <arglist>[]</arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__SUBFRAME_LPC_QLP_COEFF_PRECISION_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga5fe1e86b574141cd1e11d9be6b1b8202</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__SUBFRAME_LPC_QLP_SHIFT_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga1d957b042a1e673e39a815427c6aa494</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__SUBFRAME_ZERO_PAD_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga9aace0e3dfa3bd2a3195e134852082c2</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__SUBFRAME_TYPE_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gab4b4a36a869e404834e01e288f9105a6</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__SUBFRAME_WASTED_BITS_FLAG_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gacf0f85d0592281227a5a1400087c466e</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__SUBFRAME_TYPE_CONSTANT_BYTE_ALIGNED_MASK</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gad783daf32785798dcc3a23e1a4cecefc</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__SUBFRAME_TYPE_VERBATIM_BYTE_ALIGNED_MASK</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gadf86648f3eb3ffecd547019577a2ab0f</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__SUBFRAME_TYPE_FIXED_BYTE_ALIGNED_MASK</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga3d8dbb2da910837bc5811f8b12bb60f7</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__SUBFRAME_TYPE_LPC_BYTE_ALIGNED_MASK</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gaf4edf4982798602f2793f907ee7d7695</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const char *const</type>
      <name>FLAC__ChannelAssignmentString</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gab1a1d3929a4e5a5aff2c15010742aa21</anchor>
      <arglist>[]</arglist>
    </member>
    <member kind="variable">
      <type>const char *const</type>
      <name>FLAC__FrameNumberTypeString</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga931a0e63c0f2b31fab801e1dd693fa4e</anchor>
      <arglist>[]</arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__FRAME_HEADER_SYNC</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga100c4e86ebb9b85b2a987d1ad383596b</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__FRAME_HEADER_SYNC_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga6be66feb8358e5a39869ce3e39f7b47a</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__FRAME_HEADER_RESERVED_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gad9a46acc93058fb6aba6e0cf8f9c2713</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__FRAME_HEADER_BLOCKING_STRATEGY_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gaa1f482e9172cd95795b32724784d8be9</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__FRAME_HEADER_BLOCK_SIZE_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga2c5c93ba19375583ca27c3d288e90a03</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__FRAME_HEADER_SAMPLE_RATE_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga1ec5825f1a07d1136204840d5d89feca</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__FRAME_HEADER_CHANNEL_ASSIGNMENT_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gafa9050a64f02d18ea7426e4c382bb6a6</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__FRAME_HEADER_BITS_PER_SAMPLE_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga9d6055c79974497dc99912b0e9ebbe41</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__FRAME_HEADER_ZERO_PAD_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gaf761899e8a95a7bce3f429e6648ca14e</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__FRAME_HEADER_CRC_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga82e91fc3b93cd8573c814dd282b5b6ed</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__FRAME_FOOTER_CRC_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga238b33bf853ea3ab0fc0dbaad5e41f4e</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const char *const</type>
      <name>FLAC__MetadataTypeString</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gaa9ad23f06a579d1110d61d54c8c999f0</anchor>
      <arglist>[]</arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_METADATA_STREAMINFO_MIN_BLOCK_SIZE_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gac10f1ca2318884d9ed142350744eca1a</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_METADATA_STREAMINFO_MAX_BLOCK_SIZE_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga9049314bc422ba321414afdafad76d04</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_METADATA_STREAMINFO_MIN_FRAME_SIZE_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga3abb82a701c670c7a9f8f47fc4df5edc</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_METADATA_STREAMINFO_MAX_FRAME_SIZE_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga4e7c5315f21eaa0e3f0dfb6517eb4545</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_METADATA_STREAMINFO_SAMPLE_RATE_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga7c5529d77e0cf806e709dc147ff83452</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_METADATA_STREAMINFO_CHANNELS_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga719aee1e62d71e1e03d6299aade7a7c4</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_METADATA_STREAMINFO_BITS_PER_SAMPLE_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga4d8a3f1a75a24e8d0a966f8ad01f15ed</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_METADATA_STREAMINFO_TOTAL_SAMPLES_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gaeb69211400ad50dd6c3503e8092c6afb</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_METADATA_STREAMINFO_MD5SUM_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga492d4321cbd601ed24489ad59d9ddaf8</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_METADATA_APPLICATION_ID_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gac374cf68c046406a062daf0b811e9020</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_METADATA_SEEKPOINT_SAMPLE_NUMBER_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga2afc4c1f5b9c522274e4fb7236d645f5</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_METADATA_SEEKPOINT_STREAM_OFFSET_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gafa894a5e0e9f5fa40a76c5a9f19800cf</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_METADATA_SEEKPOINT_FRAME_SAMPLES_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga330813fcf2c4ebb133843511134b6c11</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const FLAC__uint64</type>
      <name>FLAC__STREAM_METADATA_SEEKPOINT_PLACEHOLDER</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gad5d58774aea926635e6841c411d60566</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_METADATA_VORBIS_COMMENT_ENTRY_LENGTH_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gad73e13be54a583adfa4d2e43c1a4bdff</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_METADATA_VORBIS_COMMENT_NUM_COMMENTS_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gabdd8ad7a3de9abd96358fe10f4b0e6a0</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_METADATA_CUESHEET_INDEX_OFFSET_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gaec8a23f851fb225d16b996282413704b</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_METADATA_CUESHEET_INDEX_NUMBER_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gad7e0ce539714d0cc47053a6b11711227</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_METADATA_CUESHEET_INDEX_RESERVED_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga8cafc79f8b98b5ced139b37c46b4f4bf</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_METADATA_CUESHEET_TRACK_OFFSET_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gaccf9e6a290a44763f2616ff65dd4e422</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_METADATA_CUESHEET_TRACK_NUMBER_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gad0a4822167882a1321467b0dc67f145b</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_METADATA_CUESHEET_TRACK_ISRC_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gac63e5eb63aa52b1fe780bc89c55dec92</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_METADATA_CUESHEET_TRACK_TYPE_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gac32d5288f0f80fbd9101bb0352850a4f</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_METADATA_CUESHEET_TRACK_PRE_EMPHASIS_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gafb26465f020a51491ecad3c2fe8839be</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_METADATA_CUESHEET_TRACK_RESERVED_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gad825e57e995f161d945a51bb363e4650</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_METADATA_CUESHEET_TRACK_NUM_INDICES_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga443798929acc94f4b7b6c19ab92c5b25</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_METADATA_CUESHEET_MEDIA_CATALOG_NUMBER_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga380732fda7b866b075b138e9153717b9</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_METADATA_CUESHEET_LEAD_IN_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga9e47028762ec6709a14d8b81e7712285</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_METADATA_CUESHEET_IS_CD_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gaf1817667af48cd8c122488664972fd0c</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_METADATA_CUESHEET_RESERVED_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga1ceaba7fd5e900d423b6d9537d1979fd</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_METADATA_CUESHEET_NUM_TRACKS_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gaa0046d3f2bc430feb97a4b04053db01e</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const char *const</type>
      <name>FLAC__StreamMetadata_Picture_TypeString</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga2d27672452696cb97fd39db1cf43486b</anchor>
      <arglist>[]</arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_METADATA_PICTURE_TYPE_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gaa06ecd0960798a4e16e6b6a218008da7</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_METADATA_PICTURE_MIME_TYPE_LENGTH_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gafe9e4d8a4942eef747612e0cab836c4e</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_METADATA_PICTURE_DESCRIPTION_LENGTH_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga96910250e54bb335a3c11940372c6585</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_METADATA_PICTURE_WIDTH_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga729da59256030685bf9f29cdbf571e4d</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_METADATA_PICTURE_HEIGHT_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga173238fb8a7499b67b1658bb3ac6a81a</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_METADATA_PICTURE_DEPTH_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga6b9cf7c11b6fb7f96dfbc197db280128</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_METADATA_PICTURE_COLORS_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga6ed3603f4d9092d99e4b4d57bb431cc5</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_METADATA_PICTURE_DATA_LENGTH_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga808843e986308268c5dc6c841c67a74a</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_METADATA_IS_LAST_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>ga2a2e0d9428b90662b6790404dd393830</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_METADATA_TYPE_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gaf88b0f6621a4b22b37a8fe2ef82a7204</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const unsigned</type>
      <name>FLAC__STREAM_METADATA_LENGTH_LEN</name>
      <anchorfile>group__flac__format.html</anchorfile>
      <anchor>gaa16818a9f8de9e3bd75e832cc6149eb8</anchor>
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
      <anchorfile>group__flac__metadata__level0.html</anchorfile>
      <anchor>ga804b42d9da714199b4b383ce51078d51</anchor>
      <arglist>(const char *filename, FLAC__StreamMetadata *streaminfo)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_get_tags</name>
      <anchorfile>group__flac__metadata__level0.html</anchorfile>
      <anchor>ga1626af09cd39d4fa37d5b46ebe3790fd</anchor>
      <arglist>(const char *filename, FLAC__StreamMetadata **tags)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_get_cuesheet</name>
      <anchorfile>group__flac__metadata__level0.html</anchorfile>
      <anchor>ga0f47949dca514506718276205a4fae0b</anchor>
      <arglist>(const char *filename, FLAC__StreamMetadata **cuesheet)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_get_picture</name>
      <anchorfile>group__flac__metadata__level0.html</anchorfile>
      <anchor>ga0c9cd22296400c8ce16ee1db011342cb</anchor>
      <arglist>(const char *filename, FLAC__StreamMetadata **picture, FLAC__StreamMetadata_Picture_Type type, const char *mime_type, const FLAC__byte *description, unsigned max_width, unsigned max_height, unsigned max_depth, unsigned max_colors)</arglist>
    </member>
  </compound>
  <compound kind="group">
    <name>flac_metadata_level1</name>
    <title>FLAC/metadata.h: metadata level 1 interface</title>
    <filename>group__flac__metadata__level1.html</filename>
    <member kind="typedef">
      <type>struct FLAC__Metadata_SimpleIterator</type>
      <name>FLAC__Metadata_SimpleIterator</name>
      <anchorfile>group__flac__metadata__level1.html</anchorfile>
      <anchor>ga6accccddbb867dfc2eece9ee3ffecb3a</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumeration">
      <type></type>
      <name>FLAC__Metadata_SimpleIteratorStatus</name>
      <anchorfile>group__flac__metadata__level1.html</anchorfile>
      <anchor>gac926e7d2773a05066115cac9048bbec9</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__METADATA_SIMPLE_ITERATOR_STATUS_OK</name>
      <anchorfile>group__flac__metadata__level1.html</anchorfile>
      <anchor>ggac926e7d2773a05066115cac9048bbec9a33aadd73194c0d7e307d643237e0ddcd</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__METADATA_SIMPLE_ITERATOR_STATUS_ILLEGAL_INPUT</name>
      <anchorfile>group__flac__metadata__level1.html</anchorfile>
      <anchor>ggac926e7d2773a05066115cac9048bbec9a0a3933cb38c8957a8d5c3d1afb4766f9</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__METADATA_SIMPLE_ITERATOR_STATUS_ERROR_OPENING_FILE</name>
      <anchorfile>group__flac__metadata__level1.html</anchorfile>
      <anchor>ggac926e7d2773a05066115cac9048bbec9a20e835bbb74b4d039e598617f68d2af6</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__METADATA_SIMPLE_ITERATOR_STATUS_NOT_A_FLAC_FILE</name>
      <anchorfile>group__flac__metadata__level1.html</anchorfile>
      <anchor>ggac926e7d2773a05066115cac9048bbec9a7785f77a612be8956fbe7cab73497220</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__METADATA_SIMPLE_ITERATOR_STATUS_NOT_WRITABLE</name>
      <anchorfile>group__flac__metadata__level1.html</anchorfile>
      <anchor>ggac926e7d2773a05066115cac9048bbec9af055d8c0c663e72134fe2db8037b6880</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__METADATA_SIMPLE_ITERATOR_STATUS_BAD_METADATA</name>
      <anchorfile>group__flac__metadata__level1.html</anchorfile>
      <anchor>ggac926e7d2773a05066115cac9048bbec9a14c897124887858109200723826f85b7</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__METADATA_SIMPLE_ITERATOR_STATUS_READ_ERROR</name>
      <anchorfile>group__flac__metadata__level1.html</anchorfile>
      <anchor>ggac926e7d2773a05066115cac9048bbec9a088df964f0852dd7e19304e920c3ee8e</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__METADATA_SIMPLE_ITERATOR_STATUS_SEEK_ERROR</name>
      <anchorfile>group__flac__metadata__level1.html</anchorfile>
      <anchor>ggac926e7d2773a05066115cac9048bbec9a2ad85a32e291d1e918692d68cc22fd40</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__METADATA_SIMPLE_ITERATOR_STATUS_WRITE_ERROR</name>
      <anchorfile>group__flac__metadata__level1.html</anchorfile>
      <anchor>ggac926e7d2773a05066115cac9048bbec9ac2337299c2347ca311caeaa7d71d857c</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__METADATA_SIMPLE_ITERATOR_STATUS_RENAME_ERROR</name>
      <anchorfile>group__flac__metadata__level1.html</anchorfile>
      <anchor>ggac926e7d2773a05066115cac9048bbec9a2e073843fa99419d76a0b210da96ceb6</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__METADATA_SIMPLE_ITERATOR_STATUS_UNLINK_ERROR</name>
      <anchorfile>group__flac__metadata__level1.html</anchorfile>
      <anchor>ggac926e7d2773a05066115cac9048bbec9a4f855433038c576da127fc1de9d18f9b</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__METADATA_SIMPLE_ITERATOR_STATUS_MEMORY_ALLOCATION_ERROR</name>
      <anchorfile>group__flac__metadata__level1.html</anchorfile>
      <anchor>ggac926e7d2773a05066115cac9048bbec9aa8386ed0a20d7e91b0022d203ec3cdec</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__METADATA_SIMPLE_ITERATOR_STATUS_INTERNAL_ERROR</name>
      <anchorfile>group__flac__metadata__level1.html</anchorfile>
      <anchor>ggac926e7d2773a05066115cac9048bbec9a9d821ae65a1c5de619daa88c850906df</anchor>
      <arglist></arglist>
    </member>
    <member kind="function">
      <type>FLAC__Metadata_SimpleIterator *</type>
      <name>FLAC__metadata_simple_iterator_new</name>
      <anchorfile>group__flac__metadata__level1.html</anchorfile>
      <anchor>ga017ae86f3351888f50feb47026ed2482</anchor>
      <arglist>(void)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>FLAC__metadata_simple_iterator_delete</name>
      <anchorfile>group__flac__metadata__level1.html</anchorfile>
      <anchor>ga4619be06f51429fea71e5b98900cec3e</anchor>
      <arglist>(FLAC__Metadata_SimpleIterator *iterator)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__Metadata_SimpleIteratorStatus</type>
      <name>FLAC__metadata_simple_iterator_status</name>
      <anchorfile>group__flac__metadata__level1.html</anchorfile>
      <anchor>gae8fd236fe6049c61f7f3b4a6ecbcd240</anchor>
      <arglist>(FLAC__Metadata_SimpleIterator *iterator)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_simple_iterator_init</name>
      <anchorfile>group__flac__metadata__level1.html</anchorfile>
      <anchor>gaba8daf276fd7da863a2522ac050125fd</anchor>
      <arglist>(FLAC__Metadata_SimpleIterator *iterator, const char *filename, FLAC__bool read_only, FLAC__bool preserve_file_stats)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_simple_iterator_is_writable</name>
      <anchorfile>group__flac__metadata__level1.html</anchorfile>
      <anchor>ga5150ecd8668c610f79192a2838667790</anchor>
      <arglist>(const FLAC__Metadata_SimpleIterator *iterator)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_simple_iterator_next</name>
      <anchorfile>group__flac__metadata__level1.html</anchorfile>
      <anchor>gabb7de0a1067efae353e0792dc6e51905</anchor>
      <arglist>(FLAC__Metadata_SimpleIterator *iterator)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_simple_iterator_prev</name>
      <anchorfile>group__flac__metadata__level1.html</anchorfile>
      <anchor>ga6db5313b31120b28e210ae721d6525a8</anchor>
      <arglist>(FLAC__Metadata_SimpleIterator *iterator)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_simple_iterator_is_last</name>
      <anchorfile>group__flac__metadata__level1.html</anchorfile>
      <anchor>ga9eb215059840960de69aa84469ba954f</anchor>
      <arglist>(const FLAC__Metadata_SimpleIterator *iterator)</arglist>
    </member>
    <member kind="function">
      <type>off_t</type>
      <name>FLAC__metadata_simple_iterator_get_block_offset</name>
      <anchorfile>group__flac__metadata__level1.html</anchorfile>
      <anchor>gade0a61723420daeb4bc226713671c6f0</anchor>
      <arglist>(const FLAC__Metadata_SimpleIterator *iterator)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__MetadataType</type>
      <name>FLAC__metadata_simple_iterator_get_block_type</name>
      <anchorfile>group__flac__metadata__level1.html</anchorfile>
      <anchor>ga17b61d17e83432913abf4334d6e0c073</anchor>
      <arglist>(const FLAC__Metadata_SimpleIterator *iterator)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>FLAC__metadata_simple_iterator_get_block_length</name>
      <anchorfile>group__flac__metadata__level1.html</anchorfile>
      <anchor>ga93ec66e9cfb99f04ce4125b8be906cef</anchor>
      <arglist>(const FLAC__Metadata_SimpleIterator *iterator)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_simple_iterator_get_application_id</name>
      <anchorfile>group__flac__metadata__level1.html</anchorfile>
      <anchor>gad4fea2d7d98d16e75e6d8260f690a5dc</anchor>
      <arglist>(FLAC__Metadata_SimpleIterator *iterator, FLAC__byte *id)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__StreamMetadata *</type>
      <name>FLAC__metadata_simple_iterator_get_block</name>
      <anchorfile>group__flac__metadata__level1.html</anchorfile>
      <anchor>ga1b7374cafd886ceb880b050dfa1e387a</anchor>
      <arglist>(FLAC__Metadata_SimpleIterator *iterator)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_simple_iterator_set_block</name>
      <anchorfile>group__flac__metadata__level1.html</anchorfile>
      <anchor>gae1dd863561606658f88c492682de7b80</anchor>
      <arglist>(FLAC__Metadata_SimpleIterator *iterator, FLAC__StreamMetadata *block, FLAC__bool use_padding)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_simple_iterator_insert_block_after</name>
      <anchorfile>group__flac__metadata__level1.html</anchorfile>
      <anchor>ga7a0c00e93bb37324a20926e92e604102</anchor>
      <arglist>(FLAC__Metadata_SimpleIterator *iterator, FLAC__StreamMetadata *block, FLAC__bool use_padding)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_simple_iterator_delete_block</name>
      <anchorfile>group__flac__metadata__level1.html</anchorfile>
      <anchor>gac3116c8e6e7f59914ae22c0c4c6b0a23</anchor>
      <arglist>(FLAC__Metadata_SimpleIterator *iterator, FLAC__bool use_padding)</arglist>
    </member>
    <member kind="variable">
      <type>const char *const</type>
      <name>FLAC__Metadata_SimpleIteratorStatusString</name>
      <anchorfile>group__flac__metadata__level1.html</anchorfile>
      <anchor>gaa2a8b972800c34f9f5807cadf6ecdb57</anchor>
      <arglist>[]</arglist>
    </member>
  </compound>
  <compound kind="group">
    <name>flac_metadata_level2</name>
    <title>FLAC/metadata.h: metadata level 2 interface</title>
    <filename>group__flac__metadata__level2.html</filename>
    <member kind="typedef">
      <type>struct FLAC__Metadata_Chain</type>
      <name>FLAC__Metadata_Chain</name>
      <anchorfile>group__flac__metadata__level2.html</anchorfile>
      <anchor>gaec6993c60b88f222a52af86f8f47bfdf</anchor>
      <arglist></arglist>
    </member>
    <member kind="typedef">
      <type>struct FLAC__Metadata_Iterator</type>
      <name>FLAC__Metadata_Iterator</name>
      <anchorfile>group__flac__metadata__level2.html</anchorfile>
      <anchor>ga9f3e135a07cdef7e51597646aa7b89b2</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumeration">
      <type></type>
      <name>FLAC__Metadata_ChainStatus</name>
      <anchorfile>group__flac__metadata__level2.html</anchorfile>
      <anchor>gafe2a924893b0800b020bea8160fd4531</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__METADATA_CHAIN_STATUS_OK</name>
      <anchorfile>group__flac__metadata__level2.html</anchorfile>
      <anchor>ggafe2a924893b0800b020bea8160fd4531a293be942ec54576f2b3c73613af968e9</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__METADATA_CHAIN_STATUS_ILLEGAL_INPUT</name>
      <anchorfile>group__flac__metadata__level2.html</anchorfile>
      <anchor>ggafe2a924893b0800b020bea8160fd4531a1be9400982f411173af46bf0c3acbdc7</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__METADATA_CHAIN_STATUS_ERROR_OPENING_FILE</name>
      <anchorfile>group__flac__metadata__level2.html</anchorfile>
      <anchor>ggafe2a924893b0800b020bea8160fd4531a43d2741a650576052fa3615d8cd64d86</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__METADATA_CHAIN_STATUS_NOT_A_FLAC_FILE</name>
      <anchorfile>group__flac__metadata__level2.html</anchorfile>
      <anchor>ggafe2a924893b0800b020bea8160fd4531a99748a4b12ed10f9368375cc8deeb143</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__METADATA_CHAIN_STATUS_NOT_WRITABLE</name>
      <anchorfile>group__flac__metadata__level2.html</anchorfile>
      <anchor>ggafe2a924893b0800b020bea8160fd4531ac469c6543ebb117e99064572c16672d4</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__METADATA_CHAIN_STATUS_BAD_METADATA</name>
      <anchorfile>group__flac__metadata__level2.html</anchorfile>
      <anchor>ggafe2a924893b0800b020bea8160fd4531a8efd2c76dc06308eb6eba59e1bc6300b</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__METADATA_CHAIN_STATUS_READ_ERROR</name>
      <anchorfile>group__flac__metadata__level2.html</anchorfile>
      <anchor>ggafe2a924893b0800b020bea8160fd4531a0525de5fb5d8aeeb4e848e33a8d503c6</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__METADATA_CHAIN_STATUS_SEEK_ERROR</name>
      <anchorfile>group__flac__metadata__level2.html</anchorfile>
      <anchor>ggafe2a924893b0800b020bea8160fd4531a5814bc26bcf92143198b8e7f028f43a2</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__METADATA_CHAIN_STATUS_WRITE_ERROR</name>
      <anchorfile>group__flac__metadata__level2.html</anchorfile>
      <anchor>ggafe2a924893b0800b020bea8160fd4531a66460c735e4745788b40889329e8489f</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__METADATA_CHAIN_STATUS_RENAME_ERROR</name>
      <anchorfile>group__flac__metadata__level2.html</anchorfile>
      <anchor>ggafe2a924893b0800b020bea8160fd4531af4ecf22bc3e5adf78a9c765f856efb0d</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__METADATA_CHAIN_STATUS_UNLINK_ERROR</name>
      <anchorfile>group__flac__metadata__level2.html</anchorfile>
      <anchor>ggafe2a924893b0800b020bea8160fd4531a1cd3138ed493f6a0f5b95fb8481edd1e</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__METADATA_CHAIN_STATUS_MEMORY_ALLOCATION_ERROR</name>
      <anchorfile>group__flac__metadata__level2.html</anchorfile>
      <anchor>ggafe2a924893b0800b020bea8160fd4531ab12ec938f7556a163c609194ee0aede0</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__METADATA_CHAIN_STATUS_INTERNAL_ERROR</name>
      <anchorfile>group__flac__metadata__level2.html</anchorfile>
      <anchor>ggafe2a924893b0800b020bea8160fd4531a36b9bcf93da8e0f111738a65eab36e9d</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__METADATA_CHAIN_STATUS_INVALID_CALLBACKS</name>
      <anchorfile>group__flac__metadata__level2.html</anchorfile>
      <anchor>ggafe2a924893b0800b020bea8160fd4531ab8a6aa5f115db3f07ad2ed4adbcbe060</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__METADATA_CHAIN_STATUS_READ_WRITE_MISMATCH</name>
      <anchorfile>group__flac__metadata__level2.html</anchorfile>
      <anchor>ggafe2a924893b0800b020bea8160fd4531a0d9e64ad6514c88b8ea9e9171c42ec9a</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__METADATA_CHAIN_STATUS_WRONG_WRITE_CALL</name>
      <anchorfile>group__flac__metadata__level2.html</anchorfile>
      <anchor>ggafe2a924893b0800b020bea8160fd4531af86670707345e2d02cc84aec059459d0</anchor>
      <arglist></arglist>
    </member>
    <member kind="function">
      <type>FLAC__Metadata_Chain *</type>
      <name>FLAC__metadata_chain_new</name>
      <anchorfile>group__flac__metadata__level2.html</anchorfile>
      <anchor>ga381a1b6efff8d4e9d793f1dda515bd73</anchor>
      <arglist>(void)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>FLAC__metadata_chain_delete</name>
      <anchorfile>group__flac__metadata__level2.html</anchorfile>
      <anchor>ga46b6c67f30db2955798dfb5556f63aa3</anchor>
      <arglist>(FLAC__Metadata_Chain *chain)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__Metadata_ChainStatus</type>
      <name>FLAC__metadata_chain_status</name>
      <anchorfile>group__flac__metadata__level2.html</anchorfile>
      <anchor>ga8e74773f8ca2bb2bc0b56a65ca0299f4</anchor>
      <arglist>(FLAC__Metadata_Chain *chain)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_chain_read</name>
      <anchorfile>group__flac__metadata__level2.html</anchorfile>
      <anchor>ga5a4f2056c30f78af5a79f6b64d5bfdcd</anchor>
      <arglist>(FLAC__Metadata_Chain *chain, const char *filename)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_chain_read_ogg</name>
      <anchorfile>group__flac__metadata__level2.html</anchorfile>
      <anchor>ga3995010aab28a483ad9905669e5c4954</anchor>
      <arglist>(FLAC__Metadata_Chain *chain, const char *filename)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_chain_read_with_callbacks</name>
      <anchorfile>group__flac__metadata__level2.html</anchorfile>
      <anchor>ga595f55b611ed588d4d55a9b2eb9d2add</anchor>
      <arglist>(FLAC__Metadata_Chain *chain, FLAC__IOHandle handle, FLAC__IOCallbacks callbacks)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_chain_read_ogg_with_callbacks</name>
      <anchorfile>group__flac__metadata__level2.html</anchorfile>
      <anchor>gaccc2f991722682d3c31d36f51985066c</anchor>
      <arglist>(FLAC__Metadata_Chain *chain, FLAC__IOHandle handle, FLAC__IOCallbacks callbacks)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_chain_check_if_tempfile_needed</name>
      <anchorfile>group__flac__metadata__level2.html</anchorfile>
      <anchor>ga46602f64d423cfe5d5f8a4155f8a97e2</anchor>
      <arglist>(FLAC__Metadata_Chain *chain, FLAC__bool use_padding)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_chain_write</name>
      <anchorfile>group__flac__metadata__level2.html</anchorfile>
      <anchor>ga46bf9cf7d426078101b9297ba80bb835</anchor>
      <arglist>(FLAC__Metadata_Chain *chain, FLAC__bool use_padding, FLAC__bool preserve_file_stats)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_chain_write_with_callbacks</name>
      <anchorfile>group__flac__metadata__level2.html</anchorfile>
      <anchor>ga70532b3705294dc891d8db649a4d4843</anchor>
      <arglist>(FLAC__Metadata_Chain *chain, FLAC__bool use_padding, FLAC__IOHandle handle, FLAC__IOCallbacks callbacks)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_chain_write_with_callbacks_and_tempfile</name>
      <anchorfile>group__flac__metadata__level2.html</anchorfile>
      <anchor>ga72facaa621e8d798036a4a7da3643e41</anchor>
      <arglist>(FLAC__Metadata_Chain *chain, FLAC__bool use_padding, FLAC__IOHandle handle, FLAC__IOCallbacks callbacks, FLAC__IOHandle temp_handle, FLAC__IOCallbacks temp_callbacks)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>FLAC__metadata_chain_merge_padding</name>
      <anchorfile>group__flac__metadata__level2.html</anchorfile>
      <anchor>ga0a43897914edb751cb87f7e281aff3dc</anchor>
      <arglist>(FLAC__Metadata_Chain *chain)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>FLAC__metadata_chain_sort_padding</name>
      <anchorfile>group__flac__metadata__level2.html</anchorfile>
      <anchor>ga82b66fe71c727adb9cf80a1da9834ce5</anchor>
      <arglist>(FLAC__Metadata_Chain *chain)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__Metadata_Iterator *</type>
      <name>FLAC__metadata_iterator_new</name>
      <anchorfile>group__flac__metadata__level2.html</anchorfile>
      <anchor>ga1941ca04671813fc039ea7fd35ae6461</anchor>
      <arglist>(void)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>FLAC__metadata_iterator_delete</name>
      <anchorfile>group__flac__metadata__level2.html</anchorfile>
      <anchor>ga374c246e1aeafd803d29a6e99b226241</anchor>
      <arglist>(FLAC__Metadata_Iterator *iterator)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>FLAC__metadata_iterator_init</name>
      <anchorfile>group__flac__metadata__level2.html</anchorfile>
      <anchor>ga2e93196b17a1c73e949e661e33d7311a</anchor>
      <arglist>(FLAC__Metadata_Iterator *iterator, FLAC__Metadata_Chain *chain)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_iterator_next</name>
      <anchorfile>group__flac__metadata__level2.html</anchorfile>
      <anchor>ga60449d0c1d76a73978159e3aa5e79459</anchor>
      <arglist>(FLAC__Metadata_Iterator *iterator)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_iterator_prev</name>
      <anchorfile>group__flac__metadata__level2.html</anchorfile>
      <anchor>gaa28df1c5aa56726f573f90e4bae2fe50</anchor>
      <arglist>(FLAC__Metadata_Iterator *iterator)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__MetadataType</type>
      <name>FLAC__metadata_iterator_get_block_type</name>
      <anchorfile>group__flac__metadata__level2.html</anchorfile>
      <anchor>ga83ecb59ffa16bfbb1e286e64f9270de1</anchor>
      <arglist>(const FLAC__Metadata_Iterator *iterator)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__StreamMetadata *</type>
      <name>FLAC__metadata_iterator_get_block</name>
      <anchorfile>group__flac__metadata__level2.html</anchorfile>
      <anchor>gad3e7fbc3b3d9c192a3ac425c7b263641</anchor>
      <arglist>(FLAC__Metadata_Iterator *iterator)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_iterator_set_block</name>
      <anchorfile>group__flac__metadata__level2.html</anchorfile>
      <anchor>gaf61795b21300a2b0c9940c11974aab53</anchor>
      <arglist>(FLAC__Metadata_Iterator *iterator, FLAC__StreamMetadata *block)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_iterator_delete_block</name>
      <anchorfile>group__flac__metadata__level2.html</anchorfile>
      <anchor>gadf860af967d2ee483be01fc0ed8767a9</anchor>
      <arglist>(FLAC__Metadata_Iterator *iterator, FLAC__bool replace_with_padding)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_iterator_insert_block_before</name>
      <anchorfile>group__flac__metadata__level2.html</anchorfile>
      <anchor>ga8ac45e2df8b6fd6f5db345c4293aa435</anchor>
      <arglist>(FLAC__Metadata_Iterator *iterator, FLAC__StreamMetadata *block)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_iterator_insert_block_after</name>
      <anchorfile>group__flac__metadata__level2.html</anchorfile>
      <anchor>ga55e53757f91696e2578196a2799fc632</anchor>
      <arglist>(FLAC__Metadata_Iterator *iterator, FLAC__StreamMetadata *block)</arglist>
    </member>
    <member kind="variable">
      <type>const char *const</type>
      <name>FLAC__Metadata_ChainStatusString</name>
      <anchorfile>group__flac__metadata__level2.html</anchorfile>
      <anchor>ga6498d1976b0d9fa3f8f6295c02e622dd</anchor>
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
      <anchorfile>group__flac__metadata__object.html</anchorfile>
      <anchor>ga5df7bc8c72cafed1391bdc5ffc876e0f</anchor>
      <arglist>(FLAC__MetadataType type)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__StreamMetadata *</type>
      <name>FLAC__metadata_object_clone</name>
      <anchorfile>group__flac__metadata__object.html</anchorfile>
      <anchor>ga29af0ecc2a015ef22289f206bc308d80</anchor>
      <arglist>(const FLAC__StreamMetadata *object)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>FLAC__metadata_object_delete</name>
      <anchorfile>group__flac__metadata__object.html</anchorfile>
      <anchor>ga6b3159744a1e5c4ce9d349fd0ebae800</anchor>
      <arglist>(FLAC__StreamMetadata *object)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_object_is_equal</name>
      <anchorfile>group__flac__metadata__object.html</anchorfile>
      <anchor>ga6853bcafe731b1db37105d49f3085349</anchor>
      <arglist>(const FLAC__StreamMetadata *block1, const FLAC__StreamMetadata *block2)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_object_application_set_data</name>
      <anchorfile>group__flac__metadata__object.html</anchorfile>
      <anchor>ga07cd39bbe12f4f4e144e38c5265b997d</anchor>
      <arglist>(FLAC__StreamMetadata *object, FLAC__byte *data, unsigned length, FLAC__bool copy)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_object_seektable_resize_points</name>
      <anchorfile>group__flac__metadata__object.html</anchorfile>
      <anchor>gaffa16bae5e3683c983dc137fd56f0c26</anchor>
      <arglist>(FLAC__StreamMetadata *object, unsigned new_num_points)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>FLAC__metadata_object_seektable_set_point</name>
      <anchorfile>group__flac__metadata__object.html</anchorfile>
      <anchor>gaa1a69eb95a3c17aa973466589e85f3c1</anchor>
      <arglist>(FLAC__StreamMetadata *object, unsigned point_num, FLAC__StreamMetadata_SeekPoint point)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_object_seektable_insert_point</name>
      <anchorfile>group__flac__metadata__object.html</anchorfile>
      <anchor>ga794109d40ff0065659c005f1cf86d3c9</anchor>
      <arglist>(FLAC__StreamMetadata *object, unsigned point_num, FLAC__StreamMetadata_SeekPoint point)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_object_seektable_delete_point</name>
      <anchorfile>group__flac__metadata__object.html</anchorfile>
      <anchor>ga8e42ac803f857eaa7814838c49a15c5f</anchor>
      <arglist>(FLAC__StreamMetadata *object, unsigned point_num)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_object_seektable_is_legal</name>
      <anchorfile>group__flac__metadata__object.html</anchorfile>
      <anchor>gacd3e1b83fabc1dabccb725b2876c8f53</anchor>
      <arglist>(const FLAC__StreamMetadata *object)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_object_seektable_template_append_placeholders</name>
      <anchorfile>group__flac__metadata__object.html</anchorfile>
      <anchor>ga6280327ed000ee85846a5533fd40a33b</anchor>
      <arglist>(FLAC__StreamMetadata *object, unsigned num)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_object_seektable_template_append_point</name>
      <anchorfile>group__flac__metadata__object.html</anchorfile>
      <anchor>ga0b3aca4fbebc206cd79f13ac36f653f0</anchor>
      <arglist>(FLAC__StreamMetadata *object, FLAC__uint64 sample_number)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_object_seektable_template_append_points</name>
      <anchorfile>group__flac__metadata__object.html</anchorfile>
      <anchor>gac838116fa0e48242651944ab94bab508</anchor>
      <arglist>(FLAC__StreamMetadata *object, FLAC__uint64 sample_numbers[], unsigned num)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_object_seektable_template_append_spaced_points</name>
      <anchorfile>group__flac__metadata__object.html</anchorfile>
      <anchor>ga7dcbd38a3a71a8aa26e93a6992a8f83e</anchor>
      <arglist>(FLAC__StreamMetadata *object, unsigned num, FLAC__uint64 total_samples)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_object_seektable_template_append_spaced_points_by_samples</name>
      <anchorfile>group__flac__metadata__object.html</anchorfile>
      <anchor>ga6bce5ee9332ea070d65482a2c1ce1c2d</anchor>
      <arglist>(FLAC__StreamMetadata *object, unsigned samples, FLAC__uint64 total_samples)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_object_seektable_template_sort</name>
      <anchorfile>group__flac__metadata__object.html</anchorfile>
      <anchor>gafb0449b639ba5c618826d893c2961260</anchor>
      <arglist>(FLAC__StreamMetadata *object, FLAC__bool compact)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_object_vorbiscomment_set_vendor_string</name>
      <anchorfile>group__flac__metadata__object.html</anchorfile>
      <anchor>ga5cf1a57afab200b4b67730a77d3ee162</anchor>
      <arglist>(FLAC__StreamMetadata *object, FLAC__StreamMetadata_VorbisComment_Entry entry, FLAC__bool copy)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_object_vorbiscomment_resize_comments</name>
      <anchorfile>group__flac__metadata__object.html</anchorfile>
      <anchor>ga829152404c9d160c7bc67699dd7f857e</anchor>
      <arglist>(FLAC__StreamMetadata *object, unsigned new_num_comments)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_object_vorbiscomment_set_comment</name>
      <anchorfile>group__flac__metadata__object.html</anchorfile>
      <anchor>gaefadb16fe0fff9600beab0edbac8d226</anchor>
      <arglist>(FLAC__StreamMetadata *object, unsigned comment_num, FLAC__StreamMetadata_VorbisComment_Entry entry, FLAC__bool copy)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_object_vorbiscomment_insert_comment</name>
      <anchorfile>group__flac__metadata__object.html</anchorfile>
      <anchor>ga44ec196a99c8cd7d1d50817c8532ddb3</anchor>
      <arglist>(FLAC__StreamMetadata *object, unsigned comment_num, FLAC__StreamMetadata_VorbisComment_Entry entry, FLAC__bool copy)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_object_vorbiscomment_append_comment</name>
      <anchorfile>group__flac__metadata__object.html</anchorfile>
      <anchor>ga889b8b9c5bbd1070a1214c3da8b72863</anchor>
      <arglist>(FLAC__StreamMetadata *object, FLAC__StreamMetadata_VorbisComment_Entry entry, FLAC__bool copy)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_object_vorbiscomment_replace_comment</name>
      <anchorfile>group__flac__metadata__object.html</anchorfile>
      <anchor>ga0608308e8c4c09aa610747d8dff90a34</anchor>
      <arglist>(FLAC__StreamMetadata *object, FLAC__StreamMetadata_VorbisComment_Entry entry, FLAC__bool all, FLAC__bool copy)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_object_vorbiscomment_delete_comment</name>
      <anchorfile>group__flac__metadata__object.html</anchorfile>
      <anchor>gafe14896322f7d638f5de0c61addd1dc7</anchor>
      <arglist>(FLAC__StreamMetadata *object, unsigned comment_num)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_object_vorbiscomment_entry_from_name_value_pair</name>
      <anchorfile>group__flac__metadata__object.html</anchorfile>
      <anchor>gab644c34515c04630c62a7645fab2947e</anchor>
      <arglist>(FLAC__StreamMetadata_VorbisComment_Entry *entry, const char *field_name, const char *field_value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_object_vorbiscomment_entry_to_name_value_pair</name>
      <anchorfile>group__flac__metadata__object.html</anchorfile>
      <anchor>ga29079764fabda53cb3e890e6d05c8345</anchor>
      <arglist>(const FLAC__StreamMetadata_VorbisComment_Entry entry, char **field_name, char **field_value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_object_vorbiscomment_entry_matches</name>
      <anchorfile>group__flac__metadata__object.html</anchorfile>
      <anchor>gac4c32b1055642b11cd77628ac9508ded</anchor>
      <arglist>(const FLAC__StreamMetadata_VorbisComment_Entry entry, const char *field_name, unsigned field_name_length)</arglist>
    </member>
    <member kind="function">
      <type>int</type>
      <name>FLAC__metadata_object_vorbiscomment_find_entry_from</name>
      <anchorfile>group__flac__metadata__object.html</anchorfile>
      <anchor>ga23d79d11e427e1590f406a7137c8bff2</anchor>
      <arglist>(const FLAC__StreamMetadata *object, unsigned offset, const char *field_name)</arglist>
    </member>
    <member kind="function">
      <type>int</type>
      <name>FLAC__metadata_object_vorbiscomment_remove_entry_matching</name>
      <anchorfile>group__flac__metadata__object.html</anchorfile>
      <anchor>ga017d743b3200a27b8567ef33592224b8</anchor>
      <arglist>(FLAC__StreamMetadata *object, const char *field_name)</arglist>
    </member>
    <member kind="function">
      <type>int</type>
      <name>FLAC__metadata_object_vorbiscomment_remove_entries_matching</name>
      <anchorfile>group__flac__metadata__object.html</anchorfile>
      <anchor>ga5a3ff5856098c449622ba850684aec75</anchor>
      <arglist>(FLAC__StreamMetadata *object, const char *field_name)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__StreamMetadata_CueSheet_Track *</type>
      <name>FLAC__metadata_object_cuesheet_track_new</name>
      <anchorfile>group__flac__metadata__object.html</anchorfile>
      <anchor>gafe2983a9c09685e34626cab39b3fb52c</anchor>
      <arglist>(void)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__StreamMetadata_CueSheet_Track *</type>
      <name>FLAC__metadata_object_cuesheet_track_clone</name>
      <anchorfile>group__flac__metadata__object.html</anchorfile>
      <anchor>ga1293d6df6daf2d65143d8bb40eed9261</anchor>
      <arglist>(const FLAC__StreamMetadata_CueSheet_Track *object)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>FLAC__metadata_object_cuesheet_track_delete</name>
      <anchorfile>group__flac__metadata__object.html</anchorfile>
      <anchor>gaa533fd7b72fa079e783de4b155b241ce</anchor>
      <arglist>(FLAC__StreamMetadata_CueSheet_Track *object)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_object_cuesheet_track_resize_indices</name>
      <anchorfile>group__flac__metadata__object.html</anchorfile>
      <anchor>ga484f21de7d533e4825cf807d29ef0204</anchor>
      <arglist>(FLAC__StreamMetadata *object, unsigned track_num, unsigned new_num_indices)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_object_cuesheet_track_insert_index</name>
      <anchorfile>group__flac__metadata__object.html</anchorfile>
      <anchor>gae8be6ad7b27a18c91eb0b91dc305e433</anchor>
      <arglist>(FLAC__StreamMetadata *object, unsigned track_num, unsigned index_num, FLAC__StreamMetadata_CueSheet_Index index)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_object_cuesheet_track_insert_blank_index</name>
      <anchorfile>group__flac__metadata__object.html</anchorfile>
      <anchor>gafd40ef6dcc277f99934deee5367cc627</anchor>
      <arglist>(FLAC__StreamMetadata *object, unsigned track_num, unsigned index_num)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_object_cuesheet_track_delete_index</name>
      <anchorfile>group__flac__metadata__object.html</anchorfile>
      <anchor>ga84584f2244b7d8597b8fec1d81ea5fb8</anchor>
      <arglist>(FLAC__StreamMetadata *object, unsigned track_num, unsigned index_num)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_object_cuesheet_resize_tracks</name>
      <anchorfile>group__flac__metadata__object.html</anchorfile>
      <anchor>gac99e38ed08f342665c63913bd0cc33fc</anchor>
      <arglist>(FLAC__StreamMetadata *object, unsigned new_num_tracks)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_object_cuesheet_set_track</name>
      <anchorfile>group__flac__metadata__object.html</anchorfile>
      <anchor>gaa4708c652be442b19227695621b62088</anchor>
      <arglist>(FLAC__StreamMetadata *object, unsigned track_num, FLAC__StreamMetadata_CueSheet_Track *track, FLAC__bool copy)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_object_cuesheet_insert_track</name>
      <anchorfile>group__flac__metadata__object.html</anchorfile>
      <anchor>gaa974947ae8ec1c86cbb155e0af7593e9</anchor>
      <arglist>(FLAC__StreamMetadata *object, unsigned track_num, FLAC__StreamMetadata_CueSheet_Track *track, FLAC__bool copy)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_object_cuesheet_insert_blank_track</name>
      <anchorfile>group__flac__metadata__object.html</anchorfile>
      <anchor>gab4a8c0855971e650df3331daf84d3fd1</anchor>
      <arglist>(FLAC__StreamMetadata *object, unsigned track_num)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_object_cuesheet_delete_track</name>
      <anchorfile>group__flac__metadata__object.html</anchorfile>
      <anchor>gae1912dfbc599c79732025fd5a5f279cc</anchor>
      <arglist>(FLAC__StreamMetadata *object, unsigned track_num)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_object_cuesheet_is_legal</name>
      <anchorfile>group__flac__metadata__object.html</anchorfile>
      <anchor>ga1a443d9299ce69694ad59bec4519d7b2</anchor>
      <arglist>(const FLAC__StreamMetadata *object, FLAC__bool check_cd_da_subset, const char **violation)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__uint32</type>
      <name>FLAC__metadata_object_cuesheet_calculate_cddb_id</name>
      <anchorfile>group__flac__metadata__object.html</anchorfile>
      <anchor>gaff2f825950b3e4dda4c8ddbf8e2f7ecd</anchor>
      <arglist>(const FLAC__StreamMetadata *object)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_object_picture_set_mime_type</name>
      <anchorfile>group__flac__metadata__object.html</anchorfile>
      <anchor>ga4511ae9ca994c9f4ab035a3c1aa98f45</anchor>
      <arglist>(FLAC__StreamMetadata *object, char *mime_type, FLAC__bool copy)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_object_picture_set_description</name>
      <anchorfile>group__flac__metadata__object.html</anchorfile>
      <anchor>ga293fe7d8b8b9e49d2414db0925b0f442</anchor>
      <arglist>(FLAC__StreamMetadata *object, FLAC__byte *description, FLAC__bool copy)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_object_picture_set_data</name>
      <anchorfile>group__flac__metadata__object.html</anchorfile>
      <anchor>ga00c330534ef8336ed92b30f9e676bb5f</anchor>
      <arglist>(FLAC__StreamMetadata *object, FLAC__byte *data, FLAC__uint32 length, FLAC__bool copy)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__metadata_object_picture_is_legal</name>
      <anchorfile>group__flac__metadata__object.html</anchorfile>
      <anchor>ga88268a5186e37d4b98b4df7870561128</anchor>
      <arglist>(const FLAC__StreamMetadata *object, const char **violation)</arglist>
    </member>
  </compound>
  <compound kind="group">
    <name>flac_decoder</name>
    <title>FLAC/_decoder.h: decoder interfaces</title>
    <filename>group__flac__decoder.html</filename>
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
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>ga7a5f593b9bc2d163884348b48c4285fd</anchor>
      <arglist>)(const FLAC__StreamDecoder *decoder, FLAC__byte buffer[], size_t *bytes, void *client_data)</arglist>
    </member>
    <member kind="typedef">
      <type>FLAC__StreamDecoderSeekStatus(*</type>
      <name>FLAC__StreamDecoderSeekCallback</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>ga375614289a1b868f1ead7fa70a581171</anchor>
      <arglist>)(const FLAC__StreamDecoder *decoder, FLAC__uint64 absolute_byte_offset, void *client_data)</arglist>
    </member>
    <member kind="typedef">
      <type>FLAC__StreamDecoderTellStatus(*</type>
      <name>FLAC__StreamDecoderTellCallback</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>ga02990309a9d30acc43ba01fe48021e39</anchor>
      <arglist>)(const FLAC__StreamDecoder *decoder, FLAC__uint64 *absolute_byte_offset, void *client_data)</arglist>
    </member>
    <member kind="typedef">
      <type>FLAC__StreamDecoderLengthStatus(*</type>
      <name>FLAC__StreamDecoderLengthCallback</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>ga453ffb5215a522fb74dc61d694e1453c</anchor>
      <arglist>)(const FLAC__StreamDecoder *decoder, FLAC__uint64 *stream_length, void *client_data)</arglist>
    </member>
    <member kind="typedef">
      <type>FLAC__bool(*</type>
      <name>FLAC__StreamDecoderEofCallback</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>gacc214f6b3cdae1c0f98577267ce19bdd</anchor>
      <arglist>)(const FLAC__StreamDecoder *decoder, void *client_data)</arglist>
    </member>
    <member kind="typedef">
      <type>FLAC__StreamDecoderWriteStatus(*</type>
      <name>FLAC__StreamDecoderWriteCallback</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>gaf98a4f9e2cac5747da6018c3dfc8dde1</anchor>
      <arglist>)(const FLAC__StreamDecoder *decoder, const FLAC__Frame *frame, const FLAC__int32 *const buffer[], void *client_data)</arglist>
    </member>
    <member kind="typedef">
      <type>void(*</type>
      <name>FLAC__StreamDecoderMetadataCallback</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>ga43e2329c15731c002ac4182a47990f85</anchor>
      <arglist>)(const FLAC__StreamDecoder *decoder, const FLAC__StreamMetadata *metadata, void *client_data)</arglist>
    </member>
    <member kind="typedef">
      <type>void(*</type>
      <name>FLAC__StreamDecoderErrorCallback</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>ga4fab6730ff0b22bf45ca4cd04d706569</anchor>
      <arglist>)(const FLAC__StreamDecoder *decoder, FLAC__StreamDecoderErrorStatus status, void *client_data)</arglist>
    </member>
    <member kind="enumeration">
      <type></type>
      <name>FLAC__StreamDecoderState</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>ga3adb6891c5871a87cd5bbae6c770ba2d</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_DECODER_SEARCH_FOR_METADATA</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>gga3adb6891c5871a87cd5bbae6c770ba2dacf4455f4f681a6737a553e10f614704a</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_DECODER_READ_METADATA</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>gga3adb6891c5871a87cd5bbae6c770ba2da4c1853ed1babdcede9a908e12cf7ccf7</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_DECODER_SEARCH_FOR_FRAME_SYNC</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>gga3adb6891c5871a87cd5bbae6c770ba2daccff915757978117720ba1613d088ddf</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_DECODER_READ_FRAME</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>gga3adb6891c5871a87cd5bbae6c770ba2da06dc6158a51a8eb9537b65f2fbb6dc49</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_DECODER_END_OF_STREAM</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>gga3adb6891c5871a87cd5bbae6c770ba2da28ce845052d9d1a780f4107e97f4c853</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_DECODER_OGG_ERROR</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>gga3adb6891c5871a87cd5bbae6c770ba2da3bc0343f47153c5779baf7f37f6e95cf</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_DECODER_SEEK_ERROR</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>gga3adb6891c5871a87cd5bbae6c770ba2daf2c6efcabdfe889081c2260e6681db49</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_DECODER_ABORTED</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>gga3adb6891c5871a87cd5bbae6c770ba2dadb52ab4785bd2eb84a95e8aa82311cd5</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_DECODER_MEMORY_ALLOCATION_ERROR</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>gga3adb6891c5871a87cd5bbae6c770ba2da0d08c527252420813e6a6d6d3e19324a</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_DECODER_UNINITIALIZED</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>gga3adb6891c5871a87cd5bbae6c770ba2da565eaf4d5e68b440ecec771cb22d3427</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumeration">
      <type></type>
      <name>FLAC__StreamDecoderInitStatus</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>gaaed54a24ac6310d29c5cafba79759c44</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_DECODER_INIT_STATUS_OK</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>ggaaed54a24ac6310d29c5cafba79759c44ac94c7e9396f30642f34805e5d626e011</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_DECODER_INIT_STATUS_UNSUPPORTED_CONTAINER</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>ggaaed54a24ac6310d29c5cafba79759c44a8f2188c616c9bc09638eece3ae55f152</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_DECODER_INIT_STATUS_INVALID_CALLBACKS</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>ggaaed54a24ac6310d29c5cafba79759c44a798ad4b6c4e556fd4cb1afbc29562eca</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_DECODER_INIT_STATUS_MEMORY_ALLOCATION_ERROR</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>ggaaed54a24ac6310d29c5cafba79759c44a0110567f0715c6f87357388bc7fa98f9</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_DECODER_INIT_STATUS_ERROR_OPENING_FILE</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>ggaaed54a24ac6310d29c5cafba79759c44a8184c306e0cd2565a8c5adc1381cb469</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_DECODER_INIT_STATUS_ALREADY_INITIALIZED</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>ggaaed54a24ac6310d29c5cafba79759c44a98bc501c9b2fb5d92d8bb0b3321d504f</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumeration">
      <type></type>
      <name>FLAC__StreamDecoderReadStatus</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>gad793ead451206c64a91dc0b851027b93</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_DECODER_READ_STATUS_CONTINUE</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>ggad793ead451206c64a91dc0b851027b93a9a5be0fcf0279b98b2fd462bc4871d06</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_DECODER_READ_STATUS_END_OF_STREAM</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>ggad793ead451206c64a91dc0b851027b93a0a0687d25dc9f7163e6e5e294672170f</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_DECODER_READ_STATUS_ABORT</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>ggad793ead451206c64a91dc0b851027b93a923123aebb349e35662e35a7621b7535</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumeration">
      <type></type>
      <name>FLAC__StreamDecoderSeekStatus</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>gac8d269e3c7af1a5889d3bd38409ed67d</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_DECODER_SEEK_STATUS_OK</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>ggac8d269e3c7af1a5889d3bd38409ed67daca58132d896ad7755827d3f2b72488cc</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_DECODER_SEEK_STATUS_ERROR</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>ggac8d269e3c7af1a5889d3bd38409ed67da969ce92a42a2a95609452e9cf01fcc09</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_DECODER_SEEK_STATUS_UNSUPPORTED</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>ggac8d269e3c7af1a5889d3bd38409ed67da4a01f1e48baf015e78535cc20683ec53</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumeration">
      <type></type>
      <name>FLAC__StreamDecoderTellStatus</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>ga83708207969383bd7b5c1e9148528845</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_DECODER_TELL_STATUS_OK</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>gga83708207969383bd7b5c1e9148528845a516a202ebf4bb61d4a1fb5b029a104dd</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_DECODER_TELL_STATUS_ERROR</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>gga83708207969383bd7b5c1e9148528845aceefd3feb853d5e68a149f2bdd1a9db1</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_DECODER_TELL_STATUS_UNSUPPORTED</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>gga83708207969383bd7b5c1e9148528845add75538234493c9f7a20a846a223ca91</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumeration">
      <type></type>
      <name>FLAC__StreamDecoderLengthStatus</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>gad5860157c2bb34501b8b9370472d727a</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_DECODER_LENGTH_STATUS_OK</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>ggad5860157c2bb34501b8b9370472d727aaef01bfcdc3099686e106d8f88397653d</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_DECODER_LENGTH_STATUS_ERROR</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>ggad5860157c2bb34501b8b9370472d727aab000e31c0c20c0d19df4f2203b01ea23</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_DECODER_LENGTH_STATUS_UNSUPPORTED</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>ggad5860157c2bb34501b8b9370472d727aae35949f46f887e6d826fe0fe4b2a32c1</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumeration">
      <type></type>
      <name>FLAC__StreamDecoderWriteStatus</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>ga73f67eb9e0ab57945afe038751bc62c8</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_DECODER_WRITE_STATUS_CONTINUE</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>gga73f67eb9e0ab57945afe038751bc62c8acea48326e0ab8370d2814f4126fcb84e</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_DECODER_WRITE_STATUS_ABORT</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>gga73f67eb9e0ab57945afe038751bc62c8a23bd6bfec34af704e0d5ea273f14d95d</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumeration">
      <type></type>
      <name>FLAC__StreamDecoderErrorStatus</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>ga130e70bd9a73d3c2416247a3e5132ecf</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_DECODER_ERROR_STATUS_LOST_SYNC</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>gga130e70bd9a73d3c2416247a3e5132ecfa3ceec2a553dc142ad487ae88eb6f7222</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_DECODER_ERROR_STATUS_BAD_HEADER</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>gga130e70bd9a73d3c2416247a3e5132ecfae393a9b91a6b2f23398675b5b57e1e86</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_DECODER_ERROR_STATUS_FRAME_CRC_MISMATCH</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>gga130e70bd9a73d3c2416247a3e5132ecfa208fe77a04e6ff684e50f0eae1214e26</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_DECODER_ERROR_STATUS_UNPARSEABLE_STREAM</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>gga130e70bd9a73d3c2416247a3e5132ecfa8b6864ad65edd8fea039838b6d3e5575</anchor>
      <arglist></arglist>
    </member>
    <member kind="function">
      <type>FLAC__StreamDecoder *</type>
      <name>FLAC__stream_decoder_new</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>ga529c3c1e46417570767fb8e4c76f5477</anchor>
      <arglist>(void)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>FLAC__stream_decoder_delete</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>gad9cf299956da091111d13e83517d8c44</anchor>
      <arglist>(FLAC__StreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_decoder_set_ogg_serial_number</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>ga7fd232e7a2b5070bd26450487edbc2a1</anchor>
      <arglist>(FLAC__StreamDecoder *decoder, long serial_number)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_decoder_set_md5_checking</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>ga8f402243eed54f400ddd2f296ff54497</anchor>
      <arglist>(FLAC__StreamDecoder *decoder, FLAC__bool value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_decoder_set_metadata_respond</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>gad4e685f3d055f70fbaed9ffa4f70f74b</anchor>
      <arglist>(FLAC__StreamDecoder *decoder, FLAC__MetadataType type)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_decoder_set_metadata_respond_application</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>gaee1196ff5fa97df9810f708dc2bc8326</anchor>
      <arglist>(FLAC__StreamDecoder *decoder, const FLAC__byte id[4])</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_decoder_set_metadata_respond_all</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>ga1ce03d8f305a818ff9a573473af99dc4</anchor>
      <arglist>(FLAC__StreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_decoder_set_metadata_ignore</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>gad75f067720da89c4e9d96dedc45f73e6</anchor>
      <arglist>(FLAC__StreamDecoder *decoder, FLAC__MetadataType type)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_decoder_set_metadata_ignore_application</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>gaab41e8bc505b24df4912de53de06b085</anchor>
      <arglist>(FLAC__StreamDecoder *decoder, const FLAC__byte id[4])</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_decoder_set_metadata_ignore_all</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>gaa1307f07fae5d7a4a0c18beeae7ec5e6</anchor>
      <arglist>(FLAC__StreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__StreamDecoderState</type>
      <name>FLAC__stream_decoder_get_state</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>gaf99dac2d9255f7db4df8a6d9974a9a9a</anchor>
      <arglist>(const FLAC__StreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>const char *</type>
      <name>FLAC__stream_decoder_get_resolved_state_string</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>gad28257412951ca266751a19e2cf54be2</anchor>
      <arglist>(const FLAC__StreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_decoder_get_md5_checking</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>gae27a6b30b55beda03559c12a5df21537</anchor>
      <arglist>(const FLAC__StreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__uint64</type>
      <name>FLAC__stream_decoder_get_total_samples</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>ga930d9b591fcfaea74359c722cdfb980c</anchor>
      <arglist>(const FLAC__StreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>FLAC__stream_decoder_get_channels</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>gadefff696d65e72afeb2f058e45066be1</anchor>
      <arglist>(const FLAC__StreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__ChannelAssignment</type>
      <name>FLAC__stream_decoder_get_channel_assignment</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>gae62fdf93c1fedd5fea9258ecdc78bb53</anchor>
      <arglist>(const FLAC__StreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>FLAC__stream_decoder_get_bits_per_sample</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>ga51dc941b24bfe01f8311c9aba86e77f8</anchor>
      <arglist>(const FLAC__StreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>FLAC__stream_decoder_get_sample_rate</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>ga1b0170d020b47c1b841ce8437b564429</anchor>
      <arglist>(const FLAC__StreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>FLAC__stream_decoder_get_blocksize</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>ga568a710a87dbd051b0edba6b53ae52ee</anchor>
      <arglist>(const FLAC__StreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_decoder_get_decode_position</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>gaffd9b0d0832ed01e6d75930b5391def5</anchor>
      <arglist>(const FLAC__StreamDecoder *decoder, FLAC__uint64 *position)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__StreamDecoderInitStatus</type>
      <name>FLAC__stream_decoder_init_stream</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>ga150d381abc5249168e439bc076544b29</anchor>
      <arglist>(FLAC__StreamDecoder *decoder, FLAC__StreamDecoderReadCallback read_callback, FLAC__StreamDecoderSeekCallback seek_callback, FLAC__StreamDecoderTellCallback tell_callback, FLAC__StreamDecoderLengthCallback length_callback, FLAC__StreamDecoderEofCallback eof_callback, FLAC__StreamDecoderWriteCallback write_callback, FLAC__StreamDecoderMetadataCallback metadata_callback, FLAC__StreamDecoderErrorCallback error_callback, void *client_data)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__StreamDecoderInitStatus</type>
      <name>FLAC__stream_decoder_init_ogg_stream</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>ga1b043adeb805c779c1e97cb68959d1ab</anchor>
      <arglist>(FLAC__StreamDecoder *decoder, FLAC__StreamDecoderReadCallback read_callback, FLAC__StreamDecoderSeekCallback seek_callback, FLAC__StreamDecoderTellCallback tell_callback, FLAC__StreamDecoderLengthCallback length_callback, FLAC__StreamDecoderEofCallback eof_callback, FLAC__StreamDecoderWriteCallback write_callback, FLAC__StreamDecoderMetadataCallback metadata_callback, FLAC__StreamDecoderErrorCallback error_callback, void *client_data)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__StreamDecoderInitStatus</type>
      <name>FLAC__stream_decoder_init_FILE</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>ga80aa83631460a53263c84e654586dff0</anchor>
      <arglist>(FLAC__StreamDecoder *decoder, FILE *file, FLAC__StreamDecoderWriteCallback write_callback, FLAC__StreamDecoderMetadataCallback metadata_callback, FLAC__StreamDecoderErrorCallback error_callback, void *client_data)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__StreamDecoderInitStatus</type>
      <name>FLAC__stream_decoder_init_ogg_FILE</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>ga4cc7fbaf905c24d6db48b53b7942fe72</anchor>
      <arglist>(FLAC__StreamDecoder *decoder, FILE *file, FLAC__StreamDecoderWriteCallback write_callback, FLAC__StreamDecoderMetadataCallback metadata_callback, FLAC__StreamDecoderErrorCallback error_callback, void *client_data)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__StreamDecoderInitStatus</type>
      <name>FLAC__stream_decoder_init_file</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>ga4021ead5cff29fd589c915756f902f1a</anchor>
      <arglist>(FLAC__StreamDecoder *decoder, const char *filename, FLAC__StreamDecoderWriteCallback write_callback, FLAC__StreamDecoderMetadataCallback metadata_callback, FLAC__StreamDecoderErrorCallback error_callback, void *client_data)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__StreamDecoderInitStatus</type>
      <name>FLAC__stream_decoder_init_ogg_file</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>ga548f15d7724f3bff7f2608abe8b12f6c</anchor>
      <arglist>(FLAC__StreamDecoder *decoder, const char *filename, FLAC__StreamDecoderWriteCallback write_callback, FLAC__StreamDecoderMetadataCallback metadata_callback, FLAC__StreamDecoderErrorCallback error_callback, void *client_data)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_decoder_finish</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>ga96c47c96920f363cd0972b54067818a9</anchor>
      <arglist>(FLAC__StreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_decoder_flush</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>ga95570a455e582b2ab46ab9bb529f26ac</anchor>
      <arglist>(FLAC__StreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_decoder_reset</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>gaa4183c2d925d5a5edddde9d1ca145725</anchor>
      <arglist>(FLAC__StreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_decoder_process_single</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>ga9d6df4a39892c05955122cf7f987f856</anchor>
      <arglist>(FLAC__StreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_decoder_process_until_end_of_metadata</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>ga027ffb5b75dc39b3d26f55c5e6b42682</anchor>
      <arglist>(FLAC__StreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_decoder_process_until_end_of_stream</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>ga89a0723812fa6ef7cdb173715f1bc81f</anchor>
      <arglist>(FLAC__StreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_decoder_skip_single_frame</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>ga85b666aba976f29e8dd9d7956fce4301</anchor>
      <arglist>(FLAC__StreamDecoder *decoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_decoder_seek_absolute</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>ga6a2eb6072b9fafefc3f80f1959805ccb</anchor>
      <arglist>(FLAC__StreamDecoder *decoder, FLAC__uint64 sample)</arglist>
    </member>
    <member kind="variable">
      <type>const char *const</type>
      <name>FLAC__StreamDecoderStateString</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>gac192360ac435614394bf43235cb7981e</anchor>
      <arglist>[]</arglist>
    </member>
    <member kind="variable">
      <type>const char *const</type>
      <name>FLAC__StreamDecoderInitStatusString</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>ga0effa1d3031c3206a1719faf984a4f21</anchor>
      <arglist>[]</arglist>
    </member>
    <member kind="variable">
      <type>const char *const</type>
      <name>FLAC__StreamDecoderReadStatusString</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>gab1ee941839b05045ae1d73ee0fdcb8c9</anchor>
      <arglist>[]</arglist>
    </member>
    <member kind="variable">
      <type>const char *const</type>
      <name>FLAC__StreamDecoderSeekStatusString</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>gac49aff0593584b7ed5fd0b2508f824fc</anchor>
      <arglist>[]</arglist>
    </member>
    <member kind="variable">
      <type>const char *const</type>
      <name>FLAC__StreamDecoderTellStatusString</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>ga3c1b7d5a174d6c2e6bcf1b9a87b5a5cb</anchor>
      <arglist>[]</arglist>
    </member>
    <member kind="variable">
      <type>const char *const</type>
      <name>FLAC__StreamDecoderLengthStatusString</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>ga792933fa9e8b65bfcac62d82e52415f5</anchor>
      <arglist>[]</arglist>
    </member>
    <member kind="variable">
      <type>const char *const</type>
      <name>FLAC__StreamDecoderWriteStatusString</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>ga9df7f0fd8cf9888f97a52b5f3f33cdb0</anchor>
      <arglist>[]</arglist>
    </member>
    <member kind="variable">
      <type>const char *const</type>
      <name>FLAC__StreamDecoderErrorStatusString</name>
      <anchorfile>group__flac__stream__decoder.html</anchorfile>
      <anchor>gac428c69b084529322df05ee793440b88</anchor>
      <arglist>[]</arglist>
    </member>
  </compound>
  <compound kind="group">
    <name>flac_encoder</name>
    <title>FLAC/_encoder.h: encoder interfaces</title>
    <filename>group__flac__encoder.html</filename>
    <subgroup>flac_stream_encoder</subgroup>
  </compound>
  <compound kind="group">
    <name>flac_stream_encoder</name>
    <title>FLAC/stream_encoder.h: stream encoder interface</title>
    <filename>group__flac__stream__encoder.html</filename>
    <class kind="struct">FLAC__StreamEncoder</class>
    <member kind="typedef">
      <type>FLAC__StreamEncoderReadStatus(*</type>
      <name>FLAC__StreamEncoderReadCallback</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ga97d25c75f49897422d93a9d8405043cd</anchor>
      <arglist>)(const FLAC__StreamEncoder *encoder, FLAC__byte buffer[], size_t *bytes, void *client_data)</arglist>
    </member>
    <member kind="typedef">
      <type>FLAC__StreamEncoderWriteStatus(*</type>
      <name>FLAC__StreamEncoderWriteCallback</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>gabf2f9bb39c806111c83dd16936ff6d09</anchor>
      <arglist>)(const FLAC__StreamEncoder *encoder, const FLAC__byte buffer[], size_t bytes, unsigned samples, unsigned current_frame, void *client_data)</arglist>
    </member>
    <member kind="typedef">
      <type>FLAC__StreamEncoderSeekStatus(*</type>
      <name>FLAC__StreamEncoderSeekCallback</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ga3005a69a7883da53262ec8a124d48c9e</anchor>
      <arglist>)(const FLAC__StreamEncoder *encoder, FLAC__uint64 absolute_byte_offset, void *client_data)</arglist>
    </member>
    <member kind="typedef">
      <type>FLAC__StreamEncoderTellStatus(*</type>
      <name>FLAC__StreamEncoderTellCallback</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ga4cab0b7556d8509a9f74693804c8c86e</anchor>
      <arglist>)(const FLAC__StreamEncoder *encoder, FLAC__uint64 *absolute_byte_offset, void *client_data)</arglist>
    </member>
    <member kind="typedef">
      <type>void(*</type>
      <name>FLAC__StreamEncoderMetadataCallback</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ga87778e16cdd0834a301ee8d8258cf946</anchor>
      <arglist>)(const FLAC__StreamEncoder *encoder, const FLAC__StreamMetadata *metadata, void *client_data)</arglist>
    </member>
    <member kind="typedef">
      <type>void(*</type>
      <name>FLAC__StreamEncoderProgressCallback</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>gac65f8ae0583b665933744b60fd5ba0d9</anchor>
      <arglist>)(const FLAC__StreamEncoder *encoder, FLAC__uint64 bytes_written, FLAC__uint64 samples_written, unsigned frames_written, unsigned total_frames_estimate, void *client_data)</arglist>
    </member>
    <member kind="enumeration">
      <type></type>
      <name>FLAC__StreamEncoderState</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>gac5e9db4fc32ca2fa74abd9c8a87c02a5</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_ENCODER_OK</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ggac5e9db4fc32ca2fa74abd9c8a87c02a5a3a6666ae61a64d955341cec285695bf6</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_ENCODER_UNINITIALIZED</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ggac5e9db4fc32ca2fa74abd9c8a87c02a5a04912e04a3c57d3c53de34742f96d635</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_ENCODER_OGG_ERROR</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ggac5e9db4fc32ca2fa74abd9c8a87c02a5abb312cc8318c7a541cadacd23ceb3bbb</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_ENCODER_VERIFY_DECODER_ERROR</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ggac5e9db4fc32ca2fa74abd9c8a87c02a5a4cb80be4f83eb71f04e74968af1d259e</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_ENCODER_VERIFY_MISMATCH_IN_AUDIO_DATA</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ggac5e9db4fc32ca2fa74abd9c8a87c02a5a011e3d8b2d02a940bfd0e59c05cf5ae0</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_ENCODER_CLIENT_ERROR</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ggac5e9db4fc32ca2fa74abd9c8a87c02a5a8c2b2e9efb43a4f9b25b1d2bd9af5f23</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_ENCODER_IO_ERROR</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ggac5e9db4fc32ca2fa74abd9c8a87c02a5af0e4738522e05a7248435c7148f58f91</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_ENCODER_FRAMING_ERROR</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ggac5e9db4fc32ca2fa74abd9c8a87c02a5a2c2937b7f1600a4ac7c84fc70ab34cf1</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_ENCODER_MEMORY_ALLOCATION_ERROR</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ggac5e9db4fc32ca2fa74abd9c8a87c02a5a35db99d9958bd6c2301a04715fbc44fd</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumeration">
      <type></type>
      <name>FLAC__StreamEncoderInitStatus</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ga3bb869620af2b188d77982a5c30b047d</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_ENCODER_INIT_STATUS_OK</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>gga3bb869620af2b188d77982a5c30b047da20501dce552da74c5df935eeaa0c9ee3</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_ENCODER_INIT_STATUS_ENCODER_ERROR</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>gga3bb869620af2b188d77982a5c30b047da9c64e5f9020d8799e1cd9d39d50e6955</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_ENCODER_INIT_STATUS_UNSUPPORTED_CONTAINER</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>gga3bb869620af2b188d77982a5c30b047da8a822b011de88b67c114505ffef39327</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_ENCODER_INIT_STATUS_INVALID_CALLBACKS</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>gga3bb869620af2b188d77982a5c30b047dac2cf461f02e20513003b8cadeae03f9f</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_ENCODER_INIT_STATUS_INVALID_NUMBER_OF_CHANNELS</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>gga3bb869620af2b188d77982a5c30b047da0541c4f827f081b9f1c54c9441e4aa65</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_ENCODER_INIT_STATUS_INVALID_BITS_PER_SAMPLE</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>gga3bb869620af2b188d77982a5c30b047dad6d2631f464183c0c165155200882e6b</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_ENCODER_INIT_STATUS_INVALID_SAMPLE_RATE</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>gga3bb869620af2b188d77982a5c30b047da6fdcde9e18c37450c79e8f12b9d9c134</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_ENCODER_INIT_STATUS_INVALID_BLOCK_SIZE</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>gga3bb869620af2b188d77982a5c30b047da652c445f1bd8b6cfb963a30bf416c95a</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_ENCODER_INIT_STATUS_INVALID_MAX_LPC_ORDER</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>gga3bb869620af2b188d77982a5c30b047da38a69e94b3333e4ba779d2ff8f43f64e</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_ENCODER_INIT_STATUS_INVALID_QLP_COEFF_PRECISION</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>gga3bb869620af2b188d77982a5c30b047da5be80403bd7a43450139442e0f34ad7e</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_ENCODER_INIT_STATUS_BLOCK_SIZE_TOO_SMALL_FOR_LPC_ORDER</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>gga3bb869620af2b188d77982a5c30b047da62a17a3ed3c05ddf8ea7f6fecbd4e4a1</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_ENCODER_INIT_STATUS_NOT_STREAMABLE</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>gga3bb869620af2b188d77982a5c30b047daa793405c858c7606539082750080a47e</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_ENCODER_INIT_STATUS_INVALID_METADATA</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>gga3bb869620af2b188d77982a5c30b047daa85afdd1849c75a19594416cef63e3e9</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_ENCODER_INIT_STATUS_ALREADY_INITIALIZED</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>gga3bb869620af2b188d77982a5c30b047dab4e7b50d176a127575df90383cb15e1d</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumeration">
      <type></type>
      <name>FLAC__StreamEncoderReadStatus</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ga2e81f007fb0a7414c0bbb453f37ea37f</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_ENCODER_READ_STATUS_CONTINUE</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>gga2e81f007fb0a7414c0bbb453f37ea37fa4bdd691d3666f19ec96ff99402347a2e</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_ENCODER_READ_STATUS_END_OF_STREAM</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>gga2e81f007fb0a7414c0bbb453f37ea37fa562fef84bf86a9a39682e23066d9cfee</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_ENCODER_READ_STATUS_ABORT</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>gga2e81f007fb0a7414c0bbb453f37ea37fa69b94eeab60e07d5fd33f2b3c8b85759</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_ENCODER_READ_STATUS_UNSUPPORTED</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>gga2e81f007fb0a7414c0bbb453f37ea37fa9bb730b8f6354cc1e810017a2f700316</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumeration">
      <type></type>
      <name>FLAC__StreamEncoderWriteStatus</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ga3737471fd49730bb8cf9b182bdeda05e</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_ENCODER_WRITE_STATUS_OK</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>gga3737471fd49730bb8cf9b182bdeda05ea5622e0199f0203c402fcb7b4ca76f808</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_ENCODER_WRITE_STATUS_FATAL_ERROR</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>gga3737471fd49730bb8cf9b182bdeda05ea18e7cd6a443fb8bd303c3ba89946bc85</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumeration">
      <type></type>
      <name>FLAC__StreamEncoderSeekStatus</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ga6d5be3489f45fcf0c252022c65d87aca</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_ENCODER_SEEK_STATUS_OK</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>gga6d5be3489f45fcf0c252022c65d87acaa99853066610d798627888ec2e5afa667</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_ENCODER_SEEK_STATUS_ERROR</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>gga6d5be3489f45fcf0c252022c65d87acaabf93227938b4e1bf3656fe4ba4159c60</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_ENCODER_SEEK_STATUS_UNSUPPORTED</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>gga6d5be3489f45fcf0c252022c65d87acaa8930179a426134caf30a70147448f037</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumeration">
      <type></type>
      <name>FLAC__StreamEncoderTellStatus</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>gab628f63181250eb977a28bf12b7dd9ff</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_ENCODER_TELL_STATUS_OK</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ggab628f63181250eb977a28bf12b7dd9ffa48e071d89494ac8f5471e7c0d7a6f43b</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_ENCODER_TELL_STATUS_ERROR</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ggab628f63181250eb977a28bf12b7dd9ffaf638882e04d7c58e6c29dcc7f410864b</anchor>
      <arglist></arglist>
    </member>
    <member kind="enumvalue">
      <type>@</type>
      <name>FLAC__STREAM_ENCODER_TELL_STATUS_UNSUPPORTED</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ggab628f63181250eb977a28bf12b7dd9ffa9d6bbd317f85fd2d6fc72f64e3cb56e7</anchor>
      <arglist></arglist>
    </member>
    <member kind="function">
      <type>FLAC__StreamEncoder *</type>
      <name>FLAC__stream_encoder_new</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>gab09f7620a0ba9c30020c189ce112a52f</anchor>
      <arglist>(void)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>FLAC__stream_encoder_delete</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ga7212e6846f543618b6289666de216b29</anchor>
      <arglist>(FLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_set_ogg_serial_number</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>gaf4f75f7689b6b3fff16b03028aa38326</anchor>
      <arglist>(FLAC__StreamEncoder *encoder, long serial_number)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_set_verify</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ga795be6527a9eb1219331afef2f182a41</anchor>
      <arglist>(FLAC__StreamEncoder *encoder, FLAC__bool value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_set_streamable_subset</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ga35a18815a58141b88db02317892d059b</anchor>
      <arglist>(FLAC__StreamEncoder *encoder, FLAC__bool value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_set_channels</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>gab151c81577dc385196b10727ec24459d</anchor>
      <arglist>(FLAC__StreamEncoder *encoder, unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_set_bits_per_sample</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>gabfa3c989377785cda7c496b69dcb98cb</anchor>
      <arglist>(FLAC__StreamEncoder *encoder, unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_set_sample_rate</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ga07fc8c7806381a055a1eef26387e509f</anchor>
      <arglist>(FLAC__StreamEncoder *encoder, unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_set_compression_level</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>gae49cf32f5256cb47eecd33779493ac85</anchor>
      <arglist>(FLAC__StreamEncoder *encoder, unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_set_blocksize</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ga3f2d344e090d0f6df5dfe8825c28bd61</anchor>
      <arglist>(FLAC__StreamEncoder *encoder, unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_set_do_mid_side_stereo</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ga3bff001a1efc2e4eb520c954066330f4</anchor>
      <arglist>(FLAC__StreamEncoder *encoder, FLAC__bool value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_set_loose_mid_side_stereo</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ga7965d51b93f14cbd6ad5bb9d34f10536</anchor>
      <arglist>(FLAC__StreamEncoder *encoder, FLAC__bool value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_set_apodization</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ga6598f09ac782a1f2a5743ddf247c81c8</anchor>
      <arglist>(FLAC__StreamEncoder *encoder, const char *specification)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_set_max_lpc_order</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>gac256b4ef560fcf099c02eb52bb6c30e9</anchor>
      <arglist>(FLAC__StreamEncoder *encoder, unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_set_qlp_coeff_precision</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>gaa325049ebb02d6fbe820d2268850c6de</anchor>
      <arglist>(FLAC__StreamEncoder *encoder, unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_set_do_qlp_coeff_prec_search</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ga495890067203958e5d67a641f8757b1c</anchor>
      <arglist>(FLAC__StreamEncoder *encoder, FLAC__bool value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_set_do_escape_coding</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>gaed594c373d829f77808a935c54a25fa4</anchor>
      <arglist>(FLAC__StreamEncoder *encoder, FLAC__bool value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_set_do_exhaustive_model_search</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ga054313e7f6eaf5c6122d82c6a8b3b808</anchor>
      <arglist>(FLAC__StreamEncoder *encoder, FLAC__bool value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_set_min_residual_partition_order</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ga31867a9cf8bc7276942381e4a8145969</anchor>
      <arglist>(FLAC__StreamEncoder *encoder, unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_set_max_residual_partition_order</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>gac2e2147be6e4edf68e02d011349fa08c</anchor>
      <arglist>(FLAC__StreamEncoder *encoder, unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_set_rice_parameter_search_dist</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ga11f0c589113b17507c0a620b7872036c</anchor>
      <arglist>(FLAC__StreamEncoder *encoder, unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_set_total_samples_estimate</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>gab943094585d1c0a4bec497e73567cf85</anchor>
      <arglist>(FLAC__StreamEncoder *encoder, FLAC__uint64 value)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_set_metadata</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ga9c1098e664d7997947493901ed869b64</anchor>
      <arglist>(FLAC__StreamEncoder *encoder, FLAC__StreamMetadata **metadata, unsigned num_blocks)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__StreamEncoderState</type>
      <name>FLAC__stream_encoder_get_state</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ga0803321b37189dc5eea4fe1cea25c29a</anchor>
      <arglist>(const FLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__StreamDecoderState</type>
      <name>FLAC__stream_encoder_get_verify_decoder_state</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ga820704b95a711e77d55363e8753f9f9f</anchor>
      <arglist>(const FLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>const char *</type>
      <name>FLAC__stream_encoder_get_resolved_state_string</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ga0916f813358eb6f1e44148353acd4d42</anchor>
      <arglist>(const FLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>FLAC__stream_encoder_get_verify_decoder_error_stats</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ga48197fd6507314858222a8e6903292ba</anchor>
      <arglist>(const FLAC__StreamEncoder *encoder, FLAC__uint64 *absolute_sample, unsigned *frame_number, unsigned *channel, unsigned *sample, FLAC__int32 *expected, FLAC__int32 *got)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_get_verify</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ga9efc4964992e001bcec0a8eaedee8d60</anchor>
      <arglist>(const FLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_get_streamable_subset</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ga201e64032ea4298b2379c93652b28245</anchor>
      <arglist>(const FLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>FLAC__stream_encoder_get_channels</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ga535991ba463573d7ad2271ea943051e8</anchor>
      <arglist>(const FLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>FLAC__stream_encoder_get_bits_per_sample</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ga44afb2b8e2623b1dff02e4116c6c58cb</anchor>
      <arglist>(const FLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>FLAC__stream_encoder_get_sample_rate</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ga830bc5489b60fbe814a683a8c4ebc81d</anchor>
      <arglist>(const FLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>FLAC__stream_encoder_get_blocksize</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ga21da057fd9c30bc2475b9bd18b9a1504</anchor>
      <arglist>(const FLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_get_do_mid_side_stereo</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ga32da1f89997ab94ce5d677fcd7e24d56</anchor>
      <arglist>(const FLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_get_loose_mid_side_stereo</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ga1455859cf3d233bd4dfff86af010f4fa</anchor>
      <arglist>(const FLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>FLAC__stream_encoder_get_max_lpc_order</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>gadce7c7d6af89c3bbac0bb0c76d6e257b</anchor>
      <arglist>(const FLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>FLAC__stream_encoder_get_qlp_coeff_precision</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ga5b311992ac47554ae90c8f5a8ed745a7</anchor>
      <arglist>(const FLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_get_do_qlp_coeff_prec_search</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ga65bee5a769d4c5fdc95b81c2fb95061c</anchor>
      <arglist>(const FLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_get_do_escape_coding</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ga0c944049800991422c1bfb3b1c0567a5</anchor>
      <arglist>(const FLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_get_do_exhaustive_model_search</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ga7bc8b32f58df5564db4b6114cb11042d</anchor>
      <arglist>(const FLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>FLAC__stream_encoder_get_min_residual_partition_order</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ga1f29fb94e5c54f1d46ad31c2af0dc5ac</anchor>
      <arglist>(const FLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>FLAC__stream_encoder_get_max_residual_partition_order</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ga55773d82163302c52936299068510e9d</anchor>
      <arglist>(const FLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>FLAC__stream_encoder_get_rice_parameter_search_dist</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>gafd70690deb118136ca4baed350d99cb8</anchor>
      <arglist>(const FLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__uint64</type>
      <name>FLAC__stream_encoder_get_total_samples_estimate</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>gaa22d8935bd985b9cccf6592160ffc6f2</anchor>
      <arglist>(const FLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__StreamEncoderInitStatus</type>
      <name>FLAC__stream_encoder_init_stream</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ga7d801879812b48fcbc40f409800c453c</anchor>
      <arglist>(FLAC__StreamEncoder *encoder, FLAC__StreamEncoderWriteCallback write_callback, FLAC__StreamEncoderSeekCallback seek_callback, FLAC__StreamEncoderTellCallback tell_callback, FLAC__StreamEncoderMetadataCallback metadata_callback, void *client_data)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__StreamEncoderInitStatus</type>
      <name>FLAC__stream_encoder_init_ogg_stream</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ga9d1981bcd30b8db4d73b5466be5570f5</anchor>
      <arglist>(FLAC__StreamEncoder *encoder, FLAC__StreamEncoderReadCallback read_callback, FLAC__StreamEncoderWriteCallback write_callback, FLAC__StreamEncoderSeekCallback seek_callback, FLAC__StreamEncoderTellCallback tell_callback, FLAC__StreamEncoderMetadataCallback metadata_callback, void *client_data)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__StreamEncoderInitStatus</type>
      <name>FLAC__stream_encoder_init_FILE</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ga12789a1c4a4e31cd2e7187259fe127f8</anchor>
      <arglist>(FLAC__StreamEncoder *encoder, FILE *file, FLAC__StreamEncoderProgressCallback progress_callback, void *client_data)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__StreamEncoderInitStatus</type>
      <name>FLAC__stream_encoder_init_ogg_FILE</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ga57fc668f50ffd99a93df326bfab5e2b1</anchor>
      <arglist>(FLAC__StreamEncoder *encoder, FILE *file, FLAC__StreamEncoderProgressCallback progress_callback, void *client_data)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__StreamEncoderInitStatus</type>
      <name>FLAC__stream_encoder_init_file</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ga9d5117c2ac0eeb572784116bf2eb541b</anchor>
      <arglist>(FLAC__StreamEncoder *encoder, const char *filename, FLAC__StreamEncoderProgressCallback progress_callback, void *client_data)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__StreamEncoderInitStatus</type>
      <name>FLAC__stream_encoder_init_ogg_file</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ga4891de2f56045941ae222b61b0fd83a4</anchor>
      <arglist>(FLAC__StreamEncoder *encoder, const char *filename, FLAC__StreamEncoderProgressCallback progress_callback, void *client_data)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_finish</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ga3522f9de5af29807df1b9780a418b7f3</anchor>
      <arglist>(FLAC__StreamEncoder *encoder)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_process</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>gad462406ff4d5a985eac6b4aa5472df57</anchor>
      <arglist>(FLAC__StreamEncoder *encoder, const FLAC__int32 *const buffer[], unsigned samples)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__bool</type>
      <name>FLAC__stream_encoder_process_interleaved</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ga4c56bc287efb1a0f69b117a84401baad</anchor>
      <arglist>(FLAC__StreamEncoder *encoder, const FLAC__int32 buffer[], unsigned samples)</arglist>
    </member>
    <member kind="variable">
      <type>const char *const</type>
      <name>FLAC__StreamEncoderStateString</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ga1410b7a076b0c8401682f9f812b66df5</anchor>
      <arglist>[]</arglist>
    </member>
    <member kind="variable">
      <type>const char *const</type>
      <name>FLAC__StreamEncoderInitStatusString</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ga0ec1fa7b3f55b4f07a2727846c285776</anchor>
      <arglist>[]</arglist>
    </member>
    <member kind="variable">
      <type>const char *const</type>
      <name>FLAC__StreamEncoderReadStatusString</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ga1654422c81846b9b399ac5fb98df61dd</anchor>
      <arglist>[]</arglist>
    </member>
    <member kind="variable">
      <type>const char *const</type>
      <name>FLAC__StreamEncoderWriteStatusString</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>ga9f64480accd01525cbfa25c11e6bb74e</anchor>
      <arglist>[]</arglist>
    </member>
    <member kind="variable">
      <type>const char *const</type>
      <name>FLAC__StreamEncoderSeekStatusString</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>gabb137b2d787756bf97398f0b60e54c20</anchor>
      <arglist>[]</arglist>
    </member>
    <member kind="variable">
      <type>const char *const</type>
      <name>FLAC__StreamEncoderTellStatusString</name>
      <anchorfile>group__flac__stream__encoder.html</anchorfile>
      <anchor>gaf8ab921ae968be2be255be1f136e1eec</anchor>
      <arglist>[]</arglist>
    </member>
  </compound>
  <compound kind="group">
    <name>flacpp</name>
    <title>FLAC C++ API</title>
    <filename>group__flacpp.html</filename>
    <subgroup>flacpp_decoder</subgroup>
    <subgroup>flacpp_encoder</subgroup>
    <subgroup>flacpp_export</subgroup>
    <subgroup>flacpp_metadata</subgroup>
  </compound>
  <compound kind="group">
    <name>flacpp_decoder</name>
    <title>FLAC++/decoder.h: decoder classes</title>
    <filename>group__flacpp__decoder.html</filename>
    <class kind="class">FLAC::Decoder::Stream</class>
    <class kind="class">FLAC::Decoder::Stream::State</class>
    <class kind="class">FLAC::Decoder::File</class>
  </compound>
  <compound kind="group">
    <name>flacpp_encoder</name>
    <title>FLAC++/encoder.h: encoder classes</title>
    <filename>group__flacpp__encoder.html</filename>
    <class kind="class">FLAC::Encoder::Stream</class>
    <class kind="class">FLAC::Encoder::Stream::State</class>
    <class kind="class">FLAC::Encoder::File</class>
  </compound>
  <compound kind="group">
    <name>flacpp_export</name>
    <title>FLAC++/export.h: export symbols</title>
    <filename>group__flacpp__export.html</filename>
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
    <class kind="class">FLAC::Metadata::VorbisComment::Entry</class>
    <class kind="class">FLAC::Metadata::CueSheet</class>
    <class kind="class">FLAC::Metadata::CueSheet::Track</class>
    <class kind="class">FLAC::Metadata::Picture</class>
    <class kind="class">FLAC::Metadata::Unknown</class>
    <member kind="function">
      <type>Prototype *</type>
      <name>clone</name>
      <anchorfile>group__flacpp__metadata__object.html</anchorfile>
      <anchor>gae18d91726a320349b2c3fb45e79d21fc</anchor>
      <arglist>(const Prototype *)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>is_valid</name>
      <anchorfile>group__flacpp__metadata__object.html</anchorfile>
      <anchor>ga57adba3b3a548f7d9d8803762a8216d6</anchor>
      <arglist>() const </arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>operator const ::FLAC__StreamMetadata *</name>
      <anchorfile>group__flacpp__metadata__object.html</anchorfile>
      <anchor>gadad62834e7055e4996f3f6791553a214</anchor>
      <arglist>() const </arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>operator==</name>
      <anchorfile>group__flacpp__metadata__object.html</anchorfile>
      <anchor>ga6737ae25a19d76b88d9b2b4b7070d0cb</anchor>
      <arglist>(const Prototype &amp;) const </arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>operator==</name>
      <anchorfile>group__flacpp__metadata__object.html</anchorfile>
      <anchor>ga32f7d6acac0c6d49ff6b0f26c65a7f73</anchor>
      <arglist>(const ::FLAC__StreamMetadata &amp;) const </arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>operator==</name>
      <anchorfile>group__flacpp__metadata__object.html</anchorfile>
      <anchor>ga2c78b58871feecfe32ff90c53ca1c7e1</anchor>
      <arglist>(const ::FLAC__StreamMetadata *) const </arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>operator!=</name>
      <anchorfile>group__flacpp__metadata__object.html</anchorfile>
      <anchor>ga54a7ff9432ea6e2ec6fccde29df97e7d</anchor>
      <arglist>(const Prototype &amp;) const </arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>operator!=</name>
      <anchorfile>group__flacpp__metadata__object.html</anchorfile>
      <anchor>gaf946e496c12c9ba30ce729dd76f8555b</anchor>
      <arglist>(const ::FLAC__StreamMetadata &amp;) const </arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>operator!=</name>
      <anchorfile>group__flacpp__metadata__object.html</anchorfile>
      <anchor>ga3ecae6a5b7caf789c484b86da262e7f7</anchor>
      <arglist>(const ::FLAC__StreamMetadata *) const </arglist>
    </member>
  </compound>
  <compound kind="group">
    <name>flacpp_metadata_level0</name>
    <title>FLAC++/metadata.h: metadata level 0 interface</title>
    <filename>group__flacpp__metadata__level0.html</filename>
    <member kind="function">
      <type>bool</type>
      <name>get_streaminfo</name>
      <anchorfile>group__flacpp__metadata__level0.html</anchorfile>
      <anchor>ga8fa8da652f33edeb4dabb4ce39fda04b</anchor>
      <arglist>(const char *filename, StreamInfo &amp;streaminfo)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>get_tags</name>
      <anchorfile>group__flacpp__metadata__level0.html</anchorfile>
      <anchor>ga533a71ba745ca03068523a4a45fb0329</anchor>
      <arglist>(const char *filename, VorbisComment *&amp;tags)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>get_tags</name>
      <anchorfile>group__flacpp__metadata__level0.html</anchorfile>
      <anchor>ga85166e6206f3d5635684de4257f2b00e</anchor>
      <arglist>(const char *filename, VorbisComment &amp;tags)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>get_cuesheet</name>
      <anchorfile>group__flacpp__metadata__level0.html</anchorfile>
      <anchor>ga4fad03d91f22d78acf35dd2f35df9ac7</anchor>
      <arglist>(const char *filename, CueSheet *&amp;cuesheet)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>get_cuesheet</name>
      <anchorfile>group__flacpp__metadata__level0.html</anchorfile>
      <anchor>gaea8f05f89e36af143d73b4280f05cc0e</anchor>
      <arglist>(const char *filename, CueSheet &amp;cuesheet)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>get_picture</name>
      <anchorfile>group__flacpp__metadata__level0.html</anchorfile>
      <anchor>ga2ca454c644cb6548b05545c129e4d6ef</anchor>
      <arglist>(const char *filename, Picture *&amp;picture,::FLAC__StreamMetadata_Picture_Type type, const char *mime_type, const FLAC__byte *description, unsigned max_width, unsigned max_height, unsigned max_depth, unsigned max_colors)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>get_picture</name>
      <anchorfile>group__flacpp__metadata__level0.html</anchorfile>
      <anchor>ga82705f1c0ac6d36c0a508dc33e5e7181</anchor>
      <arglist>(const char *filename, Picture &amp;picture,::FLAC__StreamMetadata_Picture_Type type, const char *mime_type, const FLAC__byte *description, unsigned max_width, unsigned max_height, unsigned max_depth, unsigned max_colors)</arglist>
    </member>
  </compound>
  <compound kind="group">
    <name>flacpp_metadata_level1</name>
    <title>FLAC++/metadata.h: metadata level 1 interface</title>
    <filename>group__flacpp__metadata__level1.html</filename>
    <class kind="class">FLAC::Metadata::SimpleIterator</class>
    <class kind="class">FLAC::Metadata::SimpleIterator::Status</class>
  </compound>
  <compound kind="group">
    <name>flacpp_metadata_level2</name>
    <title>FLAC++/metadata.h: metadata level 2 interface</title>
    <filename>group__flacpp__metadata__level2.html</filename>
    <class kind="class">FLAC::Metadata::Chain</class>
    <class kind="class">FLAC::Metadata::Chain::Status</class>
    <class kind="class">FLAC::Metadata::Iterator</class>
    <member kind="function" protection="protected">
      <type></type>
      <name>Prototype</name>
      <anchorfile>group__flacpp__metadata__level2.html</anchorfile>
      <anchor>gae49fa399a6273ccad7cb0e6f787a3f5c</anchor>
      <arglist>(const Prototype &amp;)</arglist>
    </member>
  </compound>
  <compound kind="struct">
    <name>FLAC__EntropyCodingMethod</name>
    <filename>structFLAC____EntropyCodingMethod.html</filename>
  </compound>
  <compound kind="struct">
    <name>FLAC__EntropyCodingMethod_PartitionedRice</name>
    <filename>structFLAC____EntropyCodingMethod__PartitionedRice.html</filename>
    <member kind="variable">
      <type>unsigned</type>
      <name>order</name>
      <anchorfile>structFLAC____EntropyCodingMethod__PartitionedRice.html</anchorfile>
      <anchor>a1e1c9049e31eab5113c245164b2c694a</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const FLAC__EntropyCodingMethod_PartitionedRiceContents *</type>
      <name>contents</name>
      <anchorfile>structFLAC____EntropyCodingMethod__PartitionedRice.html</anchorfile>
      <anchor>a2fbfa1bd5656bf620c0bb9f8ba77f579</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="struct">
    <name>FLAC__EntropyCodingMethod_PartitionedRiceContents</name>
    <filename>structFLAC____EntropyCodingMethod__PartitionedRiceContents.html</filename>
    <member kind="variable">
      <type>unsigned *</type>
      <name>parameters</name>
      <anchorfile>structFLAC____EntropyCodingMethod__PartitionedRiceContents.html</anchorfile>
      <anchor>a06e97d40923f195410a65da9311ab6a2</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>unsigned *</type>
      <name>raw_bits</name>
      <anchorfile>structFLAC____EntropyCodingMethod__PartitionedRiceContents.html</anchorfile>
      <anchor>a915eb5369d39924bc29c948c29834279</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>unsigned</type>
      <name>capacity_by_order</name>
      <anchorfile>structFLAC____EntropyCodingMethod__PartitionedRiceContents.html</anchorfile>
      <anchor>abb60aca8d98bf9299558f230dfe83bc1</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="struct">
    <name>FLAC__Frame</name>
    <filename>structFLAC____Frame.html</filename>
  </compound>
  <compound kind="struct">
    <name>FLAC__FrameFooter</name>
    <filename>structFLAC____FrameFooter.html</filename>
    <member kind="variable">
      <type>FLAC__uint16</type>
      <name>crc</name>
      <anchorfile>structFLAC____FrameFooter.html</anchorfile>
      <anchor>abdd6d64bf281c49c720b97b955d4eee7</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="struct">
    <name>FLAC__FrameHeader</name>
    <filename>structFLAC____FrameHeader.html</filename>
    <member kind="variable">
      <type>unsigned</type>
      <name>blocksize</name>
      <anchorfile>structFLAC____FrameHeader.html</anchorfile>
      <anchor>ace760def6dcbbde3d9d140e5bfda34b3</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>unsigned</type>
      <name>sample_rate</name>
      <anchorfile>structFLAC____FrameHeader.html</anchorfile>
      <anchor>acc23daa576f4e75885bf4f2b69cee1be</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>unsigned</type>
      <name>channels</name>
      <anchorfile>structFLAC____FrameHeader.html</anchorfile>
      <anchor>a5950c6e4f03ad81f4a03c8c6188b9bf5</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>FLAC__ChannelAssignment</type>
      <name>channel_assignment</name>
      <anchorfile>structFLAC____FrameHeader.html</anchorfile>
      <anchor>a9a31f752e16da9d690f8d5ff85aed89c</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>unsigned</type>
      <name>bits_per_sample</name>
      <anchorfile>structFLAC____FrameHeader.html</anchorfile>
      <anchor>ae1f4af58cbbb837adf670d12bc4e86f3</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>FLAC__FrameNumberType</type>
      <name>number_type</name>
      <anchorfile>structFLAC____FrameHeader.html</anchorfile>
      <anchor>a7a62ec09e6f3029297179ef65377265f</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>union FLAC__FrameHeader::@2</type>
      <name>number</name>
      <anchorfile>structFLAC____FrameHeader.html</anchorfile>
      <anchor>affc849f1d7c044302e4e5c6c733c4642</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>FLAC__uint8</type>
      <name>crc</name>
      <anchorfile>structFLAC____FrameHeader.html</anchorfile>
      <anchor>a980438c380697df6f332cb27dc4672c4</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="struct">
    <name>FLAC__IOCallbacks</name>
    <filename>structFLAC____IOCallbacks.html</filename>
  </compound>
  <compound kind="struct">
    <name>FLAC__StreamDecoder</name>
    <filename>structFLAC____StreamDecoder.html</filename>
  </compound>
  <compound kind="struct">
    <name>FLAC__StreamEncoder</name>
    <filename>structFLAC____StreamEncoder.html</filename>
  </compound>
  <compound kind="struct">
    <name>FLAC__StreamMetadata</name>
    <filename>structFLAC____StreamMetadata.html</filename>
    <member kind="variable">
      <type>FLAC__MetadataType</type>
      <name>type</name>
      <anchorfile>structFLAC____StreamMetadata.html</anchorfile>
      <anchor>a39fd0655464f2cc7c9c37ae715088aec</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>FLAC__bool</type>
      <name>is_last</name>
      <anchorfile>structFLAC____StreamMetadata.html</anchorfile>
      <anchor>aef40bbf85abe12e035f66f2d54ed316c</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>unsigned</type>
      <name>length</name>
      <anchorfile>structFLAC____StreamMetadata.html</anchorfile>
      <anchor>a3fd615e41609837a5672f9081d9d2183</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>union FLAC__StreamMetadata::@3</type>
      <name>data</name>
      <anchorfile>structFLAC____StreamMetadata.html</anchorfile>
      <anchor>ae1b9b35d9ee11764f0a796c53301c542</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="struct">
    <name>FLAC__StreamMetadata_Application</name>
    <filename>structFLAC____StreamMetadata__Application.html</filename>
  </compound>
  <compound kind="struct">
    <name>FLAC__StreamMetadata_CueSheet</name>
    <filename>structFLAC____StreamMetadata__CueSheet.html</filename>
    <member kind="variable">
      <type>char</type>
      <name>media_catalog_number</name>
      <anchorfile>structFLAC____StreamMetadata__CueSheet.html</anchorfile>
      <anchor>a776e6057ac7939fba52edecd44ec45bc</anchor>
      <arglist>[129]</arglist>
    </member>
    <member kind="variable">
      <type>FLAC__uint64</type>
      <name>lead_in</name>
      <anchorfile>structFLAC____StreamMetadata__CueSheet.html</anchorfile>
      <anchor>a43fdc0a538ef2c3e0926ee22814baf40</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>FLAC__bool</type>
      <name>is_cd</name>
      <anchorfile>structFLAC____StreamMetadata__CueSheet.html</anchorfile>
      <anchor>a6af66f921aefc6f779fbc0ab6daeab8a</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>unsigned</type>
      <name>num_tracks</name>
      <anchorfile>structFLAC____StreamMetadata__CueSheet.html</anchorfile>
      <anchor>a6924f26a8e8fa9023f23539b959fe2ae</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>FLAC__StreamMetadata_CueSheet_Track *</type>
      <name>tracks</name>
      <anchorfile>structFLAC____StreamMetadata__CueSheet.html</anchorfile>
      <anchor>a5c0c3440b01b773684d56aeb1e424fab</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="struct">
    <name>FLAC__StreamMetadata_CueSheet_Index</name>
    <filename>structFLAC____StreamMetadata__CueSheet__Index.html</filename>
    <member kind="variable">
      <type>FLAC__uint64</type>
      <name>offset</name>
      <anchorfile>structFLAC____StreamMetadata__CueSheet__Index.html</anchorfile>
      <anchor>ac221421bca83976925e2a41438157bb9</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>FLAC__byte</type>
      <name>number</name>
      <anchorfile>structFLAC____StreamMetadata__CueSheet__Index.html</anchorfile>
      <anchor>a71edc33c19a749f1dfb3d1429e08c77a</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="struct">
    <name>FLAC__StreamMetadata_CueSheet_Track</name>
    <filename>structFLAC____StreamMetadata__CueSheet__Track.html</filename>
    <member kind="variable">
      <type>FLAC__uint64</type>
      <name>offset</name>
      <anchorfile>structFLAC____StreamMetadata__CueSheet__Track.html</anchorfile>
      <anchor>a40e1c888253a56b6dc4885a44138d1bf</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>FLAC__byte</type>
      <name>number</name>
      <anchorfile>structFLAC____StreamMetadata__CueSheet__Track.html</anchorfile>
      <anchor>a429103d63c44d1861b4dc0762726701a</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>char</type>
      <name>isrc</name>
      <anchorfile>structFLAC____StreamMetadata__CueSheet__Track.html</anchorfile>
      <anchor>a4990c8b13969f4c62683d915ebbf5744</anchor>
      <arglist>[13]</arglist>
    </member>
    <member kind="variable">
      <type>unsigned</type>
      <name>type</name>
      <anchorfile>structFLAC____StreamMetadata__CueSheet__Track.html</anchorfile>
      <anchor>a848575fc7a7292867ce76a9b3705f6e7</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>unsigned</type>
      <name>pre_emphasis</name>
      <anchorfile>structFLAC____StreamMetadata__CueSheet__Track.html</anchorfile>
      <anchor>ab4a97e43166ee16d1d16cccd901ddc3a</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>FLAC__byte</type>
      <name>num_indices</name>
      <anchorfile>structFLAC____StreamMetadata__CueSheet__Track.html</anchorfile>
      <anchor>a5f1c1d7e3ddc533938b83951c7b3dda5</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>FLAC__StreamMetadata_CueSheet_Index *</type>
      <name>indices</name>
      <anchorfile>structFLAC____StreamMetadata__CueSheet__Track.html</anchorfile>
      <anchor>a14e0692a77b5b6689e208f48369edb90</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="struct">
    <name>FLAC__StreamMetadata_Padding</name>
    <filename>structFLAC____StreamMetadata__Padding.html</filename>
    <member kind="variable">
      <type>int</type>
      <name>dummy</name>
      <anchorfile>structFLAC____StreamMetadata__Padding.html</anchorfile>
      <anchor>a5214437fcba7d6abdc3b2435dcaa4124</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="struct">
    <name>FLAC__StreamMetadata_Picture</name>
    <filename>structFLAC____StreamMetadata__Picture.html</filename>
    <member kind="variable">
      <type>FLAC__StreamMetadata_Picture_Type</type>
      <name>type</name>
      <anchorfile>structFLAC____StreamMetadata__Picture.html</anchorfile>
      <anchor>addc05a87a1da1ec7dd2301944ff2819c</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>char *</type>
      <name>mime_type</name>
      <anchorfile>structFLAC____StreamMetadata__Picture.html</anchorfile>
      <anchor>a9b4af2e10b627c0e79abf4cdd79f80e0</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>FLAC__byte *</type>
      <name>description</name>
      <anchorfile>structFLAC____StreamMetadata__Picture.html</anchorfile>
      <anchor>a5bbfb168b265edfb0b29cfdb71fb413c</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>FLAC__uint32</type>
      <name>width</name>
      <anchorfile>structFLAC____StreamMetadata__Picture.html</anchorfile>
      <anchor>a18dc6cdef9fa6c815450671f631a1e04</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>FLAC__uint32</type>
      <name>height</name>
      <anchorfile>structFLAC____StreamMetadata__Picture.html</anchorfile>
      <anchor>a76dbd1212d330807cda289660f5ee754</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>FLAC__uint32</type>
      <name>depth</name>
      <anchorfile>structFLAC____StreamMetadata__Picture.html</anchorfile>
      <anchor>a0f2092ddf28a6803e9c8adb7328c1967</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>FLAC__uint32</type>
      <name>colors</name>
      <anchorfile>structFLAC____StreamMetadata__Picture.html</anchorfile>
      <anchor>af17c1738bab67eba049ee101acfd36f0</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>FLAC__uint32</type>
      <name>data_length</name>
      <anchorfile>structFLAC____StreamMetadata__Picture.html</anchorfile>
      <anchor>acb893f63a196f70263468770a90580a4</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>FLAC__byte *</type>
      <name>data</name>
      <anchorfile>structFLAC____StreamMetadata__Picture.html</anchorfile>
      <anchor>a9c71b5d77920e6d3aee6893795c43605</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="struct">
    <name>FLAC__StreamMetadata_SeekPoint</name>
    <filename>structFLAC____StreamMetadata__SeekPoint.html</filename>
    <member kind="variable">
      <type>FLAC__uint64</type>
      <name>sample_number</name>
      <anchorfile>structFLAC____StreamMetadata__SeekPoint.html</anchorfile>
      <anchor>a96a62923f1443fd3a5a3498e701e6ecf</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>FLAC__uint64</type>
      <name>stream_offset</name>
      <anchorfile>structFLAC____StreamMetadata__SeekPoint.html</anchorfile>
      <anchor>a6028398e99f937b002618af677d32c9f</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>unsigned</type>
      <name>frame_samples</name>
      <anchorfile>structFLAC____StreamMetadata__SeekPoint.html</anchorfile>
      <anchor>a247ce3f0d45a56c202e623742309fe61</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="struct">
    <name>FLAC__StreamMetadata_SeekTable</name>
    <filename>structFLAC____StreamMetadata__SeekTable.html</filename>
  </compound>
  <compound kind="struct">
    <name>FLAC__StreamMetadata_StreamInfo</name>
    <filename>structFLAC____StreamMetadata__StreamInfo.html</filename>
  </compound>
  <compound kind="struct">
    <name>FLAC__StreamMetadata_Unknown</name>
    <filename>structFLAC____StreamMetadata__Unknown.html</filename>
  </compound>
  <compound kind="struct">
    <name>FLAC__StreamMetadata_VorbisComment</name>
    <filename>structFLAC____StreamMetadata__VorbisComment.html</filename>
  </compound>
  <compound kind="struct">
    <name>FLAC__StreamMetadata_VorbisComment_Entry</name>
    <filename>structFLAC____StreamMetadata__VorbisComment__Entry.html</filename>
  </compound>
  <compound kind="struct">
    <name>FLAC__Subframe</name>
    <filename>structFLAC____Subframe.html</filename>
  </compound>
  <compound kind="struct">
    <name>FLAC__Subframe_Constant</name>
    <filename>structFLAC____Subframe__Constant.html</filename>
    <member kind="variable">
      <type>FLAC__int32</type>
      <name>value</name>
      <anchorfile>structFLAC____Subframe__Constant.html</anchorfile>
      <anchor>af1bcfcbb17f1e1edb115b002fdbaa70e</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="struct">
    <name>FLAC__Subframe_Fixed</name>
    <filename>structFLAC____Subframe__Fixed.html</filename>
    <member kind="variable">
      <type>FLAC__EntropyCodingMethod</type>
      <name>entropy_coding_method</name>
      <anchorfile>structFLAC____Subframe__Fixed.html</anchorfile>
      <anchor>a0f17f8f756cd2c8acc0262ef14c37088</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>unsigned</type>
      <name>order</name>
      <anchorfile>structFLAC____Subframe__Fixed.html</anchorfile>
      <anchor>a73f6f3cce3f811c81532729dbad2df0d</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>FLAC__int32</type>
      <name>warmup</name>
      <anchorfile>structFLAC____Subframe__Fixed.html</anchorfile>
      <anchor>a0e9a40fb89b8aa45f83bf8979d200f1f</anchor>
      <arglist>[FLAC__MAX_FIXED_ORDER]</arglist>
    </member>
    <member kind="variable">
      <type>const FLAC__int32 *</type>
      <name>residual</name>
      <anchorfile>structFLAC____Subframe__Fixed.html</anchorfile>
      <anchor>ab91be48874aec97177106a4086163188</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="struct">
    <name>FLAC__Subframe_LPC</name>
    <filename>structFLAC____Subframe__LPC.html</filename>
    <member kind="variable">
      <type>FLAC__EntropyCodingMethod</type>
      <name>entropy_coding_method</name>
      <anchorfile>structFLAC____Subframe__LPC.html</anchorfile>
      <anchor>adb1401b2f8af05132420145a99f68c6e</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>unsigned</type>
      <name>order</name>
      <anchorfile>structFLAC____Subframe__LPC.html</anchorfile>
      <anchor>a0de317accaf8a9f86194f97c378b2f86</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>unsigned</type>
      <name>qlp_coeff_precision</name>
      <anchorfile>structFLAC____Subframe__LPC.html</anchorfile>
      <anchor>a6123b031203f603eba966b95fd2ad855</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>int</type>
      <name>quantization_level</name>
      <anchorfile>structFLAC____Subframe__LPC.html</anchorfile>
      <anchor>aedcf1a3e5e62485e7ce250eda1f3e588</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>FLAC__int32</type>
      <name>qlp_coeff</name>
      <anchorfile>structFLAC____Subframe__LPC.html</anchorfile>
      <anchor>ad0b37ee925e2124a37fe3a513d5410b8</anchor>
      <arglist>[FLAC__MAX_LPC_ORDER]</arglist>
    </member>
    <member kind="variable">
      <type>FLAC__int32</type>
      <name>warmup</name>
      <anchorfile>structFLAC____Subframe__LPC.html</anchorfile>
      <anchor>a91c6c71c6fc2b812da1d2a3761e29807</anchor>
      <arglist>[FLAC__MAX_LPC_ORDER]</arglist>
    </member>
    <member kind="variable">
      <type>const FLAC__int32 *</type>
      <name>residual</name>
      <anchorfile>structFLAC____Subframe__LPC.html</anchorfile>
      <anchor>acae4d0d439ea8900c5771eb967aec9bf</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="struct">
    <name>FLAC__Subframe_Verbatim</name>
    <filename>structFLAC____Subframe__Verbatim.html</filename>
    <member kind="variable">
      <type>const FLAC__int32 *</type>
      <name>data</name>
      <anchorfile>structFLAC____Subframe__Verbatim.html</anchorfile>
      <anchor>a6abc78689650804550ac517ada884584</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="class">
    <name>FLAC::Decoder::File</name>
    <filename>classFLAC_1_1Decoder_1_1File.html</filename>
    <base>FLAC::Decoder::Stream</base>
    <member kind="function">
      <type>virtual ::FLAC__StreamDecoderInitStatus</type>
      <name>init</name>
      <anchorfile>classFLAC_1_1Decoder_1_1File.html</anchorfile>
      <anchor>a793d2d9c08900cbe6ef6e2739c1e091f</anchor>
      <arglist>(FILE *file)</arglist>
    </member>
    <member kind="function">
      <type>virtual ::FLAC__StreamDecoderInitStatus</type>
      <name>init</name>
      <anchorfile>classFLAC_1_1Decoder_1_1File.html</anchorfile>
      <anchor>a4252bc6c949ec9456eea4af2a277dd6a</anchor>
      <arglist>(const char *filename)</arglist>
    </member>
    <member kind="function">
      <type>virtual ::FLAC__StreamDecoderInitStatus</type>
      <name>init</name>
      <anchorfile>classFLAC_1_1Decoder_1_1File.html</anchorfile>
      <anchor>a104a987909937cd716d382fdef9a0245</anchor>
      <arglist>(const std::string &amp;filename)</arglist>
    </member>
    <member kind="function">
      <type>virtual ::FLAC__StreamDecoderInitStatus</type>
      <name>init_ogg</name>
      <anchorfile>classFLAC_1_1Decoder_1_1File.html</anchorfile>
      <anchor>ab840fa309cb000e041f8427cd3e6354a</anchor>
      <arglist>(FILE *file)</arglist>
    </member>
    <member kind="function">
      <type>virtual ::FLAC__StreamDecoderInitStatus</type>
      <name>init_ogg</name>
      <anchorfile>classFLAC_1_1Decoder_1_1File.html</anchorfile>
      <anchor>a1af59a2861de527e8de5697683516b6e</anchor>
      <arglist>(const char *filename)</arglist>
    </member>
    <member kind="function">
      <type>virtual ::FLAC__StreamDecoderInitStatus</type>
      <name>init_ogg</name>
      <anchorfile>classFLAC_1_1Decoder_1_1File.html</anchorfile>
      <anchor>ac88baae2ff5a4c206a953262cd7447a9</anchor>
      <arglist>(const std::string &amp;filename)</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual bool</type>
      <name>set_ogg_serial_number</name>
      <anchorfile>classFLAC_1_1Decoder_1_1Stream.html</anchorfile>
      <anchor>aa257e8156474458cd8eed2902d3c2674</anchor>
      <arglist>(long value)</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual bool</type>
      <name>set_md5_checking</name>
      <anchorfile>classFLAC_1_1Decoder_1_1Stream.html</anchorfile>
      <anchor>a8f46d34c10a65d9c48e990f9b3bbe4e2</anchor>
      <arglist>(bool value)</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual bool</type>
      <name>set_metadata_respond</name>
      <anchorfile>classFLAC_1_1Decoder_1_1Stream.html</anchorfile>
      <anchor>a9208dd09a48d7a3034119565f51f0c56</anchor>
      <arglist>(::FLAC__MetadataType type)</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual bool</type>
      <name>set_metadata_respond_application</name>
      <anchorfile>classFLAC_1_1Decoder_1_1Stream.html</anchorfile>
      <anchor>a95468ca8d92d1693b21203ad3e0d4545</anchor>
      <arglist>(const FLAC__byte id[4])</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual bool</type>
      <name>set_metadata_respond_all</name>
      <anchorfile>classFLAC_1_1Decoder_1_1Stream.html</anchorfile>
      <anchor>a2ecec7b37f6f1d16ddcfee83a6919b5b</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual bool</type>
      <name>set_metadata_ignore</name>
      <anchorfile>classFLAC_1_1Decoder_1_1Stream.html</anchorfile>
      <anchor>ae239124fe0fc8fce3dcdae904bce7544</anchor>
      <arglist>(::FLAC__MetadataType type)</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual bool</type>
      <name>set_metadata_ignore_application</name>
      <anchorfile>classFLAC_1_1Decoder_1_1Stream.html</anchorfile>
      <anchor>ac963b9eaf8271fc47ef799901b6d3650</anchor>
      <arglist>(const FLAC__byte id[4])</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual bool</type>
      <name>set_metadata_ignore_all</name>
      <anchorfile>classFLAC_1_1Decoder_1_1Stream.html</anchorfile>
      <anchor>a900ecb31410c4ce56f23477b22c1c799</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>State</type>
      <name>get_state</name>
      <anchorfile>classFLAC_1_1Decoder_1_1Stream.html</anchorfile>
      <anchor>a1ac7755d2c82a802cc97994e707b1bd2</anchor>
      <arglist>() const </arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual bool</type>
      <name>get_md5_checking</name>
      <anchorfile>classFLAC_1_1Decoder_1_1Stream.html</anchorfile>
      <anchor>a341d3bbff336f580b59aee3adc7f9357</anchor>
      <arglist>() const </arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual FLAC__uint64</type>
      <name>get_total_samples</name>
      <anchorfile>classFLAC_1_1Decoder_1_1Stream.html</anchorfile>
      <anchor>a42af32a0878f9e4b5d5a052e0733a539</anchor>
      <arglist>() const </arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual unsigned</type>
      <name>get_channels</name>
      <anchorfile>classFLAC_1_1Decoder_1_1Stream.html</anchorfile>
      <anchor>ad9361dcb0435dde6baeb98fd1e669cee</anchor>
      <arglist>() const </arglist>
    </member>
    <member kind="function">
      <type>virtual ::FLAC__ChannelAssignment</type>
      <name>get_channel_assignment</name>
      <anchorfile>classFLAC_1_1Decoder_1_1Stream.html</anchorfile>
      <anchor>afe090e604dc45400fe64c6dd9c42ca11</anchor>
      <arglist>() const </arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual unsigned</type>
      <name>get_bits_per_sample</name>
      <anchorfile>classFLAC_1_1Decoder_1_1Stream.html</anchorfile>
      <anchor>a9412939ab55513a7c5389a57d074f3f2</anchor>
      <arglist>() const </arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual unsigned</type>
      <name>get_sample_rate</name>
      <anchorfile>classFLAC_1_1Decoder_1_1Stream.html</anchorfile>
      <anchor>a31b3d6b63d75d2864eb740961a0b956c</anchor>
      <arglist>() const </arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual unsigned</type>
      <name>get_blocksize</name>
      <anchorfile>classFLAC_1_1Decoder_1_1Stream.html</anchorfile>
      <anchor>a7e777db68ffea49356cc3510732ce6da</anchor>
      <arglist>() const </arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual bool</type>
      <name>get_decode_position</name>
      <anchorfile>classFLAC_1_1Decoder_1_1Stream.html</anchorfile>
      <anchor>a433252dba465178167d6a85cc1fea881</anchor>
      <arglist>(FLAC__uint64 *position) const </arglist>
    </member>
    <member kind="function">
      <type>virtual ::FLAC__StreamDecoderInitStatus</type>
      <name>init</name>
      <anchorfile>classFLAC_1_1Decoder_1_1Stream.html</anchorfile>
      <anchor>a33169215b21ff3582c0c1f5fef6dda47</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>virtual ::FLAC__StreamDecoderInitStatus</type>
      <name>init_ogg</name>
      <anchorfile>classFLAC_1_1Decoder_1_1Stream.html</anchorfile>
      <anchor>adb52518fda2e3e544f4c8807f4227ba7</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual bool</type>
      <name>finish</name>
      <anchorfile>classFLAC_1_1Decoder_1_1Stream.html</anchorfile>
      <anchor>a0221e9ba254566331e8d0e33579ee3c0</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual bool</type>
      <name>flush</name>
      <anchorfile>classFLAC_1_1Decoder_1_1Stream.html</anchorfile>
      <anchor>a9cb00ff4543d411a9b3c64b1f3f058bb</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual bool</type>
      <name>reset</name>
      <anchorfile>classFLAC_1_1Decoder_1_1Stream.html</anchorfile>
      <anchor>a7b6b4665e139234fa80acd0a1f16ca7c</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual bool</type>
      <name>process_single</name>
      <anchorfile>classFLAC_1_1Decoder_1_1Stream.html</anchorfile>
      <anchor>ab50ff5df74c47f4e0f1c91d63a59f5ac</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual bool</type>
      <name>process_until_end_of_metadata</name>
      <anchorfile>classFLAC_1_1Decoder_1_1Stream.html</anchorfile>
      <anchor>ab0cabe42278b18e9d3dbfee39cc720cf</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual bool</type>
      <name>process_until_end_of_stream</name>
      <anchorfile>classFLAC_1_1Decoder_1_1Stream.html</anchorfile>
      <anchor>afbd6ff20477cae1ace00b8c304a4795a</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual bool</type>
      <name>skip_single_frame</name>
      <anchorfile>classFLAC_1_1Decoder_1_1Stream.html</anchorfile>
      <anchor>a30a738e7ae11f389c58a74f7ff647fe4</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual bool</type>
      <name>seek_absolute</name>
      <anchorfile>classFLAC_1_1Decoder_1_1Stream.html</anchorfile>
      <anchor>ac146128003d4ccd46bcffa82003e545c</anchor>
      <arglist>(FLAC__uint64 sample)</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual bool</type>
      <name>is_valid</name>
      <anchorfile>classFLAC_1_1Decoder_1_1Stream.html</anchorfile>
      <anchor>a6f0b329fd6e78d5e769c769a97fcc337</anchor>
      <arglist>() const </arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>operator bool</name>
      <anchorfile>classFLAC_1_1Decoder_1_1Stream.html</anchorfile>
      <anchor>a82e6e8685d0b70e7686baff4377d98cd</anchor>
      <arglist>() const </arglist>
    </member>
    <member kind="function" protection="protected">
      <type>virtual ::FLAC__StreamDecoderReadStatus</type>
      <name>read_callback</name>
      <anchorfile>classFLAC_1_1Decoder_1_1File.html</anchorfile>
      <anchor>a48c900fc010f14786e98908377f41195</anchor>
      <arglist>(FLAC__byte buffer[], size_t *bytes)</arglist>
    </member>
    <member kind="function" protection="protected">
      <type>virtual ::FLAC__StreamDecoderSeekStatus</type>
      <name>seek_callback</name>
      <anchorfile>classFLAC_1_1Decoder_1_1Stream.html</anchorfile>
      <anchor>af6f7e0811f34837752fbe20f3348f895</anchor>
      <arglist>(FLAC__uint64 absolute_byte_offset)</arglist>
    </member>
    <member kind="function" protection="protected">
      <type>virtual ::FLAC__StreamDecoderTellStatus</type>
      <name>tell_callback</name>
      <anchorfile>classFLAC_1_1Decoder_1_1Stream.html</anchorfile>
      <anchor>a0075cb08ab7bf5230ec0360ae3065a50</anchor>
      <arglist>(FLAC__uint64 *absolute_byte_offset)</arglist>
    </member>
    <member kind="function" protection="protected">
      <type>virtual ::FLAC__StreamDecoderLengthStatus</type>
      <name>length_callback</name>
      <anchorfile>classFLAC_1_1Decoder_1_1Stream.html</anchorfile>
      <anchor>a6a9af9305783c4af4b93698293dcdf84</anchor>
      <arglist>(FLAC__uint64 *stream_length)</arglist>
    </member>
    <member kind="function" protection="protected" virtualness="virtual">
      <type>virtual bool</type>
      <name>eof_callback</name>
      <anchorfile>classFLAC_1_1Decoder_1_1Stream.html</anchorfile>
      <anchor>ac06aa682efc2e819624e78a3e6b4bd7b</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function" protection="protected" virtualness="pure">
      <type>virtual ::FLAC__StreamDecoderWriteStatus</type>
      <name>write_callback</name>
      <anchorfile>classFLAC_1_1Decoder_1_1Stream.html</anchorfile>
      <anchor>af5a61e9ff720cca3eb38d1f2790f00fb</anchor>
      <arglist>(const ::FLAC__Frame *frame, const FLAC__int32 *const buffer[])=0</arglist>
    </member>
    <member kind="function" protection="protected" virtualness="virtual">
      <type>virtual void</type>
      <name>metadata_callback</name>
      <anchorfile>classFLAC_1_1Decoder_1_1Stream.html</anchorfile>
      <anchor>a20d0873073d9542e08fb48becaa607c9</anchor>
      <arglist>(const ::FLAC__StreamMetadata *metadata)</arglist>
    </member>
    <member kind="function" protection="protected" virtualness="pure">
      <type>virtual void</type>
      <name>error_callback</name>
      <anchorfile>classFLAC_1_1Decoder_1_1Stream.html</anchorfile>
      <anchor>a0dbadd163ade7bc2d1858e7a435d5e52</anchor>
      <arglist>(::FLAC__StreamDecoderErrorStatus status)=0</arglist>
    </member>
  </compound>
  <compound kind="class">
    <name>FLAC::Decoder::Stream</name>
    <filename>classFLAC_1_1Decoder_1_1Stream.html</filename>
    <class kind="class">FLAC::Decoder::Stream::State</class>
    <member kind="function" protection="protected" virtualness="pure">
      <type>virtual ::FLAC__StreamDecoderReadStatus</type>
      <name>read_callback</name>
      <anchorfile>classFLAC_1_1Decoder_1_1Stream.html</anchorfile>
      <anchor>af91735b6c715ca648493e837f513ef3d</anchor>
      <arglist>(FLAC__byte buffer[], size_t *bytes)=0</arglist>
    </member>
  </compound>
  <compound kind="class">
    <name>FLAC::Decoder::Stream::State</name>
    <filename>classFLAC_1_1Decoder_1_1Stream_1_1State.html</filename>
  </compound>
  <compound kind="class">
    <name>FLAC::Encoder::File</name>
    <filename>classFLAC_1_1Encoder_1_1File.html</filename>
    <base>FLAC::Encoder::Stream</base>
    <member kind="function">
      <type>virtual ::FLAC__StreamEncoderInitStatus</type>
      <name>init</name>
      <anchorfile>classFLAC_1_1Encoder_1_1File.html</anchorfile>
      <anchor>afefae0d1c92f0d63d7be69a54667ff79</anchor>
      <arglist>(FILE *file)</arglist>
    </member>
    <member kind="function">
      <type>virtual ::FLAC__StreamEncoderInitStatus</type>
      <name>init</name>
      <anchorfile>classFLAC_1_1Encoder_1_1File.html</anchorfile>
      <anchor>a31016dd8e1db5bb9c1c3739b94fdb3e3</anchor>
      <arglist>(const char *filename)</arglist>
    </member>
    <member kind="function">
      <type>virtual ::FLAC__StreamEncoderInitStatus</type>
      <name>init</name>
      <anchorfile>classFLAC_1_1Encoder_1_1File.html</anchorfile>
      <anchor>a4966ed5f77dbf5a03946ff25f60a0f8c</anchor>
      <arglist>(const std::string &amp;filename)</arglist>
    </member>
    <member kind="function">
      <type>virtual ::FLAC__StreamEncoderInitStatus</type>
      <name>init_ogg</name>
      <anchorfile>classFLAC_1_1Encoder_1_1File.html</anchorfile>
      <anchor>a5dfab60d9cae983899e0b0f6e1ab9377</anchor>
      <arglist>(FILE *file)</arglist>
    </member>
    <member kind="function">
      <type>virtual ::FLAC__StreamEncoderInitStatus</type>
      <name>init_ogg</name>
      <anchorfile>classFLAC_1_1Encoder_1_1File.html</anchorfile>
      <anchor>a0740ed07b77e49a76f8ddc0e79540eae</anchor>
      <arglist>(const char *filename)</arglist>
    </member>
    <member kind="function">
      <type>virtual ::FLAC__StreamEncoderInitStatus</type>
      <name>init_ogg</name>
      <anchorfile>classFLAC_1_1Encoder_1_1File.html</anchorfile>
      <anchor>a202881c81ed146e9a83f7378cf1de2d6</anchor>
      <arglist>(const std::string &amp;filename)</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual bool</type>
      <name>set_ogg_serial_number</name>
      <anchorfile>classFLAC_1_1Encoder_1_1Stream.html</anchorfile>
      <anchor>adf54d79eb0e6dce071f46be6f2c2d55c</anchor>
      <arglist>(long value)</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual bool</type>
      <name>set_verify</name>
      <anchorfile>classFLAC_1_1Encoder_1_1Stream.html</anchorfile>
      <anchor>a85c2296aedf8d4cd2d9f284b1c3205f8</anchor>
      <arglist>(bool value)</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual bool</type>
      <name>set_streamable_subset</name>
      <anchorfile>classFLAC_1_1Encoder_1_1Stream.html</anchorfile>
      <anchor>a85d78d5333b05e8a76a1edc9462dbfbc</anchor>
      <arglist>(bool value)</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual bool</type>
      <name>set_channels</name>
      <anchorfile>classFLAC_1_1Encoder_1_1Stream.html</anchorfile>
      <anchor>a0130baae7a1a56384ae8c5b9b2e36618</anchor>
      <arglist>(unsigned value)</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual bool</type>
      <name>set_bits_per_sample</name>
      <anchorfile>classFLAC_1_1Encoder_1_1Stream.html</anchorfile>
      <anchor>a37bd3f9aa78e56abde8f7119396d5698</anchor>
      <arglist>(unsigned value)</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual bool</type>
      <name>set_sample_rate</name>
      <anchorfile>classFLAC_1_1Encoder_1_1Stream.html</anchorfile>
      <anchor>a317337e199744ffdf5b27c9953baba2f</anchor>
      <arglist>(unsigned value)</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual bool</type>
      <name>set_compression_level</name>
      <anchorfile>classFLAC_1_1Encoder_1_1Stream.html</anchorfile>
      <anchor>a9e274578cab80bc8f433331c46925a2a</anchor>
      <arglist>(unsigned value)</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual bool</type>
      <name>set_blocksize</name>
      <anchorfile>classFLAC_1_1Encoder_1_1Stream.html</anchorfile>
      <anchor>acc539384c2ed09624deb1b12accfd683</anchor>
      <arglist>(unsigned value)</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual bool</type>
      <name>set_do_mid_side_stereo</name>
      <anchorfile>classFLAC_1_1Encoder_1_1Stream.html</anchorfile>
      <anchor>a034ab145e428444b0c6cc4d6818b1121</anchor>
      <arglist>(bool value)</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual bool</type>
      <name>set_loose_mid_side_stereo</name>
      <anchorfile>classFLAC_1_1Encoder_1_1Stream.html</anchorfile>
      <anchor>aa691def57681119f0cb99804db7959d0</anchor>
      <arglist>(bool value)</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual bool</type>
      <name>set_apodization</name>
      <anchorfile>classFLAC_1_1Encoder_1_1Stream.html</anchorfile>
      <anchor>a4b9a35fd8996be1a4c46fafd41e34e28</anchor>
      <arglist>(const char *specification)</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual bool</type>
      <name>set_max_lpc_order</name>
      <anchorfile>classFLAC_1_1Encoder_1_1Stream.html</anchorfile>
      <anchor>af6f402684821f1aa66e4dbb5ef8fa40f</anchor>
      <arglist>(unsigned value)</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual bool</type>
      <name>set_qlp_coeff_precision</name>
      <anchorfile>classFLAC_1_1Encoder_1_1Stream.html</anchorfile>
      <anchor>a395d2fee212257269ad05be0c2d57b1c</anchor>
      <arglist>(unsigned value)</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual bool</type>
      <name>set_do_qlp_coeff_prec_search</name>
      <anchorfile>classFLAC_1_1Encoder_1_1Stream.html</anchorfile>
      <anchor>a9a63c0657c6834229d67e64adaf61fde</anchor>
      <arglist>(bool value)</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual bool</type>
      <name>set_do_escape_coding</name>
      <anchorfile>classFLAC_1_1Encoder_1_1Stream.html</anchorfile>
      <anchor>a4a5b69ec2f0a329a662519021a022266</anchor>
      <arglist>(bool value)</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual bool</type>
      <name>set_do_exhaustive_model_search</name>
      <anchorfile>classFLAC_1_1Encoder_1_1Stream.html</anchorfile>
      <anchor>a3832c6e375edfb304ea6dcf7afb15c83</anchor>
      <arglist>(bool value)</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual bool</type>
      <name>set_min_residual_partition_order</name>
      <anchorfile>classFLAC_1_1Encoder_1_1Stream.html</anchorfile>
      <anchor>a7db0bb9381b7750bb194a1fda947e5b3</anchor>
      <arglist>(unsigned value)</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual bool</type>
      <name>set_max_residual_partition_order</name>
      <anchorfile>classFLAC_1_1Encoder_1_1Stream.html</anchorfile>
      <anchor>a1fcb2e699261a633766e5cf3d8809e2b</anchor>
      <arglist>(unsigned value)</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual bool</type>
      <name>set_rice_parameter_search_dist</name>
      <anchorfile>classFLAC_1_1Encoder_1_1Stream.html</anchorfile>
      <anchor>a67281b5fc39c0067e6c6fffb987c8b0a</anchor>
      <arglist>(unsigned value)</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual bool</type>
      <name>set_total_samples_estimate</name>
      <anchorfile>classFLAC_1_1Encoder_1_1Stream.html</anchorfile>
      <anchor>a5f9de26084c378a7cd55919381465c24</anchor>
      <arglist>(FLAC__uint64 value)</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual bool</type>
      <name>set_metadata</name>
      <anchorfile>classFLAC_1_1Encoder_1_1Stream.html</anchorfile>
      <anchor>a79dcf32db2d8bddcabe3e871a52dd52d</anchor>
      <arglist>(::FLAC__StreamMetadata **metadata, unsigned num_blocks)</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual bool</type>
      <name>set_metadata</name>
      <anchorfile>classFLAC_1_1Encoder_1_1Stream.html</anchorfile>
      <anchor>a06c7f5dde30c6f209e696100003c9779</anchor>
      <arglist>(FLAC::Metadata::Prototype **metadata, unsigned num_blocks)</arglist>
    </member>
    <member kind="function">
      <type>State</type>
      <name>get_state</name>
      <anchorfile>classFLAC_1_1Encoder_1_1Stream.html</anchorfile>
      <anchor>aeacf0b62a38ec101b07111fbb96fae4f</anchor>
      <arglist>() const </arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual Decoder::Stream::State</type>
      <name>get_verify_decoder_state</name>
      <anchorfile>classFLAC_1_1Encoder_1_1Stream.html</anchorfile>
      <anchor>ae73cb3d5e042396527a0f69343e1f9c7</anchor>
      <arglist>() const </arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual void</type>
      <name>get_verify_decoder_error_stats</name>
      <anchorfile>classFLAC_1_1Encoder_1_1Stream.html</anchorfile>
      <anchor>a03e703883a6275ddc18e6aa85034e1b4</anchor>
      <arglist>(FLAC__uint64 *absolute_sample, unsigned *frame_number, unsigned *channel, unsigned *sample, FLAC__int32 *expected, FLAC__int32 *got)</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual bool</type>
      <name>get_verify</name>
      <anchorfile>classFLAC_1_1Encoder_1_1Stream.html</anchorfile>
      <anchor>a1b43a04f7023d15a0abde91a9a0d65b3</anchor>
      <arglist>() const </arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual bool</type>
      <name>get_streamable_subset</name>
      <anchorfile>classFLAC_1_1Encoder_1_1Stream.html</anchorfile>
      <anchor>a84a6fe3a546ad5bed41c10c22ebe2608</anchor>
      <arglist>() const </arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual bool</type>
      <name>get_do_mid_side_stereo</name>
      <anchorfile>classFLAC_1_1Encoder_1_1Stream.html</anchorfile>
      <anchor>abf4379816269dd5548ed29e2f803fe2b</anchor>
      <arglist>() const </arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual bool</type>
      <name>get_loose_mid_side_stereo</name>
      <anchorfile>classFLAC_1_1Encoder_1_1Stream.html</anchorfile>
      <anchor>ab7b4ffe227c2a17bee4fb001f009b446</anchor>
      <arglist>() const </arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual unsigned</type>
      <name>get_channels</name>
      <anchorfile>classFLAC_1_1Encoder_1_1Stream.html</anchorfile>
      <anchor>a1df5ac2094960c44f698a7b6760e3035</anchor>
      <arglist>() const </arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual unsigned</type>
      <name>get_bits_per_sample</name>
      <anchorfile>classFLAC_1_1Encoder_1_1Stream.html</anchorfile>
      <anchor>afe54ba1aad7782f5b66a423e0836761f</anchor>
      <arglist>() const </arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual unsigned</type>
      <name>get_sample_rate</name>
      <anchorfile>classFLAC_1_1Encoder_1_1Stream.html</anchorfile>
      <anchor>a98cf81c61533d745a0ed19c47d317e49</anchor>
      <arglist>() const </arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual unsigned</type>
      <name>get_blocksize</name>
      <anchorfile>classFLAC_1_1Encoder_1_1Stream.html</anchorfile>
      <anchor>a3b257ccb31c9787e94cb0d50aea82a4e</anchor>
      <arglist>() const </arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual unsigned</type>
      <name>get_max_lpc_order</name>
      <anchorfile>classFLAC_1_1Encoder_1_1Stream.html</anchorfile>
      <anchor>a1de7f60d3124966c798d86d49e1a1e87</anchor>
      <arglist>() const </arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual unsigned</type>
      <name>get_qlp_coeff_precision</name>
      <anchorfile>classFLAC_1_1Encoder_1_1Stream.html</anchorfile>
      <anchor>ae6c1f413bb54fbde836cc0b294b8c827</anchor>
      <arglist>() const </arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual bool</type>
      <name>get_do_qlp_coeff_prec_search</name>
      <anchorfile>classFLAC_1_1Encoder_1_1Stream.html</anchorfile>
      <anchor>a647c407a588f29c91b49696247d52e22</anchor>
      <arglist>() const </arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual bool</type>
      <name>get_do_escape_coding</name>
      <anchorfile>classFLAC_1_1Encoder_1_1Stream.html</anchorfile>
      <anchor>a95886dd052a3c3318a0326f1edec6a7f</anchor>
      <arglist>() const </arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual bool</type>
      <name>get_do_exhaustive_model_search</name>
      <anchorfile>classFLAC_1_1Encoder_1_1Stream.html</anchorfile>
      <anchor>ad17080caeab928a403a313f9df786ac5</anchor>
      <arglist>() const </arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual unsigned</type>
      <name>get_min_residual_partition_order</name>
      <anchorfile>classFLAC_1_1Encoder_1_1Stream.html</anchorfile>
      <anchor>a6244b1521f2b68c5f8c41c3244f42115</anchor>
      <arglist>() const </arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual unsigned</type>
      <name>get_max_residual_partition_order</name>
      <anchorfile>classFLAC_1_1Encoder_1_1Stream.html</anchorfile>
      <anchor>a0d95e9aa14c49956fdfbf36f7354d8d6</anchor>
      <arglist>() const </arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual unsigned</type>
      <name>get_rice_parameter_search_dist</name>
      <anchorfile>classFLAC_1_1Encoder_1_1Stream.html</anchorfile>
      <anchor>a6a5fb52a7156c5308f2770fc2a6a1931</anchor>
      <arglist>() const </arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual FLAC__uint64</type>
      <name>get_total_samples_estimate</name>
      <anchorfile>classFLAC_1_1Encoder_1_1Stream.html</anchorfile>
      <anchor>a74f50f41815ec0d6bf0890ba55a52c36</anchor>
      <arglist>() const </arglist>
    </member>
    <member kind="function">
      <type>virtual ::FLAC__StreamEncoderInitStatus</type>
      <name>init</name>
      <anchorfile>classFLAC_1_1Encoder_1_1Stream.html</anchorfile>
      <anchor>a17bfdc6402a626db36ee23985ee959b6</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>virtual ::FLAC__StreamEncoderInitStatus</type>
      <name>init_ogg</name>
      <anchorfile>classFLAC_1_1Encoder_1_1Stream.html</anchorfile>
      <anchor>a6cd96756d387c89555b4fb36e3323f35</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual bool</type>
      <name>finish</name>
      <anchorfile>classFLAC_1_1Encoder_1_1Stream.html</anchorfile>
      <anchor>ad70a30287eb9e062454ca296b9628318</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual bool</type>
      <name>process</name>
      <anchorfile>classFLAC_1_1Encoder_1_1Stream.html</anchorfile>
      <anchor>ae9dad0b0bcda0b0147ee98cec93e1a7c</anchor>
      <arglist>(const FLAC__int32 *const buffer[], unsigned samples)</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual bool</type>
      <name>process_interleaved</name>
      <anchorfile>classFLAC_1_1Encoder_1_1Stream.html</anchorfile>
      <anchor>acafa52586305e403e3a8857f91a7d62d</anchor>
      <arglist>(const FLAC__int32 buffer[], unsigned samples)</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual bool</type>
      <name>is_valid</name>
      <anchorfile>classFLAC_1_1Encoder_1_1Stream.html</anchorfile>
      <anchor>a2032ccacef0d59384894df85d85d2378</anchor>
      <arglist>() const </arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>operator bool</name>
      <anchorfile>classFLAC_1_1Encoder_1_1Stream.html</anchorfile>
      <anchor>a9cc2181c5be961e2b6ad1945bf57e37b</anchor>
      <arglist>() const </arglist>
    </member>
    <member kind="function" protection="protected" virtualness="virtual">
      <type>virtual void</type>
      <name>progress_callback</name>
      <anchorfile>classFLAC_1_1Encoder_1_1File.html</anchorfile>
      <anchor>a9905685a62680a82d3430d9f27739870</anchor>
      <arglist>(FLAC__uint64 bytes_written, FLAC__uint64 samples_written, unsigned frames_written, unsigned total_frames_estimate)</arglist>
    </member>
    <member kind="function" protection="protected">
      <type>virtual ::FLAC__StreamEncoderWriteStatus</type>
      <name>write_callback</name>
      <anchorfile>classFLAC_1_1Encoder_1_1File.html</anchorfile>
      <anchor>a939a333eeb145f3936d9e35c68f13db1</anchor>
      <arglist>(const FLAC__byte buffer[], size_t bytes, unsigned samples, unsigned current_frame)</arglist>
    </member>
    <member kind="function" protection="protected">
      <type>virtual ::FLAC__StreamEncoderReadStatus</type>
      <name>read_callback</name>
      <anchorfile>classFLAC_1_1Encoder_1_1Stream.html</anchorfile>
      <anchor>a483965ffe35ed652a5fca622c7791811</anchor>
      <arglist>(FLAC__byte buffer[], size_t *bytes)</arglist>
    </member>
    <member kind="function" protection="protected">
      <type>virtual ::FLAC__StreamEncoderSeekStatus</type>
      <name>seek_callback</name>
      <anchorfile>classFLAC_1_1Encoder_1_1Stream.html</anchorfile>
      <anchor>a7df3745afe10cd4dbcc3433a32fcb463</anchor>
      <arglist>(FLAC__uint64 absolute_byte_offset)</arglist>
    </member>
    <member kind="function" protection="protected">
      <type>virtual ::FLAC__StreamEncoderTellStatus</type>
      <name>tell_callback</name>
      <anchorfile>classFLAC_1_1Encoder_1_1Stream.html</anchorfile>
      <anchor>a5a4f38682e33172f53f7f374372fe1e0</anchor>
      <arglist>(FLAC__uint64 *absolute_byte_offset)</arglist>
    </member>
    <member kind="function" protection="protected" virtualness="virtual">
      <type>virtual void</type>
      <name>metadata_callback</name>
      <anchorfile>classFLAC_1_1Encoder_1_1Stream.html</anchorfile>
      <anchor>ad9c6a7aa7720f215bfe3b65e032e148c</anchor>
      <arglist>(const ::FLAC__StreamMetadata *metadata)</arglist>
    </member>
  </compound>
  <compound kind="class">
    <name>FLAC::Encoder::Stream</name>
    <filename>classFLAC_1_1Encoder_1_1Stream.html</filename>
    <class kind="class">FLAC::Encoder::Stream::State</class>
    <member kind="function" protection="protected" virtualness="pure">
      <type>virtual ::FLAC__StreamEncoderWriteStatus</type>
      <name>write_callback</name>
      <anchorfile>classFLAC_1_1Encoder_1_1Stream.html</anchorfile>
      <anchor>afa6c90811480bb4fccb1cf3da8160c47</anchor>
      <arglist>(const FLAC__byte buffer[], size_t bytes, unsigned samples, unsigned current_frame)=0</arglist>
    </member>
  </compound>
  <compound kind="class">
    <name>FLAC::Encoder::Stream::State</name>
    <filename>classFLAC_1_1Encoder_1_1Stream_1_1State.html</filename>
  </compound>
  <compound kind="class">
    <name>FLAC::Metadata::Application</name>
    <filename>classFLAC_1_1Metadata_1_1Application.html</filename>
    <base>FLAC::Metadata::Prototype</base>
    <member kind="function">
      <type></type>
      <name>Application</name>
      <anchorfile>classFLAC_1_1Metadata_1_1Application.html</anchorfile>
      <anchor>a354471e537af33ba0c86de4db988efd1</anchor>
      <arglist>(::FLAC__StreamMetadata *object, bool copy)</arglist>
    </member>
    <member kind="function">
      <type>Application &amp;</type>
      <name>assign</name>
      <anchorfile>classFLAC_1_1Metadata_1_1Application.html</anchorfile>
      <anchor>a47f68d7001ef094a916d3b13fe589fc2</anchor>
      <arglist>(::FLAC__StreamMetadata *object, bool copy)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_data</name>
      <anchorfile>classFLAC_1_1Metadata_1_1Application.html</anchorfile>
      <anchor>acbb3f2b255a4c5cc4ad179a79b1588f1</anchor>
      <arglist>(const FLAC__byte *data, unsigned length)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>is_valid</name>
      <anchorfile>group__flacpp__metadata__object.html</anchorfile>
      <anchor>ga57adba3b3a548f7d9d8803762a8216d6</anchor>
      <arglist>() const </arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>get_is_last</name>
      <anchorfile>classFLAC_1_1Metadata_1_1Prototype.html</anchorfile>
      <anchor>ae9e4332c5d2b66ca39763b371420e166</anchor>
      <arglist>() const </arglist>
    </member>
    <member kind="function">
      <type>::FLAC__MetadataType</type>
      <name>get_type</name>
      <anchorfile>classFLAC_1_1Metadata_1_1Prototype.html</anchorfile>
      <anchor>ae79be48d03ca0f697c60fa4f95dd856a</anchor>
      <arglist>() const </arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>get_length</name>
      <anchorfile>classFLAC_1_1Metadata_1_1Prototype.html</anchorfile>
      <anchor>a5fd592fd29018c66d29f0940f3cf05b3</anchor>
      <arglist>() const </arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>set_is_last</name>
      <anchorfile>classFLAC_1_1Metadata_1_1Prototype.html</anchorfile>
      <anchor>af40c7c078e408f7d6d0b5f521a013315</anchor>
      <arglist>(bool)</arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>operator const ::FLAC__StreamMetadata *</name>
      <anchorfile>group__flacpp__metadata__object.html</anchorfile>
      <anchor>gadad62834e7055e4996f3f6791553a214</anchor>
      <arglist>() const </arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>Application</name>
      <anchorfile>classFLAC_1_1Metadata_1_1Application.html</anchorfile>
      <anchor>ac852c4aa3be004f1ffa4895ca54354a0</anchor>
      <arglist>(const Application &amp;object)</arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>Application</name>
      <anchorfile>classFLAC_1_1Metadata_1_1Application.html</anchorfile>
      <anchor>afea8e8477179395b175f5481b9a7f520</anchor>
      <arglist>(const ::FLAC__StreamMetadata &amp;object)</arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>Application</name>
      <anchorfile>classFLAC_1_1Metadata_1_1Application.html</anchorfile>
      <anchor>a88fa6324b6b46d41787934774f65d423</anchor>
      <arglist>(const ::FLAC__StreamMetadata *object)</arglist>
    </member>
    <member kind="function">
      <type>Application &amp;</type>
      <name>operator=</name>
      <anchorfile>classFLAC_1_1Metadata_1_1Application.html</anchorfile>
      <anchor>a3ca9dd06666b1dc7d4bdb6aef8e14d04</anchor>
      <arglist>(const Application &amp;object)</arglist>
    </member>
    <member kind="function">
      <type>Application &amp;</type>
      <name>operator=</name>
      <anchorfile>classFLAC_1_1Metadata_1_1Application.html</anchorfile>
      <anchor>aad78784867bb6c8816238a57bab91535</anchor>
      <arglist>(const ::FLAC__StreamMetadata &amp;object)</arglist>
    </member>
    <member kind="function">
      <type>Application &amp;</type>
      <name>operator=</name>
      <anchorfile>classFLAC_1_1Metadata_1_1Application.html</anchorfile>
      <anchor>afe3c7e50501b56045366d2121d084fba</anchor>
      <arglist>(const ::FLAC__StreamMetadata *object)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>operator==</name>
      <anchorfile>classFLAC_1_1Metadata_1_1Application.html</anchorfile>
      <anchor>a8e107fdc72e0afee4ee381f375c118df</anchor>
      <arglist>(const Application &amp;object) const </arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>operator==</name>
      <anchorfile>classFLAC_1_1Metadata_1_1Application.html</anchorfile>
      <anchor>ad6beb0e98e50cdfb3b17dd3e545d54ae</anchor>
      <arglist>(const ::FLAC__StreamMetadata &amp;object) const </arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>operator==</name>
      <anchorfile>classFLAC_1_1Metadata_1_1Application.html</anchorfile>
      <anchor>a59490f6d1e1bba670bb53531df8d0f86</anchor>
      <arglist>(const ::FLAC__StreamMetadata *object) const </arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>operator!=</name>
      <anchorfile>classFLAC_1_1Metadata_1_1Application.html</anchorfile>
      <anchor>a381c844b7a30a0d427495d40a04f390f</anchor>
      <arglist>(const Application &amp;object) const </arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>operator!=</name>
      <anchorfile>classFLAC_1_1Metadata_1_1Application.html</anchorfile>
      <anchor>af1bc45d6f584423c9cd6b59f23e7d49e</anchor>
      <arglist>(const ::FLAC__StreamMetadata &amp;object) const </arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>operator!=</name>
      <anchorfile>classFLAC_1_1Metadata_1_1Application.html</anchorfile>
      <anchor>a1bc922263fb36e5db3a42c13990d1563</anchor>
      <arglist>(const ::FLAC__StreamMetadata *object) const </arglist>
    </member>
    <member kind="function" protection="protected">
      <type>Prototype &amp;</type>
      <name>assign_object</name>
      <anchorfile>classFLAC_1_1Metadata_1_1Prototype.html</anchorfile>
      <anchor>acc8ddaac1f1afe9d4fd9de33354847bd</anchor>
      <arglist>(::FLAC__StreamMetadata *object, bool copy)</arglist>
    </member>
    <member kind="function" protection="protected" virtualness="virtual">
      <type>virtual void</type>
      <name>clear</name>
      <anchorfile>classFLAC_1_1Metadata_1_1Prototype.html</anchorfile>
      <anchor>aa54338931745f7f1b1d8240441efedb8</anchor>
      <arglist>()</arglist>
    </member>
  </compound>
  <compound kind="class">
    <name>FLAC::Metadata::Chain</name>
    <filename>classFLAC_1_1Metadata_1_1Chain.html</filename>
    <class kind="class">FLAC::Metadata::Chain::Status</class>
    <member kind="function">
      <type>bool</type>
      <name>is_valid</name>
      <anchorfile>classFLAC_1_1Metadata_1_1Chain.html</anchorfile>
      <anchor>a7c7799a4ed676ac334381af9e3888549</anchor>
      <arglist>() const </arglist>
    </member>
    <member kind="function">
      <type>Status</type>
      <name>status</name>
      <anchorfile>classFLAC_1_1Metadata_1_1Chain.html</anchorfile>
      <anchor>a02d7a4adc89e37b28eaccbccfe5da5b0</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>read</name>
      <anchorfile>classFLAC_1_1Metadata_1_1Chain.html</anchorfile>
      <anchor>a509bf6a75a12df65bc77947a4765d9c1</anchor>
      <arglist>(const char *filename, bool is_ogg=false)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>read</name>
      <anchorfile>classFLAC_1_1Metadata_1_1Chain.html</anchorfile>
      <anchor>a030c805328fc8b2da947830959dafb5b</anchor>
      <arglist>(FLAC__IOHandle handle, FLAC__IOCallbacks callbacks, bool is_ogg=false)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>check_if_tempfile_needed</name>
      <anchorfile>classFLAC_1_1Metadata_1_1Chain.html</anchorfile>
      <anchor>a1d54ed419365faf5429caa84b35265c3</anchor>
      <arglist>(bool use_padding)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>write</name>
      <anchorfile>classFLAC_1_1Metadata_1_1Chain.html</anchorfile>
      <anchor>a2341690885e2312013afc561e6fafd81</anchor>
      <arglist>(bool use_padding=true, bool preserve_file_stats=false)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>write</name>
      <anchorfile>classFLAC_1_1Metadata_1_1Chain.html</anchorfile>
      <anchor>a337e570e1b8fa93f7b586533bcf3eb1a</anchor>
      <arglist>(bool use_padding,::FLAC__IOHandle handle,::FLAC__IOCallbacks callbacks)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>write</name>
      <anchorfile>classFLAC_1_1Metadata_1_1Chain.html</anchorfile>
      <anchor>a4a2826329e53dda6494b41506dd39de3</anchor>
      <arglist>(bool use_padding,::FLAC__IOHandle handle,::FLAC__IOCallbacks callbacks,::FLAC__IOHandle temp_handle,::FLAC__IOCallbacks temp_callbacks)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>merge_padding</name>
      <anchorfile>classFLAC_1_1Metadata_1_1Chain.html</anchorfile>
      <anchor>aef51a0414284f468a2d73c07b540641d</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>sort_padding</name>
      <anchorfile>classFLAC_1_1Metadata_1_1Chain.html</anchorfile>
      <anchor>a779eaac12da7e7edac67089053e5907f</anchor>
      <arglist>()</arglist>
    </member>
  </compound>
  <compound kind="class">
    <name>FLAC::Metadata::Chain::Status</name>
    <filename>classFLAC_1_1Metadata_1_1Chain_1_1Status.html</filename>
  </compound>
  <compound kind="class">
    <name>FLAC::Metadata::CueSheet</name>
    <filename>classFLAC_1_1Metadata_1_1CueSheet.html</filename>
    <base>FLAC::Metadata::Prototype</base>
    <class kind="class">FLAC::Metadata::CueSheet::Track</class>
    <member kind="function">
      <type></type>
      <name>CueSheet</name>
      <anchorfile>classFLAC_1_1Metadata_1_1CueSheet.html</anchorfile>
      <anchor>add934e1916c2427197f8a5654f7ffae9</anchor>
      <arglist>(::FLAC__StreamMetadata *object, bool copy)</arglist>
    </member>
    <member kind="function">
      <type>CueSheet &amp;</type>
      <name>assign</name>
      <anchorfile>classFLAC_1_1Metadata_1_1CueSheet.html</anchorfile>
      <anchor>ac83a472ca9852f3e2e800ae57d3e1305</anchor>
      <arglist>(::FLAC__StreamMetadata *object, bool copy)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>resize_indices</name>
      <anchorfile>classFLAC_1_1Metadata_1_1CueSheet.html</anchorfile>
      <anchor>a01c7fd10f7c139890aa714b8367224f4</anchor>
      <arglist>(unsigned track_num, unsigned new_num_indices)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>insert_index</name>
      <anchorfile>classFLAC_1_1Metadata_1_1CueSheet.html</anchorfile>
      <anchor>a5efe583c258df9c4737c27347dc69175</anchor>
      <arglist>(unsigned track_num, unsigned index_num, const ::FLAC__StreamMetadata_CueSheet_Index &amp;index)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>insert_blank_index</name>
      <anchorfile>classFLAC_1_1Metadata_1_1CueSheet.html</anchorfile>
      <anchor>a4a046e4374c0c8fc7d3295d5cf9de023</anchor>
      <arglist>(unsigned track_num, unsigned index_num)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>delete_index</name>
      <anchorfile>classFLAC_1_1Metadata_1_1CueSheet.html</anchorfile>
      <anchor>aff23e60e082c00e3b70d0ff9bf8dd1d0</anchor>
      <arglist>(unsigned track_num, unsigned index_num)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>resize_tracks</name>
      <anchorfile>classFLAC_1_1Metadata_1_1CueSheet.html</anchorfile>
      <anchor>a402aaf611e67efdc2d67c57755162914</anchor>
      <arglist>(unsigned new_num_tracks)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_track</name>
      <anchorfile>classFLAC_1_1Metadata_1_1CueSheet.html</anchorfile>
      <anchor>a5fb53ab58aec0a980425bcfbae160a60</anchor>
      <arglist>(unsigned i, const Track &amp;track)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>insert_track</name>
      <anchorfile>classFLAC_1_1Metadata_1_1CueSheet.html</anchorfile>
      <anchor>ae6ea59ff07633282504b13aa28c395dc</anchor>
      <arglist>(unsigned i, const Track &amp;track)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>insert_blank_track</name>
      <anchorfile>classFLAC_1_1Metadata_1_1CueSheet.html</anchorfile>
      <anchor>a97ba90f576deda6e7c40c805f9752456</anchor>
      <arglist>(unsigned i)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>delete_track</name>
      <anchorfile>classFLAC_1_1Metadata_1_1CueSheet.html</anchorfile>
      <anchor>a87d737e57b473b11540e98b298d21f51</anchor>
      <arglist>(unsigned i)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>is_legal</name>
      <anchorfile>classFLAC_1_1Metadata_1_1CueSheet.html</anchorfile>
      <anchor>a138691cddff703a729e3e295d82e7619</anchor>
      <arglist>(bool check_cd_da_subset=false, const char **violation=0) const </arglist>
    </member>
    <member kind="function">
      <type>FLAC__uint32</type>
      <name>calculate_cddb_id</name>
      <anchorfile>classFLAC_1_1Metadata_1_1CueSheet.html</anchorfile>
      <anchor>a074694c791cbbaaf59f5baadb8e54e08</anchor>
      <arglist>() const </arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>CueSheet</name>
      <anchorfile>classFLAC_1_1Metadata_1_1CueSheet.html</anchorfile>
      <anchor>aff87fa8ab761fc12c0f37b6ff033f74e</anchor>
      <arglist>(const CueSheet &amp;object)</arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>CueSheet</name>
      <anchorfile>classFLAC_1_1Metadata_1_1CueSheet.html</anchorfile>
      <anchor>a70f56da621341cd05a14bafe3ddded70</anchor>
      <arglist>(const ::FLAC__StreamMetadata &amp;object)</arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>CueSheet</name>
      <anchorfile>classFLAC_1_1Metadata_1_1CueSheet.html</anchorfile>
      <anchor>a0248e035cd9c13338355074d68032e90</anchor>
      <arglist>(const ::FLAC__StreamMetadata *object)</arglist>
    </member>
    <member kind="function">
      <type>CueSheet &amp;</type>
      <name>operator=</name>
      <anchorfile>classFLAC_1_1Metadata_1_1CueSheet.html</anchorfile>
      <anchor>ad24bf2e19de81159d5e205ae5ef63843</anchor>
      <arglist>(const CueSheet &amp;object)</arglist>
    </member>
    <member kind="function">
      <type>CueSheet &amp;</type>
      <name>operator=</name>
      <anchorfile>classFLAC_1_1Metadata_1_1CueSheet.html</anchorfile>
      <anchor>a12ea7ba6371d328708befbc5a13b4325</anchor>
      <arglist>(const ::FLAC__StreamMetadata &amp;object)</arglist>
    </member>
    <member kind="function">
      <type>CueSheet &amp;</type>
      <name>operator=</name>
      <anchorfile>classFLAC_1_1Metadata_1_1CueSheet.html</anchorfile>
      <anchor>a70e0483a06b641a903134c7a8cd6aa9b</anchor>
      <arglist>(const ::FLAC__StreamMetadata *object)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>operator==</name>
      <anchorfile>classFLAC_1_1Metadata_1_1CueSheet.html</anchorfile>
      <anchor>a15f7a6ef9eb4e468c785e8b592154ac4</anchor>
      <arglist>(const CueSheet &amp;object) const </arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>operator==</name>
      <anchorfile>classFLAC_1_1Metadata_1_1CueSheet.html</anchorfile>
      <anchor>a830958ffaef36c12715546fdfd517fe8</anchor>
      <arglist>(const ::FLAC__StreamMetadata &amp;object) const </arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>operator==</name>
      <anchorfile>classFLAC_1_1Metadata_1_1CueSheet.html</anchorfile>
      <anchor>a7d3d828165ca297de71937579153f06d</anchor>
      <arglist>(const ::FLAC__StreamMetadata *object) const </arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>operator!=</name>
      <anchorfile>classFLAC_1_1Metadata_1_1CueSheet.html</anchorfile>
      <anchor>a4e9388122366f6eecd3c61eb4a142dfb</anchor>
      <arglist>(const CueSheet &amp;object) const </arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>operator!=</name>
      <anchorfile>classFLAC_1_1Metadata_1_1CueSheet.html</anchorfile>
      <anchor>a89e08f4e1e11e25018ab6e1a9ab05cb2</anchor>
      <arglist>(const ::FLAC__StreamMetadata &amp;object) const </arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>operator!=</name>
      <anchorfile>classFLAC_1_1Metadata_1_1CueSheet.html</anchorfile>
      <anchor>a28a1f2f3bbb5c6735ecbec2d8d4d812a</anchor>
      <arglist>(const ::FLAC__StreamMetadata *object) const </arglist>
    </member>
  </compound>
  <compound kind="class">
    <name>FLAC::Metadata::CueSheet::Track</name>
    <filename>classFLAC_1_1Metadata_1_1CueSheet_1_1Track.html</filename>
    <member kind="function" virtualness="virtual">
      <type>virtual bool</type>
      <name>is_valid</name>
      <anchorfile>classFLAC_1_1Metadata_1_1CueSheet_1_1Track.html</anchorfile>
      <anchor>a58e1424b3cd080bfa4e9ee927086c00e</anchor>
      <arglist>() const </arglist>
    </member>
  </compound>
  <compound kind="class">
    <name>FLAC::Metadata::Iterator</name>
    <filename>classFLAC_1_1Metadata_1_1Iterator.html</filename>
    <member kind="function">
      <type>bool</type>
      <name>is_valid</name>
      <anchorfile>classFLAC_1_1Metadata_1_1Iterator.html</anchorfile>
      <anchor>a9878c8330bdfb522dc2ce851b016d307</anchor>
      <arglist>() const </arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>init</name>
      <anchorfile>classFLAC_1_1Metadata_1_1Iterator.html</anchorfile>
      <anchor>ab5713af7318f10a46bd8b26ce586947c</anchor>
      <arglist>(Chain &amp;chain)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>next</name>
      <anchorfile>classFLAC_1_1Metadata_1_1Iterator.html</anchorfile>
      <anchor>a1d2871fc1fdcc5dffee1eafd7019f4a0</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>prev</name>
      <anchorfile>classFLAC_1_1Metadata_1_1Iterator.html</anchorfile>
      <anchor>ade6ee6b67b22115959e2adfc65d5d3b4</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>::FLAC__MetadataType</type>
      <name>get_block_type</name>
      <anchorfile>classFLAC_1_1Metadata_1_1Iterator.html</anchorfile>
      <anchor>ac12c815b91018dca2c3143d32bb317e9</anchor>
      <arglist>() const </arglist>
    </member>
    <member kind="function">
      <type>Prototype *</type>
      <name>get_block</name>
      <anchorfile>classFLAC_1_1Metadata_1_1Iterator.html</anchorfile>
      <anchor>a3693233f592b9cb333c437413c6be2a6</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_block</name>
      <anchorfile>classFLAC_1_1Metadata_1_1Iterator.html</anchorfile>
      <anchor>a3123daf89fca2a8981c9f361f466a418</anchor>
      <arglist>(Prototype *block)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>delete_block</name>
      <anchorfile>classFLAC_1_1Metadata_1_1Iterator.html</anchorfile>
      <anchor>a67adaa4ae39cf405ee0f4674ca8836dd</anchor>
      <arglist>(bool replace_with_padding)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>insert_block_before</name>
      <anchorfile>classFLAC_1_1Metadata_1_1Iterator.html</anchorfile>
      <anchor>a86de6d0b21ac08b74a2ea8c1a9adce36</anchor>
      <arglist>(Prototype *block)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>insert_block_after</name>
      <anchorfile>classFLAC_1_1Metadata_1_1Iterator.html</anchorfile>
      <anchor>a73e7a3f7192f369cb3a19d078da504ab</anchor>
      <arglist>(Prototype *block)</arglist>
    </member>
  </compound>
  <compound kind="class">
    <name>FLAC::Metadata::Padding</name>
    <filename>classFLAC_1_1Metadata_1_1Padding.html</filename>
    <base>FLAC::Metadata::Prototype</base>
    <member kind="function">
      <type></type>
      <name>Padding</name>
      <anchorfile>classFLAC_1_1Metadata_1_1Padding.html</anchorfile>
      <anchor>a358085e3cec897ed0b0c88c8ac04618d</anchor>
      <arglist>(::FLAC__StreamMetadata *object, bool copy)</arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>Padding</name>
      <anchorfile>classFLAC_1_1Metadata_1_1Padding.html</anchorfile>
      <anchor>abd84a1378c3fb563fe25576f81316a48</anchor>
      <arglist>(unsigned length)</arglist>
    </member>
    <member kind="function">
      <type>Padding &amp;</type>
      <name>assign</name>
      <anchorfile>classFLAC_1_1Metadata_1_1Padding.html</anchorfile>
      <anchor>a3b7508e56df71854ff1f5ad9570b5684</anchor>
      <arglist>(::FLAC__StreamMetadata *object, bool copy)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>set_length</name>
      <anchorfile>classFLAC_1_1Metadata_1_1Padding.html</anchorfile>
      <anchor>addd51552e05e0c569dc3859431bb2840</anchor>
      <arglist>(unsigned length)</arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>Padding</name>
      <anchorfile>classFLAC_1_1Metadata_1_1Padding.html</anchorfile>
      <anchor>a3a5665a824530dec2906d76e665573ee</anchor>
      <arglist>(const Padding &amp;object)</arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>Padding</name>
      <anchorfile>classFLAC_1_1Metadata_1_1Padding.html</anchorfile>
      <anchor>a8cfa2104a846a25154ca6b431683c563</anchor>
      <arglist>(const ::FLAC__StreamMetadata &amp;object)</arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>Padding</name>
      <anchorfile>classFLAC_1_1Metadata_1_1Padding.html</anchorfile>
      <anchor>a9ffc1c44b0d114998b72e2a9a4be7c0a</anchor>
      <arglist>(const ::FLAC__StreamMetadata *object)</arglist>
    </member>
    <member kind="function">
      <type>Padding &amp;</type>
      <name>operator=</name>
      <anchorfile>classFLAC_1_1Metadata_1_1Padding.html</anchorfile>
      <anchor>aece6ab03932bea3f0c32ff3cd88f2617</anchor>
      <arglist>(const Padding &amp;object)</arglist>
    </member>
    <member kind="function">
      <type>Padding &amp;</type>
      <name>operator=</name>
      <anchorfile>classFLAC_1_1Metadata_1_1Padding.html</anchorfile>
      <anchor>a659c9ca5fa9e53b434a1f08db2e052eb</anchor>
      <arglist>(const ::FLAC__StreamMetadata &amp;object)</arglist>
    </member>
    <member kind="function">
      <type>Padding &amp;</type>
      <name>operator=</name>
      <anchorfile>classFLAC_1_1Metadata_1_1Padding.html</anchorfile>
      <anchor>a7c8adf0a827ea52ffbb51549f36dc1ac</anchor>
      <arglist>(const ::FLAC__StreamMetadata *object)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>operator==</name>
      <anchorfile>classFLAC_1_1Metadata_1_1Padding.html</anchorfile>
      <anchor>a9e904b10ec56b516c481cb8322151471</anchor>
      <arglist>(const Padding &amp;object) const </arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>operator==</name>
      <anchorfile>classFLAC_1_1Metadata_1_1Padding.html</anchorfile>
      <anchor>a50d554e3d3252cfcec730faf0b6c25dc</anchor>
      <arglist>(const ::FLAC__StreamMetadata &amp;object) const </arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>operator==</name>
      <anchorfile>classFLAC_1_1Metadata_1_1Padding.html</anchorfile>
      <anchor>a1afe80a4ef6a3cbeead575aa22c974f9</anchor>
      <arglist>(const ::FLAC__StreamMetadata *object) const </arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>operator!=</name>
      <anchorfile>classFLAC_1_1Metadata_1_1Padding.html</anchorfile>
      <anchor>a21d2734b6dc97e537806eef07f0db5be</anchor>
      <arglist>(const Padding &amp;object) const </arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>operator!=</name>
      <anchorfile>classFLAC_1_1Metadata_1_1Padding.html</anchorfile>
      <anchor>ad12b6548ce88c8789a90d2c24c40f3cc</anchor>
      <arglist>(const ::FLAC__StreamMetadata &amp;object) const </arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>operator!=</name>
      <anchorfile>classFLAC_1_1Metadata_1_1Padding.html</anchorfile>
      <anchor>a15aadefaa426aa8b7a952f6a61c4c3b5</anchor>
      <arglist>(const ::FLAC__StreamMetadata *object) const </arglist>
    </member>
  </compound>
  <compound kind="class">
    <name>FLAC::Metadata::Picture</name>
    <filename>classFLAC_1_1Metadata_1_1Picture.html</filename>
    <base>FLAC::Metadata::Prototype</base>
    <member kind="function">
      <type></type>
      <name>Picture</name>
      <anchorfile>classFLAC_1_1Metadata_1_1Picture.html</anchorfile>
      <anchor>a703d5d8a88e9764714ee2dd25806e381</anchor>
      <arglist>(::FLAC__StreamMetadata *object, bool copy)</arglist>
    </member>
    <member kind="function">
      <type>Picture &amp;</type>
      <name>assign</name>
      <anchorfile>classFLAC_1_1Metadata_1_1Picture.html</anchorfile>
      <anchor>aa3d7384cb724a842c3471a9ab19f81ed</anchor>
      <arglist>(::FLAC__StreamMetadata *object, bool copy)</arglist>
    </member>
    <member kind="function">
      <type>FLAC__uint32</type>
      <name>get_colors</name>
      <anchorfile>classFLAC_1_1Metadata_1_1Picture.html</anchorfile>
      <anchor>ab2dd65f7673b3fd167c79125ea013685</anchor>
      <arglist>() const </arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_mime_type</name>
      <anchorfile>classFLAC_1_1Metadata_1_1Picture.html</anchorfile>
      <anchor>afb4e53cb8ae62ea0d9ebd1afdca40c3f</anchor>
      <arglist>(const char *string)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_description</name>
      <anchorfile>classFLAC_1_1Metadata_1_1Picture.html</anchorfile>
      <anchor>a1bbcd96802a16fc36ac1b6610cd7d4a3</anchor>
      <arglist>(const FLAC__byte *string)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>set_colors</name>
      <anchorfile>classFLAC_1_1Metadata_1_1Picture.html</anchorfile>
      <anchor>a4bac0286c0fd4de4d2be8d9ba1ba50a5</anchor>
      <arglist>(FLAC__uint32 value) const </arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_data</name>
      <anchorfile>classFLAC_1_1Metadata_1_1Picture.html</anchorfile>
      <anchor>a301630d1c8f7647d0f192e6a2a03e6ba</anchor>
      <arglist>(const FLAC__byte *data, FLAC__uint32 data_length)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>is_legal</name>
      <anchorfile>classFLAC_1_1Metadata_1_1Picture.html</anchorfile>
      <anchor>af11147e2041b46d679b077e6ac26bea0</anchor>
      <arglist>(const char **violation)</arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>Picture</name>
      <anchorfile>classFLAC_1_1Metadata_1_1Picture.html</anchorfile>
      <anchor>a368985afb060fe1024129ed808392183</anchor>
      <arglist>(const Picture &amp;object)</arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>Picture</name>
      <anchorfile>classFLAC_1_1Metadata_1_1Picture.html</anchorfile>
      <anchor>a447a5837baeb1e5de9e7ae642a15e736</anchor>
      <arglist>(const ::FLAC__StreamMetadata &amp;object)</arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>Picture</name>
      <anchorfile>classFLAC_1_1Metadata_1_1Picture.html</anchorfile>
      <anchor>a5403f0a99ef43e17c43a4ec21c275c83</anchor>
      <arglist>(const ::FLAC__StreamMetadata *object)</arglist>
    </member>
    <member kind="function">
      <type>Picture &amp;</type>
      <name>operator=</name>
      <anchorfile>classFLAC_1_1Metadata_1_1Picture.html</anchorfile>
      <anchor>a2ab3ef473f6c70aafe5bd3229f397a93</anchor>
      <arglist>(const Picture &amp;object)</arglist>
    </member>
    <member kind="function">
      <type>Picture &amp;</type>
      <name>operator=</name>
      <anchorfile>classFLAC_1_1Metadata_1_1Picture.html</anchorfile>
      <anchor>a5681e4ee272c9f604a27c2d3b95e284b</anchor>
      <arglist>(const ::FLAC__StreamMetadata &amp;object)</arglist>
    </member>
    <member kind="function">
      <type>Picture &amp;</type>
      <name>operator=</name>
      <anchorfile>classFLAC_1_1Metadata_1_1Picture.html</anchorfile>
      <anchor>a4ea95912972eee1d8c9906eae4cfe6ee</anchor>
      <arglist>(const ::FLAC__StreamMetadata *object)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>operator==</name>
      <anchorfile>classFLAC_1_1Metadata_1_1Picture.html</anchorfile>
      <anchor>ac704e9f8a9de464b480b00e87c26eaeb</anchor>
      <arglist>(const Picture &amp;object) const </arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>operator==</name>
      <anchorfile>classFLAC_1_1Metadata_1_1Picture.html</anchorfile>
      <anchor>a0278f967eb983510a3d896e1d5e788a4</anchor>
      <arglist>(const ::FLAC__StreamMetadata &amp;object) const </arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>operator==</name>
      <anchorfile>classFLAC_1_1Metadata_1_1Picture.html</anchorfile>
      <anchor>a7c52db6ddf65848f6b6a2f24b77b5cc9</anchor>
      <arglist>(const ::FLAC__StreamMetadata *object) const </arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>operator!=</name>
      <anchorfile>classFLAC_1_1Metadata_1_1Picture.html</anchorfile>
      <anchor>afa4be54429fc22789d2251b165cb47e9</anchor>
      <arglist>(const Picture &amp;object) const </arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>operator!=</name>
      <anchorfile>classFLAC_1_1Metadata_1_1Picture.html</anchorfile>
      <anchor>aa7cc7be3cde674d232192aec1242943b</anchor>
      <arglist>(const ::FLAC__StreamMetadata &amp;object) const </arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>operator!=</name>
      <anchorfile>classFLAC_1_1Metadata_1_1Picture.html</anchorfile>
      <anchor>a5f06bc498ad83582a601562180eb6211</anchor>
      <arglist>(const ::FLAC__StreamMetadata *object) const </arglist>
    </member>
  </compound>
  <compound kind="class">
    <name>FLAC::Metadata::Prototype</name>
    <filename>classFLAC_1_1Metadata_1_1Prototype.html</filename>
    <member kind="function" virtualness="virtual">
      <type>virtual</type>
      <name>~Prototype</name>
      <anchorfile>classFLAC_1_1Metadata_1_1Prototype.html</anchorfile>
      <anchor>a698fa1529af534ab5d1d98d0979844f6</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function" protection="protected">
      <type></type>
      <name>Prototype</name>
      <anchorfile>group__flacpp__metadata__level2.html</anchorfile>
      <anchor>gae49fa399a6273ccad7cb0e6f787a3f5c</anchor>
      <arglist>(const Prototype &amp;)</arglist>
    </member>
    <member kind="function" protection="protected">
      <type></type>
      <name>Prototype</name>
      <anchorfile>classFLAC_1_1Metadata_1_1Prototype.html</anchorfile>
      <anchor>a23ec8d118119578adb95de42fcbbaca2</anchor>
      <arglist>(::FLAC__StreamMetadata *object, bool copy)</arglist>
    </member>
    <member kind="function" protection="protected">
      <type>Prototype &amp;</type>
      <name>operator=</name>
      <anchorfile>classFLAC_1_1Metadata_1_1Prototype.html</anchorfile>
      <anchor>aea76819568855c4f49f2a23d42a642f2</anchor>
      <arglist>(const Prototype &amp;)</arglist>
    </member>
    <member kind="function" protection="protected">
      <type>Prototype &amp;</type>
      <name>operator=</name>
      <anchorfile>classFLAC_1_1Metadata_1_1Prototype.html</anchorfile>
      <anchor>a5a50c17eaa77149842075ed3896637e7</anchor>
      <arglist>(const ::FLAC__StreamMetadata &amp;)</arglist>
    </member>
    <member kind="function" protection="protected">
      <type>Prototype &amp;</type>
      <name>operator=</name>
      <anchorfile>classFLAC_1_1Metadata_1_1Prototype.html</anchorfile>
      <anchor>a310d01131f3d8f4a1bf0ed31b8798685</anchor>
      <arglist>(const ::FLAC__StreamMetadata *)</arglist>
    </member>
  </compound>
  <compound kind="class">
    <name>FLAC::Metadata::SeekTable</name>
    <filename>classFLAC_1_1Metadata_1_1SeekTable.html</filename>
    <base>FLAC::Metadata::Prototype</base>
    <member kind="function">
      <type></type>
      <name>SeekTable</name>
      <anchorfile>classFLAC_1_1Metadata_1_1SeekTable.html</anchorfile>
      <anchor>accd82ef77dcc489280c0f46e443b16c7</anchor>
      <arglist>(::FLAC__StreamMetadata *object, bool copy)</arglist>
    </member>
    <member kind="function">
      <type>SeekTable &amp;</type>
      <name>assign</name>
      <anchorfile>classFLAC_1_1Metadata_1_1SeekTable.html</anchorfile>
      <anchor>ad9d0036938d6ad1c81180cf1e156b844</anchor>
      <arglist>(::FLAC__StreamMetadata *object, bool copy)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>resize_points</name>
      <anchorfile>classFLAC_1_1Metadata_1_1SeekTable.html</anchorfile>
      <anchor>a8b52cd4e3d1893073d5fc299efd9f466</anchor>
      <arglist>(unsigned new_num_points)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>set_point</name>
      <anchorfile>classFLAC_1_1Metadata_1_1SeekTable.html</anchorfile>
      <anchor>a992aa37cd6a781b3e1f64a2d1d982229</anchor>
      <arglist>(unsigned index, const ::FLAC__StreamMetadata_SeekPoint &amp;point)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>insert_point</name>
      <anchorfile>classFLAC_1_1Metadata_1_1SeekTable.html</anchorfile>
      <anchor>a688ee146faf497ef96e953008adcc95a</anchor>
      <arglist>(unsigned index, const ::FLAC__StreamMetadata_SeekPoint &amp;point)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>delete_point</name>
      <anchorfile>classFLAC_1_1Metadata_1_1SeekTable.html</anchorfile>
      <anchor>af5962d076051a3e46379475c3f2b4fa8</anchor>
      <arglist>(unsigned index)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>is_legal</name>
      <anchorfile>classFLAC_1_1Metadata_1_1SeekTable.html</anchorfile>
      <anchor>aa98d82380ed3c33e1090edbf444fa280</anchor>
      <arglist>() const </arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>template_append_placeholders</name>
      <anchorfile>classFLAC_1_1Metadata_1_1SeekTable.html</anchorfile>
      <anchor>ae5be56e6dd95f27cb708206285bde4d8</anchor>
      <arglist>(unsigned num)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>template_append_point</name>
      <anchorfile>classFLAC_1_1Metadata_1_1SeekTable.html</anchorfile>
      <anchor>a9c05d6c010988cf2f336ab1c02c3c618</anchor>
      <arglist>(FLAC__uint64 sample_number)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>template_append_points</name>
      <anchorfile>classFLAC_1_1Metadata_1_1SeekTable.html</anchorfile>
      <anchor>a645edc4788158d141d67dee5d078594a</anchor>
      <arglist>(FLAC__uint64 sample_numbers[], unsigned num)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>template_append_spaced_points</name>
      <anchorfile>classFLAC_1_1Metadata_1_1SeekTable.html</anchorfile>
      <anchor>a41393ffc88aaa59772081b2b4a211446</anchor>
      <arglist>(unsigned num, FLAC__uint64 total_samples)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>template_append_spaced_points_by_samples</name>
      <anchorfile>classFLAC_1_1Metadata_1_1SeekTable.html</anchorfile>
      <anchor>ab5b672df3c57703c1db965ce12ca8f45</anchor>
      <arglist>(unsigned samples, FLAC__uint64 total_samples)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>template_sort</name>
      <anchorfile>classFLAC_1_1Metadata_1_1SeekTable.html</anchorfile>
      <anchor>a09cc5c101fc9c26655de9ec91dcb502f</anchor>
      <arglist>(bool compact)</arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>SeekTable</name>
      <anchorfile>classFLAC_1_1Metadata_1_1SeekTable.html</anchorfile>
      <anchor>a7f93d054937829a85108cd423a56299f</anchor>
      <arglist>(const SeekTable &amp;object)</arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>SeekTable</name>
      <anchorfile>classFLAC_1_1Metadata_1_1SeekTable.html</anchorfile>
      <anchor>a9a39c8eef9d57d84008eb68474c6fa6f</anchor>
      <arglist>(const ::FLAC__StreamMetadata &amp;object)</arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>SeekTable</name>
      <anchorfile>classFLAC_1_1Metadata_1_1SeekTable.html</anchorfile>
      <anchor>af544e2467d49d7b8610bf4ad8e14969b</anchor>
      <arglist>(const ::FLAC__StreamMetadata *object)</arglist>
    </member>
    <member kind="function">
      <type>SeekTable &amp;</type>
      <name>operator=</name>
      <anchorfile>classFLAC_1_1Metadata_1_1SeekTable.html</anchorfile>
      <anchor>ac1094c0536952a569e41ba619f9b4ff5</anchor>
      <arglist>(const SeekTable &amp;object)</arglist>
    </member>
    <member kind="function">
      <type>SeekTable &amp;</type>
      <name>operator=</name>
      <anchorfile>classFLAC_1_1Metadata_1_1SeekTable.html</anchorfile>
      <anchor>a72426d86f7e7f9ddc4889b2efcbcbc19</anchor>
      <arglist>(const ::FLAC__StreamMetadata &amp;object)</arglist>
    </member>
    <member kind="function">
      <type>SeekTable &amp;</type>
      <name>operator=</name>
      <anchorfile>classFLAC_1_1Metadata_1_1SeekTable.html</anchorfile>
      <anchor>a4fade1457b75e99d30a0877403ff8e76</anchor>
      <arglist>(const ::FLAC__StreamMetadata *object)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>operator==</name>
      <anchorfile>classFLAC_1_1Metadata_1_1SeekTable.html</anchorfile>
      <anchor>af17b7d85c8077020a69549d520b18542</anchor>
      <arglist>(const SeekTable &amp;object) const </arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>operator==</name>
      <anchorfile>classFLAC_1_1Metadata_1_1SeekTable.html</anchorfile>
      <anchor>af6e520d60392c77e1fc59e2d86a61203</anchor>
      <arglist>(const ::FLAC__StreamMetadata &amp;object) const </arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>operator==</name>
      <anchorfile>classFLAC_1_1Metadata_1_1SeekTable.html</anchorfile>
      <anchor>ac3b20d126863b9d4e3de695960ff3b31</anchor>
      <arglist>(const ::FLAC__StreamMetadata *object) const </arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>operator!=</name>
      <anchorfile>classFLAC_1_1Metadata_1_1SeekTable.html</anchorfile>
      <anchor>ab120ab96e99d68d607badd473b8a74eb</anchor>
      <arglist>(const SeekTable &amp;object) const </arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>operator!=</name>
      <anchorfile>classFLAC_1_1Metadata_1_1SeekTable.html</anchorfile>
      <anchor>a3e264fa0e0ec795b47e8d204d27b418e</anchor>
      <arglist>(const ::FLAC__StreamMetadata &amp;object) const </arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>operator!=</name>
      <anchorfile>classFLAC_1_1Metadata_1_1SeekTable.html</anchorfile>
      <anchor>a267984dede404dc9cf544aa752dbb6df</anchor>
      <arglist>(const ::FLAC__StreamMetadata *object) const </arglist>
    </member>
  </compound>
  <compound kind="class">
    <name>FLAC::Metadata::SimpleIterator</name>
    <filename>classFLAC_1_1Metadata_1_1SimpleIterator.html</filename>
    <class kind="class">FLAC::Metadata::SimpleIterator::Status</class>
    <member kind="function">
      <type>bool</type>
      <name>is_valid</name>
      <anchorfile>classFLAC_1_1Metadata_1_1SimpleIterator.html</anchorfile>
      <anchor>acd4f5f18a3b5f7c42677e6a234543366</anchor>
      <arglist>() const </arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>init</name>
      <anchorfile>classFLAC_1_1Metadata_1_1SimpleIterator.html</anchorfile>
      <anchor>a67dc75f18d282f41696467f1fbf5c3e8</anchor>
      <arglist>(const char *filename, bool read_only, bool preserve_file_stats)</arglist>
    </member>
    <member kind="function">
      <type>Status</type>
      <name>status</name>
      <anchorfile>classFLAC_1_1Metadata_1_1SimpleIterator.html</anchorfile>
      <anchor>a9e681b6ad35b10633002ecea5cab37c3</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>is_writable</name>
      <anchorfile>classFLAC_1_1Metadata_1_1SimpleIterator.html</anchorfile>
      <anchor>aef81191b644d6c0faade9da888301bcd</anchor>
      <arglist>() const </arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>next</name>
      <anchorfile>classFLAC_1_1Metadata_1_1SimpleIterator.html</anchorfile>
      <anchor>ab399f6b8c5e35a1d18588279613ea63c</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>prev</name>
      <anchorfile>classFLAC_1_1Metadata_1_1SimpleIterator.html</anchorfile>
      <anchor>a75a859af156322f451045418876eb6a3</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>is_last</name>
      <anchorfile>classFLAC_1_1Metadata_1_1SimpleIterator.html</anchorfile>
      <anchor>af3bd51827d4768d9ee1a244c157490b9</anchor>
      <arglist>() const </arglist>
    </member>
    <member kind="function">
      <type>off_t</type>
      <name>get_block_offset</name>
      <anchorfile>classFLAC_1_1Metadata_1_1SimpleIterator.html</anchorfile>
      <anchor>ae347bb1435a83a389672079833ac9ebe</anchor>
      <arglist>() const </arglist>
    </member>
    <member kind="function">
      <type>::FLAC__MetadataType</type>
      <name>get_block_type</name>
      <anchorfile>classFLAC_1_1Metadata_1_1SimpleIterator.html</anchorfile>
      <anchor>a6e0179859e65917dc0d0f97934f95225</anchor>
      <arglist>() const </arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>get_block_length</name>
      <anchorfile>classFLAC_1_1Metadata_1_1SimpleIterator.html</anchorfile>
      <anchor>a9c1c90462df4e4b67279a44769d600e2</anchor>
      <arglist>() const </arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>get_application_id</name>
      <anchorfile>classFLAC_1_1Metadata_1_1SimpleIterator.html</anchorfile>
      <anchor>a426d06a9d079f74e82eaa217f14997a5</anchor>
      <arglist>(FLAC__byte *id)</arglist>
    </member>
    <member kind="function">
      <type>Prototype *</type>
      <name>get_block</name>
      <anchorfile>classFLAC_1_1Metadata_1_1SimpleIterator.html</anchorfile>
      <anchor>ab206e5d7145d3726335d336cbc452598</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_block</name>
      <anchorfile>classFLAC_1_1Metadata_1_1SimpleIterator.html</anchorfile>
      <anchor>a0ebd4df55346cbcec9ace04f7d7b484d</anchor>
      <arglist>(Prototype *block, bool use_padding=true)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>insert_block_after</name>
      <anchorfile>classFLAC_1_1Metadata_1_1SimpleIterator.html</anchorfile>
      <anchor>a1d0e512147967b7e12ac22914fbe3818</anchor>
      <arglist>(Prototype *block, bool use_padding=true)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>delete_block</name>
      <anchorfile>classFLAC_1_1Metadata_1_1SimpleIterator.html</anchorfile>
      <anchor>a67824deff81e2f49c2f51db6b71565e8</anchor>
      <arglist>(bool use_padding=true)</arglist>
    </member>
  </compound>
  <compound kind="class">
    <name>FLAC::Metadata::SimpleIterator::Status</name>
    <filename>classFLAC_1_1Metadata_1_1SimpleIterator_1_1Status.html</filename>
  </compound>
  <compound kind="class">
    <name>FLAC::Metadata::StreamInfo</name>
    <filename>classFLAC_1_1Metadata_1_1StreamInfo.html</filename>
    <base>FLAC::Metadata::Prototype</base>
    <member kind="function">
      <type></type>
      <name>StreamInfo</name>
      <anchorfile>classFLAC_1_1Metadata_1_1StreamInfo.html</anchorfile>
      <anchor>aaf4d96124e2b323398f7edf1aaf28003</anchor>
      <arglist>(::FLAC__StreamMetadata *object, bool copy)</arglist>
    </member>
    <member kind="function">
      <type>StreamInfo &amp;</type>
      <name>assign</name>
      <anchorfile>classFLAC_1_1Metadata_1_1StreamInfo.html</anchorfile>
      <anchor>ad1193a408a5735845dea17a131b7282c</anchor>
      <arglist>(::FLAC__StreamMetadata *object, bool copy)</arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>StreamInfo</name>
      <anchorfile>classFLAC_1_1Metadata_1_1StreamInfo.html</anchorfile>
      <anchor>ab86611073f13dd3e7aea386bb6f1a7a4</anchor>
      <arglist>(const StreamInfo &amp;object)</arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>StreamInfo</name>
      <anchorfile>classFLAC_1_1Metadata_1_1StreamInfo.html</anchorfile>
      <anchor>a3f23948afbcb54758d0ed20edd86515c</anchor>
      <arglist>(const ::FLAC__StreamMetadata &amp;object)</arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>StreamInfo</name>
      <anchorfile>classFLAC_1_1Metadata_1_1StreamInfo.html</anchorfile>
      <anchor>a24e2028916ac96e0ed0d1e53e003b150</anchor>
      <arglist>(const ::FLAC__StreamMetadata *object)</arglist>
    </member>
    <member kind="function">
      <type>StreamInfo &amp;</type>
      <name>operator=</name>
      <anchorfile>classFLAC_1_1Metadata_1_1StreamInfo.html</anchorfile>
      <anchor>a353a63aa812f125fedec844142946142</anchor>
      <arglist>(const StreamInfo &amp;object)</arglist>
    </member>
    <member kind="function">
      <type>StreamInfo &amp;</type>
      <name>operator=</name>
      <anchorfile>classFLAC_1_1Metadata_1_1StreamInfo.html</anchorfile>
      <anchor>aeed95856e0b773e6634848da322d4e43</anchor>
      <arglist>(const ::FLAC__StreamMetadata &amp;object)</arglist>
    </member>
    <member kind="function">
      <type>StreamInfo &amp;</type>
      <name>operator=</name>
      <anchorfile>classFLAC_1_1Metadata_1_1StreamInfo.html</anchorfile>
      <anchor>a16b032050bc7ac632d9f48ac43b04eb2</anchor>
      <arglist>(const ::FLAC__StreamMetadata *object)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>operator==</name>
      <anchorfile>classFLAC_1_1Metadata_1_1StreamInfo.html</anchorfile>
      <anchor>af0d9d9b8c04978a557f31405f84bc5bc</anchor>
      <arglist>(const StreamInfo &amp;object) const </arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>operator==</name>
      <anchorfile>classFLAC_1_1Metadata_1_1StreamInfo.html</anchorfile>
      <anchor>a6c9c3933f0e860ba3341c1018aa8d311</anchor>
      <arglist>(const ::FLAC__StreamMetadata &amp;object) const </arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>operator==</name>
      <anchorfile>classFLAC_1_1Metadata_1_1StreamInfo.html</anchorfile>
      <anchor>ab6ffc53c9a75e9e0e108148777b03c26</anchor>
      <arglist>(const ::FLAC__StreamMetadata *object) const </arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>operator!=</name>
      <anchorfile>classFLAC_1_1Metadata_1_1StreamInfo.html</anchorfile>
      <anchor>a0612e481e3e2d1885b61beb8284565df</anchor>
      <arglist>(const StreamInfo &amp;object) const </arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>operator!=</name>
      <anchorfile>classFLAC_1_1Metadata_1_1StreamInfo.html</anchorfile>
      <anchor>aac1c0513b2503964bd98b14f60543f57</anchor>
      <arglist>(const ::FLAC__StreamMetadata &amp;object) const </arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>operator!=</name>
      <anchorfile>classFLAC_1_1Metadata_1_1StreamInfo.html</anchorfile>
      <anchor>a36983ed34f4bc9218ac66426983e0a3a</anchor>
      <arglist>(const ::FLAC__StreamMetadata *object) const </arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>get_min_blocksize</name>
      <anchorfile>classFLAC_1_1Metadata_1_1StreamInfo.html</anchorfile>
      <anchor>a037f1536c4e513a83451593d81154229</anchor>
      <arglist>() const </arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>get_max_blocksize</name>
      <anchorfile>classFLAC_1_1Metadata_1_1StreamInfo.html</anchorfile>
      <anchor>a7914773d3467ff2d28e42f0336db6ec1</anchor>
      <arglist>() const </arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>get_min_framesize</name>
      <anchorfile>classFLAC_1_1Metadata_1_1StreamInfo.html</anchorfile>
      <anchor>ac3bf7cf9fece15653e65274f6edc66ba</anchor>
      <arglist>() const </arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>get_max_framesize</name>
      <anchorfile>classFLAC_1_1Metadata_1_1StreamInfo.html</anchorfile>
      <anchor>a3f6bc12d3df2b281041d3430d1ad1358</anchor>
      <arglist>() const </arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>get_sample_rate</name>
      <anchorfile>classFLAC_1_1Metadata_1_1StreamInfo.html</anchorfile>
      <anchor>a647472ee26bd431dd8f54fd160ba42ac</anchor>
      <arglist>() const </arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>get_channels</name>
      <anchorfile>classFLAC_1_1Metadata_1_1StreamInfo.html</anchorfile>
      <anchor>ae4a45951634aa55d776ca7180756c18f</anchor>
      <arglist>() const </arglist>
    </member>
    <member kind="function">
      <type>unsigned</type>
      <name>get_bits_per_sample</name>
      <anchorfile>classFLAC_1_1Metadata_1_1StreamInfo.html</anchorfile>
      <anchor>aca391f88a484da9c26236b8234e3fe80</anchor>
      <arglist>() const </arglist>
    </member>
    <member kind="function">
      <type>FLAC__uint64</type>
      <name>get_total_samples</name>
      <anchorfile>classFLAC_1_1Metadata_1_1StreamInfo.html</anchorfile>
      <anchor>ac37fd8bb9ab3b45a11d0a4787a2b0a93</anchor>
      <arglist>() const </arglist>
    </member>
    <member kind="function">
      <type>const FLAC__byte *</type>
      <name>get_md5sum</name>
      <anchorfile>classFLAC_1_1Metadata_1_1StreamInfo.html</anchorfile>
      <anchor>a940e28eb59a960fe12aca839ac32e5ce</anchor>
      <arglist>() const </arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>set_min_blocksize</name>
      <anchorfile>classFLAC_1_1Metadata_1_1StreamInfo.html</anchorfile>
      <anchor>ab6f6626737e0c42066dba690edce36de</anchor>
      <arglist>(unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>set_max_blocksize</name>
      <anchorfile>classFLAC_1_1Metadata_1_1StreamInfo.html</anchorfile>
      <anchor>a1f61e1285252714f536f5cbc9d21a37b</anchor>
      <arglist>(unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>set_min_framesize</name>
      <anchorfile>classFLAC_1_1Metadata_1_1StreamInfo.html</anchorfile>
      <anchor>a2bd0480bae277d2b88c55b73f6d14b23</anchor>
      <arglist>(unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>set_max_framesize</name>
      <anchorfile>classFLAC_1_1Metadata_1_1StreamInfo.html</anchorfile>
      <anchor>a67735d6f89d3e0387abec989dcd4fd23</anchor>
      <arglist>(unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>set_sample_rate</name>
      <anchorfile>classFLAC_1_1Metadata_1_1StreamInfo.html</anchorfile>
      <anchor>a08df92c03bf07fb9f3a438820eacb734</anchor>
      <arglist>(unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>set_channels</name>
      <anchorfile>classFLAC_1_1Metadata_1_1StreamInfo.html</anchorfile>
      <anchor>a7d813d6f3d64570126858492a786b885</anchor>
      <arglist>(unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>set_bits_per_sample</name>
      <anchorfile>classFLAC_1_1Metadata_1_1StreamInfo.html</anchorfile>
      <anchor>a53e22a2b6765d96552ff31a0b21fbbdd</anchor>
      <arglist>(unsigned value)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>set_total_samples</name>
      <anchorfile>classFLAC_1_1Metadata_1_1StreamInfo.html</anchorfile>
      <anchor>a57250a3a3a7a666c39ec0ac6d8432472</anchor>
      <arglist>(FLAC__uint64 value)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>set_md5sum</name>
      <anchorfile>classFLAC_1_1Metadata_1_1StreamInfo.html</anchorfile>
      <anchor>afef84aaea3c333ad880e7843c70aed02</anchor>
      <arglist>(const FLAC__byte value[16])</arglist>
    </member>
  </compound>
  <compound kind="class">
    <name>FLAC::Metadata::Unknown</name>
    <filename>classFLAC_1_1Metadata_1_1Unknown.html</filename>
    <base>FLAC::Metadata::Prototype</base>
    <member kind="function">
      <type></type>
      <name>Unknown</name>
      <anchorfile>classFLAC_1_1Metadata_1_1Unknown.html</anchorfile>
      <anchor>a2fb76f94e891c3eea7209a461cab4279</anchor>
      <arglist>(::FLAC__StreamMetadata *object, bool copy)</arglist>
    </member>
    <member kind="function">
      <type>Unknown &amp;</type>
      <name>assign</name>
      <anchorfile>classFLAC_1_1Metadata_1_1Unknown.html</anchorfile>
      <anchor>a4dc5e794c8d529245888414b2bf7d404</anchor>
      <arglist>(::FLAC__StreamMetadata *object, bool copy)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_data</name>
      <anchorfile>classFLAC_1_1Metadata_1_1Unknown.html</anchorfile>
      <anchor>a8621d3eff6af5acc278297b3d2837a76</anchor>
      <arglist>(const FLAC__byte *data, unsigned length)</arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>Unknown</name>
      <anchorfile>classFLAC_1_1Metadata_1_1Unknown.html</anchorfile>
      <anchor>a686a799c353cf7a3dc95bb8899318a6b</anchor>
      <arglist>(const Unknown &amp;object)</arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>Unknown</name>
      <anchorfile>classFLAC_1_1Metadata_1_1Unknown.html</anchorfile>
      <anchor>ad3e590e4c78eeda42021fe88b85bdf91</anchor>
      <arglist>(const ::FLAC__StreamMetadata &amp;object)</arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>Unknown</name>
      <anchorfile>classFLAC_1_1Metadata_1_1Unknown.html</anchorfile>
      <anchor>a320c150b6c1c9b1386390eef1c581172</anchor>
      <arglist>(const ::FLAC__StreamMetadata *object)</arglist>
    </member>
    <member kind="function">
      <type>Unknown &amp;</type>
      <name>operator=</name>
      <anchorfile>classFLAC_1_1Metadata_1_1Unknown.html</anchorfile>
      <anchor>a295f824df8ed10c3386df72272fdca47</anchor>
      <arglist>(const Unknown &amp;object)</arglist>
    </member>
    <member kind="function">
      <type>Unknown &amp;</type>
      <name>operator=</name>
      <anchorfile>classFLAC_1_1Metadata_1_1Unknown.html</anchorfile>
      <anchor>a5b51e8afe12e5359386d7a85ac330d6e</anchor>
      <arglist>(const ::FLAC__StreamMetadata &amp;object)</arglist>
    </member>
    <member kind="function">
      <type>Unknown &amp;</type>
      <name>operator=</name>
      <anchorfile>classFLAC_1_1Metadata_1_1Unknown.html</anchorfile>
      <anchor>ab0f5ba02518c5893fe93429292f62ef6</anchor>
      <arglist>(const ::FLAC__StreamMetadata *object)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>operator==</name>
      <anchorfile>classFLAC_1_1Metadata_1_1Unknown.html</anchorfile>
      <anchor>a39867d2a96585a55eac42e490704dd6b</anchor>
      <arglist>(const Unknown &amp;object) const </arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>operator==</name>
      <anchorfile>classFLAC_1_1Metadata_1_1Unknown.html</anchorfile>
      <anchor>a90b655d4ad2d09a9cd14e428f9c9444a</anchor>
      <arglist>(const ::FLAC__StreamMetadata &amp;object) const </arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>operator==</name>
      <anchorfile>classFLAC_1_1Metadata_1_1Unknown.html</anchorfile>
      <anchor>a62f32c3673818b30d585e2a4620a88a3</anchor>
      <arglist>(const ::FLAC__StreamMetadata *object) const </arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>operator!=</name>
      <anchorfile>classFLAC_1_1Metadata_1_1Unknown.html</anchorfile>
      <anchor>a73017531fd845a31bb7cd731261e1aad</anchor>
      <arglist>(const Unknown &amp;object) const </arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>operator!=</name>
      <anchorfile>classFLAC_1_1Metadata_1_1Unknown.html</anchorfile>
      <anchor>a82f5022359870567cf2ef96c2400faf2</anchor>
      <arglist>(const ::FLAC__StreamMetadata &amp;object) const </arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>operator!=</name>
      <anchorfile>classFLAC_1_1Metadata_1_1Unknown.html</anchorfile>
      <anchor>a49d25242fa6451fb99271678764d1b2b</anchor>
      <arglist>(const ::FLAC__StreamMetadata *object) const </arglist>
    </member>
  </compound>
  <compound kind="class">
    <name>FLAC::Metadata::VorbisComment</name>
    <filename>classFLAC_1_1Metadata_1_1VorbisComment.html</filename>
    <base>FLAC::Metadata::Prototype</base>
    <class kind="class">FLAC::Metadata::VorbisComment::Entry</class>
    <member kind="function">
      <type></type>
      <name>VorbisComment</name>
      <anchorfile>classFLAC_1_1Metadata_1_1VorbisComment.html</anchorfile>
      <anchor>a65a73f4665db16ac7aec76e9f5e699f2</anchor>
      <arglist>(::FLAC__StreamMetadata *object, bool copy)</arglist>
    </member>
    <member kind="function">
      <type>VorbisComment &amp;</type>
      <name>assign</name>
      <anchorfile>classFLAC_1_1Metadata_1_1VorbisComment.html</anchorfile>
      <anchor>a9db2171c398cd62a5907e625c3a6228d</anchor>
      <arglist>(::FLAC__StreamMetadata *object, bool copy)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_vendor_string</name>
      <anchorfile>classFLAC_1_1Metadata_1_1VorbisComment.html</anchorfile>
      <anchor>ad8cffdb4c43ba01eaa9a3f7be0d5926a</anchor>
      <arglist>(const FLAC__byte *string)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>resize_comments</name>
      <anchorfile>classFLAC_1_1Metadata_1_1VorbisComment.html</anchorfile>
      <anchor>a64795c2fc4a098e753f576e85e62c3c8</anchor>
      <arglist>(unsigned new_num_comments)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>set_comment</name>
      <anchorfile>classFLAC_1_1Metadata_1_1VorbisComment.html</anchorfile>
      <anchor>ac75ecd354178786318c5c3af1919d949</anchor>
      <arglist>(unsigned index, const Entry &amp;entry)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>insert_comment</name>
      <anchorfile>classFLAC_1_1Metadata_1_1VorbisComment.html</anchorfile>
      <anchor>ad2aabf071235c0f00e580828e9c93f58</anchor>
      <arglist>(unsigned index, const Entry &amp;entry)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>append_comment</name>
      <anchorfile>classFLAC_1_1Metadata_1_1VorbisComment.html</anchorfile>
      <anchor>a1126c7a0f25a2cf78efc8317d3a861f2</anchor>
      <arglist>(const Entry &amp;entry)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>replace_comment</name>
      <anchorfile>classFLAC_1_1Metadata_1_1VorbisComment.html</anchorfile>
      <anchor>a240eb83264d05d953395e75e18e15ee2</anchor>
      <arglist>(const Entry &amp;entry, bool all)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>delete_comment</name>
      <anchorfile>classFLAC_1_1Metadata_1_1VorbisComment.html</anchorfile>
      <anchor>af3c0467a369fff79f07af781c0d24c0c</anchor>
      <arglist>(unsigned index)</arglist>
    </member>
    <member kind="function">
      <type>int</type>
      <name>find_entry_from</name>
      <anchorfile>classFLAC_1_1Metadata_1_1VorbisComment.html</anchorfile>
      <anchor>a36095a90fa9f8c6c14282be9392fd271</anchor>
      <arglist>(unsigned offset, const char *field_name)</arglist>
    </member>
    <member kind="function">
      <type>int</type>
      <name>remove_entry_matching</name>
      <anchorfile>classFLAC_1_1Metadata_1_1VorbisComment.html</anchorfile>
      <anchor>af0770518f35fe18fb9a0cc5c0542c4b7</anchor>
      <arglist>(const char *field_name)</arglist>
    </member>
    <member kind="function">
      <type>int</type>
      <name>remove_entries_matching</name>
      <anchorfile>classFLAC_1_1Metadata_1_1VorbisComment.html</anchorfile>
      <anchor>adde2dc584e31f29d67fcc6d15d2d1034</anchor>
      <arglist>(const char *field_name)</arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>VorbisComment</name>
      <anchorfile>classFLAC_1_1Metadata_1_1VorbisComment.html</anchorfile>
      <anchor>a436a5c6a42a83a88206376805743fe3b</anchor>
      <arglist>(const VorbisComment &amp;object)</arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>VorbisComment</name>
      <anchorfile>classFLAC_1_1Metadata_1_1VorbisComment.html</anchorfile>
      <anchor>a2a788c0d96b5b8b22d089663b5e53b72</anchor>
      <arglist>(const ::FLAC__StreamMetadata &amp;object)</arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>VorbisComment</name>
      <anchorfile>classFLAC_1_1Metadata_1_1VorbisComment.html</anchorfile>
      <anchor>a9ca0e61561f14b1fff423b3334e14a62</anchor>
      <arglist>(const ::FLAC__StreamMetadata *object)</arglist>
    </member>
    <member kind="function">
      <type>VorbisComment &amp;</type>
      <name>operator=</name>
      <anchorfile>classFLAC_1_1Metadata_1_1VorbisComment.html</anchorfile>
      <anchor>a135650367ce6c2c5ce12b534307f1cca</anchor>
      <arglist>(const VorbisComment &amp;object)</arglist>
    </member>
    <member kind="function">
      <type>VorbisComment &amp;</type>
      <name>operator=</name>
      <anchorfile>classFLAC_1_1Metadata_1_1VorbisComment.html</anchorfile>
      <anchor>a6330469036affc1255e0d9528f93c191</anchor>
      <arglist>(const ::FLAC__StreamMetadata &amp;object)</arglist>
    </member>
    <member kind="function">
      <type>VorbisComment &amp;</type>
      <name>operator=</name>
      <anchorfile>classFLAC_1_1Metadata_1_1VorbisComment.html</anchorfile>
      <anchor>a4bed0b3d4a75c482dff89691be750546</anchor>
      <arglist>(const ::FLAC__StreamMetadata *object)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>operator==</name>
      <anchorfile>classFLAC_1_1Metadata_1_1VorbisComment.html</anchorfile>
      <anchor>a426e3fcf457530f18ca37e878e1f52db</anchor>
      <arglist>(const VorbisComment &amp;object) const </arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>operator==</name>
      <anchorfile>classFLAC_1_1Metadata_1_1VorbisComment.html</anchorfile>
      <anchor>a3df8d2b436a4bcacbfd8876ff421c391</anchor>
      <arglist>(const ::FLAC__StreamMetadata &amp;object) const </arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>operator==</name>
      <anchorfile>classFLAC_1_1Metadata_1_1VorbisComment.html</anchorfile>
      <anchor>af011746f1404c9550e6b355c12b366fa</anchor>
      <arglist>(const ::FLAC__StreamMetadata *object) const </arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>operator!=</name>
      <anchorfile>classFLAC_1_1Metadata_1_1VorbisComment.html</anchorfile>
      <anchor>a534ad61a7bf4580b786fc5ff7de1bdb7</anchor>
      <arglist>(const VorbisComment &amp;object) const </arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>operator!=</name>
      <anchorfile>classFLAC_1_1Metadata_1_1VorbisComment.html</anchorfile>
      <anchor>a511cc194f41b48922f01632e26ec40e5</anchor>
      <arglist>(const ::FLAC__StreamMetadata &amp;object) const </arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>operator!=</name>
      <anchorfile>classFLAC_1_1Metadata_1_1VorbisComment.html</anchorfile>
      <anchor>a2774fbb6849d04439f2f7ee84c0b9723</anchor>
      <arglist>(const ::FLAC__StreamMetadata *object) const </arglist>
    </member>
  </compound>
  <compound kind="class">
    <name>FLAC::Metadata::VorbisComment::Entry</name>
    <filename>classFLAC_1_1Metadata_1_1VorbisComment_1_1Entry.html</filename>
    <member kind="function" virtualness="virtual">
      <type>virtual bool</type>
      <name>is_valid</name>
      <anchorfile>classFLAC_1_1Metadata_1_1VorbisComment_1_1Entry.html</anchorfile>
      <anchor>affc33d6186da730409e62802e841a5f8</anchor>
      <arglist>() const </arglist>
    </member>
  </compound>
  <compound kind="dir">
    <name>include/FLAC</name>
    <path>/home/erikd/Git/flac/include/FLAC/</path>
    <filename>dir_1982b5890de532b4beef7221dae776e2.html</filename>
    <file>all.h</file>
    <file>assert.h</file>
    <file>callback.h</file>
    <file>export.h</file>
    <file>format.h</file>
    <file>metadata.h</file>
    <file>ordinals.h</file>
    <file>stream_decoder.h</file>
    <file>stream_encoder.h</file>
  </compound>
  <compound kind="dir">
    <name>include/FLAC++</name>
    <path>/home/erikd/Git/flac/include/FLAC++/</path>
    <filename>dir_527642952c2881b3e5b36abb4a29ebef.html</filename>
    <file>all.h</file>
    <file>decoder.h</file>
    <file>encoder.h</file>
    <file>export.h</file>
    <file>metadata.h</file>
  </compound>
  <compound kind="dir">
    <name>include</name>
    <path>/home/erikd/Git/flac/include/</path>
    <filename>dir_d44c64559bbebec7f509842c48db8b23.html</filename>
    <dir>include/FLAC</dir>
    <dir>include/FLAC++</dir>
  </compound>
  <compound kind="page">
    <name>index</name>
    <title></title>
    <filename>index</filename>
    <docanchor file="index" title="Introduction">intro</docanchor>
    <docanchor file="index" title="FLAC C API">c_api</docanchor>
    <docanchor file="index" title="FLAC C++ API">cpp_api</docanchor>
    <docanchor file="index" title="Getting Started">getting_started</docanchor>
    <docanchor file="index" title="Porting Guide">porting_guide</docanchor>
    <docanchor file="index" title="Embedded Developers">embedded_developers</docanchor>
  </compound>
</tagfile>
