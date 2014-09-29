#!/usr/bin/perl

%vars = 
  (
   "__pFrameList"            => "__frames",
   "__pBinaryList"           => "__binaries",
   "__pFindCursor"           => "__cursor",
   "__bSyncOn"               => "__is_unsync",
   "__bCompression"          => "__is_compressed",
   "__bPadding"              => "__is_padded",
   "__bExtendedHeader"       => "__is_extended",
   "__bHasChanged"           => "__changed",
   "__bFileWritable"         => "__is_file_writable",
   "__fFileHandle"           => "__file_handle",
   "__ulFileSize"            => "__file_size",
   "__ulOldTagSize"          => "__orig_tag_size",
   "__ulExtraBytes"          => "__extra_bytes",
   "__bHasV1Tag"             => "__has_v1_tag",
   "__ulTagsToParse"         => "__tags_to_parse",
   "__sFileName"             => "__file_name",
   "s_ulInstances"           => "__instances",
   "__sEncryptionID"         => "__encryption_id",
   "__sGroupingID"           => "__grouping_id",
   "__bHasChanged"           => "__changed",
   "__auiFieldBits"          => "__field_bitset",
   "__ulNumFields"           => "__num_fields",
   "__apFields"              => "__fields",
   "__FrmHdr"                => "__hdr",
   "__eName"                 => "__id",
   "__eType"                 => "__type",
   "__ulFixedLength"         => "__length",
   "__eSpecBegin"            => "__spec_begin",
   "__eSpecEnd"              => "__spec_end",
   "__ulFlags"               => "__flags",
   "__bHasChanged"           => "__changed",
   "__sData"                 => "__data",
   "__ulSize"                => "__size",
   "__eError"                => "__error",
   "__nErrLine"              => "__line_num",
   "__sErrFileName"          => "__file_name",
   "__sErrDesc"              => "__description",
  );

#open(VARIABLES, "variables.txt") or die "Can't open variables.txt: $!\n";
#while ($line = <VARIABLES>)
#  {
#    ($oldname, $newname) = split(" ", $line);
#    $vars{$oldname} = $newname;
#  }

while (<>)
  {
    foreach $oldvar (sort keys %vars)
      {
        s/$oldvar/$vars{$oldvar}/g;
      }
    print;
  }
