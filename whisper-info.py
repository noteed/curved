#! /usr/bin/env python2

import sys
import whisper

if __name__ == '__main__':
  print sys.argv
  assert len(sys.argv) == 2

  filename = sys.argv[1]
  with open(filename, 'rb') as fh:
    info = whisper.__readHeader(fh)

    print "Metadata size: %s bytes." % whisper.metadataSize
    print info

    xs = whisper.file_fetch(fh, 0, None)
    print xs
