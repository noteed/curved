#! /usr/bin/env python2

import sys
import whisper

if __name__ == '__main__':
  print sys.argv
  assert len(sys.argv) == 2

  filename = sys.argv[1]
  info = whisper.__readHeader(open(filename, "rb"))

  print "Metadata size: %s bytes." % whisper.metadataSize
  print info
