#! /usr/bin/env python2
import argparse
import whisper

def run(args):
  with open(args.filename, 'rb') as fh:
    info = whisper.__readHeader(fh)

    print '%s: period=%s, aggregation=%s, propagation=%s, archives=%s' % \
      (args.filename, info['maxRetention'], info['aggregationMethod'],
      info['xFilesFactor'], len(info['archives']))

    for i, archive in enumerate(info['archives']):
      print '  period=%s, points=%s, seconds=%s, size=%s, offset=%s' % \
        (archive['retention'], archive['points'], archive['secondsPerPoint'],
        archive['size'], archive['offset'])

    if not args.header:
      # Request all points until now. toInterval will be equal to now + step.
      (fromInterval, toInterval, step), points = whisper.file_fetch(fh, 0, None)
      print 'from=%s, to=%s, step=%s' % (fromInterval, toInterval, step)
      if any(points):
        for point in points:
          print point


if __name__ == '__main__':
    parser = argparse.ArgumentParser(
        description="Display a Whisper file's data."
    )
    parser.add_argument('filename', metavar='PATH',
        help="Path to the Whisper file.")
    parser.add_argument('--header', action='store_true',
        help="Only display the header's data.")
    parser.set_defaults(run=run)

    args = parser.parse_args()
    args.run(args)
