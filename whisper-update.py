#! /usr/bin/env python2
import argparse
import whisper

def run(args):
  whisper.update(args.filename, args.value, timestamp=None)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(
        description="Add a point to a Whisper file."
    )
    parser.add_argument('filename', metavar='PATH',
        help="Path to the Whisper file.")
    parser.add_argument('value', metavar='FLOAT',
        help="The float value to add.")
    parser.add_argument('--now',
        help="Override time.time() with the given EPOCH-based time.")
    parser.set_defaults(run=run)

    args = parser.parse_args()
    args.run(args)
