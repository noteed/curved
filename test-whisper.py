# -*- coding: utf-8 -*-

"""
Check a few Whisper behavior.
"""

import os
import time
import unittest2

import whisper

FILENAME = 'test-whisper.whisper'
SECONDS_PER_POINT = 60
NUMBER_OF_POINTS = 5

class test_whiper(unittest2.TestCase):

    def test_00_create_empty_whisper(self):
        """
        Create a whisper file with one archive of 5 points where each point
        covers 60 second, and default xFilesFactor and aggregationMethod.
        """
        self.assertTrue(not os.path.exists(FILENAME))
        whisper.create(FILENAME, [(SECONDS_PER_POINT, NUMBER_OF_POINTS)])
        self.assertTrue(os.path.exists(FILENAME))

    def test_01_add_point(self):
        """
        Add a point and check the created time range.
        """
        now = int(time.time())
        whisper.update(FILENAME, 1234, timestamp=now)
        (fromInterval, toInterval, step), points = whisper.fetch(FILENAME, 0, None)
        now = now - (now % 60)
        # The upper bound is (roughly) 'now'.
        self.assertEqual(toInterval, now + SECONDS_PER_POINT)
        # The lower bound is (roughly) now minus the covered time.
        self.assertEqual(fromInterval, now - (NUMBER_OF_POINTS - 1) * SECONDS_PER_POINT)

    def test_zz(self):
        os.remove(FILENAME)

if __name__ == '__main__':
    unittest2.main()

# vim:expandtab:smartindent:tabstop=4:softtabstop=4:shiftwidth=4:
