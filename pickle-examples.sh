#! /bin/sh

./pickle-dump.py "{}" --output example.pickle
runghc bin/pickle-read.hs example.pickle

./pickle-dump.py "'cache-query'" --output example.pickle
runghc bin/pickle-read.hs example.pickle

./pickle-dump.py "{'type': 'cache-query', 'metric': 'some.metric.name'}" --output example.pickle
runghc bin/pickle-read.hs example.pickle

./pickle-dump.py "1" --output example.pickle
runghc bin/pickle-read.hs example.pickle

./pickle-dump.py "1.1" --output example.pickle
runghc bin/pickle-read.hs example.pickle

./pickle-dump.py "(1,)" --output example.pickle
runghc bin/pickle-read.hs example.pickle

./pickle-dump.py "(1, 2)" --output example.pickle
runghc bin/pickle-read.hs example.pickle

./pickle-dump.py "[1, 2]" --output example.pickle
runghc bin/pickle-read.hs example.pickle

./pickle-dump.py "[(1, 2)]" --output example.pickle
runghc bin/pickle-read.hs example.pickle

./pickle-dump.py "{'datapoints': [(1, 2)]}" --output example.pickle
runghc bin/pickle-read.hs example.pickle

./pickle-dump.py "{'datapoints': [(1, 2)], (2,): 'foo'}" --output example.pickle
runghc bin/pickle-read.hs example.pickle

rm -f example.pickle
