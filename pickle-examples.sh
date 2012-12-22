#! /bin/sh

./pickle-dump.py "{}" --output example.pickle
runghc Data/Pickle.hs example.pickle

./pickle-dump.py "{'type': 'cache-query', 'metric': 'some.metric.name'}" --output example.pickle
runghc Data/Pickle.hs example.pickle

rm -f example.pickle
