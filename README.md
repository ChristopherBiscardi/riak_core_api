# Objective
Build an API using Webmachine and Riak Core with Riak KV as a dependency.
Ideally this would create a masterless ring to respond to API requests.
The API could present a specialized REST interface to a Riak KV store.

# Build
make rel

# Run
./rel/bars/bin/bars console
