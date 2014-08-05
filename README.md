a-star-scheme
=============

Naive A* implementation in Scheme

How to use
----------

Provide estimate function that knows about all graph nodes:

(a-star-search test-graph (spatial-weight-for test-locations) 0 4)

If we have no estimate function we can use it as Dijkstra's algorithm:

(a-star-search test-graph zero-weight 0 4)
