frp-benchmarks
==========

A set of benchmarks to compare scalability of various FRP libraries.

Each task is written assuming a "classic FRP" semantics.  For arrowized or
other alternative semantics, attempt to capture the spirit of the task using
idiomatic code.

This project includes an individual executable for each tested framework, as
well as a monolithic simulation of all frameworks.  In the monolithic
simulation, frameworks may be omitted from certain tests if it's clear that
they are not competitive at that scale.

Test executables should be run with stderr redirected to /dev/null.

Performance evaluations
----------

1. __First-order__: generate 1,000 "Event String" nodes.  Create a network that prints the output of each node.  At each network step push a string (show stepNumber) into 10 randomly-selected nodes.  Measure the time required to run for 1,000 and 10,000 steps.

2. __Second-order__: generate 1000 "Event ()" nodes, then create 1000 "Behavior Int" nodes  that count the number of times each Event is fired.  Create an "Event (Behavior Int)" that every 10 network steps, sequentially moves to the next Behavior.  Create a "Behavior Int" from the "Event (Behavior Int)".  At each network step, fire 10 randomly-selected Event () nodes, then print the current value of the "Behavior Int". Measure the time required to run for 1,000 and 10,000 steps.

3. __Second-order__: generate 1000 "Event [Int]" nodes.  Create a type and function

        data Box = Box (Behavior [Event Int])
        fromBox :: Box -> Behavior (Event Int)
        -- notionally, this is
        -- fromBox (Box events) = mconcat <$> events

    Maintain a list of 10 "Event Int" nodes in a "Behavior Box", replacing a node with another randomly-selected node every 10 network steps.  Every network step, fire 500 randomly-chosen "Event Int" nodes.  Using the fromBox function, create an "Event Int" from the "Box" and print it every network step.  Measure the time required to run for 1,000 and 10,000 steps.

4. __Third-order performance__ (TODO):
     By third-order performance, I mean recursively-nested structures as in UIxTreeCollection.  If performance differences aren't clear based on prior tests, we can write a spec for this.

### Notes

* All computations should be performed strictly
* netwire doesn't have an "Event" type.  Events could be modeled as in Euphoria, however it would seem more idiomatic to create a function "fireEvent :: (a -> IO ()) -> Wire IO a" that would have the value of 'a' when fired, and be inhibited otherwise.
* we can adjust runtimes/stepcounts as performance warrants.

* for even comparisons, I suggest we use System.Random.MWC (from mwc-random) for random number generation, and IntMaps for collections.
