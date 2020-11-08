Storage Option american monte carlo valuation
---------------------------------------------

Models a gas storage unit, which permits unit injections/withdrawals over a given number of days.

Calcluates both the value, and also the deltas for each day (via pathwise differentiation)


Run minimal example
--------------------

First install the scala build tool 

  > curl -Ls https://git.io/sbt > ~/bin/sbt && chmod 0755 ~/bin/sbt

From the top level directory run

  > sbt "runMain example.TimeStorageValuation 100 500 12"

This will run a valuation (and delta calculation) for a 100 day option 12 times, with 500 monte carlo paths each time. The time taken 
should also be reported


Development
-----------

Probably easiest to simply install the community edition of Intellij, https://www.jetbrains.com/idea/. Add the scala plugin (File -> Settings... -> Plugins), 
then open the project (File -> Open...)







