(*Make global constants dynamic parameters later on*)
eL = 100; (*environment length*)
eW = 100;
foodDensity = 1;
startingAgents = 10;
timestep = 0.1; (*seconds*)

testAgent1 = <|
    "pos" -> {0, 0}, (*{x, y}*)
    "energy" -> 100,
    "age" -> 15
|>;

testAgent2 = <|
    "pos" -> {2, 2}, (*{x, y}*)
    "energy" -> 60,
    "age" -> 20
|>;

agents = {testAgent1, testAgent2};