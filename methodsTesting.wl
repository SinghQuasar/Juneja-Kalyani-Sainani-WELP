parameters = <| (*agent energy is currently unbounded, but we can change that*)
  "proximityRadius"  1, (*proximity radius for food consumption action*)
  "metabolism"  1, (*energy lost due to metabolism*)
  "dt"  0.1, (*timestep in seconds (we can change this later)*)
  "foodEnergy"  5.0, (*how much energy 1 food particle gives*)
  "foodSpawnCooldown"  20,
  "nFoodSpawn"  5,
  "nStartingAgents"  5,
  "nStartingFood"  20,
  "squareBounds"  {0, 10}, (*min, max. Square environment*)
  "startingEnergy"  10,
  "stepLength"  0.5,
  "lifespan"  20 (*20 units*)
|>;

(*helper function testing*)
initializeTestModel[params_] :=

  Module[{agents, foods, nStartingAgents, nStartingFood, startingEnergy, min, max},  

    nStartingAgents = params["nStartingAgents"]; (*parameter for number of agents*)
    nStartingFood = params["nStartingFood"]; (*parameter for number of food items*)
    startingEnergy = params["startingEnergy"];
    {min, max} = params["squareBounds"];

    agents =
    Table[
      createAgent[RandomReal[{min, max}, 2], startingEnergy, 0.0], nStartingAgents
    ];

    foods = RandomReal[{min, max}, {nStartingFood, 2}];
	
    <| 
      "agents"  agents, (*list of agent associations*)
      "foods"  foods, (*list of food coordinates*)
      "bounds"  {min, max},
      "time"  0
    |>
 ]
 
(*FUNCTION TESTING*)
 
model = initializeTestModel[parameters];
model["agents"]
model["foods"]
model["bounds"]
model["time"]

findNearestSensableFoodI[#, model["foods"], parameters]& /@ model["agents"]

model["agents"] = updateAgentInformation[model["agents"], 1, "energy", -1];
model["agents"] = updateAgentInformation[model["agents"], 2, "age", 30];
model["agents"]
model = randomizeAndKill[model, parameters];
model["agents"]

model = metabolizeAgents[model, parameters];
model["agents"]

model = ageAgents[model, parameters];
model["agents"]

model["agents"][[1]]
randomActionWalk[model["agents"][[1]], model["foods"], parameters][[1]]
model["agents"] = updateAgentInformation[model["agents"], 1, "pos", model["foods"][[1]] + {0.01, 0.01}]
findNearestSensableFoodI[model["agents"][[1]], model["foods"], parameters]
eat[model["agents"][[1]], model["foods"], findNearestSensableFoodI[model["agents"][[1]], model["foods"], parameters], parameters]

Print["Testing Main Agent Decision and Action Taking"]
model["agents"]
Length[model["foods"]]

findNearestSensableFoodI[#, model["foods"], parameters]& /@ model["agents"]
model = agentActions[model, parameters]

Length[model["foods"]]
