scriptDir = NotebookDirectory[];
SetDirectory[scriptDir];

<< basicMethods.wl (*import basicMethods.wl*)

parameters:= <|
  "proximityRadius" -> 2.0, (*proximity radius for food consumption action*)
  "metabolismPerAction" -> 0.2, (*energy lost due to metabolism*)
  "dt" -> 0.1, (*timestep (we can change this later)*)
  "foodEnergy" -> 5.0, (*how much energy 1 food particle gives*)
  "foodSpawnCooldown" -> 20,
  "nFoodSpawn" -> 5,
  "nStartingAgents" -> 5,
  "nStartingFood" -> 20,
  "squareBounds" -> {0, 10}, (*min, max. Square environment*)
  "startingEnergy" -> 10,
  "stepLength" -> 10.0/100
|>;

initializeModel[params_] :=

  Module[{agents, food, nStartingAgents, nStartingFood, startingEnergy, min, max},  

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
      "agents" -> agents, (*list of agent associations*)
      "foods" -> foods, (*list of food coordinates*)
      "bounds" -> {min, max},
      "time" -> 0
    |>
 ]

model = initializeModel[parameters];

propogateModel[params_, model_] := ( (*propogates by one timestep dt*)

  time = model["time"];

  If[Mod[time, params["foodSpawnCooldown"]] == 0, 
    model["foods"] = spawnFoods[model["foods"], params]
  ]; (*spawns food if time is appropriate.*)

  (*updating agents' actions*)
  
  model["time"] = model["time"] + params["dt"]
  model
)