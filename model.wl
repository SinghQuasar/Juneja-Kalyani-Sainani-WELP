(* ::Package:: *)

scriptDir = NotebookDirectory[];
SetDirectory[scriptDir];

<< basicMethods.wl (*import basicMethods.wl*)

parameters:= <| (*agent energy is currently unbounded, but we can change that*)
  "proximityRadius" -> 2.0, (*proximity radius for food consumption action*)
  "metabolismPerAction" -> 0.2, (*energy lost due to metabolism*)
  "dt" -> 0.1, (*timestep in seconds (we can change this later)*)
  "foodEnergy" -> 5.0, (*how much enlergy 1 food particle gives*)
  "foodSpawnCooldown" -> 20,
  "nFoodSpawn" -> 5,
  "nStartingAgents" -> 5,
  "nStartingFood" -> 20,
  "squareBounds" -> {0, 10}, (*min, max. Square environment*)
  "startingEnergy" -> 10,
  "stepLength" -> 0.1,
  "lifespan" -> 20 (*20 units*)
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

propogateModel[params_, model_] := ( (*propogates by one timestep dt*)

  time = model["time"];

  If[Mod[time, params["foodSpawnCooldown"]] == 0, (*spawns food if time is appropriate.*)
    model["foods"] = spawnFoods[model["foods"], params]
  ];

  (*updating agents' actions*)
  model["agents"] = RandomSample[model["agents"]] (*shuffle to not give any agents an advantage (food order).*)
  model["agents"] = DeleteCases[model["agents"], (#["age"] >= params["lifespan"] || #["energy"] <= 0)&];   (*deaths*)

  For[i = 1, i <= Length[model["agents"]], i++, (*current agent removal process may be inefficient*)

    agenti = model["agents"][[i]];
    nearFoodI = findNearestFoodI[agenti["pos"], model["foods"]];

    If[nearFoodI >= 0, (*have the agent eat the food*)
      model["foods"] = Delete[model["foods"], nearFoodI]; (*list shifting operation*)
      model["agents"][[i]]["energy"] = agenti["energy"] + params["foodEnergy"];
      , model["agents"][[i]]["pos"] = randomActionWalk[agentI, params]; (*else, move*)
    ];

  ];
  
  model["time"] = model["time"] + params["dt"];
  model
)

runSim[params_, model_, duration_] := (
  model = propogateModel[params, model];
  (*visualize model...*)
)

main[] := (
  model = initializeModel[parameters];
  runSim[parameters, model, 200];
)