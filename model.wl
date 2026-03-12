(* ::Package:: *)

scriptDir = NotebookDirectory[];
SetDirectory[scriptDir];

(*<< basicMethods.wl -- we're initializing them in the kernel right now, but working on changing to formal package*)

parameters = <| (*agent energy is currently unbounded, but we can change that*)
  "proximityRadius" -> 2.0, (*proximity radius for food consumption action*)
  "metabolism" -> 0.2, (*energy lost due to metabolism*)
  "dt" -> 0.1, (*timestep in seconds (we can change this later)*)
  "foodEnergy" -> 5.0, (*how much energy 1 food particle gives*)
  "foodSpawnCooldown" -> 1,
  "nFoodSpawn" -> 5,
  "nStartingAgents" -> 5,
  "nStartingFood" -> 20,
  "squareBounds" -> {0, 10}, (*min, max. Square environment*)
  "startingEnergy" -> 10,
  "stepLength" -> 0.1,
  "lifespan" -> 20 (*20 units*)
|>;

initializeModel[params_] :=

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
      "agents" -> agents, (*list of agent associations*)
      "foods" -> foods, (*list of food coordinates*)
      "bounds" -> {min, max},
      "time" -> 0
    |>
 ]

propagateModelStep[params_, model_] := Module[{model2=model}, (*working on changing to DynamicModule/Manipulate. This propogateModel method is not confirmed to be working, but the indiivdual helper methods have been tested.*)
  (*model2["foods"]   = spawnFoodCheck[model2, params];*)
  model2            = randomizeAndKill[model2, params];
  model2            = agentActions[model2, params];
  model2            = metabolizeAgents[model2, params];
  model2            = ageAgents[model2, params];
  model2["time"]    = model2["time"] + params["dt"];
  model2
 ];

 
 propagateModel[params_, model_, simulationDuration_Integer] :=
  Fold[propagateModelStep[params, #1]&, model, Range[simulationDuration]];
 
DynamicModule[{model = initializeModel[parameters]},
  model = propagateModel[parameters, model, 100];
  model
]


test1 = Block[
{model = initializeModel[parameters]}, Table[
  model = propagateModel[parameters, model, t];
  {{model["bounds"], model["time"], Length@model["foods"], Length@model["agents"]},{Dataset@model["agents"], Dataset@model["foods"], SpanFromLeft,  SpanFromLeft}},
{t, 0, 20, 1}
  ]
  
];


Manipulate[Grid@@test1[[i]], {i, 1, Length@test1}]


Block[
{model = initializeModel[parameters]}, Manipulate[
  model = propagateModel[parameters, model, t];
  Grid[{{model["bounds"], model["time"], Length@model["foods"], Length@model["agents"]},{Dataset@model["agents"], Dataset@model["foods"], SpanFromLeft}}],
{t, 0, 30, 1}
  ]
  
]


(* ::Input:: *)
(*Manipulate[Module[{start=<|"A"->1,"B"->2|>},start["A"]+=x;*)
(*Dataset[start]],{x,0,1, 0.2}]*)
