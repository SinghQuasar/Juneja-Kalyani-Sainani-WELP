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
  "nFoodSpawn" -> 1,
  "nStartingAgents" -> 5,
  "nStartingFood" -> 3,
  "squareBounds" -> {0, 10}, (*min, max. Square environment*)
  "startingEnergy" -> 10,
  "stepLength" -> 1,
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
  model2 = spawnFoodCheck[model2, params];
  model2 = randomizeAndKill[model2, params]; (*randomization disabled for clarity atm.*)
  model2 = agentActions[model2, params];
  model2 = metabolizeAgents[model2, params];
  model2 = ageAgents[model2, params];
  model2["time"] = model2["time"] + params["dt"];
  model2
 ];
 
displayGrid[model_, params_] := Module[
  {
    agents, foods, squareBounds, min, max, maxEnergy,
    agentGraphics, foodGraphics, infoText
  },

  agents = model["agents"];
  foods = model["foods"];
  squareBounds = params["squareBounds"];
  min = squareBounds[[1]];
  max = squareBounds[[2]];
  maxEnergy = params["startingEnergy"];

  (* Energy color *)
  energyColor[e_] := Which[
    e >= maxEnergy, Green,
    e >= 0.75 maxEnergy, Yellow,
    e >= 0.5 maxEnergy, Orange,
    True, Red
  ];

  (* Agents *)
  agentGraphics = Table[
    {energyColor[agent["energy"]], PointSize[0.02], Point[agent["pos"]], White, Text[Style[Row[{"(", NumberForm[agent["pos"][[1]], {4, 2}], ", ", NumberForm[agent["pos"][[2]], {4, 2}], ")", " , age: ", NumberForm[agent["age"], {3, 1}] }], 10], agent["pos"] + {0, 0.5}]},
    {agent, agents}
  ];

  (* Food *)
  foodGraphics = Table[
    {White, PointSize[0.015], Point[f], Gray, Text[ Style[ Row[{ "(", NumberForm[f[[1]], {4, 2}], ", ", NumberForm[f[[2]], {4, 2}], ")" }], 9], f + {0, 0.3} ] }, {f, foods}];

  (* Time and food count stuff *)
  infoText = Text[ Style[Column[{
        "Time: " <> ToString@NumberForm[model["time"], {4, 2}],
        "Food Count: " <> ToString@Length[foods]}], 12, White],
    {max - 1.2, max - 0.8}
  ];

  Graphics[
    {agentGraphics, foodGraphics, infoText},
    Background -> Black,
    PlotRange -> {{min, max}, {min, max}},
    ImageSize -> 500
  ]
]
	 
genSimulation[params_, model_, timesteps_] := Module[{model2 = model},
  Join[{displayGrid[model, params]},
    Table[
      model2 = propagateModelStep[params, model2];
      displayGrid[model2, params],
      {s, timesteps}
    ]
  ]
]

(*old version of displayGrid*)
(*displayGrid[model_] := Grid[{{"Food Length: "<>ToString@Length@model["foods"], "Time: "<>ToString@model["time"]}, {Dataset@model["agents"], Dataset@model["foods"]}}*)


(* ::Input:: *)
(*model = initializeModel[parameters];*)
(*Simulation = genSimulation[parameters, model, 100];*)
(**)
(*Manipulate[Simulation[[j]], {j, 1, Length@Simulation, 1, Appearance->Labeled}]*)
