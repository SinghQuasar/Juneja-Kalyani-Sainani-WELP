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
  {agents, foods, squareBounds, min, max, maxEnergy, agentGraphics, foodGraphics, infoText, legend, barW, barH, barOffset, textOffset},

  agents = model["agents"];
  foods = model["foods"];
  squareBounds = params["squareBounds"];
  min = squareBounds[[1]];
  max = squareBounds[[2]];
  maxEnergy = params["startingEnergy"];

  (* size of the bar *)
  barW = 0.7;
  barH = 0.12;
  barOffset = 0.22;
  textOffset = 0.42;

  energyColor[frac_] := Blend[{Red, Yellow, Green}, frac];
  
  agentGraphics = Table[
    Module[
      {pos, e, age, frac, percent, verticalDir, xShift, barCenter, barLeft, barBottom, textPos},
      pos = agent["pos"];
      e = agent["energy"];
      age = agent["age"];
      frac = Clip[e/maxEnergy, {0, 1}];
      percent = Round[100 frac];
      verticalDir = If[pos[[2]] > max - 0.8, -1, 1];
      
      xShift = Which[
        pos[[1]] < min + 1.2, 0.55,
        pos[[1]] > max - 1.2, -0.55,
        True, 0
      ];

      barCenter = pos + {xShift, verticalDir*barOffset};
      barLeft = barCenter[[1]] - barW/2;
      barBottom = barCenter[[2]] - barH/2;

      textPos = pos + {xShift, verticalDir*textOffset};

      {
        (* agent *)
        Blue, PointSize[0.02], Point[pos],

        (* coordinates and age *)
        White, Text[Style[Row[{"(",NumberForm[pos[[1]], {4, 2}], ", ", NumberForm[pos[[2]], {4, 2}],")  age: ",NumberForm[age, {3, 1}]}],10],textPos],

        (* energy bar *)
        EdgeForm[Directive[White, Thickness[0.0015]]], Darker[Gray, 0.7], Rectangle[{barLeft, barBottom}, {barLeft + barW, barBottom + barH}],

        (* energy fill *)
        energyColor[frac], Rectangle[{barLeft, barBottom}, {barLeft + barW*frac, barBottom + barH}],

        (* percent in the bar *)
        Magenta, Text[Style[ToString[percent], 8],barCenter]}
    ],{agent, agents}];

  foodGraphics = Table[{White, PointSize[0.015], Point[f], Gray, Text[Style[Row[{"(",NumberForm[f[[1]], {4, 2}], ", ",NumberForm[f[[2]], {4, 2}],")"}],8],f + {0, 0.22}]},{f, foods}];

  infoText = Text[Style[Column[{"Time: " <> ToString@NumberForm[model["time"], {4, 2}],"Food Count: " <> ToString@Length[foods]}],12,White],{max - 1.2, max - 0.8}];

  legend = {
    Blue, PointSize[0.02], Point[{min + 0.35, max - 0.45}],
    White, Text[Style["Agent", 10], {min + 1.1, max - 0.45}],

    EdgeForm[Directive[White, Thickness[0.0015]]],
    Darker[Gray, 0.7],
    Rectangle[{min + 0.1, max - 1.05}, {min + 0.8, max - 0.93}],
    Blend[{Red, Yellow, Green}, 0.75],
    Rectangle[{min + 0.1, max - 1.05}, {min + 0.625, max - 0.93}],
    White, Text[Style["Energy bar", 10], {min + 1.35, max - 0.99}],

    White, PointSize[0.015], Point[{min + 0.35, max - 1.55}],
    White, Text[Style["Food", 10], {min + 1.1, max - 1.55}]
  };

  Graphics[
    {agentGraphics, foodGraphics, infoText, legend},
    Background -> Black,
    PlotRange -> {{min, max}, {min, max}},
    PlotRangePadding -> Scaled[0.05],
    ImagePadding -> 25,
    ImageSize -> 500
  ]
] 
	 
genSimulationStates[params_, model_, timesteps_] := Module[{model2 = model}, Join[{model},Table[model2 = propagateModelStep[params, model2];model2,{s, timesteps}]]]

renderSimulation[states_List, params_] := displayGrid[#, params] & /@ states

plotPopulationStats[simulation_List] := Module[
  {times, agentCounts, foodCounts},
  times = #["time"] & /@ simulation;
  agentCounts = Length[#["agents"]] & /@ simulation;
  foodCounts = Length[#["foods"]] & /@ simulation;

  ListLinePlot[
    {agentCounts, foodCounts},
    DataRange  -> {First[times], Last[times]},
    PlotLegends -> {"Agents", "Food"},
    PlotStyle  -> {Blue, Green},
    AxesLabel  -> {"Time", "Count"},
    PlotLabel  -> "Population Over Time",
    GridLines  -> Automatic,
    ImageSize  -> 500
  ]
];

model = initializeModel[parameters];
Simulation = genSimulationStates[parameters, model, 100];
simulationGraphics = renderSimulation[Simulation, parameters];

Manipulate[simulationGraphics[[j]], {j, 1, Length@simulationGraphics, 1, Appearance->Labeled}]

plotPopulationStats[Simulation]

(*old version of displayGrid*)
(*displayGrid[model_] := Grid[{{"Food Length: "<>ToString@Length@model["foods"], "Time: "<>ToString@model["time"]}, {Dataset@model["agents"], Dataset@model["foods"]}}*)


(* ::Input:: *)
(*model = initializeModel[parameters];*)
(*Simulation = genSimulation[parameters, model, 100];*)
(**)
(*Manipulate[Simulation[[j]], {j, 1, Length@Simulation, 1, Appearance->Labeled}]*)
(*(*OR*)*)
(*(*ListAnimate[Simulation, AnimationRate -> 10]*)*)
(**)
(**)
