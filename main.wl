(* ::Package:: *)

SetDirectory[NotebookDirectory[]];
Needs["jksABM`"]

parameters = <|
  "proximityRadius" -> 0.5, (*proximity radius for food consumption action*)
  "sensingDistance" -> 4,
  "metabolism" -> 0.05, (*energy lost due to metabolism*)
  "dt" -> 0.1, (*timestep in seconds (we can change this later)*)
  "foodEnergy" -> 5.0, (*how much energy 1 food particle gives*)
  "foodSpawnCooldown" -> 1,
  "nFoodSpawn" -> 5,
  "nStartingAgents" -> 4,
  "nStartingFood" -> 4,
  "squareBounds" -> {0, 20}, (*min, max. Square environment*)
  "startingEnergy" -> 10,
  "stepLength" -> 0.4,
  "dirChangeProb" -> 0.025,
  "lifespan" -> 30,
  "maxEnergy" -> 10,
  "imageSize" -> 500,
  "agentSize" -> 0.005, (*radius in pure length units*)
  "foodSize" -> 0.0025,
  "reproductionRadius" -> 1.5, (*we should cut this out in favor of proximityRadius, since proxRad is supposed to encompass this as well.*)
  "minReproductionEnergy" -> 5.0,
  "minReproductionAge" -> 4.0,
  "reproductionEnergyCost" -> 2.0,
  "reproductionCooldown" -> 2,
  "epsilon" -> 0.001,
  "rnd" -> N@10^(-3)
|>;

model = initializeModel[parameters];
(*Simulation = genSimulationStates[parameters, model, 1000];*)
(*Export[NotebookDirectory[] <> "carryingCSims\\sim1.wdx", Simulation];*)
Simulation = Import[NotebookDirectory[] <> "carryingCSims\\sim1.wdx"];
simulationGraphics = renderSimulation[Simulation, parameters];
Manipulate[simulationGraphics[[j]], {j, 1, Length@simulationGraphics, 1, Appearance->Labeled}]
plotPopulationStats[Simulation]


logisticGrowth[simulation_, parameters_] := Module[
  {
   agentCounts, times, agentData,
   logisticModel, populationFit, carryingCapacity,
   basePopulationPlot
   },
  
  agentCounts = Length[#["agents"]] & /@ simulation;
  times = Range[0, Length[agentCounts] - 1] * parameters["dt"];
  agentData = Transpose[{times, agentCounts}];
  
  basePopulationPlot = plotPopulationStats[simulation];
  
  Clear[t, K, r, t0, y0];
  
  logisticModel[t_] := y0 + (K - y0)/(1 + Exp[-r (t - t0)]);
  
  populationFit = NonlinearModelFit[
    agentData,
    {
     logisticModel[t],
     K > Max[agentCounts],
     r > 0,
     y0 >= 0,
     y0 <= First[agentCounts] + 5,
     Min[times] <= t0 <= Max[times]
     },
    {
     {K, Max[agentCounts]},
     {r, 0.25},
     {t0, Mean[times]},
     {y0, First[agentCounts]}
     },
    t,
    Method -> "NMinimize"
    ];
  
  carryingCapacity = K /. populationFit["BestFitParameters"];
  
  Column[
   { 
    Row[{"Carrying Capacity: ", 
      NumberForm[carryingCapacity, {6, 2}]}],
    Show[
     basePopulationPlot,
     Plot[
      populationFit[t],
      {t, Min[times], Max[times]},
      PlotStyle -> {Red, Thick, Dashed}
      ]
     ]
    }
   ]
  ]
  
  logisticGrowth[Simulation, parameters]
showCarryCLogistic[Simulation]


Table[
	parameters["nFoodSpawn"] = nFS;
	model = initializeModel[parameters];
	Simulation = genSimulationStates[parameters, model, 500];
	simulationGraphics = renderSimulation[Simulation, parameters];
	
	{nFS, Manipulate[simulationGraphics[[j]], {j, 1, Length@simulationGraphics, 1, Appearance->Labeled}],
	plotPopulationStats[Simulation]},
	{nFS, 1, 4}
]


Table[
	parameters["metabolism"] = met;
	model = initializeModel[parameters];
	Simulation = genSimulationStates[parameters, model, 500];
	simulationGraphics = renderSimulation[Simulation, parameters];
	
	{met, Manipulate[simulationGraphics[[j]], {j, 1, Length@simulationGraphics, 1, Appearance->Labeled}],
	plotPopulationStats[Simulation]},
	{met, {0.025, 0.05, 0.1}}
]
