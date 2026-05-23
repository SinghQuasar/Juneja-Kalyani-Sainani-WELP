(* ::Package:: *)

SetDirectory[NotebookDirectory[]];
Needs["jksABM`"]

parameters = <|
  "proximityRadius" -> 0.5, (*proximity radius for food consumption action*)
  "sensingDistance" -> 2,
  "metabolism" -> 0.05, (*energy lost due to metabolism*)
  "dt" -> 0.1, (*timestep in seconds (we can change this later)*)
  "foodEnergy" -> 5.0, (*how much energy 1 food particle gives*)
  "foodSpawnCooldown" -> 0.5,
  "nFoodSpawn" -> 3,
  "nStartingAgents" -> 5,
  "nStartingFood" -> 3,
  "squareBounds" -> {0, 13}, (*min, max. Square environment*)
  "startingEnergy" -> 10,
  "stepLength" -> 0.2,
  "dirChangeProb" -> 0.025,
  "lifespan" -> 40, (*20 units*)
  "maxEnergy" -> 10,
  "imageSize" -> 500,
  "agentSize" -> 0.005, (*radius in pure length units*)
  "foodSize" -> 0.0025,
  "reproductionRadius" -> 1.5, (*we should cut this out in favor of proximityRadius, since proxRad is supposed to encompass this as well.*)
  "minReproductionEnergy" -> 6.0,
  "minReproductionAge" -> 2.0,
  "reproductionEnergyCost" -> 3.0,
  "nextAgentID" -> 1, (*if this is a holder for the next agent id, and it evolves as the model does, it should go in the model data struct. If it's a parameter for number ID to start at, then it should stay here.*)
  "epsilon" -> 0.001
|>;

model = initializeModel[parameters];
Simulation = genSimulationStates[parameters, model, 500];
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
