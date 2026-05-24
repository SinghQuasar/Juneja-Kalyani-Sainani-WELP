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

(*model = initializeModel[parameters];*)
(*Simulation = genSimulationStates[parameters, model, 1000];*)
(*Export[NotebookDirectory[] <> "carryingCSims\\sim1.wdx", Simulation];*)
Simulation = Import[NotebookDirectory[] <> "carryingCSims\\sim1.wdx"];


plotPopulationStats[Simulation]


simVisualize[Simulation, parameters]


showLogistic[Simulation]
logisticStats[Simulation]
