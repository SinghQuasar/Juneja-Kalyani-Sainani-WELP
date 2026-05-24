(* ::Package:: *)

SetDirectory[NotebookDirectory[]];
Needs["jksABM`"]

parameters = <|
  "proximityRadius" -> 0.5, (*proximity radius for food consumption action*)
  "sensingDistance" -> 4,
  "metabolism" -> 0.05, (*energy lost due to metabolism*)
  "dt" -> 0.1, (*timestep in seconds*)
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
  "agentSize" -> 0.01, (*radius in pure length units*)
  "foodSize" -> 0.0075,
  "reproductionRadius" -> 1.0,
  "minReproductionEnergy" -> 5.0,
  "minReproductionAge" -> 4.0,
  "reproductionEnergyCost" -> 2.0,
  "reproductionCooldown" -> 2,
  "epsilon" -> 0.001,
  "rnd" -> N@10^(-3)
|>;

(*Simulation = genSimulation[parameters, 100];
ExportSim[Simulation, "sim2"];
Clear[Simulation]*) (*clear the variable after saving to not waste RAM and instead put in file for later access.*)

Simulation = ImportSim["sim1"];


plotPopulationStats[Simulation]


simVisualize[Simulation, parameters, 1, 100]


showLogistic[Simulation]
logisticStats[Simulation]
