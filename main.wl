(* ::Package:: *)

SetDirectory[NotebookDirectory[]];
Needs["jksABM`"]

parameters = <|
  "proximityRadius" -> 0.5, (*proximity radius for food consumption action*)
  "sensingDistance" -> 3,
  "metabolism" -> 0.025, (*energy lost due to metabolism*)
  "dt" -> 0.1, (*timestep in seconds (we can change this later)*)
  "foodEnergy" -> 1.5, (*how much energy 1 food particle gives*)
  "foodSpawnCooldown" -> 1,
  "nFoodSpawn" -> 2,
  "nStartingAgents" -> 5,
  "nStartingFood" -> 2,
  "squareBounds" -> {0, 17}, (*min, max. Square environment*)
  "startingEnergy" -> 10,
  "stepLength" -> 0.2,
  "dirChangeProb" -> 0.015,
  "lifespan" -> 40, (*20 units*)
  "maxEnergy" -> 10,
  "imageSize" -> 500,
  "agentSize" -> 0.01, (*radius in pure length units*)
  "foodSize" -> 0.005,
  "minReproductionEnergy" -> 5.0,
  "minReproductionAge" -> 3.0,
  "reproductionEnergyCost" -> 3.0,
|>;

model = initializeModel[parameters];
Simulation = genSimulationStates[parameters, model, 1000];
simulationGraphics = renderSimulation[Simulation, parameters];

Manipulate[simulationGraphics[[j]], {j, 1, Length@simulationGraphics, 1, Appearance->Labeled}]

plotPopulationStats[Simulation]





