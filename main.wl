(* ::Package:: *)

SetDirectory[NotebookDirectory[]];
Needs["jksABM`"]

parameters = <|
  "proximityRadius" -> 0.5, (*proximity radius for food consumption action*)
  "sensingDistance" -> 2,
  "metabolism" -> 0.05, (*energy lost due to metabolism*)
  "dt" -> 0.1, (*timestep in seconds (we can change this later)*)
  "foodEnergy" -> 5.0, (*how much energy 1 food particle gives*)
  "foodSpawnCooldown" -> 1,
  "nFoodSpawn" -> 6,
  "nStartingAgents" -> 4,
  "nStartingFood" -> 7,
  "squareBounds" -> {0, 13}, (*min, max. Square environment*)
  "startingEnergy" -> 10,
  "stepLength" -> 0.2,
  "dirChangeProb" -> 0.025,
  "lifespan" -> 40, (*20 units*)
  "maxEnergy" -> 10,
  "imageSize" -> 500,
  "agentSize" -> 0.005, (*radius in pure length units*)
  "foodSize" -> 0.0025,
  "minReproductionEnergy" -> 6.0,
  "minReproductionAge" -> 2.0,
  "reproductionEnergyCost" -> 3.0
  |>;

model = initializeModel[parameters];
Simulation = genSimulationStates[parameters, model, 3000];
simulationGraphics = renderSimulation[Simulation, parameters];

Manipulate[simulationGraphics[[j]], {j, 1, Length@simulationGraphics, 1, Appearance->Labeled}]

plotPopulationStats[Simulation]




