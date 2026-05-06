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



