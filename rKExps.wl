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
  "minReproductionEnergy" -> 3.0,
  "minReproductionAge" -> 4.0,
  "reproductionEnergyCost" -> 2.0,
  "reproductionCooldown" -> 2.0,
  "epsilon" -> 0.001,
  "rnd" -> N@10^(-3)
|>;

TIMESTEPS = 2000;


(*Metabolism - r/K*)

metabolismIV = Table[i, {i, 0.01, 0.1, 0.005}]
Simulations = Table[
	parameters["metabolism"] = metabolism;
	genSimulation[parameters, TIMESTEPS],
{metabolism, metabolismIV}];
ExportSim[Simulations, "metabolismIV", NotebookDirectory[] <> "simFiles\\" <> "rKSims\\"]
Clear[Simulations]


(*Food Spawned/Turn - r/K*)
parameters["metabolism"] = 0.05;

nFoodSpawnIV = Table[i, {i, 10}]
Simulations = Table[
	parameters["nFoodSpawn"] = nFoodSpawn;
	genSimulation[parameters, TIMESTEPS],
{nFoodSpawn, nFoodSpawnIV}];
ExportSim[Simulations, "nFoodSpawnIV", NotebookDirectory[] <> "simFiles\\" <> "rKSims\\"]
Clear[Simulations]


(*Food Energy - r/K*)
parameters["nFoodSpawn"] = 5;

foodEnergyIV = Table[i, {i, 1.0, 7.5, 0.25}]
Simulations = Table[
	parameters["foodEnergy"] = foodEnergy;
	genSimulation[parameters, TIMESTEPS],
{foodEnergy, foodEnergyIV}];
ExportSim[Simulations, "foodEnergyIV", NotebookDirectory[] <> "simFiles\\" <> "rKSims\\"]
Clear[Simulations]


(*Reproduction Cooldown - r/K*)
parameters["foodEnergy"] = 5.0;

reproductionCooldownIV = Table[i, {i, 1.0, 3.0, 0.25}]
Simulations = Table[
	parameters["reproductionCooldown"] = reproductionCooldown;
	genSimulation[parameters, TIMESTEPS],
{reproductionCooldown, reproductionCooldownIV}];
ExportSim[Simulations, "reproductionCooldownIV", NotebookDirectory[] <> "simFiles\\" <> "rKSims\\"]
Clear[Simulations]

(*Reproduction Energy Cost - r/K*)
(*Min Energy = Energy Cost + 1*)

parameters["reproductionCooldown"] = 2.0;

reproductionEnergyCostIV = Table[i, {i, 1.0, 4.0, 0.5}]
Simulations = Table[
	parameters["reproductionEnergyCost"] = reproductionEnergyCost;
	parameters["minReproductionEnergy"] = reproductionEnergyCost + 1;
	genSimulation[parameters, TIMESTEPS],
{reproductionEnergyCost, reproductionEnergyCostIV}];
ExportSim[Simulations, "reproductionEnergyCostIV", NotebookDirectory[] <> "simFiles\\" <> "rKSims\\"]
Clear[Simulations]


metabolismIV = Table[i, {i, 0.01, 0.1, 0.005}];
nFoodSpawnIV = Table[i, {i, 10}];
foodEnergyIV = Table[i, {i, 1.0, 7.5, 0.25}];
reproductionCooldownIV = Table[i, {i, 1.0, 3.0, 0.25}];
reproductionEnergyCostIV = Table[i, {i, 1.0, 4.0, 0.5}];

makeRKPipeline[name_String, label_String, impDir_String, expDir_String] := Module[
  {
    iv, sims,
    kSeries, rSeries
  },

  iv = Symbol[name <> "IV"];
  sims = Symbol[name <> "Simulations"];

  sims = ImportSim[name <> "IV", impDir];

  kSeries = Transpose[{iv, logisticStats[#][K] & /@ sims}];
  rSeries = Transpose[{iv, logisticStats[#][r] & /@ sims}];

  ExportSim[kSeries, name <> "KSeries", expDir];
  ExportSim[rSeries, name <> "rSeries", expDir];

  Clear[sims];

  <|"KSeries" -> kSeries, "rSeries" -> rSeries|>
]


impDir = "G:\\My Drive\\Academic ECs\\WELP\\Juneja, Kalyani, Sainani WELP\\Simulations\\"
expDir = "C:\\Users\\Shrer\\Desktop\\Local\\Local Work\\Juneja-Kalyani-Sainani-WELP\\rKExpGraphs\\"
makeRKPipeline["metabolism", "Metabolism (E/T)", impDir, expDir]
makeRKPipeline["nFoodSpawn", "Food Spawned/Timestep (1/T)", impDir, expDir]
makeRKPipeline["foodEnergy", "Food Energy (E)", impDir, expDir]
makeRKPipeline["reproductionCooldown", "Reproduction Cooldown (T)", impDir, expDir]
makeRKPipeline["reproductionEnergyCost", "Reproduction Energy Cost (E)", impDir, expDir]


graphRKPipeline[name_String, label_String, impDir_String] := Module[{kSeries, rSeries},
  ImportSim[name <> "rSeries", impDir];
		
  kSeries = ImportSim[name <> "KSeries", impDir];
  rSeries = ImportSim[name <> "rSeries", impDir];

  Row[{rKIVPlot[kSeries, "Carrying Capacity", label <> " vs Carrying Capacity"], 
  rKIVPlot[rSeries, "Growth Rate r", label <> " vs Growth Rate"]}]
]


impDir = "C:\\Users\\Shrer\\Desktop\\Local\\Local Work\\Juneja-Kalyani-Sainani-WELP\\rKExpGraphs\\"
graphRKPipeline["metabolism", "Metabolism (E/T)", impDir]
graphRKPipeline["nFoodSpawn", "Food Spawned/Timestep (1/T)", impDir]
graphRKPipeline["foodEnergy", "Food Energy (E)", impDir]
graphRKPipeline["reproductionCooldown", "Reproduction Cooldown (T)", impDir]
graphRKPipeline["reproductionEnergyCost", "Reproduction Energy Cost (E)", impDir]



