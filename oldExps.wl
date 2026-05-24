(* ::Package:: *)

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
