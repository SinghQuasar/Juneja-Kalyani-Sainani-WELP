(* ::Package:: *)

scriptDir = NotebookDirectory[];
SetDirectory[scriptDir];

checkWithinRadius[p1_, p2_, r_] := EuclideanDistance[p1, p2] <= r; (*agent's radius*)

(*checking the proximity for 1 food particle*)
checkFoodWithinRadius[agentPos_, foodPos_, proxRadius_] :=
  checkWithinRadius[agentPos, foodPos, proxRadius];

(*checking the proximity for all foods and putting it into a list*)
findNearestSensableFoodI[agent_, foodPositions_List, params_] := Module[{agentPos, foodPosAndI, foodsWithinRadiusPosAndI, dists, closest},
  agentPos = agent["pos"];
  foodPosAndI = Table[{foodPositions[[i]], i}, {i, Length[foodPositions]}];
  foodsWithinRadiusPosAndI = Select[foodPosAndI, checkFoodWithinRadius[agentPos, #[[1]], params["proximityRadius"]]&];
  If[Length[foodsWithinRadiusPosAndI] == 0,
    -1, (*no food*)
    dists = {EuclideanDistance[agentPos, #[[1]]], #[[2]]}& /@ foodsWithinRadiusPosAndI;
    closest = First@MinimalBy[dists, First];
    closest[[2]]
  ]
];

createAgent[pos_, energy_, age_] :=
  <|
    "pos" -> pos,
    "energy" -> energy,
    "age" -> age
  |>
  
updateAgentInformation[agentList_List, agentI_Integer, agentAttribute_String, newValue_] := Module[{agents = agentList},
	agents[[agentI]] = ReplacePart[agents[[agentI]], agentAttribute -> newValue];
	agents
]

spawnFoods[foods_, params_] := Join[foods, RandomReal[params["squareBounds"], {params["nFoodSpawn"], 2}]]

spawnFoodCheck[model_, params_] := Module[{model2 = model},
	model2["foods"] = If[Mod[model["time"], params["foodSpawnCooldown"]] < 10^(-1), spawnFoods[model["foods"], params], model["foods"]];
	model2]
	
randomizeAndKill[model_, params_] := Module[{agents = model["agents"], model2=model},	
	(*agents = RandomSample[agents];*) (*disabled right now for clarity*)
	model2["agents"] = Select[agents, (#["age"] < params["lifespan"] && #["energy"] > 0)&];
	model2
]

metabolizeAgents[model_, params_] := Module[{agents = model["agents"], model2=model},
	agents = MapAt[(# - params["metabolism"])&, agents, {All, "energy"}];
	model2["agents"] = agents;
	model2
]

ageAgents[model_, params_] := Module[{agents = model["agents"], model2=model},
	agents = MapAt[(# + params["dt"])&, agents, {All, "age"}];
	model2["agents"] = agents;
	model2
]

randomActionWalk[agent_, foods_, params_] := Module[{theta = RandomReal[{0, 2 Pi}], agent2 = agent, newPos, min, max},
  {min, max} = params["squareBounds"];
  newPos = agent2["pos"] + {params["stepLength"] * Cos[theta], params["stepLength"] * Sin[theta]}; (*+ operator threadwise*)
  agent2["pos"] = Clip[newPos,  {min, max}];
  {agent2, foods}
];

eat[agent_, foods_, foodI_, params_] := Module[{agent2 = agent, foods2 = foods},
	agent2["energy"] = agent2["energy"] + params["foodEnergy"];
	foods2 = Delete[foods2, foodI];
	{agent2, foods2}
]

agentActions[model_, params_] := Module[{model2 = model, agents = model["agents"], agentI = 1, foods = model["foods"], agent, nearFoodI},
	For[agentI = 1, agentI <= Length[agents], agentI++, (
		agent = agents[[agentI]];
		nearFoodI = findNearestSensableFoodI[agent, foods, params];
		{agents[[agentI]], foods} = If[nearFoodI >= 1, eat[agent, foods, nearFoodI, params], randomActionWalk[agent, foods, params]];
	)];
	model2["foods"] = foods;
	model2["agents"] = agents;
	model2
]


parameters = <| (*agent energy is currently unbounded, but we can change that*)
  "proximityRadius" -> 1, (*proximity radius for food consumption action*)
  "metabolism" -> 1, (*energy lost due to metabolism*)
  "dt" -> 0.1, (*timestep in seconds (we can change this later)*)
  "foodEnergy" -> 5.0, (*how much energy 1 food particle gives*)
  "foodSpawnCooldown" -> 20,
  "nFoodSpawn" -> 5,
  "nStartingAgents" -> 5,
  "nStartingFood" -> 20,
  "squareBounds" -> {0, 10}, (*min, max. Square environment*)
  "startingEnergy" -> 10,
  "stepLength" -> 0.5,
  "lifespan" -> 20 (*20 units*)
|>;

(*helper function testing*)
initializeTestModel[params_] :=

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
 
(*FUNCTION TESTING*)
 
model = initializeTestModel[parameters];
model["agents"]
model["foods"]
model["bounds"]
model["time"]

findNearestSensableFoodI[#, model["foods"], parameters]& /@ model["agents"]

model["agents"] = updateAgentInformation[model["agents"], 1, "energy", -1];
model["agents"] = updateAgentInformation[model["agents"], 2, "age", 30];
model["agents"]
model = randomizeAndKill[model, parameters];
model["agents"]

model = metabolizeAgents[model, parameters];
model["agents"]

model = ageAgents[model, parameters];
model["agents"]

model["agents"][[1]]
randomActionWalk[model["agents"][[1]], model["foods"], parameters][[1]]
model["agents"] = updateAgentInformation[model["agents"], 1, "pos", model["foods"][[1]] + {0.01, 0.01}]
findNearestSensableFoodI[model["agents"][[1]], model["foods"], parameters]
eat[model["agents"][[1]], model["foods"], findNearestSensableFoodI[model["agents"][[1]], model["foods"], parameters], parameters]

Print["Testing Main Agent Decision and Action Taking"]
model["agents"]
Length[model["foods"]]

findNearestSensableFoodI[#, model["foods"], parameters]& /@ model["agents"]
model = agentActions[model, parameters]

Length[model["foods"]]




