(* ::Package:: *)

scriptDir = NotebookDirectory[];
SetDirectory[scriptDir];

checkWithinRadius[p1_, p2_, r_] := EuclideanDistance[p1, p2] <= r; (*agent's radius*)

(*checking the proximity for 1 food particle*)
checkFoodWithinRadius[agentPos_, foodPos_, proxRadius_] :=
  checkWithinRadius[agentPos, foodPos, proxRadius];

(*checking the proximity for all foods and putting it into a list. Implement grid/sector based optimization later.*)
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
]

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

randomActionWalk[agent_, params_] := Module[{theta = RandomReal[{0, 2 Pi}], agent2 = agent, newPos, min, max},
  {min, max} = params["squareBounds"];
  newPos = agent2["pos"] + {params["stepLength"] * Cos[theta], params["stepLength"] * Sin[theta]}; (*+ operator threadwise*)
  agent2["pos"] = Clip[newPos,  {min, max}];
  agent2
]

agentEat[agent_, params_] := Module[{agent2 = agent, newE},
	newE = agent2["energy"] + params["foodEnergy"];
	agent2["energy"] = If[newE > params["maxEnergy"], params["maxEnergy"], newE];
	agent2
]

(*I believe agents will never have more than max energy due to metabolism sending it to max-1 metabolism*)
agentActions[model_, params_] :=  Module[{model2 = model, agents = model["agents"], foods = model["foods"], agent, nearFoodI},
	agents = Table[
	agent = agents[[i]];
	nearFoodI = findNearestSensableFoodI[agent, foods, params];
		If[nearFoodI>=1, foods = Delete[foods, nearFoodI]; agentEat[agent,params], randomActionWalk[agent, params]]
		,{i,Length@agents}
	];
	model2["foods"] = foods;
	model2["agents"] = agents;
	model2
]
