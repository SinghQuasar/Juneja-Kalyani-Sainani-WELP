(* ::Package:: *)

BeginPackage["BasicMethods`"]

checkWithinRadius[p1_, p2_, r_] := EuclideanDistance[p1, p2] <= r; (*agent's radius*)

(*checking the proximity for 1 food particle*)
checkFoodWithinRadius[agentPos_, foodPos_, proxRadius_] :=
  checkWithinRadius[agentPos, foodPos, proxRadius];

(*checking the proximity for all foods and putting it into a list. Implement grid/sector based optimization later.*)
(*currently revising to do this evaluation in sensing distance, and to find the nearest piece of food. Proximity distance for eating will be checked in the main loop.*)
findNearestSensableFoodPosAndI[agent_, foodPositions_List, params_] := Module[{agentPos, foodPosAndI, foodsWithinRadiusPosAndI, dists, closest},
  agentPos = agent["pos"];
  foodPosAndI = Table[{foodPositions[[i]], i}, {i, Length[foodPositions]}];
  foodsWithinRadiusPosAndI = Select[foodPosAndI, checkFoodWithinRadius[agentPos, #[[1]], params["sensingDistance"]]&];
  If[Length[foodsWithinRadiusPosAndI] == 0,
    {{-1, -1}, -1}, (*no food*)
    dists = {EuclideanDistance[agentPos, #[[1]]], #[[2]]}& /@ foodsWithinRadiusPosAndI;
    closest = First@MinimalBy[dists, First];
    closest
  ]
]

createAgent[pos_, energy_, age_, id_, parents_:{-1, -1}] :=
  <|
    "pos" -> pos,
    "energy" -> energy,
    "age" -> age,
	"id" -> id,
	"parents" -> parents
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

(*
(*need 2 new model parameters: sensing distance and direction reset cooldown. Need 1 new agent parameter: currentDirection*)
ActionWalk[agent_, params_] := Module[{theta = agent["currentDirection"], agent2 = agent, newPos, min, max}, (*Uses direction reset cooldown parameter*)
	{min, max} = params["squareBounds"];
]
*)

agentEat[agent_, params_] := Module[{agent2 = agent, newE},
	newE = agent2["energy"] + params["foodEnergy"];
	agent2["energy"] = If[newE > params["maxEnergy"], params["maxEnergy"], newE];
	agent2
]

(*I believe agents will never have more than max energy due to metabolism sending it to max-1 metabolism*)
agentActions[model_, params_] :=  Module[{model2 = model, agents = model["agents"], foods = model["foods"], agent, nearFoodI, nearFoodPos},
	agents = Table[
		agent = agents[[i]];
		{nearFoodPos, nearFoodI} = findNearestSensableFoodPosAndI[agent, foods, params];
			If[nearFoodI>=1 && EuclideanDistance[agent["pos"], nearFoodPos]<params["proximityRadius"], 
				foods = Delete[foods, nearFoodI]; agentEat[agent,params], randomActionWalk[agent, params]]
		,{i,Length@agents}
	];
	model2["foods"] = foods;
	model2["agents"] = agents;
	model2
]

findClosestAgentI[agentI_Integer, agents_List] := Module[{pos, dists, closest},
  If[Length[agents] < 2, Return[-1]];
  pos = agents[[agentI]]["pos"];
  dists = Table[
    If[i == agentI, Infinity, EuclideanDistance[pos, agents[[i]]["pos"]]],
    {i, Length[agents]}
  ];
  closest = First@Ordering[dists, 1];
  closest
]

reproduceAgents[model_, params_] := Module[
  {agents = model["agents"], model2 = model, newAgents = {}, 
   reproduced, nextID, i, j, ai, aj, dist, midPos, newAgent},

  nextID = params["nextAgentID"];
  reproduced = ConstantArray[False, Length[agents]];

  Do[
    If[reproduced[[i]], Continue[]];
    j = findClosestAgentI[i, agents];
    If[j == -1 || reproduced[[j]], Continue[]];

    ai = agents[[i]];
    aj = agents[[j]];
    dist = EuclideanDistance[ai["pos"], aj["pos"]];

    If[dist <= params["reproductionRadius"] &&
       ai["energy"] >= params["minReproductionEnergy"] &&
       aj["energy"] >= params["minReproductionEnergy"] &&
       ai["age"] >= params["minReproductionAge"] &&
       aj["age"] >= params["minReproductionAge"],

      (* both parents pay energy cost *)
      agents[[i]] = ReplacePart[agents[[i]], 
        "energy" -> ai["energy"] - params["reproductionEnergyCost"]];
      agents[[j]] = ReplacePart[agents[[j]], 
        "energy" -> aj["energy"] - params["reproductionEnergyCost"]];

      midPos = (ai["pos"] + aj["pos"]) / 2;
      newAgent = createAgent[midPos, params["startingEnergy"] / 2, 0.0, nextID, {ai["id"], aj["id"]}];
      AppendTo[newAgents, newAgent];
      nextID++;

      reproduced[[i]] = True;
      reproduced[[j]] = True;
    ];
  , {i, Length[agents]}];

  model2["agents"] = Join[agents, newAgents];
  model2["nextAgentID"] = nextID;
  model2
]

EndPackage[]
