(* ::Package:: *)

BeginPackage["BasicMethods`"]

createAgent::usage = "Returns agent association."
spawnFoodCheck::usage = "Spawns food at the appropriate time."
randomizeAndKill::usage = "Randomizes and kills agents that reach death condition."
agentActions::usage = "Main agent decision logic."
reproduceAgents::usage = "Agent reproduction function."
metabolizeAgents::usage = "Applies metabolism to agents"
ageAgents::usage = "Ages agents"

Begin["`Private`"]

checkWithinRadius[p1_, p2_, r_] := EuclideanDistance[p1, p2] <= r;

createAgent[pos_, energy_, age_, id_, currentDirection_, parents_:{-1, -1}] := <|
    "pos" -> pos,
    "energy" -> energy,
    "age" -> age, 
	"id" -> id,
	"currentDirection" -> currentDirection, (*unit vector*)
	"parents" -> parents,
    "ancestors" -> ancestors
|>

(* checking the proximity for all foods and putting it into a list. Implement grid/sector based optimization later. *)
findNearestSensableFoodPosAndI[agent_, foodPositions_List, params_] := Module[
    {agentPos, foodPosAndI, foodsWithinRadiusPosAndI, dists},
    agentPos = agent["pos"];
    foodPosAndI = Table[{foodPositions[[i]], i}, {i, Length[foodPositions]}];
    foodsWithinRadiusPosAndI = Select[foodPosAndI, checkWithinRadius[agentPos, #[[1]], params["sensingDistance"]]&];
    If[
        Length[foodsWithinRadiusPosAndI] == 0,
        {-1, -1}, (*no food*)
        dists = {EuclideanDistance[agentPos, #[[1]]], #[[2]]}& /@ foodsWithinRadiusPosAndI;
        First@MinimalBy[dists, First]
    ]
]

spawnFoods[foods_, params_] := Join[foods, RandomReal[params["squareBounds"], {params["nFoodSpawn"], 2}]]

spawnFoodCheck[model_, params_] := Module[
    {model2 = model},
	model2["foods"] = If[
        Mod[model["time"], params["foodSpawnCooldown"]] < 10^(-1),
        spawnFoods[model["foods"], params],
        model["foods"]
    ];
	model2
]

randomizeAndKill[model_, params_] := Module[
    {agents = model["agents"], model2=model},	
	agents = RandomSample[agents];
	model2["agents"] = Select[agents, (#["age"] < params["lifespan"] && #["energy"] > 0)&];
	model2
]

metabolizeAgents[model_, params_] := Module[
    {agents = model["agents"], model2=model},
	agents = MapAt[(# - params["metabolism"])&, agents, {All, "energy"}];
	model2["agents"] = agents;
	model2
]

ageAgents[model_, params_] := Module[
    {agents = model["agents"], model2=model},
	agents = MapAt[(# + params["dt"])&, agents, {All, "age"}];
	model2["agents"] = agents;
	model2
]

(*Obsolete.*)
randomActionWalk[agent_, params_] := Module[
    {theta = RandomReal[{0, 2 Pi}], agent2 = agent, newPos, min, max},
    {min, max} = params["squareBounds"];
    newPos = agent2["pos"] + {params["stepLength"] * Cos[theta], params["stepLength"] * Sin[theta]}; (*+ operator threadwise*)
    agent2["pos"] = Clip[newPos,  {min, max}];
    agent2
]

explorationOrIntentionalWalk[agent_, params_] := Module[ (*direction is set in agentActions[], depending on exploration or intentional*)
	{agent2 = agent, currentDir = agent["currentDirection"], newPos, min, max},
	{min, max} = params["squareBounds"];
	newPos = agent2["pos"] + {params["stepLength"] * currentDir[[1]], params["stepLength"] * currentDir[[2]]}; (*+ operator threadwise*)
	agent2["pos"] = Clip[newPos, {min, max}];
	agent2
]

agentEat[agent_, params_] := Module[
    {agent2 = agent, newE},
	newE = agent2["energy"] + params["foodEnergy"];
	agent2["energy"] = If[newE > params["maxEnergy"], params["maxEnergy"], newE];
	agent2
]

agentSetDir[agent_, foodPos_] := Module[
	{agent2 = agent, dirV},
	dirV = foodPos - agent["pos"]; (*- operator threadwise*)
	agent2["currentDirection"] = (1/Norm[dirV]) * dirV;
	agent2
]

agentSetExploreDir[agent_, params_] := Module[
	{agent2 = agent, randomTheta},
	agent2["currentDirection"] = 
	If[RandomReal[] < params["dirChangeProb"],
		randomTheta = RandomReal[{0, 2 Pi}]; {Cos[randomTheta], Sin[randomTheta]}, 
		agent2["currentDirection"]
	];
	agent2
]

(*I believe agents will never have more than max energy due to metabolism sending it to max-1 metabolism*)

(*Old agentActions[] without intentional movement.*)

(*
agentActions[model_, params_] :=  Module[
    {model2 = model, agents = model["agents"], foods = model["foods"], agent, nearFoodI, nearFoodDist},
	agents = Table[
		agent = agents[[i]];
		{nearFoodDist, nearFoodI} = findNearestSensableFoodPosAndI[agent, foods, params];
		If[
            nearFoodI>=1 && nearFoodDist < params["proximityRadius"], 
			foods = Delete[foods, nearFoodI]; agentEat[agent, params], 
			randomActionWalk[agent, params]
        ],
	    {i, Length@agents}
    ];
	model2["foods"] = foods;
	model2["agents"] = agents;
	model2
]
*)



agentActions[model_, params_] :=  Module[
    {model2 = model, agents = model["agents"], foods = model["foods"], agent, nearFoodI, nearFoodDist, nearFoodPos},
	agents = Table[
		agent = agents[[i]];
		{nearFoodDist, nearFoodI} = findNearestSensableFoodPosAndI[agent, foods, params];
		nearFoodPos = foods[[nearFoodI]];
		Which[
            nearFoodI>=1 && nearFoodDist < params["proximityRadius"], 
			foods = Delete[foods, nearFoodI]; agentEat[agent, params], 
			nearFoodI>=1,
			agent = agentSetDir[agent, nearFoodPos]; agent = explorationOrIntentionalWalk[agent, params]; agent, (*don't know if we want to use up a timestep as a change of direction cost...*)
			True,
			agent = agentSetExploreDir[agent, params]; agent = explorationOrIntentionalWalk[agent, params]; agent
        ],
	    {i, Length@agents}
    ];
	model2["foods"] = foods;
	model2["agents"] = agents;
	model2
]


findClosestAgentI[agentI_Integer, agents_List] := Module[
    {pos, dists},
    If[Length[agents] < 2, Return[-1]];
    pos = agents[[agentI]]["pos"];
    dists = Table[
        If[i == agentI, Infinity, EuclideanDistance[pos, agents[[i]]["pos"]]],
        {i, Length[agents]}
    ];
    First@Ordering[dists, 1]
]

(* WHAT IS THIS LONG BOOLEAN EXPRESSION? WHY DO WE HAVE THESE EXTRA VARS? *)
areRelated[ai_, aj_] := Or[
    MemberQ[ai["ancestors"], aj["id"]],
    MemberQ[aj["ancestors"], ai["id"]],
    (ai["parents"] =!= {-1, -1} && ai["parents"] === aj["parents"])
]

reproduceAgents[model_, params_] := Module[
    {
        agents = model["agents"], model2 = model, newAgents = {},
        reproduced, nextID, j, ai, aj, dist, midPos, newAgent
    },

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
       aj["age"] >= params["minReproductionAge"] &&
       !areRelated[ai, aj],

      (* both parents pay energy cost *)
      agents[[i]] = ReplacePart[agents[[i]], 
        "energy" -> ai["energy"] - params["reproductionEnergyCost"]];
      agents[[j]] = ReplacePart[agents[[j]], 
        "energy" -> aj["energy"] - params["reproductionEnergyCost"]];

      midPos = (ai["pos"] + aj["pos"]) / 2;
      newAncestors = DeleteDuplicates[Join[{ai["id"], aj["id"]}, ai["ancestors"], aj["ancestors"]]];
      newAgent = createAgent[midPos, params["startingEnergy"] / 2, 0.0, nextID, {ai["id"], aj["id"]}, newAncestors];
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

End[]
EndPackage[]
