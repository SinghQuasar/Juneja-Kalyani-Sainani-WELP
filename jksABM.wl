(* ::Package:: *)

BeginPackage["jksABM`"]

initializeModel::usage = "Creates model object with desired parameters"
genSimulationStates::usage = "Generates simulation time-series"
renderSimulation::usage = "Renders frames from time-series"
plotPopulationStats::usage = "Plots population statistics from simulation time-series"

Begin["`Private`"]

checkWithinRadius[p1_, p2_, r_] := EuclideanDistance[p1, p2] <= r;

createAgent[pos_, energy_, age_, id_, currentDirection_, parents_:{-1, -1}] := <|
    "pos" -> pos,
    "energy" -> energy,
    "age" -> age, 
	"id" -> id,
	"currentDirection" -> currentDirection, (*unit vector*)
	"parents" -> parents
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

(*Without wall avoidance logic.*)
(*
agentSetExploreDir[agent_, params_] := Module[
	{agent2 = agent, randomTheta},
	agent2["currentDirection"] = 
	If[RandomReal[] < params["dirChangeProb"],
		randomTheta = RandomReal[{0, 2 Pi}]; {Cos[randomTheta], Sin[randomTheta]}, 
		agent2["currentDirection"]
	];
	agent2
]
*)

(*Wall avoidance logic.*)
(*Having proxRad be << stepLength ensures no repeated direction flipping.*)
agentSetExploreDir[agent_, params_] := Module[
	{agent2 = agent, ax, ay, sqmin, sqmax, proxRad, randomTheta},
	{ax, ay} = agent["pos"];
	{sqmin, sqmax} = params["squareBounds"];
	proxRad = params["stepLength"]/2;
	agent2["currentDirection"] = 
	Which[
		(ax < sqmin + proxRad || ax > sqmax - proxRad || ay < sqmin + proxRad || ay > sqmax - proxRad),
		-1*agent2["currentDirection"], (*direction flipping*)
		
		RandomReal[] < params["dirChangeProb"],
		randomTheta = RandomReal[{0, 2 Pi}]; {Cos[randomTheta], Sin[randomTheta]}, 
		
		True,
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
    MemberQ[ai["parents"], aj["id"]],
    MemberQ[aj["parents"], ai["id"]],
    (ai["parents"] =!= {-1, -1} && ai["parents"] === aj["parents"])
]

reproduceAgents[model_, params_] := Module[
    {
        agents = model["agents"],
        model2 = model,
        newAgents = {},
        reproduced,
        nextID = model["nextAgentID"],
        j,
        ai, aj,
        dist,
        midPos,
        theta, dir
    },

    (* tracks which agents have already paired this step *)
    reproduced = ConstantArray[False, Length[agents]];

    Do[
        (* skip if already paired *)
        If[reproduced[[i]], Continue[]];

        j = findClosestAgentI[i, agents];

        (* skip if no neighbor or neighbor already paired *)
        If[j == -1 || reproduced[[j]], Continue[]];

        ai = agents[[i]];
        aj = agents[[j]];
        dist = EuclideanDistance[ai["pos"], aj["pos"]];

        (* range, energy, age, and incest checks *)
        If[
            dist <= params["proximityRadius"]
            && ai["energy"] >= params["minReproductionEnergy"]
            && aj["energy"] >= params["minReproductionEnergy"]
            && ai["age"] >= params["minReproductionAge"]
            && aj["age"] >= params["minReproductionAge"]
            && !areRelated[ai, aj]
            ,

            (* parents pay energy cost *)
            agents[[i, "energy"]] = ai["energy"] - params["reproductionEnergyCost"];
            agents[[j, "energy"]] = aj["energy"] - params["reproductionEnergyCost"];

            midPos = (ai["pos"] + aj["pos"]) / 2;
            theta = RandomReal[{0, 2 Pi}];
            dir = {Cos[theta], Sin[theta]};
            
            AppendTo[
                newAgents,
                createAgent[
                    midPos,
                    params["startingEnergy"] / 2,
                    0.0,
                    nextID,
                    dir,
                    {ai["id"], aj["id"]}
                ]
            ];

            nextID++;
            reproduced[[i]] = True;
            reproduced[[j]] = True;
        ];
        ,
        {i, Length[agents]}
    ];

    model2["agents"] = Join[agents, newAgents];
    model2["nextAgentID"] = nextID;
    model2
]

(*Previously in model.wl*)

(*useful for image scaling...*)
lUnitsToPixels[lUnits_, params_] := lUnits * (params["imageSize"]/(params["squareBounds"][[2]] - params["squareBounds"][[1]]))
lPixelsToUnits[lPixels_, params_]:= lPixels * ((params["squareBounds"][[2]] - params["squareBounds"][[1]])/params["imageSize"])

initializeModel[params_] :=

  Module[{agents, foods, nStartingAgents, nStartingFood, startingEnergy, min, max, randomDir},  

    nStartingAgents = params["nStartingAgents"]; (*parameter for number of agents*)
    nStartingFood = params["nStartingFood"]; (*parameter for number of food items*)
    startingEnergy = params["startingEnergy"];
    {min, max} = params["squareBounds"];

    agents =
    Table[
      randomDir = RandomReal[{0, 2 Pi}];
      createAgent[RandomReal[{min, max}, 2], startingEnergy, 0.0, i, {Cos[randomDir], Sin[randomDir]}], {i, nStartingAgents}
    ];

    foods = RandomReal[{min, max}, {nStartingFood, 2}];
	
    <| 
      "agents" -> agents, (*list of agent associations*)
      "foods" -> foods, (*list of food coordinates*)
      "bounds" -> {min, max},
      "time" -> 0,
      "nextAgentID" -> nStartingAgents + 1
    |>
 ]

propagateModelStep[params_, model_] := Module[{model2=model},
  model2 = spawnFoodCheck[model2, params];
  model2 = randomizeAndKill[model2, params]; (*randomization*)
  model2 = agentActions[model2, params];
  model2 = reproduceAgents[model2, params]; (*why do we have params2?*)
  model2 = metabolizeAgents[model2, params];
  model2 = ageAgents[model2, params];
  model2["time"] = model2["time"] + params["dt"];
  model2
 ];
 
 (*functions for displayGrid*)

ageColor[age_, params_] := 
  Blend[{Green, Yellow, Orange, Red},
    Clip[age/(0.3 params["lifespan"]), {0, 1}]
  ];

ageTextColor[age_, params_] := If[age < 0.6 params["lifespan"], Black, White];

energyColor[frac_] := Blend[{Red, Yellow, Green}, Clip[frac, {0, 1}]];

otherAgentPositions[pos_, agents_] := DeleteCases[Lookup[agents, "pos"], pos];

chooseBarCenter[pos_, others_, min_, max_, barW_, barH_, barGap_] := Module[{candidates, scores},

  candidates = {
    pos + {0, barGap},
    pos + {0, -barGap},
    pos + {barGap, 0},
    pos + {-barGap, 0}
  };

  candidates = ({Clip[#[[1]], {min + barW/2, max - barW/2}], 
                 Clip[#[[2]], {min + barH/2, max - barH/2}]} &) /@ candidates;

  scores = If[
    Length[others] == 0,
    ConstantArray[1, Length[candidates]],
    Min[Norm[# - #2] & @@@ Tuples[{{#}, others}]] & /@ candidates
  ];

  candidates[[First @ Ordering[scores, -1]]]
];
 
(* 
  - drawAgentGraphic creates the graphics for one individual agent (agent circle, age counter, and energy bar)
  - Helps reduce bloat b/c this method allows us to avoid using Module inside of Table
  - Can consider putting this method inside of basicMethods.wl
*)

drawAgentGraphic[agent_, agents_, params_, min_, max_, span_, agentR_, barW_, barH_, barGap_, showEnergyBars_] := Module[
  {
    pos, age, e, frac, percent,
    fillColor, txtColor,
    barCenter, barLeft, barBottom
  },

  (* get each agent's position, age, and energy from the agent association *)
  
  pos = agent["pos"];
  age = agent["age"];
  e = agent["energy"];

(* convert the agent's current energy into a fraction from 0 to 1 so that we can use it to find the energy bar length & color*)
  frac = Clip[e/params["maxEnergy"], {0, 1}];
  percent = Round[100 frac];

  fillColor = ageColor[age, params]; (*younger agents are greener, older agents are redder*)
  txtColor = ageTextColor[age, params];

  barCenter = chooseBarCenter[ (*this checks nearby agents and tries to place the bar in the least crowded area*)
    pos,
    otherAgentPositions[pos, agents],
    min, max, barW, barH, barGap
  ];

(*the bar's center becomes the lower left corner so the bar doesn't go over the boundaries*)
  barLeft = barCenter[[1]] - barW/2;
  barBottom = barCenter[[2]] - barH/2;

  {
    EdgeForm[Directive[White, Thickness[0.0015]]],
    fillColor,
    Disk[pos, agentR],
    Text[ (*display agent's rounded age*)
      Style[
        ToString[Round[age]],
        Max[7, Round[0.018 params["imageSize"]]],
        txtColor,
        Bold
      ],
      pos
    ],

    If[
      showEnergyBars, (*only showing energy bars when n<6*)
      {
      (*this code below defines specifics for the energy bar (color, numbers, size, etc.)*)
        EdgeForm[Directive[White, Thickness[0.0012]]],
        Darker[Gray, 0.75],
        Rectangle[{barLeft, barBottom}, {barLeft + barW, barBottom + barH}],
        energyColor[frac],
        Rectangle[{barLeft, barBottom}, {barLeft + barW frac, barBottom + barH}],
        Text[
          Style[
            ToString[percent],
            Max[6, Round[0.014 params["imageSize"]]],
            Magenta
          ],
          barCenter
        ]
      },
      Nothing (*if n>6, don't create the bars*)
    ]
  }
];

(* 
  - displayGrid renders the full simulation (food, agents, legend, time, agent count, etc.)
  - each agent is drawn by the drawAgentGraphic method
*)

displayGrid[model_, params_] := Module[ (*we should use conversion functions instead of span imo.*)
  {
    agents, foods, min, max, span, nAgents,
    hudPad, agentR, foodR, barW, barH, barGap,
    showEnergyBars, legendX, legendY, infoX, infoY,
    agentGraphics, foodGraphics, legend, infoText
  },

  agents = model["agents"];
  foods = model["foods"];
  {min, max} = params["squareBounds"];
  span = max - min;
  nAgents = Length[agents];

  agentR = lUnitsToPixels[params["agentSize"], params];
  foodR = lUnitsToPixels[params["foodSize"], params];

  barW = 0.10 span;
  barH = 0.015 span;
  barGap = 0.040 span;

  hudPad = 0.001 span;

  showEnergyBars = nAgents < 6;

  agentGraphics = drawAgentGraphic[#,agents,params,min,max,span,agentR,barW,barH,barGap,showEnergyBars] & /@ agents;

  foodGraphics = Table[{White,Disk[f, foodR]},{f, foods}];

  legendX = min + hudPad;
  legendY = max - hudPad;

  infoX = max - hudPad;
  infoY = max - hudPad;

  legend = {Text[Style["Legend", 11, White, Bold],{legendX, legendY},{-1, 1}],
  EdgeForm[Directive[White, Thickness[0.0015]]],ageColor[0.2 params["lifespan"], params],Disk[{legendX + 0.020 span, legendY - 0.050 span}, params["foodSize"]*span*1.3],Text[Style["Agent", 9, White],{legendX + 0.042 span, legendY - 0.050 span},{-1, 0}],

    If[showEnergyBars,{EdgeForm[Directive[White, Thickness[0.0012]]],Darker[Gray, 0.75],Rectangle[{legendX, legendY - 0.090 span},{legendX + 0.070 span, legendY - 0.077 span}],energyColor[0.75],Rectangle[{legendX, legendY - 0.090 span},{legendX + 0.0525 span, legendY - 0.077 span}],
        Text[Style["Energy", 9, White],{legendX + 0.085 span, legendY - 0.0835 span},{-1, 0}]},Nothing],White,Disk[{legendX + 0.020 span, legendY - 0.130 span}, params["foodSize"]*span],Text[Style["Food", 9, White],{legendX + 0.042 span, legendY - 0.130 span},{-1, 0}]};

  infoText = Text[Style[Column[{"Time: " <> ToString @ NumberForm[model["time"], {4, 2}],"Food Count: " <> ToString @ Length[foods],"Agents: " <> ToString @ nAgents},
        Alignment -> Right],11,White],{infoX, infoY},{1, 1}];

  Graphics[{foodGraphics,agentGraphics,legend,infoText},
    Background -> Black,
    PlotRange -> {{min, max}, {min, max}},
    PlotRangePadding -> Scaled[0.01],
    ImagePadding -> 10,
    ImageSize -> params["imageSize"]
  ]
] 
	 
(*old version of displayGrid*)
(*displayGrid[model_] := Grid[{{"Food Length: "<>ToString@Length@model["foods"], "Time: "<>ToString@model["time"]}, {Dataset@model["agents"], Dataset@model["foods"]}}*)
	 
genSimulationStates[params_, model_, timesteps_] := Module[{model2 = model}, Join[{model},Table[model2 = propagateModelStep[params, model2];model2,{s, timesteps}]]]

renderSimulation[states_List, params_] := displayGrid[#, params] & /@ states

plotPopulationStats[simulation_List] := Module[
  {times, agentCounts, foodCounts},
  times = #["time"] & /@ simulation;
  agentCounts = Length[#["agents"]] & /@ simulation;
  foodCounts = Length[#["foods"]] & /@ simulation;

  ListLinePlot[
    {agentCounts, foodCounts},
    DataRange  -> {First[times], Last[times]},
    PlotLegends -> {"Agents", "Food"},
    PlotStyle  -> {Blue, Green},
    AxesLabel  -> {"Time", "Count"},
    PlotLabel  -> "Population Over Time",
    GridLines  -> Automatic,
    ImageSize  -> 500
  ]
];

End[]
EndPackage[]
