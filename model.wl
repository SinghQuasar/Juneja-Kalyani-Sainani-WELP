(* ::Package:: *)

SetDirectory[NotebookDirectory[]];
Needs["BasicMethods`"]

(*<< basicMethods.wl -- we're initializing them in the kernel right now, but working on changing to formal package*)

parameters = <| (*agent energy is currently unbounded, but we can change that*)
  "proximityRadius" -> 1, (*proximity radius for food consumption action*)
  "sensingDistance" -> 1,
  "metabolism" -> 0.2, (*energy lost due to metabolism*)
  "dt" -> 0.1, (*timestep in seconds (we can change this later)*)
  "foodEnergy" -> 5.0, (*how much energy 1 food particle gives*)
  "foodSpawnCooldown" -> 1,
  "nFoodSpawn" -> 40,
  "nStartingAgents" -> 50,
  "nStartingFood" -> 3,
  "squareBounds" -> {0, 5}, (*min, max. Square environment*)
  "startingEnergy" -> 10,
  "stepLength" -> 1,
  "lifespan" -> 20, (*20 units*)
  "maxEnergy" -> 10,
  "imageSize" -> 500,
  "agentSize" -> 0.025, (*radius in pure length units*)
  "foodSize" -> 0.01,
  "reproductionRadius" -> 1.5,
  "minReproductionEnergy" -> 6.0,
  "minReproductionAge" -> 2.0,
  "reproductionEnergyCost" -> 3.0,
  "nextAgentID" -> 1
|>;

(*useful for image scaling...*)
lUnitsToPixels[lUnits_, params_] := lUnits * (params["imageSize"]/(params["squareBounds"][[2]] - params["squareBounds"][[1]]))
lPixelsToUnits[lPixels_, params_]:= lPixels * ((params["squareBounds"][[2]] - params["squareBounds"][[1]])/params["imageSize"])

initializeModel[params_] :=

  Module[{agents, foods, nStartingAgents, nStartingFood, startingEnergy, min, max},  

    nStartingAgents = params["nStartingAgents"]; (*parameter for number of agents*)
    nStartingFood = params["nStartingFood"]; (*parameter for number of food items*)
    startingEnergy = params["startingEnergy"];
    {min, max} = params["squareBounds"];

    agents =
    Table[
      createAgent[RandomReal[{min, max}, 2], startingEnergy, 0.0, i], {i, nStartingAgents}
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

propagateModelStep[params_, model_] := Module[{model2=model, params2=params}, (*working on changing to DynamicModule/Manipulate. This propogateModel method is not confirmed to be working, but the indiivdual helper methods have been tested.*)
  params2["nextAgentID"] = model2["nextAgentID"];
  model2 = spawnFoodCheck[model2, params];
  model2 = randomizeAndKill[model2, params]; (*randomization disabled for clarity atm.*)
  model2 = agentActions[model2, params];
  model2 = reproduceAgents[model2, params2];
  model2 = metabolizeAgents[model2, params];
  model2 = ageAgents[model2, params];
  model2["time"] = model2["time"] + params["dt"];
  model2
 ];
 
displayGrid[model_, params_] := Module[
  {
    agents, foods, min, max, span, nAgents,
    hudPad, agentR, foodR, barW, barH, barGap,
    showEnergyBars, legendX, legendY, infoX, infoY,
    ageColor, ageTextColor, energyColor,
    chooseBarCenter, otherAgentPositions,
    agentGraphics, foodGraphics, legend, infoText
  },

  agents = model["agents"];
  foods = model["foods"];
  {min, max} = params["squareBounds"];
  span = max - min;
  nAgents = Length[agents];

  agentR = params["agentSize"]*span;
  foodR  = params["foodSize"]*span;

  barW = 0.10 span;
  barH = 0.015 span;
  barGap = 0.040 span;

  hudPad = 0.001 span;

  showEnergyBars = nAgents < 6;

  ageColor[age_] := Blend[{Green, Yellow, Orange, Red},Clip[age/(0.3 params["lifespan"]), {0, 1}]];
  ageTextColor[age_] := If[age < 0.6 params["lifespan"], Black, White];
  energyColor[frac_] := Blend[{Red, Yellow, Green},Clip[frac, {0, 1}]];
  otherAgentPositions[pos_] := DeleteCases[Lookup[agents, "pos"], pos];
  chooseBarCenter[pos_, others_] := Module[{candidates, scores},candidates = {pos + {0, barGap},pos + {0, -barGap},pos + {barGap, 0},pos + {-barGap, 0}};

    candidates = {
      {Clip[candidates[[1, 1]], {min + barW/2, max - barW/2}], Clip[candidates[[1, 2]], {min + barH/2, max - barH/2}]},
      {Clip[candidates[[2, 1]], {min + barW/2, max - barW/2}], Clip[candidates[[2, 2]], {min + barH/2, max - barH/2}]},
      {Clip[candidates[[3, 1]], {min + barW/2, max - barW/2}], Clip[candidates[[3, 2]], {min + barH/2, max - barH/2}]},
      {Clip[candidates[[4, 1]], {min + barW/2, max - barW/2}], Clip[candidates[[4, 2]], {min + barH/2, max - barH/2}]}};

    scores = If[Length[others] == 0, ConstantArray[1, Length[candidates]], Min[Norm[# - #2] & @@@ Tuples[{{#}, others}]] & /@ candidates];

    candidates[[First @ Ordering[scores, -1]]]];

  agentGraphics = Table[
    Module[
      {
        pos, age, e, frac, percent,
        fillColor, txtColor,
        barCenter, barLeft, barBottom
      },

      pos = agent["pos"];
      age = agent["age"];
      e = agent["energy"];

      frac = Clip[e/params["maxEnergy"], {0, 1}];
      percent = Round[100 frac];

      fillColor = ageColor[age];
      txtColor = ageTextColor[age];

      barCenter = chooseBarCenter[pos, otherAgentPositions[pos]];
      barLeft = barCenter[[1]] - barW/2;
      barBottom = barCenter[[2]] - barH/2;

      {EdgeForm[Directive[White, Thickness[0.0015]]],fillColor,Disk[pos, agentR],Text[Style[ToString[Round[age]],Max[7, Round[0.018 params["imageSize"]]],txtColor,Bold],pos],

        If[showEnergyBars,{EdgeForm[Directive[White, Thickness[0.0012]]],Darker[Gray, 0.75],Rectangle[{barLeft, barBottom},{barLeft + barW, barBottom + barH}],energyColor[frac],
        Rectangle[{barLeft, barBottom},{barLeft + barW frac, barBottom + barH}],
            Text[Style[ToString[percent],Max[6, Round[0.014 params["imageSize"]]],Magenta],barCenter]},Nothing]}],{agent, agents}];

  foodGraphics = Table[{White,Disk[f, foodR]},{f, foods}];

  legendX = min + hudPad;
  legendY = max - hudPad;

  infoX = max - hudPad;
  infoY = max - hudPad;

  legend = {Text[Style["Legend", 11, White, Bold],{legendX, legendY},{-1, 1}],
  EdgeForm[Directive[White, Thickness[0.0015]]],ageColor[0.2 params["lifespan"]],Disk[{legendX + 0.020 span, legendY - 0.050 span}, params["foodSize"]*span*1.3],Text[Style["Agent", 9, White],{legendX + 0.042 span, legendY - 0.050 span},{-1, 0}],

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

model = initializeModel[parameters];
Simulation = genSimulationStates[parameters, model, 100];
simulationGraphics = renderSimulation[Simulation, parameters];

Manipulate[simulationGraphics[[j]], {j, 1, Length@simulationGraphics, 1, Appearance->Labeled}]

plotPopulationStats[Simulation]
|
(*old version of displayGrid*)
(*displayGrid[model_] := Grid[{{"Food Length: "<>ToString@Length@model["foods"], "Time: "<>ToString@model["time"]}, {Dataset@model["agents"], Dataset@model["foods"]}}*)





