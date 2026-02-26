(* ::Package:: *)

scriptDir = NotebookDirectory[];
SetDirectory[scriptDir]

checkWithinRadius[p1_, p2_, r_] := EuclideanDistance[p1, p2] <= r; (*agent's radius*)

(*checking the proximity for 1 food particle*)
checkFoodWithinRadius[agentPos_, foodPos_, proxRadius] :=
  checkWithinRadius[agentPos, foodPos, proxRadius];

(*checking the proximity for all foods and putting it into a list*)
foodsWithinRadius[agentPos_, foodPositions_List, proxRadius] :=
  Select[foodPositions, checkFoodWithinRadius[agentPos, #, proxRadius] &];

findNearestFoodI[agentPos_, foodPositions_List] := Module[{dists, index}, (*function for finding nearest food*)
  If[Length[foodPositions] == 0,
    {-1, {-1, -1}}, (*no food*)
    dists = EuclideanDistance[agentPos, #] & /@ foodPositions;
    index = First@Ordering[dists, 1];
    index
  ]
];

createAgent[pos_, energy_, age_] :=   (*Create agent function, will be used in intializeModel function*)
  <|
    "pos" -> pos,
    "energy" -> energy,
    "age" -> age
  |>

getPos[agent_Association] := agent["pos"];

spawnFoods[foods, params] := (
  Join[foods, RandomReal[params["squareBounds"], {params["nFoodSpawn"], 2}]]
)

randomActionWalk[agent, params] := (
  theta = RandomReal[{0, 2 Pi}];
  agent["pos"] + {params["stepLength"] * Cos[theta], params["stepLength"] * Sin[Theta]} (*+ operator threadwise*)
)

testBasicMethods[] := ( (*for debugging*)
  params = <|
    "proximityRadius" -> 2.0, (*proximity radius for food*)
    "metabolismPerAction" -> 0.2, (*energy lost due to metabolism*)
    "dt" -> 0.1, (*timestep (we can change this later)*)
    "foodEnergy" -> 5.0 (*how much energy 1 food particle gives*)
  |>;
  agent = <|"pos" -> {0., 0.}, "energy" -> 10|>;
  food = {{3., 4.}, {1.5,1.5}, {1., 1.}, {10., 10.}};

  Print[food];

  nearestFood = findNearestFood[getPos[agent], food] (*food that is closest to our agent*)
)

testBasicMethods[]