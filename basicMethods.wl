(* ::Package:: *)

parameters[] := <|
  "proximityRadius" -> 2.0, (*proximity radius for food*)
  "metabolismPerStep" -> 0.2, (*energy lost due to metabolism*)
  "dt" -> 0.1, (*timestep (we can change this later)*)
  "foodEnergy" -> 5.0 (*how much energy 1 food particle gives*)
|>;

checkWithinRadius[a_, b_, r_] := EuclideanDistance[a, b] <= r; (*agent's radius*)

(*checking the proximity for 1 food particle*)
checkFoodWithinRadius[agentPos_, foodPos_, params_] :=
  checkWithinRadius[agentPos, foodPos, params["proximityRadius"]];

(*checking the proximity for all foods and putting it into a list*)
foodsWithinRadius[agentPos_, foodPositions_List, params_] :=
  Select[foodPositions, checkFoodWithinRadius[agentPos, #, params] &];

findNearestFood[agentPos_, foodPositions_List] := Module[{dists, index}, (*function for finding nearest food*)
  If[Length[foodPositions] == 0,
    Missing["NoFood"],
    dists = EuclideanDistance[agentPos, #] & /@ foodPositions;
    index = First@Ordering[dists, 1];
    foodPositions[[index]]
  ]
];

changeMetabolism[energy_, params_] := energy - params["metabolismPerStep"]; (*energy lost due to metabolism*)

getPos[agent_Association] := agent["pos"];

(*creating 1 agent (test case)*)
params = parameters[];
agent = <|"pos" -> {0., 0.}, "energy" -> 10|>;
food = {{3., 4.}, {1.5,1.5}, {1., 1.}, {10., 10.}};

Print[food]

nearestFood = findNearestFood[getPos[agent], food] (*food that is closest to our agent*)
updatedFoods = foodsWithinRadius[getPos[agent], food, params] (*foods that are within the radius of 2 units*)
updatedEnergy = changeMetabolism[agent["energy"], params] (*current energy should be: 10-0.2 = 9.8*)

