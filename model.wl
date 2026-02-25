(* ::Package:: *)

createAgent[pos_, energy_, age_] :=   (*Create agent function, will be used in intializeModel function*)
  <|
    "pos" -> pos,
    "energy" -> energy,
    "age" -> age
  |>;

initializeModel[  
   nAgents_, (*parameter for number of agents*)
   nFood_, (*parameter for number of food items*)
   {min_, max_} (*parameter for bounds of environment, will be used to generate random positions for 
   agents and food*)
] :=

 Module[{agents, food},  
  agents =
   Table[
    createAgent[RandomReal[{min, max}, 2], 10.0, 0.0], nAgents
   ];

  food =
   RandomReal[{min, max}, {nFood, 2}];

  <|
   "agents" -> agents,
   "food" -> food,
   "bounds" -> {min, max},
   "time" -> 0
  |>
 ]


model = initializeModel[5, 20, {0, 10}]; (*test case for initializeModel function*)
agents = model["agents"]
food = model["food"]