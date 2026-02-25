visualizeModel[model_] :=
 Module[{agents, food, bounds},

 agents = model["agents"][[All ,"pos"]];
 food = model["food"];
 bounds = model["bounds"];

  Graphics[
   {
   Blue, PointSize[.02], Point[agents], 
   Red, PointSize[.02], Point[food]
   },
    Axes -> True, PlotRange -> {bounds, bounds}, ImageSize -> 500
  ]
 ]
 
 (*to animate, call this code:
 ListAnimate[
  NestList[(*insert model step function here*), visualizeModel[(*insert model here*)], (*number of timesteps*))]
 ]
*)