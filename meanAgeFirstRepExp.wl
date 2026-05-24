(* ::Package:: *)

(*mean first reproduction age represented by a smooth histogram*)
meanFirstReproductionAge[simulation_, params_] := Module[
  {
    firstAges = <||>,
    prevAgents, currAgents, prevIDs, newAgents,
    parentIDs, parentAges
  },

  Do[
    prevAgents = simulation[[i - 1, "agents"]];
    currAgents = simulation[[i, "agents"]];

    prevIDs = Lookup[prevAgents, "id"];

    newAgents = Select[
      currAgents,
      ! MemberQ[prevIDs, #["id"]] && #["parents"] =!= {-1, -1} &
    ];

    Do[
      parentIDs = child["parents"];

      parentAges = Cases[
        prevAgents,
        a_ /; MemberQ[parentIDs, a["id"]] :> a["age"]
      ];

      Do[
        If[! KeyExistsQ[firstAges, parentIDs[[j]]],
          firstAges[parentIDs[[j]]] = parentAges[[j]]
        ],
        {j, Length[parentAges]}
      ],

      {child, newAgents}
    ],

    {i, 2, Length[simulation]}
  ];

  If[
    Length[Values[firstAges]] == 0,

    "No reproduction events occurred.",

    Column[{
      Row[{
        "Mean First Age at Reproduction: ",
        NumberForm[Mean[Values[firstAges]], {5, 2}]
      }],

      Row[{
        "Number of Agents Who Reproduced: ",
        Length[Values[firstAges]]
      }],

SmoothHistogram[
  Values[firstAges],
  Filling -> Axis,
  AxesLabel -> {
    "First Age at Reproduction",
    "Probability Density"
  },
  PlotLabel -> "Density of First Reproduction Ages",
  ImageSize -> 500
]
    }]
  ]
]

meanFirstReproductionAge[Simulation, parameters]
