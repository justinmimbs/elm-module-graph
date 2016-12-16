module Main exposing (..)

import AcyclicDigraph exposing (AcyclicDigraph)
import Diagram
import DiagramConnectivity
import Dict exposing (Dict)
import Digraph exposing (Node, Edge)
import Html exposing (Html)
import Html.Attributes
import Set exposing (Set)
import Svg exposing (Svg)
import Svg.Attributes

import Json.Decode exposing (Decoder)


input : String
input =
  """{"VirtualDom.Report": {"imports": [], "package": "elm-lang/virtual-dom"}, "RoseTree": {"imports": ["Lazy.List"], "package": "elm-community/elm-test"}, "VirtualDom.Metadata": {"imports": ["Array", "Dict", "Json.Decode", "Json.Encode", "VirtualDom.Report"], "package": "elm-lang/virtual-dom"}, "TestResult": {"imports": ["Expect", "Html", "Html.Attributes", "String", "Test.Runner"], "package": "justinmimbs/elm-date-extra"}, "VirtualDom.Debug": {"imports": ["Json.Decode", "Json.Encode", "Task", "VirtualDom.Expando", "VirtualDom.Helpers", "VirtualDom.History", "VirtualDom.Metadata", "VirtualDom.Overlay", "VirtualDom.Report"], "package": "elm-lang/virtual-dom"}, "Platform": {"imports": ["Basics", "Platform.Cmd", "Platform.Sub"], "package": "elm-lang/core"}, "Html": {"imports": ["VirtualDom"], "package": "elm-lang/html"}, "VirtualDom.Expando": {"imports": ["Dict", "Json.Decode", "VirtualDom.Helpers"], "package": "elm-lang/virtual-dom"}, "Test.Convert": {"imports": ["Date", "Date.Extra", "Expect", "Regex", "Test", "Test.Utilities"], "package": "justinmimbs/elm-date-extra"}, "Date.Extra.Facts": {"imports": ["Date"], "package": "justinmimbs/elm-date-extra"}, "Test": {"imports": ["Test.Internal", "Expect", "Fuzz"], "package": "elm-community/elm-test"}, "Shrink": {"imports": ["Lazy.List", "Lazy", "List", "Array", "Char", "String"], "package": "elm-community/shrink"}, "VirtualDom.History": {"imports": ["Array", "Json.Decode", "Json.Encode", "VirtualDom.Helpers", "VirtualDom.Metadata"], "package": "elm-lang/virtual-dom"}, "Util": {"imports": ["Random.Pcg", "Array", "String"], "package": "elm-community/elm-test"}, "Tests": {"imports": ["Html", "Random.Pcg", "Test", "Test.Convert", "Test.Create", "Test.Examples", "Test.Extract", "Test.Math", "Test.Runner", "TestResult"], "package": "justinmimbs/elm-date-extra"}, "Test.Runner": {"imports": ["Test", "Test.Internal", "Expect", "Random.Pcg", "String"], "package": "elm-community/elm-test"}, "String": {"imports": ["Char", "Maybe", "Result"], "package": "elm-lang/core"}, "Tuple": {"imports": [], "package": "elm-lang/core"}, "Json.Encode": {"imports": ["Array"], "package": "elm-lang/core"}, "Platform.Sub": {"imports": [], "package": "elm-lang/core"}, "Regex": {"imports": ["Maybe"], "package": "elm-lang/core"}, "Date.Internal.Parse": {"imports": ["Date", "Date.Extra.Facts", "Date.Internal.Core", "Regex"], "package": "justinmimbs/elm-date-extra"}, "Test.Expectation": {"imports": [], "package": "elm-community/elm-test"}, "VirtualDom": {"imports": ["Json.Decode", "VirtualDom.Debug"], "package": "elm-lang/virtual-dom"}, "Task": {"imports": ["Basics", "List", "Maybe", "Platform", "Platform.Cmd", "Result"], "package": "elm-lang/core"}, "Lazy": {"imports": [], "package": "elm-lang/lazy"}, "Date": {"imports": ["Task", "Time", "Result"], "package": "elm-lang/core"}, "Test.Internal": {"imports": ["Random.Pcg", "Test.Expectation", "Dict", "Shrink", "Fuzz", "Fuzz.Internal", "RoseTree", "Lazy.List"], "package": "elm-community/elm-test"}, "Expect": {"imports": ["Test.Expectation", "Dict", "Set", "String"], "package": "elm-community/elm-test"}, "Basics": {"imports": [], "package": "elm-lang/core"}, "Date.Extra": {"imports": ["Date", "Date.Extra.Facts", "Date.Internal.Core", "Date.Internal.Extract", "Date.Internal.Format", "Date.Internal.Parse"], "package": "justinmimbs/elm-date-extra"}, "Maybe": {"imports": [], "package": "elm-lang/core"}, "Random.Pcg": {"imports": ["Bitwise", "Json.Encode", "Json.Decode", "Task", "Tuple", "Time"], "package": "mgold/elm-random-pcg"}, "List": {"imports": ["Basics", "Maybe", "Maybe"], "package": "elm-lang/core"}, "Lazy.List": {"imports": ["Array", "List", "Random", "Lazy"], "package": "elm-community/lazy-list"}, "Test.Extract": {"imports": ["Date", "Date.Extra", "Date.Extra.Facts", "Test", "Test.Utilities"], "package": "justinmimbs/elm-date-extra"}, "Fuzz.Internal": {"imports": ["RoseTree", "Random.Pcg"], "package": "elm-community/elm-test"}, "Platform.Cmd": {"imports": [], "package": "elm-lang/core"}, "Date.Internal.Core": {"imports": ["Date", "Date.Extra.Facts", "Date.Internal.RataDie"], "package": "justinmimbs/elm-date-extra"}, "Time": {"imports": ["Basics", "Dict", "List", "Maybe", "Platform", "Platform.Sub", "Task"], "package": "elm-lang/core"}, "Date.Internal.Extract": {"imports": ["Date", "Date.Extra.Facts", "Date.Internal.Core"], "package": "justinmimbs/elm-date-extra"}, "Date.Internal.Format": {"imports": ["Date", "Date.Extra.Facts", "Date.Internal.Extract", "Regex", "String"], "package": "justinmimbs/elm-date-extra"}, "VirtualDom.Overlay": {"imports": ["Json.Decode", "Json.Encode", "VirtualDom.Helpers", "VirtualDom.Metadata", "VirtualDom.Report"], "package": "elm-lang/virtual-dom"}, "Test.Examples": {"imports": ["Date", "Date.Extra", "Test", "Test.Utilities"], "package": "justinmimbs/elm-date-extra"}, "Test.Create": {"imports": ["Date", "Date.Extra", "Regex", "String", "Test", "Test.Utilities", "Tuple"], "package": "justinmimbs/elm-date-extra"}, "Json.Decode": {"imports": ["Array", "Dict", "Json.Encode", "List", "Maybe", "Result"], "package": "elm-lang/core"}, "Test.Utilities": {"imports": ["Date", "Date.Extra", "Date.Extra.Facts", "Expect", "Test"], "package": "justinmimbs/elm-date-extra"}, "Set": {"imports": ["Basics", "Dict", "List"], "package": "elm-lang/core"}, "Test.Math": {"imports": ["Date", "Date.Extra", "Test", "Test.Utilities"], "package": "justinmimbs/elm-date-extra"}, "Random": {"imports": ["Basics", "List", "Platform", "Platform.Cmd", "Task", "Time", "Tuple"], "package": "elm-lang/core"}, "Bitwise": {"imports": [], "package": "elm-lang/core"}, "Char": {"imports": ["Basics"], "package": "elm-lang/core"}, "Date.Internal.RataDie": {"imports": ["Date", "Date.Extra.Facts"], "package": "justinmimbs/elm-date-extra"}, "Fuzz": {"imports": ["Array", "Char", "Util", "Lazy.List", "Shrink", "RoseTree", "Random.Pcg", "Fuzz.Internal"], "package": "elm-community/elm-test"}, "VirtualDom.Helpers": {"imports": ["Json.Decode", "Json.Encode"], "package": "elm-lang/virtual-dom"}, "Result": {"imports": ["Maybe"], "package": "elm-lang/core"}, "Html.Attributes": {"imports": ["Html", "Json.Encode", "VirtualDom"], "package": "elm-lang/html"}, "Dict": {"imports": ["Basics", "Maybe", "List", "String"], "package": "elm-lang/core"}, "Array": {"imports": ["Basics", "Maybe", "List"], "package": "elm-lang/core"}}"""


type alias ModuleGraph =
  Dict String (Set String, String)


decodeModuleGraph : Decoder ModuleGraph
decodeModuleGraph =
  Json.Decode.dict
    (Json.Decode.map2
      (,)
      (Json.Decode.field "imports" <| Json.Decode.map Set.fromList <| Json.Decode.list Json.Decode.string)
      (Json.Decode.field "package" Json.Decode.string)
    )


--

main : Program Never Model Msg
main =
  Html.program
    { init = input |> Json.Decode.decodeString decodeModuleGraph |> Result.withDefault Dict.empty |> init
    , update = update
    , view = view
    , subscriptions = \_ -> Sub.none
    }


-- Model

type alias Model =
  { graphs : Graphs
  , excludedPackages : Set Node
  , selectedModule : Maybe Node
  }


type alias Graphs =
  { packages : (Set Edge, Dict Node String)
  , modules : (Set Edge, Dict Node (String, String))
  }


init : ModuleGraph -> (Model, Cmd a)
init moduleGraph =
  let
    graphs = graphsFromModuleGraph moduleGraph
  in
    ( Model
        graphs
        (initExcludedPackages (graphs.packages |> Tuple.second))
        Nothing
    , Cmd.none
    )


graphsFromModuleGraph : ModuleGraph -> Graphs
graphsFromModuleGraph moduleGraph =
  let
    moduleIdFromName : String -> Node
    moduleIdFromName =
      moduleGraph
        |> Dict.keys
        |> List.indexedMap (flip (,))
        |> Dict.fromList
        |> lookup -1

    --(Set Edge, Dict Node (String, String))
    (moduleEdges, moduleLabels) =
      moduleGraph
        |> Dict.foldl
            (\moduleName (imports, packageName) (edges, labels) ->
              let
                moduleId = moduleIdFromName moduleName
              in
                ( Set.union
                    (imports |> Set.map ((flip (,)) moduleId << moduleIdFromName))
                    edges
                , Dict.insert
                    moduleId
                    (moduleName, packageName)
                    labels
                )
            )
            (Set.empty, Dict.empty)

    packageNameFromModuleId : Node -> String
    packageNameFromModuleId =
      (flip Dict.get) moduleLabels >> Maybe.map Tuple.second >> Maybe.withDefault "<package>"

    packageNameEdges : Set (String, String)
    packageNameEdges =
      moduleEdges
        |> Set.map (mapTuple packageNameFromModuleId)
        |> Set.filter (uncurry (/=))

    packageIdFromName : Dict String Node
    packageIdFromName =
      packageNameEdges
        |> Set.foldl
            (\(x, y) -> Set.insert x >> Set.insert y)
            Set.empty
        |> Set.toList
        |> List.indexedMap (flip (,))
        |> Dict.fromList
  in
    Graphs
      ( packageNameEdges |> Set.map (mapTuple (lookup -1 packageIdFromName))
      , invertDict packageIdFromName
      )
      ( moduleEdges, moduleLabels )


initExcludedPackages : Dict Node String -> Set Node
initExcludedPackages packageLabels =
  [ "elm-lang/core", "elm-lang/html", "elm-lang/virtual-dom" ]
    |> List.filterMap
        ((flip Dict.get) (invertDict packageLabels))
    |> Set.fromList


-- Msg

type Msg
  = ToggleModule Node
  | TogglePackage Node


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ToggleModule node ->
      ( { model | selectedModule = model.selectedModule |> toggleMaybe node }
      , Cmd.none
      )

    TogglePackage node ->
      ( { model | excludedPackages = model.excludedPackages |> toggleSet node }
      , Cmd.none
      )


-- view

defaultOptions = Diagram.defaultOptions
defaultOptionsConnectivity = DiagramConnectivity.defaultOptions


view : Model -> Html Msg
view { graphs, excludedPackages, selectedModule } =
  let
    (packageEdges, packageLabels) = graphs.packages
    (moduleEdges, moduleLabels) = graphs.modules

    isExcludedPackage =
      (flip Set.member) excludedPackages

    packageView =
      packageEdges
        |> AcyclicDigraph.fromEdges
        |> unpack
            (always <| Html.text "Graph contains cycles")
            (Diagram.viewWithOptions
              { defaultOptions
                | viewLabel = viewLabel << isExcludedPackage <<* lookup "" packageLabels
                , colorNode = nodeColor << isExcludedPackage
                , colorEdge = edgeColor << (mapTuple isExcludedPackage)
              }
            )

    excludedPackageNames = Set.map (lookup "" packageLabels) excludedPackages

    includedModuleIds =
      Dict.foldl
        (\moduleId (_, packageName) set ->
          if Set.member packageName excludedPackageNames then
            set
          else
            Set.insert moduleId set
        )
        Set.empty
        moduleLabels

    moduleView =
      moduleEdges
        |> induceSubgraph includedModuleIds
        |> AcyclicDigraph.fromEdges
        |> unpack
            (always <| Html.text "Graph contains cycles")
            (viewDiagram moduleLabels selectedModule)

  in
    Html.div
      [ Html.Attributes.style
          [ ("margin", "40px")
          , ("font-family", "Helvetica, Arial, san-serif")
          ]
      ]
      [ Html.div
          []
          [ viewHeader "Packages"
          , packageView |> Html.map TogglePackage
          ]
      , Html.div
          []
          [ viewHeader "Modules"
          , moduleView |> Html.map ToggleModule
          ]
      ]


viewHeader : String -> Html a
viewHeader string =
  Html.h2
    [ Html.Attributes.style
        [ ("margin", "20px 0")
        , ("font-size", "20px")
        , ("font-weight", "normal")
        ]
    ]
    [ Html.text string
    ]


viewDiagram : Dict Node (String, String) -> Maybe Node -> AcyclicDigraph -> Html Node
viewDiagram moduleLabels selectedNode graph =
  let
    moduleLabelFromNode : Node -> (String, String)
    moduleLabelFromNode =
      lookup ("<module name>", "<package name>") moduleLabels
  in
    case selectedNode of
      Just node ->
        DiagramConnectivity.viewWithOptions
          { defaultOptionsConnectivity | viewLabel = \d n -> viewLabel2 (isNothing d) (moduleLabelFromNode n) }
          node
          graph

      Nothing ->
        Diagram.viewWithOptions
          { defaultOptions | viewLabel = viewLabel2 False << moduleLabelFromNode }
          graph


induceSubgraph : Set Node -> Set Edge -> Set Edge
induceSubgraph nodes =
  Set.filter
    (\(x, y) ->
      Set.member x nodes && Set.member y nodes
    )


edgeColor : (Bool, Bool) -> String
edgeColor (xIsDimmed, yIsDimmed) =
  if xIsDimmed || yIsDimmed then
    "rgba(0, 0, 0, 0.1)"
  else
    "gray"


nodeColor : Bool -> String
nodeColor isDimmed =
  if isDimmed then
    "rgb(200, 200, 200)"
  else
    "black"


labelAttributes : List (Svg.Attribute a)
labelAttributes =
  [ Svg.Attributes.x "4px"
  , Svg.Attributes.fontFamily "Helvetica, Arial"
  , Svg.Attributes.fontSize "12px"
  , Svg.Attributes.dominantBaseline "middle"
  ]


labelText : Bool -> String -> Svg a
labelText isDimmed label =
  Svg.tspan
    [ Svg.Attributes.fill (if isDimmed then "rgb(200, 200, 200)" else "black")
    ]
    [ Svg.text label
    ]


viewLabel : Bool -> String -> Svg a
viewLabel isDimmed label =
  Svg.text_
    labelAttributes
    [ labelText isDimmed label
    ]


viewLabel2 : Bool -> (String, String) -> Svg a
viewLabel2 isDimmed (label, sublabel) =
  Svg.text_
    labelAttributes
    [ labelText isDimmed label
    , labelText True <| " (" ++ sublabel ++ ")"
    ]


-- extra

isNothing : Maybe a -> Bool
isNothing m =
  case m of
    Just _  -> False
    Nothing -> True


toggleMaybe : a -> Maybe a -> Maybe a
toggleMaybe a ma =
  if ma == Just a then
    Nothing
  else
    Just a


unpack : (e -> x) -> (a -> x) -> Result e a -> x
unpack fromErr fromOk result =
  case result of
    Err e ->
      fromErr e
    Ok a ->
      fromOk a


mapTuple : (a -> b) -> (a, a) -> (b, b)
mapTuple f (x, y) =
  (f x, f y)


toggleSet : comparable -> Set comparable -> Set comparable
toggleSet a set =
  if Set.member a set then
    Set.remove a set
  else
    Set.insert a set


lookup : v -> (Dict comparable v) -> comparable -> v
lookup default dict =
  (flip Dict.get) dict >> Maybe.withDefault default


{-| Given a Dict x y, return the Dict y x. Assume the Dict represents a
bijective mapping.
-}
invertDict : Dict comparable comparable1 -> Dict comparable1 comparable
invertDict =
  Dict.foldl
    (flip Dict.insert)
    Dict.empty


infixl 8 <<*

(<<*) : (x -> a -> b) -> (x -> a) -> x -> b
(<<*) f g x =
  f x (g x)
