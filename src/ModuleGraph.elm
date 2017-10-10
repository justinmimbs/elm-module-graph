module ModuleGraph exposing (Input, decodeInput, Model, init, Msg, update, view)

import AcyclicDigraph exposing (Node, Edge, Cycle, AcyclicDigraph)
import ArcDiagram
import ArcDiagram.Distance
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes
import Json.Decode exposing (Decoder)
import Set exposing (Set)
import Svg exposing (Svg)
import Svg.Attributes


-- Input


type alias Input =
    Dict String ( Set String, String )


decodeInput : Decoder Input
decodeInput =
    Json.Decode.dict
        (Json.Decode.map2
            (,)
            (Json.Decode.field "imports" <| Json.Decode.map Set.fromList <| Json.Decode.list Json.Decode.string)
            (Json.Decode.field "package" Json.Decode.string)
        )



-- Model


type alias Model =
    { graphs : Graphs
    , excludedPackages : Set Node
    , selectedModule : Maybe Node
    }


type alias Graphs =
    { packages : ( Set Edge, Dict Node String )
    , modules : ( Set Edge, Dict Node ( String, String ) )
    }


init : Input -> Model
init input =
    let
        graphs =
            graphsFromInput input
    in
        Model
            graphs
            (initExcludedPackages (graphs.packages |> Tuple.second))
            Nothing


graphsFromInput : Input -> Graphs
graphsFromInput input =
    let
        moduleIdFromName : String -> Node
        moduleIdFromName =
            input
                |> Dict.keys
                |> List.indexedMap (flip (,))
                |> Dict.fromList
                |> lookup -1

        --(Set Edge, Dict Node (String, String))
        ( moduleEdges, moduleLabels ) =
            input
                |> Dict.foldl
                    (\moduleName ( imports, packageName ) ( edges, labels ) ->
                        let
                            moduleId =
                                moduleIdFromName moduleName
                        in
                            ( Set.union
                                (imports |> Set.map ((flip (,)) moduleId << moduleIdFromName))
                                edges
                            , Dict.insert
                                moduleId
                                ( moduleName, packageName )
                                labels
                            )
                    )
                    ( Set.empty, Dict.empty )

        packageNameFromModuleId : Node -> String
        packageNameFromModuleId =
            (flip Dict.get) moduleLabels >> Maybe.map Tuple.second >> Maybe.withDefault "<package>"

        packageNameEdges : Set ( String, String )
        packageNameEdges =
            moduleEdges
                |> Set.map (mapTuple packageNameFromModuleId)
                |> Set.filter (uncurry (/=))

        packageIdFromName : Dict String Node
        packageIdFromName =
            packageNameEdges
                |> Set.foldl
                    (\( x, y ) -> Set.insert x >> Set.insert y)
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


update : Msg -> Model -> Model
update msg model =
    case msg of
        ToggleModule node ->
            { model | selectedModule = model.selectedModule |> toggleMaybe node }

        TogglePackage node ->
            { model | excludedPackages = model.excludedPackages |> toggleSet node }



-- view


defaultLayout : ArcDiagram.Layout
defaultLayout =
    ArcDiagram.defaultLayout


packagesLayout : ArcDiagram.Layout
packagesLayout =
    { defaultLayout
        | nodePadding = 2
        , labelWidth = 240
        , labelMinHeight = 18
    }


modulesLayout : ArcDiagram.Layout
modulesLayout =
    { packagesLayout
        | labelWidth = 360
    }


view : Model -> Html Msg
view { graphs, excludedPackages, selectedModule } =
    let
        ( packageEdges, packageLabels ) =
            graphs.packages

        ( moduleEdges, moduleLabels ) =
            graphs.modules

        packageNameFromId =
            lookup "<package>" packageLabels

        moduleLabelFromId =
            lookup ( "<module>", "<package>" ) moduleLabels

        isExcludedPackage =
            (flip Set.member) excludedPackages

        packageView =
            packageEdges
                |> AcyclicDigraph.fromEdges
                |> unpack
                    (viewCycles packageNameFromId)
                    (ArcDiagram.view
                        packagesLayout
                        { viewLabel = viewLabel << isExcludedPackage <<* packageNameFromId
                        , colorNode = nodeColor << isExcludedPackage
                        , colorEdge = edgeColor << (mapTuple isExcludedPackage)
                        }
                    )

        excludedPackageNames =
            Set.map packageNameFromId excludedPackages

        includedModuleIds =
            Dict.foldl
                (\moduleId ( _, packageName ) set ->
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
                    (viewCycles (moduleLabelFromId >> Tuple.first))
                    (viewModulesDiagram moduleLabelFromId selectedModule)
    in
        Html.div
            [ Html.Attributes.style
                [ ( "font-family", "Helvetica, Arial, san-serif" )
                ]
            ]
            [ viewSection
                [ viewHeader "Packages" (Dict.size packageLabels)
                , packageView |> Html.map TogglePackage
                ]
            , viewSection
                [ viewHeader "Modules" (Set.size includedModuleIds)
                , moduleView |> Html.map ToggleModule
                ]
            ]


viewSection : List (Html a) -> Html a
viewSection =
    Html.div
        [ Html.Attributes.style
            [ ( "margin", "20px 0 40px" )
            ]
        ]


viewCycles : (Node -> String) -> List Cycle -> Html a
viewCycles toLabel cycles =
    Html.div
        []
        [ Html.text "Graph has the following cycles:"
        , Html.ol
            [ Html.Attributes.style
                [ ( "font-family", labelFontFamily )
                ]
            ]
            (cycles |> List.map (viewCycle toLabel))
        ]


viewCycle : (Node -> String) -> Cycle -> Html a
viewCycle toLabel cycle =
    Html.li
        []
        [ Html.text (cycle |> List.map toLabel |> String.join " -> ") ]


viewHeader : String -> Int -> Html a
viewHeader title n =
    Html.h2
        [ Html.Attributes.style
            [ ( "margin", "0 0 20px" )
            , ( "font-size", "20px" )
            , ( "font-weight", "normal" )
            ]
        ]
        [ Html.text <| title ++ " (" ++ toString n ++ ")"
        ]


defaultPaint : ArcDiagram.Paint
defaultPaint =
    ArcDiagram.defaultPaint


defaultDistancePaint =
    ArcDiagram.Distance.defaultDistancePaint


viewModulesDiagram : (Node -> ( String, String )) -> Maybe Node -> AcyclicDigraph -> Html Node
viewModulesDiagram moduleLabelFromNode selectedNode graph =
    let
        paint : ArcDiagram.Paint
        paint =
            case selectedNode of
                Just node ->
                    ArcDiagram.Distance.paint
                        { defaultDistancePaint
                            | viewLabel = \n d -> viewLabelPair (isNothing d) (moduleLabelFromNode n)
                        }
                        graph
                        node

                Nothing ->
                    { defaultPaint
                        | viewLabel = moduleLabelFromNode >> (viewLabelPair False)
                    }
    in
        ArcDiagram.view
            modulesLayout
            paint
            graph


induceSubgraph : Set Node -> Set Edge -> Set Edge
induceSubgraph nodes =
    Set.filter
        (\( x, y ) ->
            Set.member x nodes && Set.member y nodes
        )


edgeColor : ( Bool, Bool ) -> String
edgeColor ( xIsDimmed, yIsDimmed ) =
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


labelFontFamily : String
labelFontFamily =
    "Menlo, Monaco, Consolas, monospace"


labelAttributes : List (Svg.Attribute a)
labelAttributes =
    [ Svg.Attributes.x "4px"
    , Svg.Attributes.fontFamily labelFontFamily
    , Svg.Attributes.fontSize "11px"
    , Svg.Attributes.dominantBaseline "middle"
    ]


labelText : Bool -> String -> Svg a
labelText isDimmed label =
    Svg.tspan
        [ Svg.Attributes.fill (nodeColor isDimmed)
        ]
        [ Svg.text label
        ]


viewLabel : Bool -> String -> Svg a
viewLabel isDimmed label =
    Svg.text_
        labelAttributes
        [ labelText isDimmed label
        ]


viewLabelPair : Bool -> ( String, String ) -> Svg a
viewLabelPair isDimmed ( label, sublabel ) =
    Svg.text_
        labelAttributes
        [ labelText isDimmed label
        , labelText True <| " (" ++ sublabel ++ ")"
        ]



-- extra


isNothing : Maybe a -> Bool
isNothing m =
    case m of
        Just _ ->
            False

        Nothing ->
            True


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


mapTuple : (a -> b) -> ( a, a ) -> ( b, b )
mapTuple f ( x, y ) =
    ( f x, f y )


toggleSet : comparable -> Set comparable -> Set comparable
toggleSet a set =
    if Set.member a set then
        Set.remove a set
    else
        Set.insert a set


lookup : v -> Dict comparable v -> comparable -> v
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
